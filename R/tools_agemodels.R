# Tools: agemodels ####
#
# The Age models menu: minimal tuning, depth-to-time conversion with a tracked curve, a sedimentation-rate curve or
# tie points, Monte Carlo re-tracking and age models with uncertainty, gap durations, Bayesian age-depth models of
# radioisotopic dates (with or without a floating astrochronology), the de-chirp check of an age model and a
# sedimentation-rate ramp model (WaverideR).

# Helpers ####

# Tracked cycle curve in the format of WaverideR::retrack_wt_MC (depth, mean frequency, sd of the frequency), from a
# re-tracked curve (three columns) or from a tracked period curve with a relative uncertainty (percent)
cd_am_curve <- function(d, type = "period", rel = 3) {
  d <- as.data.frame(d)
  if (identical(type, "retrack")) {
    if (ncol(d) < 3) {
      stop("a re-tracked curve needs three columns: depth, mean frequency and its standard deviation", call. = FALSE)
    }
    m <- d[stats::complete.cases(d[, 1:3]), 1:3, drop = FALSE]
    m <- m[order(m[, 1]), , drop = FALSE]
    out <- cbind(depth = m[, 1], mean_freq = m[, 2], sd = m[, 3])
  } else {
    s <- cd_clean(d)
    if (any(s$y <= 0)) {
      stop("the tracked periods must be positive (column 2 of the curve: period in depth units)", call. = FALSE)
    }
    # a hand-tracked curve has few points: it is resampled to 501 depths so that the age model is not coarser than that
    if (nrow(s) < 501) {
      x <- seq(s$x[1], s$x[nrow(s)], length.out = 501)
      s <- data.frame(x = x, y = stats::approx(s$x, s$y, x)$y)
    }
    out <- cbind(depth = s$x, mean_freq = 1 / s$y, sd = rel / 100 / s$y)
  }
  if (nrow(out) < 3) {
    stop("the tracked curve has fewer than three points", call. = FALSE)
  }
  # a zero sd in the first row makes curve2time_unc() and dur_gaps() draw an infinite frequency for every simulation
  out[, 3] <- pmax(out[, 3], 1e-6 * abs(out[, 2]))
  rownames(out) <- NULL
  out
}

# R code that builds the same curve from 'dat' (variable var)
cd_am_curve_code <- function(type, rel, var = "dat", out = "curve") {
  floor_sd <- sprintf("%s[, 3] <- pmax(%s[, 3], 1e-6 * abs(%s[, 2]))  # a zero sd breaks the simulations", out, out, out)
  if (identical(type, "retrack")) {
    return(c(sprintf("%s <- as.matrix(%s[stats::complete.cases(%s[, 1:3]), 1:3])", out, var, var),
        sprintf("%s <- %s[order(%s[, 1]), ]", out, out, out), floor_sd))
  }
  c(sprintf("trk <- %s[stats::complete.cases(%s[, 1:2]), 1:2]", var, var), "trk <- trk[order(trk[, 1]), ]",
      "if (nrow(trk) < 501) {",
      "  x <- seq(trk[1, 1], trk[nrow(trk), 1], length.out = 501)",
      "  trk <- data.frame(x, approx(trk[, 1], trk[, 2], x)$y)",
      "}",
      sprintf("%s <- cbind(depth = trk[, 1], mean_freq = 1 / trk[, 2], sd = %s / 100 / trk[, 2])", out, cd_fmt(rel)), floor_sd)
}

# Radioisotopic dates from a table read with read_series: columns found by name (position/depth/height, age, sd or
# 2sd, thickness), otherwise in the order position, age, sd, thickness. Returns the table and the R code line.
cd_am_dates <- function(d, var = "dat") {
  d <- as.data.frame(d)
  if (ncol(d) < 3) {
    stop("the dates table needs at least three columns: position (depth or height), age and 1 sigma", call. = FALSE)
  }
  nm <- tolower(names(d))
  hit <- function(pattern, exclude = integer()) {
    k <- which(grepl(pattern, nm))
    k <- setdiff(k, exclude)
    if (length(k)) {
      return(k[1])
    }
    NA_integer_
  }
  ip <- hit("^(position|pos|depth|height|level|z|strat|metre|meter|m)($|[^a-z])")
  ia <- hit("^(age|date|ma|ka)($|[^a-z])", ip)
  i2 <- hit("(2sd|2s|sd2|2sigma|2 sigma|2_sigma|2se)", c(ip, ia))
  is <- hit("(^sd|^1sd|^1s$|sigma|^err|^unc|^se$|age_sd|sd_age)", c(ip, ia, i2))
  it <- hit("thick", c(ip, ia, i2, is))
  if (is.na(ip) || is.na(ia) || (is.na(is) && is.na(i2))) {
    ip <- 1L
    ia <- 2L
    is <- 3L
    i2 <- NA_integer_
    it <- if (ncol(d) >= 4) { 4L } else { NA_integer_ }
  }
  half <- is.na(is)
  isd <- if (half) { i2 } else { is }
  out <- data.frame(position = d[[ip]], age = d[[ia]], sd = d[[isd]] / if (half) { 2 } else { 1 })
  out$thickness <- if (is.na(it)) { 0 } else { d[[it]] }
  out$thickness[!is.finite(out$thickness)] <- 0
  out <- out[stats::complete.cases(out[, 1:3]), , drop = FALSE]
  if (nrow(out) < 2) {
    stop("fewer than two complete dates (position, age, sd) in the table", call. = FALSE)
  }
  if (any(out$sd <= 0)) {
    stop("the uncertainties of the dates must be positive", call. = FALSE)
  }
  rownames(out) <- NULL
  code <- sprintf("dates <- data.frame(position = %s[, %d], age = %s[, %d], sd = %s[, %d]%s, thickness = %s)",
      var, ip, var, ia, var, isd, if (half) { " / 2" } else { "" },
      if (is.na(it)) { "0" } else { sprintf("%s[, %d]", var, it) })
  code <- c(code, "dates <- dates[stats::complete.cases(dates[, 1:3]), ]")
  attr(out, "code") <- code
  out
}

# minimal_tuning() and sedrate2tune() rebuild the depth axis with seq(start, end, by = median step), which loses the
# last point to rounding for most evenly sampled records; moving the last depth by a millionth of a step avoids it
cd_am_nudge <- function(s) {
  s <- as.data.frame(s)
  n <- nrow(s)
  s[n, 1] <- s[n, 1] + 1e-6 * stats::median(diff(s[, 1]))
  s
}

cd_am_nudge_code <- function(var = "inp") {
  sprintf("%s[nrow(%s), 1] <- %s[nrow(%s), 1] + 1e-6 * median(diff(%s[, 1]))  # avoids a row-count error in WaverideR",
      var, var, var, var, var)
}

# Row quantiles of a matrix of simulations (one column per simulation)
cd_am_rowq <- function(m, probs = c(0.025, 0.16, 0.5, 0.84, 0.975)) {
  m <- as.matrix(m)
  out <- matrix(NA_real_, nrow(m), length(probs))
  for (i in seq_len(nrow(m))) {
    out[i, ] <- stats::quantile(m[i, ], probs, na.rm = TRUE, names = FALSE)
  }
  out
}

cd_am_rowq_code <- function(m, out = "q") {
  c(sprintf("%s <- matrix(NA, nrow(%s), 5)", out, m),
      sprintf("for (i in seq_len(nrow(%s))) {", m),
      sprintf("  %s[i, ] <- quantile(%s[i, ], c(0.025, 0.16, 0.5, 0.84, 0.975), na.rm = TRUE)", out, m),
      "}")
}

# Summary table of simulated age models: depth, mean, sd and quantiles
cd_am_summary <- function(depth, ages) {
  ages <- as.matrix(ages)
  q <- cd_am_rowq(ages)
  sdv <- numeric(nrow(ages))
  for (i in seq_len(nrow(ages))) {
    sdv[i] <- stats::sd(ages[i, ], na.rm = TRUE)
  }
  data.frame(depth = depth, mean = rowMeans(ages, na.rm = TRUE), sd = sdv, q2.5 = q[, 1], q16 = q[, 2], median = q[, 3],
      q84 = q[, 4], q97.5 = q[, 5])
}

cd_am_summary_code <- function(depth, ages) {
  c(cd_am_rowq_code(ages),
      sprintf("res <- data.frame(depth = %s, mean = rowMeans(%s), sd = NA, q2.5 = q[, 1], q16 = q[, 2], median = q[, 3],", depth,
          ages),
      "    q84 = q[, 4], q97.5 = q[, 5])",
      sprintf("for (i in seq_len(nrow(%s))) {", ages),
      sprintf("  res$sd[i] <- sd(%s[i, ])", ages),
      "}")
}

# Shaded band between two curves
cd_am_band <- function(x, lo, hi, col = "#1f4e79", alpha = 0.25) {
  ok <- is.finite(x) & is.finite(lo) & is.finite(hi)
  if (sum(ok) < 2) {
    return(invisible(NULL))
  }
  graphics::polygon(c(x[ok], rev(x[ok])), c(lo[ok], rev(hi[ok])), col = grDevices::adjustcolor(col, alpha), border = NA)
  invisible(NULL)
}

# Thin a matrix of simulations to at most n columns (for the figures)
cd_am_thin <- function(m, n = 50) {
  m <- as.matrix(m)
  if (ncol(m) <= n) {
    return(m)
  }
  m[, unique(round(seq(1, ncol(m), length.out = n))), drop = FALSE]
}

# Parse a list of numbers given in the form ("1, 2; 3"); recycle a single value to length n
cd_am_rep <- function(v, n, what) {
  if (!length(v)) {
    stop(sprintf("give %s", what), call. = FALSE)
  }
  if (length(v) == 1) {
    return(rep(v, n))
  }
  if (length(v) != n) {
    stop(sprintf("give one value of %s, or one value per item (%d)", what, n), call. = FALSE)
  }
  v
}

# Slim the de-chirped maps of an age-model check to what plot_agemodel_check() draws: no complex coefficients, the
# depth power only for the squeezed transforms (the others draw Power_norm) and at most max_cols positions per map
cd_am_slim_check <- function(chk, max_cols = 600) {
  for (nm in names(chk$dechirped)) {
    dc <- chk$dechirped[[nm]]
    sparse <- isTRUE(dc$transform %in% c("superlet_sst", "wavelet_sst", "chirplet_sst"))
    for (part in c("tuned", "depth")) {
      w <- dc[[part]]
      if (!is.list(w)) {
        next
      }
      for (el in c("Wave", "Phase", "Ampl", "Wave_cwt", "inst_period", "Power.pval")) {
        w[[el]] <- NULL
      }
      if (part == "depth" && !sparse) {
        w$Power <- NULL
      }
      n <- length(w$axis.1)
      if (n > max_cols) {
        idx <- seq(1, n, by = ceiling(n / max_cols))
        if (is.matrix(w$Power) && ncol(w$Power) == n) {
          w$Power <- w$Power[, idx, drop = FALSE]
        }
        if (part == "depth") {
          for (el in c("Power_norm", "Power_sig", "Power_pval")) {
            if (is.matrix(dc[[el]]) && ncol(dc[[el]]) == n) {
              dc[[el]] <- dc[[el]][, idx, drop = FALSE]
            }
          }
          if (length(dc$rate) == n) {
            dc$rate <- dc$rate[idx]
          }
        }
        w$axis.1 <- w$axis.1[idx]
      }
      dc[[part]] <- w
    }
    chk$dechirped[[nm]] <- dc
  }
  chk
}

cd_tools_agemodels <- function() {
  M <- "Age models"
  mid_default <- function(d) {
    signif(mean(range(d[, 1], na.rm = TRUE)), 5)
  }
  curve_types <- c("Tracked period curve (depth, period)" = "period",
      "Re-tracked curve (depth, mean frequency, sd), from Re-track" = "retrack")
  dc_palettes <- c("Viridis" = "viridis", "Magma" = "magma", "Inferno" = "inferno", "Plasma" = "plasma",
      "Cividis (colour-blind safe)" = "cividis", "Turbo (rainbow-like)" = "turbo", "Spectral" = "Spectral",
      "Zissou" = "Zissou 1")
  list(
    cd_tool("am_mintune", M, "Minimal tuning (WaverideR)", group = "Age models from a cycle", icon = "clock",
        info = paste("Age model from a filtered or extracted cycle (depth in metres): the distance between successive",
            "maxima, minima or both of the cycle is set to its duration. Use a filtered series (Time Series > Filtering)",
            "or a cycle extracted in the Track tab."),
        uses = "WaverideR::minimal_tuning",
        params = list(
          cd_num("cycle", "Duration of the cycle (kyr)", 405, min = 0),
          cd_sel("tune_opt", "Tie points at", c("Maxima (one cycle apart)" = "max", "Minima (one cycle apart)" = "min",
              "Maxima and minima (half a cycle apart)" = "minmax")),
          cd_num("pts", "Points compared on each side (peak detection)", 5, min = 2, step = 1),
          cd_num("t0", "Age of the top of the record (kyr)", 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          inp <- s
          opt <- p$tune_opt
          code <- c(cd_even_code(d), "inp <- dat")
          if (identical(opt, "min")) {
            # minimal_tuning(tune_opt = "min") uses the maxima: the minima are tuned as maxima of the inverted cycle
            inp$y <- -inp$y
            opt <- "max"
            code <- c(code, "inp[, 2] <- -inp[, 2]  # minimal_tuning(tune_opt = \"min\") uses the maxima: invert the cycle")
          }
          inp <- cd_am_nudge(inp)
          # equal neighbouring extrema (flat steps) make minimal_tuning() loop forever: break exact ties
          inp[, 2] <- inp[, 2] + seq_len(nrow(inp)) * 1e-9 * (diff(range(inp[, 2])) + 1)
          r <- WaverideR::minimal_tuning(data = inp, pts = p$pts, cycle = p$cycle, tune_opt = opt, output = 0)
          r <- as.data.frame(r)
          am <- data.frame(depth = s$x, time_kyr = r[, 4] + p$t0)
          sr <- data.frame(depth = s$x, sedrate_cm_kyr = r[, 3])
          o <- list(cd_output(cd_out_name(file, sprintf("-mintune%s-agemodel", cd_fmt(p$cycle))), am, var = "am"),
              cd_output(cd_out_name(file, sprintf("-mintune%s-sedrate", cd_fmt(p$cycle))), sr, var = "sr"))
          code <- c(code, cd_am_nudge_code("inp"),
              "inp[, 2] <- inp[, 2] + seq_len(nrow(inp)) * 1e-9 * (diff(range(inp[, 2])) + 1)  # no equal extrema (endless loop)",
              sprintf("res <- WaverideR::minimal_tuning(data = inp, pts = %d, cycle = %s, tune_opt = %s, output = 0)",
                  as.integer(p$pts), cd_fmt(p$cycle), cd_dep(opt)),
              sprintf("am <- data.frame(depth = dat[, 1], time_kyr = res[, 4] + %s)", cd_fmt(p$t0)),
              "sr <- data.frame(depth = dat[, 1], sedrate_cm_kyr = res[, 3])", cd_code_write(o))
          list(result = list(s = s, am = am, sr = sr), outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("Duration %s kyr; sedimentation rate %s to %s cm/kyr (mean %s)",
                  cd_fmt(diff(range(am[, 2]))), cd_fmt(min(sr[, 2])), cd_fmt(max(sr[, 2])), cd_fmt(mean(sr[, 2])))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE))
          cd_par()
          cd_plot_one(res$s, col = "#1f4e79", xlab = "Depth (m)", ylab = "Cycle")
          cd_par()
          graphics::plot(res$sr[, 1], res$sr[, 2], type = "l", col = "#2e8b57", lwd = 1.5, xlab = "Depth (m)",
              ylab = "Sed. rate (cm/kyr)")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          graphics::plot(res$am[, 1], res$am[, 2], type = "l", col = "#1f4e79", lwd = 1.5, xlab = "Depth (m)",
              ylab = "Age (kyr)")
          graphics::grid(col = "grey90", lty = 1)
        }),
    cd_tool("am_convert", M, "Depth to time: tracked curve, sed. rate or tie points (2 files)", group = "Age models from a cycle",
        inputs = 2L, icon = "hourglass-half",
        info = paste("Converts a record (file 1: depth in metres, value) to time with file 2: a tracked period curve of",
            "one cycle (depth, period), a sedimentation-rate curve (depth, cm/kyr) or tie points (depth, age)."),
        uses = "WaverideR::curve2tune, WaverideR::sedrate2tune, WaverideR::anchor2time",
        params = list(
          cd_sel("method", "File 2 holds", c("Tracked period curve of one cycle" = "curve",
              "Sedimentation-rate curve (cm/kyr)" = "sedrate", "Tie points (depth, age)" = "anchor")),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0, show_if = list(param = "method", values = "curve")),
          cd_num("t0", "Age of the top of the record (kyr)", 0, show_if = list(param = "method", values = c("curve", "sedrate"))),
          cd_chk("interp", "Interpolate the result at an even time step", FALSE),
          cd_num("dt", "Time step (kyr)", 1, min = 0, show_if = list(param = "interp", values = "true"))),
        compute = function(d, p, d2, file, file2) {
          code <- c(cd_code_read(file2, var = "dat2"), "dat2 <- dat2[stats::complete.cases(dat2[, 1:2]), 1:2]")
          if (p$method == "curve") {
            s <- cd_clean(d)
            tc <- cd_clean(d2)
            res <- WaverideR::curve2tune(data = s, tracked_cycle_curve = tc, tracked_cycle_period = p$cycle)
            res <- as.data.frame(res)
            res[, 1] <- res[, 1] + p$t0
            code <- c(code, sprintf("res <- WaverideR::curve2tune(data = dat, tracked_cycle_curve = dat2, tracked_cycle_period = %s)",
                cd_fmt(p$cycle)), sprintf("res[, 1] <- res[, 1] + %s", cd_fmt(p$t0)))
            tag <- paste0("curve", cd_fmt(p$cycle))
          } else if (p$method == "sedrate") {
            s <- cd_even(d)
            sc <- cd_clean(d2)
            if (any(sc$y <= 0)) {
              stop("the sedimentation rates must be positive", call. = FALSE)
            }
            # sedrate2tune() pairs the proxy row by row with the times of the (resampled) rate curve, so the rate curve
            # is first interpolated onto the depths of the record
            sed <- cd_am_nudge(data.frame(depth = s$x, sedrate = stats::approx(sc$x, sc$y, s$x, rule = 2)$y))
            res <- as.data.frame(WaverideR::sedrate2tune(data = s, sed_curve = sed))
            if (nrow(res) != nrow(s)) {
              stop("sedrate2tune returned a different number of rows than the record", call. = FALSE)
            }
            res[, 1] <- res[, 1] + p$t0
            code <- c(code, cd_even_code(d), "sed <- data.frame(depth = dat[, 1], sedrate = approx(dat2[, 1], dat2[, 2], dat[, 1], rule = 2)$y)",
                cd_am_nudge_code("sed"), "res <- WaverideR::sedrate2tune(data = dat, sed_curve = sed)",
                sprintf("res[, 1] <- res[, 1] + %s", cd_fmt(p$t0)))
            tag <- "sedrate"
          } else {
            s <- cd_clean(d)
            ap <- cd_clean(d2)
            if (nrow(ap) < 2) {
              stop("give at least two tie points (depth, age)", call. = FALSE)
            }
            res <- as.data.frame(WaverideR::anchor2time(anchor_points = ap, data = s))
            code <- c(code, "dat2 <- dat2[order(dat2[, 1]), ]", "res <- WaverideR::anchor2time(anchor_points = dat2, data = dat)")
            tag <- "anchor"
          }
          names(res) <- c("time_kyr", names(d)[2])
          am <- data.frame(depth = s$x, time_kyr = res[, 1])
          code <- c(code, "am <- data.frame(depth = dat[, 1], time_kyr = res[, 1])")
          tuned <- res[order(res[, 1]), ]
          tuned <- tuned[!duplicated(tuned[, 1]), ]
          code <- c(code, "res <- res[order(res[, 1]), ]", "res <- res[!duplicated(res[, 1]), ]")
          sfx <- paste0("-tuned-", tag)
          if (p$interp) {
            tuned <- as.data.frame(astrochron::linterp(tuned, dt = p$dt, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::linterp(res, dt = %s, genplot = FALSE)", cd_fmt(p$dt)))
            sfx <- paste0(sfx, "-rsp", cd_fmt(p$dt))
          }
          o <- list(cd_output(cd_out_name(file, sfx), tuned, var = "res"),
              cd_output(cd_out_name(file, paste0("-agemodel-", tag)), am, var = "am"))
          list(result = list(s = s, am = am, tuned = tuned, ties = if (p$method == "anchor") { ap } else { NULL }),
              outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points from %s to %s kyr (duration %s kyr)", nrow(tuned), cd_fmt(min(tuned[, 1])),
                  cd_fmt(max(tuned[, 1])), cd_fmt(diff(range(tuned[, 1])))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), widths = c(2.5, 1))
          cd_par()
          cd_plot_one(res$s, col = "#7f8c8d", xlab = "Depth (m)", ylab = "Record")
          cd_par()
          graphics::plot(res$am[, 1], res$am[, 2], type = "l", col = "#1f4e79", lwd = 1.5, xlab = "Depth (m)",
              ylab = "Time (kyr)")
          graphics::grid(col = "grey90", lty = 1)
          if (!is.null(res$ties)) {
            graphics::points(res$ties[, 1], res$ties[, 2], pch = 21, bg = "#c0392b", col = "white")
          }
          cd_par()
          cd_plot_one(res$tuned, col = "#1f4e79", xlab = "Time (kyr)", ylab = "Record in time")
        }),
    cd_tool("am_retrack", M, "Re-track a cycle (Monte Carlo, multi-proxy)", group = "Uncertainty (Monte Carlo)",
        inputs = 2L, full_table = TRUE, heavy = TRUE, icon = "route",
        info = paste("File 1: depth and one or more proxy columns; file 2: a tracked period curve (depth, period), e.g.",
            "from the Track tab. Every simulation blends randomly perturbed copies of the track and follows the nearest",
            "ridge in the scalogram of a randomly chosen proxy; the spread of the re-tracked curves is the tracking",
            "uncertainty, used by Age model with uncertainty, Gap duration and Dates + astrochronology."),
        uses = "WaverideR::analyse_wavelet, WaverideR::retrack_wt_MC, WaverideR::plot_wavelet",
        params = list(
          cd_txt("cols", "Proxy columns of file 1 (numbers; empty = all)", "", numeric = TRUE),
          cd_num("n_sim", "Number of simulations", 20, min = 2, step = 5),
          cd_num("spread", "Perturbation of the track (+/- %)", 5, min = 0, step = 1),
          cd_num("period_up", "Upper limit of the ridge search (factor of the track)", 1.2, min = 1, step = 0.05),
          cd_num("period_down", "Lower limit of the ridge search (factor of the track)", 0.8, min = 0, max = 1, step = 0.05),
          cd_sel("smoothing", "Smoothing of every re-tracked curve", c("LOESS, automatic span" = "auto", "LOESS, given span" = "loess",
              "Moving average" = "window", "None" = "none")),
          cd_num("smooth_val", "Span (0 to 1) or window length (depth units)", 0.1, min = 0,
              show_if = list(param = "smoothing", values = c("loess", "window"))),
          cd_num("lowerPeriod", "Shortest period of the scalograms", function(d) { signif(3 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_num("upperPeriod", "Longest period of the scalograms", function(d) { signif(diff(range(d[, 1], na.rm = TRUE)) * 0.33, 3) },
              min = 0),
          cd_num("dj", "Scale resolution dj", 0.02, min = 0.002, step = 0.005),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 8, min = 1, step = 1),
          cd_num("seed", "Random seed", 1337, step = 1),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          if (ncol(d) < 2) {
            stop("file 1 needs a depth column and at least one proxy column", call. = FALSE)
          }
          cols <- if (length(p$cols)) { as.integer(p$cols) } else { 2:ncol(d) }
          cols <- unique(cols[cols >= 2 & cols <= ncol(d)])
          if (!length(cols)) {
            stop("no valid proxy columns selected", call. = FALSE)
          }
          lo <- -Inf
          hi <- Inf
          dts <- numeric(length(cols))
          for (j in seq_along(cols)) {
            s <- cd_clean(d[, c(1, cols[j])])
            lo <- max(lo, min(s$x))
            hi <- min(hi, max(s$x))
            dts[j] <- cd_dt(s$x)
          }
          dt <- stats::median(dts)
          tc <- cd_clean(d2)
          if (!(hi > lo) || nrow(tc) < 2) {
            stop("the proxies do not overlap, or the track has fewer than two points", call. = FALSE)
          }
          # the grid is built from the rounded values that the R code shows, so that the code gives the same result
          lo <- as.numeric(cd_fmt(lo))
          hi <- as.numeric(cd_fmt(hi))
          dt <- as.numeric(cd_fmt(dt))
          grid <- seq(lo, hi, by = dt)
          wt_list <- vector("list", length(cols))
          for (j in seq_along(cols)) {
            s <- cd_clean(d[, c(1, cols[j])])
            y <- stats::approx(s$x, s$y, grid, rule = 2)$y
            wt_list[[j]] <- WaverideR::analyse_wavelet(data = cbind(grid, y), dj = p$dj, lowerPeriod = p$lowerPeriod,
                upperPeriod = p$upperPeriod, omega_nr = p$omega_nr, verbose = FALSE)
          }
          trk <- stats::approx(tc$x, tc$y, grid, rule = 2)$y
          f <- p$spread / 100
          dtrack <- if (f > 0) { cbind(trk * (1 - f), trk * (1 + f)) } else { cbind(trk) }
          sm <- switch(p$smoothing, auto = "auto", loess = c("loess", p$smooth_val), window = c("window", p$smooth_val), "none")
          r <- WaverideR::retrack_wt_MC(wt_list = wt_list, data_track = dtrack, x_axis = grid, smoothing = sm,
              nr_simulations = p$n_sim, seed_nr = p$seed, period_up = p$period_up, period_down = p$period_down, output = 2,
              verbose = FALSE)
          # the last row can be lost to rounding inside retrack_wt_MC (NaN): rows without a re-tracked value are dropped
          ok <- is.finite(r[[1]][, 2])
          summ <- as.data.frame(r[[1]])[ok, ]
          runs <- as.matrix(r[[2]])[ok, , drop = FALSE]
          grid <- grid[ok]
          trk <- trk[ok]
          rownames(summ) <- NULL
          q <- cd_am_rowq(runs)
          per <- data.frame(depth = grid, period = rowMeans(runs, na.rm = TRUE))
          runs_df <- data.frame(depth = grid, runs)
          names(runs_df) <- c("depth", paste0("run_", seq_len(ncol(runs))))
          o <- list(cd_output(cd_out_name(file, "-retrack"), summ, header = TRUE, var = "retrack"),
              cd_output(cd_out_name(file, "-retrack-period"), per, var = "per"),
              cd_output(cd_out_name(file, "-retrack-runs"), runs_df, header = TRUE, var = "runs"))
          code <- c(cd_code_read(file2, var = "dat2"), "dat2 <- dat2[stats::complete.cases(dat2[, 1:2]), 1:2]",
              sprintf("cols <- %s", cd_dep(cols)),
              sprintf("grid <- seq(%s, %s, by = %s)", cd_fmt(lo), cd_fmt(hi), cd_fmt(dt)),
              "wt_list <- list()",
              "for (j in seq_along(cols)) {",
              "  ok <- !is.na(dat[, cols[j]])",
              "  y <- approx(dat[ok, 1], dat[ok, cols[j]], grid, rule = 2)$y",
              sprintf(paste0("  wt_list[[j]] <- WaverideR::analyse_wavelet(data = cbind(grid, y), dj = %s, lowerPeriod = %s,",
                  " upperPeriod = %s, omega_nr = %s, verbose = FALSE)"), cd_fmt(p$dj), cd_fmt(p$lowerPeriod),
                  cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr)),
              "}",
              "trk <- approx(dat2[, 1], dat2[, 2], grid, rule = 2)$y",
              if (f > 0) {
                sprintf("dtrack <- cbind(trk * %s, trk * %s)", cd_fmt(1 - f), cd_fmt(1 + f))
              } else {
                "dtrack <- cbind(trk)"
              },
              sprintf(paste0("res <- WaverideR::retrack_wt_MC(wt_list = wt_list, data_track = dtrack, x_axis = grid, smoothing = %s,\n",
                  "    nr_simulations = %d, seed_nr = %d, period_up = %s, period_down = %s, output = 2)"), cd_dep(sm),
                  as.integer(p$n_sim), as.integer(p$seed), cd_fmt(p$period_up), cd_fmt(p$period_down)),
              "ok <- is.finite(res[[1]][, 2])", "retrack <- as.data.frame(res[[1]])[ok, ]",
              "per <- data.frame(depth = grid[ok], period = rowMeans(res[[2]][ok, , drop = FALSE]))",
              "runs <- data.frame(depth = grid[ok], res[[2]][ok, , drop = FALSE])",
              "names(runs) <- c(\"depth\", paste0(\"run_\", seq_len(ncol(runs) - 1)))", cd_code_write(o))
          rel <- 100 * stats::median(summ$sd / summ$mean_freq, na.rm = TRUE)
          list(result = list(wt = cd_wt_slim(wt_list[[1]]), grid = grid, mean = per$period, lo = q[, 1], hi = q[, 5],
              runs = cd_am_thin(runs, 50), track = data.frame(depth = grid, period = trk)),
              outputs = o, code = code,
              text = c(sprintf("%d simulations on %d proxy record(s); median relative sd of the frequency %s %%", ncol(runs),
                  length(cols), cd_fmt(signif(rel, 3))),
                  "Use the -retrack file (three columns) as a 'Re-tracked curve' in the other tools of this menu."))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          pal <- cd_pal_split(p$palette)
          lines <- cbind(res$grid, res$mean, res$lo, res$hi)
          WaverideR::plot_wavelet(res$wt, add_lines = lines, palette_name = pal$name, color_brewer = pal$family,
              periodlab = "Period", x_lab = "Depth", add_avg = FALSE, dev_new = FALSE)
        }),
    cd_tool("am_unc", M, "Age model with uncertainty (Monte Carlo)", group = "Uncertainty (Monte Carlo)",
        full_table = TRUE, icon = "chart-area",
        heavy = function(p) { !identical(p$anchor, "none") || isTRUE(p$n_sim > 500) },
        info = paste("Simulates age models from a tracked cycle with its uncertainty (a re-tracked curve, or a tracked",
            "period curve with a relative uncertainty) and the uncertainty of the cycle duration; optionally anchored",
            "to dated levels (ages in kyr) with hiatuses. Writes the mean, sd and quantiles of the age at every depth."),
        uses = "WaverideR::curve2time_unc, WaverideR::curve2time_unc_anchor",
        params = list(
          cd_sel("type", "The data file is a", curve_types),
          cd_num("rel", "Relative uncertainty of the tracked period (1 sd, %)", 3, min = 0,
              show_if = list(param = "type", values = "period")),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_num("cycle_unc", "Uncertainty of the cycle duration (kyr)", 0, min = 0),
          cd_sel("cycle_dist", "Distribution of the cycle duration", c("Normal (uncertainty = 1 sd)" = "n",
              "Uniform (uncertainty = half-width)" = "u")),
          cd_num("n_sim", "Number of simulations", 50, min = 5, step = 10),
          cd_sel("anchor", "Ages", c("Floating (time from the top)" = "none", "Anchored to dated levels" = "anchor")),
          cd_num("t0", "Age of the top of the record (kyr)", 0, show_if = list(param = "anchor", values = "none")),
          cd_txt("anc_depth", "Depths of the dated levels", mid_default, numeric = TRUE,
              show_if = list(param = "anchor", values = "anchor")),
          cd_txt("anc_age", "Ages of the dated levels (kyr)", "", numeric = TRUE, show_if = list(param = "anchor", values = "anchor")),
          cd_txt("anc_sd", "Age uncertainties (1 sd, kyr; one value for all)", "10", numeric = TRUE,
              show_if = list(param = "anchor", values = "anchor")),
          cd_num("anc_dunc", "Depth uncertainty of the dated levels (full width)", 0, min = 0,
              show_if = list(param = "anchor", values = "anchor")),
          cd_txt("gap_depth", "Depths of hiatuses (empty = none)", "", numeric = TRUE, show_if = list(param = "anchor", values = "anchor")),
          cd_txt("gap_dur", "Durations of the hiatuses (kyr)", "", numeric = TRUE, show_if = list(param = "anchor", values = "anchor")),
          cd_txt("gap_sd", "Uncertainties of the hiatuses (1 sd, kyr)", "", numeric = TRUE,
              show_if = list(param = "anchor", values = "anchor")),
          cd_num("max_runs", "Rejections before a disagreeing date is dropped", 200, min = 10, step = 50,
              show_if = list(param = "anchor", values = "anchor")),
          cd_chk("dir", "Age increases with depth", TRUE, show_if = list(param = "anchor", values = "anchor")),
          cd_num("seed", "Random seed", 1337, step = 1)),
        compute = function(d, p, d2, file, file2) {
          cv <- cd_am_curve(d, p$type, p$rel)
          code <- cd_am_curve_code(p$type, p$rel)
          anc <- NULL
          if (identical(p$anchor, "none")) {
            set.seed(p$seed)
            ages <- WaverideR::curve2time_unc(tracked_cycle_curve = cv, tracked_cycle_period = p$cycle,
                tracked_cycle_period_unc = p$cycle_unc, tracked_cycle_period_unc_dist = p$cycle_dist, n_simulations = p$n_sim,
                output = 1)
            ages <- as.matrix(ages) + p$t0
            dep <- seq(cv[1, 1], by = stats::median(diff(cv[, 1])), length.out = nrow(ages))
            code <- c(code, sprintf("set.seed(%d)", as.integer(p$seed)),
                sprintf(paste0("ages <- WaverideR::curve2time_unc(tracked_cycle_curve = curve, tracked_cycle_period = %s,\n",
                    "    tracked_cycle_period_unc = %s, tracked_cycle_period_unc_dist = %s, n_simulations = %d, output = 1) + %s"),
                    cd_fmt(p$cycle), cd_fmt(p$cycle_unc), cd_dep(p$cycle_dist), as.integer(p$n_sim), cd_fmt(p$t0)),
                "depth <- seq(curve[1, 1], by = median(diff(curve[, 1])), length.out = nrow(ages))")
          } else {
            z <- p$anc_depth
            if (!length(z) || !length(p$anc_age)) {
              stop("give the depths and ages of the dated levels", call. = FALSE)
            }
            a <- cd_am_rep(p$anc_age, length(z), "the ages")
            sa <- cd_am_rep(p$anc_sd, length(z), "the age uncertainties")
            anc <- data.frame(id = paste0("date_", seq_along(z)), age = a, unc = sa, dist = "n", depth = z,
                depth_unc = p$anc_dunc, depth_dist = "u", stringsAsFactors = FALSE)
            gaps <- NULL
            if (length(p$gap_depth)) {
              ng <- length(p$gap_depth)
              gaps <- data.frame(dur = cd_am_rep(p$gap_dur, ng, "the hiatus durations"),
                  unc = cd_am_rep(p$gap_sd, ng, "the hiatus uncertainties"), depth = p$gap_depth, dist = "n",
                  stringsAsFactors = FALSE)
            }
            r <- WaverideR::curve2time_unc_anchor(age_constraint = anc, tracked_cycle_curve = cv, tracked_cycle_period = p$cycle,
                tracked_cycle_period_unc = p$cycle_unc, tracked_cycle_period_unc_dist = p$cycle_dist, n_simulations = p$n_sim,
                gap_constraints = gaps, max_runs = p$max_runs, seed_nr = p$seed, dir = p$dir, verbose = FALSE)
            dep <- r[[1]]
            ages <- as.matrix(r[[2]])
            code <- c(code, sprintf(paste0("anchors <- data.frame(id = paste0(\"date_\", 1:%d), age = %s, unc = %s, dist = \"n\",",
                " depth = %s, depth_unc = %s, depth_dist = \"u\")"), length(z), cd_dep(a), cd_dep(sa), cd_dep(z), cd_fmt(p$anc_dunc)),
                if (is.null(gaps)) {
                  "gaps <- NULL"
                } else {
                  sprintf("gaps <- data.frame(dur = %s, unc = %s, depth = %s, dist = \"n\")", cd_dep(gaps$dur), cd_dep(gaps$unc),
                      cd_dep(gaps$depth))
                },
                sprintf(paste0("r <- WaverideR::curve2time_unc_anchor(age_constraint = anchors, tracked_cycle_curve = curve,\n",
                    "    tracked_cycle_period = %s, tracked_cycle_period_unc = %s, tracked_cycle_period_unc_dist = %s,\n",
                    "    n_simulations = %d, gap_constraints = gaps, max_runs = %d, seed_nr = %d, dir = %s)"), cd_fmt(p$cycle),
                    cd_fmt(p$cycle_unc), cd_dep(p$cycle_dist), as.integer(p$n_sim), as.integer(p$max_runs), as.integer(p$seed), p$dir),
                "depth <- r[[1]]", "ages <- r[[2]]")
          }
          if (!length(ages) || ncol(ages) < 2) {
            stop("fewer than two age models were kept (all simulations gave a non-monotonic age model)", call. = FALSE)
          }
          summ <- cd_am_summary(dep, ages)
          am <- data.frame(depth = summ$depth, age_kyr = summ$mean)
          o <- list(cd_output(cd_out_name(file, "-agemodel-unc"), summ, header = TRUE, var = "res"),
              cd_output(cd_out_name(file, "-agemodel-mean"), am, var = "am"))
          code <- c(code, cd_am_summary_code("depth", "ages"), "am <- data.frame(depth = res$depth, age_kyr = res$mean)",
              cd_code_write(o))
          w <- summ$q97.5 - summ$q2.5
          list(result = list(summ = summ, runs = cd_am_thin(ages, 40), anchors = anc), outputs = o, code = code,
              text = sprintf("%d age models kept; duration %s kyr; 95%% range of the age: median width %s kyr, largest %s kyr",
                  ncol(ages), cd_fmt(signif(diff(range(summ$mean)), 5)), cd_fmt(signif(stats::median(w), 3)),
                  cd_fmt(signif(max(w), 3))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          s <- res$summ
          graphics::layout(matrix(1:2, ncol = 1), heights = c(2, 1))
          cd_par()
          graphics::plot(s$depth, s$mean, type = "n", ylim = range(c(s$q2.5, s$q97.5), finite = TRUE), xlab = "Depth",
              ylab = "Age (kyr)")
          graphics::grid(col = "grey90", lty = 1)
          cd_am_band(s$depth, s$q2.5, s$q97.5, alpha = 0.18)
          cd_am_band(s$depth, s$q16, s$q84, alpha = 0.3)
          graphics::matlines(s$depth, res$runs, col = grDevices::adjustcolor("grey30", 0.25), lty = 1, lwd = 0.6)
          graphics::lines(s$depth, s$mean, col = "#1f4e79", lwd = 1.8)
          if (!is.null(res$anchors)) {
            a <- res$anchors
            graphics::arrows(a$depth, a$age - 2 * a$unc, a$depth, a$age + 2 * a$unc, angle = 90, code = 3, length = 0.03,
                col = "#c0392b", lwd = 1.4)
            graphics::points(a$depth, a$age, pch = 21, bg = "#c0392b", col = "white")
          }
          graphics::legend("topleft", legend = c("mean", "68% range", "95% range", "simulations"), bty = "n", cex = 0.8,
              col = c("#1f4e79", grDevices::adjustcolor("#1f4e79", 0.5), grDevices::adjustcolor("#1f4e79", 0.25), "grey50"),
              lwd = c(1.8, 8, 8, 1))
          cd_par()
          graphics::plot(s$depth, s$q97.5 - s$q2.5, type = "l", col = "#c0392b", lwd = 1.5, xlab = "Depth",
              ylab = "95% range (kyr)")
          graphics::grid(col = "grey90", lty = 1)
        }),
    cd_tool("am_gaps", M, "Duration of a hiatus (Monte Carlo, 2 files)", group = "Uncertainty (Monte Carlo)", inputs = 2L,
        full_table = TRUE, heavy = TRUE, icon = "scissors",
        info = paste("Estimates the duration of a gap at a given depth from a stable cycle: file 1 is the tracked curve on",
            "both sides of the gap (a re-tracked curve, or a period curve with a relative uncertainty), file 2 the proxy",
            "record. Both sides are tuned, the cycle is extracted and the missing part of the cycle across the gap is the",
            "gap duration."),
        uses = "WaverideR::dur_gaps",
        params = list(
          cd_sel("type", "File 1 is a", curve_types),
          cd_num("rel", "Relative uncertainty of the tracked period (1 sd, %)", 3, min = 0,
              show_if = list(param = "type", values = "period")),
          cd_num("gap", "Depth of the gap", mid_default),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_num("cycle_unc", "Uncertainty of the cycle duration (kyr)", 0, min = 0),
          cd_sel("cycle_dist", "Distribution of the cycle duration", c("Uniform (uncertainty = half-width)" = "u",
              "Normal (uncertainty = 1 sd)" = "n")),
          cd_num("missing", "Duration of the cycle missing in the gap (kyr)", 405, min = 0),
          cd_num("n_missing", "Number of missing cycles", 1, min = 0, step = 1),
          cd_num("missing_unc", "Uncertainty of the missing-cycle duration (kyr, if it differs from the tracked cycle)", 0, min = 0),
          cd_num("band_lo", "Extracted band: shortest period (factor of the missing cycle)", 0.8, min = 0.1, step = 0.05),
          cd_num("band_hi", "Extracted band: longest period (factor of the missing cycle)", 1.2, min = 1, step = 0.05),
          cd_sel("min_max", "Measure between", c("Maxima of the cycle" = "max", "Minima of the cycle" = "min")),
          cd_num("pts", "Points compared on each side (peak detection)", 5, min = 1, step = 1),
          cd_num("dj", "Scale resolution dj of the tuned scalograms", 0.02, min = 0.002, step = 0.005),
          cd_num("n_sim", "Number of simulations", 10, min = 1, step = 5),
          cd_num("seed", "Random seed", 1337, step = 1)),
        compute = function(d, p, d2, file, file2) {
          cv <- cd_am_curve(d, p$type, p$rel)
          r1 <- cv[cv[, 1] < p$gap, , drop = FALSE]
          r2 <- cv[cv[, 1] >= p$gap, , drop = FALSE]
          if (nrow(r1) < 3 || nrow(r2) < 3) {
            stop("the tracked curve must extend on both sides of the gap depth", call. = FALSE)
          }
          s <- cd_clean(d2)
          pmin <- p$band_lo * p$missing
          pmax <- p$band_hi * p$missing
          # the serial branch of dur_gaps() mixes periods and frequencies when drawing the curves; the parallel branch
          # with one core is used, which runs the same simulations correctly in this session
          r <- WaverideR::dur_gaps(proxies = list(s), retracked_period_1 = r1, retracked_period_2 = r2, min_max = p$min_max,
              n_simulations = p$n_sim, tracked_cycle_period = p$cycle, tracked_cycle_period_unc = p$cycle_unc,
              tracked_cycle_period_unc_dist = p$cycle_dist, pts = p$pts, dj = p$dj, lowerPeriod = pmin / 2,
              upperPeriod = pmax * 2, period_max = pmax, period_min = pmin, missing_cycle_dur = p$missing,
              n_cycles_missing = p$n_missing, missing_cycle_unc = p$missing_unc, missing_cycle_unc_dist = p$cycle_dist,
              seed_nr = p$seed, run_multicore = TRUE, n_cores = 1)
          g <- as.numeric(r)
          g <- g[is.finite(g)]
          if (!length(g)) {
            stop("no finite gap duration: check the band of the extracted cycle and the peak detection", call. = FALSE)
          }
          tab <- data.frame(simulation = seq_along(g), gap_kyr = g)
          o <- list(cd_output(cd_out_name(file2, sprintf("-gap%s", cd_fmt(p$gap))), tab, header = TRUE, var = "res"))
          code <- c(cd_code_read(file2, var = "dat2"), "dat2 <- dat2[stats::complete.cases(dat2[, 1:2]), 1:2]",
              cd_am_curve_code(p$type, p$rel),
              sprintf("r1 <- curve[curve[, 1] < %s, ]", cd_fmt(p$gap)), sprintf("r2 <- curve[curve[, 1] >= %s, ]", cd_fmt(p$gap)),
              sprintf(paste0("g <- WaverideR::dur_gaps(proxies = list(dat2), retracked_period_1 = r1, retracked_period_2 = r2,\n",
                  "    min_max = %s, n_simulations = %d, tracked_cycle_period = %s, tracked_cycle_period_unc = %s,\n",
                  "    tracked_cycle_period_unc_dist = %s, pts = %d, dj = %s, lowerPeriod = %s, upperPeriod = %s,\n",
                  "    period_max = %s, period_min = %s, missing_cycle_dur = %s, n_cycles_missing = %s,\n",
                  "    missing_cycle_unc = %s, missing_cycle_unc_dist = %s, seed_nr = %d,\n",
                  "    run_multicore = TRUE, n_cores = 1)  # the serial branch mixes periods and frequencies"),
                  cd_dep(p$min_max), as.integer(p$n_sim), cd_fmt(p$cycle), cd_fmt(p$cycle_unc), cd_dep(p$cycle_dist),
                  as.integer(p$pts), cd_fmt(p$dj), cd_fmt(pmin / 2), cd_fmt(pmax * 2), cd_fmt(pmax), cd_fmt(pmin),
                  cd_fmt(p$missing), cd_fmt(p$n_missing), cd_fmt(p$missing_unc), cd_dep(p$cycle_dist), as.integer(p$seed)),
              "res <- data.frame(simulation = seq_along(g), gap_kyr = as.numeric(g))", cd_code_write(o))
          qs <- stats::quantile(g, c(0.025, 0.5, 0.975), names = FALSE)
          list(result = list(g = g, s = s, gap = p$gap), outputs = o, code = code,
              text = c(sprintf("Gap at %s: median %s kyr, mean %s kyr, 95%% range %s to %s kyr (%d simulations)", cd_fmt(p$gap),
                  cd_fmt(signif(qs[2], 4)), cd_fmt(signif(mean(g), 4)), cd_fmt(signif(qs[1], 4)), cd_fmt(signif(qs[3], 4)),
                  length(g)),
                  if (any(g < 0)) { "Negative durations: no cycle seems to be missing at this depth." } else { character() }))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$s, col = "#1f4e79", xlab = "Depth", ylab = "Proxy")
          graphics::abline(v = res$gap, col = "#c0392b", lwd = 2, lty = 2)
          cd_par()
          graphics::hist(res$g, breaks = max(5, min(30, length(res$g))), col = grDevices::adjustcolor("#1f4e79", 0.5),
              border = "white", main = "", xlab = "Gap duration (kyr)", ylab = "Simulations")
          graphics::abline(v = stats::median(res$g), col = "#c0392b", lwd = 2)
        }),
    cd_tool("am_bchron", M, "U-Pb age-depth model (modBchron)", group = "Bayesian age-depth models", full_table = TRUE,
        heavy = TRUE, icon = "chart-line",
        info = paste("Bayesian age-depth model of radioisotopic dates alone (modified Bchron; Trayler et al., 2020). The",
            "file is a table with a header: position (depth or height), age, sd (1 sigma, or a 2sd column) and optionally",
            "thickness; rows at the same position are combined into one dated level."),
        uses = "WaverideR::modBchron, WaverideR::modBchron_predict",
        params = list(
          cd_sel("ptype", "Positions are", c("Depths (increasing downwards, older)" = "depth",
              "Heights (increasing upwards, younger)" = "height")),
          cd_num("MC", "Number of MCMC iterations", 2000, min = 200, step = 500),
          cd_num("burn", "Burn-in iterations", 500, min = 50, step = 100),
          cd_num("probability", "Probability of the interval", 0.95, min = 0.5, max = 0.999, step = 0.01),
          cd_num("n_pred", "Number of positions of the model", 200, min = 20, step = 50),
          cd_txt("predict", "Also predict the age at positions (optional)", "", numeric = TRUE),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          dates <- cd_am_dates(d)
          sg <- if (p$ptype == "depth") { -1 } else { 1 }
          ids <- format(dates$position, trim = TRUE)
          pos <- sg * dates$position
          pp <- seq(min(pos), max(pos), length.out = p$n_pred)
          set.seed(p$seed)
          mb <- NULL
          utils::capture.output(mb <- suppressWarnings(WaverideR::modBchron(ages = dates$age, ageSds = dates$sd, positions = pos,
              positionThicknesses = dates$thickness, ids = ids, MC = p$MC, burn = p$burn, probability = p$probability,
              predictPositions = pp)))
          hdi <- as.matrix(mb$HDI)
          tab <- data.frame(position = sg * mb$predictPositions, lower = hdi[1, ], median = hdi[2, ], upper = hdi[3, ])
          tab <- tab[order(tab$position), ]
          o <- list(cd_output(cd_out_name(file, "-bchron"), tab, header = TRUE, var = "res"),
              cd_output(cd_out_name(file, "-bchron-agemodel"), tab[, c("position", "median")], var = "am"))
          code <- c(attr(dates, "code"), sprintf("set.seed(%d)", as.integer(p$seed)),
              sprintf(paste0("mb <- WaverideR::modBchron(ages = dates$age, ageSds = dates$sd, positions = %sdates$position,\n",
                  "    positionThicknesses = dates$thickness, ids = format(dates$position, trim = TRUE), MC = %d, burn = %d,\n",
                  "    probability = %s, predictPositions = seq(%s, %s, length.out = %d))"), if (sg < 0) { "-" } else { "" },
                  as.integer(p$MC), as.integer(p$burn), cd_fmt(p$probability), cd_fmt(min(pos)), cd_fmt(max(pos)),
                  as.integer(p$n_pred)),
              sprintf("res <- data.frame(position = %smb$predictPositions, lower = mb$HDI[1, ], median = mb$HDI[2, ], upper = mb$HDI[3, ])",
                  if (sg < 0) { "-" } else { "" }),
              "res <- res[order(res$position), ]", "am <- res[, c(\"position\", \"median\")]")
          pred <- NULL
          if (length(p$predict)) {
            np <- p$predict[p$predict * sg >= min(pos) & p$predict * sg <= max(pos)]
            if (length(np)) {
              pr <- WaverideR::modBchron_predict(mb, newPositions = sg * np, ids = format(np, trim = TRUE),
                  probability = p$probability)
              h <- pr$HDI
              pred <- data.frame(position = np, lower = h[, 2], median = h[, 3], upper = h[, 4])
              o[[3]] <- cd_output(cd_out_name(file, "-bchron-predict"), pred, header = TRUE, var = "pred")
              code <- c(code, sprintf("pr <- WaverideR::modBchron_predict(mb, newPositions = %s%s, probability = %s)",
                  if (sg < 0) { "-" } else { "" }, cd_dep(np), cd_fmt(p$probability)),
                  sprintf("pred <- data.frame(position = %s, lower = pr$HDI[, 2], median = pr$HDI[, 3], upper = pr$HDI[, 4])",
                      cd_dep(np)))
            }
          }
          code <- c(code, cd_code_write(o))
          curves <- cd_am_thin(mb$model[, -seq_len(min(p$burn, ncol(mb$model) - 1)), drop = FALSE], 40)
          out <- list(tab = tab, curves = curves, cpos = sg * mb$predictPositions, dates = dates, outliers = mb$outliers,
              ids = mb$ids, pred = pred, ptype = p$ptype)
          nout <- sum(mb$outliers)
          list(result = out, outputs = o, code = code,
              text = c(sprintf("%d dated levels; %s %% interval width: median %s, largest %s (age units)", length(mb$ids),
                  cd_fmt(100 * p$probability), cd_fmt(signif(stats::median(tab$upper - tab$lower), 3)),
                  cd_fmt(signif(max(tab$upper - tab$lower), 3))),
                  if (nout) { sprintf("%d date(s) do not overlap the model (possible outliers): %s", nout,
                      paste(mb$ids[mb$outliers], collapse = ", ")) } else { character() }))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          cd_par()
          t <- res$tab
          dts <- res$dates
          yl <- range(c(t$lower, t$upper, dts$age - 2 * dts$sd, dts$age + 2 * dts$sd), finite = TRUE)
          graphics::plot(t$position, t$median, type = "n", ylim = rev(yl), xlab = if (res$ptype == "depth") { "Depth" } else { "Height" },
              ylab = "Age")
          graphics::grid(col = "grey90", lty = 1)
          cd_am_band(t$position, t$lower, t$upper, alpha = 0.3)
          o <- order(res$cpos)
          graphics::matlines(res$cpos[o], res$curves[o, , drop = FALSE], col = grDevices::adjustcolor("grey30", 0.2), lty = 1,
              lwd = 0.6)
          graphics::lines(t$position, t$median, col = "#1f4e79", lwd = 1.8)
          graphics::arrows(dts$position, dts$age - 2 * dts$sd, dts$position, dts$age + 2 * dts$sd, angle = 90, code = 3,
              length = 0.03, col = "#c0392b", lwd = 1.3)
          out <- format(dts$position, trim = TRUE) %in% res$ids[res$outliers]
          graphics::points(dts$position, dts$age, pch = 21, bg = ifelse(out, "white", "#c0392b"), col = "#c0392b")
          if (!is.null(res$pred)) {
            graphics::arrows(res$pred$position, res$pred$lower, res$pred$position, res$pred$upper, angle = 90, code = 3,
                length = 0.03, col = "#2e8b57", lwd = 1.3)
            graphics::points(res$pred$position, res$pred$median, pch = 23, bg = "#2e8b57", col = "white")
          }
          graphics::legend("topleft", bty = "n", cex = 0.8, legend = c("median", sprintf("%s%% interval", cd_fmt(100 * p$probability)),
              "dates (2 sd; open = outlier)", if (!is.null(res$pred)) { "predicted" } else { NULL }),
              col = c("#1f4e79", grDevices::adjustcolor("#1f4e79", 0.4), "#c0392b", if (!is.null(res$pred)) { "#2e8b57" } else { NULL }),
              lwd = c(1.8, 8, NA, if (!is.null(res$pred)) { NA } else { NULL }),
              pch = c(NA, NA, 21, if (!is.null(res$pred)) { 23 } else { NULL }), pt.bg = c(NA, NA, "#c0392b", "#2e8b57"))
        }),
    cd_tool("am_astrobchron", M, "Dates + astrochronology (astroMCBchron, 2 files)", group = "Bayesian age-depth models",
        inputs = 2L, full_table = TRUE, heavy = TRUE, icon = "layer-group",
        info = paste("Joint Bayesian age-depth model of radioisotopic dates (file 1: table with position, age, sd and",
            "optionally thickness, or single-zircon dates) and a floating astrochronology (file 2: tracked period of the",
            "cycle, depth and period). Estimates hiatus durations, outlier probabilities and, optionally, compares with",
            "the dates-only modBchron model."),
        uses = "WaverideR::astroMCBchron, WaverideR::upb_grain_loglik, WaverideR::modBchron",
        params = list(
          cd_sel("date_type", "File 1 holds", c("One row per dated level (position, age, sd)" = "dates",
              "Single-zircon dates (one row per grain; grains at one position form a level)" = "grains")),
          cd_num("D_max", "Longest crystallisation interval before eruption (kyr)", 600, min = 0,
              show_if = list(param = "date_type", values = "grains")),
          cd_sel("ptype", "Positions are", c("Depths (increasing downwards, older)" = "depth",
              "Heights (increasing upwards, younger)" = "height")),
          cd_sel("age_unit", "Unit of the ages", c("Ma" = "Ma", "ka (kyr)" = "ka")),
          cd_num("target_period", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_num("track_unc", "Relative uncertainty of the tracked period (1 sd, %)", 3, min = 0),
          cd_txt("hiatus", "Positions of hiatuses (empty = none)", "", numeric = TRUE),
          cd_txt("hiatus_mean", "Prior durations of the hiatuses (kyr)", "", numeric = TRUE),
          cd_txt("hiatus_sd", "Prior uncertainties of the hiatuses (1 sd, kyr)", "", numeric = TRUE),
          cd_sel("date_model", "Outlier model of the dates", c("Mixture with outlier probability" = "mixture",
              "Two-stage screening" = "two_stage", "Gaussian (no outliers)" = "gaussian")),
          cd_sel("outlier_type", "Outliers can be", c("Too old or too young" = "scaled", "Too old (inheritance)" = "old",
              "Too young (Pb loss)" = "young"), show_if = list(param = "date_model", values = "mixture")),
          cd_num("n_iter", "Number of MCMC iterations", 10000, min = 1000, step = 5000),
          cd_num("thin", "Thinning interval", 20, min = 1, step = 5),
          cd_num("n_tracks", "Track realisations per segment", 200, min = 10, step = 50),
          cd_chk("loo", "Leave-one-out test of every date (one extra run per date, slow)", FALSE),
          cd_chk("bchron", "Also run the dates-only modBchron model", FALSE),
          cd_num("bchron_mc", "modBchron iterations", 2000, min = 200, step = 500, show_if = list(param = "bchron", values = "true")),
          cd_txt("predict", "Also predict the age at positions (optional)", "", numeric = TRUE),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          dates <- cd_am_dates(d)
          code <- c(attr(dates, "code"), cd_code_read(file2, var = "dat2"), "dat2 <- dat2[stats::complete.cases(dat2[, 1:2]), 1:2]",
              sprintf("set.seed(%d)", as.integer(p$seed)))
          set.seed(p$seed)
          loglik <- NULL
          o <- list()
          if (identical(p$date_type, "grains")) {
            uf <- if (p$age_unit == "Ma") { 1 } else { 1000 }
            grains <- data.frame(id = format(dates$position, trim = TRUE), age = dates$age, sd = dates$sd,
                stringsAsFactors = FALSE)
            gl <- WaverideR::upb_grain_loglik(grains, D_max = p$D_max / 1000 * uf, W_inherit = 20 * uf, W_pbloss = 3 * uf,
                step = 0.002 * uf)
            gs <- gl$summary
            lev <- unique(grains$id)
            pos <- numeric(length(lev))
            th <- numeric(length(lev))
            loglik <- vector("list", length(lev))
            for (k in seq_along(lev)) {
              pos[k] <- dates$position[match(lev[k], grains$id)]
              th[k] <- dates$thickness[match(lev[k], grains$id)]
              loglik[[k]] <- gl$loglik[[lev[k]]]
            }
            m <- match(lev, as.character(gs$id))
            dates <- data.frame(id = paste0("z", lev), position = pos, age = gs$te_mean[m], sd = gs$te_2sd[m] / 2, thickness = th)
            eru <- data.frame(position = pos, gs[m, setdiff(names(gs), "id")])
            o[[1]] <- cd_output(cd_out_name(file, "-eruption-ages"), eru, header = TRUE, var = "eru")
            code <- c(code, "grains <- data.frame(id = format(dates$position, trim = TRUE), age = dates$age, sd = dates$sd)",
                sprintf("gl <- WaverideR::upb_grain_loglik(grains, D_max = %s, W_inherit = %s, W_pbloss = %s, step = %s)",
                    cd_fmt(p$D_max / 1000 * uf), cd_fmt(20 * uf), cd_fmt(3 * uf), cd_fmt(0.002 * uf)),
                "lev <- unique(grains$id)", "m <- match(lev, as.character(gl$summary$id))",
                paste0("dates <- data.frame(id = paste0(\"z\", lev), position = dates$position[match(lev, grains$id)],",
                    " age = gl$summary$te_mean[m], sd = gl$summary$te_2sd[m] / 2,",
                    " thickness = dates$thickness[match(lev, grains$id)])"),
                "loglik <- gl$loglik[lev]", "eru <- data.frame(position = dates$position, gl$summary[m, -1])")
          } else {
            dates$id <- paste0("date_", seq_len(nrow(dates)))
            dates <- dates[, c("id", "position", "age", "sd", "thickness")]
            code <- c(code, "dates$id <- paste0(\"date_\", seq_len(nrow(dates)))", "loglik <- NULL")
          }
          if (anyDuplicated(dates$position)) {
            stop("two dates share a position: give one row per dated level, or choose single-zircon dates", call. = FALSE)
          }
          tc <- cd_clean(d2)
          if (any(tc$y <= 0)) {
            stop("the tracked periods (file 2, column 2) must be positive", call. = FALSE)
          }
          nreal <- 50
          rel <- p$track_unc / 100
          per <- matrix(tc$y, nrow(tc), nreal)
          for (i in seq_len(nreal)) {
            per[, i] <- tc$y * exp(stats::rnorm(1, 0, rel))
          }
          code <- c(code, sprintf("per <- matrix(dat2[, 2], nrow(dat2), %d)", nreal), sprintf("for (i in 1:%d) {", nreal),
              sprintf("  per[, i] <- dat2[, 2] * exp(rnorm(1, 0, %s))", cd_fmt(rel)), "}")
          hia <- NULL
          if (length(p$hiatus)) {
            nh <- length(p$hiatus)
            hia <- data.frame(position = p$hiatus, mean = cd_am_rep(p$hiatus_mean, nh, "the hiatus durations"),
                sd = cd_am_rep(p$hiatus_sd, nh, "the hiatus uncertainties"))
            # one track segment per continuous interval, from the youngest to the oldest
            cuts <- sort(p$hiatus, decreasing = p$ptype == "height")
            seg <- findInterval(if (p$ptype == "depth") { tc$x } else { -tc$x }, if (p$ptype == "depth") { cuts } else { -cuts })
            tracks <- list()
            for (k in sort(unique(seg))) {
              w <- seg == k
              tracks[[length(tracks) + 1]] <- list(position = tc$x[w], period = per[w, , drop = FALSE])
            }
            if (length(tracks) != nh + 1) {
              stop("the track must have points in every interval between the hiatuses", call. = FALSE)
            }
            code <- c(code, sprintf("hiatuses <- data.frame(position = %s, mean = %s, sd = %s)", cd_dep(hia$position),
                cd_dep(hia$mean), cd_dep(hia$sd)),
                sprintf("seg <- findInterval(%sdat2[, 1], %s)", if (p$ptype == "depth") { "" } else { "-" },
                    cd_dep(if (p$ptype == "depth") { cuts } else { -cuts })),
                "tracks <- list()", "for (k in sort(unique(seg))) {",
                "  tracks[[length(tracks) + 1]] <- list(position = dat2[seg == k, 1], period = per[seg == k, , drop = FALSE])", "}")
          } else {
            tracks <- list(position = tc$x, period = per)
            code <- c(code, "hiatuses <- NULL", "tracks <- list(position = dat2[, 1], period = per)")
          }
          fit <- NULL
          utils::capture.output(fit <- suppressWarnings(WaverideR::astroMCBchron(dates, tracks = tracks, hiatuses = hia,
              position_type = p$ptype, age_unit = p$age_unit, target_period = p$target_period, date_model = p$date_model,
              outlier_type = p$outlier_type, n_iter = p$n_iter, thin = p$thin, n_tracks = p$n_tracks, loo = p$loo,
              max_curves = 1000, date_loglik = loglik, bchron = p$bchron, bchron_args = list(MC = p$bchron_mc,
                  burn = round(p$bchron_mc / 4)), seed = p$seed, verbose = FALSE)))
          code <- c(code, sprintf(paste0("fit <- WaverideR::astroMCBchron(dates, tracks = tracks, hiatuses = hiatuses,",
              " position_type = %s, age_unit = %s,\n    target_period = %s, date_model = %s, outlier_type = %s, n_iter = %d,",
              " thin = %d, n_tracks = %d, loo = %s,\n    max_curves = 1000, date_loglik = loglik, bchron = %s,",
              " bchron_args = list(MC = %d, burn = %d), seed = %d)"), cd_dep(p$ptype), cd_dep(p$age_unit),
              cd_fmt(p$target_period), cd_dep(p$date_model), cd_dep(p$outlier_type), as.integer(p$n_iter), as.integer(p$thin),
              as.integer(p$n_tracks), p$loo, p$bchron, as.integer(p$bchron_mc), as.integer(round(p$bchron_mc / 4)),
              as.integer(p$seed)), "plot(fit)")
          summ <- fit$summary
          dt <- as.data.frame(fit$dates)
          am <- data.frame(position = summ$position, median = summ$median)
          o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-astroMCBchron"), summ, header = TRUE, var = "summ")
          o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-astroMCBchron-agemodel"), am, var = "am")
          o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-astroMCBchron-dates"), dt, header = TRUE, var = "dt")
          code <- c(code, "summ <- fit$summary", "am <- data.frame(position = summ$position, median = summ$median)",
              "dt <- as.data.frame(fit$dates)")
          if (length(p$predict)) {
            np <- p$predict[p$predict >= min(fit$predictPositions) & p$predict <= max(fit$predictPositions)]
            if (length(np)) {
              pr <- stats::predict(fit, newPositions = np)
              o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-astroMCBchron-predict"), pr$HDI, header = TRUE, var = "pred")
              code <- c(code, sprintf("pred <- predict(fit, newPositions = %s)$HDI", cd_dep(np)))
            }
          }
          code <- c(code, cd_code_write(o))
          fit$posterior <- NULL
          fit$model <- cd_am_thin(fit$model, 200)
          if (!is.null(fit$bchron)) {
            fit$bchron <- list(summary = fit$bchron$summary)
          }
          w <- if (is.null(summ$sd2_kyr)) { summ$upper - summ$lower } else { 2 * summ$sd2_kyr }
          nout <- if (is.null(dt$outlier)) { 0 } else { sum(dt$outlier, na.rm = TRUE) }
          list(result = fit, outputs = o, code = code,
              text = c(sprintf("%d dated levels, %d hiatus(es); 95%% range of the age: median %s kyr, largest %s kyr",
                  nrow(dt), if (is.null(hia)) { 0L } else { nrow(hia) }, cd_fmt(signif(stats::median(w), 3)),
                  cd_fmt(signif(max(w), 3))),
                  if (nout) { sprintf("%d date(s) flagged as outliers: %s", nout, paste(dt$id[dt$outlier], collapse = ", ")) } else {
                    character() },
                  if (NCOL(fit$hiatus)) { sprintf("Hiatus duration(s): %s kyr (posterior median)",
                      paste(cd_fmt_period(cd_am_colmed(fit$hiatus)), collapse = ", ")) } else { character() }))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::plot(res)
        }),
    cd_tool("am_check", M, "Check an age model (de-chirped record, 2 files)", group = "Checks and models", inputs = 2L,
        heavy = TRUE, icon = "magnifying-glass-chart",
        info = paste("De-chirps the record (file 1) with an existing age model (file 2: a tracked period curve, a",
            "sedimentation-rate curve or depth-age points): in the time of the age model every astronomical cycle should",
            "be a horizontal line; intervals where the cycles bend away are flagged with the rate correction they ask for."),
        uses = "WaverideR::as_wr_track, WaverideR::check_agemodel, WaverideR::plot_agemodel_check",
        params = list(
          cd_sel("type", "File 2 holds", c("Tracked period of one cycle (depth, period)" = "period",
              "Sedimentation rate (depth, rate)" = "sedrate", "Age model (depth, age)" = "age")),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0, show_if = list(param = "type", values = "period")),
          cd_sel("rate_factor", "Unit of the rate", c("cm/kyr with depth in metres" = "0.01", "depth units per kyr" = "1",
              "m/Myr with depth in metres" = "0.001"), show_if = list(param = "type", values = "sedrate")),
          cd_sel("age_factor", "Unit of the ages", c("kyr" = "1", "Ma" = "1000"), show_if = list(param = "type", values = "age")),
          cd_num("age_ma", "Approximate age of the record (Ma, picks the cycles)", 0, min = 0),
          cd_txt("astro_periods", "Cycles to check (kyr; empty = eccentricity, obliquity, precession)", "", numeric = TRUE),
          cd_chk("tf_wavelet", "Transform: Morlet wavelet", TRUE),
          cd_chk("tf_eha", "Transform: multitaper EHA", TRUE),
          cd_chk("tf_superlet", "Transform: superlet SST (slower)", FALSE),
          cd_chk("tf_chirplet", "Transform: chirplet SST (slow)", FALSE),
          cd_num("tolerance", "Rate correction that is flagged (0.1 = 10 %)", 0.1, min = 0.01, step = 0.05),
          cd_num("min_cycles", "Cycles that must agree", 2, min = 1, step = 1),
          cd_chk("retrack", "Also run a blind re-track for comparison (slower)", FALSE),
          cd_sel("view", "Horizontal axis of the maps", c("Time of the age model" = "tuned", "Depth" = "depth")),
          cd_sel("palette", "Colour palette", dc_palettes)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          x <- cd_clean(d2)
          rf <- switch(p$type, sedrate = as.numeric(p$rate_factor), age = as.numeric(p$age_factor), 1)
          trk <- WaverideR::as_wr_track(x, type = p$type, cycle = p$cycle, rate_factor = rf)
          tf <- c("wavelet", "eha", "superlet_sst", "chirplet_sst")[c(isTRUE(p$tf_wavelet), isTRUE(p$tf_eha),
              isTRUE(p$tf_superlet), isTRUE(p$tf_chirplet))]
          if (!length(tf)) {
            stop("select at least one transform", call. = FALSE)
          }
          ap <- if (length(p$astro_periods) >= 2) { p$astro_periods } else { NULL }
          mt <- min(2L, length(tf))
          chk <- WaverideR::check_agemodel(data = s, track = trk, age_ma = p$age_ma, astro_periods = ap, transforms = tf,
              tolerance = p$tolerance, min_cycles = p$min_cycles, min_transforms = mt, retrack = p$retrack, verbose = FALSE)
          prof <- as.data.frame(chk$profile)
          iv <- as.data.frame(chk$intervals)
          o <- list(cd_output(cd_out_name(file, "-agecheck-profile"), prof, header = TRUE, var = "prof"))
          if (nrow(iv)) {
            o[[2]] <- cd_output(cd_out_name(file, "-agecheck-intervals"), iv, header = TRUE, var = "iv")
          }
          code <- c(cd_code_read(file2, var = "dat2"), "dat2 <- dat2[stats::complete.cases(dat2[, 1:2]), 1:2]",
              sprintf("track <- WaverideR::as_wr_track(dat2, type = %s, cycle = %s, rate_factor = %s)", cd_dep(p$type),
                  cd_fmt(p$cycle), cd_fmt(rf)),
              sprintf(paste0("chk <- WaverideR::check_agemodel(data = dat[, 1:2], track = track, age_ma = %s, astro_periods = %s,\n",
                  "    transforms = %s, tolerance = %s, min_cycles = %d, min_transforms = %d, retrack = %s)"), cd_fmt(p$age_ma),
                  if (is.null(ap)) { "NULL" } else { cd_dep(ap) }, cd_dep(tf), cd_fmt(p$tolerance), as.integer(p$min_cycles), mt,
                  p$retrack),
              sprintf("WaverideR::plot_agemodel_check(chk, view = %s, palette = %s)", cd_dep(p$view), cd_dep(p$palette)),
              "prof <- as.data.frame(chk$profile)", if (nrow(iv)) { "iv <- as.data.frame(chk$intervals)" } else { character() },
              cd_code_write(o))
          txt <- if (nrow(iv)) {
            c(sprintf("%d flagged interval(s):", nrow(iv)), sprintf("  %s to %s: rate correction x %s (%s cycles, %s transforms)",
                cd_fmt_period(iv$from), cd_fmt_period(iv$to), cd_fmt_period(iv$correction), iv$cycles, iv$transforms))
          } else {
            "No interval flagged: the cycles stay at their expected periods in the time of the age model."
          }
          list(result = cd_am_slim_check(chk), outputs = o, code = code,
              text = c(sprintf("Cycles checked (kyr): %s", paste(cd_fmt_period(chk$astro_periods), collapse = ", ")), txt))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_agemodel_check(res, view = p$view, palette = p$palette)
        }),
    cd_tool("am_sedramp", M, "Exponential sed.-rate ramp (time to depth)", group = "Checks and models", icon = "arrow-trend-up",
        info = paste("Converts a time series (time in kyr, value), e.g. an ETP or insolation series, to a synthetic",
            "stratigraphic record whose sedimentation rate changes exponentially from the start to the end value; use it",
            "to test how tracking and age-model tools cope with a changing rate."),
        uses = "WaverideR::expSedRamp",
        params = list(
          cd_num("sr_start", "Sedimentation rate at the start of the series (cm/kyr)", 1, min = 0),
          cd_num("sr_end", "Sedimentation rate at the end of the series (cm/kyr)", 5, min = 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          if (!(p$sr_start > 0) || !(p$sr_end > 0)) {
            stop("the sedimentation rates must be positive", call. = FALSE)
          }
          r <- WaverideR::expSedRamp(s, sr_start = p$sr_start, sr_end = p$sr_end)
          dep <- as.data.frame(r[[1]])
          names(dep) <- c("depth_m", names(d)[2])
          sr <- as.data.frame(r[[2]])
          names(sr) <- c("depth_m", "sedrate_cm_kyr")
          sfx <- sprintf("-ramp%s-%s", cd_fmt(p$sr_start), cd_fmt(p$sr_end))
          o <- list(cd_output(cd_out_name(file, sfx), dep, var = "dep"),
              cd_output(cd_out_name(file, paste0(sfx, "-sedrate")), sr, var = "sr"))
          code <- c(sprintf("r <- WaverideR::expSedRamp(dat[, 1:2], sr_start = %s, sr_end = %s)", cd_fmt(p$sr_start),
              cd_fmt(p$sr_end)), "dep <- r[[1]]", "sr <- r[[2]]", cd_code_write(o))
          list(result = list(s = s, dep = dep, sr = sr), outputs = o, code = code,
              text = sprintf("%d points, %s m of synthetic section", nrow(dep), cd_fmt(signif(diff(range(dep[, 1])), 4))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE))
          cd_par()
          cd_plot_one(res$s, col = "#7f8c8d", xlab = "Time (kyr)", ylab = "Input")
          cd_par()
          cd_plot_one(res$dep, col = "#1f4e79", xlab = "Depth (m)", ylab = "Synthetic record")
          cd_par()
          graphics::plot(res$sr[, 1], res$sr[, 2], type = "l", col = "#2e8b57", lwd = 1.5, xlab = "Depth (m)",
              ylab = "Sed. rate (cm/kyr)")
          graphics::grid(col = "grey90", lty = 1)
        })
  )
}

# Column medians of the posterior hiatus durations
cd_am_colmed <- function(m) {
  m <- as.matrix(m)
  out <- numeric(ncol(m))
  for (j in seq_len(ncol(m))) {
    out[j] <- stats::median(m[, j], na.rm = TRUE)
  }
  out
}
