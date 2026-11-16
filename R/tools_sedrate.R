# Tools: sedrate ####
#
# Sedimentation-rate estimation, tracking and astronomical testing with WaverideR (menu "Sed. rate"). Every tool that
# gives a sedimentation-rate curve writes it as a two-column file (depth, sed. rate in cm/kyr) that can be used in
# Time Series > Sedimentation rate to age model, and where natural also an age model (depth, time in kyr).

# Helpers ####

cd_sr_units <- function() {
  c("metres" = "100", "centimetres" = "1", "feet" = "30.48")
}

cd_sr_unit_param <- function() {
  cd_sel("unit", "Depth unit of the data", cd_sr_units(),
      help = "Sedimentation rates are given in cm/kyr; the depth unit converts them to the depth unit of the data.")
}

# Astronomical periods: at the age of the record (AstroGeo22) or an own list
cd_sr_period_params <- function(own = "405, 125, 95, 41, 23.7, 22.4, 19") {
  list(
    cd_sel("periods", "Astronomical periods", c("At the age of the record (AstroGeo22)" = "age", "Own list (kyr)" = "list")),
    cd_num("age_ma", "Age of the record (Ma)", 0, min = 0, show_if = list(param = "periods", values = "age"),
        help = "Obliquity and precession periods at this age (lunar recession), from WaverideR::astro_periods_tracking."),
    cd_txt("astro_list", "Periods (kyr)", own, numeric = TRUE, show_if = list(param = "periods", values = "list")))
}

cd_sr_astro <- function(p) {
  if (identical(p$periods, "list")) {
    per <- p$astro_list
    per <- per[is.finite(per) & per > 0]
    if (!length(per)) {
      stop("give at least one astronomical period (kyr)", call. = FALSE)
    }
    return(list(period = per, table = NULL, arg = cd_dep(per), code = character()))
  }
  ap <- WaverideR::astro_periods_tracking(age_ma = p$age_ma)
  list(period = ap$period, table = ap, arg = "periods",
      code = sprintf("periods <- WaverideR::astro_periods_tracking(age_ma = %s)", cd_fmt(p$age_ma)))
}

# The nine COCO periods (5 eccentricity, 1 obliquity, 3 precession, descending) at an age
cd_sr_orbit9 <- function(age_ma) {
  ap <- WaverideR::astro_periods_tracking(age_ma = age_ma)
  ecc <- ap$period[ap$family == "eccentricity"]
  obl <- ap[ap$family == "obliquity", ]
  pre <- ap[ap$family == "precession", ]
  ob <- obl$period[order(-obl$weight)][1]
  pr <- pre$period[order(-pre$weight)][seq_len(min(3, nrow(pre)))]
  sort(c(ecc[order(-ecc)][1:5], ob, pr), decreasing = TRUE)
}

cd_sr_orbit_params <- function() {
  list(
    cd_sel("orbit", "Astronomical target periods", c("Acycle default (La2004, nine periods)" = "default",
        "At the age of the record (AstroGeo22)" = "age", "Own nine periods (kyr)" = "list")),
    cd_num("age_ma", "Age of the record (Ma)", 0, min = 0, show_if = list(param = "orbit", values = "age")),
    cd_txt("orbit_list", "Nine periods (kyr, descending)", "405.7, 130.7, 123.9, 98.9, 94.9, 41, 23.7, 22.4, 19",
        numeric = TRUE, show_if = list(param = "orbit", values = "list")))
}

cd_sr_orbit <- function(p) {
  if (identical(p$orbit, "age")) {
    o <- cd_sr_orbit9(p$age_ma)
    return(list(orbit = signif(o, 7), arg = cd_dep(signif(o, 7))))
  }
  if (identical(p$orbit, "list")) {
    o <- p$orbit_list
    if (length(o) != 9) {
      stop("give exactly nine target periods (kyr)", call. = FALSE)
    }
    o <- sort(o, decreasing = TRUE)
    return(list(orbit = o, arg = cd_dep(o)))
  }
  o <- c(405.6912, 130.6979, 123.8532, 98.8517, 94.8856, 40.9897, 23.682, 22.3758, 18.9519)
  list(orbit = o, arg = cd_dep(o))
}

# Period range of a scalogram that holds the cycles at all rates (data units)
cd_sr_period_range <- function(s, per, sr, max_frac = 0.5, max_period = Inf) {
  dt <- cd_dt(s$x)
  len <- diff(range(s$x))
  lo <- max(3 * dt, 0.7 * min(per) * sr[1])
  hi <- min(max_frac * len, max_period, 1.5 * max(per) * sr[2])
  if (!(hi > 1.5 * lo)) {
    stop("no astronomical cycle fits into the record at these sedimentation rates: check the rates and the depth unit",
        call. = FALSE)
  }
  signif(c(lo, hi), 4)
}

# Scalogram of a chosen type, with the equivalent code
cd_sr_scalogram <- function(s, type, lim, omega_nr = 12, dj = 0.02) {
  dm <- as.matrix(s)
  if (type == "sst") {
    w <- WaverideR::analyse_wavelet_sst(data = dm, dj = dj, lowerPeriod = lim[1], upperPeriod = lim[2], omega_nr = omega_nr)
    code <- sprintf("w <- WaverideR::analyse_wavelet_sst(data = as.matrix(dat), dj = %s, lowerPeriod = %s, upperPeriod = %s, omega_nr = %s)",
        cd_fmt(dj), cd_fmt(lim[1]), cd_fmt(lim[2]), cd_fmt(omega_nr))
  } else if (type == "superlet_sst") {
    w <- WaverideR::analyse_superlet_sst(data = dm, lowerPeriod = lim[1], upperPeriod = lim[2], dj = dj)
    code <- sprintf("w <- WaverideR::analyse_superlet_sst(data = as.matrix(dat), lowerPeriod = %s, upperPeriod = %s, dj = %s)",
        cd_fmt(lim[1]), cd_fmt(lim[2]), cd_fmt(dj))
  } else {
    w <- WaverideR::analyse_wavelet(data = dm, dj = dj, lowerPeriod = lim[1], upperPeriod = lim[2], omega_nr = omega_nr,
        verbose = FALSE)
    code <- sprintf("w <- WaverideR::analyse_wavelet(data = as.matrix(dat), dj = %s, lowerPeriod = %s, upperPeriod = %s, omega_nr = %s, verbose = FALSE)",
        cd_fmt(dj), cd_fmt(lim[1]), cd_fmt(lim[2]), cd_fmt(omega_nr))
  }
  list(w = w, code = code)
}

# Age (kyr) by integrating a sedimentation rate (cm/kyr) along depth (unit: cm per depth unit)
cd_sr_age <- function(x, sr_cm, u = 100) {
  n <- length(x)
  tt <- numeric(n)
  if (n < 2) {
    return(tt)
  }
  for (i in 2:n) {
    r <- 0.5 * (sr_cm[i] + sr_cm[i - 1])
    tt[i] <- tt[i - 1] + (x[i] - x[i - 1]) * u / r
  }
  tt
}

cd_sr_age_code <- function(u = 100) {
  sprintf("am <- data.frame(depth = sr[, 1], time_kyr = c(0, cumsum(diff(sr[, 1]) * %s / (0.5 * (sr[-1, 2] + sr[-nrow(sr), 2])))))",
      cd_fmt(u))
}

# Sedimentation-rate and age-model outputs
cd_sr_outputs <- function(file, tag, depth, sr_cm, time = NULL) {
  ok <- is.finite(depth) & is.finite(sr_cm)
  sr <- data.frame(depth = depth[ok], sedrate_cm_kyr = sr_cm[ok])
  o <- list(cd_output(cd_out_name(file, paste0("-", tag, "_sedrate")), sr, var = "sr"))
  if (!is.null(time)) {
    am <- data.frame(depth = depth[ok], time_kyr = time[ok])
    o[[2]] <- cd_output(cd_out_name(file, paste0("-", tag, "_age")), am, var = "am")
  }
  o
}

cd_sr_text <- function(o, sr_cm, time = NULL) {
  sr_cm <- sr_cm[is.finite(sr_cm)]
  txt <- sprintf("Sedimentation rate %s to %s cm/kyr (mean %s).", cd_fmt(min(sr_cm)), cd_fmt(max(sr_cm)), cd_fmt(mean(sr_cm)))
  if (!is.null(time)) {
    txt <- c(txt, sprintf("Duration %s kyr.", cd_fmt(diff(range(time, na.rm = TRUE)))))
  }
  nm <- character()
  for (x in o) {
    nm <- c(nm, x$name)
  }
  c(txt, sprintf("Sedimentation-rate curve (depth, cm/kyr) written to %s: use it in Age models > Build an age model > Sedimentation rate to age model.",
      nm[grepl("_sedrate\\.txt$", nm)][1]),
      if (any(grepl("_age\\.txt$", nm))) {
        sprintf("Age model (depth, time in kyr) written to %s: use it in Age models > Build an age model > Age scale / tuning.",
            nm[grepl("_age\\.txt$", nm)][1])
      } else {
        character()
      })
}

# Name of a hcl palette for WaverideR functions that take one (viridis when the palette has no hcl equivalent)
cd_sr_hcl <- function(spec) {
  nm <- cd_pal_split(spec)$name
  if (nm %in% c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")) {
    return(nm)
  }
  pals <- grDevices::hcl.pals()
  if (nm %in% pals) {
    return(nm)
  }
  "viridis"
}

# Map (depth x sed. rate) with a colour bar and optional curves
cd_sr_plot_map <- function(x, y, z, curves = list(), xlab = "Depth", ylab = "Sed. rate (cm/kyr)", zlab = "",
    palette = "viridis|viridis", main = NULL, logy = FALSE) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, nrow = 1), widths = c(1, 0.12))
  cd_par()
  ox <- order(x)
  oy <- order(y)
  x <- x[ox]
  y <- y[oy]
  z <- z[ox, oy, drop = FALSE]
  cols <- cd_palette(100, palette)
  zr <- range(z, finite = TRUE)
  if (!all(is.finite(zr))) {
    zr <- c(0, 1)
  }
  if (zr[1] == zr[2]) {
    zr <- zr + c(-0.5, 0.5)
  }
  reg <- cd_regular(x) && cd_regular(y) && !logy
  graphics::image(x, y, z, col = cols, zlim = zr, xlab = xlab, ylab = ylab, main = main, useRaster = reg,
      log = if (logy) { "y" } else { "" })
  labs <- character()
  lcols <- character()
  ltys <- numeric()
  for (cv in curves) {
    col <- if (is.null(cv$col)) { "white" } else { cv$col }
    lty <- if (is.null(cv$lty)) { 1 } else { cv$lty }
    graphics::lines(cv$x, cv$y, col = col, lwd = if (is.null(cv$lwd)) { 1.5 } else { cv$lwd }, lty = lty,
        type = if (is.null(cv$type)) { "l" } else { cv$type }, pch = 16, cex = 0.6)
    if (!is.null(cv$label)) {
      labs <- c(labs, cv$label)
      lcols <- c(lcols, col)
      ltys <- c(ltys, lty)
    }
  }
  if (length(labs)) {
    graphics::legend("topright", legend = labs, col = lcols, lty = ltys, lwd = 1.5, bty = "o", cex = 0.8,
        bg = grDevices::adjustcolor("grey25", 0.75), box.col = NA, text.col = "white")
  }
  graphics::box()
  graphics::par(mar = c(4, 0.5, 1.6, 3.5))
  zz <- seq(zr[1], zr[2], length.out = 100)
  graphics::image(1, zz, matrix(zz, nrow = 1), col = cols, axes = FALSE, xlab = "", ylab = "")
  graphics::axis(4, cex.axis = 0.8)
  graphics::mtext(zlab, side = 4, line = 2.3, cex = 0.8, las = 0)
  graphics::box()
  invisible(NULL)
}

# Sed. rate curve with a band and the age model below
cd_sr_plot_curve <- function(x, sr, lo = NULL, hi = NULL, time = NULL, extra = list(), xlab = "Depth") {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  if (!is.null(time)) {
    graphics::layout(matrix(1:2, ncol = 1))
  }
  cd_par()
  yr <- range(c(sr, lo, hi), finite = TRUE)
  for (e in extra) {
    yr <- range(c(yr, e$y), finite = TRUE)
  }
  graphics::plot(x, sr, type = "n", ylim = yr, xlab = xlab, ylab = "Sed. rate (cm/kyr)")
  graphics::grid(col = "grey90", lty = 1)
  if (!is.null(lo) && !is.null(hi)) {
    ok <- is.finite(lo) & is.finite(hi)
    graphics::polygon(c(x[ok], rev(x[ok])), c(lo[ok], rev(hi[ok])), col = grDevices::adjustcolor("#1f4e79", 0.2), border = NA)
  }
  for (e in extra) {
    graphics::lines(e$x, e$y, col = e$col, lty = if (is.null(e$lty)) { 1 } else { e$lty })
  }
  graphics::lines(x, sr, col = "#1f4e79", lwd = 1.5)
  if (!is.null(time)) {
    cd_par()
    graphics::plot(x, time, type = "l", col = "#2e8b57", lwd = 1.5, xlab = xlab, ylab = "Time (kyr)")
    graphics::grid(col = "grey90", lty = 1)
  }
  invisible(NULL)
}

# eCOCO track with the statistics of the map at the tracked rates
cd_sr_ecoco_fill <- function(res, depth, sr) {
  n <- length(depth)
  out <- data.frame(depth = depth, sr = sr, rho = NA_real_, p_global = NA_real_, norbit = NA_real_, score = NA_real_,
      sr_low = sr, sr_high = sr)
  for (i in seq_len(n)) {
    j <- which.min(abs(res$depth - depth[i]))
    if (!is.finite(sr[i]) || !length(j)) {
      next
    }
    k <- which.min(abs(res$sr - sr[i]))
    for (nm in c("rho", "p_global", "norbit", "score")) {
      if (is.matrix(res[[nm]])) {
        out[[nm]][i] <- res[[nm]][k, j]
      }
    }
  }
  out
}

# Points typed as "depth rate; depth rate"
cd_sr_points <- function(txt) {
  v <- cd_parse_nums(gsub(";", " ", txt))
  if (length(v) < 4 || length(v) %% 2 != 0) {
    stop("give the points as pairs of depth and sed. rate, e.g. \"156 2.4; 165 2.2; 172 2.5\"", call. = FALSE)
  }
  m <- matrix(v, ncol = 2, byrow = TRUE)
  data.frame(depth = m[, 1], sr = m[, 2])
}

cd_sr_srgrid_params <- function(lo = 0.5, hi = 5, step = 0.1) {
  list(
    cd_num("srmin", "Lowest sed. rate (cm/kyr)", lo, min = 0),
    cd_num("srmax", "Highest sed. rate (cm/kyr)", hi, min = 0),
    cd_num("srstep", "Sed. rate step (cm/kyr)", step, min = 0))
}

cd_sr_check_range <- function(lo, hi) {
  if (!(is.finite(lo) && is.finite(hi) && lo > 0 && hi > lo)) {
    stop("the highest sedimentation rate must be larger than the lowest, and both larger than 0", call. = FALSE)
  }
  invisible(TRUE)
}

# Drop the large attributes of a WaverideR track
cd_sr_track_slim <- function(tr, keep = c("astro_periods", "posterior")) {
  at <- attributes(tr)
  out <- as.data.frame(tr)
  for (nm in keep) {
    if (!is.null(at[[nm]])) {
      attr(out, nm) <- at[[nm]]
    }
  }
  class(out) <- c("wr_track", "data.frame")
  out
}

cd_sr_track_table <- function(tr, u) {
  tr <- as.data.frame(tr)
  out <- data.frame(depth = tr$depth)
  for (nm in c("sedrate", "sedrate_mean", "sedrate_lower", "sedrate_upper")) {
    if (!is.null(tr[[nm]])) {
      out[[paste0(nm, "_cm_kyr")]] <- tr[[nm]] * u
    }
  }
  for (nm in c("gradient", "confidence", "confidence10", "time", "time_median", "time_lower", "time_upper")) {
    if (!is.null(tr[[nm]])) {
      out[[nm]] <- tr[[nm]]
    }
  }
  out
}

cd_tools_sedrate <- function() {
  len_default <- function(frac) {
    function(d) {
      signif(diff(range(d[, 1], na.rm = TRUE)) * frac, 3)
    }
  }
  band_default <- function(fac) {
    function(d) {
      signif(1 / (cd_peak_freq(d) * fac), 3)
    }
  }
  list(
    # Astronomical HMM tracking ####
    cd_tool("sr_hmm", "Sed. rate", "Astronomical HMM tracking", group = "Scalogram tracking", heavy = TRUE, icon = "route",
        info = paste("Tracks the sedimentation rate through a scalogram with all astronomical cycles at once (hidden Markov",
            "model), with a credible band and an age model. The iterative method repeats the tracking on the record",
            "converted to time, which sharpens the cycles."),
        uses = paste("WaverideR::track_astro_hmm, WaverideR::track_astro_iterative, WaverideR::astro_periods_tracking,",
            "WaverideR::plot_astro_evidence, WaverideR::plot_track"),
        params = c(list(
          cd_sel("method", "Method", c("One pass on a scalogram (track_astro_hmm)" = "hmm",
              "Iterative, in the time domain (track_astro_iterative)" = "iterative")),
          cd_num("srmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("srmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_sr_unit_param()),
          cd_sr_period_params(),
          list(
          cd_sel("scalogram", "Scalogram", c("Morlet CWT (fast)" = "cwt", "Synchrosqueezed CWT" = "sst",
              "Superlet SST (slower)" = "superlet_sst"), show_if = list(param = "method", values = "hmm")),
          cd_sel("scalogram_it", "Scalogram of the first pass", c("Morlet CWT (fast)" = "cwt",
              "Multi-synchrosqueezed (msst)" = "msst", "Superlet SST" = "superlet_sst", "Chirplet SST (slow)" = "chirplet_sst"),
              show_if = list(param = "method", values = "iterative")),
          cd_num("omega_nr", "Number of cycles of the wavelet (omega_nr)", 12, min = 3, step = 1,
              show_if = list(param = "method", values = "hmm")),
          cd_num("n_iter", "Iterations in the time domain", 1, min = 1, max = 4, step = 1,
              show_if = list(param = "method", values = "iterative")),
          cd_num("duration", "Approximate duration (kyr, 0 = unknown)", 0, min = 0,
              show_if = list(param = "method", values = "iterative"),
              help = "Turned into a guide for the tracking; strongly recommended for real records."),
          cd_sel("background", "Background of the power", c("None" = "none", "AR(1) red noise" = "ar1"),
              show_if = list(param = "method", values = "hmm")),
          cd_num("sd_change", "Rate change allowed per shortest cycle (sd of log2 rate)", 0.15, min = 0.001, step = 0.05),
          cd_num("jump_prob", "Probability of an abrupt jump", 1e-4, min = 0, max = 1),
          cd_txt("jump_depths", "Depths of possible jumps, e.g. hiatuses (optional)", "", numeric = TRUE),
          cd_sel("smooth_path", "Smoothing of the tracked path", c("None" = "none", "Piecewise constant (TV)" = "tv",
              "Piecewise linear" = "trend"), show_if = list(param = "method", values = "hmm")),
          cd_num("n_samples", "Posterior samples (uncertainty)", 100, min = 10, step = 50),
          cd_sel("show", "Figure", c("Track on the scalogram" = "track", "Evidence map (rate x depth)" = "evidence",
              "Posterior of the rate" = "posterior", "Sed. rate and age model" = "curve")),
          cd_sel("palette", "Colour palette", cd_palettes()))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          cd_sr_check_range(p$srmin, p$srmax)
          sr <- c(p$srmin, p$srmax) / u
          ap <- cd_sr_astro(p)
          jd <- if (length(p$jump_depths)) { p$jump_depths } else { NULL }
          code <- c(cd_even_code(d), ap$code)
          if (p$method == "hmm") {
            lim <- cd_sr_period_range(s, ap$period, sr)
            sc <- cd_sr_scalogram(s, p$scalogram, lim, p$omega_nr)
            per <- if (is.null(ap$table)) { ap$period } else { ap$table }
            tr <- WaverideR::track_astro_hmm(sc$w, astro_periods = per, sedrate = sr, background = p$background,
                sd_change = p$sd_change, jump_prob = p$jump_prob, jump_depths = jd, smooth_path = p$smooth_path,
                n_samples = p$n_samples)
            code <- c(code, sc$code, sprintf(paste0("tr <- WaverideR::track_astro_hmm(w, astro_periods = %s, sedrate = %s, background = %s,\n",
                "    sd_change = %s, jump_prob = %s, jump_depths = %s, smooth_path = %s, n_samples = %d)"), ap$arg,
                cd_dep(signif(sr, 6)), cd_dep(p$background), cd_fmt(p$sd_change), format(p$jump_prob), cd_dep(jd),
                cd_dep(p$smooth_path), as.integer(p$n_samples)), "WaverideR::plot_track(tr, w)")
            w <- cd_wt_slim(sc$w)
            first <- NULL
            iters <- NULL
          } else {
            dur <- if (is.finite(p$duration) && p$duration > 0) { p$duration } else { NULL }
            tr <- WaverideR::track_astro_iterative(data = as.matrix(s), sedrate = sr, n_iter = p$n_iter,
                scalogram = p$scalogram_it, age_ma = if (p$periods == "age") { p$age_ma } else { 0 },
                astro_periods = if (p$periods == "list") { ap$period } else { NULL }, duration = dur, jump_depths = jd,
                n_samples = p$n_samples, sd_change = p$sd_change, jump_prob = p$jump_prob, verbose = FALSE)
            code <- c(cd_even_code(d), sprintf(paste0("tr <- WaverideR::track_astro_iterative(data = as.matrix(dat), sedrate = %s, n_iter = %d,\n",
                "    scalogram = %s, age_ma = %s, astro_periods = %s, duration = %s, jump_depths = %s, n_samples = %d,\n",
                "    sd_change = %s, jump_prob = %s, verbose = FALSE)"), cd_dep(signif(sr, 6)), as.integer(p$n_iter),
                cd_dep(p$scalogram_it), cd_fmt(if (p$periods == "age") { p$age_ma } else { 0 }),
                if (p$periods == "list") { ap$arg } else { "NULL" }, cd_dep(dur), cd_dep(jd), as.integer(p$n_samples),
                cd_fmt(p$sd_change), format(p$jump_prob)), "WaverideR::plot_track(tr, attr(tr, \"first\")$scalogram)")
            fa <- attr(tr, "first")
            iters <- attr(tr, "iterations")
            w <- cd_wt_slim(fa$scalogram)
            first <- cd_sr_track_slim(fa$track)
          }
          tab <- cd_sr_track_table(tr, u)
          o <- c(list(cd_output(cd_out_name(file, "-HMMtrack"), tab, header = TRUE, var = "track")),
              cd_sr_outputs(file, "HMM", tab$depth, tab$sedrate_cm_kyr, tab$time))
          code <- c(code, "track <- as.data.frame(tr)[, c(\"depth\", \"sedrate\", \"sedrate_lower\", \"sedrate_upper\", \"time\", \"time_lower\", \"time_upper\")]",
              sprintf("sr <- data.frame(depth = tr$depth, sedrate_cm_kyr = tr$sedrate * %s)", cd_fmt(u)),
              "am <- data.frame(depth = tr$depth, time_kyr = tr$time)", cd_code_write(o))
          txt <- c(cd_even_note(d, s), cd_sr_text(o[-1], tab$sedrate_cm_kyr, tab$time))
          if (!is.null(tab$time_lower)) {
            txt <- c(txt, sprintf("95%% credible duration %s to %s kyr.", cd_fmt(max(tab$time_lower, na.rm = TRUE)),
                cd_fmt(max(tab$time_upper, na.rm = TRUE))))
          }
          if (!is.null(iters)) {
            txt <- c(txt, sprintf("Iteration %d: mean confidence %s, duration %s kyr", as.integer(iters$iteration),
                cd_fmt(iters$confidence10), cd_fmt(iters$duration)))
          }
          list(result = list(track = cd_sr_track_slim(tr), first = first, w = w, tab = tab, u = u, method = p$method),
              outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          if (p$show == "curve") {
            tb <- res$tab
            cd_sr_plot_curve(tb$depth, tb$sedrate_cm_kyr, tb$sedrate_lower_cm_kyr, tb$sedrate_upper_cm_kyr, tb$time)
            return(invisible(NULL))
          }
          if (p$show %in% c("evidence", "posterior")) {
            trk <- if (is.null(res$first)) { res$track } else { res$first }
            WaverideR::plot_astro_evidence(track = trk, type = p$show, rate_unit = c(res$u, "cm/kyr"),
                palette = cd_sr_hcl(p$palette))
            return(invisible(NULL))
          }
          WaverideR::plot_track(res$track, res$w, palette = cd_pal_split(p$palette)$name)
        }),
    # Chirp-matched EHA tracking ####
    cd_tool("sr_chirp", "Sed. rate", "Chirp-matched EHA tracking", group = "Scalogram tracking", heavy = TRUE,
        icon = "wave-square",
        info = paste("Sliding-window multitaper spectra de-chirped for a linear change of the rate, scored against the",
            "astronomical template. Gives a tracked rate (HMM on rate and gradient) or the spectral rate estimate per",
            "window (rate-gram)."),
        uses = "WaverideR::analyse_chirp_eha, WaverideR::track_chirp_hmm, WaverideR::rate_gram",
        params = c(list(
          cd_sel("method", "Estimate", c("HMM tracking of rate and gradient (track_chirp_hmm)" = "hmm",
              "Best rate per window, median filtered (rate_gram)" = "rategram")),
          cd_num("srmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("srmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_sr_unit_param()),
          cd_sr_period_params("41, 23.7, 22.4, 19"),
          list(
          cd_num("window", "Window length (depth units)", len_default(0.15), min = 0),
          cd_num("step", "Step (depth units)", len_default(0.01), min = 0),
          cd_num("chirp_max", "Largest relative change of the rate across a window", 0.3, min = 0.01, step = 0.05),
          cd_num("nw", "Time-bandwidth product (NW)", 3, min = 1, step = 0.5),
          cd_num("weight", "Weight of the spectral evidence", 10, min = 0.1, show_if = list(param = "method", values = "hmm")),
          cd_num("n_samples", "Posterior samples (uncertainty)", 100, min = 10, step = 50,
              show_if = list(param = "method", values = "hmm")),
          cd_num("k", "Median filter (windows)", 11, min = 1, step = 2),
          cd_sel("palette", "Colour palette", cd_palettes()))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          cd_sr_check_range(p$srmin, p$srmax)
          sr <- c(p$srmin, p$srmax) / u
          ap <- cd_sr_astro(p)
          lim <- cd_sr_period_range(s, ap$period, sr, max_period = p$window / 2)
          cm <- p$chirp_max / p$window
          chirp <- signif(seq(-cm, cm, length.out = 11), 4)
          ce <- WaverideR::analyse_chirp_eha(data = as.matrix(s), window = p$window, step = p$step, chirp = chirp,
              nw = p$nw, lowerPeriod = lim[1], upperPeriod = lim[2], criterion = "template", astro_periods = ap$period,
              sedrate = sr)
          rg <- WaverideR::rate_gram(ce, k = p$k, plot = FALSE)
          code <- c(cd_even_code(d), ap$code, sprintf(paste0("ce <- WaverideR::analyse_chirp_eha(data = as.matrix(dat), window = %s, step = %s,\n",
              "    chirp = %s, nw = %s, lowerPeriod = %s, upperPeriod = %s, criterion = \"template\",\n",
              "    astro_periods = %s, sedrate = %s)"), cd_fmt(p$window), cd_fmt(p$step), cd_dep(chirp), cd_fmt(p$nw),
              cd_fmt(lim[1]), cd_fmt(lim[2]), if (is.null(ap$table)) { ap$arg } else { "periods$period" },
              cd_dep(signif(sr, 6))),
              sprintf("rg <- WaverideR::rate_gram(ce, k = %d, rate_unit = c(%s, \"cm/kyr\"))", as.integer(p$k), cd_fmt(u)))
          tr <- NULL
          if (p$method == "hmm") {
            tr <- WaverideR::track_chirp_hmm(ce, weight = p$weight, n_samples = p$n_samples)
            tab <- cd_sr_track_table(tr, u)
            code <- c(code, sprintf("tr <- WaverideR::track_chirp_hmm(ce, weight = %s, n_samples = %d)", cd_fmt(p$weight),
                as.integer(p$n_samples)), sprintf("sr <- data.frame(depth = tr$depth, sedrate_cm_kyr = tr$sedrate * %s)", cd_fmt(u)),
                "am <- data.frame(depth = tr$depth, time_kyr = tr$time)")
            o <- c(list(cd_output(cd_out_name(file, "-chirptrack"), tab, header = TRUE, var = "tr")),
                cd_sr_outputs(file, "chirp", tab$depth, tab$sedrate_cm_kyr, tab$time))
            srv <- tab$sedrate_cm_kyr
            tm <- tab$time
          } else {
            srv <- rg$smooth * u
            tm <- cd_sr_age(rg$depth, srv, u)
            tab <- data.frame(depth = rg$depth, best_cm_kyr = rg$best * u, smooth_cm_kyr = srv)
            o <- c(list(cd_output(cd_out_name(file, "-rategram"), tab, header = TRUE, var = "tab")),
                cd_sr_outputs(file, "rategram", rg$depth, srv, tm))
            code <- c(code, sprintf("tab <- data.frame(depth = rg$depth, best_cm_kyr = rg$best * %s, smooth_cm_kyr = rg$smooth * %s)",
                cd_fmt(u), cd_fmt(u)), sprintf("sr <- data.frame(depth = rg$depth, sedrate_cm_kyr = rg$smooth * %s)", cd_fmt(u)),
                cd_sr_age_code(u))
          }
          code <- c(code, cd_code_write(o))
          list(result = list(rg = rg, tab = tab, u = u, method = p$method), outputs = o, code = code,
              text = c(cd_even_note(d, s), cd_sr_text(o[-1], srv, tm)))
        },
        plot = function(res, p) {
          rg <- res$rg
          u <- res$u
          cv <- list(list(x = rg$depth, y = rg$best * u, col = "grey85", type = "p", label = "best per window"),
              list(x = rg$depth, y = rg$smooth * u, col = "white", lty = 2, label = "median filtered"))
          if (res$method == "hmm") {
            tb <- res$tab
            cv <- c(cv, list(list(x = tb$depth, y = tb$sedrate_cm_kyr, col = "#c0392b", lwd = 2, label = "HMM track"),
                list(x = tb$depth, y = tb$sedrate_lower_cm_kyr, col = "#c0392b", lty = 3),
                list(x = tb$depth, y = tb$sedrate_upper_cm_kyr, col = "#c0392b", lty = 3)))
          }
          cd_sr_plot_map(rg$depth, rg$sedrate * u, t(rg$map), curves = cv, zlab = "Template score", palette = p$palette,
              logy = !cd_regular(rg$sedrate))
        }),
    # Evolutive MESA ridge tracking ####
    cd_tool("sr_mesa", "Sed. rate", "Evolutive MESA ridge tracking", group = "Scalogram tracking", icon = "chart-line",
        info = paste("Follows one cycle (e.g. precession or the 405-kyr eccentricity) through an evolutive maximum-entropy",
            "spectrum and converts its period to a sedimentation rate with the duration of the cycle."),
        uses = "WaverideR::mesa_evolutive, WaverideR::track_mesa, WaverideR::plot_mesa_evolutive",
        params = list(
          cd_num("window", "Window length (depth units)", len_default(0.15), min = 0),
          cd_num("step", "Step (depth units)", len_default(0.01), min = 0),
          cd_num("pmin", "Shortest period of the tracked cycle (depth units)", band_default(1.25), min = 0),
          cd_num("pmax", "Longest period of the tracked cycle (depth units)", band_default(0.8), min = 0),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 21, min = 0,
              help = "e.g. 405 (long eccentricity), 100, 41 (obliquity) or about 20 (precession)."),
          cd_num("max_jump", "Largest change of frequency between windows (fraction)", 0.1, min = 0.01, step = 0.05),
          cd_num("penalty", "Penalty for changes of frequency", 0.3, min = 0, step = 0.1),
          cd_sr_unit_param()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          if (!(p$pmax > p$pmin && p$pmin > 0)) {
            stop("the longest period must be larger than the shortest", call. = FALSE)
          }
          fmax <- signif(min(0.5 / cd_dt(s$x), 2 / p$pmin), 4)
          ev <- WaverideR::mesa_evolutive(data = as.matrix(s), window = p$window, step = p$step, fmax = fmax)
          tr <- WaverideR::track_mesa(ev, period = c(p$pmin, p$pmax), max_jump = p$max_jump, penalty = p$penalty)
          tab <- as.data.frame(tr)
          srv <- tab$period * u / p$cycle
          tm <- cd_sr_age(tab$position, srv, u)
          o <- c(list(cd_output(cd_out_name(file, "-MESAtrack"), tab, header = TRUE, var = "tr")),
              cd_sr_outputs(file, "MESA", tab$position, srv, tm))
          code <- c(cd_even_code(d), sprintf("ev <- WaverideR::mesa_evolutive(data = as.matrix(dat), window = %s, step = %s, fmax = %s)",
              cd_fmt(p$window), cd_fmt(p$step), cd_fmt(fmax)),
              sprintf("tr <- WaverideR::track_mesa(ev, period = c(%s, %s), max_jump = %s, penalty = %s)", cd_fmt(p$pmin),
                  cd_fmt(p$pmax), cd_fmt(p$max_jump), cd_fmt(p$penalty)), "WaverideR::plot_mesa_evolutive(ev, track = tr)",
              sprintf("sr <- data.frame(depth = tr$position, sedrate_cm_kyr = tr$period * %s / %s)", cd_fmt(u), cd_fmt(p$cycle)),
              cd_sr_age_code(u), cd_code_write(o))
          list(result = list(ev = ev, tr = tr), outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("Tracked period %s to %s (depth units), taken as a %s-kyr cycle.",
                  cd_fmt(min(tab$period)), cd_fmt(max(tab$period)), cd_fmt(p$cycle)), cd_sr_text(o[-1], srv, tm)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_mesa_evolutive(res$ev, track = res$tr)
        }),
    # Sed.-rate scan of a scalogram ####
    cd_tool("sr_flmw", "Sed. rate", "Sed.-rate scan of a scalogram (FLMW)", group = "Scalogram tracking", heavy = TRUE,
        icon = "magnifying-glass-chart",
        info = paste("Scores every trial sedimentation rate at every depth by how well the astronomical periods match the",
            "wavelet spectrum (FLMW fit, peak or template match, or summed power above red noise), then finds the most",
            "probable smooth rate curve with a credible band."),
        uses = "WaverideR::flmw, WaverideR::sum_power_sedrate, WaverideR::model_red_noise_wt, WaverideR::flmw_path",
        params = c(list(
          cd_sel("method", "Score", c("Template match (fast)" = "template", "Weighted peak match" = "weighted",
              "Linear fit to spectral peaks (classic FLMW)" = "classic", "Summed power above red noise" = "sumpower"))),
          cd_sr_srgrid_params(1, 4, 0.05),
          list(cd_sr_unit_param()),
          cd_sr_period_params(),
          list(
          cd_num("omega_nr", "Number of cycles of the wavelet (omega_nr)", 10, min = 3, step = 1),
          cd_num("tolerance", "Period tolerance (relative)", 0.1, min = 0.01, step = 0.02,
              show_if = list(param = "method", values = c("template", "weighted"))),
          cd_num("n_sim", "Red-noise simulations", 5, min = 2, step = 5, show_if = list(param = "method", values = "sumpower")),
          cd_num("percentile", "Red-noise percentile", 0.75, min = 0.01, max = 0.99, step = 0.05,
              show_if = list(param = "method", values = "sumpower")),
          cd_num("jump_sd", "Rate change allowed per metre (sd of the log rate)", 0.2, min = 0.001, step = 0.05),
          cd_txt("hiatus", "Depths of hiatuses or breaks (optional)", "", numeric = TRUE),
          cd_num("sharpness", "Sharpness of the score", 3, min = 0.1, step = 0.5),
          cd_sel("palette", "Colour palette", cd_palettes()))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          cd_sr_check_range(p$srmin, p$srmax)
          ap <- cd_sr_astro(p)
          per <- ap$period
          lim <- cd_sr_period_range(s, per, c(p$srmin, p$srmax) / u)
          sc <- cd_sr_scalogram(s, "cwt", lim, p$omega_nr, dj = 0.04)
          fac <- 100 / u
          a <- signif(c(p$srmin, p$srmax, p$srstep) * fac, 6)
          cyc <- signif(per, 6)
          code <- c(cd_even_code(d), sc$code)
          if (p$method == "sumpower") {
            rn <- WaverideR::model_red_noise_wt(wavelet = sc$w, n_simulations = p$n_sim, run_multicore = FALSE, verbose = FALSE)
            r <- WaverideR::sum_power_sedrate(red_noise = rn, wavelet = sc$w, percentile = p$percentile, sedrate_low = a[1],
                sedrate_high = a[2], spacing = a[3], cycles = cyc, genplot = FALSE)
            score <- r$power_max_mat
            code <- c(code, sprintf("rn <- WaverideR::model_red_noise_wt(wavelet = w, n_simulations = %d)", as.integer(p$n_sim)),
                sprintf(paste0("sc <- WaverideR::sum_power_sedrate(red_noise = rn, wavelet = w, percentile = %s, sedrate_low = %s,\n",
                    "    sedrate_high = %s, spacing = %s, cycles = %s)"), cd_fmt(p$percentile), cd_fmt(a[1]), cd_fmt(a[2]),
                    cd_fmt(a[3]), cd_dep(cyc)),
                "sc <- list(score_mat = sc$power_max_mat, depth = sc$depth, y_axis = sc$y_axis)")
          } else {
            r <- WaverideR::flmw(wavelet = sc$w, sedrate_low = a[1], sedrate_high = a[2], spacing = a[3], cycles = cyc,
                method = p$method, tolerance = p$tolerance, genplot = FALSE)
            score <- r$score_mat
            code <- c(code, sprintf(paste0("sc <- WaverideR::flmw(wavelet = w, sedrate_low = %s, sedrate_high = %s, spacing = %s,\n",
                "    cycles = %s, method = %s, tolerance = %s)"), cd_fmt(a[1]), cd_fmt(a[2]), cd_fmt(a[3]), cd_dep(cyc),
                cd_dep(p$method), cd_fmt(p$tolerance)))
          }
          x <- list(score_mat = score, depth = r$depth, y_axis = r$y_axis)
          # jump_sd of flmw_path is per metre of depth: convert it to the depth unit of the data
          jsd <- signif(p$jump_sd * sqrt(u / 100), 6)
          hi <- if (length(p$hiatus)) { p$hiatus } else { NULL }
          fp <- WaverideR::flmw_path(x, jump_sd = jsd, sharpness = p$sharpness, hiatus = hi)
          code <- c(code, sprintf("pth <- WaverideR::flmw_path(sc, jump_sd = %s, sharpness = %s, hiatus = %s, genplot = TRUE)",
              cd_fmt(jsd), cd_fmt(p$sharpness), cd_dep(hi)))
          k <- ncol(fp$interval)
          tab <- data.frame(depth = fp$depth, sedrate_path = fp$path / fac, sedrate_mean = fp$mean / fac,
              lower = fp$interval[, 1] / fac, upper = fp$interval[, k] / fac)
          tm <- cd_sr_age(tab$depth, tab$sedrate_path, u)
          o <- c(list(cd_output(cd_out_name(file, paste0("-FLMW_", p$method)), tab, header = TRUE, var = "tab")),
              cd_sr_outputs(file, "FLMW", tab$depth, tab$sedrate_path, tm))
          code <- c(code, sprintf(paste0("tab <- data.frame(depth = pth$depth, sedrate_path = pth$path / %s, sedrate_mean = pth$mean / %s,\n",
              "    lower = pth$interval[, 1] / %s, upper = pth$interval[, ncol(pth$interval)] / %s)"), cd_fmt(fac), cd_fmt(fac),
              cd_fmt(fac), cd_fmt(fac)), "sr <- tab[, 1:2]", cd_sr_age_code(u), cd_code_write(o))
          # thin the depth axis of the maps for storage
          nd <- length(fp$depth)
          idx <- seq(1, nd, by = max(1L, ceiling(nd / 800)))
          list(result = list(depth = fp$depth[idx], rate = r$y_axis / fac, score = score[, idx, drop = FALSE],
              post = fp$posterior[, idx, drop = FALSE], tab = tab), outputs = o, code = code,
              text = c(cd_even_note(d, s), cd_sr_text(o[-1], tab$sedrate_path, tm)))
        },
        plot = function(res, p) {
          tb <- res$tab
          cv <- list(list(x = tb$depth, y = tb$sedrate_path, col = "white", lwd = 2, label = "most probable path"),
              list(x = tb$depth, y = tb$lower, col = "white", lty = 3, label = "credible interval"),
              list(x = tb$depth, y = tb$upper, col = "white", lty = 3))
          z <- res$score
          z[!is.finite(z)] <- NA
          cd_sr_plot_map(res$depth, res$rate, t(z), curves = cv, zlab = "Score", palette = p$palette)
        }),
    # eCOCO, adaptive and cross-validated ####
    cd_tool("sr_ecoco", "Sed. rate", "eCOCO: adaptive / cross-validated", group = "eCOCO tracking", heavy = TRUE,
        icon = "table-cells",
        info = paste("Evolutive COCO of Acycle 3: adaptive targets, or targets trained on separate data (blocked or",
            "interleaved validation) so that the correlation is not inflated. The sed.-rate ridge is tracked",
            "automatically or through points you give. Depth in metres."),
        uses = paste("WaverideR::ecoco_adaptive / ecoco_blocked / ecoco_interleaved, WaverideR::plot_ecoco_cv,",
            "WaverideR::ecoco_track_ridge / ecoco_best_per_window / ecoco_track / track_ecoco"),
        params = c(list(
          cd_sel("method", "Method", c("Blocked validation (ecoco_blocked)" = "blocked",
              "Interleaved validation (ecoco_interleaved, slow)" = "interleaved", "Adaptive (ecoco_adaptive)" = "adaptive")),
          cd_num("window", "Window length (m)", len_default(0.25), min = 0),
          cd_num("step_depth", "Step (m)", len_default(0.025), min = 0)),
          cd_sr_srgrid_params(0.5, 5, 0.1),
          list(
          cd_num("nsim", "Monte Carlo simulations", 50, min = 10, step = 50),
          cd_sel("cor", "Correlation", c("Pearson", "Spearman"))),
          cd_sr_orbit_params(),
          list(
          cd_sel("tracker", "Ridge tracking", c("Viterbi ridge (ecoco_track_ridge)" = "ridge",
              "Best rate per window (ecoco_best_per_window)" = "best", "Acycle depth slices (ecoco_track)" = "slices",
              "Through given points (track_ecoco)" = "points")),
          cd_num("n_slices", "Number of depth slices", 5, min = 1, step = 1, show_if = list(param = "tracker", values = "slices")),
          cd_num("sr_threshold", "Largest rate jump between windows (cm/kyr)", 2.5, min = 0.01,
              show_if = list(param = "tracker", values = "slices")),
          cd_num("p_max", "Largest p-value of a tracked peak", 0.5, min = 0, max = 1,
              show_if = list(param = "tracker", values = "slices")),
          cd_txt("points", "Points: depth rate; depth rate; ...", "", show_if = list(param = "tracker", values = "points")),
          cd_num("seed", "Random seed", 1, step = 1),
          cd_sel("palette", "Colour palette", cd_palettes()))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          dm <- as.matrix(s)
          cd_sr_check_range(p$srmin, p$srmax)
          ob <- cd_sr_orbit(p)
          srg <- seq(p$srmin, p$srmax, by = p$srstep)
          srcode <- sprintf("seq(%s, %s, by = %s)", cd_fmt(p$srmin), cd_fmt(p$srmax), cd_fmt(p$srstep))
          if (p$method == "adaptive") {
            res <- WaverideR::ecoco_adaptive(data = dm, orbit9 = ob$orbit, window = p$window, sr = srg,
                step_depth = p$step_depth, nsim = p$nsim, method = p$cor, seed = p$seed, verbose = FALSE)
            code <- sprintf(paste0("res <- WaverideR::ecoco_adaptive(data = as.matrix(dat), orbit9 = %s, window = %s, sr = %s,\n",
                "    step_depth = %s, nsim = %d, method = %s, seed = %d, verbose = FALSE)"), ob$arg, cd_fmt(p$window), srcode,
                cd_fmt(p$step_depth), as.integer(p$nsim), cd_dep(p$cor), as.integer(p$seed))
          } else {
            fn <- if (p$method == "blocked") { WaverideR::ecoco_blocked } else { WaverideR::ecoco_interleaved }
            res <- fn(data = dm, orbit9 = ob$orbit, window = p$window, sr = srg, step_depth = p$step_depth, n_sim = p$nsim,
                method = p$cor, seed = p$seed)
            code <- sprintf(paste0("res <- WaverideR::ecoco_%s(data = as.matrix(dat), orbit9 = %s, window = %s, sr = %s,\n",
                "    step_depth = %s, n_sim = %d, method = %s, seed = %d)"), p$method, ob$arg, cd_fmt(p$window), srcode,
                cd_fmt(p$step_depth), as.integer(p$nsim), cd_dep(p$cor), as.integer(p$seed))
          }
          if (p$tracker == "best") {
            tr <- WaverideR::ecoco_best_per_window(sr = res$sr, depth = res$depth, rho = res$rho, p = res$p_global,
                norbit = res$norbit, score = res$score)
            code <- c(code, paste0("tr <- WaverideR::ecoco_best_per_window(sr = res$sr, depth = res$depth, rho = res$rho, ",
                "p = res$p_global, norbit = res$norbit, score = res$score)"))
          } else if (p$tracker == "slices") {
            et <- WaverideR::ecoco_track(res, n_slices = p$n_slices, sr_threshold = p$sr_threshold, p_max = p$p_max)
            tr <- cd_sr_ecoco_fill(res, et$depth, et$sr)
            code <- c(code, sprintf("et <- WaverideR::ecoco_track(res, n_slices = %d, sr_threshold = %s, p_max = %s)",
                as.integer(p$n_slices), cd_fmt(p$sr_threshold), cd_fmt(p$p_max)),
                "tr <- data.frame(depth = et$depth, sr = et$sr)")
          } else if (p$tracker == "points") {
            pts <- cd_sr_points(p$points)
            et <- WaverideR::track_ecoco(res, points = pts, interpolate = TRUE)
            tr <- cd_sr_ecoco_fill(res, et$depth, et$sedrate)
            code <- c(code, sprintf("pts <- data.frame(depth = %s, sr = %s)", cd_dep(pts$depth), cd_dep(pts$sr)),
                "et <- WaverideR::track_ecoco(res, points = pts, interpolate = TRUE)",
                "tr <- data.frame(depth = et$depth, sr = et$sedrate)")
          } else {
            tr <- WaverideR::ecoco_track_ridge(score = res$score, sr = res$sr, depth = res$depth, rho = res$rho,
                p = res$p_global, norbit = res$norbit)
            code <- c(code, paste0("tr <- WaverideR::ecoco_track_ridge(score = res$score, sr = res$sr, depth = res$depth, ",
                "rho = res$rho, p = res$p_global, norbit = res$norbit)"))
          }
          tr <- as.data.frame(tr)
          res$track <- tr
          tm <- NULL
          ok <- is.finite(tr$sr)
          if (sum(ok) >= 2) {
            tm <- rep(NA_real_, nrow(tr))
            tm[ok] <- cd_sr_age(tr$depth[ok], tr$sr[ok], 100)
          }
          o <- c(list(cd_output(cd_out_name(file, paste0("-eCOCO_", p$method)), tr, header = TRUE, var = "tr")),
              if (sum(ok) >= 1) { cd_sr_outputs(file, "eCOCO", tr$depth, tr$sr, tm) } else { list() })
          code <- c(code, "res$track <- tr", "WaverideR::plot_ecoco_cv(res)",
              "sr <- tr[is.finite(tr$sr), c(\"depth\", \"sr\")]", cd_sr_age_code(100), cd_code_write(o))
          txt <- if (sum(ok) >= 1) { cd_sr_text(o[-1], tr$sr, tm) } else { "No sedimentation rate could be tracked." }
          if (!is.null(tr$p_global)) {
            txt <- c(sprintf("%d of %d windows with global p <= 0.05 at the tracked rate.", sum(tr$p_global <= 0.05, na.rm = TRUE),
                nrow(tr)), txt)
          }
          res$forward <- NULL
          res$backward <- NULL
          list(result = res, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_ecoco_cv(res, palette = cd_palette(64, p$palette))
        }),
    # Adaptive COCO and cvCOCO ####
    cd_tool("sr_coco", "Sed. rate", "COCO: adaptive / cross-validated", group = "Astronomical testing", heavy = TRUE,
        icon = "scale-balanced",
        info = paste("Correlation of the data spectrum with an astronomical target over a range of constant",
            "sedimentation rates (Acycle 3): adaptive target amplitudes, or cvCOCO with targets trained on one half of the",
            "record and tested on the other. Depth in metres."),
        uses = "WaverideR::coco_adaptive, WaverideR::cvcoco, WaverideR::plot_cvcoco, WaverideR::coco_target_fixed",
        params = c(list(
          cd_sel("method", "Method", c("cvCOCO, blocked halves" = "blocked", "cvCOCO, interleaved samples" = "interleaved",
              "Adaptive COCO" = "adaptive"))),
          cd_sr_srgrid_params(0.5, 5, 0.05),
          list(
          cd_num("nsim", "Monte Carlo simulations", 200, min = 10, step = 100),
          cd_sel("cor", "Correlation", c("Pearson", "Spearman")),
          cd_sel("amplitude_mode", "Target amplitudes", c("Fitted per orbital group" = "four-group-area",
              "Fitted per period" = "per-orbit", "Fixed (Acycle weights)" = "fixed"),
              show_if = list(param = "method", values = "adaptive"))),
          cd_sr_orbit_params(),
          list(
          cd_num("seed", "Random seed", 1, step = 1),
          cd_sel("page", "Figure", c("Correlation and p-values" = "result", "Folds and validation spectra" = "spectra",
              "pCOCO" = "pcoco", "Monte Carlo audit" = "audit", "Data spectrum and fixed target at the best rate" = "target")))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          dm <- as.matrix(s)
          cd_sr_check_range(p$srmin, p$srmax)
          ob <- cd_sr_orbit(p)
          if (p$method == "adaptive") {
            srg <- seq(p$srmin, p$srmax, by = p$srstep)
            res <- WaverideR::coco_adaptive(data = dm, orbit9 = ob$orbit, sr = srg, nsim = p$nsim, method = p$cor,
                amplitude_mode = p$amplitude_mode, seed = p$seed, verbose = FALSE)
            tab <- as.data.frame(res$result)
            best <- tab$sr[which.max(tab$rho)]
            code <- sprintf(paste0("res <- WaverideR::coco_adaptive(data = as.matrix(dat), orbit9 = %s, sr = seq(%s, %s, by = %s),\n",
                "    nsim = %d, method = %s, amplitude_mode = %s, seed = %d, verbose = FALSE)"), ob$arg, cd_fmt(p$srmin),
                cd_fmt(p$srmax), cd_fmt(p$srstep), as.integer(p$nsim), cd_dep(p$cor), cd_dep(p$amplitude_mode),
                as.integer(p$seed))
            code <- c(code, "plot(res)", "tab <- res$result")
            txt <- sprintf("Highest correlation at %s cm/kyr (rho = %s, global p = %s).", cd_fmt(best), cd_fmt(max(tab$rho)),
                cd_fmt(tab$p_global[which.max(tab$rho)]))
          } else {
            res <- WaverideR::cvcoco(data = dm, orbit9 = ob$orbit, sr1 = p$srmin, sr2 = p$srmax, srstep = p$srstep,
                n_sim = p$nsim, cor_method = p$cor, method = p$method, seed = p$seed)
            cs <- res$consensus
            tab <- data.frame(sr = res$sr_grid, rho_AtoB = res$validate_AtoB$curve, rho_BtoA = res$validate_BtoA$curve,
                rho_consensus = cs$curve, p_global = cs$p_global_curve, p_local = cs$p_local_curve, pcoco = cs$pcoco)
            best <- cs$best_rate
            code <- sprintf(paste0("res <- WaverideR::cvcoco(data = as.matrix(dat), orbit9 = %s, sr1 = %s, sr2 = %s, srstep = %s,\n",
                "    n_sim = %d, cor_method = %s, method = %s, seed = %d)"), ob$arg, cd_fmt(p$srmin), cd_fmt(p$srmax),
                cd_fmt(p$srstep), as.integer(p$nsim), cd_dep(p$cor), cd_dep(p$method), as.integer(p$seed))
            code <- c(code, "WaverideR::plot_cvcoco(res, pages = \"result\")",
                paste0("tab <- data.frame(sr = res$sr_grid, rho_AtoB = res$validate_AtoB$curve, rho_BtoA = res$validate_BtoA$curve,\n",
                    "    rho_consensus = res$consensus$curve, p_global = res$consensus$p_global_curve,\n",
                    "    p_local = res$consensus$p_local_curve, pcoco = res$consensus$pcoco)"))
            txt <- c(sprintf("%s: consensus best rate %s cm/kyr (rho = %s, global p = %s).", res$name, cd_fmt(best),
                cd_fmt(cs$best_correlation), cd_fmt(cs$p_global)),
                sprintf("Robust p-value of the held-out test: %s.", cd_fmt(res$p_robust)))
          }
          # data spectrum against the fixed Acycle target at the best rate
          tg <- NULL
          if (is.finite(best)) {
            sp <- astrochron::periodogram(cd_even(s), padfac = 4, output = 1, genplot = FALSE, verbose = FALSE)
            ft <- sp$Frequency * best / 100
            keep <- ft <= 1.2 * max(1 / ob$orbit) & ft > 0
            tp <- WaverideR::coco_target_fixed(data = as.matrix(cd_even(s)), orbit9 = ob$orbit, freq = ft[keep], sr = best)
            tg <- data.frame(f = ft[keep], data = sp$Power[keep] / max(sp$Power[keep]), target = tp / max(tp))
          }
          o <- list(cd_output(cd_out_name(file, paste0("-COCO_", p$method)), tab, header = TRUE, var = "tab"))
          list(result = list(res = res, tab = tab, tg = tg, best = best, orbit = ob$orbit, method = p$method), outputs = o,
              code = c(code, cd_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          if (p$page == "target") {
            if (is.null(res$tg)) {
              cd_message_plot("No best rate")
              return(invisible(NULL))
            }
            cd_par()
            tg <- res$tg
            graphics::plot(tg$f, tg$data, type = "l", col = "#1f4e79", xlab = "Frequency (cycles/kyr)",
                ylab = "Normalised power", main = sprintf("Data at %s cm/kyr and fixed target", cd_fmt(res$best)),
                ylim = c(0, 1.05))
            graphics::lines(tg$f, tg$target, col = "#c0392b", lwd = 1.5)
            graphics::abline(v = 1 / res$orbit, col = "grey70", lty = 3)
            graphics::legend("topright", c("data", "fixed target (coco_target_fixed)"), col = c("#1f4e79", "#c0392b"), lwd = 1.5,
                bty = "n", cex = 0.8)
            return(invisible(NULL))
          }
          if (res$method == "adaptive") {
            plot(res$res)
          } else {
            WaverideR::plot_cvcoco(res$res, pages = p$page)
          }
        }),
    # COCO with a target series ####
    cd_tool("sr_coco_series", "Sed. rate", "COCO / eCOCO with a target series (2 files)", group = "Astronomical testing",
        heavy = TRUE, inputs = 2L, icon = "bullseye",
        info = paste("COCO or evolutive COCO with the spectrum of an astronomical series (series 2: time in kyr, value,",
            "e.g. an ETP or insolation series from Basic Series) as the target. Depth in metres."),
        uses = "WaverideR::coco_target_from_series, WaverideR::coco_classic, WaverideR::ecoco_classic",
        params = c(list(
          cd_sel("method", "Method", c("COCO (one rate for the record)" = "coco", "eCOCO (sliding windows)" = "ecoco"))),
          cd_sr_srgrid_params(0.5, 5, 0.1),
          list(
          cd_num("window", "Window length (m)", len_default(0.25), min = 0, show_if = list(param = "method", values = "ecoco")),
          cd_num("step", "Step (samples)", function(d) { max(1, round(nrow(d) / 40)) }, min = 1, step = 1,
              show_if = list(param = "method", values = "ecoco")),
          cd_num("fmax", "Highest target frequency (cycles/kyr)", 0.06, min = 0),
          cd_num("nsim", "Monte Carlo simulations (0 = none)", 100, min = 0, step = 100),
          cd_sel("cor", "Correlation", c("Pearson", "Spearman")),
          cd_num("seed", "Random seed", 1, step = 1))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          a <- cd_clean(d2)
          cd_sr_check_range(p$srmin, p$srmax)
          tk <- a$x - min(a$x)
          tg <- WaverideR::coco_target_from_series(time_kyr = tk, value = a$y, f1 = 0, f2 = p$fmax)
          srg <- seq(p$srmin, p$srmax, by = p$srstep)
          code <- c(cd_even_code(d), cd_code_read(file2, var = "astro"),
              sprintf("tg <- WaverideR::coco_target_from_series(time_kyr = astro[, 1] - min(astro[, 1]), value = astro[, 2], f1 = 0, f2 = %s)",
                  cd_fmt(p$fmax)))
          srcode <- sprintf("seq(%s, %s, by = %s)", cd_fmt(p$srmin), cd_fmt(p$srmax), cd_fmt(p$srstep))
          if (p$method == "coco") {
            r <- WaverideR::coco_classic(data = as.matrix(s), target = tg, sr = srg, nsim = p$nsim, method = p$cor,
                seed = p$seed, verbose = FALSE)
            tab <- as.data.frame(r$result)
            o <- list(cd_output(cd_out_name(file, "-COCOseries"), tab, header = TRUE, var = "res$result"))
            code <- c(code, sprintf("res <- WaverideR::coco_classic(data = as.matrix(dat), target = tg, sr = %s, nsim = %d, method = %s, seed = %d)",
                srcode, as.integer(p$nsim), cd_dep(p$cor), as.integer(p$seed)), "plot(res)", cd_code_write(o))
            txt <- sprintf("Highest correlation at %s cm/kyr (rho = %s)", cd_fmt(tab$sr[which.max(tab$rho)]), cd_fmt(max(tab$rho)))
          } else {
            r <- WaverideR::ecoco_classic(data = as.matrix(s), target = tg, window = p$window, step = p$step, sr = srg,
                nsim = p$nsim, method = p$cor, seed = p$seed, verbose = FALSE)
            tr <- as.data.frame(r$track)
            ok <- is.finite(tr$sr)
            tm <- rep(NA_real_, nrow(tr))
            if (sum(ok) >= 2) {
              tm[ok] <- cd_sr_age(tr$depth[ok], tr$sr[ok], 100)
            }
            o <- c(list(cd_output(cd_out_name(file, "-eCOCOseries_track"), tr, header = TRUE, var = "tr")),
                cd_sr_outputs(file, "eCOCOseries", tr$depth, tr$sr, tm))
            code <- c(code, sprintf(paste0("res <- WaverideR::ecoco_classic(data = as.matrix(dat), target = tg, window = %s, step = %d,\n",
                "    sr = %s, nsim = %d, method = %s, seed = %d)"), cd_fmt(p$window), as.integer(p$step), srcode,
                as.integer(p$nsim), cd_dep(p$cor), as.integer(p$seed)), "plot(res)", "tr <- as.data.frame(res$track)",
                "sr <- tr[is.finite(tr$sr), c(\"depth\", \"sr\")]", cd_sr_age_code(100), cd_code_write(o))
            txt <- cd_sr_text(o[-1], tr$sr, tm)
          }
          list(result = r, outputs = o, code = code, text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          plot(res)
        }),
    # win_timeOpt with several fits ####
    cd_tool("sr_wto_multi", "Sed. rate", "Windowed TimeOpt with several fits", group = "Astronomical testing",
        heavy = TRUE, icon = "layer-group",
        info = paste("Runs windowed TimeOpt (bias corrected against noise) with the precession, short-eccentricity and",
            "obliquity amplitude-modulation fits and compares their best rates per window: rates that agree are robust,",
            "rates in the ratio of two cycles point to an alias. Depth in metres."),
        uses = "WaverideR::win_timeOpt_multi, WaverideR::plot_win_timeOpt_multi",
        params = list(
          cd_chk("fit1", "Fit 1: precession amplitude modulation", TRUE),
          cd_chk("fit2", "Fit 2: short-eccentricity amplitude modulation", TRUE),
          cd_chk("fit3", "Fit 3: obliquity amplitude modulation", FALSE),
          cd_sel("window_mode", "Window", c("Fixed length in depth" = "depth", "Fixed duration in time" = "time")),
          cd_num("window_size", "Window length (m)", len_default(0.5), min = 0,
              show_if = list(param = "window_mode", values = "depth")),
          cd_num("window_duration", "Window duration (kyr)", 1000, min = 0,
              show_if = list(param = "window_mode", values = "time")),
          cd_num("step", "Step (samples)", function(d) { max(1, round(nrow(d) / 40)) }, min = 1, step = 1),
          cd_num("sedmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 20, min = 5, step = 5),
          cd_num("n_null", "Noise simulations per window", 10, min = 2, step = 10),
          cd_num("ratio_tol", "Tolerance of the rate ratios", 0.15, min = 0.01, step = 0.05),
          cd_sel("periods", "Astronomical periods", c("TimeOpt default" = "default", "At the age of the record (AstroGeo22)" = "age")),
          cd_num("age_ma", "Age of the record (Ma)", 0, min = 0, show_if = list(param = "periods", values = "age")),
          cd_sel("sr_fit", "Fit written as sed. rate curve", c("Fit 1" = "1", "Fit 2" = "2", "Fit 3" = "3"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          cd_sr_check_range(p$sedmin, p$sedmax)
          fits <- c(1, 2, 3)[c(isTRUE(p$fit1), isTRUE(p$fit2), isTRUE(p$fit3))]
          if (!length(fits)) {
            stop("choose at least one fit", call. = FALSE)
          }
          code <- cd_even_code(d)
          args <- sprintf(paste0("fits = %s, ratio_tol = %s, window_mode = %s, window_size = %s, window_duration = %s,\n",
              "    step = %d, sedmin = %s, sedmax = %s, numsed = %d, n_null = %d"), cd_dep(fits), cd_fmt(p$ratio_tol),
              cd_dep(p$window_mode), cd_fmt(p$window_size), cd_fmt(p$window_duration), as.integer(p$step), cd_fmt(p$sedmin),
              cd_fmt(p$sedmax), as.integer(p$numsed), as.integer(p$n_null))
          if (p$periods == "age") {
            ap <- WaverideR::astro_periods_tracking(age_ma = p$age_ma)
            tP <- signif(sort(ap$period[ap$family == "precession"], decreasing = TRUE), 6)
            tO <- signif(sort(ap$period[ap$family == "obliquity"], decreasing = TRUE), 6)
            r <- WaverideR::win_timeOpt_multi(data = as.matrix(s), fits = fits, ratio_tol = p$ratio_tol,
                window_mode = p$window_mode, window_size = p$window_size, window_duration = p$window_duration,
                step = as.integer(p$step), sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed, n_null = p$n_null,
                targetP = tP, targetO = tO)
            code <- c(code, sprintf("res <- WaverideR::win_timeOpt_multi(data = as.matrix(dat), %s,\n    targetP = %s, targetO = %s)",
                args, cd_dep(tP), cd_dep(tO)))
          } else {
            r <- WaverideR::win_timeOpt_multi(data = as.matrix(s), fits = fits, ratio_tol = p$ratio_tol,
                window_mode = p$window_mode, window_size = p$window_size, window_duration = p$window_duration,
                step = as.integer(p$step), sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed, n_null = p$n_null)
            code <- c(code, sprintf("res <- WaverideR::win_timeOpt_multi(data = as.matrix(dat), %s)", args))
          }
          sm <- as.data.frame(r$summary)
          names(sm)[1] <- "depth"
          o <- list(cd_output(cd_out_name(file, "-winTimeOpt_multi"), sm, header = TRUE, var = "res$summary"))
          code <- c(code, "WaverideR::plot_win_timeOpt_multi(res)")
          f <- as.integer(p$sr_fit)
          if (!(f %in% fits)) {
            f <- fits[1]
          }
          b <- sm[[paste0("best_fit", f)]]
          ok <- is.finite(b)
          txt <- character()
          for (k in fits) {
            bk <- sm[[paste0("best_fit", k)]]
            txt <- c(txt, sprintf("Fit %d: %d of %d windows testable, %d significant.", k, sum(is.finite(bk)), nrow(sm),
                sum(sm[[paste0("sig_fit", k)]], na.rm = TRUE)))
          }
          if (sum(ok) >= 1) {
            o <- c(o, cd_sr_outputs(file, paste0("winTimeOpt_fit", f), sm$depth, b))
            code <- c(code, sprintf("sr <- res$summary[is.finite(res$summary$best_fit%d), c(\"x\", \"best_fit%d\")]", f, f))
            txt <- c(txt, sprintf("Best rates of fit %d (%d windows; gaps where untestable) written to %s.", f, sum(ok),
                o[[2]]$name))
          } else {
            txt <- c(txt, "No window could be tested: use a longer window or higher sedimentation rates.")
          }
          list(result = r, outputs = o, code = c(code, cd_code_write(o)), text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          sm <- res$summary
          any_ok <- FALSE
          for (nm in names(sm)[grepl("^best_fit", names(sm))]) {
            any_ok <- any_ok || any(is.finite(sm[[nm]]))
          }
          if (!any_ok) {
            cd_message_plot("No window could be tested (window too short for these rates)")
            return(invisible(NULL))
          }
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_win_timeOpt_multi(res)
        }),
    # Astronomical sedimentation-rate tests ####
    cd_tool("sr_astro_test", "Sed. rate", "Astronomical sed.-rate test", group = "Astronomical testing", heavy = TRUE,
        icon = "check-double",
        info = paste("Tests constant sedimentation rates against noise analysed at the same rate (TimeOpt, likelihood and",
            "COCO scores, several noise models), or estimates the rate history and tests the minimally tuned record",
            "against surrogates. Depth in metres."),
        uses = "WaverideR::astro_sedrate_test, WaverideR::astro_tune_test",
        params = list(
          cd_sel("method", "Test", c("Constant rates, TimeOpt / COCO scores (astro_sedrate_test)" = "sedrate",
              "Tracked rate and tuning against surrogates (astro_tune_test)" = "tune")),
          cd_num("sedmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 25, min = 5, step = 5),
          cd_sel("fit", "TimeOpt fit", c("Precession amplitude modulation" = "1", "Short-eccentricity modulation" = "2"),
              show_if = list(param = "method", values = "sedrate")),
          cd_sel("periods", "Astronomical periods", c("Default" = "default", "At the age of the record (AstroGeo22)" = "age")),
          cd_num("age_ma", "Age of the record (Ma)", 0, min = 0, show_if = list(param = "periods", values = "age")),
          cd_chk("coco", "Include the COCO score", TRUE, show_if = list(param = "method", values = "sedrate")),
          cd_chk("tune", "Minimal tuning at the best rate (writes an age model)", TRUE,
              show_if = list(param = "method", values = "sedrate")),
          cd_num("n_null", "Noise series", 20, min = 5, step = 10, show_if = list(param = "method", values = "sedrate")),
          cd_num("n_sur", "Surrogates", 20, min = 5, step = 10, show_if = list(param = "method", values = "tune")),
          cd_num("n_paths", "Sampled rate paths", 50, min = 10, step = 10, show_if = list(param = "method", values = "tune"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          cd_sr_check_range(p$sedmin, p$sedmax)
          age <- if (p$periods == "age") { p$age_ma } else { NULL }
          code <- cd_even_code(d)
          if (p$method == "sedrate") {
            tn <- if (isTRUE(p$tune)) { "minimal" } else { "none" }
            r <- WaverideR::astro_sedrate_test(data = as.matrix(s), sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed,
                fit = as.integer(p$fit), age_ma = age, coco = isTRUE(p$coco), tune = tn, n_null = p$n_null,
                sensitivity = FALSE, target_sensitivity = FALSE)
            code <- c(code, sprintf(paste0("res <- WaverideR::astro_sedrate_test(data = as.matrix(dat), sedmin = %s, sedmax = %s, numsed = %d,\n",
                "    fit = %s, age_ma = %s, coco = %s, tune = %s, n_null = %d, sensitivity = FALSE, target_sensitivity = FALSE)"),
                cd_fmt(p$sedmin), cd_fmt(p$sedmax), as.integer(p$numsed), p$fit, cd_dep(age), isTRUE(p$coco), cd_dep(tn),
                as.integer(p$n_null)), "plot(res)")
            o <- list(cd_output(cd_out_name(file, "-astrotest_scores"), as.data.frame(r$scores), header = TRUE, var = "res$scores"),
                cd_output(cd_out_name(file, "-astrotest_best"), as.data.frame(r$best), header = TRUE, var = "res$best"))
            txt <- character()
            bt <- as.data.frame(r$best)
            for (i in seq_len(nrow(bt))) {
              txt <- c(txt, sprintf("%s: best %s cm/kyr, value %s%s", bt$score[i], cd_fmt(bt$best_sedrate[i]), cd_fmt(bt$value[i]),
                  if (is.finite(bt$p_fw[i])) { sprintf(", family-wise p = %s", cd_fmt(bt$p_fw[i])) } else { "" }))
            }
            if (!is.null(r$tuning)) {
              tu <- as.data.frame(r$tuning)
              o <- c(o, cd_sr_outputs(file, "astrotest", tu$depth, tu$sedrate, tu$time_kyr))
              code <- c(code, "sr <- res$tuning[, c(\"depth\", \"sedrate\")]", "am <- res$tuning[, c(\"depth\", \"time_kyr\")]")
              txt <- c(txt, cd_sr_text(o[3:4], tu$sedrate, tu$time_kyr))
            }
            r$data <- NULL
          } else {
            r <- WaverideR::astro_tune_test(data = as.matrix(s), sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed,
                age_ma = age, n_sur = p$n_sur, n_paths = p$n_paths)
            code <- c(code, sprintf(paste0("res <- WaverideR::astro_tune_test(data = as.matrix(dat), sedmin = %s, sedmax = %s, numsed = %d,\n",
                "    age_ma = %s, n_sur = %d, n_paths = %d)"), cd_fmt(p$sedmin), cd_fmt(p$sedmax), as.integer(p$numsed), cd_dep(age),
                as.integer(p$n_sur), as.integer(p$n_paths)), "plot(res)", "sr <- res$rate[, c(\"depth\", \"rate\")]",
                "am <- res$age_model[, c(\"depth\", \"time\")]")
            rt <- as.data.frame(r$rate)
            ag <- as.data.frame(r$age_model)
            sm <- as.data.frame(r$summary)
            o <- list(cd_output(cd_out_name(file, "-tunetest_summary"), sm, header = TRUE, var = "res$summary"),
                cd_output(cd_out_name(file, "-tunetest_rate"), rt, header = TRUE, var = "res$rate"))
            o <- c(o, cd_sr_outputs(file, "tunetest", rt$depth, rt$rate))
            o[[4]] <- cd_output(cd_out_name(file, "-tunetest_age"), data.frame(depth = ag$depth, time_kyr = ag$time), var = "am")
            txt <- c(sprintf("Mean rate %s cm/kyr, duration %s kyr (95%%: %s to %s).", cd_fmt(sm$mean_rate), cd_fmt(sm$duration_kyr),
                cd_fmt(sm$duration_lo), cd_fmt(sm$duration_hi)),
                sprintf("Amplitude modulation S_am = %s (p = %s); all tests S_all = %s (p = %s).", cd_fmt(sm$S_am),
                    cd_fmt(sm$p_S_am), cd_fmt(sm$S_all), cd_fmt(sm$p_S_all)),
                cd_sr_text(o[3:4], rt$rate, ag$time))
          }
          list(result = r, outputs = o, code = c(code, cd_code_write(o)), text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          plot(res)
        }),
    cd_tool("sr_astro_multi", "Sed. rate", "Astronomical sed.-rate test, two proxies (2 files)",
        group = "Astronomical testing", heavy = TRUE, inputs = 2L, icon = "flask",
        info = paste("Runs the astronomical sedimentation-rate test on two proxies of the same section and combines their",
            "evidence rate by rate (they share one sedimentation rate). Depth in metres."),
        uses = "WaverideR::astro_sedrate_multi",
        params = list(
          cd_num("sedmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 20, min = 5, step = 5),
          cd_chk("fit1", "TimeOpt fit 1 (precession modulation)", TRUE),
          cd_chk("fit2", "TimeOpt fit 2 (short-eccentricity modulation)", FALSE),
          cd_sel("combine", "Combination", c("Minimum p" = "minp", "Stouffer" = "stouffer")),
          cd_chk("coco", "Include the COCO score", FALSE),
          cd_sel("periods", "Astronomical periods", c("Default" = "default", "At the age of the record (AstroGeo22)" = "age")),
          cd_num("age_ma", "Age of the record (Ma)", 0, min = 0, show_if = list(param = "periods", values = "age")),
          cd_num("n_null", "Noise series", 20, min = 5, step = 10)),
        compute = function(d, p, d2, file, file2) {
          s1 <- cd_even(d)
          s2 <- cd_even(d2)
          cd_sr_check_range(p$sedmin, p$sedmax)
          fits <- c(1, 2)[c(isTRUE(p$fit1), isTRUE(p$fit2))]
          if (!length(fits)) {
            stop("choose at least one fit", call. = FALSE)
          }
          age <- if (p$periods == "age") { p$age_ma } else { NULL }
          r <- WaverideR::astro_sedrate_multi(list(series1 = as.matrix(s1), series2 = as.matrix(s2)), sedmin = p$sedmin,
              sedmax = p$sedmax, numsed = p$numsed, fit = fits, combine = p$combine, age_ma = age, n_null = p$n_null,
              coco = isTRUE(p$coco))
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf(paste0("res <- WaverideR::astro_sedrate_multi(list(series1 = as.matrix(dat), series2 = as.matrix(dat2)),\n",
              "    sedmin = %s, sedmax = %s, numsed = %d, fit = %s, combine = %s, age_ma = %s, n_null = %d, coco = %s)"),
              cd_fmt(p$sedmin), cd_fmt(p$sedmax), as.integer(p$numsed), cd_dep(fits), cd_dep(p$combine), cd_dep(age),
              as.integer(p$n_null), isTRUE(p$coco)), "plot(res)")
          o <- list(cd_output(cd_out_name(file, "-astromulti"), as.data.frame(r$sections[[1]]), header = TRUE,
              var = "res$sections[[1]]"),
              cd_output(cd_out_name(file, "-astromulti_components"), as.data.frame(r$components), header = TRUE,
                  var = "res$components"))
          bt <- as.data.frame(r$best)
          txt <- sprintf("Combined best rate %s cm/kyr (S = %s, family-wise p = %s, %d series).", cd_fmt(bt$best_sedrate[1]),
              cd_fmt(bt$S_multi[1]), cd_fmt(bt$p_fw[1]), as.integer(bt$n_series[1]))
          cp <- as.data.frame(r$components)
          r$runs <- NULL
          list(result = r, outputs = o, code = c(code, cd_code_write(o)), text = c(txt, utils::capture.output(print(cp))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          plot(res)
        }),
    # Power decomposition, Acycle legacy ####
    cd_tool("sr_pda_legacy", "Sed. rate", "Power decomposition (Acycle legacy)", group = "Power decomposition",
        icon = "chart-area",
        info = paste("Ratio of the power in a target band to the total power in sliding windows, exactly as the Acycle",
            "menu and DYNOT compute it (adaptive multitaper, historical bin mapping)."),
        uses = "WaverideR::pda_legacy",
        params = list(
          cd_txt("bands", "Target band (two frequencies, cycles/unit)", function(d) {
            paste(signif(cd_peak_freq(d) * c(0.8, 1.2), 3), collapse = ", ")
          }, numeric = TRUE),
          cd_num("window", "Window length", len_default(0.15), min = 0),
          cd_num("nw", "Time-bandwidth product (NW)", 2, min = 1, step = 0.5),
          cd_num("step", "Step (samples)", 5, min = 1, step = 1),
          cd_num("pad", "Padding (FFT length)", 5000, min = 0, step = 1000)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          b <- p$bands
          if (length(b) < 2) {
            stop("give two frequencies for the target band", call. = FALSE)
          }
          r <- WaverideR::pda_legacy(data = as.matrix(s), target_bands = b[1:2], window = p$window, nw = p$nw,
              step = as.integer(p$step), pad = p$pad)
          tab <- as.data.frame(r)
          o <- list(cd_output(cd_out_name(file, "-PDAlegacy"), tab, header = TRUE, var = "res"))
          code <- c(cd_even_code(d), sprintf("res <- WaverideR::pda_legacy(data = as.matrix(dat), target_bands = %s, window = %s, nw = %s, step = %d, pad = %s)",
              cd_dep(b[1:2]), cd_fmt(p$window), cd_fmt(p$nw), as.integer(p$step), cd_fmt(p$pad)), cd_code_write(o))
          list(result = list(s = s, tab = tab), outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("Target / total power: mean %s, range %s to %s", cd_fmt(mean(tab[, 2])),
                  cd_fmt(min(tab[, 2])), cd_fmt(max(tab[, 2])))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$s, col = "#7f8c8d", ylab = "input", xlab = "")
          cd_par()
          graphics::plot(res$tab[, 1], res$tab[, 2], type = "l", col = "#1f4e79", lwd = 1.5, xlab = "Depth / time",
              ylab = "Target / total power")
          graphics::grid(col = "grey90", lty = 1)
        })
  )
}
