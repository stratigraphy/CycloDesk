# Tools: decompose ####
#
# Empirical mode decomposition with DecomposeR (Wouters et al. 2022), singular spectrum analysis, and methods of Acycle
# that CycloDesk lacked: the Lomb-Scargle periodogram of unevenly sampled series (lomb), stratigraphic correlation by
# dynamic time warping (dtw), recurrence plots and Fourier cross-spectral analysis (coherence, phase, lead-lag).
# DecomposeR, lomb and dtw are suggested packages: each tool checks for them when it runs.

# Helpers ####

cd_dc_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("This tool needs the R package '%s': install.packages(\"%s\")", pkg, pkg), call. = FALSE)
  }
  invisible(TRUE)
}

# Integers from text such as "3, 5-7" or "3 5:7"
cd_dc_ints <- function(s) {
  if (is.numeric(s)) {
    return(unique(as.integer(s[is.finite(s)])))
  }
  s <- trimws(paste(as.character(s), collapse = " "))
  if (!length(s) || !nzchar(s)) {
    return(integer())
  }
  parts <- strsplit(s, "[,; ]+")[[1]]
  out <- integer()
  for (q in parts) {
    if (grepl("^[0-9]+[-:][0-9]+$", q)) {
      ab <- as.integer(strsplit(q, "[-:]")[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else if (grepl("^[0-9]+$", q)) {
      out <- c(out, as.integer(q))
    }
  }
  unique(out)
}

# Groups of integers separated by semicolons: "1, 2; 3-5"
cd_dc_groups <- function(s) {
  s <- trimws(paste(as.character(s), collapse = " "))
  if (!nzchar(s)) {
    return(list())
  }
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  out <- list()
  for (q in parts) {
    v <- cd_dc_ints(q)
    if (length(v)) {
      out[[length(out) + 1]] <- v
    }
  }
  out
}

# Text of integer groups for code: "1, 2", "3, 4"
cd_dc_join_groups <- function(grps) {
  out <- character(length(grps))
  for (i in seq_along(grps)) {
    out[i] <- paste(grps[[i]], collapse = ", ")
  }
  out
}

# Code text of an integer vector: 4 or c(3, 5)
cd_dc_vec <- function(v) {
  if (length(v) == 1) {
    return(as.character(v))
  }
  paste0("c(", paste(v, collapse = ", "), ")")
}

# Messages and printed output of an expression
cd_dc_capture <- function(expr) {
  env <- new.env()
  env$msgs <- character()
  out <- utils::capture.output(withCallingHandlers(expr, message = function(m) {
    env$msgs <- c(env$msgs, sub("\n$", "", conditionMessage(m)))
    invokeRestart("muffleMessage")
  }))
  txt <- c(env$msgs, out)
  txt[nzchar(trimws(txt))]
}

# Mean period of a zero-mean mode from its zero crossings
cd_dc_zc_period <- function(x, y) {
  y <- y - mean(y, na.rm = TRUE)
  sg <- sign(y[is.finite(y)])
  sg <- sg[sg != 0]
  nz <- sum(diff(sg) != 0)
  if (nz < 2) {
    return(NA_real_)
  }
  2 * diff(range(x)) / nz
}

# x limits of the zoom in the tool view, applied to all panels of a figure with a common x axis
cd_dc_vx <- function(xlim) {
  v <- getOption("CycloDesk.view")
  if (!is.null(v) && length(v$x) == 2 && all(is.finite(v$x)) && diff(v$x) != 0) {
    return(sort(v$x))
  }
  xlim
}

# Log axis with plain numbers (periods, ratios)
cd_dc_logaxis <- function(side) {
  at <- graphics::axTicks(side)
  graphics::axis(side, at = at, labels = cd_fmt_period(at))
}

# Range of positive values without the most extreme 0.5% at each end (spikes of instantaneous frequencies)
cd_dc_robust <- function(v) {
  v <- v[is.finite(v) & v > 0]
  if (length(v) < 10) {
    return(range(v))
  }
  stats::quantile(v, c(0.005, 0.995), names = FALSE)
}

# Modes of a decomposition table (depth, signal, mode columns; last columns residue or trend) or of one series
cd_dc_modes_of <- function(d) {
  d <- as.data.frame(d)
  d <- d[!is.na(d[[1]]), , drop = FALSE]
  d <- d[order(d[[1]]), , drop = FALSE]
  nm <- names(d)
  if (ncol(d) < 2) {
    stop("the data file needs at least two columns", call. = FALSE)
  }
  if (ncol(d) == 2) {
    cols <- 2L
  } else {
    cols <- 2:ncol(d)
    cols <- cols[tolower(nm[cols]) != "signal"]
  }
  resid <- grepl("^(residue|trend)", tolower(nm[cols]))
  list(d = d, x = d[[1]], cols = cols, names = nm[cols], residue = resid, table = ncol(d) > 2)
}

cd_dc_pick_modes <- function(md, s) {
  k <- length(md$cols)
  sel <- cd_dc_ints(s)
  if (!length(sel)) {
    sel <- which(!md$residue)
    if (!length(sel)) {
      sel <- seq_len(k)
    }
  }
  bad <- sel[sel < 1 | sel > k]
  if (length(bad)) {
    stop(sprintf("mode %s does not exist: the file has %d mode column(s) (%s)", paste(bad, collapse = ", "), k,
        paste(md$names, collapse = ", ")), call. = FALSE)
  }
  sel
}

# Instantaneous frequency and amplitude of the columns of m (one mode each) at the depths x
cd_dc_pulse <- function(x, m, method = "HT", delta = NULL, lag = 1) {
  m <- as.matrix(m)
  n <- nrow(m)
  k <- ncol(m)
  f <- matrix(NA_real_, n, k)
  a <- matrix(NA_real_, n, k)
  idt <- NULL
  notes <- character()
  if (method %in% c("HT", "NHT", "DQ")) {
    run1 <- function(mm) {
      tryCatch(suppressWarnings(DecomposeR::inst.pulse(m = mm, dt = x, last = TRUE, plot = FALSE, method = method,
          delta = delta)), error = function(e) { e })
    }
    pl <- run1(m)
    if (!inherits(pl, "error")) {
      f[] <- as.matrix(pl$f)
      a[] <- as.matrix(pl$a)
      if (!is.null(pl$idt)) {
        idt <- as.matrix(pl$idt)
      }
    } else {
      # one mode at a time, so that a mode on which the normalisation fails does not stop the others
      nfail <- 0L
      msg <- conditionMessage(pl)
      if (method == "DQ") {
        idt <- matrix(NA_real_, n, k)
      }
      for (j in seq_len(k)) {
        pj <- if (k == 1) { pl } else { run1(m[, j]) }
        if (inherits(pj, "error")) {
          nfail <- nfail + 1L
          msg <- conditionMessage(pj)
          notes <- c(notes, sprintf("Method %s failed for mode %d (%s): its frequency is left empty.", method, j, msg))
          next
        }
        f[, j] <- as.numeric(pj$f)
        a[, j] <- as.numeric(pj$a)
        if (method == "DQ" && !is.null(pj$idt)) {
          idt[, j] <- as.numeric(pj$idt)
        }
      }
      if (nfail == k) {
        stop(sprintf("DecomposeR::inst.pulse (method %s) failed: %s%s", method, msg,
            if (method != "HT") { ". The normalisation of NHT and DQ can fail: try HT or GZC." } else { "" }), call. = FALSE)
      }
    }
  } else if (method == "GZC") {
    for (j in seq_len(k)) {
      g <- tryCatch(suppressWarnings(DecomposeR::gzc(m = m[, j], dt = x, output = 2, dtout = x, warn = FALSE)),
          error = function(e) { e })
      if (inherits(g, "error")) {
        notes <- c(notes, sprintf("GZC failed for mode %d (%s): its frequency is left empty.", j, conditionMessage(g)))
        next
      }
      f[, j] <- as.numeric(g$f)
      a[, j] <- as.numeric(g$a)
    }
  } else {
    dx <- if (is.null(delta)) { cd_dt(x) } else { delta }
    xe <- seq(min(x), max(x), by = dx)
    for (j in seq_len(k)) {
      ye <- stats::approx(x, m[, j], xe, rule = 2)$y
      h <- DecomposeR::HilbertTransform(ye)
      fe <- DecomposeR::InstantaneousFrequency(h, xe, method = "chain", lag = lag)
      ae <- DecomposeR::HilbertEnvelope(h)
      f[, j] <- stats::approx(xe, fe, x, rule = 2)$y
      a[, j] <- stats::approx(xe, ae, x, rule = 2)$y
    }
  }
  f[!is.finite(f) | f <= 0] <- NA
  list(f = f, a = a, idt = idt, notes = notes)
}

# R code of cd_dc_pulse: f and a matrices from the columns cols of dat
cd_dc_pulse_code <- function(cols, method, delta = NULL, lag = 1) {
  cv <- cd_dc_vec(cols)
  dl <- if (is.null(delta)) { "" } else { sprintf(", delta = %s", cd_fmt(delta)) }
  if (method == "HT") {
    return(c(sprintf("pl <- DecomposeR::inst.pulse(m = as.matrix(dat[, %s]), dt = dat[, 1], last = TRUE, plot = FALSE, method = %s%s)",
        cv, cd_dep(method), dl), "f <- as.matrix(pl$f)", "a <- as.matrix(pl$a)"))
  }
  if (method %in% c("NHT", "DQ")) {
    # one mode at a time: the normalisation can fail on a mode
    return(c(sprintf("cols <- %s", cv), "f <- matrix(NA, nrow(dat), length(cols))", "a <- f",
        "for (k in seq_along(cols)) {",
        sprintf("  pl <- try(DecomposeR::inst.pulse(m = dat[, cols[k]], dt = dat[, 1], last = TRUE, plot = FALSE, method = %s%s))",
            cd_dep(method), dl),
        "  if (!inherits(pl, \"try-error\")) {", "    f[, k] <- pl$f", "    a[, k] <- pl$a", "  }", "}"))
  }
  if (method == "GZC") {
    return(c(sprintf("cols <- %s", cv), "f <- matrix(NA, nrow(dat), length(cols))", "a <- f",
        "for (k in seq_along(cols)) {",
        "  g <- DecomposeR::gzc(m = dat[, cols[k]], dt = dat[, 1], output = 2, dtout = dat[, 1], warn = FALSE)",
        "  f[, k] <- g$f", "  a[, k] <- g$a", "}"))
  }
  c(sprintf("cols <- %s", cv), "f <- matrix(NA, nrow(dat), length(cols))", "a <- f",
      sprintf("xe <- seq(min(dat[, 1]), max(dat[, 1]), by = %s)", if (is.null(delta)) { "median(diff(dat[, 1]))" } else { cd_fmt(delta) }),
      "for (k in seq_along(cols)) {",
      "  h <- DecomposeR::HilbertTransform(approx(dat[, 1], dat[, cols[k]], xe, rule = 2)$y)",
      sprintf("  f[, k] <- approx(xe, DecomposeR::InstantaneousFrequency(h, xe, method = \"chain\", lag = %d), dat[, 1], rule = 2)$y",
          as.integer(lag)),
      "  a[, k] <- approx(xe, DecomposeR::HilbertEnvelope(h), dat[, 1], rule = 2)$y", "}",
      "f[!is.finite(f) | f <= 0] <- NA")
}

cd_dc_pulse_methods <- function() {
  c("Hilbert transform (HT)" = "HT", "Normalised Hilbert transform (NHT)" = "NHT", "Direct quadrature (DQ)" = "DQ",
      "Generalised zero-crossing (GZC)" = "GZC", "Hilbert transform, analytic derivative (chain rule)" = "chain")
}

cd_dc_pulse_params <- function() {
  list(
    cd_sel("method", "Instantaneous frequency method", cd_dc_pulse_methods(),
        help = paste("HT: Hilbert transform; NHT and DQ first normalise the mode to unit amplitude (Huang et al. 2009),",
            "which can fail; GZC: generalised zero crossing, robust, precise to a quarter period (needs simple modes);",
            "chain: Hilbert transform with the analytic derivative of the phase.")),
    cd_num("delta", "Interpolation interval for the Hilbert transform (0 = automatic)", 0, min = 0,
        show_if = list(param = "method", values = c("HT", "NHT", "chain")),
        help = "Uneven series are interpolated before the transform; 0 uses the greatest common divisor of the spacing (HT, NHT) or the median spacing (chain)."),
    cd_num("lag", "Differentiation lag (samples)", 1, min = 1, step = 1, show_if = list(param = "method", values = "chain")))
}

cd_dc_delta <- function(p) {
  if (is.null(p$delta) || !is.finite(p$delta) || p$delta <= 0) {
    return(NULL)
  }
  p$delta
}

# Running median of a period curve (k points, odd)
cd_dc_smooth <- function(y, k) {
  if (is.null(k) || !is.finite(k) || k < 3) {
    return(y)
  }
  k <- as.integer(k)
  if (k %% 2 == 0) {
    k <- k + 1L
  }
  ok <- is.finite(y)
  if (sum(ok) <= k) {
    return(y)
  }
  out <- y
  out[ok] <- stats::runmed(y[ok], k, endrule = "median")
  out
}

# Stacked panels with a common x axis (signal and modes)
cd_dc_plot_stack <- function(x, mat, labels, xlab = "Depth / time", cols = NULL, max_panels = 12) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  mat <- as.matrix(mat)
  np <- ncol(mat)
  note <- NULL
  if (np > max_panels) {
    note <- sprintf("first %d of %d panels shown: list the modes to show", max_panels, np)
    mat <- mat[, seq_len(max_panels), drop = FALSE]
    labels <- labels[seq_len(max_panels)]
    np <- max_panels
  }
  if (is.null(cols)) {
    cols <- rep("#1f4e79", np)
  }
  graphics::layout(matrix(seq_len(np), ncol = 1))
  cd_par()
  graphics::par(mar = c(0.25, 4.6, 0.25, 1), oma = c(3.6, 0, if (is.null(note)) { 0.5 } else { 1.6 }, 0), cex.axis = 0.75)
  xl <- cd_dc_vx(range(x, finite = TRUE))
  for (i in seq_len(np)) {
    y <- mat[, i]
    inx <- x >= min(xl) & x <= max(xl) & is.finite(y)
    yl <- if (any(inx)) { range(y[inx]) } else { range(y, finite = TRUE) }
    if (i == np) {
      vw <- cd_view_lims(range(x, finite = TRUE), yl, x = x, y = y, last = TRUE)
      xl <- vw$x
      yl <- vw$y
    }
    graphics::plot(x, y, type = "n", xlim = xl, ylim = yl, xaxt = "n", xlab = "", ylab = "")
    graphics::grid(col = "grey92", lty = 1)
    graphics::abline(h = 0, col = "grey75", lty = 3)
    graphics::lines(x, y, col = cols[i], lwd = 0.9)
    graphics::mtext(labels[i], side = 2, line = 3.4, las = 0, cex = 0.7)
    if (i == np) {
      graphics::axis(1)
      graphics::mtext(xlab, side = 1, line = 2.4, cex = 0.85)
    }
  }
  if (!is.null(note)) {
    graphics::mtext(note, side = 3, outer = TRUE, line = 0.3, cex = 0.8, col = "grey40")
  }
  invisible(NULL)
}

# Base-R singular spectrum analysis: elementary reconstructed components of an evenly spaced series
cd_dc_ssa <- function(y, L, ncomp) {
  N <- length(y)
  L <- as.integer(L)
  if (!is.finite(L) || L < 2 || L > N / 2) {
    stop(sprintf("the window length must be between 2 and %d points (half the record)", floor(N / 2)), call. = FALSE)
  }
  K <- N - L + 1L
  X <- matrix(0, L, K)
  for (j in seq_len(L)) {
    X[j, ] <- y[j:(j + K - 1L)]
  }
  e <- eigen(tcrossprod(X), symmetric = TRUE)
  lam <- pmax(e$values, 0)
  ncomp <- min(as.integer(ncomp), L, sum(lam > 1e-12 * lam[1]))
  w <- pmin(seq_len(N), L, K, N - seq_len(N) + 1L)
  rec <- matrix(0, N, ncomp)
  for (i in seq_len(ncomp)) {
    u <- e$vectors[, i]
    v <- as.numeric(crossprod(X, u))
    acc <- numeric(N)
    for (j in seq_len(L)) {
      idx <- j:(j + K - 1L)
      acc[idx] <- acc[idx] + u[j] * v
    }
    rec[, i] <- acc / w
  }
  list(rec = rec, sigma = sqrt(lam[seq_len(ncomp)]), share = lam[seq_len(ncomp)] / sum(lam))
}

cd_dc_ssa_code <- function(L, ncomp) {
  c(sprintf("L <- %d", as.integer(L)), "N <- length(y)", "K <- N - L + 1",
      "X <- matrix(0, L, K)", "for (j in seq_len(L)) {", "  X[j, ] <- y[j:(j + K - 1)]", "}",
      "e <- eigen(tcrossprod(X), symmetric = TRUE)", "w <- pmin(seq_len(N), L, K, N - seq_len(N) + 1)",
      sprintf("rec <- matrix(0, N, %d)", as.integer(ncomp)),
      "for (i in seq_len(ncol(rec))) {", "  u <- e$vectors[, i]", "  v <- as.numeric(crossprod(X, u))", "  acc <- numeric(N)",
      "  for (j in seq_len(L)) {", "    idx <- j:(j + K - 1)", "    acc[idx] <- acc[idx] + u[j] * v", "  }",
      "  rec[, i] <- acc / w", "}")
}

# Two-column outputs for chosen columns of a table
cd_dc_series_outputs <- function(file, tag, x, mat, names, sel, var_prefix = "res_") {
  o <- list()
  for (k in sel) {
    lab <- gsub("[^A-Za-z0-9_.-]", "_", names[k])
    o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-", tag, "_", lab)), data.frame(x, mat[, k]),
        var = paste0(var_prefix, k))
  }
  o
}

# Recurrence quantification of a binary recurrence matrix (lines of at least lmin points)
cd_dc_rqa <- function(R, lmin = 2) {
  n <- nrow(R)
  rr <- (sum(R) - n) / (n * n - n)
  dl <- integer()
  for (k in seq_len(n - 1)) {
    r <- R[cbind(seq_len(n - k), (1 + k):n)]
    if (!any(r)) {
      next
    }
    rl <- rle(r)
    dl <- c(dl, rl$lengths[rl$values])
  }
  vl <- integer()
  for (j in seq_len(n)) {
    r <- R[, j]
    r[j] <- FALSE
    rl <- rle(r)
    vl <- c(vl, rl$lengths[rl$values])
  }
  dd <- dl[dl >= lmin]
  vv <- vl[vl >= lmin]
  ent <- NA_real_
  if (length(dd)) {
    pr <- table(dd) / length(dd)
    ent <- -sum(pr * log(pr))
  }
  data.frame(points = n, RR = rr, DET = if (length(dl)) { sum(dd) / sum(dl) } else { NA_real_ },
      L_mean = if (length(dd)) { mean(dd) } else { NA_real_ }, L_max = if (length(dd)) { max(dd) } else { NA_real_ },
      ENTR = ent, LAM = if (length(vl)) { sum(vv) / sum(vl) } else { NA_real_ },
      TT = if (length(vv)) { mean(vv) } else { NA_real_ })
}

# Default delay: first lag at which the autocorrelation falls below 1/e
cd_dc_tau <- function(d) {
  s <- cd_even(d)
  n <- nrow(s)
  ac <- stats::acf(s$y, lag.max = max(2, floor(n / 4)), plot = FALSE)$acf[-1]
  k <- which(ac < exp(-1))
  if (!length(k)) {
    return(1)
  }
  max(1, min(k[1], floor(n / 10)))
}

cd_dc_step_patterns <- function() {
  c("symmetric2 (default, normalisable)" = "symmetric2", "symmetric1" = "symmetric1",
      "asymmetric (needed for open begin)" = "asymmetric", "symmetricP05 (slope limited)" = "symmetricP05",
      "symmetricP1 (slope limited)" = "symmetricP1", "symmetricP2 (slope limited)" = "symmetricP2",
      "asymmetricP05" = "asymmetricP05", "asymmetricP1" = "asymmetricP1", "asymmetricP2" = "asymmetricP2",
      "Rabiner-Juang type IIIc" = "typeIIIc", "Rabiner-Juang type IVc" = "typeIVc")
}

# Even resampling of a record with at most n points
cd_dc_resample <- function(d, n) {
  s <- cd_clean(d)
  m <- min(as.integer(n), nrow(s))
  if (m < 10) {
    stop("each record needs at least 10 points", call. = FALSE)
  }
  xo <- seq(min(s$x), max(s$x), length.out = m)
  data.frame(x = xo, y = stats::approx(s$x, s$y, xo)$y)
}

cd_dc_prep <- function(y, how) {
  if (how == "z") {
    return((y - mean(y)) / stats::sd(y))
  }
  if (how == "dz") {
    i <- seq_along(y)
    r <- stats::residuals(stats::lm(y ~ i))
    return(r / stats::sd(r))
  }
  y
}

cd_dc_prep_code <- function(v, how) {
  if (how == "z") {
    return(sprintf("%s <- (%s - mean(%s)) / sd(%s)", v, v, v, v))
  }
  if (how == "dz") {
    return(c(sprintf("%s <- residuals(lm(%s ~ seq_along(%s)))", v, v, v), sprintf("%s <- %s / sd(%s)", v, v, v)))
  }
  character()
}

# Tools ####

cd_tools_decompose <- function() {
  TS <- "Time Series"
  G <- "Empirical mode decomposition (DecomposeR)"
  nimf_default <- function(d) {
    n <- sum(stats::complete.cases(d[, 1:2]))
    as.integer(min(12, max(2, floor(log2(max(n, 8))) - 3)))
  }
  list(
    cd_tool("dc_emd", TS, "EMD / ensemble EMD (extricate)", group = G, icon = "layer-group",
        heavy = function(p) { !identical(p$method, "emd") },
        info = paste("Splits the series into intrinsic mode functions (IMFs, from short to long periods) plus a residue",
            "and the removed trend, by (ensemble) empirical mode decomposition. Writes all modes as one table (input for",
            "Instantaneous frequency and Ratios), chosen modes or the sum of modes as series, and checks of the decomposition.",
            "Uneven sampling is allowed."),
        uses = paste("DecomposeR::extricate, respace, mode.bind, mode.out, integrity, parsimony, symmetry, check.emd,",
            "plot_emd"),
        params = list(
          cd_sel("method", "Method", c("Ensemble EMD (EEMD, added white noise)" = "eemd", "EMD (no added noise)" = "emd")),
          cd_num("nimf", "Number of IMFs", nimf_default, min = 1, max = 30, step = 1,
              help = "The residue (and, unless bound to it, the removed trend) is added as an extra mode."),
          cd_num("comb", "Ensemble size (decompositions combined, even)", 50, min = 2, step = 2,
              show_if = list(param = "method", values = "eemd")),
          cd_num("factor_noise", "Noise amplitude factor", 3, min = 0, show_if = list(param = "method", values = "eemd")),
          cd_sel("unit_noise", "Noise amplitude unit", c("Mean of the lag-1 differences" = "1stdiff", "Data units" = "native"),
              show_if = list(param = "method", values = "eemd")),
          cd_chk("mirror_noise", "Mirrored noise (cancels when combined)", TRUE, show_if = list(param = "method", values = "eemd")),
          cd_num("sifting", "Sifting iterations", 10, min = 1, step = 1),
          cd_sel("remove", "Removed before decomposition (added back as a mode)", c("Linear trend" = "lin.trend", "Mean" = "mean")),
          cd_chk("bind", "Add the removed trend to the residue (one mode instead of two)", FALSE),
          cd_sel("interp", "Resampling", c("None (uneven sampling allowed)" = "none",
              "Even, at the greatest common divisor of the spacing (respace)" = "gcrd", "Even, at an own interval" = "dt")),
          cd_num("delta", "Sampling interval", function(d) { signif(cd_dt(d[, 1]), 4) }, min = 0,
              show_if = list(param = "interp", values = "dt"),
              help = "respace only refines: the interval must divide the greatest common divisor of the depth steps."),
          cd_num("seed", "Random seed", 42, step = 1, show_if = list(param = "method", values = "eemd")),
          cd_txt("merge", "Merge modes (groups separated by ;)", "",
              help = "For example 6, 7 or 1, 2; 7, 8 (mode.bind). Mode numbers of the decomposition before merging."),
          cd_txt("series", "Write these modes as two-column series", "", help = "For example 4 or 4, 6 (numbers after merging)."),
          cd_txt("sum", "Write the sum of these modes as a series", "",
              help = "For example 3-5: a band-pass reconstruction (mode.out, keep). Numbers after merging."),
          cd_chk("checks", "Check the decomposition (integrity, parsimony, symmetry, check.emd)", TRUE),
          cd_sel("show", "Figure", c("Signal and all modes" = "modes", "DecomposeR plot_emd (modes listed below)" = "plot_emd")),
          cd_txt("fig_modes", "Modes in the figure (empty = all)", "")),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("DecomposeR")
          s <- cd_clean(d)
          if (nrow(s) < 20) {
            stop("the series needs at least 20 points", call. = FALSE)
          }
          x <- s$x
          y <- s$y
          code <- c("dat <- dat[stats::complete.cases(dat[, 1:2]), 1:2]", "dat <- dat[order(dat[, 1]), ]")
          txt <- character()
          if (p$interp != "none") {
            delta <- if (p$interp == "dt") { p$delta } else { NULL }
            if (!is.null(delta) && !(is.finite(delta) && delta > 0)) {
              stop("give a positive sampling interval", call. = FALSE)
            }
            rs <- tryCatch(suppressWarnings(DecomposeR::respace(dt = x, xy = y, delta = delta)), error = function(e) { e })
            if (inherits(rs, "error")) {
              stop("DecomposeR::respace failed: ", conditionMessage(rs), call. = FALSE)
            }
            x <- as.numeric(rs$dt)
            y <- as.numeric(rs$xy)
            code <- c(code, sprintf("rs <- DecomposeR::respace(dt = dat[, 1], xy = dat[, 2]%s)",
                if (is.null(delta)) { "" } else { sprintf(", delta = %s", cd_fmt(delta)) }),
                "dat <- data.frame(rs$dt, rs$xy)")
            txt <- c(txt, sprintf("Resampled at an interval of %s (%d points).", cd_fmt(stats::median(diff(x))), length(x)))
          }
          eemd <- !identical(p$method, "emd")
          comb <- if (eemd) { max(2L, 2L * as.integer(round(p$comb / 2))) } else { 2L }
          fn <- if (eemd) { p$factor_noise } else { 0 }
          nimf <- as.integer(p$nimf)
          if (!is.finite(nimf) || nimf < 1) {
            stop("give at least one IMF", call. = FALSE)
          }
          if (eemd) {
            set.seed(p$seed)
          }
          dec <- tryCatch(DecomposeR::extricate(y, x, nimf = nimf, comb = comb, factor_noise = fn, unit_noise = p$unit_noise,
              mirror_noise = isTRUE(p$mirror_noise), sifting = as.integer(p$sifting), remove = p$remove, bind = isTRUE(p$bind),
              speak = FALSE), error = function(e) { e })
          if (inherits(dec, "error")) {
            stop("DecomposeR::extricate failed: ", conditionMessage(dec), ". Try fewer IMFs.", call. = FALSE)
          }
          code <- c(code, if (eemd) { sprintf("set.seed(%s)", cd_fmt(p$seed)) } else { character() },
              sprintf(paste0("dec <- DecomposeR::extricate(dat[, 2], dat[, 1], nimf = %d, comb = %d, factor_noise = %s,",
                  " unit_noise = %s, mirror_noise = %s, sifting = %d, remove = %s, bind = %s)"), nimf, comb, cd_fmt(fn),
                  cd_dep(p$unit_noise), isTRUE(p$mirror_noise), as.integer(p$sifting), cd_dep(p$remove), isTRUE(p$bind)))
          k <- ncol(dec$m)
          nm <- c(paste0("imf_", seq_len(nimf)), if (isTRUE(p$bind)) { "residue" } else { c("residue", "trend") })
          if (length(nm) != k) {
            nm <- paste0("mode_", seq_len(k))
          }
          # Merge modes, the group with the highest numbers first so that the other numbers stay valid
          grps <- cd_dc_groups(p$merge)
          if (length(grps)) {
            mins <- numeric(length(grps))
            for (i in seq_along(grps)) {
              mins[i] <- min(grps[[i]])
            }
            for (i in order(-mins)) {
              gi <- sort(grps[[i]])
              if (length(gi) < 2 || any(gi < 1 | gi > ncol(dec$m))) {
                stop(sprintf("cannot merge modes %s: give two or more of the %d modes", paste(gi, collapse = ", "), k),
                    call. = FALSE)
              }
              dec <- DecomposeR::mode.bind(dec, mode = gi)
              lab <- paste(sub("^imf_", "", nm[gi]), collapse = "+")
              if (all(grepl("^imf_", nm[gi]))) {
                lab <- paste0("imf_", lab)
              } else if (all(nm[gi] %in% c("residue", "trend"))) {
                lab <- "residue"
              } else {
                lab <- paste0("mode_", lab)
              }
              rest <- nm[-gi]
              nm <- append(rest, lab, after = min(gi) - 1)
              code <- c(code, sprintf("dec <- DecomposeR::mode.bind(dec, mode = %s)", cd_dc_vec(gi)))
            }
            k <- ncol(dec$m)
          }
          m <- as.matrix(dec$m)
          colnames(m) <- nm
          tab <- data.frame(depth = dec$dt, signal = dec$xy, m, check.names = FALSE)
          tag <- if (eemd) { "eemd" } else { "emd" }
          o <- list(cd_output(cd_out_name(file, paste0("-", tag)), tab, header = TRUE))
          code <- c(code, "res <- data.frame(depth = dec$dt, signal = dec$xy, dec$m)",
              sprintf("names(res) <- %s", cd_dep(c("depth", "signal", nm))))
          sel <- cd_dc_ints(p$series)
          sel <- sel[sel >= 1 & sel <= k]
          if (length(sel)) {
            so <- cd_dc_series_outputs(file, tag, dec$dt, m, nm, sel)
            o <- c(o, so)
            for (j in seq_along(sel)) {
              code <- c(code, sprintf("res_%d <- data.frame(dec$dt, dec$m[, %d])", sel[j], sel[j]))
            }
          }
          ks <- cd_dc_ints(p$sum)
          ks <- ks[ks >= 1 & ks <= k]
          if (length(ks)) {
            part <- DecomposeR::mode.out(dec, keep = ks, adjust = TRUE)$xy
            o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-", tag, "_sum", paste(ks, collapse = "_"))),
                data.frame(dec$dt, as.numeric(part)), var = "res_sum")
            code <- c(code, sprintf("res_sum <- data.frame(dec$dt, DecomposeR::mode.out(dec, keep = %s, adjust = TRUE)$xy)",
                cd_dc_vec(ks)))
          }
          # Summary of the modes
          per <- numeric(k)
          for (j in seq_len(k)) {
            per[j] <- cd_dc_zc_period(dec$dt, m[, j])
          }
          sdv <- numeric(k)
          for (j in seq_len(k)) {
            sdv[j] <- stats::sd(m[, j])
          }
          txt <- c(txt, sprintf("%s: %d modes (%d IMFs%s).", if (eemd) { sprintf("EEMD of %d decompositions", comb) } else { "EMD" },
              k, nimf, if (length(grps)) { ", after merging" } else { "" }),
              "Mode: mean period from zero crossings, standard deviation")
          for (j in seq_len(k)) {
            txt <- c(txt, sprintf("  %d %s: %s, sd %s", j, nm[j],
                if (grepl("^(residue|trend)", nm[j])) { "trend" } else { cd_fmt(signif(per[j], 3)) }, cd_fmt(signif(sdv[j], 3))))
          }
          checks <- NULL
          if (isTRUE(p$checks)) {
            integ <- as.numeric(DecomposeR::integrity(y, dec))
            pars <- as.numeric(DecomposeR::parsimony(dec))
            sym <- as.numeric(DecomposeR::symmetry(dec$m))
            ce <- tryCatch(cd_dc_capture(suppressWarnings(DecomposeR::check.emd(dec, xy = y, timelimit = 5))),
                error = function(e) { paste("check.emd failed:", conditionMessage(e)) })
            checks <- data.frame(mode = seq_len(k), name = nm, mean_period = per, sd = sdv, symmetry = sym)
            o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-", tag, "_checks")), checks, header = TRUE,
                var = "res_checks")
            txt <- c(txt, sprintf("Integrity (signal minus the sum of the modes, cumulated): %s; parsimony: %s (1 = no amplitude added).",
                format(signif(integ, 3)), cd_fmt(signif(pars, 3))),
                "Symmetry per mode (0.5 = symmetric; check.emd flags values above 0.6):",
                paste0("  ", paste(sprintf("%d: %s", seq_len(k), format(round(sym, 2), nsmall = 2)), collapse = ", ")),
                "check.emd:", paste0("  ", ce))
            code <- c(code, "DecomposeR::integrity(dat[, 2], dec)", "DecomposeR::parsimony(dec)",
                "DecomposeR::symmetry(dec$m)", "DecomposeR::check.emd(dec, xy = dat[, 2])",
                "res_checks <- data.frame(mode = seq_len(ncol(dec$m)), symmetry = DecomposeR::symmetry(dec$m))")
          }
          txt <- c(txt, sprintf("All modes written to %s: use it in Instantaneous frequency and amplitude, or Ratios between modes.",
              o[[1]]$name))
          fm <- cd_dc_ints(p$fig_modes)
          fm <- fm[fm >= 1 & fm <= k]
          if (identical(p$show, "plot_emd")) {
            code <- c(code, sprintf("DecomposeR::plot_emd(dec, pdf = FALSE%s)",
                if (length(fm)) { sprintf(", select = %s", cd_dc_vec(fm)) } else { "" }))
          }
          list(result = list(x = dec$dt, signal = dec$xy, m = m, names = nm, dec = dec, tag = tag, checks = checks),
              outputs = o, text = txt, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          k <- ncol(res$m)
          fm <- cd_dc_ints(p$fig_modes)
          fm <- fm[fm >= 1 & fm <= k]
          if (!length(fm)) {
            fm <- seq_len(k)
          }
          if (identical(p$show, "plot_emd")) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            r <- try(DecomposeR::plot_emd(res$dec, pdf = FALSE, select = fm), silent = TRUE)
            if (inherits(r, "try-error")) {
              graphics::par(oldpar)
              graphics::layout(1)
              cd_message_plot("Too many panels for plot_emd: list two to five modes under 'Modes in the figure'")
            }
            return(invisible(NULL))
          }
          mat <- cbind(res$signal, res$m[, fm, drop = FALSE])
          cols <- c("#1f4e79", rep("#c0392b", length(fm)))
          r <- try(cd_dc_plot_stack(res$x, mat, c("signal", res$names[fm]), cols = cols), silent = TRUE)
          if (inherits(r, "try-error")) {
            graphics::layout(1)
            cd_message_plot("The figure is too small for all modes: list fewer modes under 'Modes in the figure'")
          }
        }),
    cd_tool("dc_ssa", TS, "Singular spectrum analysis (SSA)", group = G, icon = "table-list",
        info = paste("Decomposes the series into components of a trajectory matrix (singular spectrum analysis), to be",
            "grouped into trend, cycles and noise. Writes the same table as EMD (input for Instantaneous frequency and",
            "Ratios). Uneven series are interpolated and the components given at the original depths."),
        uses = "base R (eigen decomposition of the trajectory matrix), DecomposeR::simple.ssa (needs Rssa)",
        params = list(
          cd_sel("method", "Implementation", c("Base R (CycloDesk)" = "base", "DecomposeR::simple.ssa (needs the package Rssa)" = "rssa")),
          cd_num("L", "Window length (points)", function(d) { as.integer(min(1000, max(2, round(sum(stats::complete.cases(d[, 1:2])) / 4)))) },
              min = 2, step = 1, help = "Longer than the longest period of interest; at most half the record."),
          cd_num("ncomp", "Number of components", 10, min = 1, max = 100, step = 1),
          cd_sel("remove", "Removed before decomposition (written as the trend)", c("Linear trend" = "trend", "Mean" = "mean",
              "Nothing" = "none")),
          cd_txt("groups", "Group components (groups separated by ;)", "",
              help = "For example 1, 2; 3, 4: each pair of a cycle gives one component. Components not listed stay single."),
          cd_txt("series", "Write these components as two-column series", "", help = "Numbers of the columns after grouping."),
          cd_sel("show", "Figure", c("Signal and components" = "comps", "Share of variance per component" = "norms"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          if (nrow(s) < 20) {
            stop("the series needs at least 20 points", call. = FALSE)
          }
          x <- s$x
          y <- s$y
          ncomp <- as.integer(p$ncomp)
          grps <- cd_dc_groups(p$groups)
          gall <- integer()
          for (g in grps) {
            gall <- c(gall, g)
          }
          if (anyDuplicated(gall)) {
            stop("each component may appear in one group only", call. = FALSE)
          }
          if (any(gall < 1 | gall > ncomp)) {
            stop(sprintf("the groups use components outside 1 to %d", ncomp), call. = FALSE)
          }
          code <- c("dat <- dat[stats::complete.cases(dat[, 1:2]), 1:2]", "dat <- dat[order(dat[, 1]), ]")
          glist <- if (length(grps)) { sprintf("list(%s)", paste(sprintf("c(%s)", cd_dc_join_groups(grps)), collapse = ", ")) } else { "list()" }
          if (identical(p$method, "rssa")) {
            cd_dc_need("DecomposeR")
            cd_dc_need("Rssa")
            r <- DecomposeR::simple.ssa(xy = y, dt = x, n = ncomp, remove = p$remove, groups = grps, plot = FALSE, L = as.integer(p$L))
            m <- as.matrix(r$m)
            nm <- colnames(m)
            if (is.null(nm) || length(nm) != ncol(m)) {
              nm <- paste0("comp_", seq_len(ncol(m)))
            }
            nm[length(nm)] <- if (p$remove == "none") { nm[length(nm)] } else { "trend" }
            share <- NULL
            code <- c(code, sprintf("dec <- DecomposeR::simple.ssa(xy = dat[, 2], dt = dat[, 1], n = %d, remove = %s, groups = %s, plot = FALSE, L = %d)",
                ncomp, cd_dep(p$remove), glist, as.integer(p$L)),
                "res <- data.frame(depth = dec$dt, signal = dec$xy, dec$m)")
            xo <- r$dt
            sig <- r$xy
          } else {
            se <- cd_even(s)
            even <- nrow(se) == nrow(s) && cd_is_even(s$x)
            if (p$remove == "trend") {
              tr <- stats::fitted(stats::lm(se$y ~ se$x))
            } else if (p$remove == "mean") {
              tr <- rep(mean(se$y), nrow(se))
            } else {
              tr <- rep(0, nrow(se))
            }
            ss <- cd_dc_ssa(se$y - tr, p$L, ncomp)
            ncomp <- ncol(ss$rec)
            gall <- gall[gall <= ncomp]
            comps <- list()
            nm <- character()
            for (g in grps) {
              g <- g[g <= ncomp]
              if (!length(g)) {
                next
              }
              v <- numeric(nrow(se))
              for (i in g) {
                v <- v + ss$rec[, i]
              }
              comps[[length(comps) + 1]] <- v
              nm <- c(nm, paste0("comp_", paste(g, collapse = "+")))
            }
            for (i in setdiff(seq_len(ncomp), gall)) {
              comps[[length(comps) + 1]] <- ss$rec[, i]
              nm <- c(nm, paste0("comp_", i))
            }
            me <- matrix(0, nrow(se), length(comps))
            for (j in seq_along(comps)) {
              me[, j] <- comps[[j]]
            }
            noise <- se$y - tr - rowSums(me)
            me <- cbind(me, noise)
            nm <- c(nm, "noise")
            if (p$remove != "none") {
              me <- cbind(me, tr)
              nm <- c(nm, "trend")
            }
            if (even) {
              m <- me
              xo <- se$x
              sig <- se$y
            } else {
              m <- matrix(0, nrow(s), ncol(me))
              for (j in seq_len(ncol(me))) {
                m[, j] <- stats::approx(se$x, me[, j], s$x, rule = 2)$y
              }
              xo <- s$x
              sig <- s$y
            }
            share <- ss$share
            code <- c(code, if (even) { "xe <- dat[, 1]" } else { "xe <- seq(min(dat[, 1]), max(dat[, 1]), by = median(diff(dat[, 1])))" },
                "y <- approx(dat[, 1], dat[, 2], xe)$y",
                switch(p$remove, trend = "tr <- fitted(lm(y ~ xe))", mean = "tr <- rep(mean(y), length(y))", none = "tr <- rep(0, length(y))"),
                "y <- y - tr", cd_dc_ssa_code(p$L, ncomp),
                sprintf("groups <- %s", glist),
                "comps <- NULL", "for (g in groups) {", "  comps <- cbind(comps, rowSums(rec[, g, drop = FALSE]))", "}",
                "comps <- cbind(comps, rec[, setdiff(seq_len(ncol(rec)), unlist(groups)), drop = FALSE])",
                "comps <- cbind(comps, noise = y - rowSums(comps))",
                if (p$remove != "none") { "comps <- cbind(comps, trend = tr)" } else { character() },
                "res <- data.frame(depth = dat[, 1], signal = dat[, 2])",
                "for (j in seq_len(ncol(comps))) {", "  res[[j + 2]] <- approx(xe, comps[, j], dat[, 1], rule = 2)$y", "}")
          }
          colnames(m) <- nm
          tab <- data.frame(depth = xo, signal = sig, m, check.names = FALSE)
          code <- c(code, sprintf("names(res) <- %s", cd_dep(c("depth", "signal", nm))))
          o <- list(cd_output(cd_out_name(file, "-ssa"), tab, header = TRUE))
          sel <- cd_dc_ints(p$series)
          sel <- sel[sel >= 1 & sel <= ncol(m)]
          if (length(sel)) {
            o <- c(o, cd_dc_series_outputs(file, "ssa", xo, m, nm, sel))
            for (j in sel) {
              code <- c(code, sprintf("res_%d <- res[, c(1, %d)]", j, j + 2))
            }
          }
          txt <- c(sprintf("SSA with a window of %d points: %d columns (%s).", as.integer(p$L), ncol(m), paste(nm, collapse = ", ")))
          if (!is.null(share)) {
            txt <- c(txt, "Share of variance of the components:",
                paste0("  ", paste(sprintf("%d: %.1f%%", seq_along(share), 100 * share), collapse = ", ")))
          }
          per <- numeric(ncol(m))
          for (j in seq_len(ncol(m))) {
            per[j] <- cd_dc_zc_period(xo, m[, j])
          }
          txt <- c(txt, "Mean period from zero crossings:",
              paste0("  ", paste(sprintf("%s: %s", nm, cd_fmt_period(per)), collapse = ", ")),
              sprintf("All columns written to %s.", o[[1]]$name))
          list(result = list(x = xo, signal = sig, m = m, names = nm, share = share), outputs = o, text = txt,
              code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          if (identical(p$show, "norms")) {
            if (is.null(res$share)) {
              cd_message_plot("The share of variance is given by the base-R implementation only")
              return(invisible(NULL))
            }
            oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
            on.exit(graphics::par(oldpar))
            cd_par()
            k <- seq_along(res$share)
            vw <- cd_view_lims(range(k), range(100 * res$share), x = k, y = 100 * res$share)
            graphics::plot(k, 100 * res$share, type = "o", pch = 19, col = "#1f4e79", xlab = "Component",
                ylab = "Share of variance (%)", log = "y", xlim = vw$x, ylim = if (vw$on) { vw$y } else { NULL })
            graphics::grid(col = "grey90", lty = 1)
            return(invisible(NULL))
          }
          mat <- cbind(res$signal, res$m)
          r <- try(cd_dc_plot_stack(res$x, mat, c("signal", res$names), cols = c("#1f4e79", rep("#2e8b57", ncol(res$m)))),
              silent = TRUE)
          if (inherits(r, "try-error")) {
            graphics::layout(1)
            cd_message_plot("The figure is too small for all components")
          }
        }),
    cd_tool("dc_pulse", TS, "Instantaneous frequency and amplitude of modes", group = G, icon = "wave-square",
        full_table = TRUE,
        info = paste("Instantaneous frequency, period and amplitude of IMFs: select a decomposition table written by",
            "EMD / SSA (all modes) or a two-column series (one IMF or a filtered cycle). The period curves can be used as",
            "a tracked cycle in Age models > Build an age model > Depth to time."),
        uses = paste("DecomposeR::inst.pulse (HT, NHT, DQ: normalise, dq.algorithm), gzc (gzc.algorithm), HilbertTransform,",
            "HilbertEnvelope, InstantaneousFrequency, gzc.departure, plot_pulse, plot_imf"),
        params = c(list(
          cd_txt("modes", "Modes to analyse (empty = all but residue and trend)", "",
              help = "Positions among the mode columns: 1 = first column after depth and signal (imf_1 of an EMD table).")),
          cd_dc_pulse_params(),
          list(
            cd_num("smooth", "Running median of the period curves (points, 0 = none)", 0, min = 0, step = 2,
                help = "Smooths the written period curves (useful for an age model); the table also keeps the raw values."),
            cd_chk("departure", "Compare with the zero-crossing frequency (gzc.departure)", FALSE,
                show_if = list(param = "method", values = c("HT", "NHT", "DQ", "chain"))),
            cd_sel("show", "Figure", c("Period curves of all modes" = "periods", "One mode: envelope and period" = "mode",
                "Period distribution (plot_pulse)" = "dist", "Spectral population, period vs amplitude (plot_pulse)" = "pop",
                "DecomposeR plot_imf (one mode)" = "imf")),
            cd_num("focus", "Mode for the one-mode figures", 1, min = 1, step = 1,
                show_if = list(param = "show", values = c("mode", "imf"))),
            cd_txt("lines", "Reference periods drawn in the figure", ""))),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("DecomposeR")
          md <- cd_dc_modes_of(d)
          sel <- cd_dc_pick_modes(md, p$modes)
          dd <- md$d[, c(1, md$cols[sel]), drop = FALSE]
          dd <- dd[stats::complete.cases(dd), , drop = FALSE]
          if (nrow(dd) < 10) {
            stop("too few complete rows", call. = FALSE)
          }
          x <- dd[[1]]
          m <- as.matrix(dd[, -1, drop = FALSE])
          nm <- md$names[sel]
          delta <- cd_dc_delta(p)
          pu <- cd_dc_pulse(x, m, p$method, delta = delta, lag = p$lag)
          per <- 1 / pu$f
          k <- ncol(m)
          pers <- per
          for (j in seq_len(k)) {
            pers[, j] <- cd_dc_smooth(per[, j], p$smooth)
          }
          cols_file <- md$cols[sel]
          tab <- data.frame(depth = x)
          for (j in seq_len(k)) {
            tab[[paste0("frequency_", nm[j])]] <- pu$f[, j]
            tab[[paste0("period_", nm[j])]] <- per[, j]
            if (isTRUE(p$smooth >= 3)) {
              tab[[paste0("period_smooth_", nm[j])]] <- pers[, j]
            }
            tab[[paste0("amplitude_", nm[j])]] <- pu$a[, j]
          }
          tag <- paste0("pulse_", p$method)
          o <- list(cd_output(cd_out_name(file, paste0("-", tag)), tab, header = TRUE))
          code <- c("dat <- dat[stats::complete.cases(dat), ]", "dat <- dat[order(dat[, 1]), ]",
              cd_dc_pulse_code(cols_file, p$method, delta = delta, lag = p$lag),
              "per <- 1 / f", "pers <- per")
          if (isTRUE(p$smooth >= 3)) {
            code <- c(code, "for (k in seq_len(ncol(per))) {", "  ok <- is.finite(per[, k])",
                sprintf("  pers[ok, k] <- runmed(per[ok, k], %d, endrule = \"median\")",
                    as.integer(p$smooth) + as.integer(as.integer(p$smooth) %% 2 == 0)), "}")
          }
          code <- c(code, "res <- data.frame(depth = dat[, 1], f, per, a)",
              sprintf("names(res) <- c(\"depth\", %s, %s, %s)", paste(cd_dep(paste0("frequency_", nm)), collapse = ", "),
                  paste(cd_dep(paste0("period_", nm)), collapse = ", "), paste(cd_dep(paste0("amplitude_", nm)), collapse = ", ")))
          txt <- c(pu$notes, sprintf("Method %s, %d mode(s).", names(cd_dc_pulse_methods())[cd_dc_pulse_methods() == p$method], k),
              "Mode: median period (interquartile range), median amplitude")
          for (j in seq_len(k)) {
            q <- stats::quantile(per[, j], c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
            txt <- c(txt, sprintf("  %s: %s (%s to %s), %s", nm[j], cd_fmt(signif(q[2], 3)), cd_fmt(signif(q[1], 3)),
                cd_fmt(signif(q[3], 3)), cd_fmt(signif(stats::median(pu$a[, j], na.rm = TRUE), 3))))
          }
          if (k <= 3) {
            for (j in seq_len(k)) {
              lab <- gsub("[^A-Za-z0-9_.-]", "_", nm[j])
              o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-period_", lab)), data.frame(x, pers[, j]),
                  var = sprintf("per_%d", j))
              o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-amplitude_", lab)), data.frame(x, pu$a[, j]),
                  var = sprintf("amp_%d", j))
              code <- c(code, sprintf("per_%d <- data.frame(dat[, 1], pers[, %d])", j, j),
                  sprintf("amp_%d <- data.frame(dat[, 1], a[, %d])", j, j))
            }
            txt <- c(txt, "Period curves (depth, period) written as series: use one in Age models > Build an age model > Depth to time (tracked period curve).")
          } else {
            txt <- c(txt, "Analyse three modes or fewer to also get each period and amplitude curve as a two-column series.")
          }
          dep <- NULL
          if (isTRUE(p$departure) && p$method != "GZC") {
            dep <- rep(NA_real_, k)
            for (j in seq_len(k)) {
              pl <- DecomposeR::as.pulse(dt = x, f = pu$f[, j], a = pu$a[, j], m = m[, j])
              r <- tryCatch(suppressWarnings(DecomposeR::gzc.departure(pulse = pl)), error = function(e) { NULL })
              if (!is.null(r)) {
                dep[j] <- as.numeric(r)[1]
              }
            }
            txt <- c(txt, "Departure from the zero-crossing frequency (gzc.departure, 1 = identical; empty: GZC failed):",
                paste0("  ", paste(sprintf("%s: %s", nm, cd_fmt_period(dep)), collapse = ", ")))
          }
          list(result = list(x = x, m = m, names = nm, modes = sel, f = pu$f, a = pu$a, per = pers, idt = pu$idt,
              method = p$method, departure = dep), outputs = o, text = txt, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          lines_at <- cd_parse_nums(p$lines)
          k <- ncol(res$m)
          j <- match(as.integer(p$focus), res$modes)
          if (is.na(j)) {
            j <- 1L
          }
          if (p$show %in% c("dist", "pop")) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            pl <- DecomposeR::as.pulse(dt = res$x, f = res$f, a = res$a, m = res$m, mode = res$modes)
            ok <- is.finite(pl$f) & is.finite(pl$a)
            pl$f[!ok] <- stats::median(pl$f[ok])
            pl$a[!ok] <- 0
            r <- try(DecomposeR::plot_pulse(pl, style = if (p$show == "dist") { "d" } else { "p" },
                lines = if (length(lines_at)) { lines_at } else { NULL }), silent = TRUE)
            if (inherits(r, "try-error")) {
              cd_message_plot(paste("plot_pulse failed:", conditionMessage(attr(r, "condition"))))
            }
            return(invisible(NULL))
          }
          if (p$show == "imf") {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            f <- res$f[, j]
            ok <- is.finite(f)
            idt <- if (is.null(res$idt)) { NULL } else { res$idt[, j] }
            pl <- DecomposeR::as.pulse(dt = res$x[ok], m = res$m[ok, j], f = f[ok], a = res$a[ok, j],
                idt = if (is.null(idt)) { NULL } else { idt[ok] })
            r <- try(DecomposeR::plot_imf(pl, flim = range(f[ok]), fline = if (length(lines_at)) { 1 / lines_at } else { NULL }),
                silent = TRUE)
            if (inherits(r, "try-error")) {
              graphics::layout(1)
              cd_message_plot(paste("plot_imf failed:", conditionMessage(attr(r, "condition"))))
            }
            return(invisible(NULL))
          }
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          cols <- cd_cols(k)
          if (p$show == "mode") {
            graphics::layout(matrix(1:2, ncol = 1))
            cd_par()
            xl <- cd_dc_vx(range(res$x))
            y <- res$m[, j]
            graphics::plot(res$x, y, type = "l", col = "#1f4e79", xlim = xl, xlab = "", ylab = res$names[j])
            graphics::grid(col = "grey90", lty = 1)
            graphics::lines(res$x, res$a[, j], col = "#c0392b", lwd = 1.5)
            graphics::lines(res$x, -res$a[, j], col = "#c0392b", lwd = 1.5)
            cd_par()
            pr <- res$per[, j]
            vw <- cd_view_lims(range(res$x), cd_dc_robust(pr), x = res$x, y = pr, logy = TRUE, last = TRUE)
            graphics::plot(res$x, pr, type = "l", col = "#2e8b57", lwd = 1.3, log = "y", xlim = vw$x, ylim = vw$y,
                xlab = "Depth / time", ylab = "Period", yaxt = "n")
            cd_dc_logaxis(2)
            graphics::grid(col = "grey90", lty = 1)
            if (length(lines_at)) {
              graphics::abline(h = lines_at, col = "#c0392b", lty = 2)
            }
            return(invisible(NULL))
          }
          cd_par()
          vw <- cd_view_lims(range(res$x), cd_dc_robust(res$per), x = rep(res$x, k), y = as.numeric(res$per), logy = TRUE)
          graphics::plot(NA, xlim = vw$x, ylim = vw$y, log = "y", xlab = "Depth / time", ylab = "Instantaneous period",
              yaxt = "n")
          cd_dc_logaxis(2)
          graphics::grid(col = "grey90", lty = 1)
          for (i in seq_len(k)) {
            graphics::lines(res$x, res$per[, i], col = cols[i], lwd = 1.1)
          }
          if (length(lines_at)) {
            graphics::abline(h = lines_at, col = "grey30", lty = 2)
          }
          graphics::legend("topright", legend = res$names, col = cols, lwd = 2, bty = "n", cex = 0.8)
        }),
    cd_tool("dc_ratio", TS, "Ratios between modes (e.g. eccentricity / precession)", group = G, icon = "divide",
        full_table = TRUE,
        info = paste("Instantaneous ratios of the frequencies of pairs of modes of a decomposition table (EMD / SSA),",
            "with the product of their amplitudes. Compare them with the ratios of astronomical periods",
            "(e.g. 405 / 21 kyr) to test an astronomical interpretation without an age model."),
        uses = "DecomposeR::inst.ratio, ratios, plot_ratio, inst.pulse, gzc",
        params = c(list(
          cd_txt("modes", "Modes (at least two; empty = all but residue and trend)", "",
              help = "Positions among the mode columns, e.g. 4, 6, 7.")),
          cd_dc_pulse_params(),
          list(
            cd_txt("ref", "Reference periods (expected ratios between them)", "405, 100, 41, 21", numeric = TRUE,
                help = "Periods in any one unit (e.g. kyr); every ratio between them is listed and drawn."),
            cd_txt("pairs", "Pairs in the figure (e.g. 4/6; empty = automatic)", ""),
            cd_sel("show", "Figure", c("Ratios along the record" = "depth", "Ratio population (plot_ratio)" = "pop")))),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("DecomposeR")
          md <- cd_dc_modes_of(d)
          if (!md$table) {
            stop("select a decomposition table (depth, signal, modes) written by EMD or SSA", call. = FALSE)
          }
          sel <- cd_dc_pick_modes(md, p$modes)
          if (length(sel) < 2) {
            stop("give at least two modes", call. = FALSE)
          }
          dd <- md$d[, c(1, md$cols[sel]), drop = FALSE]
          dd <- dd[stats::complete.cases(dd), , drop = FALSE]
          x <- dd[[1]]
          m <- as.matrix(dd[, -1, drop = FALSE])
          nm <- md$names[sel]
          delta <- cd_dc_delta(p)
          pu <- cd_dc_pulse(x, m, p$method, delta = delta, lag = p$lag)
          pl <- DecomposeR::as.pulse(dt = x, f = pu$f, a = pu$a, mode = sel)
          ra <- DecomposeR::inst.ratio(pulse = pl, plot = FALSE)
          lr <- as.character(ra$lr[1, ])
          rat <- as.matrix(ra$ratio)
          pw <- sqrt(as.matrix(ra$rpwr))
          np <- ncol(rat)
          tab <- data.frame(depth = x)
          summ <- data.frame(pair = lr, modes = character(np), median_ratio = NA_real_, weighted_median = NA_real_,
              q25 = NA_real_, q75 = NA_real_)
          for (j in seq_len(np)) {
            lab <- gsub("/", "_", lr[j], fixed = TRUE)
            tab[[paste0("ratio_", lab)]] <- rat[, j]
            tab[[paste0("amplitude_", lab)]] <- pw[, j]
            ab <- as.integer(strsplit(lr[j], "/", fixed = TRUE)[[1]])
            summ$modes[j] <- paste(md$names[ab], collapse = "/")
            ok <- is.finite(rat[, j]) & is.finite(pw[, j])
            if (any(ok)) {
              q <- stats::quantile(rat[ok, j], c(0.25, 0.5, 0.75), names = FALSE)
              summ$median_ratio[j] <- q[2]
              summ$q25[j] <- q[1]
              summ$q75[j] <- q[3]
              o2 <- order(rat[ok, j])
              cw <- cumsum(pw[ok, j][o2])
              summ$weighted_median[j] <- rat[ok, j][o2][which(cw >= 0.5 * cw[length(cw)])[1]]
            }
          }
          o <- list(cd_output(cd_out_name(file, paste0("-ratio_", p$method)), tab, header = TRUE),
              cd_output(cd_out_name(file, paste0("-ratio_", p$method, "_summary")), summ, header = TRUE, var = "res_summary"))
          if (np <= 3) {
            for (j in seq_len(np)) {
              lab <- gsub("/", "_", lr[j], fixed = TRUE)
              o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-ratio_", lab)), data.frame(x, rat[, j]),
                  var = sprintf("res_%d", j))
            }
          }
          ref <- p$ref[is.finite(p$ref) & p$ref > 0]
          refr <- NULL
          txt <- c(pu$notes, sprintf("%d pairs of modes (frequency of the first / frequency of the second = period ratio).", np),
              "Pair: median ratio (interquartile range), amplitude-weighted median")
          for (j in seq_len(np)) {
            txt <- c(txt, sprintf("  %s (%s): %s (%s to %s), %s", lr[j], summ$modes[j], cd_fmt(signif(summ$median_ratio[j], 3)),
                cd_fmt(signif(summ$q25[j], 3)), cd_fmt(signif(summ$q75[j], 3)), cd_fmt(signif(summ$weighted_median[j], 3))))
          }
          if (length(ref) >= 2) {
            refr <- DecomposeR::ratios(ref)
            txt <- c(txt, "Expected ratios of the reference periods:",
                paste0("  ", paste(sprintf("%s/%s = %s", cd_fmt_period(refr$x2), cd_fmt_period(refr$x1),
                    cd_fmt_period(refr$ratio)), collapse = ", ")))
          }
          code <- c("dat <- dat[stats::complete.cases(dat), ]", "dat <- dat[order(dat[, 1]), ]",
              cd_dc_pulse_code(md$cols[sel], p$method, delta = delta, lag = p$lag),
              sprintf("pl <- DecomposeR::as.pulse(dt = dat[, 1], f = f, a = a, mode = %s)", cd_dc_vec(sel)),
              "rat <- DecomposeR::inst.ratio(pulse = pl, plot = FALSE)",
              "res <- data.frame(depth = dat[, 1], rat$ratio, sqrt(rat$rpwr))",
              "names(res) <- c(\"depth\", paste0(\"ratio_\", rat$lr[1, ]), paste0(\"amplitude_\", rat$lr[1, ]))",
              "res_summary <- data.frame(pair = rat$lr[1, ], median_ratio = NA)")
          code <- c(code, "for (j in seq_len(ncol(rat$ratio))) {",
              "  res_summary$median_ratio[j] <- median(rat$ratio[, j], na.rm = TRUE)", "}")
          for (j in seq_len(min(np, 3))) {
            if (np <= 3) {
              code <- c(code, sprintf("res_%d <- data.frame(dat[, 1], rat$ratio[, %d])", j, j))
            }
          }
          if (length(ref) >= 2) {
            code <- c(code, sprintf("DecomposeR::ratios(%s)", cd_dep(ref)))
          }
          if (identical(p$show, "pop")) {
            code <- c(code, sprintf("DecomposeR::plot_ratio(rat, style = \"s\"%s)",
                if (length(refr)) { sprintf(", lines = %s", cd_dep(signif(refr$ratio, 4))) } else { "" }))
          }
          list(result = list(x = x, ratio = rat, power = pw, lr = lr, ra = ra, ref = refr, summary = summ),
              outputs = o, text = txt, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          pairs <- trimws(strsplit(paste(p$pairs, collapse = " "), "[,; ]+")[[1]])
          pairs <- pairs[pairs %in% res$lr]
          if (!length(pairs)) {
            if (length(res$lr) <= 6) {
              pairs <- res$lr
            } else {
              ab <- strsplit(res$lr, "/", fixed = TRUE)
              ml <- integer(length(ab))
              for (i in seq_along(ab)) {
                ml[i] <- as.integer(ab[[i]][1])
              }
              um <- sort(unique(ml))
              keep <- character()
              for (i in seq_along(ab)) {
                a1 <- as.integer(ab[[i]][1])
                a2 <- as.integer(ab[[i]][2])
                if (match(a1, um) + 1 <= length(um) && a2 == um[match(a1, um) + 1]) {
                  keep <- c(keep, res$lr[i])
                }
              }
              pairs <- if (length(keep)) { keep } else { res$lr[1:6] }
            }
          }
          refl <- if (is.null(res$ref)) { NULL } else { res$ref$ratio }
          if (identical(p$show, "pop")) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            ra <- res$ra
            bad <- !is.finite(ra$ratio) | !is.finite(ra$rpwr) | ra$ratio <= 0
            ra$ratio[bad] <- 1
            ra$rpwr[bad] <- 0
            r <- try(DecomposeR::plot_ratio(ra, style = "s", select = pairs, lines = refl), silent = TRUE)
            if (inherits(r, "try-error")) {
              cd_message_plot(paste("plot_ratio failed:", conditionMessage(attr(r, "condition"))))
            }
            return(invisible(NULL))
          }
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          graphics::par(mar = c(4, 4.3, 1.6, 3.2))
          j <- match(pairs, res$lr)
          vals <- res$ratio[, j, drop = FALSE]
          vw <- cd_view_lims(range(res$x), cd_dc_robust(vals), x = rep(res$x, length(j)), y = as.numeric(vals), logy = TRUE)
          graphics::plot(NA, xlim = vw$x, ylim = vw$y, log = "y", xlab = "Depth / time", ylab = "Ratio of frequencies", yaxt = "n")
          cd_dc_logaxis(2)
          graphics::grid(col = "grey90", lty = 1)
          if (length(refl)) {
            graphics::abline(h = refl, col = "grey55", lty = 2)
            graphics::axis(4, at = refl, labels = cd_fmt_period(refl), cex.axis = 0.7, col.axis = "grey35", tcl = -0.2,
                las = 1, gap.axis = 0)
          }
          cols <- cd_cols(length(j))
          for (i in seq_along(j)) {
            graphics::lines(res$x, res$ratio[, j[i]], col = cols[i], lwd = 1)
          }
          graphics::legend("topleft", legend = pairs, col = cols, lwd = 2, bty = "n", cex = 0.8)
        }),
    cd_tool("dc_precision", TS, "Precision of the Hilbert instantaneous frequency (test signal)", group = G, inputs = 0L,
        icon = "bullseye",
        info = paste("Compares the instantaneous frequency computed with the Hilbert transform with the exact value for a",
            "test signal a sin(2 pi f1 t + phi1) + b sin(2 pi f2 t + phi2) + c, to see how sampling, an offset or a",
            "second cycle bias the method."),
        uses = "DecomposeR::PrecisionTester, HilbertTransform, InstantaneousFrequency",
        params = list(
          cd_num("tmax", "Length of the test signal", 10, min = 0),
          cd_num("dt", "Sampling interval", 0.01, min = 0),
          cd_chk("uneven", "Random (uneven) sampling with the same number of points", FALSE),
          cd_sel("method", "Phase derivative", c("Arctangent of the phase (arctan)" = "arctan", "Analytic derivative (chain)" = "chain")),
          cd_num("lag", "Differentiation lag (samples)", 1, min = 1, step = 1),
          cd_num("a", "Amplitude a of cycle 1", 1),
          cd_num("f1", "Frequency f1 of cycle 1", 1, min = 0),
          cd_num("phi1", "Phase phi1 of cycle 1 (radians)", 0),
          cd_num("b", "Amplitude b of cycle 2", 1),
          cd_num("f2", "Frequency f2 of cycle 2", 2, min = 0),
          cd_num("phi2", "Phase phi2 of cycle 2 (radians)", 0.5236),
          cd_num("c", "Offset c", 1),
          cd_num("seed", "Random seed", 42, step = 1, show_if = list(param = "uneven", values = "true"))),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("DecomposeR")
          if (!(p$tmax > 0 && p$dt > 0) || p$tmax / p$dt < 10 || p$tmax / p$dt > 1e6) {
            stop("give a positive length and sampling interval (10 to 1e6 points)", call. = FALSE)
          }
          if (isTRUE(p$uneven)) {
            set.seed(p$seed)
            tt <- sort(stats::runif(round(p$tmax / p$dt) + 1, 0, p$tmax))
            tcode <- c(sprintf("set.seed(%s)", cd_fmt(p$seed)),
                sprintf("tt <- sort(runif(%d, 0, %s))", as.integer(round(p$tmax / p$dt) + 1), cd_fmt(p$tmax)))
          } else {
            tt <- seq(0, p$tmax, by = p$dt)
            tcode <- sprintf("tt <- seq(0, %s, by = %s)", cd_fmt(p$tmax), cd_fmt(p$dt))
          }
          r <- DecomposeR::PrecisionTester(tt = tt, method = p$method, lag = as.integer(p$lag), a = p$a, b = p$b, c = p$c,
              omega.1 = 2 * pi * p$f1, omega.2 = 2 * pi * p$f2, phi.1 = p$phi1, phi.2 = p$phi2, plot.signal = FALSE,
              plot.instfreq = FALSE, plot.error = FALSE, new.device = FALSE)
          tab <- data.frame(t = tt, signal = r$sig, analytic_frequency = r$analytic, numeric_frequency = r$numeric,
              error = r$analytic - r$numeric)
          o <- list(cd_output(paste0("precision_test_", p$method, ".txt"), tab, header = TRUE))
          e <- tab$error
          inner <- tt > min(tt) + 0.1 * diff(range(tt)) & tt < max(tt) - 0.1 * diff(range(tt))
          txt <- c(sprintf("Root-mean-square error of the instantaneous frequency: %s (%s without the outer 10%% at each end).",
              cd_fmt(signif(sqrt(mean(e^2)), 3)), cd_fmt(signif(sqrt(mean(e[inner]^2)), 3))),
              sprintf("Largest error: %s; mean exact frequency %s.", cd_fmt(signif(max(abs(e)), 3)), cd_fmt(signif(mean(r$analytic), 4))))
          code <- c(tcode, sprintf(paste0("r <- DecomposeR::PrecisionTester(tt = tt, method = %s, lag = %d, a = %s, b = %s, c = %s,",
              " omega.1 = 2 * pi * %s, omega.2 = 2 * pi * %s, phi.1 = %s, phi.2 = %s, new.device = FALSE)"), cd_dep(p$method),
              as.integer(p$lag), cd_fmt(p$a), cd_fmt(p$b), cd_fmt(p$c), cd_fmt(p$f1), cd_fmt(p$f2), cd_fmt(p$phi1), cd_fmt(p$phi2)),
              "res <- data.frame(t = tt, signal = r$sig, analytic_frequency = r$analytic, numeric_frequency = r$numeric, error = r$analytic - r$numeric)",
              cd_code_write(o))
          list(result = tab, outputs = o, text = txt, code = code)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:3, ncol = 1))
          graphics::par(cex = 0.85)
          xl <- cd_dc_vx(range(res$t))
          cd_par()
          graphics::plot(res$t, res$signal, type = "l", col = "#1f4e79", xlim = xl, xlab = "", ylab = "Signal")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          inx <- res$t >= min(xl) & res$t <= max(xl)
          graphics::plot(res$t, res$analytic_frequency, type = "l", col = "#c0392b", xlim = xl,
              ylim = range(c(res$analytic_frequency[inx], res$numeric_frequency[inx]), finite = TRUE), xlab = "",
              ylab = "Frequency")
          graphics::grid(col = "grey90", lty = 1)
          graphics::lines(res$t, res$numeric_frequency, col = "black", lty = 2)
          graphics::legend("topright", legend = c("exact", "Hilbert"), col = c("#c0392b", "black"), lty = c(1, 2), bty = "n",
              cex = 0.8)
          cd_par()
          vw <- cd_view_lims(range(res$t), range(res$error), x = res$t, y = res$error, last = TRUE)
          graphics::plot(res$t, res$error, type = "l", col = "#2e8b57", xlim = vw$x, ylim = vw$y, xlab = "t",
              ylab = "Error (exact - Hilbert)")
          graphics::grid(col = "grey90", lty = 1)
        }),
    cd_tool("dc_lomb", TS, "Lomb-Scargle periodogram (uneven sampling)", group = "Spectral analysis", icon = "signal",
        heavy = function(p) { isTRUE(p$randomise) },
        info = paste("Periodogram of an unevenly sampled series without interpolation (Lomb-Scargle), with the power",
            "level that the highest peak exceeds by chance with probability alpha (Baluev 2008 or Press et al. 1994),",
            "and optionally a p-value of the highest peak by randomisation."),
        uses = "lomb::lsp, lomb::randlsp",
        params = list(
          cd_num("fmin", "Lowest frequency (0 = 1 / record length)", 0, min = 0),
          cd_num("fmax", "Highest frequency (cycles/unit)", function(d) { signif(0.5 / cd_dt(d[, 1]), 4) }, min = 0,
              help = "Default: half the inverse of the median sampling interval."),
          cd_num("ofac", "Oversampling factor", 4, min = 1, step = 1),
          cd_num("alpha", "Significance level alpha", 0.01, min = 0.0001, max = 0.5),
          cd_sel("normalize", "Normalisation", c("Standard (0 to 1, Baluev 2008)" = "standard", "Press et al. (1994)" = "press")),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_chk("randomise", "p-value of the highest peak by randomisation (randlsp)", FALSE),
          cd_num("repeats", "Randomisations", 200, min = 10, step = 10, show_if = list(param = "randomise", values = "true")),
          cd_num("seed", "Random seed", 42, step = 1, show_if = list(param = "randomise", values = "true")),
          cd_sel("xaxis", "Horizontal axis", c("Frequency" = "frequency", "Period" = "period")),
          cd_chk("logy", "Logarithmic power axis", FALSE)),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("lomb")
          s <- cd_clean(d)
          if (nrow(s) < 10) {
            stop("the series needs at least 10 points", call. = FALSE)
          }
          y <- s$y
          code <- c("dat <- dat[stats::complete.cases(dat[, 1:2]), 1:2]")
          if (isTRUE(p$detrend)) {
            y <- stats::residuals(stats::lm(y ~ s$x))
            code <- c(code, "dat[, 2] <- residuals(lm(dat[, 2] ~ dat[, 1]))")
          }
          x <- data.frame(x = s$x, y = y)
          from <- if (is.finite(p$fmin) && p$fmin > 0) { p$fmin } else { NULL }
          to <- if (is.finite(p$fmax) && p$fmax > 0) { p$fmax } else { NULL }
          r <- tryCatch(lomb::lsp(x, from = from, to = to, type = "frequency", ofac = as.integer(p$ofac), alpha = p$alpha,
              normalize = p$normalize, plot = FALSE), error = function(e) { e })
          if (inherits(r, "error")) {
            stop("lomb::lsp failed: ", conditionMessage(r), call. = FALSE)
          }
          spec <- data.frame(frequency = r$scanned, period = 1 / r$scanned, power = r$power, sig_level = r$sig.level)
          o <- list(cd_output(cd_out_name(file, "-lomb"), spec, header = TRUE))
          rng <- function(v) { if (is.null(v)) { "NULL" } else { cd_fmt(v) } }
          code <- c(code, sprintf("res <- lomb::lsp(dat, from = %s, to = %s, type = \"frequency\", ofac = %d, alpha = %s, normalize = %s, plot = FALSE)",
              rng(from), rng(to), as.integer(p$ofac), cd_fmt(p$alpha), cd_dep(p$normalize)),
              "res_table <- data.frame(frequency = res$scanned, period = 1 / res$scanned, power = res$power, sig_level = res$sig.level)")
          o[[1]]$var <- "res_table"
          dx <- diff(s$x)
          txt <- c(sprintf("%d samples, not interpolated; spacing %s to %s (median %s).", nrow(s), cd_fmt(signif(min(dx), 3)),
              cd_fmt(signif(max(dx), 3)), cd_fmt(signif(stats::median(dx), 3))),
              sprintf("Highest peak at period %s (frequency %s), power %s, p = %s (%s).", cd_fmt(signif(1 / r$peak.at[1], 4)),
                  cd_fmt(signif(r$peak.at[1], 4)), cd_fmt(signif(r$peak, 3)), format(signif(r$p.value, 3)),
                  if (p$normalize == "standard") { "Baluev 2008" } else { "Press et al. 1994" }),
              sprintf("Power level for alpha = %s: %s.", cd_fmt(p$alpha), cd_fmt(signif(r$sig.level, 3))))
          sig <- cd_peaks(spec$power, rep(r$sig.level, nrow(spec)))
          if (length(sig)) {
            sig <- sig[order(-spec$power[sig])][seq_len(min(12, length(sig)))]
            txt <- c(txt, "Peaks above that level (period):", paste0("  ", paste(cd_fmt_period(spec$period[sig]), collapse = ", ")))
          } else {
            txt <- c(txt, "No peak above that level.")
          }
          rp <- NULL
          if (isTRUE(p$randomise)) {
            set.seed(p$seed)
            rr <- lomb::randlsp(repeats = as.integer(p$repeats), x = x, from = from, to = to, type = "frequency",
                ofac = as.integer(p$ofac), alpha = p$alpha, plot = FALSE, trace = FALSE)
            rp <- list(p = rr$p.value, peaks = rr$random.peaks)
            txt <- c(txt, sprintf("Randomisation (%d shuffles of the values, standard normalisation): p = %s for the highest peak.",
                as.integer(p$repeats), format(rr$p.value)))
            code <- c(code, sprintf("set.seed(%s)", cd_fmt(p$seed)),
                sprintf("rnd <- lomb::randlsp(repeats = %d, x = dat, from = %s, to = %s, ofac = %d, alpha = %s, plot = FALSE, trace = FALSE)",
                    as.integer(p$repeats), rng(from), rng(to), as.integer(p$ofac), cd_fmt(p$alpha)), "rnd$p.value")
          }
          list(result = list(spec = spec, level = r$sig.level, rand = rp), outputs = o, text = txt,
              code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          sp <- res$spec
          cl <- matrix(res$level, nrow(sp), 1)
          cd_plot_spectrum(sp$frequency, sp$power, cl = cl, cl_names = sprintf("alpha = %s", cd_fmt(p$alpha)), xaxis = p$xaxis,
              logy = isTRUE(p$logy), peaks_above = cl[, 1], ylab = "Normalised power")
        }),
    cd_tool("dc_xspec", TS, "Cross-spectral analysis: coherence and phase (2 series)", group = "Spectral analysis",
        inputs = 2L, icon = "code-compare",
        info = paste("Fourier coherence and phase of two series (Acycle: lead-lag), with the lead or lag of series 2",
            "behind series 1 in the units of column 1 at each frequency and the 95% significance level of the coherence.",
            "Both series are interpolated onto a common grid over their overlap."),
        uses = "stats::spec.pgram",
        params = list(
          cd_num("dt", "Common sampling interval", function(d) { signif(cd_dt(d[, 1]), 4) }, min = 0),
          cd_num("spans", "Smoothing: modified Daniell span (odd, at least 3)", 7, min = 3, step = 2,
              help = "Applied twice; wider spans give a smoother coherence with more degrees of freedom."),
          cd_num("taper", "Cosine taper proportion", 0.1, min = 0, max = 0.5, step = 0.05),
          cd_num("pad", "Zero padding (proportion of the length)", 0, min = 0, step = 1),
          cd_chk("detrend", "Remove linear trends", TRUE),
          cd_num("conf", "Confidence level", 0.95, min = 0.5, max = 0.999),
          cd_num("fmax", "Highest frequency shown", function(d) { signif(0.5 / cd_dt(d[, 1]), 4) }, min = 0),
          cd_sel("xaxis", "Horizontal axis", c("Frequency" = "frequency", "Period" = "period")),
          cd_sel("third", "Lowest panel", c("Phase (degrees)" = "phase", "Lag of series 2 (units of column 1)" = "lag"))),
        compute = function(d, p, d2, file, file2) {
          s1 <- cd_clean(d)
          s2 <- cd_clean(d2)
          lo <- max(min(s1$x), min(s2$x))
          hi <- min(max(s1$x), max(s2$x))
          if (!(p$dt > 0)) {
            stop("give a positive sampling interval", call. = FALSE)
          }
          xg <- seq(lo, hi, by = p$dt)
          if (length(xg) < 32) {
            stop("the two series overlap in fewer than 32 points of the common grid", call. = FALSE)
          }
          y1 <- stats::approx(s1$x, s1$y, xg)$y
          y2 <- stats::approx(s2$x, s2$y, xg)$y
          sp <- as.integer(p$spans)
          if (sp %% 2 == 0) {
            sp <- sp + 1L
          }
          if (sp < 3) {
            stop("the span must be at least 3: without smoothing the coherence is 1 everywhere", call. = FALSE)
          }
          z <- stats::spec.pgram(stats::ts(cbind(y1, y2), start = xg[1], deltat = p$dt), spans = c(sp, sp), taper = p$taper,
              pad = p$pad, detrend = isTRUE(p$detrend), demean = TRUE, fast = TRUE, plot = FALSE)
          f <- as.numeric(z$freq)
          coh <- as.numeric(z$coh)
          ph <- as.numeric(z$phase)
          df <- z$df
          lev <- 1 - (1 - p$conf)^(1 / max(df / 2 - 1, 1e-6))
          gg <- 2 / df
          cl <- asin(pmin(0.9999, stats::qt(p$conf, 2 / gg - 2) * sqrt(gg * (1 / pmax(coh, 1e-12) - 1) / (2 * (1 - gg)))))
          lag <- ph / (2 * pi * f)
          lagw <- cl / (2 * pi * f)
          tab <- data.frame(frequency = f, period = 1 / f, power_1 = z$spec[, 1], power_2 = z$spec[, 2], coherence = coh,
              coherence_level = lev, phase_deg = ph * 180 / pi, phase_low = (ph - cl) * 180 / pi, phase_high = (ph + cl) * 180 / pi,
              lag = lag, lag_low = lag - lagw, lag_high = lag + lagw)
          o <- list(cd_output(cd_out_name(file, "-xspec"), tab, header = TRUE))
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf("xg <- seq(%s, %s, by = %s)", cd_fmt(lo), cd_fmt(hi), cd_fmt(p$dt)),
              "y <- cbind(approx(dat[, 1], dat[, 2], xg)$y, approx(dat2[, 1], dat2[, 2], xg)$y)",
              sprintf("z <- spec.pgram(ts(y, start = xg[1], deltat = %s), spans = c(%d, %d), taper = %s, pad = %s, detrend = %s, plot = FALSE)",
                  cd_fmt(p$dt), sp, sp, cd_fmt(p$taper), cd_fmt(p$pad), isTRUE(p$detrend)),
              sprintf("level <- 1 - (1 - %s)^(1 / (z$df / 2 - 1))", cd_fmt(p$conf)),
              "gg <- 2 / z$df",
              sprintf("ci <- asin(pmin(0.9999, qt(%s, 2 / gg - 2) * sqrt(gg * (1 / z$coh - 1) / (2 * (1 - gg)))))", cd_fmt(p$conf)),
              paste0("res <- data.frame(frequency = z$freq, period = 1 / z$freq, power_1 = z$spec[, 1], power_2 = z$spec[, 2],",
                  " coherence = as.numeric(z$coh), coherence_level = level, phase_deg = as.numeric(z$phase) * 180 / pi,",
                  " phase_low = as.numeric(z$phase - ci) * 180 / pi, phase_high = as.numeric(z$phase + ci) * 180 / pi,",
                  " lag = as.numeric(z$phase) / (2 * pi * z$freq), lag_low = as.numeric(z$phase - ci) / (2 * pi * z$freq),",
                  " lag_high = as.numeric(z$phase + ci) / (2 * pi * z$freq))"),
              cd_code_write(o))
          keep <- f <= p$fmax
          pk <- cd_peaks(coh, rep(lev, length(coh)))
          pk <- pk[keep[pk]]
          txt <- c(sprintf("Overlap %s to %s, %d points at %s; %s degrees of freedom; %s%% level of the squared coherence: %s.",
              cd_fmt(lo), cd_fmt(hi), length(xg), cd_fmt(p$dt), cd_fmt(signif(df, 3)), cd_fmt(100 * p$conf), cd_fmt(signif(lev, 3))))
          if (length(pk)) {
            pk <- pk[order(-coh[pk])][seq_len(min(10, length(pk)))]
            txt <- c(txt, "Coherent periods: period, coherence, phase (degrees), lag of series 2 (positive: series 2 later, at larger x):")
            for (i in pk) {
              txt <- c(txt, sprintf("  %s: %s, %s (+/- %s), %s (+/- %s)", cd_fmt_period(1 / f[i]), cd_fmt(signif(coh[i], 2)),
                  cd_fmt(round(ph[i] * 180 / pi, 1)), cd_fmt(round(cl[i] * 180 / pi, 1)), cd_fmt(signif(lag[i], 3)),
                  cd_fmt(signif(lagw[i], 2))))
            }
          } else {
            txt <- c(txt, "No coherence peak above the significance level.")
          }
          list(result = list(tab = tab, level = lev), outputs = o, text = txt, code = code)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          tb <- res$tab
          tb <- tb[tb$frequency > 0 & tb$frequency <= p$fmax, ]
          per <- identical(p$xaxis, "period")
          xv <- if (per) { tb$period } else { tb$frequency }
          lg <- if (per) { "x" } else { "" }
          xlab <- if (per) { "Period" } else { "Frequency (cycles/unit)" }
          xl <- cd_dc_vx(range(xv))
          graphics::layout(matrix(1:3, ncol = 1), heights = c(1, 1, 1.15))
          graphics::par(cex = 0.85)
          cd_par()
          graphics::par(mar = c(0.5, 4.3, 1, 1))
          inx <- xv >= min(xl) & xv <= max(xl)
          pr <- range(c(tb$power_1[inx], tb$power_2[inx]), finite = TRUE)
          graphics::plot(xv, tb$power_1, type = "l", log = paste0("y", lg), col = "#1f4e79", xlim = xl, ylim = pr, xaxt = "n",
              xlab = "", ylab = "Power")
          graphics::lines(xv, tb$power_2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          graphics::legend("topright", legend = c("series 1", "series 2"), col = c("#1f4e79", "#c0392b"), lwd = 2, bty = "n",
              cex = 0.8)
          cd_par()
          graphics::par(mar = c(0.5, 4.3, 0.5, 1))
          graphics::plot(xv, tb$coherence, type = "l", log = lg, col = "#1f4e79", xlim = xl, ylim = c(0, 1), xaxt = "n", xlab = "",
              ylab = "Squared coherence")
          graphics::grid(col = "grey90", lty = 1)
          graphics::abline(h = res$level, col = "#c0392b", lty = 2)
          sig <- tb$coherence > res$level
          cd_par()
          graphics::par(mar = c(4, 4.3, 0.5, 1))
          if (identical(p$third, "lag")) {
            yv <- tb$lag
            ylo <- tb$lag_low
            yhi <- tb$lag_high
            ylab <- "Lag of series 2"
          } else {
            yv <- tb$phase_deg
            ylo <- tb$phase_low
            yhi <- tb$phase_high
            ylab <- "Phase (degrees)"
          }
          yr <- if (any(sig & inx)) { range(c(ylo[sig & inx], yhi[sig & inx]), finite = TRUE) } else { range(yv[inx], finite = TRUE) }
          if (!identical(p$third, "lag")) {
            yr <- c(-180, 180)
          }
          vw <- cd_view_lims(range(xv), yr, last = TRUE)
          graphics::plot(xv, yv, type = "n", log = lg, xlim = if (vw$on) { vw$x } else { xl }, ylim = vw$y, xlab = xlab, ylab = ylab)
          graphics::grid(col = "grey90", lty = 1)
          graphics::abline(h = 0, col = "grey60")
          ok <- is.finite(ylo) & is.finite(yhi)
          graphics::segments(xv[ok & sig], ylo[ok & sig], xv[ok & sig], yhi[ok & sig], col = grDevices::adjustcolor("#c0392b", 0.35))
          graphics::points(xv, yv, pch = 16, cex = 0.35, col = "grey65")
          graphics::points(xv[sig], yv[sig], pch = 16, cex = 0.6, col = "#c0392b")
        }),
    cd_tool("dc_recurrence", TS, "Recurrence plot", group = "Nonlinear analysis", icon = "border-all",
        info = paste("Recurrence plot of the series after time-delay embedding (Acycle: Recurrence plot): dark points mark",
            "pairs of depths at which the system is in a similar state. Regular cycles give diagonal lines; changes in",
            "the pattern mark changes in the dynamics. Gives the recurrence quantification (RR, DET, LAM, ...)."),
        uses = "base R (stats::dist)",
        params = list(
          cd_num("dim", "Embedding dimension", 3, min = 1, max = 20, step = 1),
          cd_num("tau", "Delay (samples)", cd_dc_tau, min = 1, step = 1,
              help = "Default: the first lag at which the autocorrelation falls below 1/e."),
          cd_sel("thr", "Distance threshold", c("Fraction of the largest distance" = "fraction", "Recurrence rate" = "rr")),
          cd_num("eps", "Fraction of the largest distance", 0.1, min = 0, max = 1, show_if = list(param = "thr", values = "fraction")),
          cd_num("rr", "Recurrence rate (share of recurrent pairs)", 0.05, min = 0.001, max = 0.9,
              show_if = list(param = "thr", values = "rr")),
          cd_sel("norm", "Distance", c("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan")),
          cd_chk("standardise", "Standardise the series first", TRUE),
          cd_num("maxn", "Most points in the plot (subsampling)", 1500, min = 100, max = 3000, step = 100),
          cd_num("lmin", "Shortest line for DET and LAM (points)", 2, min = 2, step = 1),
          cd_sel("show", "Figure", c("Recurrence plot" = "rp", "Distance matrix (unthresholded)" = "dist"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          y <- s$y
          if (isTRUE(p$standardise)) {
            y <- (y - mean(y)) / stats::sd(y)
          }
          m <- as.integer(p$dim)
          tau <- as.integer(p$tau)
          ne <- length(y) - (m - 1L) * tau
          if (ne < 20) {
            stop("too few points for this embedding: lower the dimension or the delay", call. = FALSE)
          }
          E <- matrix(0, ne, m)
          for (j in seq_len(m)) {
            E[, j] <- y[(1 + (j - 1) * tau):(ne + (j - 1) * tau)]
          }
          xe <- s$x[seq_len(ne) + (m - 1) * tau]
          step <- max(1L, as.integer(ceiling(ne / p$maxn)))
          ix <- seq(1, ne, by = step)
          E <- E[ix, , drop = FALSE]
          xe <- xe[ix]
          D <- as.matrix(stats::dist(E, method = p$norm))
          if (p$thr == "rr") {
            dv <- D[upper.tri(D)]
            eps <- as.numeric(stats::quantile(dv, p$rr, names = FALSE))
          } else {
            eps <- p$eps * max(D)
          }
          R <- D <= eps
          rq <- cd_dc_rqa(R, lmin = as.integer(p$lmin))
          rq <- data.frame(dimension = m, delay = tau, step = step, threshold = eps, rq)
          o <- list(cd_output(cd_out_name(file, "-rqa"), rq, header = TRUE, var = "rqa"))
          txt <- c(cd_even_note(d, s),
              sprintf("Embedding dimension %d, delay %d samples (%s); %d states%s; threshold %s.", m, tau,
                  cd_fmt(signif(tau * cd_dt(s$x), 3)), nrow(E),
                  if (step > 1) { sprintf(" (one in %d of %d)", step, ne) } else { "" }, cd_fmt(signif(eps, 3))),
              sprintf("RR %s, DET %s, mean diagonal line %s (longest %s), ENTR %s, LAM %s, trapping time %s.",
                  cd_fmt(signif(rq$RR, 3)), cd_fmt(signif(rq$DET, 3)), cd_fmt(signif(rq$L_mean, 3)), cd_fmt(rq$L_max),
                  cd_fmt(signif(rq$ENTR, 3)), cd_fmt(signif(rq$LAM, 3)), cd_fmt(signif(rq$TT, 3))))
          code <- c(cd_even_code(d), "y <- dat[, 2]",
              if (isTRUE(p$standardise)) { "y <- (y - mean(y)) / sd(y)" } else { character() },
              sprintf("m <- %d", m), sprintf("tau <- %d", tau), "ne <- length(y) - (m - 1) * tau", "E <- matrix(0, ne, m)",
              "for (j in seq_len(m)) {", "  E[, j] <- y[(1 + (j - 1) * tau):(ne + (j - 1) * tau)]", "}",
              "xe <- dat[seq_len(ne) + (m - 1) * tau, 1]",
              sprintf("ix <- seq(1, ne, by = %d)", step), "E <- E[ix, , drop = FALSE]", "xe <- xe[ix]",
              sprintf("D <- as.matrix(dist(E, method = %s))", cd_dep(p$norm)),
              if (p$thr == "rr") { sprintf("eps <- quantile(D[upper.tri(D)], %s)", cd_fmt(p$rr)) } else { sprintf("eps <- %s * max(D)", cd_fmt(p$eps)) },
              "R <- D <= eps",
              "rqa <- data.frame(RR = (sum(R) - nrow(R)) / (nrow(R)^2 - nrow(R)))",
              "image(xe, xe, R, col = c(\"white\", \"black\"), useRaster = TRUE, asp = 1, xlab = \"Depth / time\", ylab = \"Depth / time\")",
              cd_code_write(o))
          list(result = list(x = xe, D = D, eps = eps, rqa = rq), outputs = o, text = txt, code = code)
        },
        plot = function(res, p) {
          if (identical(p$show, "dist")) {
            cd_plot_image(res$x, res$x, res$D, xlab = "Depth / time", ylab = "Depth / time", zlab = "Distance")
            return(invisible(NULL))
          }
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par(pty = "s")
          vw <- cd_view_lims(range(res$x), range(res$x))
          graphics::image(res$x, res$x, res$D <= res$eps, col = c("white", "#1f2a44"), useRaster = cd_regular(res$x),
              xlab = "Depth / time", ylab = "Depth / time", xlim = vw$x, ylim = vw$y)
          graphics::box()
        }),
    cd_tool("dc_dtw", "Age models", "Correlate two records (dynamic time warping, 2 files)", group = "Correlation (dtw)",
        inputs = 2L, icon = "arrows-left-right-to-line",
        info = paste("Aligns record 2 to record 1 by dynamic time warping and writes the depth-to-depth tie function",
            "(depth in record 2, equivalent depth in record 1) and record 2 on the depth scale of record 1. With an open",
            "end (and begin), record 2 may match only part of record 1. Use the tie function in Age scale / tuning to move",
            "other proxies of record 2."),
        uses = "dtw::dtw",
        params = list(
          cd_num("npts", "Points per record (even resampling)", 1000, min = 20, max = 4000, step = 100,
              help = "Both records are resampled evenly with at most this many points; the cost matrix grows with its square."),
          cd_sel("prep", "Values", c("Standardised" = "z", "Detrended and standardised" = "dz", "As they are" = "raw")),
          cd_sel("step", "Step pattern", cd_dc_step_patterns()),
          cd_sel("window", "Window constraint", c("None" = "none", "Sakoe-Chiba band" = "sakoechiba",
              "Slanted band" = "slantedband", "Itakura parallelogram" = "itakura")),
          cd_num("wsize", "Window size (share of the record)", 0.1, min = 0.001, max = 1,
              show_if = list(param = "window", values = c("sakoechiba", "slantedband"))),
          cd_chk("open_end", "Open end (record 2 matches a leading part of record 1)", FALSE),
          cd_chk("open_begin", "Open begin (needs open end and an asymmetric or type c step pattern)", FALSE),
          cd_sel("show", "Figure", c("Alignment of the two records" = "align", "Record 2 on the depth scale of record 1" = "overlay",
              "Tie function" = "tie"))),
        compute = function(d, p, d2, file, file2) {
          cd_dc_need("dtw")
          r1 <- cd_dc_resample(d, p$npts)
          r2 <- cd_dc_resample(d2, p$npts)
          q <- cd_dc_prep(r2$y, p$prep)
          ref <- cd_dc_prep(r1$y, p$prep)
          ws <- max(1L, as.integer(round(p$wsize * max(nrow(r1), nrow(r2)))))
          if (isTRUE(p$open_begin) && !isTRUE(p$open_end)) {
            stop("an open begin needs an open end as well", call. = FALSE)
          }
          sp <- getExportedValue("dtw", p$step)
          if (p$window %in% c("sakoechiba", "slantedband")) {
            al <- tryCatch(dtw::dtw(x = q, y = ref, step.pattern = sp, window.type = p$window, window.size = ws,
                open.end = isTRUE(p$open_end), open.begin = isTRUE(p$open_begin)), error = function(e) { e })
          } else {
            al <- tryCatch(dtw::dtw(x = q, y = ref, step.pattern = sp, window.type = p$window, open.end = isTRUE(p$open_end),
                open.begin = isTRUE(p$open_begin)), error = function(e) { e })
          }
          if (inherits(al, "error")) {
            stop("dtw::dtw failed: ", conditionMessage(al), call. = FALSE)
          }
          d2x <- r2$x[al$index1]
          d1x <- r1$x[al$index2]
          u <- unique(d2x)
          g <- match(d2x, u)
          tie <- data.frame(depth_2 = u, depth_1 = as.numeric(rowsum(d1x, g) / rowsum(rep(1, length(g)), g)))
          s2 <- cd_clean(d2)
          inr <- s2$x >= min(tie$depth_2) & s2$x <= max(tie$depth_2)
          on1 <- data.frame(depth_1 = stats::approx(tie$depth_2, tie$depth_1, s2$x[inr], ties = mean)$y, value = s2$y[inr])
          b1 <- tools::file_path_sans_ext(basename(file))
          f2 <- if (is.null(file2)) { "series2" } else { file2 }
          o <- list(cd_output(cd_out_name(f2, paste0("-dtw_tie_", b1)), tie, var = "tie"),
              cd_output(cd_out_name(f2, paste0("-dtw_on_", b1)), on1, var = "on1"))
          wcode <- if (p$window %in% c("sakoechiba", "slantedband")) { sprintf(", window.size = %d", ws) } else { "" }
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf("r1 <- data.frame(x = seq(min(dat[, 1]), max(dat[, 1]), length.out = %d))", nrow(r1)),
              "r1$y <- approx(dat[, 1], dat[, 2], r1$x)$y",
              sprintf("r2 <- data.frame(x = seq(min(dat2[, 1]), max(dat2[, 1]), length.out = %d))", nrow(r2)),
              "r2$y <- approx(dat2[, 1], dat2[, 2], r2$x)$y", "ref <- r1$y", "q <- r2$y",
              cd_dc_prep_code("ref", p$prep), cd_dc_prep_code("q", p$prep),
              sprintf("al <- dtw::dtw(x = q, y = ref, step.pattern = dtw::%s, window.type = %s%s, open.end = %s, open.begin = %s)",
                  p$step, cd_dep(p$window), wcode, isTRUE(p$open_end), isTRUE(p$open_begin)),
              "tie <- data.frame(depth_2 = r2$x[al$index1], depth_1 = r1$x[al$index2])",
              "tie <- aggregate(depth_1 ~ depth_2, data = tie, FUN = mean)",
              "dat2 <- dat2[dat2[, 1] >= min(tie$depth_2) & dat2[, 1] <= max(tie$depth_2), ]",
              "on1 <- data.frame(depth_1 = approx(tie$depth_2, tie$depth_1, dat2[, 1], ties = mean)$y, value = dat2[, 2])",
              cd_code_write(o))
          txt <- c(sprintf("Record 1: %d points (%s to %s); record 2: %d points (%s to %s), resampled evenly.", nrow(r1),
              cd_fmt(min(r1$x)), cd_fmt(max(r1$x)), nrow(r2), cd_fmt(min(r2$x)), cd_fmt(max(r2$x))),
              sprintf("Normalised distance %s (step pattern %s).", if (is.finite(al$normalizedDistance)) {
                cd_fmt(signif(al$normalizedDistance, 4)) } else { "not defined" }, p$step),
              sprintf("Record 2 corresponds to %s to %s of record 1.", cd_fmt(signif(min(tie$depth_1), 6)), cd_fmt(signif(max(tie$depth_1), 6))),
              sprintf("Tie function (depth in record 2, depth in record 1) written to %s: use it as the age model in Age scale / tuning.",
                  o[[1]]$name))
          list(result = list(r1 = r1, r2 = r2, ref = ref, q = q, i1 = al$index1, i2 = al$index2, tie = tie, on1 = on1),
              outputs = o, text = txt, code = code)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          if (identical(p$show, "tie")) {
            cd_par()
            vw <- cd_view_lims(range(res$tie$depth_2), range(res$tie$depth_1), x = res$tie$depth_2, y = res$tie$depth_1)
            graphics::plot(res$tie$depth_2, res$tie$depth_1, type = "l", col = "#1f4e79", lwd = 1.5, xlim = vw$x, ylim = vw$y,
                xlab = "Depth in record 2", ylab = "Depth in record 1")
            graphics::grid(col = "grey90", lty = 1)
            return(invisible(NULL))
          }
          if (identical(p$show, "overlay")) {
            r1 <- res$r1
            o1 <- res$on1[stats::complete.cases(res$on1), ]
            o1 <- o1[order(o1[, 1]), ]
            z1 <- (r1$y - mean(r1$y)) / stats::sd(r1$y)
            z2 <- (o1[, 2] - mean(o1[, 2])) / stats::sd(o1[, 2])
            cd_plot_series(list(data.frame(r1$x, z1), data.frame(o1[, 1], z2)), labels = c("record 1", "record 2 (aligned)"),
                xlab = "Depth in record 1", ylab = "Standardised value")
            return(invisible(NULL))
          }
          cd_par()
          r1 <- res$r1
          r2 <- res$r2
          a <- res$ref
          b <- res$q
          sc <- function(v) { (v - min(v)) / max(diff(range(v)), 1e-12) }
          ya <- sc(a) + 1.2
          yb <- sc(b)
          t1 <- range(res$tie$depth_1)
          x2 <- t1[1] + (r2$x - min(r2$x)) / diff(range(r2$x)) * diff(t1)
          vw <- cd_view_lims(range(r1$x), c(-0.05, 2.25))
          graphics::plot(NA, xlim = vw$x, ylim = vw$y, xlab = "Depth in record 1", ylab = "", yaxt = "n")
          nl <- min(80, length(res$i1))
          k <- unique(round(seq(1, length(res$i1), length.out = nl)))
          graphics::segments(r1$x[res$i2[k]], ya[res$i2[k]], x2[res$i1[k]], yb[res$i1[k]], col = "grey70")
          graphics::lines(r1$x, ya, col = "#1f4e79")
          graphics::lines(x2, yb, col = "#c0392b")
          at <- pretty(r2$x)
          at <- at[at >= min(r2$x) & at <= max(r2$x)]
          graphics::axis(3, at = t1[1] + (at - min(r2$x)) / diff(range(r2$x)) * diff(t1), labels = at, col.axis = "#c0392b")
          graphics::legend("bottomright", legend = c("record 1 (bottom axis)", "record 2 (top axis)"), col = c("#1f4e79", "#c0392b"),
              lwd = 2, bty = "n", cex = 0.8)
        })
  )
}
