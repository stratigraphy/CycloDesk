# Tools: astrochron_testing ####
#
# Spectral testing, surrogates, astronomical testing (TimeOpt family, ASM, amplitude-modulation tests), anchoring,
# geochronology and astronomical periods from astrochron, spread over the menus Time Series, Sed. rate, Age models and
# Astronomy. astrochron functions print a lot and some open their own graphics devices: they are run through
# cd_at_quiet() (output captured, figures sent to a null device) or cd_at_pages() (their figures recorded so that the
# tool can show them).

# Helpers ####

# Close a temporary device and make the previous device current again
cd_at_close <- function(dev, prev) {
  if (dev %in% grDevices::dev.list()) {
    grDevices::dev.off(dev)
  }
  if (prev > 1 && prev %in% grDevices::dev.list()) {
    grDevices::dev.set(prev)
  }
  invisible(NULL)
}

# Evaluate an astrochron call without printing and without drawing: returns list(value, log)
cd_at_quiet <- function(expr) {
  prev <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  oldop <- options(device = function(...) {
    invisible(NULL)
  })
  on.exit(options(oldop), add = TRUE)
  on.exit(cd_at_close(dev, prev), add = TRUE)
  val <- NULL
  log <- utils::capture.output(val <- expr)
  list(value = val, log = log, pages = list())
}

# Evaluate an astrochron call and record every figure it draws (also those it would draw on new devices) as a list
# of recorded plots that the tool replays
cd_at_pages <- function(expr, width = 10, height = 7.5) {
  prev <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = width, height = height)
  dev <- grDevices::dev.cur()
  on.exit(cd_at_close(dev, prev), add = TRUE)
  grDevices::dev.control("enable")
  op0 <- graphics::par(no.readonly = TRUE)
  st <- new.env(parent = emptyenv())
  st$pages <- list()
  snap <- function() {
    rp <- grDevices::recordPlot()
    if (length(rp[[1]])) {
      st$pages[[length(st$pages) + 1]] <- rp
    }
    invisible(NULL)
  }
  newdev <- function(...) {
    snap()
    graphics::plot.new()
    graphics::par(op0)
    invisible(NULL)
  }
  oldop <- options(device = newdev)
  on.exit(options(oldop), add = TRUE)
  val <- NULL
  log <- utils::capture.output(val <- expr)
  snap()
  list(value = val, log = log, pages = st$pages)
}

# Draw an astrochron figure on the current device (its dev.new calls are ignored)
cd_at_here <- function(expr) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  oldop <- options(device = function(...) {
    invisible(NULL)
  })
  on.exit(options(oldop), add = TRUE)
  utils::capture.output(expr)
  invisible(NULL)
}

# Replay one recorded figure
cd_at_replay <- function(pages, k = 1) {
  if (!length(pages)) {
    cd_message_plot("No figure")
    return(invisible(NULL))
  }
  k <- suppressWarnings(as.integer(k))
  if (!length(k) || is.na(k)) {
    k <- 1L
  }
  k <- min(max(1L, k), length(pages))
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  grDevices::replayPlot(pages[[k]])
  invisible(NULL)
}

# Call an astrochron function: args is a named list of values; symbols (e.g. quote(dat)) refer to objects in 'data'.
# Returns list(value, log, pages, code), where code is the equivalent line of R code.
cd_at_call <- function(fn, args, data = list(), var = "res", mode = "quiet", warn = TRUE) {
  keep <- list()
  for (nm in names(args)) {
    if (!is.null(args[[nm]])) {
      keep[nm] <- list(args[[nm]])
    }
  }
  cl <- as.call(c(list(call("::", as.name("astrochron"), as.name(fn))), keep))
  env <- list2env(data, parent = environment())
  if (mode == "pages") {
    r <- cd_at_pages(eval(cl, envir = env))
  } else if (warn) {
    r <- cd_at_quiet(eval(cl, envir = env))
  } else {
    r <- cd_at_quiet(suppressWarnings(eval(cl, envir = env)))
  }
  r$code <- paste0(var, " <- ", paste(deparse(cl, width.cutoff = 100L), collapse = "\n    "))
  r
}

# First number after a text in the captured output, NA when absent
cd_at_grab <- function(log, pattern, which = 1L) {
  k <- grep(pattern, log, fixed = TRUE)
  if (length(k) < which) {
    return(NA_real_)
  }
  line <- log[k[which]]
  pos <- regexpr(pattern, line, fixed = TRUE)
  rest <- substring(line, pos + attr(pos, "match.length"))
  m <- regmatches(rest, regexpr("-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", rest))
  if (!length(m)) {
    return(NA_real_)
  }
  as.numeric(m)
}

# All numbers on a line of the captured output that starts with a label (e.g. "u:")
cd_at_line_nums <- function(log, label, after = NULL) {
  start <- 1L
  if (!is.null(after)) {
    k0 <- grep(after, log, fixed = TRUE)
    if (!length(k0)) {
      return(numeric())
    }
    start <- k0[1]
  }
  lines <- log[start:length(log)]
  k <- which(startsWith(trimws(lines), label))
  if (!length(k)) {
    return(numeric())
  }
  rest <- substring(trimws(lines[k[1]]), nchar(label) + 1L)
  m <- regmatches(rest, gregexpr("-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", rest))[[1]]
  as.numeric(m)
}

# Monte Carlo p-value as astrochron reports it: never below 10 / nsim
cd_at_pval <- function(ngt, nsim) {
  if (nsim < 1) {
    return(list(p = NA_real_, text = "no simulations"))
  }
  p <- ngt / nsim
  lim <- 10 / nsim
  if (lim >= 1) {
    return(list(p = 1, text = "too few simulations for a p-value (at least 11 needed)"))
  }
  if (p < lim) {
    return(list(p = lim, text = sprintf("p < %s", cd_fmt(lim))))
  }
  list(p = p, text = sprintf("p = %s", cd_fmt(signif(p, 3))))
}

# Frequency bands typed as "low high; low high"
cd_at_bands <- function(txt) {
  v <- cd_parse_nums(gsub(";", " ", paste(txt, collapse = " ")))
  if (!length(v)) {
    return(NULL)
  }
  if (length(v) %% 2 != 0) {
    stop("give the frequency bands as pairs of low and high frequency, e.g. \"1.6 2.4; 4 5\"", call. = FALSE)
  }
  m <- matrix(v, ncol = 2, byrow = TRUE)
  lo <- numeric(nrow(m))
  hi <- numeric(nrow(m))
  for (i in seq_len(nrow(m))) {
    lo[i] <- min(m[i, ])
    hi[i] <- max(m[i, ])
  }
  list(flow = lo, fhigh = hi)
}

# A positive number, or NULL (0, NA or empty means "default")
cd_at_pos <- function(x) {
  if (is.null(x) || !length(x) || !is.finite(x[1]) || x[1] <= 0) {
    return(NULL)
  }
  x[1]
}

# A numeric vector from a parsed field, or NULL when empty
cd_at_vec <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NULL)
  }
  x
}

cd_at_code_write <- function(outputs) {
  out <- character()
  for (o in outputs) {
    out <- c(out, sprintf("CycloDesk::write_series(%s, %s, header = %s)", o$var, cd_dep(o$name),
        if (isTRUE(o$header)) { "TRUE" } else { "FALSE" }))
  }
  out
}

# Table as text lines for the result window
cd_at_table_text <- function(df, title = NULL, digits = 5, max_rows = 40) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  n <- nrow(df)
  if (n > max_rows) {
    df <- df[seq_len(max_rows), , drop = FALSE]
  }
  for (j in seq_len(ncol(df))) {
    if (is.numeric(df[[j]])) {
      df[[j]] <- signif(df[[j]], digits)
    }
  }
  out <- utils::capture.output(print(df, row.names = FALSE))
  c(if (is.null(title)) { character() } else { c("", title) }, out,
      if (n > max_rows) { sprintf("... %d more rows in the output file", n - max_rows) } else { character() })
}

# Image panel without colour bar (several panels in one figure); the last panel follows the zoom of the figure
cd_at_image_panel <- function(x, y, z, xlab = "", ylab = "", main = NULL, palette = "viridis|viridis", last = FALSE,
    zlim = NULL) {
  ox <- order(x)
  oy <- order(y)
  x <- x[ox]
  y <- y[oy]
  z <- z[ox, oy, drop = FALSE]
  if (is.null(zlim)) {
    zlim <- range(z, finite = TRUE)
  }
  if (!all(is.finite(zlim))) {
    zlim <- c(0, 1)
  }
  if (zlim[1] == zlim[2]) {
    zlim <- zlim + c(-0.5, 0.5)
  }
  vw <- cd_view_lims(range(x), range(y), last = last)
  graphics::image(x, y, z, col = cd_palette(100, palette), zlim = zlim, xlab = xlab, ylab = ylab, main = main,
      useRaster = cd_regular(x) && cd_regular(y), xlim = vw$x, ylim = vw$y)
  graphics::box()
  invisible(NULL)
}

# Histogram of simulated values with the observed value
cd_at_hist <- function(sims, obs, xlab = "Simulated value", main = NULL, two_sided = FALSE) {
  sims <- sims[is.finite(sims)]
  if (!length(sims)) {
    cd_message_plot("No simulations")
    return(invisible(NULL))
  }
  xr <- range(c(sims, obs, if (two_sided) { -obs } else { numeric() }), finite = TRUE)
  vw <- cd_view_lims(xr, NULL)
  h <- graphics::hist(sims, breaks = max(10, min(60, round(sqrt(length(sims)) * 2))), plot = FALSE)
  graphics::plot(h, col = "#c3cad4", border = "white", xlim = vw$x, xlab = xlab, ylab = "Number of simulations",
      main = main)
  graphics::abline(v = obs, col = "#c0392b", lwd = 2)
  if (two_sided) {
    graphics::abline(v = -obs, col = "#c0392b", lwd = 1, lty = 2)
  }
  graphics::box()
  invisible(NULL)
}

# Sticks at periods with labels (astronomical periods, tones)
cd_at_sticks <- function(period, label, col = "#1f4e79", main = NULL, xlab = "Period (kyr)", height = NULL, vline = NULL) {
  ok <- is.finite(period) & period > 0
  period <- period[ok]
  label <- label[ok]
  col <- rep(col, length.out = length(ok))[ok]
  if (!length(period)) {
    cd_message_plot("No periods")
    return(invisible(NULL))
  }
  if (is.null(height)) {
    height <- rep(1, length(period))
  } else {
    height <- height[ok]
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  cd_par()
  xr <- range(period) * c(0.8, 1.25)
  vw <- cd_view_lims(xr, c(0, 1.25 * max(height)), last = TRUE)
  graphics::plot(NA, xlim = vw$x, ylim = vw$y, log = "x", xlab = xlab, ylab = "", yaxt = "n", main = main)
  graphics::grid(col = "grey90", lty = 1)
  graphics::segments(period, 0, period, height, col = col, lwd = 2)
  graphics::text(period, height, labels = label, srt = 90, adj = c(-0.1, 0.5), cex = 0.7, col = col, xpd = NA)
  if (!is.null(vline)) {
    graphics::abline(v = vline, col = "#c0392b", lty = 2, lwd = 1.5)
  }
  invisible(NULL)
}

# Default TimeOpt targets and filter settings (as in astrochron::timeOpt)
cd_at_timeopt_defaults <- function(fit) {
  tE <- c(405.6795, 130.719, 123.839, 98.86307, 94.87666)
  tP <- c(23.62069, 22.31868, 19.06768, 18.91979)
  if (identical(as.integer(fit), 2L)) {
    return(list(targetE = tE, targetP = tP, flow = 0.007, fhigh = 0.0115, roll = 1e5))
  }
  list(targetE = tE, targetP = tP, flow = 0.035, fhigh = 0.065, roll = 1e3)
}

# TimeOpt targets and filter from the form (empty = astrochron default)
cd_at_timeopt_settings <- function(p) {
  df <- cd_at_timeopt_defaults(p$fit)
  tE <- cd_at_vec(p$targetE)
  tP <- cd_at_vec(p$targetP)
  out <- list(targetE = if (is.null(tE)) { df$targetE } else { sort(tE, decreasing = TRUE) },
      targetP = if (is.null(tP)) { df$targetP } else { sort(tP, decreasing = TRUE) },
      flow = if (is.null(cd_at_pos(p$flow))) { df$flow } else { p$flow },
      fhigh = if (is.null(cd_at_pos(p$fhigh))) { df$fhigh } else { p$fhigh },
      roll = if (is.null(cd_at_pos(p$roll))) { df$roll } else { p$roll })
  out
}

cd_at_timeopt_params <- function() {
  list(
    cd_txt("targetE", "Eccentricity periods (kyr, empty = default)", "", numeric = TRUE,
        help = "In decreasing order, starting with 405 kyr; default 405.7, 130.7, 123.8, 98.9, 94.9 kyr."),
    cd_txt("targetP", "Precession periods (kyr, empty = default)", "", numeric = TRUE,
        help = "Default 23.6, 22.3, 19.1, 18.9 kyr."),
    cd_num("flow", "Low cut-off of the bandpass (cycles/kyr, 0 = default)", 0, min = 0,
        help = "Default 0.035 (precession modulation) or 0.007 (short eccentricity modulation)."),
    cd_num("fhigh", "High cut-off of the bandpass (cycles/kyr, 0 = default)", 0, min = 0,
        help = "Default 0.065 (precession modulation) or 0.0115 (short eccentricity modulation)."),
    cd_num("roll", "Roll-off of the Taner filter (dB/octave, 0 = default)", 0, min = 0))
}

# Skew-normal priors of the g and s frequencies at an age (Hoang et al. 2021): mean and standard deviation
cd_at_hoang_moments <- function(type, i, age_ga) {
  h <- astrochron::hoangPriors(ssftype = type, i = i, age = age_ga)
  dl <- h$alpha / sqrt(1 + h$alpha^2)
  list(mu = h$mu, sigma = h$sigma, alpha = h$alpha, mean = h$mu + h$sigma * dl * sqrt(2 / pi),
      sd = h$sigma * sqrt(1 - 2 * dl^2 / pi))
}

# Precession constant k (arcsec/yr) at an age from AstroGeo22 (Farhat et al. 2022)
cd_at_k_age <- function(age_ma) {
  a <- WaverideR::astrogeo22(age_ma)
  list(k = a$p_arcsec[1], sd = max(0.5, (a$p_max_arcsec[1] - a$p_min_arcsec[1]) / 2))
}

cd_tools_astrochron_testing <- function() {
  c(cd_at_tools_spectral(), cd_at_tools_sedrate(), cd_at_tools_agemodels(), cd_at_tools_astronomy())
}

# Time Series: spectral testing, surrogates and correlation ####

# Power column and background / confidence columns of an astrochron MTM spectrum
cd_at_spec_cols <- function(m) {
  pre <- switch(m, mtm = "AR1", mtmML96 = "AR1", lowspec = "LOWSPEC", mtmPL = "PowerLaw")
  fitcol <- switch(m, mtm = "AR1_fit", mtmML96 = "AR1_fit", lowspec = "LOWSPEC_back", mtmPL = "PowerLaw_fit")
  list(power = if (m == "lowspec") { "Prewhite_power" } else { "Power" },
      cl = c(fitcol, paste0(pre, c("_90_power", "_95_power", "_99_power"))))
}

# Text lines listing the highest peaks above a level
cd_at_peak_text <- function(f, pw, above, what) {
  sig <- cd_peaks(pw, above)
  if (!length(sig)) {
    return(sprintf("No peaks above %s.", what))
  }
  sig <- cd_spread_peaks(sig, pw, f, 0.01 * max(f), 12)
  c(sprintf("Peaks above %s (period, frequency):", what),
      paste0("  ", cd_fmt_period(1 / f[sig]), "  (f = ", cd_fmt_period(f[sig]), ")"))
}

# Spectrum panel inside a multi-panel figure
cd_at_spec_panel <- function(f, pw, cl = NULL, cl_names = NULL, fmax = NULL, logy = TRUE, ylab = "Power", main = NULL,
    last = FALSE, fmin = 0) {
  ok <- is.finite(f) & is.finite(pw) & f > 0 & f >= fmin
  if (!is.null(fmax) && is.finite(fmax) && fmax > 0) {
    ok <- ok & f <= fmax
  }
  if (logy) {
    ok <- ok & pw > 0
  }
  yr <- range(pw[ok], finite = TRUE)
  if (!is.null(cl)) {
    cl <- as.matrix(cl)[ok, , drop = FALSE]
    yr <- range(c(yr, cl[is.finite(cl) & cl > 0]))
  }
  vw <- cd_view_lims(range(f[ok]), yr, x = f[ok], y = pw[ok], logy = logy, last = last)
  if (logy) {
    mr <- graphics::par("mar")
    mr[2] <- max(mr[2], 5.2)
    graphics::par(mar = mr)
  }
  graphics::plot(f[ok], pw[ok], type = "n", log = if (logy) { "y" } else { "" }, xlab = "Frequency (cycles/unit)",
      ylab = if (logy) { "" } else { ylab }, xlim = vw$x, ylim = vw$y, main = main)
  if (logy) {
    graphics::title(ylab = ylab, line = 3.9)
  }
  graphics::grid(col = "grey90", lty = 1)
  cc <- c("#7f8c8d", "#c0392b", "#d35400", "#8e44ad")
  lt <- c(1, 2, 1, 3)
  if (!is.null(cl)) {
    for (j in seq_len(ncol(cl))) {
      graphics::lines(f[ok], cl[, j], col = cc[(j - 1) %% 4 + 1], lty = lt[(j - 1) %% 4 + 1])
    }
  }
  graphics::lines(f[ok], pw[ok], col = "#1f4e79", lwd = 1.1)
  if (!is.null(cl_names)) {
    graphics::legend("topright", legend = c("power", cl_names), col = c("#1f4e79", cc[seq_along(cl_names)]),
        lty = c(1, lt[seq_along(cl_names)]), bty = "n", cex = 0.75)
  }
  invisible(NULL)
}

cd_at_tools_spectral <- function() {
  M <- "Time Series"
  G1 <- "Spectral testing (astrochron)"
  G2 <- "Surrogates and correlation (astrochron)"
  G3 <- "More (astrochron)"
  band_default <- function(d) {
    f <- cd_peak_freq(d)
    sprintf("%s %s", cd_fmt(signif(0.8 * f, 3)), cd_fmt(signif(1.25 * f, 3)))
  }
  list(
    # Spectral peak tests ####
    cd_tool("at_spectest", M, "Spectral peak tests (multiple testing, AR order, power law)", group = G1,
        icon = "list-check",
        info = paste("Tests spectral peaks more strictly than one confidence level: confidence levels and p-values",
            "corrected for testing many frequencies at once (Bonferroni and five other corrections), the intermediate",
            "spectrum of Thomson et al. (2001) to choose the order of the AR background, or a power-law (1/f) fit."),
        uses = paste("astrochron::confAdjust, astrochron::multiTest, astrochron::mtmAR, astrochron::pwrLawFit,",
            "astrochron::mtm / mtmML96 / lowspec / mtmPL / periodogram"),
        params = list(
          cd_sel("method", "Test", c("Bonferroni-corrected confidence levels (confAdjust)" = "confAdjust",
              "p-values corrected for multiple testing (multiTest)" = "multiTest",
              "Intermediate spectrum, MTM / AR(p) (mtmAR)" = "mtmAR",
              "Power-law (1/f) fit to the periodogram (pwrLawFit)" = "pwrLawFit")),
          cd_sel("spec", "Spectrum and background", c("MTM, AR(1) red noise" = "mtm",
              "MTM, robust red noise (ML96)" = "mtmML96", "LOWSPEC" = "lowspec", "MTM, power-law noise" = "mtmPL"),
              show_if = list(param = "method", values = c("confAdjust", "multiTest")),
              help = "Computed without zero padding, as the multiple-testing corrections require."),
          cd_num("tbw", "Time-bandwidth product (NW)", 3, min = 1, max = 10, step = 0.5,
              show_if = list(param = "method", values = c("confAdjust", "multiTest", "mtmAR"))),
          cd_txt("bands", "Frequency bands of interest (optional): low high; low high", "",
              show_if = list(param = "method", values = c("confAdjust", "multiTest", "pwrLawFit")),
              help = paste("Only frequencies in these bands count as tests, which makes the correction less strict.",
                  "For the power-law fit: the first band is the frequency range of the fit.")),
          cd_num("order", "Order of the AR model", 1, min = 1, max = 30, step = 1,
              show_if = list(param = "method", values = "mtmAR"),
              help = "Increase the order until the intermediate spectrum stops getting flatter."),
          cd_sel("armethod", "AR estimation", c("Maximum likelihood" = "mle", "Burg" = "burg", "Yule-Walker" = "yule-walker",
              "Least squares" = "ols"), show_if = list(param = "method", values = "mtmAR")),
          cd_num("padfac", "Padding factor", 5, min = 1, step = 1, show_if = list(param = "method", values = "mtmAR")),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_num("fmax", "Maximum frequency shown (cycles/unit)", cd_nyq, min = 0),
          cd_chk("logy", "Logarithmic power axis", TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          n <- nrow(s)
          dt <- cd_dt(s$x)
          m <- p$method
          bands <- cd_at_bands(p$bands)
          txt <- cd_even_note(d, s)
          if (m %in% c("confAdjust", "multiTest")) {
            sp <- cd_at_call(p$spec, list(dat = quote(dat), tbw = p$tbw, padfac = 1, detrend = p$detrend, output = 1,
                genplot = FALSE, verbose = FALSE), list(dat = s), var = "spec")
            spec <- as.data.frame(sp$value)
            sc <- cd_at_spec_cols(p$spec)
            code <- sp$code
            if (m == "confAdjust") {
              r <- cd_at_call("confAdjust", list(spec = quote(spec), npts = n, dt = signif(dt, 10), tbw = p$tbw,
                  ntap = as.integer(round(2 * p$tbw - 1)), flow = bands$flow, fhigh = bands$fhigh, output = TRUE,
                  genplot = FALSE, verbose = FALSE), list(spec = spec))
              tab <- as.data.frame(r$value)
              res <- list(kind = "spectrum", f = tab[, 1], pw = tab[, 2], cl = tab[, 3:6], above = tab[, 5],
                  cln = c("background", "90% (Bonferroni)", "95% (Bonferroni)", "99% (Bonferroni)"),
                  ylab = if (p$spec == "lowspec") { "Pre-whitened power" } else { "Power" })
              txt <- c(txt, cd_at_peak_text(tab[, 1], tab[, 2], tab[, 5], "the Bonferroni-corrected 95% level"))
            } else {
              r <- cd_at_call("multiTest", list(spec = quote(spec), flow = bands$flow, fhigh = bands$fhigh, output = TRUE,
                  genplot = FALSE, verbose = FALSE), list(spec = spec))
              tab <- as.data.frame(r$value, check.names = FALSE)
              res <- list(kind = "multitest", f = spec$Frequency, pw = spec[[sc$power]], cl = spec[, sc$cl], tab = tab,
                  cln = c("background", "90%", "95%", "99%"))
              pk <- cd_peaks(spec[[sc$power]])
              pk <- pk[spec$Frequency[pk] %in% tab$Frequency]
              lines <- character()
              for (k in pk) {
                j <- which(tab$Frequency == spec$Frequency[k])[1]
                if (is.finite(tab[j, 4]) && tab[j, 4] < 0.05) {
                  lines <- c(lines, sprintf("  period %s (f = %s): uncorrected p = %s, FDR-BH p = %s, Bonferroni p = %s",
                      cd_fmt_period(1 / tab$Frequency[j]), cd_fmt_period(tab$Frequency[j]), format(signif(tab[j, 3], 2)),
                      format(signif(tab[j, 4], 2)), format(signif(tab[j, 9], 2))))
                }
              }
              txt <- c(txt, if (length(lines)) {
                c("Spectral peaks with a false-discovery-rate (Benjamini-Hochberg) p-value below 0.05:", lines)
              } else {
                "No spectral peak has a false-discovery-rate (Benjamini-Hochberg) p-value below 0.05."
              })
            }
            code <- c(code, r$code)
          } else if (m == "mtmAR") {
            r <- cd_at_call("mtmAR", list(dat = quote(dat), tbw = p$tbw, order = as.integer(p$order), method = p$armethod,
                padfac = p$padfac, detrend = p$detrend, output = 1, genplot = FALSE, verbose = FALSE), list(dat = s))
            tab <- as.data.frame(r$value, check.names = FALSE)
            names(tab) <- c("Frequency", "MTM_AR", "AR_CL")
            code <- c(r$code, "names(res) <- c(\"Frequency\", \"MTM_AR\", \"AR_CL\")")
            res <- list(kind = "mtmAR", tab = tab, order = p$order)
            rng <- range(log10(tab$MTM_AR[tab$Frequency > 0 & tab$MTM_AR > 0]))
            txt <- c(txt, sprintf("AR(%d) background: the intermediate spectrum spans %s orders of magnitude.",
                as.integer(p$order), cd_fmt(signif(diff(rng), 3))),
                cd_at_peak_text(tab$Frequency, tab$AR_CL, rep(95, nrow(tab)), "the 95% level of the intermediate spectrum"))
          } else {
            pg <- cd_at_call("periodogram", list(dat = quote(dat), padfac = 1, detrend = p$detrend, output = 1,
                genplot = FALSE, verbose = FALSE), list(dat = s), var = "pg")
            pgd <- as.data.frame(pg$value)
            pgd <- pgd[pgd[, 1] > 0 & pgd[, 1] < max(pgd[, 1]), c(1, 3)]
            fl <- if (is.null(bands)) { NULL } else { bands$flow[1] }
            fh <- if (is.null(bands)) { NULL } else { bands$fhigh[1] }
            r <- cd_at_call("pwrLawFit", list(spec = quote(pg), flow = fl, fhigh = fh, output = 1, genplot = FALSE,
                verbose = FALSE), list(pg = pgd))
            r2 <- cd_at_call("pwrLawFit", list(spec = quote(pg), flow = fl, fhigh = fh, output = 2, genplot = FALSE,
                verbose = FALSE), list(pg = pgd), var = "beta")
            tab <- as.data.frame(r$value)
            beta <- as.data.frame(r2$value)
            code <- c(pg$code, "pg <- pg[pg[, 1] > 0 & pg[, 1] < max(pg[, 1]), c(1, 3)]", r$code, r2$code)
            res <- list(kind = "spectrum", f = tab[, 1], pw = tab[, 2], cl = tab[, 4:7], above = tab[, 6],
                cln = c("power-law fit", "90%", "95%", "99%"), ylab = "Power")
            txt <- c(txt, sprintf("Power-law exponent beta = %s (power ~ 1 / f^beta).", cd_fmt(signif(beta[1, 1], 4))),
                cd_at_peak_text(tab[, 1], tab[, 2], tab[, 6], "the 95% power-law level"))
          }
          o <- list(cd_output(cd_out_name(file, paste0("-", m)), tab, header = TRUE, var = "res"))
          list(result = res, outputs = o, code = c(cd_even_code(d), code, cd_at_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          if (res$kind == "spectrum") {
            cd_plot_spectrum(res$f, res$pw, cl = res$cl, cl_names = res$cln, logy = p$logy, fmax = p$fmax,
                peaks_above = res$above, ylab = res$ylab)
            return(invisible(NULL))
          }
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          if (res$kind == "mtmAR") {
            tb <- res$tab
            cd_at_spec_panel(tb$Frequency, tb$MTM_AR, fmax = p$fmax, logy = p$logy, ylab = "MTM / AR power",
                main = sprintf("Intermediate spectrum, AR(%d) background", as.integer(res$order)))
            cd_par()
            ok <- tb$Frequency <= p$fmax
            vw <- cd_view_lims(range(tb$Frequency[ok]), c(0, 100), x = tb$Frequency[ok], y = tb$AR_CL[ok], last = TRUE)
            graphics::plot(tb$Frequency[ok], tb$AR_CL[ok], type = "l", col = "#1f4e79", xlab = "Frequency (cycles/unit)",
                ylab = "Confidence level (%)", xlim = vw$x, ylim = vw$y)
            graphics::abline(h = c(90, 95, 99), col = c("#c0392b", "#d35400", "#8e44ad"), lty = c(2, 1, 3))
            return(invisible(NULL))
          }
          fr <- range(res$tab[, 1])
          fr <- fr + c(-0.1, 0.1) * diff(fr)
          cd_at_spec_panel(res$f, res$pw, cl = res$cl, cl_names = res$cln, fmax = min(p$fmax, fr[2]), logy = p$logy,
              fmin = max(0, fr[1]))
          cd_par()
          tb <- res$tab[res$tab[, 1] <= p$fmax, , drop = FALSE]
          # break the lines between frequency bands
          gap <- which(diff(tb[, 1]) > 1.5 * min(diff(tb[, 1])))
          for (g in rev(gap)) {
            na_row <- tb[g, , drop = FALSE]
            na_row[1, ] <- NA
            na_row[1, 1] <- mean(tb[g:(g + 1), 1])
            tb <- rbind(tb[seq_len(g), , drop = FALSE], na_row, tb[(g + 1):nrow(tb), , drop = FALSE])
          }
          ok <- rep(TRUE, nrow(tb))
          pv <- as.matrix(tb[, 3:9])
          pv[!is.finite(pv) | pv <= 0] <- NA
          yr <- c(max(1e-6, min(pv, na.rm = TRUE)), 1)
          vw <- cd_view_lims(range(tb[ok, 1]), yr, last = TRUE)
          graphics::plot(NA, xlim = vw$x, ylim = vw$y, log = "y", xlab = "Frequency (cycles/unit)", ylab = "p-value")
          graphics::grid(col = "grey90", lty = 1)
          cc <- c("#7f8c8d", "#1f4e79", "#2e8b57", "#8e44ad", "#d35400", "#16a085", "#c0392b")
          for (j in seq_len(ncol(pv))) {
            graphics::lines(tb[ok, 1], pv[, j], col = cc[j], lwd = if (j == 2) { 1.6 } else { 1 })
          }
          graphics::abline(h = c(0.05, 0.01), lty = c(2, 3), col = "grey30")
          graphics::legend("bottomright", legend = names(tb)[3:9], col = cc, lwd = 1, bty = "n", cex = 0.7, ncol = 2)
        }),
    # False-positive simulation ####
    cd_tool("at_testbg", M, "False positives of a spectral background (testBackground)", group = G1, heavy = TRUE,
        icon = "dice",
        info = paste("Simulates noise series with the length, sampling and noise of your record and counts how often",
            "each background method (AR(1), ML96, LOWSPEC, power law) finds false peaks, in total and per frequency,",
            "also after multiple-testing corrections. Use it to choose a background method you can trust."),
        uses = "astrochron::testBackground, astrochron::pwrLawFit",
        params = list(
          cd_sel("noise", "Noise of the simulated series", c("AR(1) red noise" = "ar1", "Power-law noise" = "pwrLaw")),
          cd_num("coeff", "AR(1) coefficient rho or power-law exponent beta (0 = from the data)", 0, min = 0,
              help = "By default rho is the lag-1 autocorrelation of the detrended record, beta its power-law slope."),
          cd_sel("bmethod", "Background method tested", c("MTM, AR(1)" = "mtmAR1", "MTM, robust red noise (ML96)" = "mtmML96",
              "LOWSPEC" = "lowspec", "MTM, power law" = "mtmPL", "Periodogram, power law" = "periodogramPL",
              "Periodogram, AR(1)" = "periodogramAR1")),
          cd_num("tbw", "Time-bandwidth product (NW, MTM methods)", 3, min = 1, max = 10, step = 0.5),
          cd_chk("detrend", "Remove a linear trend from the simulated series", FALSE),
          cd_num("low", "Remove a LOWESS trend (span 0-1, 0 = no)", 0, min = 0, max = 1, step = 0.05),
          cd_chk("multi", "Also evaluate multiple-testing corrections", TRUE),
          cd_num("iter", "Number of simulated series", 200, min = 10, step = 100)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          n <- nrow(s)
          dt <- cd_dt(s$x)
          y <- s$y - stats::fitted(stats::lm(s$y ~ s$x))
          note <- character()
          coeff <- cd_at_pos(p$coeff)
          code0 <- character()
          if (is.null(coeff)) {
            if (p$noise == "ar1") {
              coeff <- signif(stats::cor(y[-1], y[-n]), 4)
              note <- sprintf("AR(1) coefficient rho = %s (lag-1 autocorrelation of the detrended record).", cd_fmt(coeff))
              code0 <- "# rho: lag-1 autocorrelation of the detrended record"
            } else {
              pg <- astrochron::periodogram(s, padfac = 1, output = 1, genplot = FALSE, verbose = FALSE)
              pg <- pg[pg[, 1] > 0 & pg[, 1] < max(pg[, 1]), c(1, 3)]
              b <- cd_at_quiet(astrochron::pwrLawFit(pg, output = 2, genplot = FALSE, verbose = FALSE))$value
              coeff <- signif(b[1, 1], 4)
              note <- sprintf("Power-law exponent beta = %s (pwrLawFit of the record).", cd_fmt(coeff))
              code0 <- "# beta: astrochron::pwrLawFit(periodogram of the record, output = 2)"
            }
          }
          r <- cd_at_call("testBackground", list(npts = n, dt = signif(dt, 10), noiseType = p$noise, coeff = coeff,
              method = p$bmethod, low = p$low, detrend = p$detrend, tbw = p$tbw, multi = p$multi,
              iter = as.integer(p$iter), output = TRUE, genplot = FALSE, verbose = TRUE), warn = FALSE)
          out <- as.data.frame(r$value)
          # testBackground recycles the frequency column over npts rows: keep the rows up to the Nyquist frequency
          nf <- which(diff(out[, 1]) <= 0)
          nf <- if (length(nf)) { nf[1] } else { nrow(out) }
          tab <- data.frame(frequency = out[seq_len(nf), 1], false_pos_90 = 100 * out[seq_len(nf), 2] / p$iter,
              false_pos_95 = 100 * out[seq_len(nf), 3] / p$iter, false_pos_99 = 100 * out[seq_len(nf), 4] / p$iter)
          # percentage of each simulated spectrum above the 90, 95 and 99% levels (printed as a matrix)
          rows <- grep("^ *\\[[0-9]+,\\]", r$log)
          frac <- matrix(NA_real_, length(rows), 3)
          for (i in seq_along(rows)) {
            v <- suppressWarnings(as.numeric(strsplit(trimws(sub("^ *\\[[0-9]+,\\]", "", r$log[rows[i]])), "[[:space:]]+")[[1]]))
            if (length(v) == 3) {
              frac[i, ] <- v
            }
          }
          summ <- character()
          lev <- c("90%", "95%", "99%")
          k <- 0L
          for (line in trimws(r$log)) {
            if (grepl("^Median estimate", line)) {
              summ <- c(summ, paste("  Median percentage of a spectrum above the", sub("^Median estimate ", "", line)))
            } else if (grepl("^Total percent", line) && !grepl("% spectra", line, fixed = TRUE)) {
              summ <- c(summ, paste("  Percentage of all frequencies above the", sub("^Total percent > ", "", line)))
            } else if (grepl("\\(.*[0-9]{4}\\):$|^Bonferroni:$", line)) {
              summ <- c(summ, paste0(sub(":$", "", line), ", spectra with at least one false positive:"))
              k <- 0L
            } else if (grepl("% spectra", line, fixed = TRUE)) {
              k <- k + 1L
              summ <- c(summ, sprintf("  at the %s level: %s%%", lev[min(k, 3L)],
                  cd_fmt(signif(cd_at_grab(line, "% spectra ="), 3))))
            }
          }
          o <- list(cd_output(cd_out_name(file, paste0("-testBackground_", p$bmethod)), tab, header = TRUE, var = "tab"))
          code <- c(code0, r$code, "nf <- which(diff(res[, 1]) <= 0)[1]",
              sprintf("tab <- data.frame(frequency = res[1:nf, 1], 100 * res[1:nf, 2:4] / %d)", as.integer(p$iter)),
              cd_at_code_write(o))
          list(result = list(tab = tab, frac = frac, method = p$bmethod), outputs = o, code = code,
              text = c(note, sprintf("%d simulated series of %d points, %s noise, background method %s:", as.integer(p$iter),
                  n, p$noise, p$bmethod), summ))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cc <- c("#c0392b", "#8e44ad", "#2e8b57")
          cd_par()
          fr <- res$frac
          fr <- fr[stats::complete.cases(fr), , drop = FALSE]
          if (nrow(fr) > 2) {
            dens <- list()
            ymax <- 0
            for (j in 1:3) {
              dens[[j]] <- stats::density(fr[, j], from = 0)
              ymax <- max(ymax, dens[[j]]$y)
            }
            graphics::plot(NA, xlim = c(0, max(20, max(fr) * 1.1)), ylim = c(0, ymax), xlab = "Percentage of the spectrum above the level",
                ylab = "Density", main = "False positives per simulated spectrum")
            for (j in 1:3) {
              graphics::lines(dens[[j]], col = cc[j], lwd = 1.5)
            }
            graphics::abline(v = c(10, 5, 1), col = cc, lty = 3)
            graphics::legend("topright", c("90% level", "95% level", "99% level"), col = cc, lwd = 1.5, bty = "n", cex = 0.8)
          } else {
            graphics::plot.new()
          }
          cd_par()
          tb <- res$tab
          yr <- c(0, max(c(10, as.matrix(tb[, 2:4])), na.rm = TRUE))
          vw <- cd_view_lims(range(tb$frequency), yr, last = TRUE)
          graphics::plot(NA, xlim = vw$x, ylim = vw$y, xlab = "Frequency (cycles/unit)", ylab = "False positives (%)",
              main = "Distribution of false positives across the spectrum")
          graphics::grid(col = "grey90", lty = 1)
          for (j in 1:3) {
            graphics::lines(tb$frequency, tb[, j + 1], col = cc[j])
          }
          graphics::abline(h = c(10, 5, 1), col = cc, lty = 3)
        }),
    # Band power and spectra from EHA ####
    cd_tool("at_bandpower", M, "EHA band power and spectrum at a depth", group = G1, icon = "chart-column",
        info = paste("Evolutive harmonic analysis (EHA), then the power in a frequency band through the record and its",
            "share of the total power (e.g. how strong precession is along the section), or the spectrum at one depth."),
        uses = "astrochron::eha, astrochron::integratePower, astrochron::extract",
        params = list(
          cd_sel("method", "Result", c("Power in a band through the record (integratePower)" = "integrate",
              "Spectrum at one depth (extract)" = "extract")),
          cd_num("win", "Window length (depth units)", function(d) { signif(diff(range(d[, 1], na.rm = TRUE)) * 0.2, 3) },
              min = 0),
          cd_num("step", "Step (depth units)", function(d) { signif(diff(range(d[, 1], na.rm = TRUE)) * 0.01, 3) }, min = 0),
          cd_num("tbw", "Time-bandwidth product (NW)", 2, min = 1, max = 10, step = 0.5),
          cd_num("fmax", "Highest frequency of the EHA (cycles/unit)",
              function(d) { signif(min(cd_nyq(d), 5 * cd_peak_freq(d)), 3) }, min = 0),
          cd_txt("band", "Frequency band: low high (cycles/unit)", band_default,
              show_if = list(param = "method", values = "integrate"),
              help = "Default: around the highest peak of the periodogram."),
          cd_chk("unity", "Normalise each spectrum to unit total power", FALSE,
              show_if = list(param = "method", values = "integrate")),
          cd_num("at", "Depth of the spectrum", function(d) { signif(mean(range(d[, 1], na.rm = TRUE)), 4) },
              show_if = list(param = "method", values = "extract")),
          cd_sel("quantity", "Quantity", c("Power" = "2", "Amplitude" = "3", "Harmonic F-test probability" = "4"),
              show_if = list(param = "method", values = "extract")),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dt <- cd_dt(s$x)
          winpts <- as.integer(floor(p$win / dt) + 1)
          if (winpts >= nrow(s)) {
            stop("the window is longer than the record", call. = FALSE)
          }
          pad <- as.integer(max(2 * 2^ceiling(log2(winpts)), 512))
          out <- if (p$method == "extract") { as.integer(p$quantity) } else { 2L }
          e <- cd_at_call("eha", list(dat = quote(dat), tbw = p$tbw, pad = pad, fmax = signif(p$fmax, 6), step = p$step,
              win = p$win, output = out, genplot = FALSE, verbose = FALSE), list(dat = s), var = "spec")
          spec <- as.data.frame(e$value)
          x <- as.numeric(sub("^X", "", names(spec)[-1]))
          z <- t(as.matrix(spec[, -1]))
          code <- c(cd_even_code(d), e$code)
          if (p$method == "integrate") {
            b <- cd_at_bands(p$band)
            if (is.null(b)) {
              stop("give the frequency band as two numbers, e.g. \"1.6 2.4\"", call. = FALSE)
            }
            r <- cd_at_call("integratePower", list(spec = quote(spec), flow = b$flow[1], fhigh = b$fhigh[1],
                unity = p$unity, npts = winpts, pad = pad, genplot = FALSE, verbose = FALSE), list(spec = spec))
            tab <- as.data.frame(r$value, check.names = FALSE)
            names(tab) <- c("location", "band_power", "total_power", "band_total_ratio")
            ser <- data.frame(location = tab$location, ratio = tab$band_total_ratio)
            o <- list(cd_output(cd_out_name(file, "-bandpower"), tab, header = TRUE, var = "res"),
                cd_output(cd_out_name(file, "-bandratio"), ser, var = "ratio"))
            code <- c(code, r$code, "names(res) <- c(\"location\", \"band_power\", \"total_power\", \"band_total_ratio\")",
                "ratio <- res[, c(1, 4)]", cd_at_code_write(o))
            txt <- sprintf("Band %s-%s cycles/unit holds %s to %s of the power in the windows (mean %s).",
                cd_fmt(b$flow[1]), cd_fmt(b$fhigh[1]), cd_fmt(signif(min(tab$band_total_ratio), 3)),
                cd_fmt(signif(max(tab$band_total_ratio), 3)), cd_fmt(signif(mean(tab$band_total_ratio), 3)))
            res <- list(method = "integrate", x = x, f = spec[, 1], z = z, tab = tab, band = c(b$flow[1], b$fhigh[1]))
          } else {
            j <- which.min(abs(x - p$at))
            sp1 <- data.frame(frequency = spec[, 1], value = spec[, j + 1])
            code <- c(code, sprintf("one <- astrochron::extract(spec, get = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(x[j])))
            r <- cd_at_quiet(astrochron::extract(spec, get = x[j], genplot = FALSE, verbose = FALSE))
            if (is.data.frame(r$value) || is.matrix(r$value)) {
              sp1 <- data.frame(frequency = r$value[, 1], value = r$value[, 2])
            }
            qn <- c("2" = "power", "3" = "amplitude", "4" = "probability")[[p$quantity]]
            names(sp1)[2] <- qn
            o <- list(cd_output(cd_out_name(file, sprintf("-eha_%s_at_%s", qn, cd_fmt(x[j]))), sp1, header = TRUE,
                var = "one"))
            code <- c(code, sprintf("names(one) <- c(\"frequency\", %s)", cd_dep(qn)), cd_at_code_write(o))
            pk <- cd_peaks(sp1[, 2])
            pk <- cd_spread_peaks(pk, sp1[, 2], sp1[, 1], 0.02 * max(sp1[, 1]), 8)
            txt <- c(sprintf("Spectrum of the window centred at %s.", cd_fmt(x[j])), "Highest peaks (period):",
                paste0("  ", cd_fmt_period(1 / sp1[pk, 1])))
            res <- list(method = "extract", x = x, f = spec[, 1], z = z, at = x[j], one = sp1, qn = qn)
          }
          list(result = res, outputs = o, code = code, text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1), heights = c(1, 1))
          cd_par()
          zz <- res$z
          if (res$method == "integrate" || identical(res$qn, "power")) {
            zz <- log10(pmax(zz, max(zz, na.rm = TRUE) * 1e-8))
          }
          cd_at_image_panel(res$x, res$f, zz, xlab = "Depth", ylab = "Frequency (cycles/unit)", palette = p$palette,
              main = if (res$method == "integrate" || identical(res$qn, "power")) { "EHA, log10 power" } else { paste("EHA,", res$qn) })
          if (res$method == "integrate") {
            graphics::abline(h = res$band, col = "white", lty = 2, lwd = 1.5)
            cd_par()
            tb <- res$tab
            vw <- cd_view_lims(range(tb$location), range(tb$band_total_ratio), x = tb$location, y = tb$band_total_ratio,
                last = TRUE)
            graphics::plot(tb$location, tb$band_total_ratio, type = "l", col = "#1f4e79", lwd = 1.5, xlab = "Depth",
                ylab = "Band / total power", xlim = vw$x, ylim = vw$y,
                main = sprintf("Power between %s and %s cycles/unit", cd_fmt(res$band[1]), cd_fmt(res$band[2])))
            graphics::grid(col = "grey90", lty = 1)
            return(invisible(NULL))
          }
          graphics::abline(v = res$at, col = "white", lty = 2, lwd = 1.5)
          cd_par()
          one <- res$one
          cd_at_spec_panel(one[, 1], one[, 2], logy = identical(res$qn, "power"), ylab = tools::toTitleCase(res$qn),
              main = sprintf("Window centred at %s", cd_fmt(res$at)), last = TRUE)
        }),
    # Bicoherence ####
    cd_tool("at_bicoherence", M, "Bicoherence (WOSA, astrochron)", group = G1, heavy = TRUE, icon = "border-all",
        info = paste("Bispectrum and bicoherence by Welch overlapping segments (Choudhury et al. 2008), as in Sullivan",
            "et al. (2023): high bicoherence at (f1, f2) means f1 + f2 is a combination tone with coupled phases."),
        uses = "astrochron::bicoherence",
        params = list(
          cd_num("segments", "Number of segments", 8, min = 2, step = 1),
          cd_num("overlap", "Overlap of the segments (%)", 50, min = 0, max = 50, step = 10),
          cd_num("padfac", "Padding factor", 2, min = 1, step = 1),
          cd_num("CL", "Confidence level of the contour (%)", 95, min = 50, max = 99.9),
          cd_num("maxF", "Highest frequency (cycles/unit)", function(d) { signif(min(cd_nyq(d), 5 * cd_peak_freq(d)), 3) },
              min = 0),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_chk("taper", "Hanning taper", TRUE),
          cd_txt("id", "Frequencies to mark (optional)", "", numeric = TRUE),
          cd_sel("show", "Figure", c("Bicoherence" = "bic", "Bispectrum" = "bis", "Power spectrum" = "pwr")),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          r <- cd_at_call("bicoherence", list(dat = quote(dat), overlap = p$overlap, segments = as.integer(p$segments),
              CL = p$CL, padfac = p$padfac, detrend = p$detrend, taper = p$taper, maxF = signif(p$maxF, 6), output = 4,
              genplot = FALSE, verbose = FALSE), list(dat = s))
          v <- r$value
          pw <- data.frame(frequency = v[[1]][, 1], power = v[[1]][, 2])
          f <- v[[2]][, 1]
          bis <- as.matrix(v[[2]][, -1])
          bic <- as.matrix(v[[3]][, -1])
          cl <- as.matrix(v[[4]][, -1])
          tab <- data.frame(frequency = f, bic, check.names = FALSE)
          names(tab) <- c("frequency", cd_fmt_period(f))
          o <- list(cd_output(cd_out_name(file, "-bicoherence"), tab, header = TRUE, var = "bic"),
              cd_output(cd_out_name(file, "-bicoherence_power"), pw, header = TRUE, var = "pwr"))
          code <- c(cd_even_code(d), r$code, "bic <- res[[3]]", "pwr <- res[[1]]", cd_at_code_write(o))
          bic2 <- bic
          bic2[!is.finite(cl) | cl < p$CL] <- NA
          txt <- character()
          if (any(is.finite(bic2))) {
            k <- which(bic2 == max(bic2, na.rm = TRUE), arr.ind = TRUE)[1, ]
            txt <- sprintf("Highest bicoherence above the %s%% level: %s at f1 = %s, f2 = %s (f1 + f2 = %s).",
                cd_fmt(p$CL), cd_fmt(signif(bic[k[1], k[2]], 3)), cd_fmt(signif(f[k[1]], 4)), cd_fmt(signif(f[k[2]], 4)),
                cd_fmt(signif(f[k[1]] + f[k[2]], 4)))
          } else {
            txt <- sprintf("No bicoherence above the %s%% level.", cd_fmt(p$CL))
          }
          list(result = list(f = f, bis = bis, bic = bic, cl = cl, pw = pw, CL = p$CL), outputs = o, code = code,
              text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          if (p$show == "pwr") {
            cd_plot_spectrum(res$pw$frequency, res$pw$power, label_peaks = TRUE)
            return(invisible(NULL))
          }
          z <- if (p$show == "bic") { res$bic } else { res$bis }
          f <- res$f
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(2, 1), nrow = 1), widths = c(1, graphics::lcm(3.1)))
          cols <- cd_palette(100, p$palette)
          zr <- range(z, finite = TRUE)
          cd_par()
          graphics::par(mar = c(4, 0.4, 1.6, 3.4))
          zz <- seq(zr[1], zr[2], length.out = 100)
          graphics::image(1, zz, matrix(zz, nrow = 1), col = cols, axes = FALSE, xlab = "", ylab = "")
          graphics::axis(4, cex.axis = 0.8)
          graphics::mtext(if (p$show == "bic") { "Bicoherence" } else { "Bispectrum" }, side = 4, line = 2.3, cex = 0.8, las = 0)
          graphics::box()
          cd_par()
          vw <- cd_view_lims(range(f), range(f), last = TRUE)
          graphics::image(f, f, z, col = cols, zlim = zr, xlab = "Frequency f1 (cycles/unit)", ylab = "Frequency f2 (cycles/unit)",
              xlim = vw$x, ylim = vw$y, useRaster = cd_regular(f))
          clm <- res$cl
          clm[!is.finite(clm)] <- 0
          if (any(clm >= res$CL)) {
            graphics::contour(f, f, clm, levels = res$CL, add = TRUE, drawlabels = FALSE, col = "white", lwd = 1.2)
          }
          idf <- cd_at_vec(p$id)
          for (fi in idf) {
            graphics::abline(a = fi, b = -1, col = "#ff6b4a", lty = 2)
          }
          graphics::box()
        }),
    # Surrogates and noise ####
    cd_tool("at_surrogates", M, "Phase-randomised surrogates and spectral noise", group = G2, icon = "shuffle",
        info = paste("Surrogate series with the same power spectrum as the record but random phases (Ebisuzaki 1997),",
            "or noise generated from a theoretical spectrum (power law or AR(1); Timmer and Konig 1995) with the",
            "length, sampling, mean and standard deviation of the record."),
        uses = "astrochron::surrogates, astrochron::makeNoise, astrochron::pwrLawFit",
        params = list(
          cd_sel("method", "Method", c("Phase-randomised surrogates of the record (surrogates)" = "surrogates",
              "Noise from a theoretical spectrum (makeNoise)" = "makeNoise")),
          cd_num("nsim", "Number of series", 1, min = 1, step = 1),
          cd_chk("preserveMean", "Keep the mean of the record", TRUE, show_if = list(param = "method", values = "surrogates")),
          cd_chk("std", "Keep the variance of the record", TRUE, show_if = list(param = "method", values = "surrogates")),
          cd_sel("model", "Spectrum", c("Power law 1 / f^beta" = "pl", "AR(1) red noise" = "ar1"),
              show_if = list(param = "method", values = "makeNoise")),
          cd_num("coeff", "beta or rho (0 = from the record)", 0, min = 0, show_if = list(param = "method", values = "makeNoise")),
          cd_num("seed", "Random seed (0 = random)", 0, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          n <- nrow(s)
          dt <- cd_dt(s$x)
          nsim <- max(1L, as.integer(p$nsim))
          code <- cd_even_code(d)
          if (is.finite(p$seed) && p$seed != 0) {
            set.seed(as.integer(p$seed))
            code <- c(code, sprintf("set.seed(%d)", as.integer(p$seed)))
          }
          txt <- cd_even_note(d, s)
          if (p$method == "surrogates") {
            r <- cd_at_call("surrogates", list(dat = quote(dat), nsim = nsim, preserveMean = p$preserveMean, std = p$std,
                genplot = FALSE, verbose = FALSE), list(dat = s))
            v <- r$value
            sims <- if (nsim == 1) { matrix(as.data.frame(v)[, 2], ncol = 1) } else { as.matrix(v) }
            code <- c(code, r$code, if (nsim == 1) { "sims <- res" } else { "sims <- data.frame(dat[, 1], res)" })
          } else {
            y <- s$y - stats::fitted(stats::lm(s$y ~ s$x))
            coeff <- cd_at_pos(p$coeff)
            if (is.null(coeff)) {
              if (p$model == "ar1") {
                coeff <- signif(stats::cor(y[-1], y[-n]), 4)
                txt <- c(txt, sprintf("AR(1) coefficient rho = %s (lag-1 autocorrelation of the detrended record).",
                    cd_fmt(coeff)))
              } else {
                pg <- astrochron::periodogram(s, padfac = 1, output = 1, genplot = FALSE, verbose = FALSE)
                pg <- pg[pg[, 1] > 0 & pg[, 1] < max(pg[, 1]), c(1, 3)]
                coeff <- signif(cd_at_quiet(astrochron::pwrLawFit(pg, output = 2, genplot = FALSE, verbose = FALSE))$value[1, 1], 4)
                txt <- c(txt, sprintf("Power-law exponent beta = %s (pwrLawFit of the record).", cd_fmt(coeff)))
              }
            }
            nS <- floor(n / 2) + 1
            f <- seq(0, 0.5 / dt, length.out = nS)
            if (p$model == "ar1") {
              S <- (1 - coeff^2) / (1 - 2 * coeff * cos(2 * pi * f * dt) + coeff^2)
              scode <- sprintf("S <- (1 - %s^2) / (1 - 2 * %s * cos(2 * pi * f * %s) + %s^2)", cd_fmt(coeff), cd_fmt(coeff),
                  cd_fmt(dt), cd_fmt(coeff))
            } else {
              S <- c(0, f[-1]^(-coeff))
              scode <- sprintf("S <- c(0, f[-1]^(-%s))", cd_fmt(coeff))
            }
            r <- cd_at_call("makeNoise", list(S = quote(S), dt = signif(dt, 10), mean = signif(mean(s$y), 8),
                sdev = signif(stats::sd(s$y), 8), nsim = nsim, genplot = FALSE, verbose = FALSE), list(S = S))
            v <- as.data.frame(r$value)
            sims <- as.matrix(v[, -1, drop = FALSE])
            code <- c(code, sprintf("f <- seq(0, 0.5 / %s, length.out = %d)", cd_fmt(dt), as.integer(nS)), scode, r$code,
                sprintf("res[, 1] <- res[, 1] + %s", cd_fmt(s$x[1])), "sims <- res")
          }
          m <- nrow(sims)
          xx <- s$x[1] + (seq_len(m) - 1) * dt
          if (m == n) {
            xx <- s$x
          }
          tab <- data.frame(x = xx, sims)
          names(tab) <- c(names(d)[1], paste0("sim", seq_len(ncol(sims))))
          tag <- if (p$method == "surrogates") { "-surrogate" } else { paste0("-noise_", p$model) }
          o <- list(cd_output(cd_out_name(file, tag), tab, header = ncol(tab) > 2, var = "sims"))
          list(result = list(s = s, tab = tab), outputs = o, code = c(code, cd_at_code_write(o)),
              text = c(txt, sprintf("%d series of %d points written.", ncol(sims), m)))
        },
        plot = function(res, p) {
          tb <- res$tab
          k <- min(3, ncol(tb) - 1)
          ser <- list(res$s)
          lab <- "record"
          for (j in seq_len(k)) {
            ser[[j + 1]] <- tb[, c(1, j + 1)]
            lab <- c(lab, names(tb)[j + 1])
          }
          cd_plot_series(ser, labels = lab, stack = TRUE, xlab = "Depth / time")
        }),
    # Correlation of two series ####
    cd_tool("at_correlate", M, "Correlation of two series (surrogateCor / slideCor)", group = G2, inputs = 2L,
        heavy = function(p) { identical(p$method, "surrogateCor") }, icon = "arrows-left-right-to-line",
        info = paste("Correlation of two records with a significance test that respects their serial correlation",
            "(phase-randomised surrogates, also for different sampling; Baddouh et al. 2016), or the shift of the second",
            "record that correlates best with the first (slideCor)."),
        uses = "astrochron::surrogateCor, astrochron::slideCor",
        params = list(
          cd_sel("method", "Method", c("Correlation with surrogate significance (surrogateCor)" = "surrogateCor",
              "Best shift between the records (slideCor)" = "slideCor")),
          cd_sel("cormethod", "Correlation coefficient", c("Pearson" = "1", "Spearman rank" = "2", "Kendall rank" = "3")),
          cd_chk("firstDiff", "Correlate first differences", FALSE, show_if = list(param = "method", values = "surrogateCor")),
          cd_num("nsim", "Number of surrogates", 1000, min = 10, step = 100,
              show_if = list(param = "method", values = "surrogateCor")),
          cd_chk("rev", "Reverse the polarity of the second record", FALSE,
              show_if = list(param = "method", values = "slideCor")),
          cd_chk("detrend", "Remove the linear trend of each window", FALSE,
              show_if = list(param = "method", values = "slideCor")),
          cd_num("minpts", "Smallest overlap (points, 0 = default)", 0, min = 0, step = 10,
              show_if = list(param = "method", values = "slideCor"))),
        compute = function(d, p, d2, file, file2) {
          s1 <- cd_clean(d)
          s2 <- cd_clean(d2)
          cm <- as.integer(p$cormethod)
          code <- c(cd_code_read(file2, var = "dat2"))
          if (p$method == "surrogateCor") {
            r <- cd_at_call("surrogateCor", list(dat1 = quote(dat), dat2 = quote(dat2), firstDiff = p$firstDiff,
                cormethod = cm, nsim = as.integer(p$nsim), output = 1, genplot = FALSE, verbose = TRUE),
                list(dat = s1, dat2 = s2), var = "sims")
            sims <- as.numeric(unlist(r$value))
            rdat <- cd_at_grab(r$log, "Data correlation coefficient =")
            pv <- cd_at_pval(sum(abs(sims) > abs(rdat)), length(sims))
            r3 <- cd_at_call("surrogateCor", list(dat1 = quote(dat), dat2 = quote(dat2), firstDiff = p$firstDiff,
                cormethod = cm, nsim = 0L, output = 3, genplot = FALSE, verbose = FALSE), list(dat = s1, dat2 = s2),
                var = "pairs")
            pairs <- as.data.frame(r3$value)
            tab <- data.frame(r = rdat, p = pv$p, n = nrow(pairs), nsim = length(sims))
            o <- list(cd_output(cd_out_name(file, "-surrogateCor"), tab, header = TRUE, var = "tab"),
                cd_output(cd_out_name(file, "-surrogateCor_pairs"), pairs, header = TRUE, var = "pairs"))
            code <- c(code, r$code, r3$code, "r <- cor(pairs[, 2], pairs[, 3])",
                "p <- sum(abs(sims[, 1]) > abs(r)) / nrow(sims)  # astrochron reports p < 10 / nsim at least",
                "tab <- data.frame(r = r, p = max(p, 10 / nrow(sims)), n = nrow(pairs), nsim = nrow(sims))",
                cd_at_code_write(o))
            txt <- sprintf("Correlation r = %s on %d resampled pairs; %s (%d phase-randomised surrogates).",
                cd_fmt(signif(rdat, 4)), nrow(pairs), pv$text, length(sims))
            res <- list(method = "surrogateCor", sims = sims, r = rdat, pairs = pairs)
          } else {
            e1 <- cd_even(d)
            e2 <- cd_even(d2)
            mp <- cd_at_pos(p$minpts)
            r <- cd_at_call("slideCor", list(dat1 = quote(dat), dat2 = quote(dat2), rev = p$rev, cormethod = cm,
                minpts = if (is.null(mp)) { NULL } else { as.integer(mp) }, detrend = p$detrend, output = TRUE,
                genplot = FALSE, verbose = FALSE), list(dat = e1, dat2 = e2))
            tab <- as.data.frame(r$value)
            names(tab) <- c("shift", "r", "points")
            k <- which.max(abs(tab$r))
            o <- list(cd_output(cd_out_name(file, "-slideCor"), tab, header = TRUE, var = "res"))
            code <- c(code, sprintf("dat <- astrochron::linterp(dat, dt = %s, genplot = FALSE)", cd_fmt(cd_dt(e1$x))),
                sprintf("dat2 <- astrochron::linterp(dat2, dt = %s, genplot = FALSE)", cd_fmt(cd_dt(e2$x))), r$code,
                "names(res) <- c(\"shift\", \"r\", \"points\")", cd_at_code_write(o))
            txt <- sprintf("Strongest correlation r = %s at a shift of %s: record 2 starts at %s of record 1 (%d points overlap).",
                cd_fmt(signif(tab$r[k], 4)), cd_fmt(tab$shift[k]), cd_fmt(e1$x[1] + tab$shift[k]), as.integer(tab$points[k]))
            res <- list(method = "slideCor", tab = tab, best = tab[k, ], s1 = e1, s2 = e2, rev = p$rev)
          }
          list(result = res, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          if (res$method == "surrogateCor") {
            pr <- res$pairs
            graphics::plot(pr[, 2], pr[, 3], pch = 16, cex = 0.6, col = grDevices::adjustcolor("#1f4e79", 0.6),
                xlab = "Record 1", ylab = "Record 2", main = sprintf("r = %s", cd_fmt(signif(res$r, 3))))
            graphics::grid(col = "grey90", lty = 1)
            cd_par()
            cd_at_hist(res$sims, res$r, xlab = "Correlation of the surrogates", two_sided = TRUE)
            return(invisible(NULL))
          }
          b <- res$best
          s1 <- res$s1
          s2 <- res$s2
          y2 <- if (isTRUE(res$rev)) { -s2$y } else { s2$y }
          x2 <- s2$x - s2$x[1] + s1$x[1] + b$shift
          z1 <- (s1$y - mean(s1$y)) / stats::sd(s1$y)
          z2 <- (y2 - mean(y2)) / stats::sd(y2)
          graphics::plot(s1$x, z1, type = "l", col = "#1f4e79", xlim = range(c(s1$x, x2)), ylim = range(c(z1, z2)),
              xlab = "Position in record 1", ylab = "Standardised value",
              main = sprintf("Best shift %s, r = %s", cd_fmt(b$shift), cd_fmt(signif(b$r, 3))))
          graphics::lines(x2, z2, col = "#c0392b")
          graphics::legend("topright", c("record 1", "record 2, shifted"), col = c("#1f4e79", "#c0392b"), lwd = 1,
              bty = "n", cex = 0.8)
          cd_par()
          tb <- res$tab
          vw <- cd_view_lims(range(tb$shift), range(tb$r), x = tb$shift, y = tb$r, last = TRUE)
          graphics::plot(tb$shift, tb$r, type = "l", col = "#1f4e79", xlab = "Shift", ylab = "Correlation r",
              xlim = vw$x, ylim = vw$y)
          graphics::grid(col = "grey90", lty = 1)
          graphics::abline(v = b$shift, lty = 2, col = "#c0392b")
        }),
    # Bioturbation impulse response ####
    cd_tool("at_impulse", M, "Bioturbation impulse response", group = G3, inputs = 0L, icon = "worm",
        info = paste("Response of the advection-diffusion bioturbation model of Schink and Guinasso (1975) to an impulse:",
            "how a single event is smeared over depth and time by a mixed layer."),
        uses = "astrochron::impulseResponse",
        params = list(
          cd_num("G", "Bioturbation parameter G = D / (ML v)", 4, min = 0.01),
          cd_num("ML", "Mixed-layer depth (cm)", 10, min = 0.01),
          cd_num("v", "Sedimentation rate (cm/kyr)", 2, min = 0.001)),
        compute = function(d, p, d2, file, file2) {
          r <- cd_at_call("impulseResponse", list(G = p$G, ML = p$ML, v = p$v, genplot = FALSE, verbose = FALSE), var = "fc")
          fc <- as.numeric(r$value)
          xx <- seq_along(fc) * 0.01
          tab <- data.frame(depth_ML = xx, depth_cm = xx * p$ML, time_kyr = xx * p$ML / p$v, response = fc)
          nm <- sprintf("impulseResponse_G%s_ML%s_v%s.txt", cd_fmt(p$G), cd_fmt(p$ML), cd_fmt(p$v))
          o <- list(cd_output(nm, tab, header = TRUE, var = "tab"))
          code <- c(r$code, "x <- seq_along(fc) * 0.01",
              sprintf("tab <- data.frame(depth_ML = x, depth_cm = x * %s, time_kyr = x * %s / %s, response = fc)",
                  cd_fmt(p$ML), cd_fmt(p$ML), cd_fmt(p$v)), cd_at_code_write(o))
          k <- which.max(fc)
          list(result = list(tab = tab), outputs = o, code = code,
              text = sprintf("The impulse is deposited at %s cm (%s kyr); the response peaks at %s cm (%s kyr).",
                  cd_fmt(5 * p$ML), cd_fmt(signif(5 * p$ML / p$v, 4)), cd_fmt(signif(tab$depth_cm[k], 4)),
                  cd_fmt(signif(tab$time_kyr[k], 4))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          tb <- res$tab
          cd_par()
          graphics::plot(tb$depth_cm, tb$response, type = "l", lwd = 1.5, col = "#1f4e79", xlab = "Depth (cm)",
              ylab = "Response")
          graphics::abline(v = 5 * p$ML, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          vw <- cd_view_lims(range(tb$time_kyr), range(tb$response), x = tb$time_kyr, y = tb$response, last = TRUE)
          graphics::plot(tb$time_kyr, tb$response, type = "l", lwd = 1.5, col = "#2e8b57", xlab = "Time (kyr, present to past)",
              ylab = "Response", xlim = vw$x, ylim = vw$y)
          graphics::abline(v = 5 * p$ML / p$v, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
        })
  )
}

# Sed. rate: astronomical testing ####

# Template of a variable sedimentation rate for timeOptTemplate; returns list(template, code)
cd_at_template <- function(s, type, depths) {
  if (type == "data") {
    return(list(template = NULL, code = character()))
  }
  tm <- s
  code <- "template <- dat"
  if (type == "linear") {
    tm[, 2] <- s[, 1]
    code <- c(code, "template[, 2] <- dat[, 1]")
    return(list(template = tm, code = code))
  }
  dp <- cd_at_vec(depths)
  if (is.null(dp)) {
    stop("give the depths of the steps or hiatuses", call. = FALSE)
  }
  dp <- sort(dp)
  if (type == "step") {
    tm[, 2] <- 1 + findInterval(s[, 1], dp)
    code <- c(code, sprintf("template[, 2] <- 1 + findInterval(dat[, 1], %s)", cd_dep(dp)))
    return(list(template = tm, code = code))
  }
  tm[, 2] <- 10
  code <- c(code, "template[, 2] <- 10")
  for (z in dp) {
    k <- which.min(abs(s[, 1] - z))
    tm[k, 2] <- 1
    code <- c(code, sprintf("template[which.min(abs(dat[, 1] - %s)), 2] <- 1", cd_fmt(z)))
  }
  list(template = tm, code = code)
}

# Posterior summary of a chain as a table row
cd_at_post_row <- function(name, v) {
  v <- v[is.finite(v)]
  q <- stats::quantile(v, c(0.025, 0.5, 0.975), names = FALSE)
  data.frame(parameter = name, mean = mean(v), sd = stats::sd(v), median = q[2], lower95 = q[1], upper95 = q[3])
}

# Earth-Moon distance and length of day from k (arcsec/yr) with alodfromk, as text
cd_at_alod_text <- function(k, sdk) {
  if (!is.finite(k) || !is.finite(sdk) || sdk <= 0) {
    return(character())
  }
  a <- cd_at_quiet(astrochron::alodfromk(k = k, sdevk = sdk, genplot = FALSE, verbose = FALSE))$value
  if (is.null(a) || length(a) < 4) {
    return(character())
  }
  sprintf("From k (alodfromk): Earth-Moon distance %s +/- %s Earth radii, length of day %s +/- %s hours.",
      cd_fmt(signif(a[1], 5)), cd_fmt(signif(a[2], 3)), cd_fmt(signif(a[3], 5)), cd_fmt(signif(a[4], 3)))
}

# Best sedimentation rate per location from a track table (location, sedrate, score); best = max or min score
cd_at_track_best <- function(tr, best = "max", tie = NULL) {
  loc <- unique(tr[, 1])
  sr <- numeric(length(loc))
  sc <- numeric(length(loc))
  for (i in seq_along(loc)) {
    k <- which(tr[, 1] == loc[i])
    v <- tr[k, 3]
    j <- if (best == "max") { k[v == max(v)] } else { k[v == min(v)] }
    if (length(j) > 1 && !is.null(tie)) {
      j <- j[which.min(tie[j])]
    }
    sr[i] <- stats::median(tr[j, 2])
    sc[i] <- tr[j[1], 3]
  }
  data.frame(location = loc, sedrate = sr, score = sc)
}

cd_at_tools_sedrate <- function() {
  M <- "Sed. rate"
  G <- "Astronomical testing (astrochron)"
  len_default <- function(frac) {
    function(d) {
      signif(diff(range(d[, 1], na.rm = TRUE)) * frac, 3)
    }
  }
  target_default <- "405.7, 130.7, 123.9, 98.9, 94.9, 41, 23.7, 22.4, 19"
  list(
    # TimeOpt significance ####
    cd_tool("at_timeopt_sim", M, "TimeOpt with Monte Carlo significance", group = G, heavy = TRUE, icon = "bullseye",
        info = paste("TimeOpt (Meyers 2015) over a grid of sedimentation rates, with the p-value of the best fit from",
            "simulated AR(1) or power-law noise series that are analysed in the same way, and the TimeOpt summary",
            "figure. Depth in metres."),
        uses = "astrochron::timeOpt, astrochron::timeOptSim, astrochron::timeOptSimPwrLaw, astrochron::timeOptPlot",
        params = c(list(
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 50, min = 10, step = 10),
          cd_sel("linLog", "Sed. rate grid", c("Logarithmic" = "1", "Linear" = "0")),
          cd_sel("fit", "Envelope fit", c("Precession amplitude modulation" = "1", "Short eccentricity modulation" = "2")),
          cd_sel("r2max", "Statistic tested", c("r2 opt (envelope x power)" = "1", "r2 spectral power" = "2",
              "r2 envelope" = "3")),
          cd_sel("noise", "Noise model of the simulations", c("AR(1) red noise (timeOptSim)" = "ar1",
              "Power law (timeOptSimPwrLaw)" = "pl")),
          cd_num("coeff", "rho or beta of the noise (0 = from the record)", 0, min = 0),
          cd_num("numsim", "Number of simulations", 100, min = 10, step = 100,
              help = "Meyers (2015) used 2000; with 100 the smallest p-value that can be reported is 0.1."),
          cd_num("ncores", "Processor cores", 2, min = 2, step = 1),
          cd_chk("fitModPwr", "Include the modulation periods in the spectral fit", TRUE),
          cd_chk("detrend", "Remove linear trend", TRUE)),
          cd_at_timeopt_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          st <- cd_at_timeopt_settings(p)
          common <- list(sedmin = p$sedmin, sedmax = p$sedmax, numsed = as.integer(p$numsed), linLog = as.integer(p$linLog),
              fit = as.integer(p$fit), fitModPwr = p$fitModPwr, flow = st$flow, fhigh = st$fhigh, roll = st$roll,
              targetE = st$targetE, targetP = if (p$fit == "1") { st$targetP } else { NULL }, detrend = p$detrend)
          r1 <- cd_at_call("timeOpt", c(list(dat = quote(dat)), common, list(r2max = as.integer(p$r2max), output = 1,
              genplot = FALSE, verbose = FALSE)), list(dat = s), var = "res1")
          r2 <- cd_at_call("timeOpt", c(list(dat = quote(dat)), common, list(r2max = as.integer(p$r2max), output = 2,
              genplot = FALSE, verbose = FALSE)), list(dat = s), var = "res2")
          t1 <- as.data.frame(r1$value)
          t2 <- as.data.frame(r2$value)
          col <- c(4, 3, 2)[as.integer(p$r2max)]
          obs <- max(t1[, col])
          best <- t1$sedrate[which.max(t1[, col])]
          fn <- if (p$noise == "ar1") { "timeOptSim" } else { "timeOptSimPwrLaw" }
          ca <- cd_at_pos(p$coeff)
          sa <- c(list(dat = quote(dat), numsim = as.integer(p$numsim)),
              if (p$noise == "ar1") { list(rho = ca) } else { list(beta = ca) },
              common, list(r2max = as.integer(p$r2max), ncores = as.integer(max(2, p$ncores)), output = 2,
              genplot = FALSE, verbose = TRUE))
          sm <- cd_at_call(fn, sa, list(dat = s), var = "sims")
          sims <- as.numeric(unlist(sm$value))
          pv <- cd_at_pval(sum(sims > obs), length(sims))
          coef_used <- if (p$noise == "ar1") { cd_at_grab(sm$log, "Estimated AR1 coefficient=") } else {
            cd_at_grab(sm$log, "beta") }
          o <- list(cd_output(cd_out_name(file, "-timeOpt"), t1, header = TRUE, var = "res1"),
              cd_output(cd_out_name(file, "-timeOptSim"), data.frame(sim_r2 = sims), header = TRUE, var = "sims"),
              cd_output(cd_out_name(file, "-timeOpt_tuned"), t2[, 1:2], var = "tuned"))
          code <- c(cd_even_code(d), r1$code, r2$code, sm$code,
              sprintf("p <- max(sum(sims[, 1] > max(res1[, %d])) / nrow(sims), 10 / nrow(sims))", col),
              "tuned <- res2[, 1:2]",
              sprintf(paste0("astrochron::timeOptPlot(dat, res1, res2, sims, fit = %s, fitModPwr = %s, flow = %s, fhigh = %s,\n",
                  "    roll = %s, targetE = %s, targetP = %s, fitR = max(res1[, %d]))"), p$fit, p$fitModPwr, cd_dep(st$flow),
                  cd_dep(st$fhigh), cd_dep(st$roll), cd_dep(st$targetE), cd_dep(st$targetP), col),
              cd_at_code_write(o))
          lab <- c("r2 opt", "r2 spectral power", "r2 envelope")[as.integer(p$r2max)]
          txt <- c(cd_even_note(d, s), sprintf("Optimal sedimentation rate %s cm/kyr, %s = %s; %s (%d %s simulations%s).",
              cd_fmt(signif(best, 4)), lab, cd_fmt(signif(obs, 4)), pv$text, length(sims),
              if (p$noise == "ar1") { "AR(1)" } else { "power-law" },
              if (is.finite(coef_used)) { sprintf(", %s = %s", if (p$noise == "ar1") { "rho" } else { "beta" },
                  cd_fmt(signif(coef_used, 4))) } else { "" }),
              sprintf("The record tuned at %s cm/kyr is written to %s.", cd_fmt(signif(best, 4)), o[[3]]$name))
          list(result = list(s = s, t1 = t1, t2 = t2, sims = data.frame(simres = sims), st = st, fit = p$fit,
              fitModPwr = p$fitModPwr, fitR = obs), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          st <- res$st
          cd_at_here(astrochron::timeOptPlot(res$s, res$t1, res$t2, res$sims, fit = as.integer(res$fit),
              fitModPwr = res$fitModPwr, flow = st$flow, fhigh = st$fhigh, roll = st$roll, targetE = st$targetE,
              targetP = st$targetP, xlab = "Depth (m)", ylab = "Value", fitR = res$fitR, verbose = FALSE))
        }),
    # TimeOpt with variable sedimentation rate ####
    cd_tool("at_timeopt_template", M, "TimeOpt with variable sed. rate (timeOptTemplate)", group = G, heavy = TRUE,
        icon = "chart-line",
        info = paste("TimeOpt for a sedimentation rate that changes along the record following a template: faster",
            "deposition of one lithology (the proxy itself as template), a linear change, steps at given depths or",
            "possible hiatuses. Gives the instantaneous sed. rate, an age model and optionally a Monte Carlo p-value.",
            "Slow: each average rate is a full optimisation. Depth in metres."),
        uses = "astrochron::timeOptTemplate, astrochron::timeOptTemplateSim, astrochron::timeOptTemplatePlot",
        params = c(list(
          cd_sel("template", "Sedimentation-rate template", c("The proxy itself (differential accumulation)" = "data",
              "Linear change along the record" = "linear", "Steps at given depths" = "step",
              "Possible hiatuses at given depths" = "hiatus")),
          cd_txt("depths", "Depths of the steps or hiatuses", "", numeric = TRUE,
              show_if = list(param = "template", values = c("step", "hiatus"))),
          cd_num("sedmin", "Minimum average sed. rate (cm/kyr)", 1, min = 0),
          cd_num("sedmax", "Maximum average sed. rate (cm/kyr)", 4, min = 0),
          cd_num("numsed", "Number of average sed. rates", 20, min = 3, step = 5),
          cd_sel("linLog", "Sed. rate grid", c("Logarithmic" = "1", "Linear" = "0")),
          cd_num("difmin", "Lowest instantaneous sed. rate (cm/kyr, 0 = default)", 0, min = 0),
          cd_num("difmax", "Highest instantaneous sed. rate (cm/kyr, 0 = factor below)", 0, min = 0),
          cd_num("fac", "Highest instantaneous rate as a factor of the average (0 = default 5)", 0, min = 0),
          cd_sel("iopt", "Optimise on", c("Modulations x spectral power" = "3", "Modulations" = "1", "Spectral power" = "2")),
          cd_sel("cormethod", "Correlation", c("Pearson" = "1", "Spearman" = "2")),
          cd_chk("detrendTemplate", "Remove the linear trend of the template", FALSE),
          cd_chk("flipTemplate", "Flip the template", FALSE),
          cd_chk("fitModPwr", "Include the modulation periods in the spectral fit", TRUE),
          cd_num("ncores", "Processor cores", 2, min = 1, step = 1),
          cd_chk("sim", "Monte Carlo significance (timeOptTemplateSim, slow)", FALSE),
          cd_num("numsim", "Number of simulations", 20, min = 2, step = 10, show_if = list(param = "sim", values = TRUE))),
          cd_at_timeopt_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          p$fit <- "1"
          st <- cd_at_timeopt_settings(p)
          tp <- cd_at_template(s, p$template, p$depths)
          cd_sr_check_range(p$sedmin, p$sedmax)
          dat_list <- list(dat = s)
          targ <- list(template = NULL)
          if (!is.null(tp$template)) {
            dat_list$template <- tp$template
            targ <- list(template = quote(template))
          }
          common <- c(targ, list(difmin = cd_at_pos(p$difmin), difmax = cd_at_pos(p$difmax), fac = cd_at_pos(p$fac),
              linLog = as.integer(p$linLog), fit = 1L, fitModPwr = p$fitModPwr, iopt = as.integer(p$iopt), flow = st$flow,
              fhigh = st$fhigh, roll = st$roll, targetE = st$targetE, targetP = st$targetP, cormethod = as.integer(p$cormethod),
              detrendTemplate = p$detrendTemplate, flipTemplate = p$flipTemplate))
          r1 <- cd_at_call("timeOptTemplate", c(list(dat = quote(dat)), common, list(sedmin = p$sedmin, sedmax = p$sedmax,
              numsed = as.integer(p$numsed), ncores = as.integer(p$ncores), output = 1, genplot = 0, verbose = 1)),
              dat_list, var = "res1")
          t1 <- as.data.frame(r1$value)
          best <- t1$sedrate[which.max(t1$r2)]
          # the instantaneous rates and the tuned series at the best average rate (a two-point grid around it)
          dat_list$best <- best
          fine <- list(sedmin = quote(best * 0.99999), sedmax = quote(best * 1.00001), numsed = 2L, ncores = 1L, genplot = 0,
              verbose = 1)
          r3 <- cd_at_call("timeOptTemplate", c(list(dat = quote(dat)), common, fine, list(output = 3)), dat_list, var = "sr")
          r2 <- cd_at_call("timeOptTemplate", c(list(dat = quote(dat)), common, fine, list(output = 2)), dat_list, var = "res2")
          srd <- as.data.frame(r3$value)
          t2 <- as.data.frame(r2$value)
          tm <- cd_sr_age(srd[, 1], srd[, 2], 100)
          o <- c(list(cd_output(cd_out_name(file, "-timeOptTemplate"), t1, header = TRUE, var = "res1"),
              cd_output(cd_out_name(file, "-timeOptTemplate_tuned"), t2[, 1:2], var = "tuned")),
              cd_sr_outputs(file, "timeOptTemplate", srd[, 1], srd[, 2], tm))
          code <- c(cd_even_code(d), tp$code, r1$code, "best <- res1$sedrate[which.max(res1$r2)]",
              "# instantaneous rates (output = 3) and tuned series (output = 2) at the best average rate",
              r3$code, r2$code,
              "tuned <- res2[, 1:2]", cd_sr_age_code(100))
          pages <- list()
          sims <- NULL
          txt <- c(cd_even_note(d, s), sprintf("Best average sedimentation rate %s cm/kyr (r2 = %s); instantaneous rate %s to %s cm/kyr.",
              cd_fmt(signif(best, 4)), cd_fmt(signif(max(t1$r2), 4)), cd_fmt(signif(min(srd[, 2]), 4)),
              cd_fmt(signif(max(srd[, 2]), 4))))
          if (isTRUE(p$sim)) {
            sm <- cd_at_call("timeOptTemplateSim", c(list(dat = quote(dat)), common, list(corVal = quote(max(res1$r2)),
                numsim = as.integer(p$numsim), sedmin = p$sedmin, sedmax = p$sedmax, numsed = as.integer(p$numsed),
                ncores = as.integer(p$ncores), output = 2, genplot = FALSE, verbose = FALSE)), c(dat_list, list(res1 = t1)),
                var = "sims")
            sims <- as.numeric(unlist(sm$value))
            pv <- cd_at_pval(sum(sims > max(t1$r2)), length(sims))
            txt <- c(txt, sprintf("Monte Carlo: %s (%d AR(1) simulations).", pv$text, length(sims)))
            o <- c(o, list(cd_output(cd_out_name(file, "-timeOptTemplateSim"), data.frame(sim_r2 = sims), header = TRUE,
                var = "sims")))
            code <- c(code, sm$code)
            pl <- cd_at_call("timeOptTemplatePlot", c(list(dat = quote(dat)), targ, list(detrendTemplate = p$detrendTemplate,
                flipTemplate = p$flipTemplate, srMin = quote(min(sr[, 2])), srMax = quote(max(sr[, 2])), res1 = quote(res1),
                simres = quote(sims), fit = 1L, flow = st$flow, fhigh = st$fhigh, roll = st$roll, targetE = st$targetE,
                targetP = st$targetP, xlab = "Depth (m)", ylab = "Value", fitR = quote(max(res1$r2)), verbose = FALSE)),
                c(dat_list, list(res1 = t1, sims = data.frame(simres = sims), sr = srd)), mode = "pages", var = "plt")
            pages <- pl$pages
            code <- c(code, sub("^plt <- ", "", pl$code))
          }
          code <- c(code, cd_at_code_write(o))
          txt <- c(txt, cd_sr_text(o[-(1:2)], srd[, 2], tm))
          list(result = list(t1 = t1, t2 = t2, srd = srd, best = best, pages = pages), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          if (length(res$pages)) {
            cd_at_replay(res$pages, 1)
            return(invisible(NULL))
          }
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:3, ncol = 1))
          cd_par()
          t1 <- res$t1
          graphics::plot(t1$sedrate, t1$r2, type = "o", pch = 16, cex = 0.7, col = "#1f4e79", log = "x",
              xlab = "Average sed. rate (cm/kyr)", ylab = "r2")
          graphics::abline(v = res$best, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          graphics::plot(res$srd[, 1], res$srd[, 2], type = "l", lwd = 1.5, col = "#2e8b57", xlab = "Depth (m)",
              ylab = "Sed. rate (cm/kyr)")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          t2 <- res$t2
          vw <- cd_view_lims(range(t2[, 1]), range(t2[, 3:5], na.rm = TRUE), last = TRUE)
          graphics::plot(t2[, 1], t2[, 3], type = "l", col = "grey60", xlab = "Time (kyr)", ylab = "Precession band",
              xlim = vw$x, ylim = vw$y)
          graphics::lines(t2[, 1], t2[, 4], col = "#1f4e79", lwd = 1.5)
          graphics::lines(t2[, 1], t2[, 5], col = "#c0392b", lwd = 1.5)
          graphics::legend("topright", c("filtered", "envelope", "eccentricity model"), col = c("grey60", "#1f4e79", "#c0392b"),
              lwd = 1.5, bty = "n", cex = 0.8)
        }),
    # Bayesian TimeOpt on a grid ####
    cd_tool("at_timeoptb", M, "Bayesian TimeOpt (timeOptB)", group = G, heavy = TRUE, icon = "chart-area",
        info = paste("Bayesian TimeOpt of Malinverno and Meyers (2024): posterior probability of the sedimentation rate",
            "and the precession frequency k on a grid, with age-specific priors, the best astronomical fit and optionally",
            "the p-values of AR(2) noise simulations. k also gives the Earth-Moon distance and day length. Depth in metres."),
        uses = "astrochron::timeOptB, astrochron::timeOptBSim, astrochron::alodfromk",
        params = list(
          cd_num("age", "Age of the record (Ma)", 100, min = 0.001),
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 80, min = 10, step = 10),
          cd_num("kmin", "Lowest k (arcsec/yr, 0 = from the prior)", 0, min = 0),
          cd_num("kmax", "Highest k (arcsec/yr, 0 = from the prior)", 0, min = 0),
          cd_num("numk", "Number of k values", 60, min = 10, step = 10),
          cd_chk("env", "Include the envelope fit", TRUE),
          cd_sel("mvopt", "Obliquity terms", c("s1 to s6" = "0", "s3 and s4" = "1", "None" = "2")),
          cd_num("ftol", "Tolerance of the precession band (cycles/kyr)", 0.005, min = 0),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_chk("sim", "Significance with 1000 AR(2) simulations (timeOptBSim)", FALSE),
          cd_sel("show", "Figure", c("Likelihood maps" = "1", "Posterior" = "2", "Best fit to the data" = "3",
              "AR(2) noise fit" = "4", "Simulations (timeOptBSim)" = "5"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          cd_sr_check_range(p$sedmin, p$sedmax)
          kmin <- cd_at_pos(p$kmin)
          kmax <- cd_at_pos(p$kmax)
          if (is.null(kmin) || is.null(kmax)) {
            kmin <- NULL
            kmax <- NULL
          }
          r <- cd_at_call("timeOptB", list(dat = quote(dat), age = p$age, env = p$env, mvopt = as.integer(p$mvopt),
              sedmin = p$sedmin, sedmax = p$sedmax, numsed = as.integer(p$numsed), kmin = kmin, kmax = kmax,
              numk = as.integer(p$numk), ftol = p$ftol, detrend = p$detrend, genplot = TRUE, verbose = TRUE),
              list(dat = s), mode = "pages")
          log <- r$log
          u <- cd_at_line_nums(log, "u:", after = "Marginal posterior PDF")
          k <- cd_at_line_nums(log, "k:", after = "Marginal posterior PDF")
          if (length(u) < 5 || length(k) < 5) {
            stop("timeOptB gave no posterior summary", call. = FALSE)
          }
          tab <- data.frame(parameter = c("sedrate_cm_kyr", "k_arcsec_yr"), MAP = c(u[1], k[1]), mean = c(u[2], k[2]),
              sd = c(u[3], k[3]), lower95 = c(u[4], k[4]), upper95 = c(u[5], k[5]))
          r2 <- c(cd_at_grab(log, "all astronomical cycles:"), cd_at_grab(log, "eccentricity only:"),
              cd_at_grab(log, "obliquity only:"), cd_at_grab(log, "climatic precession only:"))
          fit <- data.frame(cycles = c("all", "eccentricity", "obliquity", "precession"), r2_MAP = r2)
          pages <- r$pages
          code <- c(cd_even_code(d), r$code)
          txt <- c(cd_even_note(d, s), sprintf("Sedimentation rate (MAP) %s cm/kyr, 95%% interval %s - %s.", cd_fmt(u[1]),
              cd_fmt(u[4]), cd_fmt(u[5])), sprintf("Precession frequency k (MAP) %s arcsec/yr, 95%% interval %s - %s.",
              cd_fmt(k[1]), cd_fmt(k[4]), cd_fmt(k[5])),
              sprintf("r2 of the MAP fit: all cycles %s, eccentricity %s, obliquity %s, precession %s.", cd_fmt(r2[1]),
                  cd_fmt(r2[2]), cd_fmt(r2[3]), cd_fmt(r2[4])), cd_at_alod_text(k[2], k[3]))
          if (isTRUE(p$sim)) {
            sm <- cd_at_call("timeOptBSim", list(res = quote(res), Nsim = 1000L, genplot = TRUE, verbose = TRUE),
                list(res = r$value), mode = "pages", var = "sim")
            pv <- c(cd_at_grab(sm$log, "All astronomical cycles, p ="), cd_at_grab(sm$log, "Eccentricity only, p ="),
                cd_at_grab(sm$log, "Obliquity only, p ="), cd_at_grab(sm$log, "Climatic precession only, p ="))
            fit$p_value <- pv
            pages <- c(pages, sm$pages)
            code <- c(code, sm$code)
            txt <- c(txt, sprintf("p-values (1000 AR(2) simulations): all cycles %s, eccentricity %s, obliquity %s, precession %s.",
                cd_fmt(pv[1]), cd_fmt(pv[2]), cd_fmt(pv[3]), cd_fmt(pv[4])),
                "Use the p-values of single families only when the p-value of all cycles is 0.1 or lower.")
          }
          o <- list(cd_output(cd_out_name(file, "-timeOptB"), tab, header = TRUE, var = "post"),
              cd_output(cd_out_name(file, "-timeOptB_fit"), fit, header = TRUE, var = "fit"))
          code <- c(code, sprintf("astrochron::alodfromk(k = %s, sdevk = %s)", cd_fmt(k[2]), cd_fmt(k[3])),
              "# the posterior summary and r2 values are printed by timeOptB (and the p-values by timeOptBSim)")
          list(result = list(pages = pages, tab = tab, fit = fit), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          k <- as.integer(p$show)
          if (k > length(res$pages)) {
            cd_message_plot("Run with the simulations (timeOptBSim) to see this figure")
            return(invisible(NULL))
          }
          cd_at_replay(res$pages, k)
        }),
    # Bayesian TimeOpt by MCMC ####
    cd_tool("at_timeopt_mcmc", M, "Bayesian TimeOpt by MCMC (timeOptBMCMC / timeOptMCMC)", group = G, heavy = TRUE,
        icon = "route",
        info = paste("Markov chain Monte Carlo versions of Bayesian TimeOpt: posterior distributions of the sedimentation",
            "rate, the precession frequency k and the Solar System frequencies g (and s), hence of all astronomical",
            "periods and grand cycles. Slow: astrochron recommends 50 000 samples for timeOptBMCMC. Depth in metres."),
        uses = "astrochron::timeOptBMCMC, astrochron::timeOptBMCMCplot, astrochron::timeOptMCMC, astrochron::hoangPriors, astrochron::alodfromk",
        params = list(
          cd_sel("method", "Method", c("timeOptBMCMC (Malinverno and Meyers 2024)" = "bmcmc",
              "timeOptMCMC (Meyers and Malinverno 2018)" = "mcmc")),
          cd_num("age", "Age of the record (Ma)", 100, min = 0.001,
              help = "Sets the priors of k and of the g (and s) frequencies (Hoang et al. 2021)."),
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("nsamples", "Number of MCMC samples", 1000, min = 100, step = 1000),
          cd_chk("env", "Include the envelope fit", TRUE, show_if = list(param = "method", values = "bmcmc")),
          cd_sel("mvopt", "Obliquity terms", c("s1 to s6" = "0", "s3 and s4" = "1", "None" = "2"),
              show_if = list(param = "method", values = "bmcmc")),
          cd_sel("iopt", "Fit", c("Power and envelope" = "1", "Power only" = "2"), show_if = list(param = "method", values = "mcmc")),
          cd_num("kAve", "Prior mean of k (arcsec/yr, 0 = AstroGeo22 at the age)", 0, min = 0,
              show_if = list(param = "method", values = "mcmc")),
          cd_num("kSd", "Prior standard deviation of k (arcsec/yr, 0 = AstroGeo22 range)", 0, min = 0,
              show_if = list(param = "method", values = "mcmc")),
          cd_num("ftol", "Tolerance of the precession band (cycles/kyr)", 0.005, min = 0),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_num("seed", "Random seed (0 = random)", 0, step = 1),
          cd_num("page", "Figure number", 1, min = 1, step = 1,
              help = paste("timeOptBMCMC: 1 MCMC diagnostics, 2 posterior and prior PDFs, 3 Milankovitch periods,",
                  "4-5 grand cycles, 6 correlations. timeOptMCMC: 1 stability, 2-3 candidates along the chain,",
                  "4 kernel densities, 5 histograms."))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          cd_sr_check_range(p$sedmin, p$sedmax)
          code <- cd_even_code(d)
          if (is.finite(p$seed) && p$seed != 0) {
            set.seed(as.integer(p$seed))
            code <- c(code, sprintf("set.seed(%d)", as.integer(p$seed)))
          }
          txt <- cd_even_note(d, s)
          if (p$method == "bmcmc") {
            r <- cd_at_call("timeOptBMCMC", list(dat = quote(dat), age = p$age, env = p$env, mvopt = as.integer(p$mvopt),
                sedmin = p$sedmin, sedmax = p$sedmax, ftol = p$ftol, nsamples = as.integer(p$nsamples), detrend = p$detrend,
                genplot = FALSE, verbose = TRUE), list(dat = s))
            v <- r$value
            pl <- cd_at_pages(astrochron::timeOptBMCMCplot(v$dat, v$mcmcres, v$pdfpara))
            code <- c(code, r$code, "periods <- astrochron::timeOptBMCMCplot(res$dat, res$mcmcres, res$pdfpara)",
                "samples <- res$mcmcres$mv")
            smp <- as.data.frame(v$mcmcres$mv)
            per <- as.data.frame(pl$value, check.names = FALSE)
            lines <- pl$log[grepl("^(u|k|g4-g3|s4-s3):", trimws(pl$log))]
            txt <- c(txt, "Marginal posterior (MAP, mean, sd, 95% interval):", paste0("  ", gsub("\t", " ", trimws(lines))))
            o <- list(cd_output(cd_out_name(file, "-timeOptBMCMC_samples"), smp, header = TRUE, var = "samples"),
                cd_output(cd_out_name(file, "-timeOptBMCMC_periods"), per, header = TRUE, var = "periods"))
            ks <- smp$k[ceiling(nrow(smp) / 2):nrow(smp)]
            pages <- pl$pages
          } else {
            gA <- numeric(5)
            gS <- numeric(5)
            for (i in 1:5) {
              h <- cd_at_hoang_moments("g", i, p$age / 1000)
              gA[i] <- signif(h$mean, 8)
              gS[i] <- signif(h$sd, 6)
            }
            ka <- cd_at_k_age(p$age)
            kA <- if (is.null(cd_at_pos(p$kAve))) { signif(ka$k, 6) } else { p$kAve }
            kS <- if (is.null(cd_at_pos(p$kSd))) { signif(ka$sd, 4) } else { p$kSd }
            r <- cd_at_call("timeOptMCMC", list(dat = quote(dat), iopt = as.integer(p$iopt), sedmin = p$sedmin,
                sedmax = p$sedmax, gAve = gA, gSd = gS, kAve = kA, kSd = kS, ftol = p$ftol, nsamples = as.integer(p$nsamples),
                detrend = p$detrend, output = 1, genplot = 1, verbose = TRUE), list(dat = s), mode = "pages")
            code <- c(code, sprintf("# priors: g from astrochron::hoangPriors at %s Ga, k from WaverideR::astrogeo22(%s)",
                cd_fmt(p$age / 1000), cd_fmt(p$age)), r$code, "samples <- res")
            smp <- as.data.frame(r$value, check.names = FALSE)
            half <- smp[ceiling(nrow(smp) / 2):nrow(smp), , drop = FALSE]
            post <- rbind(cd_at_post_row("sedrate_cm_kyr", half$sedrate), cd_at_post_row("k_arcsec_yr", half$k))
            for (i in 1:5) {
              post <- rbind(post, cd_at_post_row(paste0("g", i), half[[paste0("g", i)]]))
            }
            txt <- c(txt, sprintf("Priors: k = %s +/- %s arcsec/yr; g1-g5 from Hoang et al. (2021) at %s Ga.", cd_fmt(kA),
                cd_fmt(kS), cd_fmt(p$age / 1000)), "Posterior from the second half of the chain:",
                cd_at_table_text(post))
            o <- list(cd_output(cd_out_name(file, "-timeOptMCMC_samples"), smp, header = TRUE, var = "samples"))
            ks <- half$k
            pages <- r$pages
          }
          txt <- c(txt, cd_at_alod_text(mean(ks, na.rm = TRUE), stats::sd(ks, na.rm = TRUE)))
          list(result = list(pages = pages), outputs = o, code = c(code, cd_at_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          cd_at_replay(res$pages, p$page)
        }),
    # eTimeOpt ridge tracking ####
    cd_tool("at_etimeopt_track", M, "eTimeOpt with ridge tracking (eTimeOptTrack)", group = G, heavy = TRUE,
        icon = "crosshairs",
        info = paste("Evolutive TimeOpt in sliding windows, then the r2 maxima in every window (eTimeOptTrack) and the",
            "best sedimentation rate along the record as a sed.-rate curve and age model. Depth in metres."),
        uses = "astrochron::eTimeOpt, astrochron::eTimeOptTrack",
        params = list(
          cd_num("win", "Window length (m)", len_default(0.3), min = 0),
          cd_num("step", "Step (m)", len_default(0.025), min = 0),
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 50, min = 10, step = 10),
          cd_sel("linLog", "Sed. rate grid", c("Logarithmic" = "1", "Linear" = "0")),
          cd_sel("fit", "Envelope fit", c("Precession amplitude modulation" = "1", "Short eccentricity modulation" = "2")),
          cd_sel("stat", "Statistic tracked", c("r2 opt" = "3", "r2 envelope" = "1", "r2 spectral power" = "2")),
          cd_num("threshold", "Smallest r2 of a tracked maximum", 0, min = 0, max = 1, step = 0.05),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          cd_sr_check_range(p$sedmin, p$sedmax)
          r <- tryCatch(cd_at_call("eTimeOpt", list(dat = quote(dat), win = p$win, step = p$step, sedmin = p$sedmin,
              sedmax = p$sedmax, numsed = as.integer(p$numsed), linLog = as.integer(p$linLog), fit = as.integer(p$fit),
              output = 1, genplot = FALSE, verbose = 0), list(dat = s)),
              error = function(e) {
                stop(sprintf(paste("eTimeOpt could not test these sedimentation rates (%s): use a longer window or a",
                    "narrower range of sedimentation rates."), conditionMessage(e)), call. = FALSE)
              })
          k <- as.integer(p$stat)
          map <- as.data.frame(r$value[[k]])
          tr <- cd_at_call("eTimeOptTrack", list(res = quote(map), threshold = p$threshold, genplot = FALSE,
              verbose = FALSE), list(map = map), var = "track")
          trk <- as.data.frame(tr$value)
          names(trk) <- c("location", "sedrate", "r2")
          if (!nrow(trk)) {
            stop("no r2 maximum above the threshold", call. = FALSE)
          }
          bst <- cd_at_track_best(trk, "max")
          tm <- cd_sr_age(bst$location, bst$sedrate, 100)
          o <- c(list(cd_output(cd_out_name(file, "-eTimeOptTrack"), trk, header = TRUE, var = "track")),
              cd_sr_outputs(file, "eTimeOpt", bst$location, bst$sedrate, tm))
          code <- c(cd_even_code(d), r$code, sprintf("map <- res[[%d]]", k), tr$code,
              "names(track) <- c(\"location\", \"sedrate\", \"r2\")",
              "best <- track[order(track$location, -track$r2), ]",
              "best <- best[!duplicated(best$location), ]",
              "sr <- data.frame(depth = best$location, sedrate_cm_kyr = best$sedrate)", cd_sr_age_code(100),
              cd_at_code_write(o[1]), cd_code_write(o[-1]))
          x <- as.numeric(sub("^X", "", names(map)[-1]))
          list(result = list(x = x, sed = map[, 1], z = t(as.matrix(map[, -1])), trk = trk, best = bst),
              outputs = o, code = code, text = c(cd_even_note(d, s), sprintf("%d r2 maxima tracked in %d windows.",
                  nrow(trk), length(unique(trk$location))), cd_sr_text(o[-1], bst$sedrate, tm)))
        },
        plot = function(res, p) {
          cv <- list(list(x = res$trk$location, y = res$trk$sedrate, col = "grey85", type = "p", label = "r2 maxima"),
              list(x = res$best$location, y = res$best$sedrate, col = "#ff6b4a", lwd = 2, label = "highest maximum"))
          cd_sr_plot_map(res$x, res$sed, res$z, curves = cv, xlab = "Depth (m)", zlab = "r2", palette = p$palette,
              logy = !cd_regular(res$sed))
        }),
    # Average spectral misfit ####
    cd_tool("at_asm", M, "Average spectral misfit (ASM / eASM)", group = G, heavy = function(p) { identical(p$method, "easm") },
        icon = "magnifying-glass-chart",
        info = paste("Average spectral misfit (Meyers and Sageman 2007; Meyers et al. 2012): compares the frequencies of",
            "significant spectral peaks with astronomical target frequencies over a range of sedimentation rates and",
            "gives the null-hypothesis significance level (Ho-SL). Evolutive: in sliding EHA windows, with the Ho-SL",
            "minima tracked along the record. Depth in metres."),
        uses = "astrochron::asm, astrochron::eAsm, astrochron::eAsmTrack, astrochron::mtm, astrochron::eha",
        params = list(
          cd_sel("method", "Method", c("Whole record (asm)" = "asm", "Evolutive, sliding windows (eAsm, eAsmTrack)" = "easm")),
          cd_txt("target", "Astronomical target periods (kyr)", target_default, numeric = TRUE),
          cd_num("fper", "Relative uncertainty of the targets (0-1, 0 = none)", 0, min = 0, max = 1),
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 1, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 50, min = 10, max = 500, step = 10),
          cd_sel("linLog", "Sed. rate grid", c("Logarithmic" = "1", "Linear" = "0")),
          cd_num("iter", "Monte Carlo simulations", 10000, min = 1000, max = 100000, step = 10000),
          cd_num("tbw", "Time-bandwidth product of the spectra (NW)", 2, min = 1, max = 10, step = 0.5),
          cd_num("siglevel", "Harmonic F-test level of the peaks (0-1)", 0.9, min = 0.5, max = 0.999),
          cd_txt("freqs", "Own peak frequencies (cycles/m, optional)", "", numeric = TRUE,
              show_if = list(param = "method", values = "asm")),
          cd_num("win", "Window length (m)", len_default(0.25), min = 0, show_if = list(param = "method", values = "easm")),
          cd_num("step", "Step (m)", len_default(0.025), min = 0, show_if = list(param = "method", values = "easm")),
          cd_num("threshold", "Largest Ho-SL of a tracked minimum (%)", 0.5, min = 0, show_if = list(param = "method", values = "easm")),
          cd_sel("palette", "Colour palette", cd_palettes(), show_if = list(param = "method", values = "easm"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dt <- cd_dt(s$x)
          cd_sr_check_range(p$sedmin, p$sedmax)
          tg <- cd_at_vec(p$target)
          if (is.null(tg)) {
            stop("give the astronomical target periods", call. = FALSE)
          }
          target <- signif(sort(1 / tg), 8)
          fper <- if (is.null(cd_at_pos(p$fper))) { NULL } else { rep(p$fper, length(target)) }
          nyq <- signif(1 / (2 * dt), 8)
          code <- c(cd_even_code(d), sprintf("target <- sort(1 / %s)", cd_dep(tg)))
          if (p$method == "asm") {
            fr <- cd_at_vec(p$freqs)
            if (is.null(fr)) {
              pk <- cd_at_call("mtm", list(dat = quote(dat), tbw = p$tbw, padfac = 5, siglevel = p$siglevel, output = 3,
                  genplot = FALSE, verbose = FALSE), list(dat = s), var = "peaks")
              fr <- as.data.frame(pk$value)[, 1]
              code <- c(code, pk$code, "freq <- peaks[, 1]")
            } else {
              code <- c(code, sprintf("freq <- %s", cd_dep(fr)))
            }
            if (!length(fr)) {
              stop("no spectral peaks above the harmonic F-test level: lower the level", call. = FALSE)
            }
            ray <- signif(1 / (nrow(s) * dt), 8)
            r <- cd_at_call("asm", list(freq = quote(freq), target = quote(target), fper = fper, rayleigh = ray,
                nyquist = nyq, sedmin = p$sedmin, sedmax = p$sedmax, numsed = as.integer(p$numsed),
                linLog = as.integer(p$linLog), iter = as.integer(p$iter), output = TRUE, genplot = FALSE),
                list(freq = fr, target = target))
            tab <- as.data.frame(r$value)
            names(tab) <- c("sedrate", "ASM", "HoSL_percent", "n_terms")
            k <- which(tab$HoSL_percent == min(tab$HoSL_percent))
            k <- k[which.min(tab$ASM[k])]
            o <- list(cd_output(cd_out_name(file, "-ASM"), tab, header = TRUE, var = "res"))
            code <- c(code, r$code, "names(res) <- c(\"sedrate\", \"ASM\", \"HoSL_percent\", \"n_terms\")", cd_at_code_write(o))
            txt <- c(cd_even_note(d, s), sprintf("%d spectral peaks tested against %d target frequencies.", length(fr),
                length(target)), sprintf("Lowest Ho-SL %s%% at %s cm/kyr (ASM %s cycles/kyr, %d terms fitted).",
                cd_fmt(tab$HoSL_percent[k]), cd_fmt(signif(tab$sedrate[k], 4)), format(signif(tab$ASM[k], 3)),
                as.integer(tab$n_terms[k])), "Meyers and Sageman (2007) take Ho-SL below 0.5% as significant.")
            return(list(result = list(method = "asm", tab = tab, best = tab[k, ]), outputs = o, code = code, text = txt))
          }
          winpts <- as.integer(floor(p$win / dt) + 1)
          pad <- as.integer(max(2 * 2^ceiling(log2(winpts)), 512))
          e <- cd_at_call("eha", list(dat = quote(dat), tbw = p$tbw, pad = pad, win = p$win, step = p$step, output = 4,
              genplot = FALSE, verbose = FALSE), list(dat = s), var = "spec")
          ray <- signif(1 / p$win, 8)
          r <- cd_at_call("eAsm", list(spec = quote(spec), siglevel = p$siglevel, target = quote(target), fper = fper,
              rayleigh = ray, nyquist = nyq, sedmin = p$sedmin, sedmax = p$sedmax, numsed = as.integer(p$numsed),
              linLog = as.integer(p$linLog), iter = as.integer(p$iter), output = 4, genplot = FALSE),
              list(spec = as.data.frame(e$value), target = target))
          hosl <- as.data.frame(r$value[[1]])
          asmm <- as.data.frame(r$value[[2]])
          tr <- cd_at_call("eAsmTrack", list(res = quote(hosl), threshold = p$threshold, genplot = FALSE, verbose = FALSE),
              list(hosl = hosl), var = "track")
          trk <- as.data.frame(tr$value)
          names(trk) <- c("location", "sedrate", "HoSL_percent")
          if (!nrow(trk)) {
            stop("no Ho-SL minimum below the threshold: raise the threshold", call. = FALSE)
          }
          # ties of the lowest Ho-SL: the rate with the lowest ASM
          tie <- numeric(nrow(trk))
          for (i in seq_len(nrow(trk))) {
            col <- which.min(abs(as.numeric(sub("^X", "", names(asmm)[-1])) - trk$location[i])) + 1
            tie[i] <- asmm[which.min(abs(asmm[, 1] - trk$sedrate[i])), col]
          }
          bst <- cd_at_track_best(trk, "min", tie = tie)
          tm <- cd_sr_age(bst$location, bst$sedrate, 100)
          o <- c(list(cd_output(cd_out_name(file, "-eAsmTrack"), trk, header = TRUE, var = "track")),
              cd_sr_outputs(file, "eASM", bst$location, bst$sedrate, tm))
          code <- c(code, e$code, r$code, "hosl <- res[[1]]", tr$code, "names(track) <- c(\"location\", \"sedrate\", \"HoSL_percent\")",
              "best <- track[order(track$location, track$HoSL_percent), ]", "best <- best[!duplicated(best$location), ]",
              "sr <- data.frame(depth = best$location, sedrate_cm_kyr = best$sedrate)", cd_sr_age_code(100),
              cd_at_code_write(o[1]), cd_code_write(o[-1]))
          x <- as.numeric(sub("^X", "", names(hosl)[-1]))
          list(result = list(method = "easm", x = x, sed = hosl[, 1], z = t(as.matrix(hosl[, -1])), trk = trk, best = bst),
              outputs = o, code = code, text = c(cd_even_note(d, s), sprintf("%d Ho-SL minima below %s%% in %d windows.",
                  nrow(trk), cd_fmt(p$threshold), length(unique(trk$location))), cd_sr_text(o[-1], bst$sedrate, tm)))
        },
        plot = function(res, p) {
          if (res$method == "easm") {
            cv <- list(list(x = res$trk$location, y = res$trk$sedrate, col = "grey85", type = "p", label = "Ho-SL minima"),
                list(x = res$best$location, y = res$best$sedrate, col = "#ff6b4a", lwd = 2, label = "selected rate"))
            z <- -log10(pmax(res$z, 1e-3))
            cd_sr_plot_map(res$x, res$sed, z, curves = cv, xlab = "Depth (m)", zlab = "-log10 Ho-SL (%)",
                palette = p$palette, logy = !cd_regular(res$sed))
            return(invisible(NULL))
          }
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          tb <- res$tab
          cd_par()
          graphics::plot(tb$sedrate, tb$ASM, type = "l", col = "#1f4e79", lwd = 1.5, xlab = "Sed. rate (cm/kyr)",
              ylab = "ASM (cycles/kyr)")
          graphics::abline(v = res$best$sedrate, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          vw <- cd_view_lims(range(tb$sedrate), range(pmax(tb$HoSL_percent, 1e-3)), last = TRUE)
          graphics::plot(tb$sedrate, pmax(tb$HoSL_percent, 1e-3), type = "l", col = "#2e8b57", lwd = 1.5, log = "y",
              xlab = "Sed. rate (cm/kyr)", ylab = "Ho-SL (%)", xlim = vw$x, ylim = vw$y)
          graphics::abline(h = 0.5, lty = 3, col = "grey30")
          graphics::abline(v = res$best$sedrate, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
        }),
    # Amplitude-modulation tests of a tuned record ####
    cd_tool("at_am_test", M, "Amplitude-modulation test of a tuned record (testPrecession / testTilt)", group = G,
        heavy = TRUE, icon = "wave-square",
        info = paste("Tests an astronomically tuned record (time in kyr, 0-50 Ma): are the precession (or obliquity)",
            "amplitude modulations those of the La2004 solution? Significance from phase-randomised or AR(1) surrogates,",
            "with filters and added noise that guard against modulations created by tuning (Zeeden et al. 2015, 2019)."),
        uses = "astrochron::testPrecession, astrochron::testTilt, CycloDesk::cd_laskar",
        params = list(
          cd_sel("method", "Test", c("Precession amplitude modulation (testPrecession)" = "prec",
              "Obliquity amplitude modulation (testTilt)" = "tilt")),
          cd_sel("unit", "Time unit of the data", c("kyr" = "1", "Myr" = "1000")),
          cd_num("nsim", "Number of simulations", 500, min = 10, step = 100),
          cd_sel("gen", "Surrogates", c("Phase-randomised" = "1", "AR(1)" = "2")),
          cd_num("edge", "Part of each end excluded (0-1)", 0.025, min = 0, max = 0.4, step = 0.005),
          cd_num("maxNoise", "Largest noise added (standard deviations of the data)", 1, min = 0,
              show_if = list(param = "method", values = "prec")),
          cd_num("maxNoiseT", "Largest noise added (standard deviations of the data)", 0.25, min = 0,
              show_if = list(param = "method", values = "tilt")),
          cd_num("cutoff", "Low-pass cut-off of the envelope (cycles/kyr)", signif(1 / 150, 4), min = 0,
              show_if = list(param = "method", values = "tilt")),
          cd_chk("detrendEnv", "Detrend the envelope", TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          fac <- as.numeric(p$unit)
          code <- cd_even_code(d)
          if (fac != 1) {
            s$x <- s$x * fac
            code <- c(code, sprintf("dat[, 1] <- dat[, 1] * %s", cd_fmt(fac)))
          }
          if (min(s$x) < 0 || max(s$x) > 50000) {
            stop("the time scale must lie between 0 and 50 000 kyr (positive ages)", call. = FALSE)
          }
          sol <- cd_laskar("la04")
          fn <- if (p$method == "prec") { "testPrecession" } else { "testTilt" }
          a <- list(dat = quote(dat), gen = as.integer(p$gen), edge = p$edge,
              maxNoise = if (p$method == "prec") { p$maxNoise } else { p$maxNoiseT })
          if (p$method == "tilt") {
            a$cutoff <- p$cutoff
          }
          a <- c(a, list(detrendEnv = p$detrendEnv, solution = quote(sol)))
          r0 <- cd_at_call(fn, c(a, list(nsim = 0L, output = TRUE, genplot = FALSE, verbose = TRUE)), list(dat = s, sol = sol),
              var = "series")
          r <- cd_at_call(fn, c(a, list(nsim = as.integer(p$nsim), output = TRUE, genplot = FALSE, verbose = TRUE)),
              list(dat = s, sol = sol), var = "sims")
          sims <- as.numeric(unlist(r$value))
          what <- if (p$method == "prec") { "observed precession AM, r=" } else { "observed obliquity AM, r=" }
          rdat <- cd_at_grab(r$log, what)
          pv <- cd_at_pval(sum(abs(sims) > abs(rdat)), length(sims))
          ser <- as.data.frame(r0$value, check.names = FALSE)
          o <- list(cd_output(cd_out_name(file, paste0("-", fn)), ser, header = TRUE, var = "series"),
              cd_output(cd_out_name(file, paste0("-", fn, "_sims")), data.frame(sim_r = sims), header = TRUE, var = "sims"))
          code <- c(code, "sol <- CycloDesk::cd_laskar(\"la04\")", r0$code, r$code, cd_at_code_write(o))
          txt <- c(sprintf("Correlation of the observed and theoretical %s amplitude modulation r = %s; %s (%d simulations).",
              if (p$method == "prec") { "precession" } else { "obliquity" }, cd_fmt(signif(rdat, 4)), pv$text, length(sims)),
              "Report at most p < 10 / number of simulations.")
          list(result = list(ser = ser, sims = sims, r = rdat, method = p$method), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:3, ncol = 1))
          sr <- res$ser
          nm <- if (res$method == "prec") { "Precession" } else { "Obliquity" }
          cd_par()
          graphics::plot(sr[, 1], sr[, 2], type = "l", col = "grey55", xlab = "Time (kyr)", ylab = paste(nm, "band"))
          graphics::lines(sr[, 1], sr[, 3], col = "#1f4e79", lwd = 1.3)
          cd_par()
          z1 <- (sr[, 4] - mean(sr[, 4], na.rm = TRUE)) / stats::sd(sr[, 4], na.rm = TRUE)
          z2 <- (sr[, 5] - mean(sr[, 5], na.rm = TRUE)) / stats::sd(sr[, 5], na.rm = TRUE)
          graphics::plot(sr[, 1], z1, type = "l", col = "#1f4e79", lwd = 1.5, ylim = range(c(z1, z2), na.rm = TRUE),
              xlab = "Time (kyr)", ylab = "Standardised", main = sprintf("Amplitude modulation, r = %s", cd_fmt(signif(res$r, 3))))
          graphics::lines(sr[, 1], z2, col = "#c0392b", lwd = 1.5)
          graphics::legend("topright", c("observed (low-passed envelope)", "theoretical (La2004)"), col = c("#1f4e79", "#c0392b"),
              lwd = 1.5, bty = "n", cex = 0.8)
          cd_par()
          cd_at_hist(res$sims, res$r, xlab = "Correlation of the surrogates", two_sided = TRUE)
        }),
    # Tracked frequency to sedimentation rate ####
    cd_tool("at_freq2sedrate", M, "Tracked cycle to sed. rate (freq2sedrate)", group = G, icon = "gauge",
        info = paste("Converts a tracked cycle (depth and its period or frequency, e.g. from EHA or a tracked scalogram",
            "ridge) into a sedimentation-rate curve and age model, given the duration of the cycle."),
        uses = "astrochron::freq2sedrate",
        params = list(
          cd_sel("col", "Column 2 of the data", c("Period (depth units)" = "period", "Frequency (cycles/depth unit)" = "freq")),
          cd_num("cycle", "Duration of the cycle (kyr)", 21, min = 0,
              help = "e.g. 405 (long eccentricity), 100, 41 (obliquity) or about 20 (precession)."),
          cd_sr_unit_param()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          u <- as.numeric(p$unit)
          if (any(s$y <= 0)) {
            stop("the periods or frequencies must be positive", call. = FALSE)
          }
          f <- if (p$col == "period") { 1 / s$y } else { s$y }
          fm <- f * 100 / u
          r <- cd_at_call("freq2sedrate", list(freqs = quote(freqs), period = p$cycle, genplot = FALSE, verbose = FALSE),
              list(freqs = data.frame(depth = s$x, freq = fm)), var = "sr")
          sr <- as.data.frame(r$value)
          tm <- cd_sr_age(sr[, 1], sr[, 2], u)
          o <- cd_sr_outputs(file, "freq2sedrate", sr[, 1], sr[, 2], tm)
          fcode <- if (p$col == "period") { "1 / dat[, 2]" } else { "dat[, 2]" }
          code <- c(sprintf("freqs <- data.frame(depth = dat[, 1], freq = %s * 100 / %s)  # cycles per metre", fcode, cd_fmt(u)),
              r$code, cd_sr_age_code(u), cd_code_write(o))
          list(result = list(sr = sr, tm = tm), outputs = o, code = code, text = cd_sr_text(o, sr[, 2], tm))
        },
        plot = function(res, p) {
          cd_sr_plot_curve(res$sr[, 1], res$sr[, 2], time = res$tm)
        })
  )
}

# Age models: anchoring and geochronology ####

cd_at_tools_agemodels <- function() {
  M <- "Age models"
  G <- "Anchoring and geochronology (astrochron)"
  list(
    cd_tool("at_anchor", M, "Anchor a floating time scale (anchorTime)", group = G, icon = "anchor",
        info = paste("Turns a floating astrochronology (time in kyr from the start of the record) into absolute ages by",
            "giving one point of the floating scale a radioisotopic or other age."),
        uses = "astrochron::anchorTime",
        params = list(
          cd_num("time", "Floating time of the anchor point (kyr)", function(d) { signif(min(d[, 1], na.rm = TRUE), 6) }),
          cd_num("age", "Age of the anchor point (kyr)", 1000, min = 0),
          cd_sel("timeDir", "The floating time increases", c("Towards the present (older at the start)" = "1",
              "Away from the present (younger at the start)" = "2")),
          cd_chk("flipOut", "Sort the output with decreasing age", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          r <- cd_at_call("anchorTime", list(dat = quote(dat), time = p$time, age = p$age, timeDir = as.integer(p$timeDir),
              flipOut = p$flipOut, genplot = FALSE, verbose = FALSE), list(dat = s))
          out <- as.data.frame(r$value)
          names(out) <- c("age_kyr", names(d)[2])
          o <- list(cd_output(cd_out_name(file, "-anchored"), out, var = "res"))
          list(result = list(s = s, out = out), outputs = o, code = c(r$code, cd_code_write(o)),
              text = sprintf("Floating time %s kyr set to %s kyr: ages %s to %s kyr.", cd_fmt(p$time), cd_fmt(p$age),
                  cd_fmt(signif(min(out[, 1]), 7)), cd_fmt(signif(max(out[, 1]), 7))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          graphics::plot(res$s[, 1], res$s[, 2], type = "l", col = "grey40", xlab = "Floating time (kyr)", ylab = "Value")
          graphics::abline(v = p$time, lty = 2, col = "#c0392b")
          cd_par()
          o <- res$out
          vw <- cd_view_lims(rev(range(o[, 1])), range(o[, 2]), x = o[, 1], y = o[, 2], last = TRUE)
          graphics::plot(o[, 1], o[, 2], type = "l", col = "#1f4e79", xlab = "Age (kyr)", ylab = "Value", xlim = vw$x,
              ylim = vw$y)
          graphics::abline(v = p$age, lty = 2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
        }),
    cd_tool("at_wtmean", M, "Weighted mean age (wtMean / stepHeat)", group = G, full_table = TRUE, icon = "scale-balanced",
        info = paste("Weighted mean of dates with uncertainties, MSWD and probability of fit, with the confidence",
            "interval of IsoPlot or ArArCALC (also for any values with uncertainties), or the age spectrum and plateau",
            "age of an Ar/Ar step-heating experiment. Data file: date and uncertainty (wtMean; or the six Ar/Ar columns",
            "date, uncertainty, K/Ca, %Ar40*, F, F uncertainty), or the seven step-heating columns %Ar39, date,",
            "uncertainty, K/Ca, %Ar40*, F, F uncertainty."),
        uses = "astrochron::wtMean, astrochron::stepHeat",
        params = list(
          cd_sel("method", "Data", c("Dates with uncertainties (wtMean)" = "wtMean", "Ar/Ar step heating (stepHeat)" = "stepHeat")),
          cd_sel("unc", "Uncertainties in the file are", c("1 sigma" = "1", "2 sigma" = "2")),
          cd_sel("CI", "95% confidence interval", c("ArArCALC (Koppers 2002)" = "2", "IsoPlot (Ludwig 2008)" = "1")),
          cd_txt("del", "Rows left out of the mean (optional)", "", numeric = TRUE,
              help = "Row numbers of the data file, e.g. outliers by Chauvenet's criterion listed in the result."),
          cd_sel("unit", "Time unit of the dates", c("Ma" = "1", "ka" = "2")),
          cd_num("J", "J value (neutron fluence, 0 = none)", 0, min = 0, help = "Ar/Ar only: adds the uncertainty of J."),
          cd_num("Jsd", "Uncertainty of J (0 = none)", 0, min = 0),
          cd_num("lambda", "Total decay constant of 40K (1/yr)", 5.463e-10, min = 0),
          cd_num("setAr", "%Ar40* level shown", 95, min = 0, max = 100, show_if = list(param = "method", values = "stepHeat"))),
        compute = function(d, p, d2, file, file2) {
          x <- as.data.frame(d)
          x <- x[stats::complete.cases(x), , drop = FALSE]
          if (p$method == "stepHeat") {
            if (ncol(x) < 7) {
              stop("stepHeat needs seven columns: %Ar39, date, uncertainty, K/Ca, %Ar40*, F, F uncertainty", call. = FALSE)
            }
            x <- x[, 1:7]
            sel <- "dat <- dat[, 1:7]"
          } else if (ncol(x) >= 6) {
            x <- x[, 1:6]
            sel <- "dat <- dat[, 1:6]"
          } else if (ncol(x) >= 2) {
            x <- x[, 1:2]
            sel <- "dat <- dat[, 1:2]"
          } else {
            stop("the data file needs a column of dates and a column of uncertainties", call. = FALSE)
          }
          del <- cd_at_vec(p$del)
          if (!is.null(del)) {
            del <- as.integer(del[del >= 1 & del <= nrow(x)])
          }
          useJ <- !is.null(cd_at_pos(p$J)) && !is.null(cd_at_pos(p$Jsd)) && ncol(x) >= 6
          a <- list(dat = quote(dat), unc = as.integer(p$unc), lambda = p$lambda,
              J = if (useJ) { p$J } else { NULL }, Jsd = if (useJ) { p$Jsd } else { NULL }, CI = as.integer(p$CI), cull = 0L,
              del = del)
          if (p$method == "wtMean") {
            a$sort <- 0L
          }
          a <- c(a, list(output = TRUE, unit = as.integer(p$unit)),
              if (p$method == "stepHeat") { list(setAr = p$setAr) } else { list() }, list(genplot = FALSE, verbose = TRUE))
          r <- cd_at_call(p$method, a, list(dat = x))
          tab <- as.data.frame(r$value, check.names = FALSE)
          rec <- grep("RECOMMENDED", r$log, value = TRUE)
          ci <- if (length(rec)) { cd_at_grab(rec[1], "=") } else { tab[1, 4] }
          o <- list(cd_output(cd_out_name(file, paste0("-", p$method)), tab, header = TRUE, var = "res"))
          un <- if (p$unit == "1") { "Ma" } else { "ka" }
          chv <- trimws(r$log[grepl("^ *Datum=", r$log)])
          txt <- c(sprintf("Weighted mean %s +/- %s %s (95%%, %s convention); MSWD %s, probability of fit %s.",
              cd_fmt(signif(tab[1, 1], 7)), cd_fmt(signif(ci, 3)), un, if (p$CI == "2") { "ArArCALC" } else { "IsoPlot" },
              cd_fmt(signif(tab$mswd[1], 3)), cd_fmt(signif(tab$prob_fit[1], 3))),
              if (useJ) { "The uncertainty of J is included." } else { character() },
              if (length(chv)) { c("Dates to consider leaving out (Chauvenet's criterion, or < 1% of the Ar39):",
                  paste0("  ", gsub(" ;", ";", chv))) } else { character() })
          list(result = list(x = x, del = del, mean = tab[1, 1], ci = ci, unc = as.integer(p$unc), method = p$method,
              unit = un, setAr = p$setAr), outputs = o, code = c(sel, r$code, cd_at_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          x <- res$x
          n <- nrow(x)
          used <- rep(TRUE, n)
          if (length(res$del)) {
            used[res$del] <- FALSE
          }
          dcol <- if (res$method == "stepHeat") { 2 } else { 1 }
          dt <- x[, dcol]
          s2 <- x[, dcol + 1] * if (res$unc == 1) { 2 } else { 1 }
          cols <- ifelse(used, "#1f4e79", "grey65")
          ylab <- sprintf("Date (%s)", res$unit)
          if (res$method == "stepHeat") {
            graphics::layout(matrix(1:3, ncol = 1), heights = c(1, 1, 2))
            cum <- c(0, cumsum(x[, 1]))
            mid <- 0.5 * (cum[-1] + cum[-(n + 1)])
            cd_par()
            graphics::plot(mid, x[, 5], type = "n", xlim = c(0, max(cum)), xlab = "", ylab = "%Ar40*")
            graphics::rect(cum[-(n + 1)], 0, cum[-1], x[, 5], col = grDevices::adjustcolor(cols, 0.4), border = cols)
            graphics::abline(h = res$setAr, lty = 2, col = "#c0392b")
            cd_par()
            graphics::plot(mid, x[, 4], type = "n", xlim = c(0, max(cum)), xlab = "", ylab = "K/Ca", log = "y")
            graphics::segments(cum[-(n + 1)], x[, 4], cum[-1], x[, 4], col = cols, lwd = 2)
            cd_par()
            vw <- cd_view_lims(c(0, max(cum)), range(c(dt - s2, dt + s2)), last = TRUE)
            graphics::plot(NA, xlim = vw$x, ylim = vw$y, xlab = "Cumulative %Ar39 released", ylab = ylab)
            graphics::rect(cum[-(n + 1)], dt - s2, cum[-1], dt + s2, col = grDevices::adjustcolor(cols, 0.35), border = cols)
            graphics::text(mid, dt + s2, labels = seq_len(n), pos = 3, cex = 0.7, col = "grey30")
          } else {
            cd_par()
            o <- order(dt)
            k <- seq_len(n)
            vw <- cd_view_lims(c(0.5, n + 0.5), range(c(dt - s2, dt + s2)), last = TRUE)
            graphics::plot(NA, xlim = vw$x, ylim = vw$y, xlab = "Date, in increasing order", ylab = ylab, xaxt = "n")
            graphics::segments(k, (dt - s2)[o], k, (dt + s2)[o], col = cols[o], lwd = 3)
            graphics::points(k, dt[o], pch = 16, col = cols[o], cex = 0.8)
            graphics::text(k, (dt + s2)[o], labels = o, pos = 3, cex = 0.7, col = "grey30")
          }
          graphics::rect(graphics::par("usr")[1], res$mean - res$ci, graphics::par("usr")[2], res$mean + res$ci,
              col = grDevices::adjustcolor("#c0392b", 0.15), border = NA)
          graphics::abline(h = res$mean, col = "#c0392b", lwd = 1.5)
          graphics::mtext(sprintf("Weighted mean %s +/- %s %s (95%%)", cd_fmt(signif(res$mean, 7)), cd_fmt(signif(res$ci, 3)),
              res$unit), side = 3, line = 0.2, cex = 0.85)
        })
  )
}

# Astronomy: astronomical periods ####

cd_at_tools_astronomy <- function() {
  M <- "Astronomy"
  G <- "Astronomical periods (astrochron)"
  fam_col <- c(eccentricity = "#8e44ad", obliquity = "#d35400", precession = "#2e8b57", other = "#7f8c8d")
  list(
    cd_tool("at_periods", M, "Precession and obliquity periods (bergerPeriods / calcPeriods)", group = G, inputs = 0L,
        icon = "clock-rotate-left",
        info = paste("Precession and obliquity periods at an age from Berger et al. (1992), or eccentricity, obliquity and",
            "precession periods from the Solar System frequencies g and s and the precession constant k (Laskar 1999 or",
            "Malinverno and Meyers 2024)."),
        uses = "astrochron::bergerPeriods, astrochron::calcPeriods",
        params = list(
          cd_sel("method", "Method", c("At an age, Berger et al. 1992 (bergerPeriods)" = "berger",
              "From g, s and k (calcPeriods)" = "calc")),
          cd_num("age", "Age (Ma, 0-500)", 400, min = 0, max = 500, show_if = list(param = "method", values = "berger")),
          cd_txt("g", "g1 to g5 (arcsec/yr)", "5.579378, 7.456665, 17.366595, 17.910194, 4.257564", numeric = TRUE,
              show_if = list(param = "method", values = "calc")),
          cd_txt("s", "s1, s2, s3, s4, (s5,) s6 (arcsec/yr, optional)", "-5.59, -7.05, -18.85, -17.755, -26.347",
              numeric = TRUE, show_if = list(param = "method", values = "calc"),
              help = "Needed for the obliquity periods; leave empty for eccentricity and precession only."),
          cd_num("k", "Precession constant k (arcsec/yr)", 50.475838, min = 0, show_if = list(param = "method", values = "calc")),
          cd_sel("opt", "Obliquity terms", c("Laskar (1999, table 6)" = "1", "Malinverno and Meyers (2024, table 1)" = "2"),
              show_if = list(param = "method", values = "calc"))),
        compute = function(d, p, d2, file, file2) {
          if (p$method == "berger") {
            r <- cd_at_call("bergerPeriods", list(age = p$age, genplot = FALSE))
            per <- c(cd_at_grab(r$log, "o1="), cd_at_grab(r$log, "o2="), cd_at_grab(r$log, "p1="), cd_at_grab(r$log, "p2="))
            tab <- data.frame(term = c("o1", "o2", "p1", "p2"), family = c("obliquity", "obliquity", "precession", "precession"),
                period_kyr = per)
            o <- list(cd_output(sprintf("bergerPeriods_%sMa.txt", cd_fmt(p$age)), tab, header = TRUE, var = "tab"))
            code <- c(sub("^res <- ", "", r$code), "# the periods are printed; the table is written by hand:",
                sprintf("tab <- data.frame(term = c(\"o1\", \"o2\", \"p1\", \"p2\"), period_kyr = %s)", cd_dep(per)),
                cd_at_code_write(o))
            return(list(result = list(tab = tab, method = "berger", age = p$age), outputs = o, code = code,
                text = c(sprintf("Berger et al. (1992) periods at %s Ma:", cd_fmt(p$age)), cd_at_table_text(tab))))
          }
          g <- cd_at_vec(p$g)
          s <- cd_at_vec(p$s)
          if (length(g) != 5) {
            stop("give five g frequencies (g1 to g5)", call. = FALSE)
          }
          if (!is.null(s) && !(length(s) %in% c(5, 6))) {
            stop("give five (s1-s4, s6) or six (s1-s6) s frequencies, or none", call. = FALSE)
          }
          r <- cd_at_call("calcPeriods", list(g = g, s = s, k = p$k, opt = as.integer(p$opt), output = 1))
          v <- as.data.frame(r$value, check.names = FALSE)
          nm <- names(v)
          fam <- ifelse(grepl("^g", nm), "eccentricity", ifelse(grepl("^k\\+s", nm), "obliquity", "precession"))
          tab <- data.frame(term = nm, family = fam, period_kyr = as.numeric(v[1, ]))
          o <- list(cd_output(sprintf("calcPeriods_k%s.txt", cd_fmt(p$k)), tab, header = TRUE, var = "tab"))
          code <- c(r$code, "tab <- data.frame(term = names(res), period_kyr = as.numeric(res[1, ]))", cd_at_code_write(o))
          list(result = list(tab = tab, method = "calc"), outputs = o, code = code,
              text = c(sprintf("Periods for k = %s arcsec/yr:", cd_fmt(p$k)), cd_at_table_text(tab)))
        },
        plot = function(res, p) {
          if (res$method == "berger") {
            cd_at_here(astrochron::bergerPeriods(age = res$age, genplot = TRUE))
            return(invisible(NULL))
          }
          tb <- res$tab
          cd_at_sticks(tb$period_kyr, tb$term, col = fam_col[tb$family])
        }),
    cd_tool("at_priors", M, "Priors of the Solar System frequencies (hoangPriors)", group = G, inputs = 0L,
        icon = "chart-area",
        info = paste("Probability density of the fundamental frequencies g1-g5 and s1-s4, s6 at an age, from the chaotic",
            "diffusion of the Solar System (Hoang et al. 2021, table 2): the priors of the Bayesian TimeOpt methods."),
        uses = "astrochron::hoangPriors",
        params = list(
          cd_num("age", "Age (Ga)", 1, min = 0, max = 4.5, step = 0.1)),
        compute = function(d, p, d2, file, file2) {
          types <- c(rep("g", 5), rep("s", 5))
          idx <- c(1:5, 1:4, 6)
          n <- length(idx)
          tab <- data.frame(frequency = paste0(types, idx), mu = NA_real_, sigma = NA_real_, alpha = NA_real_, mean = NA_real_,
              sd = NA_real_)
          curves <- vector("list", n)
          for (i in seq_len(n)) {
            h <- cd_at_hoang_moments(types[i], idx[i], p$age)
            tab[i, 2:6] <- c(h$mu, h$sigma, h$alpha, h$mean, h$sd)
            x <- seq(h$mean - 5 * h$sd, h$mean + 5 * h$sd, length.out = 300)
            y <- astrochron::hoangPriors(ssftype = types[i], i = idx[i], age = p$age, ssfi = x)$ssfipdf
            curves[[i]] <- list(x = x, y = y)
          }
          o <- list(cd_output(sprintf("hoangPriors_%sGa.txt", cd_fmt(p$age)), tab, header = TRUE, var = "tab"))
          code <- c("tab <- NULL", "for (f in c(\"g1\", \"g2\", \"g3\", \"g4\", \"g5\", \"s1\", \"s2\", \"s3\", \"s4\", \"s6\")) {",
              sprintf("  h <- astrochron::hoangPriors(ssftype = substr(f, 1, 1), i = as.integer(substr(f, 2, 2)), age = %s)",
                  cd_fmt(p$age)),
              "  tab <- rbind(tab, data.frame(frequency = f, h))", "}",
              "dl <- tab$alpha / sqrt(1 + tab$alpha^2)", "tab$mean <- tab$mu + tab$sigma * dl * sqrt(2 / pi)",
              "tab$sd <- tab$sigma * sqrt(1 - 2 * dl^2 / pi)",
              cd_at_code_write(o))
          list(result = list(tab = tab, curves = curves, age = p$age), outputs = o, code = code,
              text = c(sprintf("Skew-normal priors at %s Ga (arcsec/yr):", cd_fmt(p$age)), cd_at_table_text(tab, digits = 7)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:10, nrow = 2, byrow = TRUE))
          tb <- res$tab
          for (i in seq_len(nrow(tb))) {
            cd_par(mar = c(3.6, 3, 1.8, 0.6))
            cv <- res$curves[[i]]
            graphics::plot(cv$x, cv$y, type = "l", lwd = 1.5, col = if (i <= 5) { "#8e44ad" } else { "#d35400" },
                xlab = "arcsec/yr", ylab = "", main = tb$frequency[i], yaxt = "n")
            graphics::abline(v = tb$mean[i], lty = 2, col = "grey40")
          }
        }),
    cd_tool("at_alod", M, "Earth-Moon distance and day length from k (alodfromk)", group = G, inputs = 0L, icon = "moon",
        info = paste("Earth-Moon distance (Earth radii) and length of day (hours), with uncertainties, from a precession",
            "frequency k and its uncertainty (e.g. from Bayesian TimeOpt), after Walker et al. (1986) with the constants",
            "of Malinverno and Meyers (2024)."),
        uses = "astrochron::alodfromk",
        params = list(
          cd_num("k", "Precession frequency k (arcsec/yr)", 60, min = 0),
          cd_num("sdevk", "Standard deviation of k (arcsec/yr)", 1, min = 0)),
        compute = function(d, p, d2, file, file2) {
          if (!(p$sdevk > 0)) {
            stop("give a standard deviation of k larger than 0", call. = FALSE)
          }
          r <- cd_at_call("alodfromk", list(k = p$k, sdevk = p$sdevk, genplot = FALSE, verbose = FALSE))
          v <- as.numeric(r$value)
          tab <- data.frame(k_arcsec_yr = p$k, sd_k = p$sdevk, a_earth_radii = v[1], sd_a = v[2], lod_hours = v[3],
              sd_lod = v[4])
          o <- list(cd_output(sprintf("alodfromk_k%s.txt", cd_fmt(p$k)), tab, header = TRUE, var = "res"))
          list(result = list(k = p$k, sdevk = p$sdevk), outputs = o,
              code = c(r$code, "res <- as.data.frame(res)", cd_at_code_write(o)),
              text = sprintf("k = %s +/- %s arcsec/yr: Earth-Moon distance %s +/- %s Earth radii, length of day %s +/- %s hours.",
                  cd_fmt(p$k), cd_fmt(p$sdevk), cd_fmt(signif(v[1], 5)), cd_fmt(signif(v[2], 3)), cd_fmt(signif(v[3], 5)),
                  cd_fmt(signif(v[4], 3))))
        },
        plot = function(res, p) {
          cd_at_here(astrochron::alodfromk(k = res$k, sdevk = res$sdevk, genplot = TRUE, verbose = FALSE))
        }),
    cd_tool("at_tones", M, "Combination and difference tones (tones)", group = G, inputs = 0L, icon = "music",
        info = paste("All difference and combination tones (f1 - f2, f1 + f2) of a set of periods, and the tone closest",
            "to a period you observed, e.g. to explain a peak that is not a primary astronomical term."),
        uses = "astrochron::tones",
        params = list(
          cd_txt("periods", "Periods (kyr)", "405, 125, 95, 23.7, 22.4, 19", numeric = TRUE),
          cd_num("target", "Observed period to explain (kyr, 0 = none)", 0, min = 0)),
        compute = function(d, p, d2, file, file2) {
          per <- cd_at_vec(p$periods)
          if (length(per) < 2) {
            stop("give at least two periods", call. = FALSE)
          }
          per <- sort(per, decreasing = TRUE)
          f <- 1 / per
          n <- length(f)
          lab <- character()
          type <- character()
          fr <- numeric()
          for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
              lab <- c(lab, sprintf("%s - %s", cd_fmt(per[j]), cd_fmt(per[i])))
              type <- c(type, "difference")
              fr <- c(fr, abs(f[j] - f[i]))
            }
          }
          for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
              lab <- c(lab, sprintf("%s + %s", cd_fmt(per[j]), cd_fmt(per[i])))
              type <- c(type, "combination")
              fr <- c(fr, f[j] + f[i])
            }
          }
          tab <- data.frame(tone = seq_along(fr), type = type, terms = lab, frequency = fr, period_kyr = 1 / fr)
          tg <- cd_at_pos(p$target)
          r <- cd_at_call("tones", list(a = if (is.null(tg)) { NULL } else { signif(1 / tg, 10) }, freqs = signif(f, 10)),
              var = "tn")
          txt <- sprintf("%d difference and %d combination tones.", sum(type == "difference"), sum(type == "combination"))
          best <- NA_integer_
          if (!is.null(tg)) {
            best <- which.min(abs(fr - 1 / tg))
            txt <- c(txt, sprintf("Closest to %s kyr: tone %d, %s kyr (%s, %s).", cd_fmt(tg), best,
                cd_fmt(signif(tab$period_kyr[best], 5)), tab$type[best], tab$terms[best]))
          }
          o <- list(cd_output("tones.txt", tab, header = TRUE, var = "tab"))
          code <- c(sprintf("# periods %s kyr", paste(cd_fmt_period(per), collapse = ", ")), sub("^tn <- ", "", r$code),
              "# tones() prints the tones; the same table by hand:",
              sprintf("f <- 1 / %s", cd_dep(per)), "pf <- function(x) {",
              "  format(signif(x, 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)", "}", "tab <- NULL",
              "for (type in c(\"difference\", \"combination\")) {", "  for (i in 1:(length(f) - 1)) {",
              "    for (j in (i + 1):length(f)) {",
              "      fr <- if (type == \"difference\") { f[j] - f[i] } else { f[j] + f[i] }",
              "      terms <- paste(pf(1 / f[j]), if (type == \"difference\") { \"-\" } else { \"+\" }, pf(1 / f[i]))",
              "      tab <- rbind(tab, data.frame(type = type, terms = terms, frequency = fr, period_kyr = 1 / fr))",
              "    }", "  }", "}", "tab <- data.frame(tone = seq_len(nrow(tab)), tab)", cd_at_code_write(o))
          list(result = list(tab = tab, per = per, target = tg, best = best), outputs = o, code = code,
              text = c(txt, cd_at_table_text(tab[, -4])))
        },
        plot = function(res, p) {
          tb <- res$tab
          pr <- c(res$per, tb$period_kyr)
          lb <- c(cd_fmt_period(res$per), as.character(tb$tone))
          cl <- c(rep("#1f4e79", length(res$per)), ifelse(tb$type == "difference", "#d35400", "#2e8b57"))
          ht <- c(rep(1, length(res$per)), rep(0.6, nrow(tb)))
          cd_at_sticks(pr, lb, col = cl, height = ht, main = "Periods (blue), difference (orange) and combination tones (green)",
              vline = res$target)
        })
  )
}
