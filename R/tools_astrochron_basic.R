# Tools: astrochron_basic ####
#
# astrochron functions for data handling, transformations, tapers, resampling, moving windows, statistics and peaks
# (Math menu, group "More (astrochron)"), lowpass / highpass filtering and kernel detrending (Time Series menu, group
# "Pre-processing (astrochron)"), and signal generators, insolation, ice-volume, sedimentation and bioturbation
# models (Basic Series menu, group "Models and insolation (astrochron)").

# Helpers ####

# Evaluates an astrochron call without its console output. device = TRUE also sends the figures that some functions
# draw whatever their genplot argument (ar1etp, synthStrat, insoMap) to a null device, so the device of the app is
# left untouched.
cd_ab_quiet <- function(expr, device = FALSE) {
  if (device) {
    cur <- grDevices::dev.cur()
    grDevices::pdf(NULL)
    dv <- grDevices::dev.cur()
    on.exit({
      grDevices::dev.off(dv)
      if (cur > 1) {
        grDevices::dev.set(cur)
      }
    }, add = TRUE)
  }
  res <- NULL
  utils::capture.output(res <- expr)
  res
}

# Default lengths as a fraction of the record
cd_ab_len <- function(frac) {
  function(d) {
    signif(diff(range(d[, 1], na.rm = TRUE)) * frac, 3)
  }
}

cd_ab_cols <- function(d) {
  names(d)[-1]
}

cd_ab_colname <- function(k) {
  function(d) {
    names(d)[min(k, ncol(d))]
  }
}

cd_ab_col <- function(d, name, default = 2L) {
  j <- match(name, names(d))
  if (is.na(j)) {
    j <- min(default, ncol(d))
  }
  j
}

# Columns of a table, without empty values, sorted by column 1, duplicated levels averaged (the moving-window
# functions of astrochron stop on unsorted or duplicated levels)
cd_ab_table <- function(d, cols) {
  s <- as.data.frame(d)[, cols, drop = FALSE]
  s <- s[stats::complete.cases(s), , drop = FALSE]
  nm <- names(s)
  s <- stats::aggregate(s[, -1, drop = FALSE], by = list(s[, 1]), FUN = mean)
  names(s) <- nm
  s
}

cd_ab_table_code <- function(cols) {
  c(sprintf("s <- dat[complete.cases(dat[, %s]), %s]", cd_dep(as.integer(cols)), cd_dep(as.integer(cols))),
      "s <- aggregate(s[, -1, drop = FALSE], by = list(s[, 1]), FUN = mean)")
}

# TRUE when every sampling step is exactly the same (sedRamp tests diff() for exact equality)
cd_ab_exact <- function(x) {
  dx <- diff(x)
  length(dx) > 0 && max(dx) == min(dx)
}

cd_ab_name <- function(file2) {
  if (is.null(file2)) {
    return("file2")
  }
  gsub("[^A-Za-z0-9_.-]", "_", tools::file_path_sans_ext(basename(file2)))
}

# Image with contours (insolation maps)
cd_ab_map <- function(long, lat, z, main, diverging = FALSE, last = FALSE) {
  cd_par()
  if (diverging) {
    zl <- max(abs(z), na.rm = TRUE) * c(-1, 1)
    cols <- grDevices::hcl.colors(100, "Blue-Red 3")
  } else {
    zl <- range(z, na.rm = TRUE)
    cols <- cd_viridis(100)
  }
  vw <- cd_view_lims(range(long), range(lat), last = last)
  graphics::image(long, lat, z, col = cols, zlim = zl, xlab = "True solar longitude (degrees, 90 = June solstice)",
      ylab = "Latitude (degrees)", main = main, xlim = vw$x, ylim = vw$y, useRaster = TRUE)
  graphics::contour(long, lat, z, add = TRUE, col = grDevices::adjustcolor("white", 0.8), lwd = 0.6, labcex = 0.6)
  graphics::abline(v = c(90, 180, 270), col = grDevices::adjustcolor("white", 0.5), lty = 3)
  graphics::box()
}

# Insolation map (Milankovitch object of palinsol, through astrochron::insoMap) as a plain matrix: rows true solar
# longitude, columns latitude
cd_ab_insomap <- function(t, S0) {
  m <- cd_ab_quiet(astrochron::insoMap(t = t, pick = 0, S0 = S0, output = TRUE, verbose = FALSE), device = TRUE)
  long <- attr(m, "long")
  lat <- attr(m, "lat")
  z <- matrix(as.numeric(m), nrow = length(long), ncol = length(lat))
  list(long = long, lat = lat, z = z)
}

cd_ab_map_table <- function(long, lat, z) {
  tab <- data.frame(long = long, z)
  names(tab) <- c("true_solar_longitude", paste0("lat_", lat))
  tab
}

cd_tools_astrochron_basic <- function() {
  c(cd_ab_tools_math(), cd_ab_tools_timeseries(), cd_ab_tools_models())
}

# Math menu ####

cd_ab_tools_math <- function() {
  M <- "Math"
  gm <- "More (astrochron)"
  list(
    cd_tool("ab_clean", M, "Clean values and select levels (all columns)", group = gm, full_table = TRUE, icon = "broom",
        info = paste("Works on all columns of a table: removes levels with an empty value, replaces negative or zero",
            "values (e.g. before a log transform), removes values above or below a threshold, or keeps an interval."),
        uses = "astrochron::rmNA / repl0 / replEps / trimAT / iso",
        params = list(
          cd_sel("method", "Operation", c("Remove levels with an empty value (rmNA)" = "rmNA",
              "Replace negative values by 0 (repl0)" = "repl0",
              "Replace values <= 0 by the smallest positive number (replEps)" = "replEps",
              "Remove values above or below a threshold (trimAT)" = "trimAT",
              "Keep an interval of all columns (iso)" = "iso")),
          cd_sel("col", "Column", cd_ab_cols, value = cd_ab_colname(2),
              help = "Column shown in the figure; trimAT tests and keeps only this column."),
          cd_num("thresh", "Threshold", function(d) { signif(stats::quantile(d[, 2], 0.99, na.rm = TRUE), 4) },
              show_if = list(param = "method", values = "trimAT")),
          cd_sel("dir", "Remove the values", c("larger than the threshold" = "2", "smaller than the threshold" = "1"),
              show_if = list(param = "method", values = "trimAT")),
          cd_num("xmin", "From", function(d) { cd_part(d, 0.25) }, show_if = list(param = "method", values = "iso")),
          cd_num("xmax", "To", function(d) { cd_part(d, 0.75) }, show_if = list(param = "method", values = "iso"))),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          j <- cd_ab_col(d, p$col)
          before <- d[stats::complete.cases(d[, c(1, j)]), c(1, j)]
          if (p$method == "rmNA") {
            res <- cd_ab_quiet(astrochron::rmNA(d, genplot = FALSE, verbose = FALSE))
            code <- "res <- astrochron::rmNA(dat, genplot = FALSE, verbose = FALSE)"
            sfx <- "-rmNA"
            txt <- sprintf("%d of %d levels kept.", nrow(res), nrow(d))
          } else if (p$method %in% c("repl0", "replEps")) {
            v <- as.matrix(d[, -1, drop = FALSE])
            if (p$method == "repl0") {
              nrep <- sum(v < 0, na.rm = TRUE)
              res <- cd_ab_quiet(astrochron::repl0(d, ID = TRUE, genplot = FALSE, verbose = FALSE))
            } else {
              nrep <- sum(v <= 0, na.rm = TRUE)
              res <- cd_ab_quiet(astrochron::replEps(d, ID = TRUE, genplot = FALSE, verbose = FALSE))
            }
            code <- sprintf("res <- astrochron::%s(dat, ID = TRUE, genplot = FALSE, verbose = FALSE)", p$method)
            sfx <- paste0("-", p$method)
            txt <- sprintf("%d values replaced.", nrep)
          } else if (p$method == "trimAT") {
            res <- cd_ab_quiet(astrochron::trimAT(before, thresh = p$thresh, dir = as.integer(p$dir), genplot = FALSE,
                verbose = FALSE))
            code <- c(sprintf("s <- dat[complete.cases(dat[, c(1, %d)]), c(1, %d)]", j, j),
                sprintf("res <- astrochron::trimAT(s, thresh = %s, dir = %d, genplot = FALSE, verbose = FALSE)",
                    cd_fmt(p$thresh), as.integer(p$dir)))
            sfx <- paste0("-trimAT", cd_fmt(p$thresh))
            txt <- sprintf("%d of %d values removed.", nrow(before) - NROW(res), nrow(before))
          } else {
            if (!(p$xmax > p$xmin)) {
              stop("'To' must be larger than 'From'", call. = FALSE)
            }
            res <- cd_ab_quiet(astrochron::iso(d, xmin = p$xmin, xmax = p$xmax, genplot = FALSE, verbose = FALSE))
            code <- sprintf("res <- astrochron::iso(dat, xmin = %s, xmax = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$xmin), cd_fmt(p$xmax))
            sfx <- sprintf("-iso%s-%s", cd_fmt(p$xmin), cd_fmt(p$xmax))
            txt <- sprintf("%d of %d levels kept.", NROW(res), nrow(d))
          }
          res <- as.data.frame(res)
          if (!nrow(res)) {
            stop("no values are left", call. = FALSE)
          }
          after <- if (ncol(res) == 2) { res } else { res[, c(1, j)] }
          after <- after[stats::complete.cases(after), , drop = FALSE]
          o <- list(cd_output(cd_out_name(file, sfx), res, header = ncol(res) > 2))
          list(result = list(before = before, after = after, lab = names(d)[j]), outputs = o, text = txt,
              code = c(code, sprintf("CycloDesk::write_series(res, %s, header = %s)", cd_dep(o[[1]]$name), ncol(res) > 2)))
        },
        plot = function(res, p) {
          cd_plot_series(list(res$before, res$after), labels = c("input", "result"), ylab = res$lab)
        }),

    cd_tool("ab_transform", M, "Transformations (astrochron)", group = gm, icon = "wand-magic-sparkles",
        info = paste("Divides by the linear trend, arcsine transform, flips the stratigraphic order, clips or mutes the",
            "values below a threshold (enhances the modulator, e.g. eccentricity), pads with zeros, or converts depth to",
            "time with a constant sedimentation rate."),
        uses = "astrochron::divTrend / arcsinT / flip / clipIt / pad / constantSedrate",
        params = list(
          cd_sel("method", "Transformation", c("Divide by the linear trend (divTrend)" = "divTrend",
              "Arcsine transform, after rescaling to 0-1 (arcsinT)" = "arcsinT",
              "Flip the stratigraphic order (flip)" = "flip",
              "Clip the values below a threshold (clipIt)" = "clipIt",
              "Mute the values below a threshold by a divisor (clipIt)" = "clipdiv",
              "Pad with zeros (pad)" = "pad",
              "Depth to time with a constant sed. rate (constantSedrate)" = "constantSedrate")),
          cd_num("begin", "Height given to the new first level", 0, show_if = list(param = "method", values = "flip")),
          cd_num("cthresh", "Threshold", function(d) { signif(mean(d[, 2], na.rm = TRUE), 5) },
              show_if = list(param = "method", values = c("clipIt", "clipdiv"))),
          cd_num("clipval", "Value given to the clipped values", function(d) { signif(mean(d[, 2], na.rm = TRUE), 5) },
              show_if = list(param = "method", values = "clipIt")),
          cd_num("clipdiv", "Divisor", 2, min = 1, show_if = list(param = "method", values = "clipdiv")),
          cd_num("zeros", "Number of zeros added at the end", function(d) { nrow(d) }, min = 1, step = 1,
              show_if = list(param = "method", values = "pad")),
          cd_num("sedrate", "Sedimentation rate (depth units per time unit)", 0.02, min = 0,
              help = "In the units of the depth column per time unit, e.g. 0.02 m/kyr for 2 cm/kyr with depths in metres.",
              show_if = list(param = "method", values = "constantSedrate")),
          cd_num("tbegin", "Time of the first level", 0, show_if = list(param = "method", values = "constantSedrate")),
          cd_sel("timeDir", "Time", c("increases with depth" = "1", "decreases with depth" = "-1"),
              show_if = list(param = "method", values = "constantSedrate"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          code <- character()
          txt <- character()
          m <- p$method
          if (m == "divTrend") {
            res <- cd_ab_quiet(astrochron::divTrend(s, genplot = FALSE, verbose = FALSE))
            call <- "astrochron::divTrend(dat, genplot = FALSE, verbose = FALSE)"
            sfx <- "-divTrend"
          } else if (m == "arcsinT") {
            res <- cd_ab_quiet(astrochron::arcsinT(s, genplot = FALSE, verbose = FALSE))
            call <- "astrochron::arcsinT(dat, genplot = FALSE, verbose = FALSE)"
            sfx <- "-arcsin"
          } else if (m == "flip") {
            res <- cd_ab_quiet(astrochron::flip(s, begin = p$begin, genplot = FALSE, verbose = FALSE))
            call <- sprintf("astrochron::flip(dat, begin = %s, genplot = FALSE, verbose = FALSE)", cd_fmt(p$begin))
            sfx <- "-flip"
          } else if (m == "clipIt") {
            res <- cd_ab_quiet(astrochron::clipIt(s, thresh = p$cthresh, clipval = p$clipval, genplot = FALSE,
                verbose = FALSE))
            call <- sprintf("astrochron::clipIt(dat, thresh = %s, clipval = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$cthresh), cd_fmt(p$clipval))
            sfx <- paste0("-clip", cd_fmt(p$cthresh))
            txt <- sprintf("%d of %d values clipped.", sum(s[, 2] < p$cthresh), nrow(s))
          } else if (m == "clipdiv") {
            res <- cd_ab_quiet(astrochron::clipIt(s, thresh = p$cthresh, clipdiv = p$clipdiv, genplot = FALSE,
                verbose = FALSE))
            call <- sprintf("astrochron::clipIt(dat, thresh = %s, clipdiv = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$cthresh), cd_fmt(p$clipdiv))
            sfx <- paste0("-clipdiv", cd_fmt(p$clipdiv))
            txt <- sprintf("%d of %d values muted.", sum(s[, 2] < p$cthresh), nrow(s))
          } else if (m == "pad") {
            s <- cd_even(d)
            names(s) <- names(d)[1:2]
            txt <- cd_even_note(d, s)
            code <- cd_even_code(d)
            res <- cd_ab_quiet(astrochron::pad(s, zeros = as.integer(p$zeros), genplot = FALSE, verbose = FALSE))
            call <- sprintf("astrochron::pad(dat, zeros = %d, genplot = FALSE, verbose = FALSE)", as.integer(p$zeros))
            sfx <- "-pad"
          } else {
            if (!(p$sedrate > 0)) {
              stop("the sedimentation rate must be positive", call. = FALSE)
            }
            res <- cd_ab_quiet(astrochron::constantSedrate(s, sedrate = p$sedrate, begin = p$tbegin,
                timeDir = as.integer(p$timeDir), genplot = FALSE, verbose = FALSE))
            call <- sprintf("astrochron::constantSedrate(dat, sedrate = %s, begin = %s, timeDir = %d, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$sedrate), cd_fmt(p$tbegin), as.integer(p$timeDir))
            sfx <- paste0("-sr", cd_fmt(p$sedrate))
            txt <- sprintf("Time from %s to %s.", cd_fmt(signif(min(res[, 1]), 6)), cd_fmt(signif(max(res[, 1]), 6)))
          }
          res <- as.data.frame(res)
          res <- res[order(res[, 1]), , drop = FALSE]
          o <- list(cd_output(cd_out_name(file, sfx), res))
          list(result = list(before = s, after = res), outputs = o, text = txt,
              code = c(code, paste0("res <- ", call), "res <- res[order(res[, 1]), ]", cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "input", "result")
        }),

    cd_tool("ab_taper", M, "Data taper (window)", group = gm, icon = "bell",
        info = paste("Applies a cosine-tapered (Tukey), DPSS, Gaussian or Hann taper to the series, e.g. before a",
            "periodogram, and shows the shape of the taper and its spectral window."),
        uses = "astrochron::cosTaper / dpssTaper / gausTaper / hannTaper",
        params = list(
          cd_sel("taper", "Taper", c("Cosine-tapered window (cosTaper)" = "cos", "Discrete prolate spheroidal sequence (dpssTaper)" = "dpss",
              "Gaussian window (gausTaper)" = "gaus", "Hann window (hannTaper)" = "hann")),
          cd_num("p", "Fraction of the series tapered (1 = Hann)", 0.25, min = 0, max = 1, step = 0.05,
              show_if = list(param = "taper", values = "cos")),
          cd_num("tbw", "Time-bandwidth product", 2, min = 0.5, step = 0.5, show_if = list(param = "taper", values = "dpss")),
          cd_num("num", "Which DPSS (1 = first)", 1, min = 1, step = 1, show_if = list(param = "taper", values = "dpss")),
          cd_num("alpha", "Gaussian alpha (1 / standard deviation, >= 2.5)", 3, min = 0.5, step = 0.5,
              show_if = list(param = "taper", values = "gaus")),
          cd_chk("rms", "Normalise the taper to RMS = 1 (keeps the power of a white process)", TRUE),
          cd_chk("demean", "Remove the mean first", TRUE),
          cd_chk("detrend", "Remove a linear trend first", FALSE),
          cd_chk("write_win", "Also write the taper itself (256 points)", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          names(s) <- names(d)[1:2]
          fn <- c(cos = "cosTaper", dpss = "dpssTaper", gaus = "gausTaper", hann = "hannTaper")[[p$taper]]
          extra <- switch(p$taper, cos = sprintf("p = %s, ", cd_fmt(p$p)),
              dpss = sprintf("tbw = %s, num = %d, ", cd_fmt(p$tbw), as.integer(p$num)),
              gaus = sprintf("alpha = %s, ", cd_fmt(p$alpha)), hann = "")
          tap <- function(x) {
            if (p$taper == "cos") {
              astrochron::cosTaper(x, p = p$p, rms = p$rms, demean = p$demean, detrend = p$detrend, genplot = FALSE,
                  verbose = FALSE)
            } else if (p$taper == "dpss") {
              astrochron::dpssTaper(x, tbw = p$tbw, num = as.integer(p$num), rms = p$rms, demean = p$demean,
                  detrend = p$detrend, genplot = FALSE, verbose = FALSE)
            } else if (p$taper == "gaus") {
              astrochron::gausTaper(x, alpha = p$alpha, rms = p$rms, demean = p$demean, detrend = p$detrend,
                  genplot = FALSE, verbose = FALSE)
            } else {
              astrochron::hannTaper(x, rms = p$rms, demean = p$demean, detrend = p$detrend, genplot = FALSE,
                  verbose = FALSE)
            }
          }
          res <- as.data.frame(cd_ab_quiet(tap(s)))
          names(res) <- names(s)
          win <- as.data.frame(cd_ab_quiet(tap(NULL)))
          names(win) <- c("point", "taper")
          w <- win[, 2]
          nfft <- 16L * length(w)
          sp <- Mod(stats::fft(c(w, numeric(nfft - length(w)))))^2
          half <- seq_len(nfft %/% 2)
          fr <- (half - 1) * length(w) / nfft
          db <- 10 * log10(pmax(sp[half] / max(sp[half]), 1e-12))
          keep <- fr <= 12
          opts <- sprintf("%srms = %s, demean = %s, detrend = %s, genplot = FALSE, verbose = FALSE", extra, p$rms,
              p$demean, p$detrend)
          o <- list(cd_output(cd_out_name(file, paste0("-", fn)), res))
          code <- c(cd_even_code(d), sprintf("res <- astrochron::%s(dat, %s)", fn, opts))
          if (isTRUE(p$write_win)) {
            o[[2]] <- cd_output(cd_out_name(file, paste0("-", fn, "-window")), win, var = "win")
            code <- c(code, sprintf("win <- astrochron::%s(NULL, %s)", fn, opts))
          }
          list(result = list(before = s, after = res, win = win, sw = data.frame(f = fr[keep], db = db[keep]), fn = fn),
              outputs = o, text = cd_even_note(d, s), code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE))
          cd_par()
          graphics::plot(res$win[, 1], res$win[, 2], type = "l", col = "#2e8b57", lwd = 1.5, xlab = "Point (of 256)",
              ylab = "Weight", main = res$fn)
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          graphics::plot(res$sw$f, res$sw$db, type = "l", col = "#8e44ad", xlab = "Frequency (Rayleigh units, 1/N)",
              ylab = "Power (dB)", ylim = c(max(-100, min(res$sw$db)), 0), main = "Spectral window")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          cd_plot_one(res$after, col = "#1f4e79", xlab = "Depth / time", ylab = "Tapered series")
        }),

    cd_tool("ab_resample", M, "Resample at the levels of a second file (2 files)", group = gm, inputs = 2L,
        icon = "arrows-to-dot",
        info = paste("Interpolates series 1 linearly at the depths / times of series 2 (e.g. to compare or cross-plot",
            "two proxies measured at different levels)."),
        uses = "astrochron::resample",
        params = list(cd_chk("join", "Also write a table with both series at the levels of file 2", TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          s2 <- cd_clean(d2)
          xo <- s2$x[s2$x >= min(s[, 1]) & s2$x <= max(s[, 1])]
          if (length(xo) < 2) {
            stop("the levels of file 2 do not overlap the range of file 1", call. = FALSE)
          }
          res <- as.data.frame(cd_ab_quiet(astrochron::resample(s, xout = xo, genplot = FALSE, verbose = FALSE)))
          names(res) <- names(s)
          b2 <- cd_ab_name(file2)
          o <- list(cd_output(cd_out_name(file, paste0("-at-", b2)), res))
          code <- c(cd_code_read(file2, var = "dat2"), "xo <- sort(unique(dat2[, 1]))",
              "xo <- xo[xo >= min(dat[, 1]) & xo <= max(dat[, 1])]",
              "res <- astrochron::resample(dat, xout = xo, genplot = FALSE, verbose = FALSE)")
          if (isTRUE(p$join)) {
            tab <- data.frame(res[, 1], res[, 2], s2$y[match(res[, 1], s2$x)])
            names(tab) <- c(names(s)[1], names(s)[2], if (ncol(d2) >= 2) { names(d2)[2] } else { "series2" })
            if (identical(names(tab)[2], names(tab)[3])) {
              names(tab)[3] <- paste0(names(tab)[3], "_2")
            }
            o[[2]] <- cd_output(cd_out_name(file, paste0("-with-", b2)), tab, header = TRUE, var = "tab")
            code <- c(code, "tab <- data.frame(res, dat2[match(res[, 1], dat2[, 1]), 2])")
          }
          code <- c(code, sprintf("CycloDesk::write_series(res, %s)", cd_dep(o[[1]]$name)))
          if (length(o) > 1) {
            code <- c(code, sprintf("CycloDesk::write_series(tab, %s, header = TRUE)", cd_dep(o[[2]]$name)))
          }
          list(result = list(before = s, after = res), outputs = o, code = code,
              text = sprintf("%d levels of file 2 inside the range of file 1.", nrow(res)))
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          cd_plot_one(res$before, col = "#9aa5b1", xlab = "Depth / time", ylab = names(res$before)[2])
          graphics::points(res$after[, 1], res$after[, 2], pch = 16, cex = 0.5, col = "#c0392b")
          graphics::legend("topright", c("series 1", "resampled at the levels of series 2"), col = c("#9aa5b1", "#c0392b"),
              lty = c(1, NA), pch = c(NA, 16), bty = "n", cex = 0.85)
        }),

    cd_tool("ab_lh13", M, "Interpolation of uneven records (Laepple and Huybers 2013)", group = gm, icon = "arrows-left-right",
        heavy = function(p) { isTRUE(p$nsim > 1) },
        info = paste("Estimates the sampling interval at which linear interpolation of an unevenly sampled record does not",
            "bias the spectral background (from power-law noise surrogates on the actual sampling grid), then interpolates",
            "with an anti-alias filter."),
        uses = "astrochron::linterpLH13",
        params = list(
          cd_sel("dtmode", "Interpolation interval", c("Estimate it (Laepple and Huybers 2013)" = "est", "Given" = "given")),
          cd_num("dt", "Interval", function(d) { signif(cd_dt(d[, 1]), 4) }, min = 0,
              show_if = list(param = "dtmode", values = "given")),
          cd_num("thresh", "Threshold of the spectral ratio R", 0.7, min = 0.05, max = 0.99, step = 0.05,
              show_if = list(param = "dtmode", values = "est")),
          cd_num("beta", "Power-law exponent of the noise surrogates", 1, min = 0, step = 0.25,
              show_if = list(param = "dtmode", values = "est")),
          cd_num("tbw", "MTM time-bandwidth product", 20, min = 1, step = 1, show_if = list(param = "dtmode", values = "est")),
          cd_num("smooth", "Smoothing of the ratio R", 0.05, min = 0, step = 0.01, show_if = list(param = "dtmode", values = "est")),
          cd_num("nsim", "Number of noise simulations (> 1 runs in parallel)", 1, min = 1, step = 1,
              show_if = list(param = "dtmode", values = "est")),
          cd_chk("antialias", "Anti-alias filter", TRUE),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          if (cd_is_even(s[, 1])) {
            stop("the series is already evenly spaced: this method is meant for unevenly sampled records", call. = FALSE)
          }
          nsim <- max(1L, as.integer(p$nsim))
          given <- p$dtmode == "given"
          set.seed(as.integer(p$seed))
          res <- cd_ab_quiet(astrochron::linterpLH13(s, dt = if (given) { p$dt } else { NULL }, thresh = p$thresh,
              beta = p$beta, tbw = p$tbw, smooth = p$smooth, antialias = isTRUE(p$antialias), nsim = nsim,
              ncores = if (nsim > 1) { 2L } else { 1L }, genplot = FALSE, verbose = 0))
          res <- as.data.frame(res)
          names(res) <- names(s)
          dtu <- res[2, 1] - res[1, 1]
          dx <- diff(s[, 1])
          o <- list(cd_output(cd_out_name(file, paste0("-LH13-", cd_fmt(signif(dtu, 4)))), res))
          code <- c(sprintf("set.seed(%d)", as.integer(p$seed)),
              sprintf(paste0("res <- astrochron::linterpLH13(dat, dt = %s, thresh = %s, beta = %s, tbw = %s, smooth = %s,\n",
                  "    antialias = %s, nsim = %d, ncores = %d, genplot = FALSE, verbose = 0)"),
                  if (given) { cd_fmt(p$dt) } else { "NULL" }, cd_fmt(p$thresh), cd_fmt(p$beta), cd_fmt(p$tbw),
                  cd_fmt(p$smooth), isTRUE(p$antialias), nsim, if (nsim > 1) { 2L } else { 1L }), cd_code_write(o))
          txt <- c(sprintf("Interpolation interval: %s%s.", cd_fmt(signif(dtu, 4)), if (given) { " (given)" } else { " (estimated)" }),
              sprintf("Sampling interval of the record: min %s, median %s, mean %s, max %s.", cd_fmt(signif(min(dx), 4)),
                  cd_fmt(signif(stats::median(dx), 4)), cd_fmt(signif(mean(dx), 4)), cd_fmt(signif(max(dx), 4))))
          list(result = list(before = s, after = res), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          cd_plot_one(res$after, col = "#1f4e79", xlab = "Depth / time", ylab = names(res$after)[2])
          graphics::points(res$before[, 1], res$before[, 2], pch = 16, cex = 0.45, col = grDevices::adjustcolor("#c0392b", 0.7))
          graphics::legend("topright", c("interpolated", "samples"), col = c("#1f4e79", "#c0392b"), lty = c(1, NA),
              pch = c(NA, 16), bty = "n", cex = 0.85)
        }),

    cd_tool("ab_peaks", M, "Peaks and troughs (astrochron)", group = gm, icon = "mountain-sun",
        info = paste("Finds the maxima of peaks and / or minima of troughs, optionally only above (peaks) or below",
            "(troughs) a level, or lists the plateau points that are not evaluated as peaks or troughs."),
        uses = "astrochron::peak, astrochron::trough",
        params = list(
          cd_sel("which", "Find", c("Peaks" = "peak", "Troughs" = "trough", "Peaks and troughs" = "both")),
          cd_chk("use_level", "Only peaks above / troughs below a level", FALSE),
          cd_num("level", "Level", function(d) { signif(mean(d[, 2], na.rm = TRUE), 5) },
              show_if = list(param = "use_level", values = "true")),
          cd_chk("plateau", "List the plateau points instead (flat tops / bottoms, not evaluated)", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          lev <- if (isTRUE(p$use_level)) { p$level } else { NULL }
          lev_c <- if (isTRUE(p$use_level)) { cd_fmt(p$level) } else { "NULL" }
          pl <- isTRUE(p$plateau)
          res <- list(data = s, level = lev)
          o <- list()
          code <- character()
          txt <- character()
          kinds <- if (p$which == "both") { c("peak", "trough") } else { p$which }
          for (k in kinds) {
            if (k == "peak") {
              r <- cd_ab_quiet(astrochron::peak(s, level = lev, plateau = pl, genplot = FALSE, verbose = FALSE))
            } else {
              r <- cd_ab_quiet(astrochron::trough(s, level = lev, plateau = pl, genplot = FALSE, verbose = FALSE))
            }
            lab <- paste0(k, if (pl) { " plateau points" } else { "s" })
            if (is.null(r) || !NROW(r)) {
              txt <- c(txt, sprintf("No %s found.", lab))
              next
            }
            r <- as.data.frame(r)
            out <- data.frame(r[, 2], r[, 3])
            names(out) <- c(names(s)[1], paste0(k, "_value"))
            res[[k]] <- out
            v <- paste0(k, "s")
            o[[length(o) + 1]] <- cd_output(cd_out_name(file, paste0("-", k, if (pl) { "-plateau" } else { "s" })), out, var = v)
            code <- c(code, sprintf("%s <- astrochron::%s(dat, level = %s, plateau = %s, genplot = FALSE, verbose = FALSE)[, 2:3]",
                v, k, lev_c, pl))
            txt <- c(txt, sprintf("%d %s%s.", nrow(out), lab, if (nrow(out) > 1 && !pl) {
              sprintf(", mean spacing %s", cd_fmt(signif(mean(diff(out[, 1])), 4))) } else { "" }))
          }
          if (!length(o)) {
            stop(paste(txt, collapse = " "), call. = FALSE)
          }
          list(result = res, outputs = o, code = c(code, cd_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          cd_plot_one(res$data, col = "#7f8c8d", xlab = "Depth / time", ylab = names(res$data)[2])
          if (!is.null(res$level)) {
            graphics::abline(h = res$level, col = "#d35400", lty = 2)
          }
          if (!is.null(res$peak)) {
            graphics::points(res$peak[, 1], res$peak[, 2], pch = 24, bg = "#c0392b", col = "#c0392b", cex = 0.7)
          }
          if (!is.null(res$trough)) {
            graphics::points(res$trough[, 1], res$trough[, 2], pch = 25, bg = "#1f4e79", col = "#1f4e79", cex = 0.7)
          }
        }),

    cd_tool("ab_mwstats", M, "Moving-window statistics", group = gm, icon = "chart-area",
        info = paste("Mean, median and variance, or minimum, maximum and range, in a window of constant length in depth or",
            "time (the number of points per window adapts to uneven sampling); on an even grid also the distribution",
            "of the values in each window."),
        uses = "astrochron::mwStats / mwStatsGrid / mwMinMax / mwin / mwinGrid",
        params = list(
          cd_sel("method", "Statistics", c("Mean, median and variance (mwStats)" = "mwStats",
              "Mean, median, variance and value densities on an even grid (mwStatsGrid)" = "grid",
              "Minimum, maximum and range (mwMinMax)" = "minmax")),
          cd_num("win", "Window length (depth / time units)", cd_ab_len(0.1), min = 0),
          cd_sel("conv", "Window placement", c("Centred on every level" = "1", "Starting at the smallest depth / time" = "2",
              "Starting at the largest depth / time" = "3"), show_if = list(param = "method", values = c("mwStats", "minmax"))),
          cd_num("CI", "Confidence interval of the mean (%, 0 = none)", 95, min = 0, max = 99.9,
              show_if = list(param = "method", values = "mwStats")),
          cd_chk("ends", "Ends: mean of the first and last half window (centred windows only)", FALSE,
              show_if = list(param = "method", values = "mwStats")),
          cd_num("step", "Grid step", cd_ab_len(0.01), min = 0, show_if = list(param = "method", values = "grid")),
          cd_chk("windows", "Also write the table of windows (mwin / mwinGrid)", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_ab_table(d, 1:2)
          names(s) <- names(d)[1:2]
          if (!(p$win > 0) || p$win >= diff(range(s[, 1]))) {
            stop("the window must be positive and shorter than the record", call. = FALSE)
          }
          conv <- as.integer(p$conv)
          code <- cd_ab_table_code(1:2)
          res <- list(data = s, method = p$method)
          if (p$method == "mwStats") {
            ends <- isTRUE(p$ends) && conv == 1L
            r <- cd_ab_quiet(astrochron::mwStats(s, win = p$win, conv = conv, ends = ends, CI = p$CI, output = TRUE,
                genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::mwStats(s, win = %s, conv = %d, ends = %s, CI = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$win), conv, ends, cd_fmt(p$CI)))
            sfx <- "-mwStats"
          } else if (p$method == "grid") {
            if (!(p$step > 0)) {
              stop("the grid step must be positive", call. = FALSE)
            }
            r <- cd_ab_quiet(astrochron::mwStatsGrid(s, win = p$win, step = p$step, output = TRUE, genplot = 0, verbose = FALSE))
            loc <- cd_ab_quiet(astrochron::mwinGrid(s, win = p$win, step = p$step, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::mwStatsGrid(s, win = %s, step = %s, genplot = 0, verbose = FALSE)",
                cd_fmt(p$win), cd_fmt(p$step)))
            yr <- range(s[, 2])
            gy <- seq(yr[1], yr[2], length.out = 150)
            dens <- matrix(NA_real_, nrow(loc), length(gy))
            for (i in seq_len(nrow(loc))) {
              y <- s[loc$n1[i]:loc$n2[i], 2]
              if (length(y) > 1 && stats::sd(y) > 0) {
                dens[i, ] <- stats::density(y, bw = "nrd0", from = yr[1], to = yr[2], n = length(gy))$y
              }
            }
            res$dens <- list(x = loc$center, y = gy, z = dens)
            sfx <- "-mwStatsGrid"
          } else {
            r <- cd_ab_quiet(astrochron::mwMinMax(s, win = p$win, conv = conv, output = TRUE, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::mwMinMax(s, win = %s, conv = %d, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$win), conv))
            sfx <- "-mwMinMax"
          }
          r <- as.data.frame(r)
          res$tab <- r
          sfx <- paste0(sfx, cd_fmt(p$win))
          main <- if (p$method == "minmax") { data.frame(r[, 1], r[, 4]) } else { data.frame(r[, 1], r[, 2]) }
          names(main) <- c(names(s)[1], if (p$method == "minmax") { "range" } else { "mean" })
          o <- list(cd_output(cd_out_name(file, sfx), r, header = TRUE),
              cd_output(cd_out_name(file, paste0(sfx, if (p$method == "minmax") { "-range" } else { "-mean" })), main,
                  var = "main"))
          code <- c(code, sprintf("main <- res[, c(1, %d)]", if (p$method == "minmax") { 4L } else { 2L }))
          if (isTRUE(p$windows)) {
            if (p$method == "grid") {
              wt <- loc
              code <- c(code, sprintf("wins <- astrochron::mwinGrid(s, win = %s, step = %s, verbose = FALSE)", cd_fmt(p$win),
                  cd_fmt(p$step)))
            } else {
              wt <- cd_ab_quiet(astrochron::mwin(s, win = p$win, conv = conv, verbose = FALSE))
              code <- c(code, sprintf("wins <- astrochron::mwin(s, win = %s, conv = %d, verbose = FALSE)", cd_fmt(p$win), conv))
            }
            o[[3]] <- cd_output(cd_out_name(file, paste0(sfx, "-windows")), as.data.frame(wt), header = TRUE, var = "wins")
          }
          code <- c(code, sprintf("CycloDesk::write_series(res, %s, header = TRUE)", cd_dep(o[[1]]$name)),
              sprintf("CycloDesk::write_series(main, %s)", cd_dep(o[[2]]$name)))
          if (length(o) > 2) {
            code <- c(code, sprintf("CycloDesk::write_series(wins, %s, header = TRUE)", cd_dep(o[[3]]$name)))
          }
          list(result = res, outputs = o, code = code,
              text = sprintf("%d windows of %s; %s to %s points per window.", nrow(r), cd_fmt(p$win),
                  min(r[, "Points"]), max(r[, "Points"])))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          grid <- !is.null(res$dens)
          graphics::layout(matrix(seq_len(if (grid) { 3 } else { 2 }), ncol = 1), heights = if (grid) { c(1, 0.8, 1.2) } else { c(1, 1) })
          tb <- res$tab
          xl <- range(res$data[, 1])
          cd_par()
          cd_plot_one(res$data, col = "#b0b8c1", xlab = "", ylab = names(res$data)[2], xlim = xl)
          if (res$method == "minmax") {
            graphics::lines(tb[, 1], tb[, 2], col = "#1f4e79", lwd = 1.5)
            graphics::lines(tb[, 1], tb[, 3], col = "#c0392b", lwd = 1.5)
            graphics::legend("topright", c("minimum", "maximum"), col = c("#1f4e79", "#c0392b"), lwd = 1.5, bty = "n", cex = 0.85)
          } else {
            if (ncol(tb) >= 6) {
              graphics::polygon(c(tb[, 1], rev(tb[, 1])), c(tb[, 2] - tb[, 6], rev(tb[, 2] + tb[, 6])),
                  col = grDevices::adjustcolor("#1f4e79", 0.2), border = NA)
            }
            graphics::lines(tb[, 1], tb[, 2], col = "#1f4e79", lwd = 1.5)
            graphics::lines(tb[, 1], tb[, 3], col = "#c0392b", lwd = 1.5)
            graphics::legend("topright", c("mean", "median"), col = c("#1f4e79", "#c0392b"), lwd = 1.5, bty = "n", cex = 0.85)
          }
          cd_par()
          graphics::plot(tb[, 1], tb[, 4], type = "l", col = "#2e8b57", lwd = 1.5, xlim = xl,
              xlab = if (grid) { "" } else { "Depth / time" }, ylab = if (res$method == "minmax") { "Range" } else { "Variance" })
          graphics::grid(col = "grey90", lty = 1)
          if (grid) {
            cd_par()
            dz <- res$dens
            vw <- cd_view_lims(xl, range(dz$y), last = TRUE)
            graphics::image(dz$x, dz$y, dz$z, col = cd_viridis(100), xlab = "Depth / time", ylab = "Value", xlim = vw$x,
                ylim = vw$y, useRaster = cd_regular(dz$x))
            graphics::lines(tb[, 1], tb[, 3], col = "white", lwd = 2)
            graphics::box()
          }
        }),

    cd_tool("ab_mwcor", M, "Moving-window correlation of two proxies", group = gm, full_table = TRUE, icon = "link",
        info = paste("Correlation coefficient (Pearson, Spearman or Kendall) of two columns of a table in a moving window of",
            "constant length in depth or time."),
        uses = "astrochron::mwCor",
        params = list(
          cd_sel("col1", "First proxy", cd_ab_cols, value = cd_ab_colname(2)),
          cd_sel("col2", "Second proxy", cd_ab_cols, value = cd_ab_colname(3)),
          cd_num("win", "Window length (depth / time units)", cd_ab_len(0.1), min = 0),
          cd_sel("conv", "Window placement", c("Centred on every level" = "1", "Starting at the smallest depth / time" = "2",
              "Starting at the largest depth / time" = "3")),
          cd_sel("cormethod", "Correlation", c("Pearson" = "1", "Spearman" = "2", "Kendall" = "3"))),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          if (ncol(d) < 3) {
            stop("this tool needs a table with at least two proxy columns", call. = FALSE)
          }
          j1 <- cd_ab_col(d, p$col1, 2L)
          j2 <- cd_ab_col(d, p$col2, 3L)
          if (j1 == j2) {
            stop("choose two different columns", call. = FALSE)
          }
          s <- cd_ab_table(d, c(1, j1, j2))
          if (!(p$win > 0) || p$win >= diff(range(s[, 1]))) {
            stop("the window must be positive and shorter than the record", call. = FALSE)
          }
          cm <- as.integer(p$cormethod)
          r <- cd_ab_quiet(astrochron::mwCor(s, cols = c(2, 3), win = p$win, conv = as.integer(p$conv), cormethod = cm,
              output = TRUE, genplot = FALSE, verbose = FALSE))
          r <- as.data.frame(r)
          meth <- c("pearson", "spearman", "kendall")[cm]
          rall <- stats::cor(s[, 2], s[, 3], method = meth)
          o <- list(cd_output(cd_out_name(file, sprintf("-mwCor-%s-%s", gsub("[^A-Za-z0-9_.]", "_", names(s)[2]),
              gsub("[^A-Za-z0-9_.]", "_", names(s)[3]))), r, header = TRUE))
          code <- c(cd_ab_table_code(c(1, j1, j2)),
              sprintf("res <- astrochron::mwCor(s, cols = c(2, 3), win = %s, conv = %d, cormethod = %d, genplot = FALSE, verbose = FALSE)",
                  cd_fmt(p$win), as.integer(p$conv), cm),
              sprintf("CycloDesk::write_series(res, %s, header = TRUE)", cd_dep(o[[1]]$name)))
          list(result = list(data = s, tab = r), outputs = o, code = code,
              text = c(sprintf("Correlation of the whole record (%s): %s.", meth, cd_fmt(signif(rall, 3))),
                  sprintf("Moving-window correlation from %s to %s.", cd_fmt(signif(min(r[, 2], na.rm = TRUE), 3)),
                      cd_fmt(signif(max(r[, 2], na.rm = TRUE), 3)))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          s <- res$data
          z1 <- (s[, 2] - mean(s[, 2])) / stats::sd(s[, 2])
          z2 <- (s[, 3] - mean(s[, 3])) / stats::sd(s[, 3])
          cd_par()
          graphics::plot(s[, 1], z1, type = "l", col = "#1f4e79", ylim = range(c(z1, z2)), xlab = "",
              ylab = "Standardised value")
          graphics::lines(s[, 1], z2, col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          graphics::legend("topright", names(s)[2:3], col = c("#1f4e79", "#c0392b"), lwd = 1, bty = "n", cex = 0.85)
          cd_par()
          tb <- res$tab
          vw <- cd_view_lims(range(s[, 1]), c(-1, 1), x = tb[, 1], y = tb[, 2])
          graphics::plot(tb[, 1], tb[, 2], type = "l", col = "#2e8b57", lwd = 1.5, xlim = vw$x, ylim = vw$y,
              xlab = "Depth / time", ylab = "Correlation")
          graphics::abline(h = 0, col = "grey50", lty = 2)
          graphics::grid(col = "grey90", lty = 1)
        }),

    cd_tool("ab_strats", M, "Summary statistics (strats)", group = gm, icon = "list-check",
        info = "Summary of the sampling interval (and its Nyquist frequency) and of the distribution of the values.",
        uses = "astrochron::strats",
        params = list(cd_sel("plot_dt", "Sampling-interval figure", c("Cumulative sampling intervals" = "1",
            "Histogram of the sampling intervals" = "2"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          txt <- utils::capture.output(r <- astrochron::strats(s, output = 3, genplot = 0))
          txt <- trimws(txt)
          txt <- txt[nzchar(txt) & !grepl("^-----", txt)]
          r <- as.data.frame(r)
          names(r) <- c(names(s)[1], "dt")
          o <- list(cd_output(cd_out_name(file, "-dt"), r))
          code <- c("res <- astrochron::strats(dat, output = 3, genplot = 0)", cd_code_write(o),
              sprintf("astrochron::strats(dat, genplot = %d)", as.integer(p$plot_dt)))
          list(result = list(data = s), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::par(mar = c(4, 4.3, 2.4, 1), mgp = c(2.4, 0.7, 0), cex.axis = 0.9)
          utils::capture.output(astrochron::strats(res$data, output = 0, genplot = as.integer(p$plot_dt)))
        }),

    cd_tool("ab_xplot", M, "Cross-plot with density curves", group = gm, full_table = TRUE, icon = "braille",
        info = paste("Cross-plot of two columns of a table with kernel density estimates of each variable along the axes;",
            "gives the Pearson and Spearman correlation."),
        uses = "astrochron::xplot (figure drawn by CycloDesk)",
        params = list(
          cd_sel("xcol", "x variable", cd_ab_cols, value = cd_ab_colname(2)),
          cd_sel("ycol", "y variable", cd_ab_cols, value = cd_ab_colname(3)),
          cd_chk("fill", "Grey fill of the density curves", TRUE)),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          if (ncol(d) < 3) {
            stop("this tool needs a table with at least two variables", call. = FALSE)
          }
          jx <- cd_ab_col(d, p$xcol, 2L)
          jy <- cd_ab_col(d, p$ycol, 3L)
          tab <- d[stats::complete.cases(d[, c(1, jx, jy)]), c(1, jx, jy)]
          if (nrow(tab) < 3) {
            stop("fewer than 3 complete pairs of values", call. = FALSE)
          }
          rp <- stats::cor(tab[, 2], tab[, 3])
          rs <- stats::cor(tab[, 2], tab[, 3], method = "spearman")
          o <- list(cd_output(cd_out_name(file, sprintf("-xplot-%s-%s", gsub("[^A-Za-z0-9_.]", "_", names(tab)[2]),
              gsub("[^A-Za-z0-9_.]", "_", names(tab)[3]))), tab, header = TRUE, var = "tab"))
          code <- c(sprintf("tab <- dat[complete.cases(dat[, c(1, %d, %d)]), c(1, %d, %d)]", jx, jy, jx, jy),
              sprintf("astrochron::xplot(tab[, 2], tab[, 3], xlab = %s, ylab = %s, fill = %s)", cd_dep(names(tab)[2]),
                  cd_dep(names(tab)[3]), isTRUE(p$fill)),
              "cor(tab[, 2], tab[, 3])", "cor(tab[, 2], tab[, 3], method = \"spearman\")",
              sprintf("CycloDesk::write_series(tab, %s, header = TRUE)", cd_dep(o[[1]]$name)))
          list(result = tab, outputs = o, code = code,
              text = sprintf("%d pairs; Pearson r = %s, Spearman rho = %s.", nrow(tab), cd_fmt(signif(rp, 3)),
                  cd_fmt(signif(rs, 3))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          x <- res[, 2]
          y <- res[, 3]
          graphics::layout(matrix(c(1, 0, 3, 2), 2, byrow = TRUE), widths = c(3, 1), heights = c(1, 3))
          dx <- stats::density(x)
          dy <- stats::density(y)
          fc <- if (isTRUE(p$fill)) { "grey80" } else { NA }
          graphics::par(mar = c(0.5, 4.3, 1, 0.5), las = 1, cex.axis = 0.8)
          graphics::plot(dx$x, dx$y, type = "n", xaxt = "n", xlab = "", ylab = "Density", xlim = range(x), bty = "n")
          graphics::polygon(c(dx$x[1], dx$x, dx$x[length(dx$x)]), c(0, dx$y, 0), col = fc, border = "grey30")
          graphics::par(mar = c(4, 0.5, 0.5, 1))
          graphics::plot(dy$y, dy$x, type = "n", yaxt = "n", ylab = "", xlab = "Density", ylim = range(y), bty = "n")
          graphics::polygon(c(0, dy$y, 0), c(dy$x[1], dy$x, dy$x[length(dy$x)]), col = fc, border = "grey30")
          graphics::par(mar = c(4, 4.3, 0.5, 0.5), mgp = c(2.4, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.9)
          vw <- cd_view_lims(range(x), range(y), x = x, y = y, last = TRUE)
          graphics::plot(x, y, pch = 16, cex = 1.1, col = grDevices::adjustcolor("#1f4e79", 0.35), xlab = names(res)[2],
              ylab = names(res)[3], xlim = vw$x, ylim = vw$y)
          graphics::grid(col = "grey90", lty = 1)
        }),

    cd_tool("ab_pca", M, "Principal components (stratPCA)", group = gm, full_table = TRUE, icon = "diagram-project",
        info = paste("Principal component analysis of all numeric columns of a table (column 1 depth or time), optionally",
            "rotated (varimax or promax); writes the loadings and the scores along the record."),
        uses = "astrochron::stratPCA",
        params = list(
          cd_sel("rot", "Rotation", c("None" = "0", "Varimax (orthogonal)" = "1", "Promax (oblique)" = "2")),
          cd_num("nPC", "Number of components (0 = all)", 0, min = 0, step = 1),
          cd_chk("standardise", "Standardise the variables first (scores of standardised data)", TRUE,
              help = "stratPCA uses the correlation matrix but multiplies the loadings with the raw values for the scores.")),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          num <- integer()
          for (j in seq_len(ncol(d))) {
            if (is.numeric(d[[j]])) {
              num <- c(num, j)
            }
          }
          if (length(num) < 3 || num[1] != 1L) {
            stop("PCA needs a numeric depth column and at least two numeric variables", call. = FALSE)
          }
          s <- d[, num]
          s <- s[stats::complete.cases(s), , drop = FALSE]
          kk <- 1L
          for (j in 2:ncol(s)) {
            if (stats::sd(s[, j]) > 0) {
              kk <- c(kk, j)
            }
          }
          s <- s[, kk, drop = FALSE]
          nv <- ncol(s) - 1L
          if (nv < 2) {
            stop("fewer than two variables that are not constant", call. = FALSE)
          }
          code <- c(sprintf("s <- dat[, %s]", cd_dep(as.integer(num))), "s <- s[complete.cases(s), ]",
              sprintf("s <- s[, %s]", cd_dep(as.integer(kk))))
          if (isTRUE(p$standardise)) {
            for (j in 2:ncol(s)) {
              s[, j] <- (s[, j] - mean(s[, j])) / stats::sd(s[, j])
            }
            code <- c(code, "s[, -1] <- scale(s[, -1])")
          }
          eig <- eigen(stats::cor(s[, -1]), only.values = TRUE)$values
          # with more variables than independent levels the last eigenvalues are numerically zero or negative, and
          # stratPCA takes their square root: keep only the components with a positive eigenvalue
          npos <- sum(eig > 1e-9 * eig[1])
          npc <- as.integer(p$nPC)
          if (is.na(npc) || npc < 1 || npc > npos) {
            npc <- npos
          }
          rot <- as.integer(p$rot)
          if (rot > 0 && npc < 2) {
            stop("a rotation needs at least two components", call. = FALSE)
          }
          nan_ok <- function(w) {
            if (grepl("NaN", conditionMessage(w), fixed = TRUE)) {
              invokeRestart("muffleWarning")
            }
          }
          ld <- withCallingHandlers(cd_ab_quiet(astrochron::stratPCA(s, id = TRUE, rot = rot, nPC = npc, output = 1,
              genplot = 0)), warning = nan_ok)
          sc <- withCallingHandlers(cd_ab_quiet(astrochron::stratPCA(s, id = TRUE, rot = rot, nPC = npc, output = 2,
              genplot = 0)), warning = nan_ok)
          ld <- matrix(as.numeric(unclass(as.matrix(ld))), nrow = nv)
          ld <- ld[, seq_len(min(npc, ncol(ld))), drop = FALSE]
          sc <- as.data.frame(sc)
          sc <- sc[, seq_len(min(npc, ncol(sc) - 1L) + 1L), drop = FALSE]
          pcs <- paste0("PC", seq_len(ncol(ld)))
          loads <- data.frame(variable = names(s)[-1], ld)
          names(loads) <- c("variable", pcs)
          names(sc) <- c(names(s)[1], pcs[seq_len(ncol(sc) - 1)])
          eig <- eig[seq_len(npos)]
          pv <- 100 * eig / sum(eig)
          o <- list(cd_output(cd_out_name(file, "-PCA-loadings"), loads, header = TRUE, var = "loads"),
              cd_output(cd_out_name(file, "-PCA-scores"), sc, header = TRUE, var = "scores"))
          code <- c(code, sprintf("loads <- astrochron::stratPCA(s, id = TRUE, rot = %d, nPC = %d, output = 1, genplot = 0)",
              rot, npc), sprintf("scores <- astrochron::stratPCA(s, id = TRUE, rot = %d, nPC = %d, output = 2, genplot = 0)",
              rot, npc), "loads <- data.frame(variable = rownames(loads), unclass(loads))",
              sprintf("loads <- loads[, 1:%d]", ncol(loads)), sprintf("scores <- scores[, 1:%d]", ncol(sc)),
              sprintf("CycloDesk::write_series(loads, %s, header = TRUE)", cd_dep(o[[1]]$name)),
              sprintf("CycloDesk::write_series(scores, %s, header = TRUE)", cd_dep(o[[2]]$name)))
          k <- min(3L, length(pv))
          txt <- c(sprintf("%d variables, %d levels; %d components%s.", nv, nrow(s), npc,
              c("", ", varimax rotation", ", promax rotation")[rot + 1]),
              sprintf("Variance explained by PC1-PC%d: %s %%.", k, paste(cd_fmt_period(pv[seq_len(k)]), collapse = ", ")))
          if (npos < nv) {
            txt <- c(txt, sprintf("Only %d of the %d eigenvalues are positive (the variables are not independent).", npos, nv))
          }
          list(result = list(loads = loads, scores = sc, eig = eig), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          sc <- res$scores
          npl <- min(2L, ncol(sc) - 1L)
          graphics::layout(matrix(if (npl == 2) { c(1, 2, 3, 4) } else { c(1, 2, 3, 3) }, 2, byrow = TRUE))
          eig <- res$eig
          cd_par()
          graphics::plot(seq_along(eig), eig, type = "b", pch = 16, col = "#1f4e79", xlab = "Principal component",
              ylab = "Eigenvalue", main = "Scree plot")
          graphics::abline(h = 1, lty = 2, col = "grey50")
          graphics::grid(col = "grey90", lty = 1)
          ld <- as.matrix(res$loads[, -1, drop = FALSE])
          ld <- ld[, seq_len(min(6L, ncol(ld))), drop = FALSE]
          nv <- nrow(ld)
          cd_par()
          graphics::par(mar = c(4, 7, 1.6, 1))
          graphics::image(seq_len(ncol(ld)), seq_len(nv), t(ld), col = grDevices::hcl.colors(100, "Blue-Red 3"),
              zlim = max(abs(ld), na.rm = TRUE) * c(-1, 1), xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "Loadings")
          graphics::axis(1, at = seq_len(ncol(ld)), labels = colnames(ld))
          graphics::axis(2, at = seq_len(nv), labels = res$loads$variable, cex.axis = max(0.4, min(0.9, 12 / nv)), las = 1)
          graphics::box()
          o <- order(sc[, 1])
          for (k in seq_len(npl)) {
            cd_par()
            cd_plot_one(sc[o, c(1, k + 1)], col = cd_cols(2)[k], xlab = names(sc)[1], ylab = paste(names(sc)[k + 1], "score"))
          }
        }),

    cd_tool("ab_accum", M, "Accumulation rates (accum)", group = gm, icon = "layer-group",
        info = paste("Accumulation rate of a component (concentration in weight %) from a constant linear sedimentation",
            "rate and dry bulk density: component, bulk and linear accumulation rates."),
        uses = "astrochron::accum",
        params = list(
          cd_num("sedrate", "Sedimentation rate (cm/kyr)", 2, min = 0),
          cd_num("density", "Dry bulk density (g/cm3)", 2.5, min = 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          if (!(p$sedrate > 0) || !(p$density > 0)) {
            stop("the sedimentation rate and density must be positive", call. = FALSE)
          }
          r <- cd_ab_quiet(astrochron::accum(s, sedrate = p$sedrate, density = p$density, output = 2, genplot = FALSE,
              verbose = FALSE))
          r <- as.data.frame(r)
          ar <- data.frame(r[, 1], r[, 2])
          names(ar) <- c(names(s)[1], "accumulation_g_cm2_kyr")
          o <- list(cd_output(cd_out_name(file, "-accum"), ar, var = "ar"),
              cd_output(cd_out_name(file, "-accum-table"), r, header = TRUE))
          code <- c(sprintf("res <- astrochron::accum(dat, sedrate = %s, density = %s, output = 2, genplot = FALSE, verbose = FALSE)",
              cd_fmt(p$sedrate), cd_fmt(p$density)), "ar <- res[, 1:2]", cd_code_write(o[1]),
              sprintf("CycloDesk::write_series(res, %s, header = TRUE)", cd_dep(o[[2]]$name)))
          txt <- sprintf("Bulk accumulation rate %s g/cm2/kyr; component accumulation rate %s to %s g/cm2/kyr.",
              cd_fmt(signif(r[1, 5], 4)), cd_fmt(signif(min(r[, 2]), 4)), cd_fmt(signif(max(r[, 2]), 4)))
          if (any(s[, 2] < 0 | s[, 2] > 100)) {
            txt <- c(txt, "Note: some values are outside 0-100; the values should be concentrations in weight %.")
          }
          list(result = list(before = s, after = ar), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "Concentration (wt %)", "Accumulation (g/cm2/kyr)")
        }),

    cd_tool("ab_rank", M, "Rank series from bed thicknesses", group = gm, full_table = TRUE, icon = "stairs",
        info = paste("Builds a lithofacies rank series from a bed table in stratigraphic order (column 1 bed thickness,",
            "another column the facies rank of the bed), sampled at an even step."),
        uses = "astrochron::rankSeries",
        params = list(
          cd_sel("col", "Rank column", cd_ab_cols, value = cd_ab_colname(2)),
          cd_num("dt", "Sampling step (0 = no interpolation)", function(d) {
            th <- d[, 1]
            signif(min(th[th > 0], na.rm = TRUE) / 5, 3)
          }, min = 0),
          cd_num("start", "Height of the base of the first bed", 0)),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          j <- cd_ab_col(d, p$col)
          beds <- d[stats::complete.cases(d[, c(1, j)]), c(1, j)]
          if (nrow(beds) < 2) {
            stop("the bed table needs at least two beds", call. = FALSE)
          }
          if (any(beds[, 1] <= 0)) {
            stop("the bed thicknesses (column 1) must be positive", call. = FALSE)
          }
          r <- as.data.frame(cd_ab_quiet(astrochron::rankSeries(beds, dt = p$dt, start = p$start, genplot = FALSE,
              verbose = FALSE)))
          names(r) <- c("height", "rank")
          o <- list(cd_output(cd_out_name(file, "-rank"), r))
          code <- c(sprintf("beds <- dat[complete.cases(dat[, c(1, %d)]), c(1, %d)]", j, j),
              sprintf("res <- astrochron::rankSeries(beds, dt = %s, start = %s, genplot = FALSE, verbose = FALSE)",
                  cd_fmt(p$dt), cd_fmt(p$start)), cd_code_write(o))
          base <- p$start + c(0, cumsum(beds[, 1]))[seq_len(nrow(beds))]
          list(result = list(series = r, beds = data.frame(base = base, rank = beds[, 2])), outputs = o, code = code,
              text = sprintf("%d beds, %s thick in total; %d points.", nrow(beds), cd_fmt(signif(sum(beds[, 1]), 5)), nrow(r)))
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          cd_plot_one(res$series, col = "#1f4e79", xlab = "Stratigraphic height", ylab = "Bed rank")
          graphics::points(res$beds$base, res$beds$rank, pch = 1, col = "#c0392b", cex = 0.8)
        })
  )
}

# Time Series menu ####

cd_ab_tools_timeseries <- function() {
  list(
    cd_tool("ab_lowpass", "Time Series", "Lowpass, highpass and kernel detrending", group = "Pre-processing (astrochron)",
        icon = "filter",
        info = paste("FFT lowpass filter with a rectangular, Gaussian or cosine-tapered window, the matching highpass",
            "(series minus lowpass), or removal of a Gaussian-kernel trend."),
        uses = "astrochron::lowpass, astrochron::noKernel",
        params = list(
          cd_sel("method", "Method", c("Lowpass filter (lowpass)" = "lowpass", "Highpass: series minus lowpass (lowpass)" = "highpass",
              "Remove a Gaussian-kernel trend (noKernel)" = "kernel")),
          cd_num("fcut", "Cut-off frequency (cycles per unit)", function(d) { signif(10 / diff(range(d[, 1], na.rm = TRUE)), 3) },
              min = 0, show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_sel("win", "Window", c("Rectangular" = "0", "Gaussian" = "1", "Cosine-tapered" = "2"),
              show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_num("alpha", "Gaussian window: alpha (>= 2.5)", 3, min = 0.5, step = 0.5,
              show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_num("p", "Cosine-tapered window: fraction tapered", 0.25, min = 0, max = 1, step = 0.05,
              show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_num("padfac", "Zero padding factor", 2, min = 1, step = 1, show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_chk("detrend", "Remove a linear trend before filtering", FALSE,
              show_if = list(param = "method", values = c("lowpass", "highpass"))),
          cd_num("smooth", "Kernel smoothing (fraction of the record, > 0)", 0.1, min = 0.001, max = 1, step = 0.05,
              show_if = list(param = "method", values = "kernel")),
          cd_chk("write_trend", "Also write the lowpass / kernel trend", FALSE)),
        compute = function(d, p, d2, file, file2) {
          code <- character()
          txt <- character()
          if (p$method == "kernel") {
            s <- cd_clean(d)
            names(s) <- names(d)[1:2]
            if (!(p$smooth > 0)) {
              stop("the smoothing must be > 0", call. = FALSE)
            }
            res <- cd_ab_quiet(astrochron::noKernel(s, smooth = p$smooth, sort = FALSE, output = 1, genplot = FALSE,
                verbose = FALSE))
            tr <- cd_ab_quiet(astrochron::noKernel(s, smooth = p$smooth, sort = FALSE, output = 2, genplot = FALSE,
                verbose = FALSE))
            code <- c(sprintf("res <- astrochron::noKernel(dat, smooth = %s, output = 1, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$smooth)), sprintf("trend <- astrochron::noKernel(dat, smooth = %s, output = 2, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$smooth)))
            sfx <- paste0("-rKernel", cd_fmt(p$smooth))
            tsfx <- paste0("-kernel", cd_fmt(p$smooth))
          } else {
            s <- cd_even(d)
            names(s) <- names(d)[1:2]
            txt <- cd_even_note(d, s)
            code <- cd_even_code(d)
            nyq <- 0.5 / cd_dt(s[, 1])
            if (!(p$fcut > 0) || p$fcut >= nyq) {
              stop(sprintf("the cut-off frequency must be between 0 and the Nyquist frequency (%s)", cd_fmt(signif(nyq, 4))),
                  call. = FALSE)
            }
            tr <- cd_ab_quiet(astrochron::lowpass(s, padfac = p$padfac, fcut = p$fcut, win = as.integer(p$win), demean = TRUE,
                detrend = isTRUE(p$detrend), addmean = TRUE, alpha = p$alpha, p = p$p, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf(paste0("trend <- astrochron::lowpass(dat, padfac = %s, fcut = %s, win = %d, detrend = %s,\n",
                "    alpha = %s, p = %s, genplot = FALSE, verbose = FALSE)"), cd_fmt(p$padfac), cd_fmt(p$fcut), as.integer(p$win),
                isTRUE(p$detrend), cd_fmt(p$alpha), cd_fmt(p$p)))
            if (p$method == "lowpass") {
              res <- tr
              code <- c(code, "res <- trend")
              sfx <- paste0("-lowpass", cd_fmt(p$fcut))
            } else {
              res <- data.frame(s[, 1], s[, 2] - tr[, 2])
              code <- c(code, "res <- data.frame(dat[, 1], dat[, 2] - trend[, 2])")
              sfx <- paste0("-highpass", cd_fmt(p$fcut))
            }
            tsfx <- paste0("-lowpass", cd_fmt(p$fcut))
          }
          res <- as.data.frame(res)
          tr <- as.data.frame(tr)
          names(res) <- names(s)
          names(tr) <- c(names(s)[1], "trend")
          o <- list(cd_output(cd_out_name(file, sfx), res))
          if (isTRUE(p$write_trend) && p$method != "lowpass") {
            o[[2]] <- cd_output(cd_out_name(file, tsfx), tr, var = "trend")
          }
          list(result = list(before = s, trend = tr, after = res, method = p$method), outputs = o, text = txt,
              code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          lab <- c(lowpass = "lowpass", highpass = "highpass", kernel = "kernel-detrended")[[res$method]]
          cd_two_panel(res$before, res$after, "input", lab, extra_top = res$trend,
              extra_lab = if (res$method == "kernel") { "kernel trend" } else { "lowpass" })
        })
  )
}

# Basic Series menu ####

cd_ab_tools_models <- function() {
  B <- "Basic Series"
  gb <- "Models and insolation (astrochron)"
  lon_choices <- c("June solstice (90)" = "90", "September equinox (180)" = "180", "December solstice (270)" = "270",
      "March equinox (0)" = "0")
  list(
    cd_tool("ab_synth", B, "Synthetic series: cycles, ARMA, ETP + AR(1)", group = gb, inputs = 0L, icon = "shuffle",
        info = paste("Sum of sinusoids with white noise, an autoregressive moving-average (ARMA) series, or an ETP signal",
            "(La2004) plus AR(1) red noise; shows the MTM spectrum with AR(1) confidence levels."),
        uses = "astrochron::cycles / armaGen / ar1etp, astrochron::etp, astrochron::mtm",
        params = list(
          cd_sel("method", "Model", c("Sum of sinusoids plus white noise (cycles)" = "cycles", "ARMA model (armaGen)" = "arma",
              "ETP plus AR(1) noise (ar1etp)" = "ar1etp")),
          cd_txt("periods", "Periods (comma separated)", "405, 125, 95, 41, 23.7, 22.4, 19", numeric = TRUE,
              show_if = list(param = "method", values = "cycles")),
          cd_txt("amps", "Amplitudes (one per period)", "1, 0.6, 0.5, 0.7, 0.8, 0.6, 0.5", numeric = TRUE,
              show_if = list(param = "method", values = "cycles")),
          cd_txt("phases", "Phases in radians (one per period, empty = 0)", "", numeric = TRUE,
              show_if = list(param = "method", values = "cycles")),
          cd_num("noisevar", "Variance of the white noise", 0.1, min = 0, show_if = list(param = "method", values = "cycles")),
          cd_num("start", "Start (ka for ETP)", 0, show_if = list(param = "method", values = c("cycles", "ar1etp"))),
          cd_num("end", "End (ka for ETP)", 1000, show_if = list(param = "method", values = c("cycles", "ar1etp"))),
          cd_num("npts", "Number of points", 1000, min = 10, step = 1, show_if = list(param = "method", values = "arma")),
          cd_num("dt", "Sampling interval", 1, min = 0),
          cd_num("m", "Mean", 0, show_if = list(param = "method", values = "arma")),
          cd_num("std", "Standard deviation", 1, min = 0, show_if = list(param = "method", values = "arma")),
          cd_txt("rhos", "AR coefficients (one per order)", "0.9", numeric = TRUE, show_if = list(param = "method", values = "arma")),
          cd_txt("thetas", "MA coefficients (one per order)", "0", numeric = TRUE, show_if = list(param = "method", values = "arma")),
          cd_num("rho", "AR(1) coefficient of the noise", 0.9, min = 0, max = 0.999, step = 0.05,
              show_if = list(param = "method", values = "ar1etp")),
          cd_num("wtAR", "Noise amplitude relative to ETP (1 = equal variance, < 0 = noise only)", 1, step = 0.1,
              show_if = list(param = "method", values = "ar1etp")),
          cd_num("seed", "Random seed", 1, step = 1),
          cd_txt("name", "File name", "synthetic_astrochron")),
        compute = function(d, p, d2, file, file2) {
          if (!(p$dt > 0)) {
            stop("the sampling interval must be positive", call. = FALSE)
          }
          seed <- as.integer(p$seed)
          set.seed(seed)
          code <- sprintf("set.seed(%d)", seed)
          if (p$method == "cycles") {
            per <- p$periods
            if (!length(per) || any(per <= 0)) {
              stop("give one or more positive periods", call. = FALSE)
            }
            amp <- rep(if (length(p$amps)) { p$amps } else { 1 }, length.out = length(per))
            ph <- rep(if (length(p$phases)) { p$phases } else { 0 }, length.out = length(per))
            if (!(p$end > p$start)) {
              stop("the end must be larger than the start", call. = FALSE)
            }
            res <- cd_ab_quiet(astrochron::cycles(freqs = 1 / per, phase = ph, amp = amp, start = p$start, end = p$end,
                dt = p$dt, noisevar = p$noisevar, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf(paste0("res <- astrochron::cycles(freqs = 1 / %s, phase = %s, amp = %s, start = %s, end = %s,\n",
                "    dt = %s, noisevar = %s, genplot = FALSE, verbose = FALSE)"), cd_dep(per), cd_dep(ph), cd_dep(amp),
                cd_fmt(p$start), cd_fmt(p$end), cd_fmt(p$dt), cd_fmt(p$noisevar)))
          } else if (p$method == "arma") {
            rhos <- if (length(p$rhos)) { p$rhos } else { 0 }
            thetas <- if (length(p$thetas)) { p$thetas } else { 0 }
            res <- cd_ab_quiet(astrochron::armaGen(npts = as.integer(p$npts), dt = p$dt, m = p$m, std = p$std, rhos = rhos,
                thetas = thetas, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::armaGen(npts = %d, dt = %s, m = %s, std = %s, rhos = %s, thetas = %s, genplot = FALSE, verbose = FALSE)",
                as.integer(p$npts), cd_fmt(p$dt), cd_fmt(p$m), cd_fmt(p$std), cd_dep(rhos), cd_dep(thetas)))
          } else {
            if (!(p$end > p$start) || p$start < 0) {
              stop("the ETP interval must be within 0 ka and later and the end larger than the start", call. = FALSE)
            }
            e <- cd_ab_quiet(astrochron::etp(tmin = p$start, tmax = p$end, dt = p$dt, solution = cd_laskar("la04"),
                genplot = FALSE, verbose = FALSE))
            e <- as.data.frame(e)
            res <- cd_ab_quiet(astrochron::ar1etp(etpdat = e, nsim = 1, rho = p$rho, wtAR = p$wtAR), device = TRUE)
            res <- as.data.frame(res)
            res[, 1] <- e[, 1]
            code <- c(code, sprintf("etpdat <- astrochron::etp(tmin = %s, tmax = %s, dt = %s, solution = astrochron::getLaskar(\"la04\"), genplot = FALSE)",
                cd_fmt(p$start), cd_fmt(p$end), cd_fmt(p$dt)),
                sprintf("res <- astrochron::ar1etp(etpdat = etpdat, nsim = 1, rho = %s, wtAR = %s)", cd_fmt(p$rho), cd_fmt(p$wtAR)),
                "res[, 1] <- etpdat[, 1]")
          }
          res <- as.data.frame(res)
          names(res) <- c("x", "y")
          sp <- cd_ab_quiet(astrochron::mtm(res, tbw = 2, ar1 = TRUE, output = 1, genplot = FALSE, verbose = FALSE))
          sp <- as.data.frame(sp)
          nm <- paste0(gsub("[^A-Za-z0-9_.-]", "_", p$name), ".txt")
          o <- list(cd_output(nm, res))
          list(result = list(series = res, spec = sp[, c("Frequency", "Power", "AR1_90_power", "AR1_95_power", "AR1_99_power")]),
              outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points; mean %s, standard deviation %s, lag-1 autocorrelation %s.", nrow(res),
                  cd_fmt(signif(mean(res$y), 3)), cd_fmt(signif(stats::sd(res$y), 3)),
                  cd_fmt(signif(stats::acf(res$y, lag.max = 1, plot = FALSE)$acf[2], 3))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$series, col = "#1f4e79", xlab = "Depth / time", ylab = "Synthetic series")
          sp <- res$spec
          cd_plot_spectrum(sp[, 1], sp[, 2], cl = sp[, 3:5], cl_names = c("AR(1) 90%", "AR(1) 95%", "AR(1) 99%"))
        }),

    cd_tool("ab_insoseries", B, "Insolation: daily, caloric, integrated, threshold", group = gb, inputs = 0L, icon = "sun",
        heavy = function(p) { isTRUE(p$opt %in% c("2", "4")) || isTRUE((p$tmax - p$tmin) / p$dt > 3000) },
        info = paste("Insolation series from La2004 computed by the palinsol package: daily mean insolation on a day,",
            "caloric summer insolation (Milankovitch), insolation integrated between two true solar longitudes, or",
            "integrated over the days above a threshold (Huybers and Tziperman 2008)."),
        uses = "astrochron::insoSeries (palinsol)",
        params = list(
          cd_sel("opt", "Quantity", c("Daily mean insolation (W/m2)" = "1", "Caloric summer insolation (kJ/m2)" = "2",
              "Insolation integrated between two true solar longitudes (kJ/m2)" = "3",
              "Integrated insolation above a threshold (kJ/m2)" = "4")),
          cd_num("lat", "Latitude (degrees N)", 65, min = -90, max = 90),
          cd_num("long", "True solar longitude (degrees; 90 = 21 June)", 90, min = 0, max = 360,
              show_if = list(param = "opt", values = "1")),
          cd_num("l1", "From true solar longitude (degrees)", 0, min = 0, max = 360, show_if = list(param = "opt", values = "3")),
          cd_num("l2", "To true solar longitude (degrees)", 70, min = 0, max = 360, show_if = list(param = "opt", values = "3")),
          cd_num("threshold", "Threshold (W/m2)", 400, min = 0, show_if = list(param = "opt", values = "4")),
          cd_num("S0", "Solar constant (W/m2)", 1365),
          cd_num("tmin", "Start (ka; negative = future)", 0, min = -21000, max = 51000),
          cd_num("tmax", "End (ka)", 1000, min = -21000, max = 51000),
          cd_num("dt", "Step (ka, >= 1)", 1, min = 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          if (!(p$tmax > p$tmin)) {
            stop("the end must be larger than the start", call. = FALSE)
          }
          if (p$dt < 1) {
            stop("the step must be 1 ka or more", call. = FALSE)
          }
          opt <- as.integer(p$opt)
          r <- cd_ab_quiet(astrochron::insoSeries(tmin = p$tmin, tmax = p$tmax, dt = p$dt, opt = opt, long = p$long,
              lat = p$lat, threshold = p$threshold, l1 = p$l1, l2 = p$l2, S0 = p$S0, genplot = FALSE, verbose = FALSE))
          r <- as.data.frame(r)
          r <- r[order(r[, 1]), , drop = FALSE]
          rownames(r) <- NULL
          what <- c("daily", "caloric", "integrated", "threshold")[opt]
          names(r) <- c("time_ka", what)
          tag <- switch(what, daily = sprintf("_lon%s", cd_fmt(p$long)), caloric = "",
              integrated = sprintf("_lon%s-%s", cd_fmt(p$l1), cd_fmt(p$l2)), threshold = sprintf("_thr%s", cd_fmt(p$threshold)))
          nm <- sprintf("Insolation_%s_%s%s%s_%s-%ska.txt", what, cd_fmt(abs(p$lat)), if (p$lat >= 0) { "N" } else { "S" }, tag,
              cd_fmt(p$tmin), cd_fmt(p$tmax))
          o <- list(cd_output(nm, r))
          code <- c(sprintf(paste0("res <- astrochron::insoSeries(tmin = %s, tmax = %s, dt = %s, opt = %d, long = %s, lat = %s,\n",
              "    threshold = %s, l1 = %s, l2 = %s, S0 = %s, genplot = FALSE, verbose = FALSE)"), cd_fmt(p$tmin), cd_fmt(p$tmax),
              cd_fmt(p$dt), opt, cd_fmt(p$long), cd_fmt(p$lat), cd_fmt(p$threshold), cd_fmt(p$l1), cd_fmt(p$l2), cd_fmt(p$S0)),
              "res <- res[order(res[, 1]), ]", cd_code_write(o))
          unit <- if (opt == 1) { "W/m2" } else { "kJ/m2" }
          list(result = list(series = r, unit = unit), outputs = o, code = code,
              text = sprintf("%d points; mean %s %s, range %s to %s %s.", nrow(r), cd_fmt(signif(mean(r[, 2]), 5)), unit,
                  cd_fmt(signif(min(r[, 2]), 5)), cd_fmt(signif(max(r[, 2]), 5)), unit))
        },
        plot = function(res, p) {
          cd_plot_series(list(res$series), labels = sprintf("%s insolation (%s)", names(res$series)[2], res$unit),
              xlab = "Time (ka)")
        }),

    cd_tool("ab_insomap", B, "Insolation map: latitude and season", group = gb, inputs = 0L, icon = "earth-europe",
        info = paste("Daily mean insolation for every latitude and true solar longitude at one time, or the difference",
            "between two times (La2004, palinsol), with the seasonal cycle at one latitude and the latitude profile on",
            "one day."),
        uses = "astrochron::insoMap, astrochron::insoDiff (palinsol)",
        params = list(
          cd_sel("method", "Map", c("Insolation at one time (insoMap)" = "map", "Difference between two times (insoDiff)" = "diff")),
          cd_num("t", "Time (ka; negative = future)", 0, min = -21000, max = 51000),
          cd_num("t2", "Second time (ka)", 11, min = -21000, max = 51000, show_if = list(param = "method", values = "diff")),
          cd_num("lat_pick", "Seasonal cycle at latitude (degrees N)", 65, min = -90, max = 90),
          cd_sel("long_pick", "Latitude profile on", lon_choices),
          cd_num("S0", "Solar constant (W/m2)", 1365)),
        compute = function(d, p, d2, file, file2) {
          m1 <- cd_ab_insomap(p$t, p$S0)
          z <- m1$z
          code <- sprintf("m1 <- astrochron::insoMap(t = %s, S0 = %s, output = TRUE, verbose = FALSE)", cd_fmt(p$t), cd_fmt(p$S0))
          res <- list(long = m1$long, lat = m1$lat, z1 = m1$z, t = p$t, method = p$method)
          if (p$method == "diff") {
            m2 <- cd_ab_insomap(p$t2, p$S0)
            z <- m1$z - m2$z
            res$z2 <- m2$z
            res$t2 <- p$t2
            code <- c(code, sprintf("m2 <- astrochron::insoMap(t = %s, S0 = %s, output = TRUE, verbose = FALSE)", cd_fmt(p$t2),
                cd_fmt(p$S0)), "z <- unclass(m1) - unclass(m2)")
            tag <- sprintf("diff_%s-%ska", cd_fmt(p$t), cd_fmt(p$t2))
          } else {
            code <- c(code, "z <- unclass(m1)")
            tag <- sprintf("%ska", cd_fmt(p$t))
          }
          res$z <- z
          il <- which.min(abs(m1$lat - p$lat_pick))
          ik <- which.min(abs(m1$long - as.numeric(p$long_pick)))
          season <- data.frame(true_solar_longitude = m1$long, insolation = z[, il])
          prof <- data.frame(latitude = m1$lat, insolation = z[ik, ])
          res$season <- season
          res$prof <- prof
          res$lat_used <- m1$lat[il]
          res$long_used <- m1$long[ik]
          tab <- cd_ab_map_table(m1$long, m1$lat, z)
          o <- list(cd_output(sprintf("Insolation_map_%s.txt", tag), tab, header = TRUE, var = "tab"),
              cd_output(sprintf("Insolation_%s_lat%s_season.txt", tag, cd_fmt(m1$lat[il])), season, var = "season"),
              cd_output(sprintf("Insolation_%s_lon%s_latitudes.txt", tag, cd_fmt(m1$long[ik])), prof, var = "prof"))
          code <- c(code, "long <- attr(m1, \"long\")", "lat <- attr(m1, \"lat\")",
              "tab <- data.frame(true_solar_longitude = long, matrix(z, nrow = length(long)))",
              "names(tab)[-1] <- paste0(\"lat_\", lat)",
              sprintf("season <- data.frame(long, z[, which.min(abs(lat - %s))])", cd_fmt(p$lat_pick)),
              sprintf("prof <- data.frame(lat, z[which.min(abs(long - %s)), ])", cd_fmt(as.numeric(p$long_pick))),
              sprintf("CycloDesk::write_series(tab, %s, header = TRUE)", cd_dep(o[[1]]$name)), cd_code_write(o[-1]))
          txt <- sprintf("%s: %s to %s W/m2.", if (p$method == "diff") { sprintf("Difference %s - %s ka", cd_fmt(p$t), cd_fmt(p$t2)) } else {
            sprintf("Insolation at %s ka", cd_fmt(p$t)) }, cd_fmt(signif(min(z), 4)), cd_fmt(signif(max(z), 4)))
          list(result = res, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          dif <- identical(res$method, "diff")
          if (dif) {
            graphics::layout(matrix(c(1, 2, 5, 3, 4, 5), 2, byrow = TRUE), widths = c(1, 1, 1.7))
            cd_ab_map(res$long, res$lat, res$z1, sprintf("%s ka (W/m2)", cd_fmt(res$t)))
            cd_ab_map(res$long, res$lat, res$z2, sprintf("%s ka (W/m2)", cd_fmt(res$t2)))
          } else {
            graphics::layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 1.3))
          }
          ylab <- if (dif) { "Difference (W/m2)" } else { "Insolation (W/m2)" }
          cd_par()
          graphics::plot(res$season[, 1], res$season[, 2], type = "l", col = "#c0392b", lwd = 1.5,
              xlab = "True solar longitude (degrees)", ylab = ylab, main = sprintf("Latitude %s", cd_fmt(res$lat_used)))
          graphics::abline(v = c(90, 180, 270), col = "grey70", lty = 3)
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          graphics::plot(res$prof[, 2], res$prof[, 1], type = "l", col = "#1f4e79", lwd = 1.5, xlab = ylab,
              ylab = "Latitude (degrees)", main = sprintf("True solar longitude %s", cd_fmt(res$long_used)))
          graphics::grid(col = "grey90", lty = 1)
          main <- if (dif) { sprintf("%s - %s ka (W/m2)", cd_fmt(res$t), cd_fmt(res$t2)) } else { sprintf("%s ka (W/m2)", cd_fmt(res$t)) }
          cd_ab_map(res$long, res$lat, res$z, main, diverging = dif, last = TRUE)
        }),

    cd_tool("ab_imbrie", B, "Ice-volume model (Imbrie and Imbrie 1980)", group = gb, inputs = 0L, icon = "snowflake",
        heavy = function(p) { isTRUE((p$tmax - p$tmin) / p$dt > 3000) },
        info = paste("Ice volume from the Imbrie and Imbrie (1980) model forced by daily insolation (La2004, palinsol),",
            "with one or several time constants and non-linearity coefficients."),
        uses = "astrochron::imbrie, astrochron::insoSeries",
        params = list(
          cd_num("lat", "Insolation latitude (degrees N)", 65, min = -90, max = 90),
          cd_sel("long", "Insolation on", lon_choices),
          cd_num("tmin", "Start (ka)", 0, min = -21000, max = 51000),
          cd_num("tmax", "End (ka)", 1000, min = -21000, max = 51000),
          cd_num("dt", "Step (ka, >= 1)", 1, min = 1, step = 1),
          cd_txt("Tm", "Mean time constant(s) Tm (ka)", "17", numeric = TRUE),
          cd_txt("b", "Non-linearity coefficient(s) b (0-1)", "0.6", numeric = TRUE),
          cd_txt("times", "Start times of each Tm and b (ka; empty = one regime)", "", numeric = TRUE,
              help = "e.g. Tm = 15, 5; b = 0.6, 0.3; times = 500, 1000 uses Tm 15 and b 0.6 from 0 to 500 ka."),
          cd_num("burnin", "Burn-in (points)", 100, min = 1, step = 10),
          cd_num("initial", "Initial ice volume", 0),
          cd_chk("standardize", "Scale the ice volume to 0-1", TRUE),
          cd_num("S0", "Solar constant (W/m2)", 1365)),
        compute = function(d, p, d2, file, file2) {
          if (!(p$tmax > p$tmin) || p$dt < 1) {
            stop("the end must be larger than the start and the step 1 ka or more", call. = FALSE)
          }
          n <- floor((p$tmax - p$tmin) / p$dt) + 1
          if (p$burnin >= n - 10) {
            stop("the burn-in must be much shorter than the series", call. = FALSE)
          }
          Tm <- p$Tm
          b <- p$b
          times <- p$times
          if (!length(Tm) || !length(b)) {
            stop("give Tm and b", call. = FALSE)
          }
          if (length(times)) {
            if (length(Tm) != length(times) || length(b) != length(times)) {
              stop("give one Tm and one b for every start time", call. = FALSE)
            }
            if (max(times) < p$tmax) {
              stop("the largest start time must be at least the end of the series", call. = FALSE)
            }
          } else if (length(Tm) != 1 || length(b) != 1) {
            stop("several Tm and b values need start times", call. = FALSE)
          }
          ins <- cd_ab_quiet(astrochron::insoSeries(tmin = p$tmin, tmax = p$tmax, dt = p$dt, opt = 1, long = as.numeric(p$long),
              lat = p$lat, S0 = p$S0, genplot = FALSE, verbose = FALSE))
          ins <- as.data.frame(ins)
          ins <- ins[order(ins[, 1]), , drop = FALSE]
          rownames(ins) <- NULL
          names(ins) <- c("time_ka", "insolation")
          ice <- cd_ab_quiet(astrochron::imbrie(insolation = ins, Tm = Tm, b = b, times = if (length(times)) { times } else { NULL },
              initial = p$initial, burnin = as.integer(p$burnin), standardize = isTRUE(p$standardize), output = TRUE,
              genplot = 0, verbose = FALSE))
          ice <- as.data.frame(ice)
          names(ice) <- c("time_ka", "ice_volume")
          rownames(ice) <- NULL
          ins_used <- ins[ins[, 1] >= min(ice[, 1]) & ins[, 1] <= max(ice[, 1]), ]
          nm <- sprintf("Imbrie_ice_Tm%s_b%s_%s-%ska.txt", cd_fmt(Tm), cd_fmt(b), cd_fmt(min(ice[, 1])), cd_fmt(max(ice[, 1])))
          o <- list(cd_output(nm, ice, var = "ice"),
              cd_output(sprintf("Insolation_daily_%s%s_lon%s_%s-%ska.txt", cd_fmt(abs(p$lat)), if (p$lat >= 0) { "N" } else { "S" },
                  p$long, cd_fmt(p$tmin), cd_fmt(p$tmax)), ins, var = "ins"))
          code <- c(sprintf("ins <- astrochron::insoSeries(tmin = %s, tmax = %s, dt = %s, opt = 1, long = %s, lat = %s, S0 = %s, genplot = FALSE)",
              cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$dt), p$long, cd_fmt(p$lat), cd_fmt(p$S0)),
              "ins <- ins[order(ins[, 1]), ]",
              sprintf(paste0("ice <- astrochron::imbrie(insolation = ins, Tm = %s, b = %s, times = %s, initial = %s, burnin = %d,\n",
                  "    standardize = %s, genplot = 0, verbose = FALSE)"), cd_dep(Tm), cd_dep(b),
                  if (length(times)) { cd_dep(times) } else { "NULL" }, cd_fmt(p$initial), as.integer(p$burnin),
                  isTRUE(p$standardize)), cd_code_write(o))
          rr <- stats::cor(ins_used[, 2], ice[match(ins_used[, 1], ice[, 1]), 2], use = "complete.obs")
          list(result = list(ins = ins_used, ice = ice), outputs = o, code = code,
              text = sprintf("%d points from %s to %s ka; correlation of ice volume with insolation %s.", nrow(ice),
                  cd_fmt(min(ice[, 1])), cd_fmt(max(ice[, 1])), cd_fmt(signif(rr, 3))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 3, 2, 3), 2, byrow = TRUE), widths = c(2.2, 1))
          cd_par()
          cd_plot_one(res$ins, col = "#c0392b", xlab = "", ylab = "Insolation (W/m2)")
          cd_par()
          ice <- res$ice
          graphics::plot(ice[, 1], ice[, 2], type = "n", xlab = "Time (ka)", ylab = "Ice volume")
          graphics::polygon(c(ice[1, 1], ice[, 1], ice[nrow(ice), 1]), c(min(ice[, 2]), ice[, 2], min(ice[, 2])),
              col = grDevices::adjustcolor("#6495ed", 0.6), border = "#1f4e79")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          y <- ice[match(res$ins[, 1], ice[, 1]), 2]
          graphics::plot(res$ins[, 2], y, type = "l", col = grDevices::adjustcolor("#2e8b57", 0.7), xlab = "Insolation (W/m2)",
              ylab = "Ice volume", main = "Phase portrait")
          graphics::grid(col = "grey90", lty = 1)
        }),

    cd_tool("ab_sedmodel", B, "Time to stratigraphy: sed.-rate ramp or differential accumulation", group = gb,
        icon = "arrow-trend-up",
        info = paste("Converts a time series (time in kyr, value), e.g. ETP or insolation, into a synthetic stratigraphic",
            "series (depth in m): with a linearly changing sedimentation rate, or with a rate that follows the signal",
            "(differential accumulation); use it to test how tools cope with changing rates."),
        uses = "astrochron::sedRamp, astrochron::diffAccum",
        params = list(
          cd_sel("method", "Model", c("Linear sed.-rate ramp (sedRamp)" = "ramp", "Differential accumulation (diffAccum)" = "diff")),
          cd_num("sr_start", "Sed. rate at the start of the series (cm/kyr)", 1, min = 0, show_if = list(param = "method", values = "ramp")),
          cd_num("sr_end", "Sed. rate at the end of the series (cm/kyr)", 5, min = 0, show_if = list(param = "method", values = "ramp")),
          cd_num("sedmin", "Lowest sed. rate (cm/kyr)", 1, min = 0, show_if = list(param = "method", values = "diff")),
          cd_num("sedmax", "Highest sed. rate (cm/kyr)", 2, min = 0, show_if = list(param = "method", values = "diff")),
          cd_sel("dir", "Highest rate at", c("peaks of the signal" = "1", "troughs of the signal" = "-1"),
              show_if = list(param = "method", values = "diff"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          names(s) <- names(d)[1:2]
          txt <- cd_even_note(d, s)
          code <- cd_even_code(d)
          dt <- s[2, 1] - s[1, 1]
          if (p$method == "ramp") {
            if (!(p$sr_start > 0) || !(p$sr_end > 0)) {
              stop("the sedimentation rates must be positive", call. = FALSE)
            }
            if (cd_ab_exact(s[, 1])) {
              res <- cd_ab_quiet(astrochron::sedRamp(s, srstart = p$sr_start / 100, srend = p$sr_end / 100, genplot = FALSE,
                  verbose = FALSE))
              code <- c(code, sprintf("res <- astrochron::sedRamp(dat, srstart = %s, srend = %s, genplot = FALSE, verbose = FALSE)",
                  cd_fmt(p$sr_start / 100), cd_fmt(p$sr_end / 100)))
            } else {
              # sedRamp tests the sampling step for exact equality: run it on the sample index with the rates per step
              si <- data.frame(i = seq_len(nrow(s)) - 1, y = s[, 2])
              res <- cd_ab_quiet(astrochron::sedRamp(si, srstart = p$sr_start / 100 * dt, srend = p$sr_end / 100 * dt,
                  genplot = FALSE, verbose = FALSE))
              code <- c(code, sprintf("dt <- %s", cd_fmt(dt)), "si <- data.frame(seq_len(nrow(dat)) - 1, dat[, 2])",
                  sprintf("res <- astrochron::sedRamp(si, srstart = %s * dt, srend = %s * dt, genplot = FALSE, verbose = FALSE)",
                      cd_fmt(p$sr_start / 100), cd_fmt(p$sr_end / 100)))
            }
            sr <- seq(p$sr_start, p$sr_end, length.out = nrow(s))
            sfx <- sprintf("-sedRamp%s-%s", cd_fmt(p$sr_start), cd_fmt(p$sr_end))
          } else {
            if (!(p$sedmin > 0) || !(p$sedmax > p$sedmin)) {
              stop("the rates must be positive and the highest rate larger than the lowest", call. = FALSE)
            }
            res <- cd_ab_quiet(astrochron::diffAccum(s, sedmin = p$sedmin / 100, sedmax = p$sedmax / 100, dir = as.integer(p$dir),
                genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::diffAccum(dat, sedmin = %s, sedmax = %s, dir = %d, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$sedmin / 100), cd_fmt(p$sedmax / 100), as.integer(p$dir)))
            res <- as.data.frame(res)
            sr <- c(NA, diff(res[, 1]) / dt * 100)
            sr[1] <- sr[2]
            sfx <- sprintf("-diffAccum%s-%s", cd_fmt(p$sedmin), cd_fmt(p$sedmax))
          }
          res <- as.data.frame(res)
          names(res) <- c("depth_m", names(s)[2])
          srd <- data.frame(depth_m = res[, 1], sedrate_cm_kyr = sr)
          o <- list(cd_output(cd_out_name(file, sfx), res),
              cd_output(cd_out_name(file, paste0(sfx, "-sedrate")), srd, var = "srd"))
          code <- c(code, if (p$method == "ramp") {
            sprintf("srd <- data.frame(res[, 1], seq(%s, %s, length.out = nrow(res)))", cd_fmt(p$sr_start), cd_fmt(p$sr_end))
          } else {
            sprintf("srd <- data.frame(res[, 1], c(NA, diff(res[, 1])) / %s * 100)", cd_fmt(dt))
          }, cd_code_write(o))
          txt <- c(txt, sprintf("%d points, %s m of synthetic section; mean sed. rate %s cm/kyr.", nrow(res),
              cd_fmt(signif(diff(range(res[, 1])), 4)), cd_fmt(signif(diff(range(res[, 1])) / diff(range(s[, 1])) * 100, 4))))
          list(result = list(s = s, dep = res, sr = data.frame(time = s[, 1], sr = sr)), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE))
          cd_par()
          cd_plot_one(res$s, col = "#7f8c8d", xlab = "Time (kyr)", ylab = "Input")
          cd_par()
          graphics::plot(res$sr[, 1], res$sr[, 2], type = "l", col = "#2e8b57", lwd = 1.5, xlab = "Time (kyr)",
              ylab = "Sed. rate (cm/kyr)")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          cd_plot_one(res$dep, col = "#1f4e79", xlab = "Depth (m)", ylab = "Synthetic record")
        }),

    cd_tool("ab_synthstrat", B, "Synthetic stratigraphy from a forcing (facies)", group = gb, icon = "bars-staggered",
        info = paste("Turns a forcing series (e.g. precession, ETP or an ice-volume model) into a series of sedimentary",
            "facies by slicing the standardised forcing into classes, and compares the spectra of forcing and facies."),
        uses = "astrochron::synthStrat, astrochron::periodogram",
        params = list(
          cd_num("nfacies", "Number of facies", 4, min = 2, step = 1),
          cd_chk("clip", "Clip the forcing at its mean value (no facies change below it)", TRUE),
          cd_chk("flip", "Reverse the sign of the forcing", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          names(s) <- names(d)[1:2]
          nf <- as.integer(p$nfacies)
          st <- cd_ab_quiet(astrochron::synthStrat(s, nfacies = nf, clip = isTRUE(p$clip), flip = isTRUE(p$flip), output = TRUE,
              genplot = 0, verbose = FALSE), device = TRUE)
          st <- as.data.frame(st)
          names(st) <- c(names(s)[1], "facies")
          z <- s
          z[, 2] <- (z[, 2] - mean(z[, 2])) / stats::sd(z[, 2])
          if (isTRUE(p$flip)) {
            z[, 2] <- -z[, 2]
          }
          sp1 <- cd_ab_quiet(astrochron::periodogram(z, detrend = TRUE, output = 1, genplot = FALSE, verbose = FALSE))
          sp2 <- cd_ab_quiet(astrochron::periodogram(st, detrend = TRUE, output = 1, genplot = FALSE, verbose = FALSE))
          o <- list(cd_output(cd_out_name(file, paste0("-facies", nf)), st))
          code <- c(cd_even_code(d), sprintf("res <- astrochron::synthStrat(dat, nfacies = %d, clip = %s, flip = %s, output = TRUE, genplot = 2, verbose = FALSE)",
              nf, isTRUE(p$clip), isTRUE(p$flip)), cd_code_write(o))
          nb <- sum(diff(st[, 2]) != 0) + 1
          list(result = list(forcing = z, strat = st, nf = nf, sp1 = as.data.frame(sp1)[, 1:2], sp2 = as.data.frame(sp2)[, 1:2],
              clip = isTRUE(p$clip)), outputs = o, code = code, text = c(cd_even_note(d, s),
                  sprintf("%d beds of %d facies; mean bed duration %s.", nb, nf, cd_fmt(signif(diff(range(st[, 1])) / nb, 4)))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:4, nrow = 1), widths = c(1.1, 0.9, 0.45, 2))
          z <- res$forcing
          st <- res$strat
          yl <- rev(range(z[, 1]))
          cd_par()
          graphics::par(mar = c(4, 4.3, 1.6, 0.3))
          graphics::plot(z[, 2], z[, 1], type = "l", ylim = yl, xlab = "Forcing (standardised)", ylab = "Time", col = "#1f4e79")
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          graphics::par(mar = c(4, 0.8, 1.6, 0.3))
          graphics::plot(st[, 2], st[, 1], type = "s", ylim = yl, xlim = c(0.5, res$nf + 0.5), xlab = "Facies", ylab = "",
              yaxt = "n", col = "#7f8c8d")
          graphics::par(mar = c(4, 0.3, 1.6, 0.8))
          graphics::plot(NA, xlim = c(0, 1), ylim = yl, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
          fill <- grDevices::gray.colors(res$nf, start = 0.9, end = 0.1, gamma = 2.2)
          ch <- c(1, which(diff(st[, 2]) != 0) + 1)
          top <- st[ch, 1]
          bot <- c(st[ch[-1], 1], st[nrow(st), 1])
          graphics::rect(0, top, 1, bot, col = fill[st[ch, 2]], border = NA)
          cd_par()
          vw <- cd_view_lims(range(res$sp1[, 1]), range(c(res$sp1[, 2], res$sp2[, 2])), last = TRUE)
          graphics::plot(res$sp1[, 1], res$sp1[, 2] / max(res$sp1[, 2]), type = "l", col = "#1f4e79", xlab = "Frequency (cycles/unit)",
              ylab = "Relative power", xlim = vw$x, ylim = c(0, 1.05))
          graphics::lines(res$sp2[, 1], res$sp2[, 2] / max(res$sp2[, 2]), col = "#c0392b")
          graphics::grid(col = "grey90", lty = 1)
          graphics::legend("topright", c("forcing", "facies"), col = c("#1f4e79", "#c0392b"), lwd = 1, bty = "n", cex = 0.85)
        }),

    cd_tool("ab_bioturb", B, "Bioturbation: mix or unmix a record", group = gb, icon = "worm",
        info = paste("Simulates the smoothing of a time series (time in kyr) by diffusive bioturbation, or removes it by",
            "Wiener deconvolution (Liu et al. 2021), for a mixed-layer depth, sedimentation rate and mixing parameter G."),
        uses = "astrochron::bioturb, astrochron::unbioturb, astrochron::impulseResponse (conv_fft, deconv)",
        params = list(
          cd_sel("method", "Direction", c("Mix: simulate bioturbation (bioturb)" = "bioturb",
              "Unmix: remove bioturbation (unbioturb)" = "unbioturb")),
          cd_num("G", "Mixing parameter G = D / (ML v)", 4, min = 0.01, step = 0.5),
          cd_num("ML", "Mixed-layer depth (cm)", 10, min = 0.1),
          cd_num("v", "Sedimentation rate (cm/kyr)", 2, min = 0.01),
          cd_num("pt", "Fraction tapered before deconvolution", 0.2, min = 0, max = 1, step = 0.05,
              show_if = list(param = "method", values = "unbioturb")),
          cd_chk("wiener", "Wiener filter (stabilises the deconvolution)", TRUE, show_if = list(param = "method", values = "unbioturb")),
          cd_num("fhigh", "Lowpass cut-off after deconvolution (cycles/kyr, 0 = none)", 0, min = 0,
              show_if = list(param = "method", values = "unbioturb"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          names(s) <- names(d)[1:2]
          if (p$method == "bioturb") {
            r <- cd_ab_quiet(astrochron::bioturb(s, G = p$G, ML = p$ML, v = p$v, output = 1, genplot = FALSE, verbose = FALSE))
            call <- sprintf("res <- astrochron::bioturb(dat, G = %s, ML = %s, v = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$G), cd_fmt(p$ML), cd_fmt(p$v))
            sfx <- sprintf("-bioturb-G%s-ML%s", cd_fmt(p$G), cd_fmt(p$ML))
          } else {
            fh <- if (p$fhigh > 0) { p$fhigh } else { NULL }
            r <- cd_ab_quiet(astrochron::unbioturb(s, G = p$G, ML = p$ML, v = p$v, pt = p$pt, wiener = isTRUE(p$wiener), fhigh = fh,
                output = 1, genplot = FALSE, verbose = FALSE))
            call <- sprintf("res <- astrochron::unbioturb(dat, G = %s, ML = %s, v = %s, pt = %s, wiener = %s, fhigh = %s, genplot = FALSE, verbose = FALSE)",
                cd_fmt(p$G), cd_fmt(p$ML), cd_fmt(p$v), cd_fmt(p$pt), isTRUE(p$wiener), if (is.null(fh)) { "NULL" } else { cd_fmt(fh) })
            sfx <- sprintf("-unbioturb-G%s-ML%s", cd_fmt(p$G), cd_fmt(p$ML))
          }
          r <- as.data.frame(r)
          names(r) <- names(s)
          # bioturb(output = 2) fails (its time axis is only defined when genplot = TRUE): impulse response directly
          fc <- astrochron::impulseResponse(p$G)
          ir <- data.frame(time_kyr = seq(0.01, 10, by = 0.01)[seq_along(fc)] * p$ML / p$v, weight = fc)
          o <- list(cd_output(cd_out_name(file, sfx), r),
              cd_output(cd_out_name(file, paste0(sfx, "-impulse")), ir, var = "ir"))
          code <- c(cd_even_code(d), call, sprintf("fc <- astrochron::impulseResponse(%s)", cd_fmt(p$G)),
              sprintf("ir <- data.frame(seq(0.01, 10, by = 0.01) * %s / %s, fc)", cd_fmt(p$ML), cd_fmt(p$v)), cd_code_write(o))
          list(result = list(before = s, after = r, ir = ir, method = p$method, depo = 5 * p$ML / p$v), outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("Mixing time scale ML / v = %s kyr; %d points.", cd_fmt(signif(p$ML / p$v, 4)),
                  nrow(r))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1), heights = c(1, 1.4))
          cd_par()
          graphics::plot(res$ir[, 1], res$ir[, 2], type = "l", col = "#2e8b57", lwd = 1.5,
              xlab = "Time (kyr, present to past)", ylab = "Impulse response", main = "Impulse response")
          graphics::abline(v = res$depo, col = "#c0392b", lty = 2)
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          s <- res$before
          vw <- cd_view_lims(range(s[, 1]), range(c(s[, 2], res$after[, 2])), x = s[, 1], y = s[, 2])
          graphics::plot(s[, 1], s[, 2], type = "l", col = "#9aa5b1", lwd = 1.5, xlab = "Time (kyr)", ylab = names(s)[2],
              xlim = vw$x, ylim = vw$y)
          graphics::lines(res$after[, 1], res$after[, 2], col = "#1f4e79", lwd = 1.5)
          graphics::grid(col = "grey90", lty = 1)
          graphics::legend("topright", c("input", if (res$method == "bioturb") { "bioturbated" } else { "unbioturbated" }),
              col = c("#9aa5b1", "#1f4e79"), lwd = 1.5, bty = "n", cex = 0.85)
        })
  )
}
