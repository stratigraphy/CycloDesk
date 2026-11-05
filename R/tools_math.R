# Math menu ####

cd_series_result_plot <- function(before, after, lab_before = "input", lab_after = "result") {
  cd_plot_series(list(before, after), labels = c(lab_before, lab_after))
}

cd_two_panel <- function(top, bottom, top_lab = "Value", bottom_lab = "Value", extra_top = NULL, extra_lab = NULL) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, ncol = 1))
  cd_par()
  cd_plot_one(top, col = "#7f8c8d", ylab = top_lab, xlab = "")
  if (!is.null(extra_top)) {
    graphics::lines(extra_top[, 1], extra_top[, 2], col = "#c0392b", lwd = 2)
    if (!is.null(extra_lab)) {
      graphics::legend("topright", legend = c("input", extra_lab), col = c("#7f8c8d", "#c0392b"), lwd = c(1, 2), bty = "n",
          cex = 0.85)
    }
  }
  cd_par()
  cd_plot_one(bottom, col = "#1f4e79", ylab = bottom_lab, xlab = "Depth / time")
}

cd_tools_math <- function() {
  dt_default <- function(d) {
    signif(cd_dt(d[, 1]), 4)
  }
  xmin_default <- function(d) {
    signif(min(d[, 1], na.rm = TRUE), 6)
  }
  xmax_default <- function(d) {
    signif(max(d[, 1], na.rm = TRUE), 6)
  }
  list(
    cd_tool("sort", "Math", "Sort / unique / remove empty",
        info = "Sorts by the first column, removes empty values and averages duplicated depths.",
        uses = "astrochron::sortNave",
        compute = function(d, p, d2, file, file2) {
          res <- astrochron::sortNave(d[stats::complete.cases(d), ], genplot = FALSE, verbose = FALSE)
          res <- as.data.frame(res)
          o <- list(cd_output(cd_out_name(file, "-sort"), res))
          list(result = list(before = d, after = res), outputs = o,
              text = sprintf("%d rows in, %d rows out", nrow(d), nrow(res)),
              code = c("res <- astrochron::sortNave(dat[complete.cases(dat), ], genplot = FALSE)", cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_series_result_plot(res$before, res$after)
        }),
    cd_tool("interpolate", "Math", "Interpolation",
        info = "Resamples the series at an even interval (linear, as Acycle) or with a cubic spline.",
        uses = "astrochron::linterp, stats::spline",
        params = list(
          cd_num("dt", "New sampling interval", dt_default, min = 0),
          cd_sel("method", "Method", c("Linear" = "linear", "Cubic spline" = "spline"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          if (p$method == "linear") {
            res <- astrochron::linterp(s, dt = p$dt, genplot = FALSE, verbose = FALSE)
            code <- sprintf("res <- astrochron::linterp(dat, dt = %s, genplot = FALSE)", cd_fmt(p$dt))
          } else {
            xo <- seq(s$x[1], s$x[nrow(s)], by = p$dt)
            res <- data.frame(x = xo, y = stats::spline(s$x, s$y, xout = xo, method = "fmm")$y)
            code <- c(sprintf("xo <- seq(min(dat[, 1]), max(dat[, 1]), by = %s)", cd_fmt(p$dt)),
                "res <- data.frame(x = xo, y = spline(dat[, 1], dat[, 2], xout = xo)$y)")
          }
          res <- as.data.frame(res)
          names(res) <- names(d)[1:2]
          o <- list(cd_output(cd_out_name(file, paste0("-rsp", cd_fmt(p$dt))), res))
          list(result = list(before = s, after = res), outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points at interval %s", nrow(res), cd_fmt(p$dt)))
        },
        plot = function(res, p) {
          cd_plot_series(list(res$before, res$after), labels = c("input", "interpolated"), type = "l")
        }),
    cd_tool("select", "Math", "Select part",
        info = "Keeps the part of the series between two depths / times (Acycle: Math > Select Parts). Drag the two lines on the figure or type the values.",
        uses = "base R", lines = c("xmin", "xmax"),
        preview = function(d, p) {
          cd_plot_interval(d, p$xmin, p$xmax, keep_inside = TRUE)
        },
        params = list(cd_num("xmin", "From", function(d) { cd_part(d, 0.25) }), cd_num("xmax", "To", function(d) { cd_part(d, 0.75) })),
        compute = function(d, p, d2, file, file2) {
          res <- d[d[, 1] >= p$xmin & d[, 1] <= p$xmax, , drop = FALSE]
          o <- list(cd_output(cd_out_name(file, sprintf("-%s-%s", cd_fmt(p$xmin), cd_fmt(p$xmax))), res))
          list(result = list(before = d, after = res), outputs = o,
              code = c(sprintf("res <- dat[dat[, 1] >= %s & dat[, 1] <= %s, ]", cd_fmt(p$xmin), cd_fmt(p$xmax)),
                  cd_code_write(o)), text = sprintf("%d of %d points kept", nrow(res), nrow(d)))
        },
        plot = function(res, p) {
          cd_plot_interval(res$before, p$xmin, p$xmax, keep_inside = TRUE)
        }),
    cd_tool("remove", "Math", "Remove part",
        info = paste("Removes the part of the series between two depths; optionally closes the gap (Acycle: Remove Parts).",
            "Drag the two lines on the figure or type the values."),
        uses = "base R", lines = c("xmin", "xmax"),
        preview = function(d, p) {
          cd_plot_interval(d, p$xmin, p$xmax, keep_inside = FALSE)
        },
        params = list(cd_num("xmin", "From", function(d) { cd_part(d, 0.4) }), cd_num("xmax", "To", function(d) { cd_part(d, 0.6) }),
            cd_chk("close", "Close the gap (shift the part below upwards)", FALSE)),
        compute = function(d, p, d2, file, file2) {
          res <- d[d[, 1] < p$xmin | d[, 1] > p$xmax, , drop = FALSE]
          code <- sprintf("res <- dat[dat[, 1] < %s | dat[, 1] > %s, ]", cd_fmt(p$xmin), cd_fmt(p$xmax))
          if (p$close) {
            res[res[, 1] > p$xmax, 1] <- res[res[, 1] > p$xmax, 1] - (p$xmax - p$xmin)
            code <- c(code, sprintf("res[res[, 1] > %s, 1] <- res[res[, 1] > %s, 1] - %s", cd_fmt(p$xmax), cd_fmt(p$xmax),
                cd_fmt(p$xmax - p$xmin)))
          }
          o <- list(cd_output(cd_out_name(file, sprintf("-rm%s-%s", cd_fmt(p$xmin), cd_fmt(p$xmax))), res))
          list(result = list(before = d, after = res), outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points removed", nrow(d) - nrow(res)))
        },
        plot = function(res, p) {
          cd_plot_interval(res$before, p$xmin, p$xmax, keep_inside = FALSE)
        }),
    cd_tool("trim", "Math", "Remove outliers",
        info = "Removes values outside c times the interquartile range from the quartiles (astrochron::trim).",
        uses = "astrochron::trim",
        params = list(cd_num("c", "Factor c (x IQR)", 1.5, min = 0.1, step = 0.1)),
        compute = function(d, p, d2, file, file2) {
          res <- as.data.frame(astrochron::trim(d, c = p$c, genplot = FALSE, verbose = FALSE))
          o <- list(cd_output(cd_out_name(file, "-trim"), res))
          list(result = list(before = d, after = res), outputs = o,
              code = c(sprintf("res <- astrochron::trim(dat, c = %s, genplot = FALSE)", cd_fmt(p$c)), cd_code_write(o)),
              text = sprintf("%d outliers removed", nrow(d) - nrow(res)))
        },
        plot = function(res, p) {
          cd_series_result_plot(res$before, res$after, "input", "trimmed")
        }),
    cd_tool("clip", "Math", "Clipping",
        info = "Values below the lower or above the upper limit are set to the limit (Acycle: Math > Clipping).",
        uses = "base R",
        params = list(
          cd_num("lower", "Lower limit", function(d) { signif(min(d[, 2], na.rm = TRUE), 6) }),
          cd_num("upper", "Upper limit", function(d) { signif(max(d[, 2], na.rm = TRUE), 6) })),
        compute = function(d, p, d2, file, file2) {
          res <- d
          res[, 2] <- pmin(pmax(res[, 2], p$lower), p$upper)
          o <- list(cd_output(cd_out_name(file, "-clip"), res))
          list(result = list(before = d, after = res), outputs = o,
              code = c("res <- dat", sprintf("res[, 2] <- pmin(pmax(res[, 2], %s), %s)", cd_fmt(p$lower), cd_fmt(p$upper)),
                  cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_series_result_plot(res$before, res$after, "input", "clipped")
        }),
    cd_tool("transform", "Math", "Data transformations",
        info = "Standardise, remove the mean, log transform, change sign or scale, or flip the depth axis.",
        uses = "astrochron::s, astrochron::demean, astrochron::logT",
        params = list(
          cd_sel("op", "Transformation", c("Standardise (z-score)" = "stand", "Remove mean" = "demean",
              "Log10" = "log10", "Natural log" = "ln", "Change sign (x -1)" = "neg", "Normalise to 0-1" = "norm",
              "Multiply depth by a factor (units)" = "xscale", "Flip depth axis (top <-> base)" = "flipx",
              "Depth to height / time to age (x -> max - x)" = "flipx2")),
          cd_num("factor", "Factor", 0.01, show_if = list(param = "op", values = "xscale"),
              help = "e.g. 0.01 converts cm to m")),
        compute = function(d, p, d2, file, file2) {
          res <- d
          y <- d[, 2]
          code <- "res <- dat"
          if (p$op == "stand") {
            res <- as.data.frame(astrochron::s(d, genplot = FALSE, verbose = FALSE))
            code <- "res <- astrochron::s(dat, genplot = FALSE)"
          } else if (p$op == "demean") {
            res <- as.data.frame(astrochron::demean(d, genplot = FALSE, verbose = FALSE))
            code <- "res <- astrochron::demean(dat, genplot = FALSE)"
          } else if (p$op == "log10") {
            if (any(y <= 0, na.rm = TRUE)) {
              stop("log10 needs values > 0", call. = FALSE)
            }
            res[, 2] <- log10(y)
            code <- c(code, "res[, 2] <- log10(res[, 2])")
          } else if (p$op == "ln") {
            res <- as.data.frame(astrochron::logT(d, genplot = FALSE, verbose = FALSE))
            code <- "res <- astrochron::logT(dat, genplot = FALSE)"
          } else if (p$op == "neg") {
            res[, 2] <- -y
            code <- c(code, "res[, 2] <- -res[, 2]")
          } else if (p$op == "norm") {
            res[, 2] <- (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE))
            code <- c(code, "res[, 2] <- (res[, 2] - min(res[, 2])) / diff(range(res[, 2]))")
          } else if (p$op == "xscale") {
            res[, 1] <- d[, 1] * p$factor
            code <- c(code, sprintf("res[, 1] <- res[, 1] * %s", cd_fmt(p$factor)))
          } else {
            res[, 1] <- max(d[, 1]) - d[, 1] + if (p$op == "flipx") { min(d[, 1]) } else { 0 }
            res <- res[order(res[, 1]), ]
            code <- c(code, sprintf("res[, 1] <- max(res[, 1]) - res[, 1]%s", if (p$op == "flipx") { " + min(dat[, 1])" } else { "" }),
                "res <- res[order(res[, 1]), ]")
          }
          sfx <- c(stand = "-stand", demean = "-demean", log10 = "-log10", ln = "-ln", neg = "-neg", norm = "-norm",
              xscale = paste0("-x", cd_fmt(p$factor)), flipx = "-flip", flipx2 = "-rev")[[p$op]]
          o <- list(cd_output(cd_out_name(file, sfx), res))
          list(result = list(before = d, after = res), outputs = o, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "input", "result")
        }),
    cd_tool("derivative", "Math", "Derivative",
        info = "First derivative dy/dx (central differences).",
        uses = "base R",
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          n <- nrow(s)
          dy <- numeric(n)
          dy[2:(n - 1)] <- (s$y[3:n] - s$y[1:(n - 2)]) / (s$x[3:n] - s$x[1:(n - 2)])
          dy[1] <- (s$y[2] - s$y[1]) / (s$x[2] - s$x[1])
          dy[n] <- (s$y[n] - s$y[n - 1]) / (s$x[n] - s$x[n - 1])
          res <- data.frame(x = s$x, dydx = dy)
          o <- list(cd_output(cd_out_name(file, "-deriv"), res))
          list(result = list(before = s, after = res), outputs = o,
              code = c("n <- nrow(dat)", "dy <- c(NA, (dat[3:n, 2] - dat[1:(n - 2), 2]) / (dat[3:n, 1] - dat[1:(n - 2), 1]), NA)",
                  "res <- data.frame(x = dat[, 1], dydx = dy)", cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "input", "dy/dx")
        }),
    cd_tool("smooth", "Math", "Smoothing",
        info = "Moving average, moving median or Gaussian kernel with a window in depth / time units, or LOESS.",
        uses = "base R (stats::loess)",
        params = list(
          cd_sel("method", "Method", c("Moving average" = "mean", "Moving median" = "median",
              "Gaussian kernel" = "gauss", "LOESS (span)" = "loess")),
          cd_num("win", "Window width (depth / time units)", function(d) { signif(11 * cd_dt(d[, 1]), 3) }, min = 0,
              show_if = list(param = "method", values = c("mean", "median", "gauss")),
              help = "For the Gaussian kernel the window is 4 sigma wide."),
          cd_num("span", "LOESS span (fraction of the record)", 0.1, min = 0.01, max = 1, step = 0.01,
              show_if = list(param = "method", values = "loess"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          res <- data.frame(x = s$x, y = cd_smooth(s$x, s$y, p$method, p$win, p$span))
          names(res) <- names(d)[1:2]
          sfx <- switch(p$method, mean = "-movmean", median = "-movmed", gauss = "-movgauss", loess = "-loess")
          sfx <- paste0(sfx, cd_fmt(if (p$method == "loess") { p$span } else { p$win }))
          o <- list(cd_output(cd_out_name(file, sfx), res))
          code <- c(sprintf("res <- data.frame(x = dat[, 1], y = CycloDesk:::cd_smooth(dat[, 1], dat[, 2], %s, %s, %s))",
              cd_dep(p$method), cd_fmt(p$win), cd_fmt(p$span)), cd_code_write(o))
          list(result = list(before = s, after = res), outputs = o, code = code)
        },
        plot = function(res, p) {
          cd_plot_series(list(res$before, res$after), labels = c("input", "smoothed"))
        }),
    cd_tool("column", "Math", "Extract column",
        info = "Writes one proxy column of a multi-column table as its own two-column series.", full_table = TRUE,
        uses = "base R",
        params = list(cd_sel("col", "Column", function(d) { names(d)[-1] }, value = function(d) { names(d)[2] })),
        compute = function(d, p, d2, file, file2) {
          j <- match(p$col, names(d))
          if (is.na(j)) {
            j <- 2L
          }
          res <- d[!is.na(d[, j]), c(1, j)]
          o <- list(cd_output(cd_out_name(file, paste0("-", gsub("[^A-Za-z0-9_.]", "_", names(d)[j]))), res))
          list(result = res, outputs = o,
              code = c(sprintf("res <- dat[!is.na(dat[, %d]), c(1, %d)]", j, j), cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = names(res)[2])
        }),
    cd_tool("changepoint", "Math", "Bayesian change points", heavy = TRUE,
        info = "Bayesian change-point regression (as Acycle: Math > Changepoint).",
        uses = "WaverideR::bayes_changepoint",
        params = list(
          cd_num("k_max", "Maximum number of change points", 10, min = 1, step = 1),
          cd_num("d_min", "Minimum segment length (points)", 5, min = 1, step = 1),
          cd_num("n_sim", "Number of simulations", 500, min = 50, step = 50),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          res <- WaverideR::bayes_changepoint(data = as.matrix(s), k_max = p$k_max, d_min = p$d_min, n_sim = p$n_sim,
              seed = p$seed, genplot = FALSE)
          o <- list(cd_output(cd_out_name(file, "-bcp_model"), as.data.frame(res$model), var = "res$model"),
              cd_output(cd_out_name(file, "-bcp_prob"), as.data.frame(res$cpt), var = "res$cpt"))
          code <- c(sprintf("res <- WaverideR::bayes_changepoint(data = as.matrix(dat), k_max = %d, d_min = %d, n_sim = %d, seed = %d)",
              as.integer(p$k_max), as.integer(p$d_min), as.integer(p$n_sim), as.integer(p$seed)),
              cd_code_write(o))
          list(result = res, outputs = o, code = code, text = sprintf("R2 = %s", cd_fmt(res$r2)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1), heights = c(2, 1))
          cd_par()
          dat <- res$data
          cd_plot_one(dat, col = "#7f8c8d", ylab = "Value", xlab = "")
          graphics::lines(res$model[, 1], res$model[, 2], col = "#c0392b", lwd = 2)
          graphics::legend("topright", c("data", "posterior mean model"), col = c("#7f8c8d", "#c0392b"), lwd = c(1, 2),
              bty = "n", cex = 0.85)
          cd_par()
          graphics::plot(res$cpt[, 1], res$cpt[, 2], type = "h", col = "#1f4e79", xlab = "Depth / time",
              ylab = "P(change point)", ylim = c(0, max(1e-3, max(res$cpt[, 2], na.rm = TRUE))))
        })
  )
}

`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

# Smoothing with a window in x units
cd_smooth <- function(x, y, method = "mean", win = 1, span = 0.1) {
  n <- length(x)
  if (method == "loess") {
    fit <- stats::loess(y ~ x, span = span, degree = 2, family = "gaussian")
    return(as.numeric(stats::predict(fit, newdata = data.frame(x = x))))
  }
  out <- numeric(n)
  h <- win / 2
  lo <- findInterval(x - h, x, left.open = TRUE) + 1L
  hi <- findInterval(x + h, x)
  if (method == "mean") {
    cs <- c(0, cumsum(y))
    out <- (cs[hi + 1L] - cs[lo]) / (hi - lo + 1L)
  } else if (method == "median") {
    for (i in seq_len(n)) {
      out[i] <- stats::median(y[lo[i]:hi[i]])
    }
  } else {
    sig <- win / 4
    for (i in seq_len(n)) {
      j <- lo[i]:hi[i]
      w <- exp(-0.5 * ((x[j] - x[i]) / sig)^2)
      out[i] <- sum(w * y[j]) / sum(w)
    }
  }
  out
}

# A depth at a fraction of the record, rounded to the sampling interval
cd_part <- function(d, frac) {
  r <- range(d[, 1], na.rm = TRUE)
  x <- r[1] + frac * diff(r)
  dt <- cd_dt(d[, 1])
  if (is.finite(dt) && dt > 0) {
    x <- r[1] + round((x - r[1]) / dt) * dt
  }
  signif(x, 7)
}
