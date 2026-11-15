# Tools: math_extra ####
#
# Math menu, group "More (WaverideR)": extremes, windowed lag-1 autocorrelation, litholog discretisation, change-point
# R2 curve and a red-noise generator.

cd_tools_math_extra <- function() {
  grp <- "More (WaverideR)"
  list(
    cd_tool("mx_extremes", "Math", "Local maxima and minima", group = grp, icon = "arrow-up-wide-short",
        info = paste("Finds the local maxima and/or minima of a (filtered or extracted) cycle, keeping only the most",
            "extreme of consecutive extremes of the same kind, e.g. as tie points for an age model."),
        uses = "WaverideR::max_detect / min_detect",
        params = list(
          cd_sel("which", "Find", c("Maxima" = "max", "Minima" = "min", "Maxima and minima" = "both")),
          cd_num("pts", "Points compared on each side", 3, min = 2, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          dm <- as.matrix(s)
          o <- list()
          res <- list(data = s)
          code <- character()
          if (p$which %in% c("max", "both")) {
            mx <- as.data.frame(WaverideR::max_detect(data = dm, pts = as.integer(p$pts)))
            res$max <- data.frame(x = mx[, 1], y = mx[, 2])
            o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-max"), res$max, var = "mx")
            code <- c(code, sprintf("mx <- WaverideR::max_detect(data = as.matrix(dat), pts = %d)[, 1:2]", as.integer(p$pts)))
          }
          if (p$which %in% c("min", "both")) {
            mn <- as.data.frame(WaverideR::min_detect(data = dm, pts = as.integer(p$pts)))
            res$min <- data.frame(x = mn[, 1], y = mn[, 2])
            o[[length(o) + 1]] <- cd_output(cd_out_name(file, "-min"), res$min, var = "mn")
            code <- c(code, sprintf("mn <- WaverideR::min_detect(data = as.matrix(dat), pts = %d)[, 1:2]", as.integer(p$pts)))
          }
          txt <- character()
          if (!is.null(res$max)) {
            txt <- c(txt, sprintf("%d maxima, mean spacing %s.", nrow(res$max), cd_fmt(signif(mean(diff(res$max$x)), 4))))
          }
          if (!is.null(res$min)) {
            txt <- c(txt, sprintf("%d minima, mean spacing %s.", nrow(res$min), cd_fmt(signif(mean(diff(res$min$x)), 4))))
          }
          list(result = res, outputs = o, code = c(code, cd_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          cd_plot_one(res$data, col = "#7f8c8d", xlab = "Depth / time", ylab = "Value")
          if (!is.null(res$max)) {
            graphics::points(res$max$x, res$max$y, pch = 24, bg = "#c0392b", col = "#c0392b", cex = 0.8)
          }
          if (!is.null(res$min)) {
            graphics::points(res$min$x, res$min$y, pch = 25, bg = "#1f4e79", col = "#1f4e79", cex = 0.8)
          }
        }),

    cd_tool("mx_lag1", "Math", "Windowed lag-1 autocorrelation (Monte Carlo)", group = grp, heavy = TRUE, icon = "link",
        info = paste("Lag-1 autocorrelation in a moving window, averaged over Monte Carlo runs with random sampling",
            "intervals and window widths between the shortest and longest window (mean and standard deviation)."),
        uses = "WaverideR::lag_1",
        params = list(
          cd_num("win_min", "Shortest window (depth / time units)", function(d) { signif(diff(range(d[, 1], na.rm = TRUE)) * 0.05, 3) },
              min = 0),
          cd_num("win_max", "Longest window (depth / time units)", function(d) { signif(diff(range(d[, 1], na.rm = TRUE)) * 0.15, 3) },
              min = 0),
          cd_num("n_sim", "Number of simulations", 10, min = 1, step = 5),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          set.seed(as.integer(p$seed))
          r <- WaverideR::lag_1(data = as.matrix(s), n_sim = as.integer(p$n_sim), win_max = p$win_max, win_min = p$win_min)
          r <- as.data.frame(r)
          tab <- data.frame(x = r[, 1], lag1_mean = r[, 2], lag1_sd = r[, 3])
          o <- list(cd_output(cd_out_name(file, "-lag1"), tab, header = TRUE, var = "tab"))
          code <- c(sprintf("set.seed(%d)", as.integer(p$seed)),
              sprintf("res <- WaverideR::lag_1(data = as.matrix(dat), n_sim = %d, win_max = %s, win_min = %s)", as.integer(p$n_sim),
                  cd_fmt(p$win_max), cd_fmt(p$win_min)),
              "tab <- data.frame(x = res[, 1], lag1_mean = res[, 2], lag1_sd = res[, 3])", cd_code_write(o))
          list(result = list(data = s, tab = tab), outputs = o, code = code,
              text = sprintf("Lag-1 autocorrelation %s to %s (mean of %d runs).", cd_fmt(signif(min(tab$lag1_mean, na.rm = TRUE), 3)),
                  cd_fmt(signif(max(tab$lag1_mean, na.rm = TRUE), 3)), as.integer(p$n_sim)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$data, col = "#7f8c8d", ylab = "Value", xlab = "")
          cd_par()
          tb <- res$tab[stats::complete.cases(res$tab), ]
          lo <- tb$lag1_mean - tb$lag1_sd
          hi <- tb$lag1_mean + tb$lag1_sd
          graphics::plot(tb$x, tb$lag1_mean, type = "n", ylim = range(c(lo, hi), finite = TRUE), xlab = "Depth / time",
              ylab = "Lag-1 autocorrelation")
          graphics::polygon(c(tb$x, rev(tb$x)), c(lo, rev(hi)), col = grDevices::adjustcolor("#1f4e79", 0.15), border = NA)
          graphics::lines(tb$x, tb$lag1_mean, col = "#1f4e79", lwd = 1.5)
          graphics::grid(col = "grey90", lty = 1)
        }),

    cd_tool("mx_lithlog", "Math", "Discretise a litholog (base, top, value)", group = grp, full_table = TRUE,
        icon = "bars-staggered",
        info = paste("Turns a bed table (column 1 base, column 2 top of every bed, column 3 rank or proxy value, as in",
            "StratigrapheR) into an evenly sampled series that keeps the bed boundaries."),
        uses = "WaverideR::lithlog_disc",
        params = list(
          cd_num("subset_fact", "Divisor of the greatest common bed thickness", 10, min = 1, step = 1),
          cd_chk("lin_interp", "Resample to a regular step", FALSE),
          cd_num("dt", "Step of the resampled series", function(d) {
            signif(stats::median(abs(d[, 2] - d[, 1]), na.rm = TRUE) / 10, 3)
          }, min = 0, show_if = list(param = "lin_interp", values = "true"))),
        compute = function(d, p, d2, file, file2) {
          d <- as.data.frame(d)
          if (ncol(d) < 3) {
            stop("the litholog needs three columns: base, top and value of every bed", call. = FALSE)
          }
          ll <- d[stats::complete.cases(d[, 1:3]), 1:3]
          ll <- ll[order(ll[, 1]), ]
          r <- WaverideR::lithlog_disc(litholog = as.matrix(ll), subset_fact = p$subset_fact, lin_interp = isTRUE(p$lin_interp),
              dt = if (isTRUE(p$lin_interp)) { p$dt } else { NULL })
          r <- as.data.frame(r)
          out <- data.frame(x = r[, 1], y = r[, 2])
          o <- list(cd_output(cd_out_name(file, "-disc"), out, var = "out"))
          code <- c("ll <- dat[complete.cases(dat[, 1:3]), 1:3]", "ll <- ll[order(ll[, 1]), ]",
              sprintf("res <- WaverideR::lithlog_disc(litholog = as.matrix(ll), subset_fact = %s, lin_interp = %s, dt = %s)",
                  cd_fmt(p$subset_fact), isTRUE(p$lin_interp), if (isTRUE(p$lin_interp)) { cd_fmt(p$dt) } else { "NULL" }),
              "out <- data.frame(res[, 1], res[, 2])", cd_code_write(o))
          list(result = out, outputs = o, code = code,
              text = sprintf("%d beds -> %d points, step %s.", nrow(ll), nrow(out), cd_fmt(signif(cd_dt(out$x), 4))))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = "Discretised litholog", type = "s")
        }),

    cd_tool("mx_bcp_r2", "Math", "Change points: R2 against their number", group = grp, heavy = TRUE, icon = "chart-simple",
        info = paste("Runs the Bayesian change-point regression (Ruggieri 2013) for 1 to k change points and gives the R2",
            "of each model, to choose the number of change points for Bayesian change points."),
        uses = "WaverideR::bayes_changepoint_r2",
        params = list(
          cd_num("k_max", "Largest number of change points", 10, min = 1, step = 1),
          cd_num("d_min", "Minimum segment length (points)", 5, min = 1, step = 1),
          cd_num("n_sim", "Number of simulations", 500, min = 50, step = 50),
          cd_num("max_n", "Largest number of points (longer records are resampled)", 1500, min = 100, step = 100),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          txt <- character()
          code <- character()
          if (nrow(s) > p$max_n) {
            # bayes_changepoint_r2 overflows (integer cumsum) for records of more than about 1800 points
            xo <- seq(min(s$x), max(s$x), length.out = as.integer(p$max_n))
            s <- data.frame(x = xo, y = stats::approx(s$x, s$y, xo)$y)
            txt <- sprintf("The record was resampled to %d points.", as.integer(p$max_n))
            code <- c(sprintf("xo <- seq(min(dat[, 1]), max(dat[, 1]), length.out = %d)", as.integer(p$max_n)),
                "dat <- data.frame(xo, approx(dat[, 1], dat[, 2], xo)$y)")
          }
          r <- WaverideR::bayes_changepoint_r2(data = as.matrix(s), k_max = as.integer(p$k_max), d_min = as.integer(p$d_min),
              n_sim = as.integer(p$n_sim), seed = as.integer(p$seed), stable = TRUE)
          tab <- data.frame(k_max = r[, 1], r2 = r[, 2])
          o <- list(cd_output(cd_out_name(file, "-bcp_r2"), tab, header = TRUE, var = "tab"))
          code <- c(code, sprintf("res <- WaverideR::bayes_changepoint_r2(data = as.matrix(dat), k_max = %d, d_min = %d, n_sim = %d, seed = %d, stable = TRUE)",
              as.integer(p$k_max), as.integer(p$d_min), as.integer(p$n_sim), as.integer(p$seed)),
              "tab <- data.frame(k_max = res[, 1], r2 = res[, 2])", cd_code_write(o))
          txt <- c(txt, paste0("k = ", tab$k_max, ": R2 = ", cd_fmt_period(tab$r2)))
          list(result = tab, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          graphics::plot(res$k_max, res$r2, type = "b", pch = 16, col = "#1f4e79", xlab = "Largest number of change points",
              ylab = expression(R^2))
          graphics::grid(col = "grey90", lty = 1)
        }),

    cd_tool("mx_noise", "Math", "Red noise (AR(1)) series", group = grp, inputs = 0L, icon = "dice",
        info = "Generates an AR(1) red-noise series with a given mean, standard deviation and lag-1 autocorrelation.",
        uses = "WaverideR::wr_coloured_noise",
        params = list(
          cd_num("n", "Number of points", 1000, min = 2, step = 100),
          cd_num("x0", "First depth / time", 0),
          cd_num("dt", "Sampling interval", 1, min = 0),
          cd_num("mean", "Mean", 0),
          cd_num("sd", "Standard deviation", 1, min = 0),
          cd_num("phi", "Lag-1 autocorrelation (0 = white noise)", 0.7, min = 0, max = 0.999, step = 0.05),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          set.seed(as.integer(p$seed))
          n <- as.integer(p$n)
          y <- WaverideR::wr_coloured_noise(timesteps = n, mean = p$mean, sd = p$sd, phi = p$phi)
          out <- data.frame(x = p$x0 + (seq_len(n) - 1) * p$dt, y = y)
          nm <- sprintf("rednoise_phi%s_n%d.txt", cd_fmt(p$phi), n)
          o <- list(cd_output(nm, out, var = "out"))
          code <- c(sprintf("set.seed(%d)", as.integer(p$seed)),
              sprintf("y <- WaverideR::wr_coloured_noise(timesteps = %d, mean = %s, sd = %s, phi = %s)", n, cd_fmt(p$mean),
                  cd_fmt(p$sd), cd_fmt(p$phi)),
              sprintf("out <- data.frame(x = %s + (seq_len(%d) - 1) * %s, y = y)", cd_fmt(p$x0), n, cd_fmt(p$dt)), cd_code_write(o))
          ac <- stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]
          list(result = out, outputs = o, code = code,
              text = sprintf("%d points; sample mean %s, sd %s, lag-1 autocorrelation %s.", n, cd_fmt(signif(mean(y), 3)),
                  cd_fmt(signif(stats::sd(y), 3)), cd_fmt(signif(ac, 3))))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = "Red noise")
        })
  )
}
