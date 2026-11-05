# Plot and Basic Series menus ####

cd_tools_plot <- function() {
  list(
    cd_tool("sampling", "Plot", "Sampling rate",
        info = "Distribution of the sampling interval (Acycle: Plot > Sampling Rate).",
        uses = "base R",
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          dx <- diff(s$x)
          xm <- (s$x[-1] + s$x[-nrow(s)]) / 2
          txt <- c(sprintf("Number of points: %d", nrow(s)),
              sprintf("Range: %s to %s", cd_fmt(min(s$x)), cd_fmt(max(s$x))),
              sprintf("Sampling interval: min %s, median %s, mean %s, max %s", cd_fmt(min(dx)), cd_fmt(stats::median(dx)),
                  cd_fmt(mean(dx)), cd_fmt(max(dx))),
              if (cd_is_even(s$x)) { "The series is evenly spaced." } else {
                "The series is NOT evenly spaced: interpolate it (Math > Interpolation) before spectral analysis." })
          res <- data.frame(x = xm, dx = dx)
          list(result = res, text = txt,
              code = c("s <- dat[order(dat[, 1]), ]", "res <- data.frame(x = s[-1, 1], dx = diff(s[, 1]))",
                  "summary(res$dx)"))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, nrow = 1), widths = c(2, 1))
          cd_par()
          yl <- range(res$dx)
          if (diff(yl) < 1e-6 * max(abs(yl))) {
            yl <- yl[1] * c(0.5, 1.5)
          }
          graphics::plot(res$x, res$dx, type = "p", pch = 16, cex = 0.4, col = "#1f4e79", xlab = "Depth / time",
              ylab = "Sampling interval", ylim = yl)
          graphics::abline(h = stats::median(res$dx), col = "#c0392b", lty = 2)
          cd_par()
          graphics::hist(signif(res$dx, 8), breaks = if (diff(range(res$dx)) < 1e-6 * max(res$dx)) { 3 } else { 40 }, col = "#9fb7cf",
              border = "white", main = "", xlab = "Sampling interval")
        }),
    cd_tool("distribution", "Plot", "Data distribution",
        info = "Histogram, density and normal Q-Q plot of the data values (Acycle: Plot > Data Distribution).",
        uses = "base R",
        params = list(cd_num("breaks", "Number of histogram bins", 30, min = 5, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          y <- s$y
          m3 <- mean((y - mean(y))^3) / stats::sd(y)^3
          txt <- c(sprintf("n = %d, mean = %s, median = %s, sd = %s", length(y), cd_fmt(mean(y)), cd_fmt(stats::median(y)),
              cd_fmt(stats::sd(y))), sprintf("min = %s, max = %s, skewness = %s", cd_fmt(min(y)), cd_fmt(max(y)), cd_fmt(m3)))
          if (length(y) >= 3 && length(y) <= 5000) {
            sw <- stats::shapiro.test(y)
            txt <- c(txt, sprintf("Shapiro-Wilk normality test: W = %s, p = %s", cd_fmt(sw$statistic), format.pval(sw$p.value)))
          }
          list(result = y, text = txt, code = c("summary(dat[, 2])", "hist(dat[, 2])", "shapiro.test(dat[, 2])"))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, nrow = 1))
          cd_par()
          graphics::hist(res, breaks = p$breaks, freq = FALSE, col = "#9fb7cf", border = "white", main = "", xlab = "Value")
          graphics::lines(stats::density(res), col = "#1f4e79", lwd = 2)
          xx <- seq(min(res), max(res), length.out = 200)
          graphics::lines(xx, stats::dnorm(xx, mean(res), stats::sd(res)), col = "#c0392b", lty = 2)
          cd_par()
          stats::qqnorm(res, pch = 16, cex = 0.5, col = "#1f4e79", main = "")
          stats::qqline(res, col = "#c0392b")
        })
  )
}

cd_tools_basic <- function() {
  sol_choices <- c("La2004 (Laskar et al. 2004)" = "la04", "La2010a" = "la10a", "La2010b" = "la10b",
      "La2010c" = "la10c", "La2010d" = "la10d", "La2011" = "la11")
  list(
    cd_tool("astro_solution", "Basic Series", "Astronomical solution", inputs = 0L,
        info = paste("Eccentricity, obliquity or precession index from an astronomical solution. The solution is",
            "downloaded once by astrochron::getLaskar() and then kept in a local cache."),
        uses = "astrochron::getLaskar, astrochron::etp",
        params = list(
          cd_sel("sol", "Solution", sol_choices),
          cd_sel("what", "Parameter", c("Eccentricity" = "ecc", "Obliquity (radians)" = "obl",
              "Precession index" = "prec", "ETP (standardised sum)" = "etp"),
              help = "Obliquity, precession and ETP are only in La2004."),
          cd_num("tmin", "Start (ka)", 0, min = 0),
          cd_num("tmax", "End (ka)", 2000, min = 1),
          cd_num("dt", "Step (ka)", 1, min = 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          what <- p$what
          sol <- p$sol
          if (what != "ecc" && sol != "la04") {
            stop("obliquity, precession and ETP are only available in La2004", call. = FALSE)
          }
          a <- cd_laskar(sol)
          if (what == "ecc") {
            res <- a[, c(1, grep("^ecc", names(a))[1])]
            code <- sprintf("a <- astrochron::getLaskar(%s)\nres <- a[, c(1, %d)]", cd_dep(sol), grep("^ecc", names(a))[1])
          } else if (what == "obl") {
            res <- a[, c("Time_ka", "tilt_LA04")]
            code <- "a <- astrochron::getLaskar(\"la04\")\nres <- a[, c(\"Time_ka\", \"tilt_LA04\")]"
          } else if (what == "prec") {
            res <- astrochron::etp(tmin = p$tmin, tmax = p$tmax, dt = p$dt, eWt = 0, oWt = 0, pWt = 1, solution = a,
                standardize = FALSE, genplot = FALSE, verbose = FALSE)
            code <- sprintf(paste0("res <- astrochron::etp(tmin = %s, tmax = %s, dt = %s, eWt = 0, oWt = 0, pWt = 1,\n",
                "    solution = astrochron::getLaskar(\"la04\"), standardize = FALSE, genplot = FALSE)"),
                cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$dt))
          } else {
            res <- astrochron::etp(tmin = p$tmin, tmax = p$tmax, dt = p$dt, solution = a, genplot = FALSE, verbose = FALSE)
            code <- sprintf("res <- astrochron::etp(tmin = %s, tmax = %s, dt = %s, solution = astrochron::getLaskar(\"la04\"), genplot = FALSE)",
                cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$dt))
          }
          res <- as.data.frame(res)
          res <- res[res[, 1] >= p$tmin & res[, 1] <= p$tmax, , drop = FALSE]
          if (what %in% c("ecc", "obl") && p$dt > 1) {
            res <- res[(seq_len(nrow(res)) - 1) %% round(p$dt) == 0, , drop = FALSE]
          }
          if (what %in% c("ecc", "obl")) {
            code <- paste0(code, sprintf("\nres <- res[res[, 1] >= %s & res[, 1] <= %s, ]", cd_fmt(p$tmin), cd_fmt(p$tmax)))
          }
          names(res) <- c("time_ka", what)
          nm <- sprintf("%s_%s_%s-%ska.txt", sub("^la", "La", sol), what, cd_fmt(p$tmin), cd_fmt(p$tmax))
          o <- list(cd_output(nm, res))
          list(result = res, outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points, %s to %s ka", nrow(res), cd_fmt(min(res[, 1])), cd_fmt(max(res[, 1]))))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = names(res)[2], xlab = "Time (ka)")
        }),
    cd_tool("insolation", "Basic Series", "Insolation", inputs = 0L,
        info = "Daily mean insolation at the top of the atmosphere (Berger 1978) from La2004.",
        uses = "CycloDesk::cd_insolation (La2004 via astrochron::getLaskar)",
        params = list(
          cd_num("lat", "Latitude (degrees N)", 65, min = -90, max = 90),
          cd_sel("lon", "True solar longitude", c("June solstice (90)" = "90", "September equinox (180)" = "180",
              "December solstice (270)" = "270", "March equinox (0)" = "0", "Other..." = "other")),
          cd_num("lon_other", "True solar longitude (degrees)", 120, min = 0, max = 360,
              show_if = list(param = "lon", values = "other")),
          cd_num("S0", "Solar constant (W/m2)", 1365),
          cd_num("tmin", "Start (ka)", 0, min = 0),
          cd_num("tmax", "End (ka)", 1000, min = 1),
          cd_num("dt", "Step (ka)", 1, min = 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          lon <- if (p$lon == "other") { p$lon_other } else { as.numeric(p$lon) }
          res <- cd_insolation(tmin = p$tmin, tmax = p$tmax, dt = p$dt, lat = p$lat, lon = lon, S0 = p$S0)
          nm <- sprintf("Insolation_La04_%s%s_lon%s_%s-%ska.txt", cd_fmt(abs(p$lat)), if (p$lat >= 0) { "N" } else { "S" },
              cd_fmt(lon), cd_fmt(p$tmin), cd_fmt(p$tmax))
          o <- list(cd_output(nm, res))
          code <- sprintf("res <- CycloDesk::cd_insolation(tmin = %s, tmax = %s, dt = %s, lat = %s, lon = %s, S0 = %s)",
              cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$dt), cd_fmt(p$lat), cd_fmt(lon), cd_fmt(p$S0))
          list(result = res, outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("Mean %s W/m2, range %s to %s W/m2", cd_fmt(mean(res[, 2])), cd_fmt(min(res[, 2])),
                  cd_fmt(max(res[, 2]))))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = "Insolation (W/m2)", xlab = "Time (ka)")
        }),
    cd_tool("etp", "Basic Series", "ETP", inputs = 0L,
        info = "Eccentricity + tilt + precession composite from La2004, each standardised and weighted.",
        uses = "astrochron::etp",
        params = list(
          cd_num("tmin", "Start (ka)", 0, min = 0),
          cd_num("tmax", "End (ka)", 2000, min = 1),
          cd_num("dt", "Step (ka)", 1, min = 0.1),
          cd_num("eWt", "Weight of eccentricity", 1),
          cd_num("oWt", "Weight of obliquity", 1),
          cd_num("pWt", "Weight of precession", 1),
          cd_chk("standardize", "Standardise the result", TRUE)),
        compute = function(d, p, d2, file, file2) {
          res <- astrochron::etp(tmin = p$tmin, tmax = p$tmax, dt = p$dt, eWt = p$eWt, oWt = p$oWt, pWt = p$pWt,
              solution = cd_laskar("la04"), standardize = p$standardize, genplot = FALSE, verbose = FALSE)
          res <- as.data.frame(res)
          names(res) <- c("time_ka", "etp")
          nm <- sprintf("ETP_La04_%s-%ska.txt", cd_fmt(p$tmin), cd_fmt(p$tmax))
          o <- list(cd_output(nm, res))
          code <- sprintf(paste0("res <- astrochron::etp(tmin = %s, tmax = %s, dt = %s, eWt = %s, oWt = %s, pWt = %s,\n",
              "    solution = astrochron::getLaskar(\"la04\"), standardize = %s, genplot = FALSE)"),
              cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$dt), cd_fmt(p$eWt), cd_fmt(p$oWt), cd_fmt(p$pWt), p$standardize)
          list(result = res, outputs = o, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = "ETP", xlab = "Time (ka)")
        }),
    cd_tool("generator", "Basic Series", "Signal / noise generator", inputs = 0L,
        info = "Sum of sinusoids plus white, AR(1) red or power-law noise (Acycle: Basic Series > Signal/Noise Generator).",
        uses = "astrochron::ar1, astrochron::pwrLaw",
        params = list(
          cd_num("npts", "Number of points", 2000, min = 10, step = 1),
          cd_num("dt", "Sampling interval", 1, min = 0),
          cd_txt("periods", "Periods (comma separated, empty = none)", "405, 125, 95, 41, 23, 19", numeric = TRUE),
          cd_txt("amps", "Amplitudes (one per period)", "1, 0.6, 0.5, 0.7, 0.8, 0.5", numeric = TRUE),
          cd_sel("noise", "Noise", c("AR(1) red noise" = "ar1", "White noise" = "white",
              "Power-law noise" = "pwr", "None" = "none")),
          cd_num("rho", "AR(1) coefficient rho", 0.7, min = 0, max = 0.999, step = 0.05,
              show_if = list(param = "noise", values = "ar1")),
          cd_num("beta", "Power-law exponent beta", 1, min = 0, max = 4, step = 0.1,
              show_if = list(param = "noise", values = "pwr")),
          cd_num("nsd", "Noise standard deviation", 1, min = 0, show_if = list(param = "noise", values = c("ar1", "white", "pwr"))),
          cd_num("seed", "Random seed", 1, step = 1),
          cd_txt("name", "File name", "synthetic_series")),
        compute = function(d, p, d2, file, file2) {
          set.seed(p$seed)
          x <- (seq_len(p$npts) - 1) * p$dt
          y <- numeric(p$npts)
          per <- p$periods
          amp <- p$amps
          if (length(per)) {
            amp <- rep(amp, length.out = length(per))
            for (k in seq_along(per)) {
              y <- y + amp[k] * sin(2 * pi * x / per[k])
            }
          }
          if (p$noise == "ar1") {
            nz <- astrochron::ar1(npts = p$npts, dt = p$dt, rho = p$rho, sdev = 1, genplot = FALSE, verbose = FALSE)[, 2]
          } else if (p$noise == "pwr") {
            nz <- astrochron::pwrLaw(npts = p$npts, dt = p$dt, beta = p$beta, sdev = 1, genplot = FALSE, verbose = FALSE)[, 2]
          } else if (p$noise == "white") {
            nz <- stats::rnorm(p$npts)
          } else {
            nz <- numeric(p$npts)
          }
          if (p$noise != "none") {
            nz <- (nz - mean(nz)) / stats::sd(nz) * p$nsd
          }
          res <- data.frame(x = x, y = y + nz)
          nm <- paste0(gsub("[^A-Za-z0-9_.-]", "_", p$name), ".txt")
          o <- list(cd_output(nm, res))
          code <- c(sprintf("set.seed(%s)", cd_fmt(p$seed)),
              sprintf("x <- (seq_len(%d) - 1) * %s", as.integer(p$npts), cd_fmt(p$dt)),
              sprintf("per <- %s", cd_dep(per)), sprintf("amp <- %s", cd_dep(amp)),
              "y <- numeric(length(x))", "for (k in seq_along(per)) {", "  y <- y + amp[k] * sin(2 * pi * x / per[k])", "}",
              switch(p$noise,
                  ar1 = sprintf("nz <- astrochron::ar1(npts = %d, dt = %s, rho = %s, genplot = FALSE)[, 2]", as.integer(p$npts),
                      cd_fmt(p$dt), cd_fmt(p$rho)),
                  pwr = sprintf("nz <- astrochron::pwrLaw(npts = %d, dt = %s, beta = %s, genplot = FALSE)[, 2]",
                      as.integer(p$npts), cd_fmt(p$dt), cd_fmt(p$beta)),
                  white = sprintf("nz <- rnorm(%d)", as.integer(p$npts)),
                  none = "nz <- 0"),
              if (p$noise != "none") { sprintf("nz <- (nz - mean(nz)) / sd(nz) * %s", cd_fmt(p$nsd)) } else { NULL },
              "res <- data.frame(x = x, y = y + nz)", cd_code_write(o))
          list(result = res, outputs = o, code = code)
        },
        plot = function(res, p) {
          cd_plot_series(list(res), labels = "Synthetic series", xlab = "Depth / time")
        }),
    cd_tool("examples", "Basic Series", "Example data", inputs = 0L,
        info = "Writes an example data set of WaverideR into the working folder.",
        uses = "WaverideR data sets",
        params = list(cd_sel("name", "Data set", cd_examples())),
        compute = function(d, p, d2, file, file2) {
          res <- cd_example_data(p$name)
          o <- list(cd_output(paste0(p$name, ".txt"), res, header = TRUE))
          code <- c(sprintf("res <- WaverideR::%s", p$name), sprintf("CycloDesk::write_series(res, \"%s.txt\", header = TRUE)", p$name))
          list(result = res, outputs = o, code = code,
              text = sprintf("%d rows, columns: %s", nrow(res), paste(names(res), collapse = ", ")))
        },
        plot = function(res, p) {
          cd_plot_series(list(res[, 1:2]), labels = names(res)[2], xlab = names(res)[1])
        })
  )
}
