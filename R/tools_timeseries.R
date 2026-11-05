# Time Series menu ####

cd_even_note <- function(d, s) {
  if (nrow(s) != nrow(cd_clean(d)) || !cd_is_even(cd_clean(d)$x)) {
    return(sprintf("Note: the series was not evenly spaced; it was linearly interpolated at %s (median interval).",
        cd_fmt(cd_dt(s$x))))
  }
  character()
}

cd_even_code <- function(d) {
  s <- cd_clean(d)
  if (!cd_is_even(s$x)) {
    return(sprintf("dat <- astrochron::linterp(dat, dt = %s, genplot = FALSE)", cd_fmt(cd_dt(s$x))))
  }
  character()
}

# Frequency of the highest periodogram peak (used as the default filter band)
cd_peak_freq <- function(d) {
  s <- cd_even(d)
  n <- nrow(s)
  y <- s$y - mean(s$y)
  y <- y - stats::fitted(stats::lm(y ~ s$x))
  npad <- 4 * n
  sp <- Mod(stats::fft(c(y, numeric(npad - n))))^2
  dt <- cd_dt(s$x)
  f <- (seq_len(npad) - 1) / (npad * dt)
  keep <- f > 3 / (n * dt) & f < 0.5 / dt
  f[keep][which.max(sp[keep])]
}

cd_nyq <- function(d) {
  signif(0.5 / cd_dt(d[, 1]), 4)
}

cd_tools_timeseries <- function() {
  dt_default <- function(d) {
    signif(cd_dt(d[, 1]), 4)
  }
  len_default <- function(frac) {
    function(d) {
      signif(diff(range(d[, 1], na.rm = TRUE)) * frac, 3)
    }
  }
  flow_default <- function(d) {
    signif(cd_peak_freq(d) * 0.8, 3)
  }
  fhigh_default <- function(d) {
    signif(cd_peak_freq(d) * 1.2, 3)
  }
  wavelets <- c("Morlet" = "morlet", "Morlet, zero mean" = "morlet0", "Paul" = "paul", "Generalised Morse" = "morse",
      "Bump" = "bump", "Complex Morlet (cmor)" = "cmor", "Complex Gaussian (cgau)" = "cgau",
      "Complex Mexican hat" = "cmhat", "Shannon" = "shannon", "Frequency B-spline" = "fbsp", "Meyer" = "meyer",
      "Log-Gabor" = "loggabor", "Gammatone" = "gammatone", "Hann" = "hann", "DOG (real)" = "dog",
      "Real Morlet" = "morl", "Hermitian hat" = "hhat", "Poisson" = "poisson", "Haar" = "haar", "Daubechies" = "db",
      "Symlets" = "sym", "Battle-Lemarie" = "battle")
  list(
    cd_tool("detrend", "Time Series", "Detrending", group = "Pre-processing",
        info = "Removes a linear, LOWESS, polynomial or automatic-span LOESS trend (Acycle: Detrending).",
        uses = "astrochron::detrend, astrochron::noLow, WaverideR::loess_auto",
        params = list(
          cd_sel("method", "Trend", c("Linear" = "linear", "LOWESS" = "lowess", "Polynomial" = "poly",
              "LOESS with automatic span (WaverideR)" = "loess_auto")),
          cd_num("span", "LOWESS span (fraction of the record)", 0.35, min = 0.01, max = 1, step = 0.05,
              show_if = list(param = "method", values = "lowess")),
          cd_num("order", "Polynomial order", 2, min = 1, max = 10, step = 1,
              show_if = list(param = "method", values = "poly")),
          cd_chk("write_trend", "Also write the trend", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          names(s) <- names(d)[1:2]
          if (p$method == "linear") {
            tr <- astrochron::detrend(s, output = 2, genplot = FALSE, verbose = FALSE)
            code <- "trend <- astrochron::detrend(dat, output = 2, genplot = FALSE)"
            sfx <- "-rLin"
          } else if (p$method == "lowess") {
            tr <- astrochron::noLow(s, smooth = p$span, output = 2, genplot = FALSE, verbose = FALSE)
            code <- sprintf("trend <- astrochron::noLow(dat, smooth = %s, output = 2, genplot = FALSE)", cd_fmt(p$span))
            sfx <- paste0("-rLowess", cd_fmt(p$span))
          } else if (p$method == "poly") {
            fit <- stats::lm(s[, 2] ~ stats::poly(s[, 1], degree = p$order, raw = FALSE))
            tr <- data.frame(s[, 1], stats::fitted(fit))
            code <- sprintf("trend <- data.frame(dat[, 1], fitted(lm(dat[, 2] ~ poly(dat[, 1], %d))))", as.integer(p$order))
            sfx <- paste0("-rPoly", as.integer(p$order))
          } else {
            la <- WaverideR::loess_auto(time_series = as.matrix(s), genplot = FALSE)
            tr <- data.frame(s[, 1], la[, 2])
            code <- "trend <- WaverideR::loess_auto(time_series = as.matrix(dat), genplot = FALSE)"
            sfx <- "-rLoessAuto"
          }
          tr <- as.data.frame(tr)
          res <- data.frame(s[, 1], s[, 2] - tr[, 2])
          names(res) <- names(s)
          names(tr) <- c(names(s)[1], "trend")
          code <- c(code, "res <- data.frame(dat[, 1], dat[, 2] - trend[, 2])")
          o <- list(cd_output(cd_out_name(file, sfx), res))
          if (p$write_trend) {
            o[[2]] <- cd_output(cd_out_name(file, sub("^-r", "-trend", sfx)), tr, var = "trend")
          }
          list(result = list(before = s, trend = tr, after = res), outputs = o, code = c(code, cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "input and trend", "detrended", extra_top = res$trend, extra_lab = "trend")
        }),
    cd_tool("prewhiten", "Time Series", "Pre-whitening", group = "Pre-processing",
        info = "Removes the AR(1) or AR(p) red-noise structure (Acycle: Pre-whitening).",
        uses = "astrochron::prewhiteAR1, astrochron::prewhiteAR",
        params = list(
          cd_sel("method", "Model", c("AR(1)" = "ar1", "AR(p), order chosen by AIC" = "arp")),
          cd_num("order", "Maximum AR order", 5, min = 1, step = 1, show_if = list(param = "method", values = "arp"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          if (p$method == "ar1") {
            res <- astrochron::prewhiteAR1(s, genplot = FALSE, verbose = FALSE)
            code <- "res <- astrochron::prewhiteAR1(dat, genplot = FALSE)"
            sfx <- "-AR1"
          } else {
            res <- astrochron::prewhiteAR(s, order = p$order, aic = TRUE, genplot = FALSE, verbose = FALSE)
            code <- sprintf("res <- astrochron::prewhiteAR(dat, order = %d, aic = TRUE, genplot = FALSE)", as.integer(p$order))
            sfx <- "-ARp"
          }
          res <- as.data.frame(res)
          o <- list(cd_output(cd_out_name(file, sfx), res))
          list(result = list(before = s, after = res), outputs = o, text = cd_even_note(d, s),
              code = c(cd_even_code(d), code, cd_code_write(o)))
        },
        plot = function(res, p) {
          cd_two_panel(res$before, res$after, "input", "pre-whitened")
        }),
    cd_tool("spectrum", "Time Series", "Spectral analysis", group = "Spectral analysis",
        info = paste("Multitaper (MTM) spectrum with AR(1), robust red-noise (ML96), LOWSPEC or power-law confidence",
            "levels; periodogram; maximum entropy (MESA) or adaptive multitaper (AMTM) of WaverideR."),
        uses = "astrochron::mtm / mtmML96 / lowspec / mtmPL / periodogram, WaverideR::mesa_spectrum / amtm_spectrum",
        params = list(
          cd_sel("method", "Method", c("MTM, AR(1) red noise" = "mtm", "MTM, robust red noise (Mann & Lees 1996)" = "mtmML96",
              "LOWSPEC (Meyers 2012)" = "lowspec", "MTM, power-law noise" = "mtmPL", "Periodogram" = "periodogram",
              "MESA, maximum entropy (WaverideR)" = "mesa", "Adaptive MTM with noise models (WaverideR)" = "amtm",
              "Spectral whitening background, SWA (WaverideR)" = "swa")),
          cd_num("tbw", "Time-bandwidth product (NW)", 2, min = 1, max = 10, step = 0.5,
              show_if = list(param = "method", values = c("mtm", "mtmML96", "lowspec", "mtmPL", "amtm", "swa"))),
          cd_num("padfac", "Padding factor", 5, min = 1, step = 1,
              show_if = list(param = "method", values = c("mtm", "mtmML96", "lowspec", "mtmPL", "periodogram"))),
          cd_chk("detrend", "Remove linear trend", TRUE),
          cd_num("fmax", "Maximum frequency shown (cycles/unit)", cd_nyq, min = 0,
              help = "Default: the Nyquist frequency, 1 / (2 x sampling interval)."),
          cd_sel("xaxis", "Horizontal axis", c("Frequency" = "frequency", "Period" = "period")),
          cd_chk("logy", "Logarithmic power axis", TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          if (p$detrend && p$method %in% c("mesa", "amtm", "swa")) {
            s$y <- s$y - stats::fitted(stats::lm(s$y ~ s$x))
          }
          m <- p$method
          cl <- NULL
          cln <- NULL
          raw <- NULL
          tb <- cd_fmt(p$tbw)
          if (m %in% c("mtm", "mtmML96", "lowspec", "mtmPL")) {
            fn <- switch(m, mtm = astrochron::mtm, mtmML96 = astrochron::mtmML96, lowspec = astrochron::lowspec,
                mtmPL = astrochron::mtmPL)
            r <- fn(s, tbw = p$tbw, padfac = p$padfac, detrend = p$detrend, output = 1, genplot = FALSE, verbose = FALSE)
            r <- as.data.frame(r)
            pcol <- if (m == "lowspec") { "Prewhite_power" } else { "Power" }
            pre <- switch(m, mtm = "AR1", mtmML96 = "AR1", lowspec = "LOWSPEC", mtmPL = "PowerLaw")
            fitcol <- switch(m, mtm = "AR1_fit", mtmML96 = "AR1_fit", lowspec = "LOWSPEC_back", mtmPL = "PowerLaw_fit")
            cols <- c(fitcol, paste0(pre, c("_90_power", "_95_power", "_99_power")))
            cl <- r[, cols]
            cln <- c("background", "90%", "95%", "99%")
            spec <- data.frame(frequency = r$Frequency, power = r[[pcol]], r[, cols], harmonic_F_CL = r$Harmonic_CL)
            code <- sprintf("res <- astrochron::%s(dat, tbw = %s, padfac = %s, detrend = %s, output = 1, genplot = FALSE)",
                m, tb, cd_fmt(p$padfac), p$detrend)
          } else if (m == "periodogram") {
            r <- as.data.frame(astrochron::periodogram(s, padfac = p$padfac, detrend = p$detrend, output = 1, genplot = FALSE,
                verbose = FALSE))
            spec <- data.frame(frequency = r$Frequency, power = r$Power, amplitude = r$Amplitude, phase = r$Phase)
            code <- sprintf("res <- astrochron::periodogram(dat, padfac = %s, detrend = %s, output = 1, genplot = FALSE)",
                cd_fmt(p$padfac), p$detrend)
          } else if (m == "swa") {
            r <- WaverideR::swa_spectrum(data = as.matrix(s), nw = p$tbw)
            raw <- r
            cl <- cbind(r$swa, r$chi2_cl[, 1:3, drop = FALSE])
            cln <- c("SWA background", "90%", "95%", "99%")
            spec <- data.frame(frequency = r$freq, power = r$power, background = r$swa, r$chi2_cl[, 1:3, drop = FALSE])
            code <- sprintf("res <- WaverideR::swa_spectrum(data = as.matrix(dat), nw = %s)", tb)
          } else if (m == "mesa") {
            r <- WaverideR::mesa_spectrum(data = as.matrix(s))
            raw <- r
            spec <- data.frame(frequency = r$spectrum$frequency, power = r$spectrum$power)
            code <- "res <- WaverideR::mesa_spectrum(data = as.matrix(dat))"
          } else {
            r <- WaverideR::amtm_spectrum(data = as.matrix(s), nw = p$tbw)
            raw <- r
            spec <- data.frame(frequency = r$freq, power = r$psd)
            if (!is.null(r$background_selected) && length(r$background_selected) == length(r$freq)) {
              spec$background <- r$background_selected
            }
            code <- sprintf("res <- WaverideR::amtm_spectrum(data = as.matrix(dat), nw = %s)\nWaverideR::plot_amtm_spectrum(res)", tb)
          }
          if (p$detrend && m %in% c("mesa", "amtm", "swa")) {
            code <- c("dat[, 2] <- residuals(lm(dat[, 2] ~ dat[, 1]))", code)
          }
          sfx <- paste0("-", m, if (m %in% c("mtm", "mtmML96", "lowspec", "mtmPL", "amtm", "swa")) { tb } else { "" })
          o <- list(cd_output(cd_out_name(file, sfx), spec, header = TRUE,
              var = if (m %in% c("mesa", "amtm", "swa")) { "res_table" } else { "res" }))
          # Significant peaks
          txt <- cd_even_note(d, s)
          if (!is.null(cl)) {
            sig <- cd_peaks(spec$power, cl[, 3])
            if (length(sig)) {
              sig <- cd_spread_peaks(sig, spec$power, spec$frequency, 0.01 * max(spec$frequency), 12)
              txt <- c(txt, "Peaks above the 95% confidence level (period, frequency):",
                  paste0("  ", cd_fmt_period(1 / spec$frequency[sig]), "  (f = ", cd_fmt_period(spec$frequency[sig]), ")"))
            } else {
              txt <- c(txt, "No peaks above the 95% confidence level.")
            }
          } else {
            pk <- cd_peaks(spec$power)
            pk <- cd_spread_peaks(pk, spec$power, spec$frequency, 0.01 * max(spec$frequency), 10)
            txt <- c(txt, "Highest peaks (period):", paste0("  ", cd_fmt_period(1 / spec$frequency[pk])))
          }
          if (m %in% c("mesa", "amtm", "swa")) {
            code <- c(code, "res_table <- data.frame(frequency = res[[1]], power = res[[2]])")
            if (m == "swa") {
              code[length(code)] <- "res_table <- data.frame(frequency = res$freq, power = res$power, background = res$swa, res$chi2_cl)"
            } else if (m == "mesa") {
              code[length(code)] <- "res_table <- res$spectrum[, c(\"frequency\", \"power\")]"
            } else {
              code[length(code)] <- "res_table <- data.frame(frequency = res$freq, power = res$psd)"
            }
          }
          list(result = list(spec = spec, cl = cl, cln = cln, raw = raw, method = m), outputs = o, text = txt,
              code = c(cd_even_code(d), code, cd_code_write(o)))
        },
        plot = function(res, p) {
          if (res$method == "amtm" && !is.null(res$raw)) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            WaverideR::plot_amtm_spectrum(res$raw, xlim = c(0, p$fmax))
            return(invisible(NULL))
          }
          sp <- res$spec
          above <- if (is.null(res$cl)) { NULL } else { res$cl[, 3] }
          cd_plot_spectrum(sp$frequency, sp$power, cl = res$cl, cl_names = res$cln, xaxis = p$xaxis, logy = p$logy,
              fmax = p$fmax, peaks_above = above, ylab = if (res$method == "lowspec") { "Pre-whitened power" } else { "Power" })
        }),
    cd_tool("evolutive", "Time Series", "Evolutionary spectral analysis", group = "Spectral analysis", heavy = TRUE,
        info = paste("Sliding-window spectra: evolutive harmonic analysis (EHA, multitaper), EHA on a log2 period axis,",
            "windowed FFT or evolutive maximum entropy (MESA)."),
        uses = "astrochron::eha, WaverideR::eha_log2 / win_fft / mesa_evolutive",
        params = list(
          cd_sel("method", "Method", c("EHA, multitaper (astrochron)" = "eha", "EHA on a log2 period axis (WaverideR)" = "eha_log2",
              "Windowed FFT (WaverideR)" = "winfft", "Evolutive MESA (WaverideR)" = "mesa")),
          cd_num("win", "Window length (depth / time units)", len_default(0.15), min = 0),
          cd_num("step", "Step (depth / time units)", function(d) { signif(5 * cd_dt(d[, 1]), 3) }, min = 0,
              show_if = list(param = "method", values = c("eha", "mesa"))),
          cd_num("tbw", "Time-bandwidth product (NW)", 2, min = 1, step = 0.5,
              show_if = list(param = "method", values = c("eha", "eha_log2"))),
          cd_num("fmax", "Maximum frequency (cycles/unit)", cd_nyq, min = 0,
              show_if = list(param = "method", values = c("eha", "winfft", "mesa"))),
          cd_num("lowerPeriod", "Shortest period", function(d) { signif(3 * cd_dt(d[, 1]), 3) }, min = 0,
              show_if = list(param = "method", values = "eha_log2")),
          cd_num("upperPeriod", "Longest period", len_default(0.1), min = 0, show_if = list(param = "method", values = "eha_log2")),
          cd_chk("logp", "Logarithmic power", TRUE, show_if = list(param = "method", values = c("eha", "winfft"))),
          cd_chk("norm", "Normalise each window to its maximum", FALSE, show_if = list(param = "method", values = c("eha", "winfft"))),
          cd_chk("swap", "Depth / time on the vertical axis (Acycle style)", FALSE,
              show_if = list(param = "method", values = c("eha", "winfft"))),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          if (p$method == "eha_log2") {
            r <- WaverideR::eha_log2(data = s, win = p$win, tbw = p$tbw, demean = TRUE, detrend = TRUE,
                lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, pad = max(1000, 4 * round(p$win / cd_dt(s$x))))
            code <- sprintf(paste0("res <- WaverideR::eha_log2(data = dat, win = %s, tbw = %s, demean = TRUE, detrend = TRUE,\n",
                "    lowerPeriod = %s, upperPeriod = %s, pad = %d)\nWaverideR::plot_eha_log2(res, dev_new = FALSE)"), cd_fmt(p$win),
                cd_fmt(p$tbw), cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), as.integer(max(1000, 4 * round(p$win / cd_dt(s$x)))))
            return(list(result = list(raw = r, method = "eha_log2"), outputs = list(), text = cd_even_note(d, s),
                code = c(cd_even_code(d), code)))
          }
          if (p$method == "mesa") {
            r <- WaverideR::mesa_evolutive(data = as.matrix(s), window = p$win, step = p$step, fmax = p$fmax)
            code <- sprintf("res <- WaverideR::mesa_evolutive(data = as.matrix(dat), window = %s, step = %s, fmax = %s)\nWaverideR::plot_mesa_evolutive(res)",
                cd_fmt(p$win), cd_fmt(p$step), cd_fmt(p$fmax))
            return(list(result = list(raw = r, method = "mesa"), outputs = list(), text = cd_even_note(d, s),
                code = c(cd_even_code(d), code)))
          }
          if (p$method == "eha") {
            r <- astrochron::eha(s, tbw = p$tbw, win = p$win, step = p$step, fmax = p$fmax, output = 2, genplot = FALSE,
                verbose = FALSE)
            r <- as.data.frame(r)
            f <- r[, 1]
            x <- as.numeric(sub("^X", "", names(r)[-1]))
            z <- t(as.matrix(r[, -1]))
            code <- sprintf("res <- astrochron::eha(dat, tbw = %s, win = %s, step = %s, fmax = %s, output = 2, genplot = FALSE)",
                cd_fmt(p$tbw), cd_fmt(p$win), cd_fmt(p$step), cd_fmt(p$fmax))
          } else {
            r <- WaverideR::win_fft(data = as.matrix(s), window_size = p$win, freq_max = p$fmax, genplot = FALSE)
            z <- as.matrix(r$Power_mat)
            x <- r$depth
            f <- r$y_axis
            if (nrow(z) != length(x)) {
              z <- t(z)
            }
            code <- sprintf("res <- WaverideR::win_fft(data = as.matrix(dat), window_size = %s, freq_max = %s)",
                cd_fmt(p$win), cd_fmt(p$fmax))
          }
          keep <- f <= p$fmax
          f <- f[keep]
          z <- z[, keep, drop = FALSE]
          if (p$norm) {
            for (i in seq_len(nrow(z))) {
              z[i, ] <- z[i, ] / max(z[i, ], na.rm = TRUE)
            }
          }
          list(result = list(x = x, f = f, z = z, method = p$method), outputs = list(), text = cd_even_note(d, s),
              code = c(cd_even_code(d), code))
        },
        plot = function(res, p) {
          pal <- cd_pal_split(p$palette)
          if (identical(res$method, "eha_log2")) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            WaverideR::plot_eha_log2(res$raw, palette_name = pal$name, color_brewer = pal$family, dev_new = FALSE)
            return(invisible(NULL))
          }
          if (identical(res$method, "mesa")) {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            WaverideR::plot_mesa_evolutive(res$raw, colors = cd_palette(100, p$palette))
            return(invisible(NULL))
          }
          cd_plot_image(res$x, res$f, res$z, log_power = isTRUE(p$logp), swap = isTRUE(p$swap),
              zlab = if (isTRUE(p$norm)) { "Normalised power" } else { "Power" }, palette = p$palette)
        }),
    cd_tool("wavelet", "Time Series", "Wavelet / superlet / synchrosqueezing", group = "Spectral analysis",
        heavy = function(p) { isTRUE(p$pval) || !identical(p$method, "cwt") },
        info = paste("Continuous wavelet transform with 22 mother wavelets, superlet transform or synchrosqueezed",
            "wavelet transform (WaverideR). Significance against red noise is optional for the CWT."),
        uses = "WaverideR::analyse_wavelet / analyse_superlet / analyse_wavelet_sst, WaverideR::plot_wavelet",
        params = list(
          cd_sel("method", "Transform", c("Continuous wavelet transform (CWT)" = "cwt", "Superlet transform" = "superlet",
              "Synchrosqueezed wavelet transform (SST)" = "sst")),
          cd_sel("wavelet", "Mother wavelet", wavelets, show_if = list(param = "method", values = c("cwt", "sst"))),
          cd_num("lowerPeriod", "Shortest period", function(d) { signif(3 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_num("upperPeriod", "Longest period", len_default(0.33), min = 0),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 8, min = 1, step = 1,
              show_if = list(param = "method", values = c("cwt", "sst"))),
          cd_num("dj", "Scale resolution dj (smaller = more periods, slower)", cd_dj_default, min = 0.001, step = 0.005,
              show_if = list(param = "method", values = c("cwt", "sst"))),
          cd_num("Nf", "Number of periods", 200, min = 20, step = 10, show_if = list(param = "method", values = "superlet")),
          cd_num("c1", "Base number of cycles c1", 3, min = 1, step = 0.5, show_if = list(param = "method", values = "superlet")),
          cd_txt("o", "Superlet order (min, max)", "1, 30", numeric = TRUE, show_if = list(param = "method", values = "superlet")),
          cd_chk("pval", "Significance against red noise (slower)", FALSE, show_if = list(param = "method", values = "cwt")),
          cd_num("n_sim", "Number of red-noise simulations", 50, min = 5, step = 5,
              show_if = list(param = "pval", values = "true")),
          cd_chk("add_avg", "Show the average spectrum", TRUE),
          cd_chk("add_data", "Show the data", TRUE),
          cd_txt("periodlab", "Period axis label", "Period (metres)"),
          cd_txt("x_lab", "Depth axis label", "Depth (metres)"),
          cd_sel("palette", "Colour palette", cd_palettes()),
          cd_sel("scale", "Colour scale", c("Default (quantiles)" = "default", "Quantiles" = "quantile", "Logarithmic" = "log",
              "Square root" = "sqrt", "Linear" = "linear", "Automatic" = "auto")),
          cd_chk("whiten", "Whiten (equal contrast at every period)", FALSE, show_if = list(param = "method",
              values = c("cwt", "sst")))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dm <- as.matrix(s)
          if (p$method == "cwt") {
            r <- WaverideR::analyse_wavelet(data = dm, dj = p$dj, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
                omega_nr = p$omega_nr, wavelet = p$wavelet, pval = p$pval, n_simulations = p$n_sim)
            code <- sprintf(paste0("res <- WaverideR::analyse_wavelet(data = as.matrix(dat), dj = %s, lowerPeriod = %s,\n",
                "    upperPeriod = %s, omega_nr = %s, wavelet = %s, pval = %s, n_simulations = %d)"), cd_fmt(p$dj),
                cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr), cd_dep(p$wavelet), p$pval, as.integer(p$n_sim))
            plotfn <- "plot_wavelet"
          } else if (p$method == "superlet") {
            o <- p$o
            if (!length(o)) {
              o <- c(1, 30)
            }
            r <- WaverideR::analyse_superlet(data = dm, upperPeriod = p$upperPeriod, lowerPeriod = p$lowerPeriod,
                Nf = p$Nf, c1 = p$c1, o = o)
            code <- sprintf(paste0("res <- WaverideR::analyse_superlet(data = as.matrix(dat), upperPeriod = %s, lowerPeriod = %s,\n",
                "    Nf = %d, c1 = %s, o = %s)"), cd_fmt(p$upperPeriod), cd_fmt(p$lowerPeriod), as.integer(p$Nf), cd_fmt(p$c1),
                cd_dep(o))
            plotfn <- "plot_superlet"
          } else {
            r <- WaverideR::analyse_wavelet_sst(data = dm, dj = p$dj, lowerPeriod = p$lowerPeriod,
                upperPeriod = p$upperPeriod, omega_nr = p$omega_nr, wavelet = p$wavelet)
            code <- sprintf(paste0("res <- WaverideR::analyse_wavelet_sst(data = as.matrix(dat), dj = %s, lowerPeriod = %s,\n",
                "    upperPeriod = %s, omega_nr = %s, wavelet = %s)"), cd_fmt(p$dj), cd_fmt(p$lowerPeriod),
                cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr), cd_dep(p$wavelet))
            plotfn <- "plot_wavelet"
          }
          avg <- NULL
          if (!is.null(r$Power.avg) && !is.null(r$Period)) {
            avg <- data.frame(period = r$Period, power_avg = r$Power.avg)
          }
          o <- list()
          if (!is.null(avg)) {
            o[[1]] <- cd_output(cd_out_name(file, paste0("-", p$method, "_avg")), avg, header = TRUE, var = "avg")
            code <- c(code, "avg <- data.frame(period = res$Period, power_avg = res$Power.avg)")
          }
          code <- c(cd_even_code(d), code,
              sprintf("WaverideR::%s(res, periodlab = %s, x_lab = %s, add_avg = %s, palette_name = %s, color_brewer = %s, dev_new = FALSE)",
                  plotfn, cd_dep(p$periodlab), cd_dep(p$x_lab), p$add_avg, cd_dep(cd_pal_split(p$palette)$name),
                  cd_dep(cd_pal_split(p$palette)$family)), cd_code_write(o))
          txt <- cd_even_note(d, s)
          if (!is.null(avg)) {
            pk <- cd_peaks(avg$power_avg)
            pk <- pk[order(-avg$power_avg[pk])][seq_len(min(8, length(pk)))]
            txt <- c(txt, sprintf("Peaks of the average spectrum (period): %s", paste(cd_fmt_period(avg$period[pk]),
                collapse = ", ")))
          }
          r <- cd_wt_slim(r)
          list(result = r, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          pal <- cd_pal_split(p$palette)
          sc <- cd_scale_arg(p$scale)
          if (p$method == "superlet") {
            WaverideR::plot_superlet(res, periodlab = p$periodlab, x_lab = p$x_lab, add_avg = p$add_avg,
                add_data = p$add_data, dev_new = FALSE, palette_name = pal$name, color_brewer = pal$family,
                scale = if (is.null(sc)) { "auto" } else { sc })
          } else {
            WaverideR::plot_wavelet(res, periodlab = p$periodlab, x_lab = p$x_lab, add_avg = p$add_avg,
                add_data = p$add_data, add_pval = isTRUE(p$pval) && p$method == "cwt", dev_new = FALSE,
                palette_name = pal$name, color_brewer = pal$family, scale = sc, whiten = isTRUE(p$whiten))
          }
        }),
    cd_tool("coherence", "Time Series", "Wavelet coherence (2 series)", group = "Spectral analysis", heavy = TRUE,
        inputs = 2L,
        info = "Wavelet coherence and phase of two series; series 2 is interpolated onto the depths of series 1.",
        uses = "WaverideR::analyse_wavelet_coherence, WaverideR::plot_wavelet_coherence",
        params = list(
          cd_num("lowerPeriod", "Shortest period", function(d) { signif(3 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_num("upperPeriod", "Longest period", len_default(0.33), min = 0),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 8, min = 1, step = 1),
          cd_num("dj", "Scale resolution dj", 0.02, min = 0.005, step = 0.005),
          cd_num("n_sim", "Number of simulations", 10, min = 1, step = 1),
          cd_txt("periodlab", "Period axis label", "Period (metres)"),
          cd_txt("x_lab", "Depth axis label", "Depth (metres)"),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s1 <- cd_even(d)
          s2 <- cd_clean(d2)
          lo <- max(min(s1$x), min(s2$x))
          hi <- min(max(s1$x), max(s2$x))
          s1 <- s1[s1$x >= lo & s1$x <= hi, ]
          if (nrow(s1) < 20) {
            stop("the two series overlap in fewer than 20 points", call. = FALSE)
          }
          s2i <- data.frame(x = s1$x, y = stats::approx(s2$x, s2$y, s1$x)$y)
          r <- WaverideR::analyse_wavelet_coherence(data_1 = as.matrix(s1), data_2 = as.matrix(s2i), dj = p$dj,
              lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, omega_nr = p$omega_nr, n_simulations = p$n_sim)
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf("dat <- dat[dat[, 1] >= %s & dat[, 1] <= %s, ]", cd_fmt(lo), cd_fmt(hi)),
              "dat2 <- data.frame(dat[, 1], approx(dat2[, 1], dat2[, 2], dat[, 1])$y)",
              sprintf(paste0("res <- WaverideR::analyse_wavelet_coherence(data_1 = as.matrix(dat), data_2 = as.matrix(dat2),\n",
                  "    dj = %s, lowerPeriod = %s, upperPeriod = %s, omega_nr = %s, n_simulations = %d)"), cd_fmt(p$dj),
                  cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr), as.integer(p$n_sim)),
              "WaverideR::plot_wavelet_coherence(res, dev_new = FALSE)")
          list(result = r, outputs = list(), code = code,
              text = sprintf("Overlap %s to %s, %d points", cd_fmt(lo), cd_fmt(hi), nrow(s1)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          pal <- cd_pal_split(p$palette)
          WaverideR::plot_wavelet_coherence(res, periodlab = p$periodlab, x_lab = p$x_lab, dev_new = FALSE,
              palette_name = pal$name, color_brewer = pal$family)
        }),
    cd_tool("xwavelet", "Time Series", "Cross wavelet (2 series)", group = "Spectral analysis", heavy = TRUE,
        inputs = 2L,
        info = "Cross-wavelet power and phase of two series; series 2 is interpolated onto the depths of series 1.",
        uses = "WaverideR::analyse_Xwavelet, WaverideR::plot_Xwavelet",
        params = list(
          cd_num("lowerPeriod", "Shortest period", function(d) { signif(3 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_num("upperPeriod", "Longest period", len_default(0.33), min = 0),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 6, min = 1, step = 1),
          cd_num("dj", "Scale resolution dj", cd_dj_default, min = 0.005, step = 0.005),
          cd_txt("periodlab", "Period axis label", "Period (metres)"),
          cd_txt("x_lab", "Depth axis label", "Depth (metres)"),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s1 <- cd_even(d)
          s2 <- cd_clean(d2)
          lo <- max(min(s1$x), min(s2$x))
          hi <- min(max(s1$x), max(s2$x))
          s1 <- s1[s1$x >= lo & s1$x <= hi, ]
          if (nrow(s1) < 20) {
            stop("the two series overlap in fewer than 20 points", call. = FALSE)
          }
          s2i <- data.frame(x = s1$x, y = stats::approx(s2$x, s2$y, s1$x)$y)
          r <- WaverideR::analyse_Xwavelet(data_1 = as.matrix(s1), data_2 = as.matrix(s2i), dj = p$dj,
              lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, omega_nr = p$omega_nr)
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf("dat <- dat[dat[, 1] >= %s & dat[, 1] <= %s, ]", cd_fmt(lo), cd_fmt(hi)),
              "dat2 <- data.frame(dat[, 1], approx(dat2[, 1], dat2[, 2], dat[, 1])$y)",
              sprintf(paste0("res <- WaverideR::analyse_Xwavelet(data_1 = as.matrix(dat), data_2 = as.matrix(dat2), dj = %s,\n",
                  "    lowerPeriod = %s, upperPeriod = %s, omega_nr = %s)"), cd_fmt(p$dj), cd_fmt(p$lowerPeriod),
                  cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr)),
              "WaverideR::plot_Xwavelet(res, dev_new = FALSE)")
          list(result = r, outputs = list(), code = code,
              text = sprintf("Overlap %s to %s, %d points", cd_fmt(lo), cd_fmt(hi), nrow(s1)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          pal <- cd_pal_split(p$palette)
          WaverideR::plot_Xwavelet(res, periodlab = p$periodlab, x_lab = p$x_lab, dev_new = FALSE, palette_name = pal$name,
              color_brewer = pal$family)
        }),
    cd_tool("bispectral", "Time Series", "Bispectral analysis", group = "Spectral analysis", heavy = TRUE,
        info = "Bispectrum and bicoherence: phase coupling between frequencies, e.g. combination tones of astronomical cycles.",
        uses = "WaverideR::bispectral_analysis, WaverideR::plot_bispectral",
        params = list(
          cd_sel("significance", "Significance test", c("None" = "none", "Surrogates (IAAFT, slow)" = "surrogate")),
          cd_num("n_sim", "Number of surrogates", 199, min = 19, step = 20,
              show_if = list(param = "significance", values = "surrogate")),
          cd_num("fmax", "Maximum frequency shown (cycles/unit)", cd_nyq, min = 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          r <- WaverideR::bispectral_analysis(data = as.matrix(s), significance = p$significance, n_sim = p$n_sim)
          code <- c(cd_even_code(d), sprintf("res <- WaverideR::bispectral_analysis(data = as.matrix(dat), significance = %s, n_sim = %d)",
              cd_dep(p$significance), as.integer(p$n_sim)), sprintf("WaverideR::plot_bispectral(res, freq_max = %s)", cd_fmt(p$fmax)))
          list(result = r, outputs = list(), code = code, text = cd_even_note(d, s))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_bispectral(res, freq_max = p$fmax)
        }),
    cd_tool("track_auto", "Time Series", "Automatic astronomical tracking", group = "Tracking (WaverideR)", heavy = TRUE,
        info = paste("Tracks the astronomical cycles in a scalogram automatically (hidden Markov model over all cycles",
            "at once) and gives the sedimentation rate and an age model with uncertainty. Use the Track tab to do it by hand."),
        uses = "WaverideR::track_astro_auto, WaverideR::plot_track",
        params = list(
          cd_num("srmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("srmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_sel("unit", "Depth unit of the data", c("metres" = "100", "centimetres" = "1", "feet" = "30.48")),
          cd_num("age_ma", "Approximate age (Ma)", 0, min = 0),
          cd_sel("scalogram", "Scalogram", c("Morlet CWT (fast)" = "cwt", "Multi-synchrosqueezed (msst)" = "msst",
              "Superlet SST" = "superlet_sst", "Chirplet SST (slow)" = "chirplet_sst")),
          cd_sel("method", "Tracker", c("HMM on the scalogram" = "hmm", "HMM and chirp-matched spectra" = "both",
              "Chirp-matched spectra" = "chirp")),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          sr <- c(p$srmin, p$srmax) / u
          r <- WaverideR::track_astro_auto(data = as.matrix(s), sedrate = sr, age_ma = p$age_ma, scalogram = p$scalogram,
              method = p$method)
          tr <- as.data.frame(r$track)
          am <- data.frame(depth = tr$depth, time_kyr = tr$time)
          srd <- data.frame(depth = tr$depth, sedrate_cm_kyr = tr$sedrate * u)
          o <- list(cd_output(cd_out_name(file, "-astrotrack"), tr, header = TRUE, var = "track"),
              cd_output(cd_out_name(file, "-astroage"), am, var = "am"),
              cd_output(cd_out_name(file, "-astrosedrate"), srd, var = "sr"))
          code <- c(cd_even_code(d), sprintf(paste0("res <- WaverideR::track_astro_auto(data = as.matrix(dat), sedrate = %s, age_ma = %s,\n",
              "    scalogram = %s, method = %s)"), cd_dep(signif(sr, 6)), cd_fmt(p$age_ma), cd_dep(p$scalogram), cd_dep(p$method)),
              "WaverideR::plot_track(res$track, res$scalogram, flags = res$flags)", "track <- as.data.frame(res$track)",
              "am <- data.frame(depth = track$depth, time_kyr = track$time)",
              sprintf("sr <- data.frame(depth = track$depth, sedrate_cm_kyr = track$sedrate * %s)", cd_fmt(u)), cd_code_write(o))
          nfl <- if (is.null(r$flags)) { 0 } else { nrow(r$flags) }
          r$scalogram <- cd_wt_slim(r$scalogram)
          list(result = r, outputs = o, code = code, text = c(cd_even_note(d, s),
              sprintf("Duration %s kyr; sed. rate %s to %s cm/kyr; %d flagged interval(s).", cd_fmt(diff(range(tr$time))),
                  cd_fmt(min(srd[, 2])), cd_fmt(max(srd[, 2])), nfl)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_track(res$track, res$scalogram, flags = res$flags, palette = cd_pal_split(p$palette)$name)
        }),
    cd_tool("filter", "Time Series", "Filtering", group = "Filtering and age models",
        info = paste("Band-pass filter: Taner (Taner 2000), Gaussian or cosine-tapered window. Optionally writes the amplitude",
            "envelope. Drag the two lines on the spectrum or type the cut-off frequencies."),
        uses = "astrochron::taner / bandpass / hilbert, WaverideR::gaussian_bandpass", lines = c("flow", "fhigh"),
        preview = function(d, p) {
          cd_plot_band(d, p$flow, p$fhigh)
        },
        params = list(
          cd_sel("method", "Filter", c("Taner (astrochron)" = "taner", "Gaussian (WaverideR)" = "gauss",
              "Cosine-tapered window (astrochron)" = "bandpass")),
          cd_num("flow", "Low frequency cut-off (cycles/unit)", flow_default, min = 0),
          cd_num("fhigh", "High frequency cut-off (cycles/unit)", fhigh_default, min = 0),
          cd_num("roll", "Taner roll-off rate", 1e10, min = 1, show_if = list(param = "method", values = "taner")),
          cd_chk("addmean", "Add the mean back to the filtered series", FALSE),
          cd_chk("envelope", "Also write the amplitude envelope (Hilbert)", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          if (!(p$fhigh > p$flow)) {
            stop("the high cut-off must be larger than the low cut-off", call. = FALSE)
          }
          if (p$method == "taner") {
            res <- astrochron::taner(s, flow = p$flow, fhigh = p$fhigh, roll = p$roll, addmean = p$addmean, output = 1,
                genplot = FALSE, verbose = FALSE)
            code <- sprintf("res <- astrochron::taner(dat, flow = %s, fhigh = %s, roll = %s, addmean = %s, genplot = FALSE)",
                cd_fmt(p$flow), cd_fmt(p$fhigh), format(p$roll), p$addmean)
          } else if (p$method == "gauss") {
            g <- WaverideR::gaussian_bandpass(data = as.matrix(s), f_low = p$flow, f_high = p$fhigh)
            res <- data.frame(s$x, g$filtered[, 2] + if (p$addmean) { mean(s$y) } else { 0 })
            code <- c(sprintf("g <- WaverideR::gaussian_bandpass(data = as.matrix(dat), f_low = %s, f_high = %s)",
                cd_fmt(p$flow), cd_fmt(p$fhigh)), sprintf("res <- data.frame(dat[, 1], g$filtered[, 2]%s)",
                if (p$addmean) { " + mean(dat[, 2])" } else { "" }))
          } else {
            res <- astrochron::bandpass(s, flow = p$flow, fhigh = p$fhigh, win = 2, addmean = p$addmean, output = 1,
                genplot = FALSE, verbose = FALSE)
            code <- sprintf("res <- astrochron::bandpass(dat, flow = %s, fhigh = %s, win = 2, addmean = %s, genplot = FALSE)",
                cd_fmt(p$flow), cd_fmt(p$fhigh), p$addmean)
          }
          res <- as.data.frame(res)
          names(res) <- c(names(d)[1], "filtered")
          env <- NULL
          o <- list(cd_output(cd_out_name(file, sprintf("-%s%s-%s", p$method, cd_fmt(p$flow), cd_fmt(p$fhigh))), res))
          if (p$envelope) {
            env <- as.data.frame(astrochron::hilbert(data.frame(res[, 1], res[, 2] - mean(res[, 2])), genplot = FALSE,
                verbose = FALSE))
            o[[2]] <- cd_output(cd_out_name(file, sprintf("-%s%s-%s-env", p$method, cd_fmt(p$flow), cd_fmt(p$fhigh))), env,
                var = "env")
            code <- c(code, "env <- astrochron::hilbert(res, genplot = FALSE)")
          }
          sp <- astrochron::periodogram(data.frame(s$x, s$y - mean(s$y)), padfac = 4, output = 1, genplot = FALSE,
              verbose = FALSE)
          list(result = list(before = s, after = res, env = env, spec = sp), outputs = o, text = c(cd_even_note(d, s),
              sprintf("Pass band: %s to %s cycles/unit = periods %s to %s", cd_fmt(p$flow), cd_fmt(p$fhigh),
                  cd_fmt_period(1 / p$fhigh), cd_fmt_period(1 / p$flow))),
              code = c(cd_even_code(d), code, cd_code_write(o)))
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:3, ncol = 1), heights = c(1, 1, 1))
          cd_par()
          cd_plot_one(res$before, col = "#7f8c8d", ylab = "input", xlab = "")
          cd_par()
          cd_plot_one(res$after, col = "#1f4e79", ylab = "filtered", xlab = "Depth / time")
          if (!is.null(res$env)) {
            graphics::lines(res$env[, 1], res$env[, 2] + if (p$addmean) { mean(res$after[, 2]) } else { 0 }, col = "#c0392b")
          }
          cd_par()
          sp <- res$spec
          fmax <- min(max(sp$Frequency), 3 * p$fhigh)
          k <- sp$Frequency <= fmax
          graphics::plot(sp$Frequency[k], sp$Power[k], type = "l", col = "#1f4e79", xlab = "Frequency (cycles/unit)",
              ylab = "Power")
          graphics::rect(p$flow, 0, p$fhigh, max(sp$Power[k]) * 1.1, col = grDevices::adjustcolor("#c0392b", 0.15), border = NA)
          graphics::abline(v = c(p$flow, p$fhigh), col = "#c0392b", lty = 2)
        }),
    cd_tool("amplitude", "Time Series", "Amplitude modulation (Hilbert)", group = "Filtering and age models",
        info = "Instantaneous amplitude of a filtered series (Acycle: Amplitude Modulation).",
        uses = "astrochron::hilbert",
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          env <- as.data.frame(astrochron::hilbert(s, genplot = FALSE, verbose = FALSE))
          o <- list(cd_output(cd_out_name(file, "-AM"), env))
          list(result = list(before = s, env = env), outputs = o,
              code = c(cd_even_code(d), "res <- astrochron::hilbert(dat, genplot = FALSE)", cd_code_write(o)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          cd_par()
          s <- res$before
          m <- mean(s$y)
          cd_plot_one(s, col = "#1f4e79", ylab = "Value", xlab = "Depth / time")
          graphics::lines(res$env[, 1], m + res$env[, 2], col = "#c0392b", lwd = 1.5)
          graphics::lines(res$env[, 1], m - res$env[, 2], col = "#c0392b", lwd = 1.5)
        }),
    cd_tool("agemodel", "Time Series", "Build age model", group = "Filtering and age models",
        info = paste("Floating age model from a filtered cycle: every maximum (or minimum) of the filtered series is",
            "one cycle period later than the previous one (Acycle: Build Age Model; minimal tuning)."),
        uses = "CycloDesk::cd_cycle_agemodel, astrochron::tune",
        params = list(
          cd_num("cycle", "Period of the filtered cycle (kyr)", 405, min = 0),
          cd_sel("use", "Tie points at", c("Maxima" = "max", "Minima" = "min")),
          cd_num("pts", "Half-width of the peak search (points)", 3, min = 1, step = 1),
          cd_num("t0", "Age of the first tie point (kyr)", 0),
          cd_chk("tuned", "Also write the tuned series", TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          am <- cd_cycle_agemodel(s, cycle = p$cycle, use = p$use, pts = p$pts, t0 = p$t0)
          sr <- data.frame(depth = (am[-1, 1] + am[-nrow(am), 1]) / 2, sedrate_cm_kyr = 100 * diff(am[, 1]) / diff(am[, 2]))
          o <- list(cd_output(cd_out_name(file, sprintf("-agemodel%s", cd_fmt(p$cycle))), am, var = "am"),
              cd_output(cd_out_name(file, sprintf("-sedrate%s", cd_fmt(p$cycle))), sr, var = "sr"))
          code <- c(sprintf("am <- CycloDesk::cd_cycle_agemodel(dat, cycle = %s, use = %s, pts = %d, t0 = %s)", cd_fmt(p$cycle),
              cd_dep(p$use), as.integer(p$pts), cd_fmt(p$t0)),
              "sr <- data.frame(depth = (am[-1, 1] + am[-nrow(am), 1]) / 2, sedrate_cm_kyr = 100 * diff(am[, 1]) / diff(am[, 2]))")
          tuned <- NULL
          if (p$tuned) {
            tuned <- as.data.frame(astrochron::tune(s, controlPts = am, extrapolate = FALSE, genplot = FALSE, verbose = FALSE))
            o[[3]] <- cd_output(cd_out_name(file, sprintf("-tuned%s", cd_fmt(p$cycle))), tuned, var = "tuned")
            code <- c(code, "tuned <- astrochron::tune(dat, controlPts = am, genplot = FALSE)")
          }
          list(result = list(s = s, am = am, sr = sr, tuned = tuned), outputs = o, code = c(code, cd_code_write(o)),
              text = c(sprintf("%d tie points, %s kyr in total", nrow(am), cd_fmt(diff(range(am[, 2])))),
                  sprintf("Sedimentation rate: mean %s, range %s to %s cm/kyr", cd_fmt(mean(sr[, 2])), cd_fmt(min(sr[, 2])),
                      cd_fmt(max(sr[, 2])))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE))
          cd_par()
          cd_plot_one(res$s, col = "#1f4e79", ylab = "filtered", xlab = "Depth")
          yv <- stats::approx(res$s$x, res$s$y, res$am[, 1])$y
          graphics::points(res$am[, 1], yv, pch = 21, bg = "#c0392b", col = "white")
          cd_par()
          graphics::plot(res$am[, 1], res$am[, 2], type = "o", pch = 16, cex = 0.6, col = "#1f4e79", xlab = "Depth",
              ylab = "Age (kyr)")
          cd_par()
          graphics::plot(res$am[, 1], c(res$sr[, 2], NA), type = "s", col = "#2e8b57", xlab = "Depth",
              ylab = "Sed. rate (cm/kyr)", lwd = 1.5)
        }),
    cd_tool("sedrate2age", "Time Series", "Sedimentation rate to age model", group = "Filtering and age models",
        info = "Integrates a sedimentation-rate curve (depth in m, rate in cm/kyr) to a depth-time model.",
        uses = "astrochron::sedrate2time",
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          res <- as.data.frame(astrochron::sedrate2time(s, genplot = FALSE, verbose = FALSE))
          o <- list(cd_output(cd_out_name(file, "-age"), res))
          list(result = list(s = s, res = res), outputs = o,
              code = c("res <- astrochron::sedrate2time(dat, genplot = FALSE)", cd_code_write(o)),
              text = sprintf("Duration %s kyr", cd_fmt(diff(range(res[, 2])))))
        },
        plot = function(res, p) {
          cd_two_panel(res$s, res$res, "Sed. rate (cm/kyr)", "Time (kyr)")
        }),
    cd_tool("tune", "Time Series", "Age scale / tuning (2 files)", group = "Filtering and age models", inputs = 2L,
        info = "Converts a depth series to time with an age model (file 2: depth, age), as Acycle: Age Scale.",
        uses = "astrochron::tune, astrochron::linterp",
        params = list(
          cd_chk("extrapolate", "Extrapolate beyond the first and last tie point", FALSE),
          cd_chk("interp", "Interpolate the tuned series at an even step", TRUE),
          cd_num("dt", "Time step (kyr)", 1, min = 0, show_if = list(param = "interp", values = "true"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          am <- cd_clean(d2)
          res <- as.data.frame(astrochron::tune(s, controlPts = am, extrapolate = p$extrapolate, genplot = FALSE, verbose = FALSE))
          code <- c(cd_code_read(file2, var = "am"),
              sprintf("res <- astrochron::tune(dat, controlPts = am, extrapolate = %s, genplot = FALSE)", p$extrapolate))
          sfx <- "-tuned"
          if (p$interp) {
            res <- as.data.frame(astrochron::linterp(res, dt = p$dt, genplot = FALSE, verbose = FALSE))
            code <- c(code, sprintf("res <- astrochron::linterp(res, dt = %s, genplot = FALSE)", cd_fmt(p$dt)))
            sfx <- paste0(sfx, "-rsp", cd_fmt(p$dt))
          }
          o <- list(cd_output(cd_out_name(file, sfx), res))
          list(result = list(am = am, res = res), outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%d points from %s to %s kyr", nrow(res), cd_fmt(min(res[, 1])), cd_fmt(max(res[, 1]))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, nrow = 1), widths = c(1, 2.5))
          cd_par()
          graphics::plot(res$am[, 1], res$am[, 2], type = "o", pch = 16, cex = 0.6, col = "#1f4e79", xlab = "Depth",
              ylab = "Age (kyr)")
          cd_par()
          cd_plot_one(res$res, col = "#1f4e79", xlab = "Time (kyr)", ylab = "Tuned series")
        }),
    cd_tool("timeopt", "Time Series", "TimeOpt", group = "Astronomical testing",
        info = "Optimises the sedimentation rate by fitting precession amplitude modulation and spectral power (Meyers 2015).",
        uses = "astrochron::timeOpt",
        params = list(
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 100, min = 10, step = 10),
          cd_sel("linLog", "Sed. rate grid", c("Logarithmic" = "1", "Linear" = "0")),
          cd_sel("fit", "Envelope fit", c("Precession amplitude modulation" = "1", "Short eccentricity modulation" = "2")),
          cd_txt("targetE", "Eccentricity periods (kyr, empty = default)", "", numeric = TRUE),
          cd_txt("targetP", "Precession periods (kyr, empty = default)", "", numeric = TRUE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          tE <- if (length(p$targetE)) { p$targetE } else { NULL }
          tP <- if (length(p$targetP)) { p$targetP } else { NULL }
          r1 <- astrochron::timeOpt(s, sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed, linLog = as.integer(p$linLog),
              fit = as.integer(p$fit), targetE = tE, targetP = tP, output = 1, genplot = FALSE, verbose = FALSE)
          r1 <- as.data.frame(r1)
          best <- r1$sedrate[which.max(r1$r2_opt)]
          r2 <- astrochron::timeOpt(s, sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed, linLog = as.integer(p$linLog),
              fit = as.integer(p$fit), targetE = tE, targetP = tP, output = 2, genplot = FALSE, verbose = FALSE)
          o <- list(cd_output(cd_out_name(file, "-timeOpt"), r1, header = TRUE))
          code <- c(cd_even_code(d), sprintf(paste0("res <- astrochron::timeOpt(dat, sedmin = %s, sedmax = %s, numsed = %d, linLog = %s,\n",
              "    fit = %s, targetE = %s, targetP = %s, output = 1, genplot = FALSE)"), cd_fmt(p$sedmin), cd_fmt(p$sedmax),
              as.integer(p$numsed), p$linLog, p$fit, cd_dep(tE), cd_dep(tP)), cd_code_write(o))
          list(result = list(r1 = r1, r2 = as.data.frame(r2), best = best), outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("Optimal sedimentation rate %s cm/kyr, r2_opt = %s, r2_envelope = %s, r2_power = %s",
                  cd_fmt(best), cd_fmt(max(r1$r2_opt)), cd_fmt(r1$r2_envelope[which.max(r1$r2_opt)]),
                  cd_fmt(r1$r2_spectral_power[which.max(r1$r2_opt)]))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE))
          r1 <- res$r1
          cd_par()
          lg <- if (p$linLog == "1") { "x" } else { "" }
          graphics::plot(r1$sedrate, r1$r2_envelope, type = "l", col = "#c0392b", log = lg, ylim = c(0, max(r1[, 2:3])),
              xlab = "Sed. rate (cm/kyr)", ylab = "r2")
          graphics::lines(r1$sedrate, r1$r2_spectral_power, col = "#1f4e79")
          graphics::legend("topright", c("envelope", "spectral power"), col = c("#c0392b", "#1f4e79"), lwd = 1, bty = "n",
              cex = 0.8)
          graphics::abline(v = res$best, lty = 2)
          cd_par()
          graphics::plot(r1$sedrate, r1$r2_opt, type = "l", col = "#2e8b57", log = lg, xlab = "Sed. rate (cm/kyr)",
              ylab = "r2 opt")
          graphics::abline(v = res$best, lty = 2)
          cd_par()
          r2 <- res$r2
          graphics::plot(r2[, 1], r2[, 3], type = "l", col = "#1f4e79", xlab = "Time (kyr)", ylab = "Filtered / envelope",
              main = sprintf("Best fit at %s cm/kyr", cd_fmt(res$best)))
          graphics::lines(r2[, 1], r2[, 4], col = "#c0392b", lwd = 1.5)
          graphics::lines(r2[, 1], r2[, 5], col = "#2e8b57", lwd = 1.5, lty = 2)
          graphics::legend("topright", c("filtered", "envelope", "fitted model"), col = c("#1f4e79", "#c0392b", "#2e8b57"),
              lty = c(1, 1, 2), bty = "n", cex = 0.8)
        }),
    cd_tool("etimeopt", "Time Series", "eTimeOpt (evolutive)", group = "Astronomical testing", heavy = TRUE,
        info = "TimeOpt in sliding windows: astrochron eTimeOpt, or WaverideR win_timeOpt with the null-model bias correction.",
        uses = "astrochron::eTimeOpt, WaverideR::win_timeOpt",
        params = list(
          cd_sel("method", "Method", c("eTimeOpt (astrochron)" = "etimeopt", "win_timeOpt, bias corrected (WaverideR)" = "win")),
          cd_num("win", "Window length (depth units)", len_default(0.2), min = 0),
          cd_num("step", "Step (depth units)", function(d) { signif(10 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_num("sedmin", "Minimum sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("sedmax", "Maximum sed. rate (cm/kyr)", 5, min = 0),
          cd_num("numsed", "Number of sed. rates", 50, min = 10, step = 10),
          cd_sel("fit", "Envelope fit", c("Precession amplitude modulation" = "1", "Short eccentricity modulation" = "2")),
          cd_num("n_null", "Null simulations per window", 20, min = 0, step = 10, show_if = list(param = "method", values = "win"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          if (p$method == "etimeopt") {
            r <- astrochron::eTimeOpt(s, win = p$win, step = p$step, sedmin = p$sedmin, sedmax = p$sedmax, numsed = p$numsed,
                fit = as.integer(p$fit), output = 1, genplot = FALSE, verbose = 0)
            sed <- r[[3]][, 1]
            x <- as.numeric(sub("^X", "", names(r[[3]])[-1]))
            z <- t(as.matrix(r[[3]][, -1]))
            track <- data.frame(depth = x, sedrate = sed[apply_which_max(z)])
            code <- sprintf(paste0("res <- astrochron::eTimeOpt(dat, win = %s, step = %s, sedmin = %s, sedmax = %s, numsed = %d,\n",
                "    fit = %s, output = 1, genplot = FALSE)"), cd_fmt(p$win), cd_fmt(p$step), cd_fmt(p$sedmin), cd_fmt(p$sedmax),
                as.integer(p$numsed), p$fit)
            out <- list(x = x, sed = sed, z = z, track = track, method = "etimeopt")
          } else {
            stp <- max(1L, as.integer(round(p$step / cd_dt(s$x))))
            bc <- if (p$n_null > 0) { "null" } else { "none" }
            r <- WaverideR::win_timeOpt(data = as.matrix(s), window_size = p$win, sedmin = p$sedmin, sedmax = p$sedmax,
                numsed = p$numsed, fit = as.integer(p$fit), step = stp, n_null = p$n_null, bias_correction = bc)
            code <- sprintf(paste0("res <- WaverideR::win_timeOpt(data = as.matrix(dat), window_size = %s, sedmin = %s, sedmax = %s,\n",
                "    numsed = %d, fit = %s, step = %d, n_null = %d, bias_correction = %s)\nWaverideR::plot_win_timeOpt(res)"),
                cd_fmt(p$win), cd_fmt(p$sedmin), cd_fmt(p$sedmax), as.integer(p$numsed), p$fit, stp, as.integer(p$n_null),
                cd_dep(bc))
            out <- list(raw = r, method = "win")
            track <- NULL
            if (bc == "null" && !is.null(r$testable) && !any(r$testable)) {
              out$note <- paste("No window could be tested against the null model: the window is too short to hold enough",
                  "eccentricity cycles at these sedimentation rates. The uncorrected r2 is shown; use a longer window.")
            }
          }
          o <- list()
          if (!is.null(track)) {
            o[[1]] <- cd_output(cd_out_name(file, "-eTimeOpt_track"), track, var = "track")
            code <- c(code, "z <- t(as.matrix(res[[3]][, -1]))", "track <- data.frame(as.numeric(sub(\"^X\", \"\", names(res[[3]])[-1])),",
                "    res[[3]][max.col(z), 1])", cd_code_write(o))
          }
          list(result = out, outputs = o, code = c(cd_even_code(d), code), text = c(cd_even_note(d, s), out$note))
        },
        plot = function(res, p) {
          if (res$method == "win") {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            corr <- is.null(res$note)
            WaverideR::plot_win_timeOpt(res$raw, corrected = corr)
            return(invisible(NULL))
          }
          cd_plot_image(res$x, res$sed, res$z, xlab = "Depth", ylab = "Sed. rate (cm/kyr)", zlab = "r2 opt")
        }),
    cd_tool("coco", "Time Series", "COCO", group = "Astronomical testing", heavy = TRUE,
        info = "Correlation coefficient between the data spectrum and an astronomical target for a range of sedimentation rates (Li et al. 2018).",
        uses = "WaverideR::coco_classic, WaverideR::coco_target_from_periods",
        params = list(
          cd_num("age_ma", "Approximate age of the record (Ma)", 100, min = 0,
              help = "Used to set the precession and obliquity periods of the target (Berger 1992 / Waltham 2015)."),
          cd_num("srmin", "Minimum sed. rate (cm/kyr)", 0.2, min = 0),
          cd_num("srmax", "Maximum sed. rate (cm/kyr)", 10, min = 0),
          cd_num("srstep", "Sed. rate step (cm/kyr)", 0.1, min = 0),
          cd_num("nsim", "Monte Carlo simulations (0 = none)", 200, min = 0, step = 100),
          cd_sel("cor", "Correlation", c("Pearson", "Spearman")),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          tg <- WaverideR::coco_target_from_periods(age_ma = p$age_ma)
          sr <- seq(p$srmin, p$srmax, by = p$srstep)
          r <- WaverideR::coco_classic(data = as.matrix(s), target = tg, sr = sr, nsim = p$nsim, method = p$cor,
              seed = p$seed, verbose = FALSE)
          tab <- as.data.frame(r$result)
          o <- list(cd_output(cd_out_name(file, "-COCO"), tab, header = TRUE, var = "res$result"))
          best <- tab$sr[which.max(tab$rho)]
          code <- c(cd_even_code(d), sprintf("tg <- WaverideR::coco_target_from_periods(age_ma = %s)", cd_fmt(p$age_ma)),
              sprintf("res <- WaverideR::coco_classic(data = as.matrix(dat), target = tg, sr = seq(%s, %s, by = %s), nsim = %d,\n    method = %s, seed = %d)",
                  cd_fmt(p$srmin), cd_fmt(p$srmax), cd_fmt(p$srstep), as.integer(p$nsim), cd_dep(p$cor), as.integer(p$seed)),
              "plot(res)", cd_code_write(o))
          list(result = r, outputs = o, code = code, text = c(cd_even_note(d, s),
              sprintf("Highest correlation at %s cm/kyr (rho = %s)", cd_fmt(best), cd_fmt(max(tab$rho)))))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          plot(res)
        }),
    cd_tool("ecoco", "Time Series", "eCOCO (evolutive)", group = "Astronomical testing", heavy = TRUE,
        info = "COCO in sliding windows with ridge tracking of the best sedimentation rate (Li et al. 2018).",
        uses = "WaverideR::ecoco_classic",
        params = list(
          cd_num("age_ma", "Approximate age of the record (Ma)", 100, min = 0),
          cd_num("window", "Window length (depth units)", len_default(0.25), min = 0),
          cd_num("step", "Step (samples)", 10, min = 1, step = 1),
          cd_num("srmin", "Minimum sed. rate (cm/kyr)", 0.2, min = 0),
          cd_num("srmax", "Maximum sed. rate (cm/kyr)", 10, min = 0),
          cd_num("srstep", "Sed. rate step (cm/kyr)", 0.2, min = 0),
          cd_num("nsim", "Monte Carlo simulations (0 = none)", 0, min = 0, step = 50)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          tg <- WaverideR::coco_target_from_periods(age_ma = p$age_ma)
          sr <- seq(p$srmin, p$srmax, by = p$srstep)
          r <- WaverideR::ecoco_classic(data = as.matrix(s), target = tg, window = p$window, step = p$step, sr = sr,
              nsim = p$nsim, verbose = FALSE)
          o <- list()
          code <- c(cd_even_code(d), sprintf("tg <- WaverideR::coco_target_from_periods(age_ma = %s)", cd_fmt(p$age_ma)),
              sprintf(paste0("res <- WaverideR::ecoco_classic(data = as.matrix(dat), target = tg, window = %s, step = %d,\n",
                  "    sr = seq(%s, %s, by = %s), nsim = %d)"), cd_fmt(p$window), as.integer(p$step), cd_fmt(p$srmin),
                  cd_fmt(p$srmax), cd_fmt(p$srstep), as.integer(p$nsim)), "plot(res)")
          if (!is.null(r$track)) {
            o[[1]] <- cd_output(cd_out_name(file, "-eCOCO_track"), as.data.frame(r$track), header = TRUE, var = "res$track")
            code <- c(code, cd_code_write(o))
          }
          list(result = r, outputs = o, code = code, text = cd_even_note(d, s))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          plot(res)
        }),
    cd_tool("rho1", "Time Series", "Evolutive lag-1 autocorrelation (rho1)", group = "Sedimentary noise models", heavy = TRUE,
        info = "Running lag-1 autocorrelation with Monte Carlo windows, a sea-level proxy (Li et al. 2018, Acycle: rho1).",
        uses = "WaverideR::rho1_evolutive",
        params = list(
          cd_num("window_min", "Shortest window (time units)", len_default(0.05), min = 0),
          cd_num("window_max", "Longest window (time units)", len_default(0.1), min = 0),
          cd_num("n_sim", "Monte Carlo simulations (at least 50)", 200, min = 50, step = 50),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          r <- WaverideR::rho1_evolutive(data = as.matrix(s), window_min = p$window_min, window_max = p$window_max,
              n_sim = p$n_sim, seed = p$seed)
          o <- list(cd_output(cd_out_name(file, "-rho1"), as.data.frame(r$percentile), header = TRUE, var = "res$percentile"))
          code <- c(sprintf("res <- WaverideR::rho1_evolutive(data = as.matrix(dat), window_min = %s, window_max = %s, n_sim = %d, seed = %d)",
              cd_fmt(p$window_min), cd_fmt(p$window_max), as.integer(p$n_sim), as.integer(p$seed)), cd_code_write(o))
          list(result = list(s = s, r = r), outputs = o, code = code)
        },
        plot = function(res, p) {
          cd_plot_percentiles(res$s, res$r$percentile, "rho1")
        }),
    cd_tool("dynot", "Time Series", "DYNOT (dynamic noise after tuning)", group = "Sedimentary noise models", heavy = TRUE,
        info = "Dynamic noise after orbital tuning, a sea-level proxy for tuned series (Li et al. 2018, Acycle: DYNOT).",
        uses = "WaverideR::dynot, WaverideR::plot_dynot",
        params = list(
          cd_num("window_min", "Shortest window (kyr)", 300, min = 0),
          cd_num("window_max", "Longest window (kyr)", 500, min = 0),
          cd_num("step", "Step (kyr)", 5, min = 0),
          cd_num("n_sim", "Monte Carlo simulations", 200, min = 10, step = 50),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          r <- WaverideR::dynot(data = as.matrix(s), window_min = p$window_min, window_max = p$window_max, step = p$step,
              n_sim = p$n_sim, seed = p$seed)
          o <- list(cd_output(cd_out_name(file, "-DYNOT"), as.data.frame(r$percentile), header = TRUE, var = "res$percentile"))
          code <- c(cd_even_code(d), sprintf("res <- WaverideR::dynot(data = as.matrix(dat), window_min = %s, window_max = %s, step = %s, n_sim = %d, seed = %d)",
              cd_fmt(p$window_min), cd_fmt(p$window_max), cd_fmt(p$step), as.integer(p$n_sim), as.integer(p$seed)),
              "WaverideR::plot_dynot(res)", cd_code_write(o))
          list(result = r, outputs = o, code = code, text = cd_even_note(d, s))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_dynot(res)
        }),
    cd_tool("pda", "Time Series", "Power decomposition analysis", group = "Sedimentary noise models", heavy = TRUE,
        info = "Ratio of power in astronomical bands to total power in sliding windows (Acycle: Power Decomposition Analysis).",
        uses = "WaverideR::pda",
        params = list(
          cd_txt("bands", "Target band (two frequencies, cycles/unit)", function(d) {
            paste(signif(cd_peak_freq(d) * c(0.8, 1.2), 3), collapse = ", ")
          }, numeric = TRUE),
          cd_num("window", "Window length", len_default(0.2), min = 0),
          cd_num("nw", "Time-bandwidth product (NW)", 2, min = 1, step = 0.5)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          b <- p$bands
          if (length(b) < 2) {
            stop("give two frequencies for the target band", call. = FALSE)
          }
          r <- WaverideR::pda(data = as.matrix(s), target_bands = b[1:2], window = p$window, nw = p$nw)
          tab <- as.data.frame(r$result)
          o <- list(cd_output(cd_out_name(file, "-PDA"), tab, header = TRUE, var = "res$result"))
          code <- c(cd_even_code(d), sprintf("res <- WaverideR::pda(data = as.matrix(dat), target_bands = %s, window = %s, nw = %s)",
              cd_dep(b[1:2]), cd_fmt(p$window), cd_fmt(p$nw)), cd_code_write(o))
          list(result = list(s = s, tab = tab), outputs = o, code = code, text = cd_even_note(d, s))
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
        }),
    cd_tool("spectral_moments", "Time Series", "Spectral moments", group = "Sedimentary noise models", heavy = TRUE,
        info = "Evolutive mean frequency and bandwidth, indicators of sedimentation-rate change (Sinnesael et al. 2018).",
        uses = "WaverideR::spectral_moments, WaverideR::plot_spectral_moments",
        params = list(
          cd_num("window", "Window length", len_default(0.15), min = 0),
          cd_num("step", "Step", function(d) { signif(5 * cd_dt(d[, 1]), 3) }, min = 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          r <- WaverideR::spectral_moments(data = as.matrix(s), window = p$window, step = p$step)
          o <- list(cd_output(cd_out_name(file, "-SpecMoments"), as.data.frame(r$result), header = TRUE, var = "res$result"))
          code <- c(cd_even_code(d), sprintf("res <- WaverideR::spectral_moments(data = as.matrix(dat), window = %s, step = %s)",
              cd_fmt(p$window), cd_fmt(p$step)), "WaverideR::plot_spectral_moments(res)", cd_code_write(o))
          list(result = r, outputs = o, code = code, text = cd_even_note(d, s))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_spectral_moments(res)
        })
  )
}

# Index of the maximum in every row
apply_which_max <- function(z) {
  max.col(replace(z, is.na(z), -Inf), ties.method = "first")
}

# Percentile band plot (rho1, DYNOT style)
cd_plot_percentiles <- function(s, pc, lab) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, ncol = 1))
  cd_par()
  cd_plot_one(s, col = "#7f8c8d", ylab = "input", xlab = "")
  cd_par()
  pc <- as.matrix(pc)
  x <- pc[, 1]
  k <- ncol(pc)
  mid <- (k + 1) %/% 2 + 1
  if (k < 3) {
    graphics::plot(x, pc[, 2], type = "l", xlab = "Depth / time", ylab = lab)
    return(invisible(NULL))
  }
  graphics::plot(x, pc[, mid], type = "n", ylim = range(pc[, -1], finite = TRUE), xlab = "Depth / time", ylab = lab)
  np <- (k - 1) %/% 2
  for (j in seq_len(np)) {
    graphics::polygon(c(x, rev(x)), c(pc[, 1 + j], rev(pc[, k + 1 - j])),
        col = grDevices::adjustcolor("#1f4e79", 0.12), border = NA)
  }
  graphics::lines(x, pc[, mid], col = "#1f4e79", lwd = 1.5)
}

#' Age model from a filtered cycle
#'
#' Finds the maxima (or minima) of a filtered series and gives each one an age one cycle period after the previous
#' one, as in the Build Age Model tool of Acycle (minimal tuning).
#'
#' @param data Data frame or matrix: depth in column 1, filtered series in column 2.
#' @param cycle Period of the filtered cycle in kyr (numeric, > 0). \code{Default=405}.
#' @param use Tie points at the maxima (\code{"max"}) or minima (\code{"min"}). \code{Default="max"}.
#' @param pts Half-width of the search window in points: a sample is a maximum when it is the largest within
#'   \code{pts} samples on either side (integer, >= 1). \code{Default=3}.
#' @param t0 Age of the first tie point in kyr (numeric). \code{Default=0}.
#' @return A data frame with columns \code{depth} and \code{age_kyr}.
#' @export
cd_cycle_agemodel <- function(data, cycle = 405, use = "max", pts = 3, t0 = 0) {
  s <- cd_clean(data)
  y <- if (use == "min") { -s$y } else { s$y }
  n <- length(y)
  pts <- max(1L, as.integer(pts))
  pk <- integer()
  for (i in seq_len(n)) {
    lo <- max(1L, i - pts)
    hi <- min(n, i + pts)
    if (i > 1 && i < n && y[i] == max(y[lo:hi]) && y[i] > y[i - 1]) {
      pk <- c(pk, i)
    }
  }
  if (length(pk) < 2) {
    stop("fewer than two peaks found: check the filtered series and the peak search width", call. = FALSE)
  }
  data.frame(depth = s$x[pk], age_kyr = t0 + (seq_along(pk) - 1) * cycle)
}

# Highest peaks, at least 'gap' apart on the frequency axis
cd_spread_peaks <- function(pk, pw, f, gap, n) {
  pk <- pk[order(-pw[pk])]
  keep <- integer()
  for (k in pk) {
    if (length(keep) >= n) {
      break
    }
    if (!length(keep) || all(abs(f[keep] - f[k]) > gap)) {
      keep <- c(keep, k)
    }
  }
  keep
}

# Default scale resolution: about 200 periods between the default shortest and longest period, at most 1/100
cd_dj_default <- function(d) {
  lo <- 3 * cd_dt(d[, 1])
  hi <- diff(range(d[, 1], na.rm = TRUE)) * 0.33
  signif(max(0.01, log2(hi / lo) / 200), 2)
}

# Wavelet result reduced for plotting and storage: no complex coefficients, at most 'max_cols' positions (evenly
# thinned, so the grid stays regular). A 20 000-point record with 1000 periods is ~850 MB in full, ~3 MB slimmed.
cd_wt_slim <- function(w, max_cols = 2000) {
  if (!is.list(w) || is.null(w$Power) || !is.matrix(w$Power) || is.null(w$axis.1)) {
    return(w)
  }
  w$Wave <- NULL
  w$Phase <- NULL
  w$Ampl <- NULL
  w$Wave_cwt <- NULL
  w$inst_period <- NULL
  n <- length(w$axis.1)
  if (n <= max_cols) {
    return(w)
  }
  idx <- seq(1, n, by = ceiling(n / max_cols))
  # positions are the columns of a CWT power matrix but the rows of a superlet power matrix
  for (nm in c("Power", "Power.pval", "Power_cwt")) {
    m <- w[[nm]]
    if (is.matrix(m) && ncol(m) == n) {
      w[[nm]] <- m[, idx, drop = FALSE]
    } else if (is.matrix(m) && nrow(m) == n) {
      w[[nm]] <- m[idx, , drop = FALSE]
    }
  }
  for (nm in c("axis.1", "x", "y")) {
    if (length(w[[nm]]) == n) {
      w[[nm]] <- w[[nm]][idx]
    }
  }
  w$nc <- length(idx)
  w$slim <- TRUE
  w
}
