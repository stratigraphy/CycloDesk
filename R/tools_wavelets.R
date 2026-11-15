# Tools: wavelets ####
#
# Wavelets menu: advanced scalograms (adaptive, synchrosqueezed, chirped, de-chirped, cross superlet), significance
# tests, cycle extraction, uncertainty and diagnostics of WaverideR. The plain CWT, superlet and SST are in the
# Time Series menu (tool "wavelet").

# Shared defaults ####

cd_wv_lo <- function(d) {
  signif(3 * cd_dt(d[, 1]), 3)
}

cd_wv_hi <- function(d) {
  signif(diff(range(d[, 1], na.rm = TRUE)) * 0.33, 3)
}

cd_wv_len <- function(frac) {
  function(d) {
    signif(diff(range(d[, 1], na.rm = TRUE)) * frac, 3)
  }
}

cd_wv_peak <- function(fac = 1) {
  function(d) {
    signif(fac / cd_peak_freq(d), 3)
  }
}

# Default largest rate gradient: the rate may change by a factor e over the record
cd_wv_chirp_default <- function(d) {
  signif(1 / diff(range(d[, 1], na.rm = TRUE)), 2)
}

cd_wv_units <- function() {
  c("metres" = "100", "centimetres" = "1", "feet" = "30.48")
}

cd_wv_astro_default <- function() {
  "405, 125, 95, 41, 23.7, 22.4, 19"
}

# CWT parameters shared by the tools that compute a scalogram first
cd_wv_cwt_params <- function(omega = 8, show_if = NULL) {
  list(
    cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0, show_if = show_if),
    cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0, show_if = show_if),
    cd_num("omega_nr", "Number of cycles of the Morlet wavelet (omega_nr)", omega, min = 1, step = 1, show_if = show_if),
    cd_num("dj", "Scale resolution dj (smaller = more periods, slower)", cd_dj_default, min = 0.001, step = 0.005,
        show_if = show_if))
}

cd_wv_cwt <- function(s, p) {
  WaverideR::analyse_wavelet(data = as.matrix(s), dj = p$dj, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
      omega_nr = p$omega_nr, verbose = FALSE)
}

cd_wv_cwt_code <- function(p, var = "wt") {
  sprintf(paste0("%s <- WaverideR::analyse_wavelet(data = as.matrix(dat), dj = %s, lowerPeriod = %s, upperPeriod = %s,\n",
      "    omega_nr = %s, verbose = FALSE)"), var, cd_fmt(p$dj), cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod),
      cd_fmt(p$omega_nr))
}

cd_wv_chirp_grid <- function(cmax, n) {
  n <- max(1L, as.integer(n))
  seq(-abs(cmax), abs(cmax), length.out = 2L * n + 1L)
}

cd_wv_astro <- function(v) {
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) {
    stop("give at least one astronomical period (kyr)", call. = FALSE)
  }
  v
}

# Tracked period curve (second file): depth, period
cd_wv_track <- function(d2) {
  if (is.null(d2)) {
    stop("select the tracked period curve (depth, period) as the second file", call. = FALSE)
  }
  t <- cd_clean(d2)
  t <- t[t$y > 0, , drop = FALSE]
  if (nrow(t) < 2) {
    stop("the tracked period curve (second file) needs at least two rows of depth and period > 0", call. = FALSE)
  }
  data.frame(depth = t$x, period = t$y)
}

cd_wv_track_code <- function(file2) {
  c(cd_code_read(file2, var = "trk"), "trk <- trk[complete.cases(trk[, 1:2]) & trk[, 2] > 0, 1:2]",
      "trk <- trk[order(trk[, 1]), ]")
}

# Sedimentation-rate track (wr_track layout: depth, sedrate in depth units per kyr, time in kyr) from a tracked period
cd_wv_sed_track <- function(trk, cycle) {
  sr <- trk$period / cycle
  tt <- c(0, cumsum(diff(trk$depth) / ((sr[-1] + sr[-length(sr)]) / 2)))
  list(depth = trk$depth, sedrate = sr, time = tt)
}

cd_wv_sed_track_code <- function(cycle) {
  c(sprintf("sr <- trk[, 2] / %s", cd_fmt(cycle)),
      "track <- list(depth = trk[, 1], sedrate = sr, time = c(0, cumsum(diff(trk[, 1]) / ((sr[-1] + sr[-length(sr)]) / 2))))")
}

# hcl.colors palette name for the plot functions that take one (plot_chirp_eha, plot_dechirped)
cd_wv_hcl <- function(spec) {
  p <- cd_pal_split(spec)
  vir <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")
  if (p$name %in% vir) {
    return(p$name)
  }
  hp <- grDevices::hcl.pals()
  k <- match(tolower(p$name), tolower(hp))
  if (!is.na(k)) {
    return(hp[k])
  }
  "viridis"
}

# Reduce a scalogram for storage and plotting; thin a significance object (or extra matrices) consistently
cd_wv_slim <- function(w, sig = NULL, max_cols = 2000) {
  n <- length(w$axis.1)
  w$Power_orders <- NULL
  w$Power_all <- NULL
  w2 <- cd_wt_slim(w, max_cols)
  if (isTRUE(w2$slim)) {
    idx <- seq(1, n, by = ceiling(n / max_cols))
    for (nm in c("Chirp", "significant")) {
      m <- w2[[nm]]
      if (is.matrix(m) && ncol(m) == n) {
        w2[[nm]] <- m[, idx, drop = FALSE]
      } else if (is.matrix(m) && nrow(m) == n) {
        w2[[nm]] <- m[idx, , drop = FALSE]
      }
    }
    if (!is.null(sig)) {
      for (nm in c("significant", "valid")) {
        m <- sig[[nm]]
        if (is.matrix(m) && ncol(m) == n) {
          sig[[nm]] <- m[, idx, drop = FALSE]
        } else if (is.matrix(m) && nrow(m) == n) {
          sig[[nm]] <- m[idx, , drop = FALSE]
        }
      }
    }
  }
  list(w = w2, sig = sig)
}

# Peaks of an average spectrum, as text
cd_wv_peaks_text <- function(period, power, lab = "Peaks of the average spectrum (period)", n = 8) {
  ok <- is.finite(power)
  if (sum(ok) < 3) {
    return(character())
  }
  pk <- cd_peaks(power)
  if (!length(pk)) {
    return(character())
  }
  pk <- pk[order(-power[pk])][seq_len(min(n, length(pk)))]
  sprintf("%s: %s", lab, paste(cd_fmt_period(period[pk]), collapse = ", "))
}

# Image with optional line on top (base graphics), colour bar on the right
cd_wv_image <- function(x, y, z, xlab, ylab, zlab, palette = "viridis|viridis", ylog = FALSE, line_x = NULL,
    line_y = NULL, main = NULL, zlim = NULL) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, nrow = 1), widths = c(1, 0.12))
  cd_par()
  cols <- cd_palette(100, palette)
  ox <- order(x)
  oy <- order(y)
  z <- z[ox, oy, drop = FALSE]
  x <- x[ox]
  y <- y[oy]
  # a log axis is drawn as log10 values with labels, so that a regular grid can be drawn as a raster (no seams)
  yy <- if (ylog) { log10(y) } else { y }
  reg <- cd_regular(x) && cd_regular(yy)
  zr <- if (is.null(zlim)) { range(z, finite = TRUE) } else { zlim }
  graphics::image(x, yy, z, col = cols, zlim = zr, xlab = xlab, ylab = ylab, main = main, useRaster = reg,
      yaxt = if (ylog) { "n" } else { "s" })
  if (ylog) {
    tk <- grDevices::axisTicks(range(yy), log = TRUE)
    graphics::axis(2, at = log10(tk), labels = cd_fmt_period(tk))
  }
  if (!is.null(line_x)) {
    ly <- if (ylog) { log10(line_y) } else { line_y }
    graphics::points(line_x, ly, pch = 21, bg = "white", col = "black", cex = 0.6)
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

# Scalogram plot of an analyze.wavelet result with the shared palette, scale and whitening settings
cd_wv_plot_wt <- function(w, p, add_signif = NULL, add_avg = TRUE) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  pal <- cd_pal_split(p$palette)
  pl <- if (is.null(p$periodlab)) { "Period" } else { p$periodlab }
  xl <- if (is.null(p$x_lab)) { "Depth" } else { p$x_lab }
  WaverideR::plot_wavelet(w, periodlab = pl, x_lab = xl, add_avg = add_avg, add_data = TRUE, dev_new = FALSE,
      palette_name = pal$name, color_brewer = pal$family, scale = cd_scale_arg(p$scale), whiten = isTRUE(p$whiten),
      add_signif = add_signif)
}

cd_wv_plot_code <- function(p, var = "res", extra = "") {
  pal <- cd_pal_split(p$palette)
  sprintf("WaverideR::plot_wavelet(%s, periodlab = %s, x_lab = %s, add_avg = TRUE, palette_name = %s, color_brewer = %s%s)",
      var, cd_dep(if (is.null(p$periodlab)) { "Period" } else { p$periodlab }),
      cd_dep(if (is.null(p$x_lab)) { "Depth" } else { p$x_lab }), cd_dep(pal$name), cd_dep(pal$family), extra)
}

cd_wv_label_params <- function() {
  list(cd_txt("periodlab", "Period axis label", "Period (metres)"), cd_txt("x_lab", "Depth axis label", "Depth (metres)"))
}

cd_tools_wavelets <- function() {
  wavelets <- c("Morlet" = "morlet", "Morlet, zero mean" = "morlet0", "Paul" = "paul", "Generalised Morse" = "morse",
      "Bump" = "bump", "Complex Morlet (cmor)" = "cmor", "Complex Gaussian (cgau)" = "cgau", "Shannon" = "shannon",
      "Frequency B-spline" = "fbsp", "Meyer" = "meyer", "Log-Gabor" = "loggabor", "Hann" = "hann", "DOG (real)" = "dog")
  list(

    # Scalograms ####

    cd_tool("wv_transform", "Wavelets", "Adaptive and synchrosqueezed scalograms", group = "Scalograms (WaverideR)",
        heavy = TRUE, icon = "wave-square",
        info = paste("Sharper scalograms than the plain CWT: adaptive Morlet wavelet (number of cycles grows with the",
            "period), superlet synchrosqueezing, chirplet synchrosqueezing (follows changes of the sedimentation rate,",
            "with a map of the rate of change of the period) and ConceFT / multitaper synchrosqueezing (robust against noise)."),
        uses = paste("WaverideR::analyse_Awavelet / analyse_superlet_sst / analyse_chirplet_sst / analyse_conceft,",
            "WaverideR::plot_Awavelet / plot_wavelet"),
        params = c(list(
          cd_sel("method", "Transform", c("Adaptive Morlet wavelet (Awavelet)" = "awavelet",
              "Superlet synchrosqueezing (superlet SST)" = "superlet_sst",
              "Chirplet synchrosqueezing (chirplet SST, slow)" = "chirplet_sst",
              "ConceFT (multitaper synchrosqueezing, Daubechies et al. 2016)" = "conceft",
              "Multitaper SST (average of Morse wavelets)" = "multitaper")),
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("dj", "Scale resolution dj (smaller = more periods, slower)", cd_dj_default, min = 0.001, step = 0.005),
          cd_num("omega_min", "Number of cycles at the shortest period (omega_min)", 6, min = 1, step = 1,
              show_if = list(param = "method", values = "awavelet")),
          cd_num("omega_max", "Number of cycles at the longest period (omega_max)", 12, min = 1, step = 1,
              show_if = list(param = "method", values = "awavelet")),
          cd_sel("scaling", "Change of the number of cycles with the period", c("Linear in log2(period)" = "log2",
              "Linear in the scale index" = "none", "Linear in period" = "linear", "Square root" = "sqrt",
              "Quadratic" = "quadratic"), show_if = list(param = "method", values = "awavelet")),
          cd_sel("wavelet", "Mother wavelet", wavelets, show_if = list(param = "method", values = "awavelet")),
          cd_txt("orders", "Number of cycles of the orders (omega_0)", "8, 12, 16", numeric = TRUE,
              show_if = list(param = "method", values = c("superlet_sst", "chirplet_sst"))),
          cd_sel("combine", "Combination of the orders", c("Geometric mean" = "geometric", "Minimum" = "min",
              "Arithmetic mean" = "mean"), show_if = list(param = "method", values = c("superlet_sst", "chirplet_sst"))),
          cd_num("chirp_max", "Largest dimensionless chirp tried", 2, min = 0.1, step = 0.25,
              show_if = list(param = "method", values = "chirplet_sst")),
          cd_num("chirp_step", "Chirp step", 0.25, min = 0.05, step = 0.05,
              show_if = list(param = "method", values = "chirplet_sst")),
          cd_chk("show_chirp", "Show the map of the rate of change of the period instead of the power", FALSE,
              show_if = list(param = "method", values = "chirplet_sst")),
          cd_num("K", "Number of Morse wavelets (K)", 2, min = 1, step = 1,
              show_if = list(param = "method", values = c("conceft", "multitaper"))),
          cd_num("n_comb", "Number of random combinations", 20, min = 1, step = 5,
              show_if = list(param = "method", values = "conceft")),
          cd_num("seed", "Random seed", 1, step = 1, show_if = list(param = "method", values = "conceft"))),
          cd_wv_label_params(), cd_scalogram_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dm <- as.matrix(s)
          m <- p$method
          ords <- p$orders
          if (!length(ords)) {
            ords <- c(8, 12, 16)
          }
          head <- sprintf("data = as.matrix(dat), lowerPeriod = %s, upperPeriod = %s, dj = %s", cd_fmt(p$lowerPeriod),
              cd_fmt(p$upperPeriod), cd_fmt(p$dj))
          if (m == "awavelet") {
            r <- WaverideR::analyse_Awavelet(data = dm, dj = p$dj, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
                omega_min = p$omega_min, omega_max = p$omega_max, scaling = p$scaling, wavelet = p$wavelet)
            code <- sprintf("res <- WaverideR::analyse_Awavelet(%s,\n    omega_min = %s, omega_max = %s, scaling = %s, wavelet = %s)",
                head, cd_fmt(p$omega_min), cd_fmt(p$omega_max), cd_dep(p$scaling), cd_dep(p$wavelet))
            pal <- cd_pal_split(p$palette)
            pcode <- sprintf("WaverideR::plot_Awavelet(res, periodlab = %s, x_lab = %s, add_avg = TRUE, palette_name = %s, color_brewer = %s)",
                cd_dep(p$periodlab), cd_dep(p$x_lab), cd_dep(pal$name), cd_dep(pal$family))
          } else if (m == "superlet_sst") {
            r <- WaverideR::analyse_superlet_sst(data = dm, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
                dj = p$dj, omega_nr = ords, combine = p$combine)
            code <- sprintf("res <- WaverideR::analyse_superlet_sst(%s,\n    omega_nr = %s, combine = %s)", head, cd_dep(ords),
                cd_dep(p$combine))
            pcode <- cd_wv_plot_code(p)
          } else if (m == "chirplet_sst") {
            ch <- seq(-abs(p$chirp_max), abs(p$chirp_max), by = p$chirp_step)
            r <- WaverideR::analyse_chirplet_sst(data = dm, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
                dj = p$dj, omega_nr = ords, chirp = ch, combine = p$combine)
            code <- sprintf("res <- WaverideR::analyse_chirplet_sst(%s,\n    omega_nr = %s, chirp = seq(%s, %s, by = %s), combine = %s)",
                head, cd_dep(ords), cd_fmt(-abs(p$chirp_max)), cd_fmt(abs(p$chirp_max)), cd_fmt(p$chirp_step), cd_dep(p$combine))
            pcode <- cd_wv_plot_code(p)
          } else {
            r <- WaverideR::analyse_conceft(data = dm, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, dj = p$dj,
                K = p$K, n_comb = p$n_comb, method = m, seed = p$seed)
            code <- sprintf("res <- WaverideR::analyse_conceft(%s,\n    K = %d, n_comb = %d, method = %s, seed = %d)", head,
                as.integer(p$K), as.integer(p$n_comb), cd_dep(m), as.integer(p$seed))
            pcode <- cd_wv_plot_code(p)
          }
          o <- list()
          txt <- cd_even_note(d, s)
          if (!is.null(r$Power.avg) && !is.null(r$Period)) {
            avg <- data.frame(period = r$Period, power_avg = r$Power.avg)
            o[[1]] <- cd_output(cd_out_name(file, paste0("-", m, "_avg")), avg, header = TRUE, var = "avg")
            code <- c(code, "avg <- data.frame(period = res$Period, power_avg = res$Power.avg)")
            txt <- c(txt, cd_wv_peaks_text(avg$period, avg$power_avg))
          }
          if (m == "chirplet_sst" && is.matrix(r$Chirp)) {
            txt <- c(txt, sprintf("Relative rate of change of the period: %s to %s per depth unit (5-95%% range).",
                cd_fmt(stats::quantile(r$Chirp, 0.05, na.rm = TRUE)), cd_fmt(stats::quantile(r$Chirp, 0.95, na.rm = TRUE))))
          }
          r <- cd_wv_slim(r)$w
          r$cd_method <- m
          list(result = r, outputs = o, code = c(cd_even_code(d), code, pcode, cd_code_write(o)), text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          if (identical(res$cd_method, "awavelet")) {
            pal <- cd_pal_split(p$palette)
            WaverideR::plot_Awavelet(res, periodlab = p$periodlab, x_lab = p$x_lab, add_avg = TRUE, dev_new = FALSE,
                palette_name = pal$name, color_brewer = pal$family)
          } else if (identical(res$cd_method, "chirplet_sst") && isTRUE(p$show_chirp) && is.matrix(res$Chirp)) {
            z <- res$Chirp
            if (nrow(z) == length(res$Period)) {
              z <- t(z)
            }
            # symmetric colour range clipped at the 98th percentile, so that a few extreme cells do not wash out the map
            lim <- stats::quantile(abs(z), 0.98, na.rm = TRUE)
            zl <- NULL
            if (is.finite(lim) && lim > 0) {
              z <- pmin(pmax(z, -lim), lim)
              zl <- c(-lim, lim)
            }
            cd_wv_image(res$axis.1, res$Period, z, xlab = p$x_lab, ylab = p$periodlab, ylog = TRUE,
                zlab = "Rate of change of the period (per unit)", palette = "RColorBrewer|RdYlBu", zlim = zl)
          } else {
            cd_wv_plot_wt(res, p)
          }
        }),

    cd_tool("wv_xsuperlet", "Wavelets", "Cross superlet (2 series)", group = "Scalograms (WaverideR)", heavy = TRUE,
        inputs = 2L, icon = "arrows-left-right",
        info = paste("Cross-superlet power and phase of two series (the superlet counterpart of the cross wavelet);",
            "series 2 is interpolated onto the depths of series 1."),
        uses = "WaverideR::analyse_Xsuperlet, WaverideR::plot_Xsuperlet",
        params = list(
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("Nf", "Number of periods", 128, min = 20, step = 10),
          cd_num("c1", "Base number of cycles c1", 3, min = 1, step = 0.5),
          cd_txt("o", "Superlet order (min, max)", "1, 10", numeric = TRUE),
          cd_chk("plot_arrows", "Show phase arrows", TRUE),
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
          o <- p$o
          if (!length(o)) {
            o <- c(1, 10)
          }
          r <- WaverideR::analyse_Xsuperlet(data_1 = as.matrix(s1), data_2 = as.matrix(s2i), upperPeriod = p$upperPeriod,
              lowerPeriod = p$lowerPeriod, Nf = p$Nf, c1 = p$c1, o = o)
          avg <- data.frame(period = r$Period, power_avg = r$Power.avg)
          out <- list(cd_output(cd_out_name(file, "-Xsuperlet_avg"), avg, header = TRUE, var = "avg"))
          pal <- cd_pal_split(p$palette)
          code <- c(cd_code_read(file2, var = "dat2"),
              sprintf("dat <- dat[dat[, 1] >= %s & dat[, 1] <= %s, ]", cd_fmt(lo), cd_fmt(hi)),
              "dat2 <- data.frame(dat[, 1], approx(dat2[, 1], dat2[, 2], dat[, 1])$y)",
              sprintf(paste0("res <- WaverideR::analyse_Xsuperlet(data_1 = as.matrix(dat), data_2 = as.matrix(dat2),\n",
                  "    upperPeriod = %s, lowerPeriod = %s, Nf = %d, c1 = %s, o = %s)"), cd_fmt(p$upperPeriod),
                  cd_fmt(p$lowerPeriod), as.integer(p$Nf), cd_fmt(p$c1), cd_dep(o)),
              sprintf("WaverideR::plot_Xsuperlet(res, periodlab = %s, x_lab = %s, plot_arrows = %s, palette_name = %s, color_brewer = %s)",
                  cd_dep(p$periodlab), cd_dep(p$x_lab), p$plot_arrows, cd_dep(pal$name), cd_dep(pal$family)),
              "avg <- data.frame(period = res$Period, power_avg = res$Power.avg)", cd_code_write(out))
          r$Wave <- NULL
          list(result = r, outputs = out, code = code,
              text = c(sprintf("Overlap %s to %s, %d points", cd_fmt(lo), cd_fmt(hi), nrow(s1)),
                  cd_wv_peaks_text(avg$period, avg$power_avg, "Peaks of the average cross power (period)")))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          pal <- cd_pal_split(p$palette)
          WaverideR::plot_Xsuperlet(res, periodlab = p$periodlab, x_lab = p$x_lab, dev_new = FALSE,
              plot_arrows = isTRUE(p$plot_arrows), palette_name = pal$name, color_brewer = pal$family)
        }),

    cd_tool("wv_chirp_eha", "Wavelets", "De-chirped EHA (changing sed. rate)", group = "Scalograms (WaverideR)",
        heavy = TRUE, icon = "chart-area",
        info = paste("Evolutive multitaper spectra in which every window is first warped by the linear change of the",
            "sedimentation rate that sharpens its spectrum most, so cycles whose period changes become sharp lines;",
            "gives the rate gradient per window. Optional significance against warped AR(1) surrogates."),
        uses = "WaverideR::analyse_chirp_eha / chirp_eha_significance / plot_chirp_eha",
        params = list(
          cd_num("win", "Window length (depth units)", cd_wv_len(0.15), min = 0),
          cd_num("step", "Step between windows (0 = window / 20)", 0, min = 0),
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_len(0.075), min = 0),
          cd_num("chirp_max", "Largest relative rate gradient tried (per depth unit)", cd_wv_chirp_default, min = 0),
          cd_num("n_chirp", "Number of gradients on each side of zero", 10, min = 1, step = 1),
          cd_num("nw", "Time-bandwidth product (NW)", 3, min = 1, step = 0.5),
          cd_sel("criterion", "Choice of the gradient of a window", c("Strength of the lines (F-test)" = "ftest",
              "Match with the astronomical template" = "template", "Highest whitened peak" = "peak",
              "Spectral entropy" = "entropy")),
          cd_txt("astro", "Astronomical periods of the template (kyr)", cd_wv_astro_default(), numeric = TRUE,
              show_if = list(param = "criterion", values = "template")),
          cd_num("srmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0, show_if = list(param = "criterion", values = "template")),
          cd_num("srmax", "Highest sed. rate (cm/kyr)", 5, min = 0, show_if = list(param = "criterion", values = "template")),
          cd_sel("unit", "Depth unit of the data", cd_wv_units(), show_if = list(param = "criterion", values = "template")),
          cd_num("n_sim", "Number of AR(1) surrogates for significance (0 = none, slow)", 0, min = 0, step = 5),
          cd_sel("style", "Figure", c("De-chirped map, significant cells outlined" = "outline",
              "Significance (ratio to the 95% surrogate level)" = "significance", "De-chirped map only" = "plain")),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          ch <- cd_wv_chirp_grid(p$chirp_max, p$n_chirp)
          st <- if (is.finite(p$step) && p$step > 0) { p$step } else { NULL }
          u <- as.numeric(p$unit)
          tmpl <- identical(p$criterion, "template")
          ap <- if (tmpl) { cd_wv_astro(p$astro) } else { NULL }
          sr <- if (tmpl) { c(p$srmin, p$srmax) / u } else { NULL }
          r <- WaverideR::analyse_chirp_eha(data = as.matrix(s), window = p$win, step = st, chirp = ch, nw = p$nw,
              lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, criterion = p$criterion, astro_periods = ap,
              sedrate = sr)
          code <- sprintf(paste0("res <- WaverideR::analyse_chirp_eha(data = as.matrix(dat), window = %s, step = %s,\n",
              "    chirp = seq(%s, %s, length.out = %d), nw = %s, lowerPeriod = %s, upperPeriod = %s, criterion = %s%s)"),
              cd_fmt(p$win), if (is.null(st)) { "NULL" } else { cd_fmt(st) }, cd_fmt(min(ch)), cd_fmt(max(ch)), length(ch),
              cd_fmt(p$nw), cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), cd_dep(p$criterion),
              if (tmpl) { sprintf(",\n    astro_periods = %s, sedrate = %s", cd_dep(ap), cd_dep(signif(sr, 6))) } else { "" })
          if (is.finite(p$n_sim) && p$n_sim > 0) {
            r <- WaverideR::chirp_eha_significance(r, as.matrix(s), n_sim = as.integer(p$n_sim))
            code <- c(code, sprintf("res <- WaverideR::chirp_eha_significance(res, as.matrix(dat), n_sim = %d)",
                as.integer(p$n_sim)))
          }
          tab <- data.frame(depth = r$axis.1, gradient = r$chirp)
          if (tmpl && length(r$sedrate) == nrow(tab)) {
            tab$sedrate_cm_kyr <- r$sedrate * u
          }
          o <- list(cd_output(cd_out_name(file, "-chirpEHA"), tab, header = TRUE, var = "tab"))
          code <- c(cd_even_code(d), code,
              sprintf("WaverideR::plot_chirp_eha(res, style = %s, palette = %s)", cd_dep(p$style), cd_dep(cd_wv_hcl(p$palette))),
              if (ncol(tab) == 3) {
                sprintf("tab <- data.frame(depth = res$axis.1, gradient = res$chirp, sedrate_cm_kyr = res$sedrate * %s)", cd_fmt(u))
              } else {
                "tab <- data.frame(depth = res$axis.1, gradient = res$chirp)"
              }, cd_code_write(o))
          txt <- c(cd_even_note(d, s), sprintf("%d windows; chosen rate gradient %s to %s per depth unit.", nrow(tab),
              cd_fmt(min(tab$gradient, na.rm = TRUE)), cd_fmt(max(tab$gradient, na.rm = TRUE))))
          if (!is.null(tab$sedrate_cm_kyr)) {
            txt <- c(txt, sprintf("Best-matching sed. rate %s to %s cm/kyr.", cd_fmt(min(tab$sedrate_cm_kyr, na.rm = TRUE)),
                cd_fmt(max(tab$sedrate_cm_kyr, na.rm = TRUE))))
          }
          r$Wave <- NULL
          r$Phase <- NULL
          r$Ampl <- NULL
          r$template_score_all <- NULL
          list(result = r, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          sty <- p$style
          if (is.null(res$Power_sig) && sty != "plain") {
            sty <- "plain"
          }
          WaverideR::plot_chirp_eha(res, style = sty, palette = cd_wv_hcl(p$palette))
        }),

    cd_tool("wv_chirp_wavelet", "Wavelets", "Chirp-matched wavelet sed. rate map", group = "Scalograms (WaverideR)",
        heavy = TRUE, icon = "gauge",
        info = paste("Scores the astronomical template on scalograms of chirped Morlet wavelets matched to every rate",
            "gradient, for every depth and sedimentation rate (an eCOCO-like evidence map that allows for changing rates).",
            "Shows the best score over the gradients and the best rate at every depth."),
        uses = "WaverideR::analyse_chirp_wavelet",
        params = list(
          cd_txt("astro", "Astronomical periods (kyr)", cd_wv_astro_default(), numeric = TRUE),
          cd_num("srmin", "Lowest sed. rate (cm/kyr)", 0.5, min = 0),
          cd_num("srmax", "Highest sed. rate (cm/kyr)", 5, min = 0),
          cd_sel("unit", "Depth unit of the data", cd_wv_units()),
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("dj", "Period and rate spacing (octaves)", 0.02, min = 0.005, step = 0.005),
          cd_num("omega_nr", "Number of cycles of the wavelet (omega_0)", 10, min = 3, step = 1),
          cd_num("chirp_max", "Largest relative rate gradient tried (per depth unit)", cd_wv_chirp_default, min = 0),
          cd_num("n_chirp", "Number of gradients on each side of zero", 5, min = 1, step = 1),
          cd_num("step", "Step between scored positions (0 = 5 samples)", 0, min = 0),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          u <- as.numeric(p$unit)
          sr <- c(p$srmin, p$srmax) / u
          ap <- cd_wv_astro(p$astro)
          ch <- cd_wv_chirp_grid(p$chirp_max, p$n_chirp)
          st <- if (is.finite(p$step) && p$step > 0) { p$step } else { NULL }
          r <- WaverideR::analyse_chirp_wavelet(data = as.matrix(s), lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
              dj = p$dj, omega_nr = p$omega_nr, chirp = ch, astro_periods = ap, sedrate = sr, step = st)
          sc <- r$template_score_all
          nr <- dim(sc)[1]
          ng <- dim(sc)[2]
          np <- dim(sc)[3]
          best <- matrix(-Inf, nr, np)
          gbest <- matrix(1L, nr, np)
          for (k in seq_len(ng)) {
            v <- sc[, k, ]
            better <- !is.na(v) & v > best
            best[better] <- v[better]
            gbest[better] <- k
          }
          best[!is.finite(best)] <- NA
          rate <- r$sedrate_grid * u
          ib <- integer(np)
          for (j in seq_len(np)) {
            col <- best[, j]
            ib[j] <- if (all(is.na(col))) { NA_integer_ } else { which.max(col) }
          }
          okb <- !is.na(ib)
          tab <- data.frame(depth = r$positions, sedrate_cm_kyr = NA_real_, gradient = NA_real_, score = NA_real_)
          tab$sedrate_cm_kyr[okb] <- rate[ib[okb]]
          tab$score[okb] <- best[cbind(ib[okb], which(okb))]
          tab$gradient[okb] <- r$chirp_grid[gbest[cbind(ib[okb], which(okb))]]
          o <- list(cd_output(cd_out_name(file, "-chirpwavelet_sedrate"), tab, header = TRUE, var = "tab"))
          code <- c(cd_even_code(d),
              sprintf(paste0("res <- WaverideR::analyse_chirp_wavelet(data = as.matrix(dat), lowerPeriod = %s, upperPeriod = %s,\n",
                  "    dj = %s, omega_nr = %s, chirp = seq(%s, %s, length.out = %d), astro_periods = %s,\n",
                  "    sedrate = %s, step = %s)"), cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), cd_fmt(p$dj), cd_fmt(p$omega_nr),
                  cd_fmt(min(ch)), cd_fmt(max(ch)), length(ch), cd_dep(ap), cd_dep(signif(sr, 6)),
                  if (is.null(st)) { "NULL" } else { cd_fmt(st) }),
              "# best score over the gradients for every rate and position",
              "best <- res$template_score_all[, 1, ]",
              "for (k in seq_len(dim(res$template_score_all)[2])) {",
              "  best <- pmax(best, res$template_score_all[, k, ], na.rm = TRUE)",
              "}",
              "ib <- max.col(t(replace(best, is.na(best), -Inf)), ties.method = \"first\")",
              sprintf("tab <- data.frame(depth = res$positions, sedrate_cm_kyr = res$sedrate_grid[ib] * %s)", cd_fmt(u)),
              sprintf("image(res$positions, res$sedrate_grid * %s, t(best), log = \"y\", col = hcl.colors(100, \"viridis\"))", cd_fmt(u)),
              "lines(tab$depth, tab$sedrate_cm_kyr, col = \"white\", lwd = 2)", cd_code_write(o))
          res <- list(x = r$positions, rate = rate, score = best, best = tab)
          list(result = res, outputs = o, code = code,
              text = c(cd_even_note(d, s), sprintf("%d positions, %d rates, %d gradients; best rate %s to %s cm/kyr.", np, nr,
                  ng, cd_fmt(min(tab$sedrate_cm_kyr, na.rm = TRUE)), cd_fmt(max(tab$sedrate_cm_kyr, na.rm = TRUE)))))
        },
        plot = function(res, p) {
          cd_wv_image(res$x, res$rate, t(res$score), xlab = "Depth", ylab = "Sedimentation rate (cm/kyr)",
              zlab = "Template score", palette = p$palette, ylog = TRUE, line_x = res$best$depth,
              line_y = res$best$sedrate_cm_kyr)
        }),

    cd_tool("wv_dechirped", "Wavelets", "De-chirped scalogram along a track (2 files)", group = "Scalograms (WaverideR)",
        heavy = TRUE, inputs = 2L, icon = "ruler-combined",
        info = paste("Second file: the tracked period curve (depth, period) of a cycle of known duration. The record is",
            "tuned with it, transformed in time and mapped back onto depth, so all cycles become sharp lines where the",
            "track is right: a check of the track. Optional significance against warped AR(1) surrogates."),
        uses = "WaverideR::analyse_dechirped, WaverideR::plot_dechirped",
        params = list(
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_sel("transform", "Transform in time", c("Morlet wavelet" = "wavelet", "Superlet SST" = "superlet_sst",
              "Multisynchrosqueezing" = "wavelet_sst", "Chirplet SST (slow)" = "chirplet_sst", "Multitaper EHA" = "eha",
              "Combined (Morlet, EHA and superlet SST)" = "combined")),
          cd_num("lowerPeriod", "Shortest period (kyr; 0 = automatic)", 0, min = 0),
          cd_num("upperPeriod", "Longest period (kyr; 0 = automatic)", 0, min = 0),
          cd_num("dj", "Period spacing (octaves)", 0.02, min = 0.005, step = 0.005),
          cd_num("omega_nr", "Number of cycles of the Morlet wavelet", 10, min = 3, step = 1,
              show_if = list(param = "transform", values = c("wavelet", "wavelet_sst", "combined"))),
          cd_num("window", "EHA window (kyr; 0 = 20 x shortest period)", 0, min = 0,
              show_if = list(param = "transform", values = c("eha", "combined"))),
          cd_num("n_sim", "Number of AR(1) surrogates for significance (0 = none, slow)", 0, min = 0, step = 5),
          cd_num("seed", "Random seed", 1, step = 1),
          cd_sel("style", "Figure", c("De-chirped map, significant cells outlined" = "outline",
              "Significance (ratio to the 95% surrogate level)" = "significance", "De-chirped map only" = "plain",
              "Tuned scalogram (time domain)" = "tuned")),
          cd_sel("palette", "Colour palette", cd_palettes())),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          trk <- cd_wv_track(d2)
          track <- cd_wv_sed_track(trk, p$cycle)
          lo <- p$lowerPeriod
          hi <- p$upperPeriod
          dur <- diff(range(track$time))
          if (!is.finite(lo) || lo <= 0) {
            lo <- signif(3 * cd_dt(s$x) / min(track$sedrate), 3)
          }
          if (!is.finite(hi) || hi <= 0) {
            hi <- signif(dur / 3, 3)
          }
          win <- if (is.finite(p$window) && p$window > 0) { p$window } else { NULL }
          r <- WaverideR::analyse_dechirped(data = as.matrix(s), track = track, transform = p$transform, lowerPeriod = lo,
              upperPeriod = hi, dj = p$dj, omega_nr = p$omega_nr, window = win, n_sim = as.integer(p$n_sim),
              seed = as.integer(p$seed))
          o <- list()
          if (!is.null(r$tuned$Power.avg)) {
            avg <- data.frame(period_kyr = r$tuned$Period, power_avg = r$tuned$Power.avg)
            o[[1]] <- cd_output(cd_out_name(file, paste0("-dechirped_", p$transform, "_avg")), avg, header = TRUE, var = "avg")
          }
          code <- c(cd_even_code(d), cd_wv_track_code(file2), cd_wv_sed_track_code(p$cycle),
              sprintf(paste0("res <- WaverideR::analyse_dechirped(data = as.matrix(dat), track = track, transform = %s,\n",
                  "    lowerPeriod = %s, upperPeriod = %s, dj = %s, omega_nr = %s, window = %s, n_sim = %d, seed = %d)"),
                  cd_dep(p$transform), cd_fmt(lo), cd_fmt(hi), cd_fmt(p$dj), cd_fmt(p$omega_nr),
                  if (is.null(win)) { "NULL" } else { cd_fmt(win) }, as.integer(p$n_sim), as.integer(p$seed)),
              sprintf("WaverideR::plot_dechirped(res, style = %s, palette = %s)", cd_dep(p$style), cd_dep(cd_wv_hcl(p$palette))))
          if (length(o)) {
            code <- c(code, "avg <- data.frame(period_kyr = res$tuned$Period, power_avg = res$tuned$Power.avg)", cd_code_write(o))
          }
          txt <- c(cd_even_note(d, s), sprintf("Track: %s kyr over %s to %s; sed. rate %s to %s (depth units/kyr); periods %s to %s kyr.",
              cd_fmt(dur), cd_fmt(min(trk$depth)), cd_fmt(max(trk$depth)), cd_fmt(min(track$sedrate)), cd_fmt(max(track$sedrate)),
              cd_fmt(lo), cd_fmt(hi)))
          if (length(o)) {
            txt <- c(txt, cd_wv_peaks_text(avg$period_kyr, avg$power_avg, "Peaks of the tuned average spectrum (kyr)"))
          }
          if (is.list(r$tuned)) {
            r$tuned <- cd_wt_slim(r$tuned, max_cols = Inf)
          }
          if (is.list(r$depth)) {
            r$depth <- cd_wt_slim(r$depth, max_cols = Inf)
          }
          list(result = r, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          sty <- p$style
          if (is.null(res$Power_sig) && sty %in% c("outline", "significance")) {
            sty <- "plain"
          }
          WaverideR::plot_dechirped(res, style = sty, palette = cd_wv_hcl(p$palette))
        }),

    # Significance ####

    cd_tool("wv_significance", "Wavelets", "Scalogram significance (CWT / superlet)", group = "Significance",
        heavy = function(p) { !identical(p$test, "analytic") || identical(p$transform, "superlet") }, icon = "check-double",
        info = paste("Which parts of a wavelet or superlet scalogram stand above noise: analytic Torrence & Compo (1998)",
            "levels, Monte Carlo surrogates (white, AR(1), signal-corrected AR(1) or power-law noise, with p-values of",
            "candidate cycles), or percentiles of the average spectrum of red-noise runs."),
        uses = "WaverideR::wavelet_significance / model_red_noise_wt / percentile_from_red_noise, WaverideR::plot_wavelet / plot_superlet",
        params = c(list(
          cd_sel("transform", "Scalogram", c("Morlet CWT" = "cwt", "Superlet" = "superlet")),
          cd_sel("test", "Test", c("Analytic (Torrence & Compo 1998, instantaneous)" = "analytic",
              "Noise surrogates (Monte Carlo)" = "surrogate",
              "Red-noise percentiles of the average spectrum" = "rednoise")),
          cd_sel("background", "Noise background", c("AR(1) red noise" = "ar1", "White noise" = "white",
              "AR(1) without the candidate cycles" = "ar1_corrected", "Power law (1/f^beta)" = "powerlaw"),
              show_if = list(param = "test", values = c("analytic", "surrogate"))),
          cd_txt("notch", "Periods removed before fitting the noise", "", numeric = TRUE,
              show_if = list(param = "background", values = c("ar1_corrected", "powerlaw"))),
          cd_txt("candidates", "Candidate periods to give p-values for", "", numeric = TRUE,
              show_if = list(param = "test", values = "surrogate")),
          cd_num("siglvl", "Significance level", 0.95, min = 0.5, max = 0.999, step = 0.01,
              show_if = list(param = "test", values = c("analytic", "surrogate"))),
          cd_txt("percentiles", "Percentiles of the red noise", "0.9, 0.95, 0.99", numeric = TRUE,
              show_if = list(param = "test", values = "rednoise")),
          cd_num("n_sim", "Number of simulations", 100, min = 5, step = 10,
              show_if = list(param = "test", values = c("surrogate", "rednoise"))),
          cd_num("seed", "Random seed", 1, step = 1, show_if = list(param = "test", values = c("surrogate", "rednoise"))),
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 8, min = 1, step = 1,
              show_if = list(param = "transform", values = "cwt")),
          cd_num("dj", "Scale resolution dj", cd_dj_default, min = 0.001, step = 0.005,
              show_if = list(param = "transform", values = "cwt")),
          cd_num("Nf", "Number of periods", 100, min = 20, step = 10, show_if = list(param = "transform", values = "superlet")),
          cd_num("c1", "Base number of cycles c1", 3, min = 1, step = 0.5, show_if = list(param = "transform", values = "superlet")),
          cd_txt("o", "Superlet order (min, max)", "1, 30", numeric = TRUE,
              show_if = list(param = "transform", values = "superlet"))),
          cd_wv_label_params(), cd_scalogram_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dm <- as.matrix(s)
          if (p$transform == "superlet") {
            if (p$test == "rednoise") {
              stop("red-noise percentiles need the Morlet CWT; choose the CWT or another test", call. = FALSE)
            }
            o <- p$o
            if (!length(o)) {
              o <- c(1, 30)
            }
            w <- WaverideR::analyse_superlet(data = dm, upperPeriod = p$upperPeriod, lowerPeriod = p$lowerPeriod, Nf = p$Nf,
                c1 = p$c1, o = o)
            code <- sprintf("wt <- WaverideR::analyse_superlet(data = as.matrix(dat), upperPeriod = %s, lowerPeriod = %s, Nf = %d, c1 = %s, o = %s)",
                cd_fmt(p$upperPeriod), cd_fmt(p$lowerPeriod), as.integer(p$Nf), cd_fmt(p$c1), cd_dep(o))
          } else {
            w <- cd_wv_cwt(s, p)
            code <- cd_wv_cwt_code(p)
          }
          txt <- cd_even_note(d, s)
          if (p$test == "rednoise") {
            pc <- p$percentiles
            pc <- pc[is.finite(pc) & pc > 0 & pc < 1]
            if (!length(pc)) {
              pc <- 0.95
            }
            set.seed(as.integer(p$seed))
            rn <- WaverideR::model_red_noise_wt(wavelet = w, n_simulations = as.integer(p$n_sim))
            tab <- data.frame(period = w$Period, power_avg = w$Power.avg)
            for (q in pc) {
              cr <- WaverideR::percentile_from_red_noise(red_noise = rn, wavelet = w, percentile = q)
              tab[[sprintf("red_noise_%s", cd_fmt(100 * q))]] <- cr[, 2]
            }
            code <- c(code, sprintf("set.seed(%d)", as.integer(p$seed)),
                sprintf("rn <- WaverideR::model_red_noise_wt(wavelet = wt, n_simulations = %d)", as.integer(p$n_sim)),
                "tab <- data.frame(period = wt$Period, power_avg = wt$Power.avg)")
            for (q in pc) {
              code <- c(code, sprintf("tab$red_noise_%s <- WaverideR::percentile_from_red_noise(red_noise = rn, wavelet = wt, percentile = %s)[, 2]",
                  cd_fmt(100 * q), cd_fmt(q)))
            }
            top <- tab[[ncol(tab)]]
            sig <- cd_peaks(tab$power_avg, top)
            txt <- c(txt, if (length(sig)) {
              sprintf("Peaks of the average spectrum above the %s%% red-noise level (period): %s", cd_fmt(100 * max(pc)),
                  paste(cd_fmt_period(tab$period[sig]), collapse = ", "))
            } else {
              sprintf("No peak of the average spectrum above the %s%% red-noise level.", cd_fmt(100 * max(pc)))
            })
            o <- list(cd_output(cd_out_name(file, "-rednoise_avg"), tab, header = TRUE, var = "tab"))
            return(list(result = list(test = "rednoise", tab = tab), outputs = o, code = c(cd_even_code(d), code, cd_code_write(o)),
                text = txt))
          }
          meth <- p$test
          if (p$transform == "superlet" && meth == "analytic") {
            meth <- "surrogate"
            txt <- c(txt, "The analytic levels are only defined for wavelets: noise surrogates were used for the superlet.")
          }
          if (meth == "analytic" && !(p$background %in% c("ar1", "white"))) {
            stop("the analytic test supports only AR(1) and white noise; choose the surrogate test for this background",
                call. = FALSE)
          }
          notch <- p$notch
          if (!length(notch)) {
            notch <- NULL
          }
          cand <- p$candidates
          if (!length(cand) || meth != "surrogate") {
            cand <- NULL
          }
          sg <- WaverideR::wavelet_significance(w, method = meth, background = p$background, siglvl = p$siglvl,
              n_sim = as.integer(p$n_sim), notch_periods = notch, candidates = cand, seed = as.integer(p$seed))
          code <- c(code, sprintf(paste0("sig <- WaverideR::wavelet_significance(wt, method = %s, background = %s, siglvl = %s,\n",
              "    n_sim = %d, notch_periods = %s, candidates = %s, seed = %d)"), cd_dep(meth), cd_dep(p$background),
              cd_fmt(p$siglvl), as.integer(p$n_sim), if (is.null(notch)) { "NULL" } else { cd_dep(notch) },
              if (is.null(cand)) { "NULL" } else { cd_dep(cand) }, as.integer(p$seed)))
          tab <- data.frame(period = sg$period, global_power = sg$gws, critical_level = sg$gws_crit)
          above <- cd_peaks(tab$global_power, tab$critical_level)
          txt <- c(txt, sprintf("%s test, %s background%s, %s%% level.", meth, p$background,
              if (is.finite(sg$lag1)) { sprintf(" (lag-1 = %s)", cd_fmt(signif(sg$lag1, 3))) } else { "" }, cd_fmt(100 * p$siglvl)),
              if (length(above)) {
                sprintf("Peaks of the global spectrum above the critical level (period): %s",
                    paste(cd_fmt_period(tab$period[above]), collapse = ", "))
              } else {
                "No peak of the global spectrum above the critical level."
              })
          if (!is.null(sg$candidates) && nrow(as.data.frame(sg$candidates))) {
            cd <- as.data.frame(sg$candidates)
            txt <- c(txt, "Candidate cycles:", utils::capture.output(print(cd, row.names = FALSE, digits = 4)))
          }
          o <- list(cd_output(cd_out_name(file, paste0("-signif_", meth)), tab, header = TRUE, var = "tab"))
          pl <- if (p$transform == "superlet") { "plot_superlet" } else { "plot_wavelet" }
          pal <- cd_pal_split(p$palette)
          code <- c(cd_even_code(d), code,
              sprintf("WaverideR::%s(wt, add_signif = sig, add_avg = TRUE, periodlab = %s, x_lab = %s, palette_name = %s, color_brewer = %s)",
                  pl, cd_dep(p$periodlab), cd_dep(p$x_lab), cd_dep(pal$name), cd_dep(pal$family)),
              "tab <- data.frame(period = sig$period, global_power = sig$gws, critical_level = sig$gws_crit)", cd_code_write(o))
          sl <- cd_wv_slim(w, sg)
          list(result = list(test = meth, wt = sl$w, sig = sl$sig, transform = p$transform), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          if (identical(res$test, "rednoise")) {
            tb <- res$tab
            cd_plot_spectrum(1 / tb$period, tb$power_avg, cl = tb[, -(1:2), drop = FALSE],
                cl_names = paste0(sub("red_noise_", "", names(tb)[-(1:2)]), "% red noise"), xaxis = "period", logy = TRUE,
                ylab = "Average wavelet power", peaks_above = tb[[ncol(tb)]])
            return(invisible(NULL))
          }
          if (identical(res$transform, "superlet")) {
            pal <- cd_pal_split(p$palette)
            sc <- cd_scale_arg(p$scale)
            WaverideR::plot_superlet(res$wt, add_signif = res$sig, add_avg = TRUE, periodlab = p$periodlab, x_lab = p$x_lab,
                dev_new = FALSE, palette_name = pal$name, color_brewer = pal$family, scale = if (is.null(sc)) { "auto" } else { sc })
          } else {
            cd_wv_plot_wt(res$wt, p, add_signif = res$sig)
          }
        }),

    cd_tool("wv_sst_significance", "Wavelets", "Synchrosqueezed scalogram significance", group = "Significance",
        heavy = TRUE, icon = "filter",
        info = paste("Keeps only the ridges of a synchrosqueezed transform (SST, superlet SST or ConceFT) that are",
            "unlikely to come from AR noise of the record (optionally with a variance that changes along the record)."),
        uses = "WaverideR::sst_significance, WaverideR::analyse_wavelet_sst / analyse_superlet_sst / analyse_conceft, WaverideR::plot_wavelet",
        params = c(list(
          cd_sel("transform", "Transform", c("Synchrosqueezed wavelet transform" = "sst", "Superlet SST" = "superlet_sst",
              "ConceFT" = "conceft")),
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("dj", "Scale resolution dj", cd_dj_default, min = 0.001, step = 0.005),
          cd_num("omega_nr", "Number of cycles (omega_nr)", 12, min = 1, step = 1,
              show_if = list(param = "transform", values = "sst")),
          cd_num("n_sim", "Number of noise simulations", 20, min = 5, step = 5),
          cd_num("alpha", "Significance level alpha", 0.05, min = 0.001, max = 0.5, step = 0.01),
          cd_num("ar_order_max", "Highest AR order of the noise model", 5, min = 1, step = 1),
          cd_chk("local_variance", "Noise variance changes along the record", FALSE),
          cd_num("seed", "Random seed", 1, step = 1)),
          cd_wv_label_params(), cd_scalogram_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          dm <- as.matrix(s)
          head <- sprintf("data = as.matrix(dat), lowerPeriod = %s, upperPeriod = %s, dj = %s", cd_fmt(p$lowerPeriod),
              cd_fmt(p$upperPeriod), cd_fmt(p$dj))
          if (p$transform == "sst") {
            w <- WaverideR::analyse_wavelet_sst(data = dm, dj = p$dj, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
                omega_nr = p$omega_nr)
            code <- sprintf("wt <- WaverideR::analyse_wavelet_sst(%s, omega_nr = %s)", head, cd_fmt(p$omega_nr))
          } else if (p$transform == "superlet_sst") {
            w <- WaverideR::analyse_superlet_sst(data = dm, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, dj = p$dj)
            code <- sprintf("wt <- WaverideR::analyse_superlet_sst(%s)", head)
          } else {
            w <- WaverideR::analyse_conceft(data = dm, lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod, dj = p$dj)
            code <- sprintf("wt <- WaverideR::analyse_conceft(%s)", head)
          }
          r <- WaverideR::sst_significance(w, n_sim = as.integer(p$n_sim), alpha = p$alpha,
              ar_order_max = as.integer(p$ar_order_max), local_variance = isTRUE(p$local_variance), seed = as.integer(p$seed))
          code <- c(code, sprintf(paste0("res <- WaverideR::sst_significance(wt, n_sim = %d, alpha = %s, ar_order_max = %d,\n",
              "    local_variance = %s, seed = %d)"), as.integer(p$n_sim), cd_fmt(p$alpha), as.integer(p$ar_order_max),
              isTRUE(p$local_variance), as.integer(p$seed)))
          tab <- data.frame(period = r$Period, threshold = r$threshold_period, power_avg_significant = r$Power.avg)
          o <- list(cd_output(cd_out_name(file, paste0("-", p$transform, "_signif")), tab, header = TRUE, var = "tab"))
          code <- c(cd_even_code(d), code, cd_wv_plot_code(p),
              "tab <- data.frame(period = res$Period, threshold = res$threshold_period, power_avg_significant = res$Power.avg)",
              cd_code_write(o))
          frac <- mean(r$significant, na.rm = TRUE)
          txt <- c(cd_even_note(d, s), sprintf("%s%% of the coefficients are significant at alpha = %s.",
              cd_fmt(signif(100 * frac, 3)), cd_fmt(p$alpha)),
              cd_wv_peaks_text(tab$period, tab$power_avg_significant, "Peaks of the significant average spectrum (period)"))
          r$significant <- NULL
          r <- cd_wv_slim(r)$w
          list(result = r, outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          cd_wv_plot_wt(res, p)
        }),

    cd_tool("wv_band_significance", "Wavelets", "Band-averaged power significance", group = "Significance",
        icon = "chart-column",
        info = paste("Averages the wavelet power over a band of periods (e.g. one Milankovitch band) and tests it at",
            "every depth against white or AR(1) noise (Torrence & Compo 1998, eq. 24): is this cycle significant here?"),
        uses = "WaverideR::wavelet_band_significance, WaverideR::analyse_wavelet",
        params = c(list(
          cd_num("pmin", "Shortest period of the band", cd_wv_peak(0.8), min = 0),
          cd_num("pmax", "Longest period of the band", cd_wv_peak(1.25), min = 0),
          cd_sel("background", "Noise background", c("AR(1) red noise" = "ar1", "White noise" = "white")),
          cd_sel("method", "Degrees of freedom", c("Exact (eigenvalues)" = "exact", "Torrence & Compo (1998)" = "torrence")),
          cd_num("siglvl", "Significance level", 0.95, min = 0.5, max = 0.999, step = 0.01)),
          cd_wv_cwt_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          w <- cd_wv_cwt(s, p)
          bs <- WaverideR::wavelet_band_significance(w, period_range = c(p$pmin, p$pmax), background = p$background,
              siglvl = p$siglvl, method = p$method)
          n <- length(bs$x)
          tab <- data.frame(depth = bs$x, band_power = bs$power, critical_level = rep_len(bs$crit, n),
              p_value = rep_len(bs$pvalue, n), significant = as.integer(rep_len(bs$significant, n)))
          o <- list(cd_output(cd_out_name(file, sprintf("-band%s-%s", cd_fmt(p$pmin), cd_fmt(p$pmax))), tab, header = TRUE,
              var = "tab"))
          code <- c(cd_even_code(d), cd_wv_cwt_code(p),
              sprintf(paste0("res <- WaverideR::wavelet_band_significance(wt, period_range = c(%s, %s), background = %s,\n",
                  "    siglvl = %s, method = %s)"), cd_fmt(p$pmin), cd_fmt(p$pmax), cd_dep(p$background), cd_fmt(p$siglvl),
                  cd_dep(p$method)), "plot(res)",
              "tab <- data.frame(depth = res$x, band_power = res$power, critical_level = res$crit, p_value = res$pvalue,",
              "    significant = as.integer(res$significant))", cd_code_write(o))
          nv <- sum(bs$valid)
          txt <- c(cd_even_note(d, s), sprintf("Band %s to %s (%d scales), %s background, %s degrees of freedom.",
              cd_fmt(bs$period_range[1]), cd_fmt(bs$period_range[2]), as.integer(bs$n_scales), bs$background,
              cd_fmt(signif(bs$dof, 3))), sprintf("Significant at %d of %d points outside the cone of influence (%s%%).",
              sum(bs$significant), nv, cd_fmt(signif(100 * sum(bs$significant) / max(1, nv), 3))))
          list(result = list(bs = bs, data = s), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$data, col = "#7f8c8d", ylab = "Value", xlab = "")
          cd_par()
          graphics::plot(res$bs, xlim = range(res$data$x))
          graphics::mtext("Depth / time", side = 1, line = 2.4, cex = 0.9)
        }),

    cd_tool("wv_surrogates", "Wavelets", "Surrogate series (IAAFT / phase randomised)", group = "Significance",
        heavy = function(p) { isTRUE(p$n_sim > 20) }, icon = "shuffle",
        info = paste("Null-hypothesis series with the same spectrum as the data: IAAFT surrogates keep the value",
            "distribution exactly and the spectrum approximately, phase-randomised (FT) surrogates keep the spectrum exactly."),
        uses = "WaverideR::bispectral_surrogate",
        params = list(
          cd_sel("type", "Surrogate type", c("IAAFT (Schreiber & Schmitz 1996)" = "iaaft", "Fourier phase randomisation" = "ft")),
          cd_num("n_sim", "Number of surrogates", 5, min = 1, step = 1),
          cd_num("iaaft_iter", "Maximum IAAFT iterations", 200, min = 1, step = 50,
              show_if = list(param = "type", values = "iaaft")),
          cd_num("seed", "Random seed", 1, step = 1)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          r <- WaverideR::bispectral_surrogate(data = as.matrix(s), type = p$type, iaaft_iter = as.integer(p$iaaft_iter),
              n_sim = as.integer(p$n_sim), seed = as.integer(p$seed))
          sm <- as.matrix(r$surrogates)
          tab <- data.frame(x = s$x, sm)
          names(tab) <- c("x", paste0("surrogate_", seq_len(ncol(sm))))
          first <- data.frame(x = s$x, y = sm[, 1])
          o <- list(cd_output(cd_out_name(file, paste0("-", p$type, "_surr1")), first, var = "first"))
          if (ncol(sm) > 1) {
            o[[2]] <- cd_output(cd_out_name(file, paste0("-", p$type, "_surrogates")), tab, header = TRUE, var = "tab")
          }
          code <- c(cd_even_code(d),
              sprintf("res <- WaverideR::bispectral_surrogate(data = as.matrix(dat), type = %s, iaaft_iter = %d, n_sim = %d, seed = %d)",
                  cd_dep(p$type), as.integer(p$iaaft_iter), as.integer(p$n_sim), as.integer(p$seed)),
              "first <- data.frame(dat[, 1], res$surrogates[, 1])",
              if (ncol(sm) > 1) { "tab <- data.frame(x = dat[, 1], res$surrogates)" } else { character() }, cd_code_write(o))
          txt <- c(cd_even_note(d, s), sprintf("%d %s surrogate(s); relative spectral error %s.", ncol(sm), p$type,
              paste(cd_fmt_period(r$relative_spectral_error), collapse = ", ")))
          list(result = list(data = s, sur = sm[, seq_len(min(3, ncol(sm))), drop = FALSE]), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          ser <- list(res$data)
          lab <- "data"
          for (j in seq_len(ncol(res$sur))) {
            ser[[j + 1]] <- data.frame(x = res$data$x, y = res$sur[, j])
            lab <- c(lab, paste("surrogate", j))
          }
          cd_plot_series(ser, labels = lab, stack = TRUE)
        }),

    # Extraction ####

    cd_tool("wv_extract", "Wavelets", "Extract a cycle of constant period", group = "Cycle extraction",
        icon = "scissors",
        info = paste("Reconstructs a cycle of fixed period from the wavelet transform (band given as factors of the period",
            "or as shortest and longest period), its spectral power, or its amplitude (wavelet analogue of the Hilbert",
            "envelope, absolute calibration); or the envelope of the whole record."),
        uses = paste("WaverideR::extract_signal_stable / extract_signal_stable_V2 / extract_power_stable / extract_amplitude,",
            "WaverideR::analyse_wavelet"),
        params = c(list(
          cd_sel("what", "Extract", c("Cycle, band as factors of the period" = "signal",
              "Cycle, band as shortest and longest period" = "signal_v2", "Spectral power of the cycle" = "power",
              "Amplitude of the cycle" = "amplitude", "Envelope of the whole record" = "envelope")),
          cd_num("cycle", "Period of the cycle", cd_wv_peak(1), min = 0,
              show_if = list(param = "what", values = c("signal", "power", "amplitude"))),
          cd_num("period_down", "Lower band factor", 0.8, min = 0.1, max = 1, step = 0.05,
              show_if = list(param = "what", values = c("signal", "power", "amplitude"))),
          cd_num("period_up", "Upper band factor", 1.2, min = 1, step = 0.05,
              show_if = list(param = "what", values = c("signal", "power", "amplitude"))),
          cd_num("period_min", "Shortest period of the band", cd_wv_peak(0.8), min = 0,
              show_if = list(param = "what", values = "signal_v2")),
          cd_num("period_max", "Longest period of the band", cd_wv_peak(1.2), min = 0,
              show_if = list(param = "what", values = "signal_v2")),
          cd_chk("add_mean", "Add the mean of the data to the cycle", TRUE,
              show_if = list(param = "what", values = c("signal", "signal_v2")))),
          cd_wv_cwt_params()),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          w <- p$what
          code <- cd_even_code(d)
          if (w %in% c("signal", "signal_v2", "power")) {
            wt <- cd_wv_cwt(s, p)
            code <- c(code, cd_wv_cwt_code(p))
          }
          if (w == "signal") {
            r <- WaverideR::extract_signal_stable(wavelet = wt, cycle = p$cycle, period_up = p$period_up,
                period_down = p$period_down, add_mean = isTRUE(p$add_mean))
            code <- c(code, sprintf("res <- WaverideR::extract_signal_stable(wavelet = wt, cycle = %s, period_up = %s, period_down = %s, add_mean = %s)",
                cd_fmt(p$cycle), cd_fmt(p$period_up), cd_fmt(p$period_down), isTRUE(p$add_mean)))
            sfx <- sprintf("-cycle%s", cd_fmt(p$cycle))
          } else if (w == "signal_v2") {
            r <- WaverideR::extract_signal_stable_V2(wavelet = wt, period_max = p$period_max, period_min = p$period_min,
                add_mean = isTRUE(p$add_mean))
            code <- c(code, sprintf("res <- WaverideR::extract_signal_stable_V2(wavelet = wt, period_max = %s, period_min = %s, add_mean = %s)",
                cd_fmt(p$period_max), cd_fmt(p$period_min), isTRUE(p$add_mean)))
            sfx <- sprintf("-cycle%s-%s", cd_fmt(p$period_min), cd_fmt(p$period_max))
          } else if (w == "power") {
            r <- WaverideR::extract_power_stable(scalogram = wt, cycle = p$cycle, period_up = p$period_up,
                period_down = p$period_down)
            code <- c(code, sprintf("res <- WaverideR::extract_power_stable(scalogram = wt, cycle = %s, period_up = %s, period_down = %s)",
                cd_fmt(p$cycle), cd_fmt(p$period_up), cd_fmt(p$period_down)))
            sfx <- sprintf("-power%s", cd_fmt(p$cycle))
          } else if (w == "amplitude") {
            r <- WaverideR::extract_amplitude(signal = as.matrix(s), cycle = p$cycle, band = c(p$period_down, p$period_up),
                omega_nr = p$omega_nr, dj = p$dj)
            code <- c(code, sprintf("res <- WaverideR::extract_amplitude(signal = as.matrix(dat), cycle = %s, band = c(%s, %s), omega_nr = %s, dj = %s)",
                cd_fmt(p$cycle), cd_fmt(p$period_down), cd_fmt(p$period_up), cd_fmt(p$omega_nr), cd_fmt(p$dj)))
            sfx <- sprintf("-ampl%s", cd_fmt(p$cycle))
          } else {
            r <- WaverideR::extract_amplitude(signal = as.matrix(s), periods = c(p$lowerPeriod, p$upperPeriod),
                omega_nr = p$omega_nr, dj = p$dj)
            code <- c(code, sprintf("res <- WaverideR::extract_amplitude(signal = as.matrix(dat), periods = c(%s, %s), omega_nr = %s, dj = %s)",
                cd_fmt(p$lowerPeriod), cd_fmt(p$upperPeriod), cd_fmt(p$omega_nr), cd_fmt(p$dj)))
            sfx <- "-envelope"
          }
          r <- as.data.frame(r)
          out <- data.frame(x = r[, 1], y = r[, 2])
          o <- list(cd_output(cd_out_name(file, sfx), out, var = "out"))
          code <- c(code, "out <- data.frame(res[, 1], res[, 2])")
          if (w == "power") {
            o[[2]] <- cd_output(cd_out_name(file, paste0(sfx, "_rel")), data.frame(x = r[, 1], y = r[, 4]), var = "rel")
            code <- c(code, "rel <- data.frame(res[, 1], res[, 4])")
          }
          lab <- switch(w, signal = "Extracted cycle", signal_v2 = "Extracted cycle", power = "Spectral power",
              amplitude = "Amplitude", envelope = "Envelope")
          list(result = list(data = s, out = out, what = w, lab = lab), outputs = o, code = c(code, cd_code_write(o)),
              text = c(cd_even_note(d, s), sprintf("%s: %s to %s.", lab, cd_fmt(signif(min(out$y, na.rm = TRUE), 4)),
                  cd_fmt(signif(max(out$y, na.rm = TRUE), 4)))))
        },
        plot = function(res, p) {
          if (res$what %in% c("signal", "signal_v2")) {
            top <- res$data
            if (!isTRUE(p$add_mean)) {
              top$y <- top$y - mean(top$y)
            }
            cd_two_panel(top, res$out, top_lab = "Data", bottom_lab = res$lab, extra_top = res$out, extra_lab = res$lab)
          } else if (res$what %in% c("amplitude", "envelope")) {
            top <- res$data
            top$y <- top$y - mean(top$y)
            cd_two_panel(top, res$out, top_lab = "Data (centred)", bottom_lab = res$lab, extra_top = res$out,
                extra_lab = res$lab)
          } else {
            cd_two_panel(res$data, res$out, top_lab = "Data", bottom_lab = res$lab)
          }
        }),

    cd_tool("wv_extract_track", "Wavelets", "Extract a tracked cycle (2 files)", group = "Cycle extraction",
        inputs = 2L, icon = "wand-magic-sparkles",
        info = paste("Second file: the tracked period curve (depth, period). Extracts the tracked cycle (or another cycle",
            "scaled from it) with band limits from the Gabor uncertainty of the wavelet, its spectral power, or its amplitude",
            "along the changing period."),
        uses = "WaverideR::extract_signal_standard_deviation / extract_power / extract_amplitude, WaverideR::analyse_wavelet",
        params = c(list(
          cd_sel("what", "Extract", c("Cycle, band from the wavelet uncertainty" = "signal",
              "Spectral power of the cycle" = "power", "Amplitude of the cycle" = "amplitude")),
          cd_num("tracked", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_num("extract", "Duration of the cycle to extract (kyr)", 405, min = 0),
          cd_num("multi", "Band half-width (standard deviations of the uncertainty)", 1, min = 0.1, step = 0.25,
              show_if = list(param = "what", values = "signal")),
          cd_chk("add_mean", "Add the mean of the data to the cycle", TRUE, show_if = list(param = "what", values = "signal")),
          cd_num("period_down", "Lower band factor", 0.8, min = 0.1, max = 1, step = 0.05,
              show_if = list(param = "what", values = c("power", "amplitude"))),
          cd_num("period_up", "Upper band factor", 1.2, min = 1, step = 0.05,
              show_if = list(param = "what", values = c("power", "amplitude")))),
          cd_wv_cwt_params(omega = 10)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          trk <- cd_wv_track(d2)
          tm <- as.matrix(trk)
          w <- p$what
          code <- c(cd_even_code(d), cd_wv_track_code(file2))
          sc <- p$extract / p$tracked
          if (w %in% c("signal", "power")) {
            wt <- cd_wv_cwt(s, p)
            code <- c(code, cd_wv_cwt_code(p))
          }
          if (w == "signal") {
            r <- WaverideR::extract_signal_standard_deviation(wavelet = wt, tracked_cycle_curve = tm, multi = p$multi,
                extract_cycle = p$extract, tracked_cycle_period = p$tracked, add_mean = isTRUE(p$add_mean))
            code <- c(code, sprintf(paste0("res <- WaverideR::extract_signal_standard_deviation(wavelet = wt, tracked_cycle_curve = as.matrix(trk),\n",
                "    multi = %s, extract_cycle = %s, tracked_cycle_period = %s, add_mean = %s)"), cd_fmt(p$multi),
                cd_fmt(p$extract), cd_fmt(p$tracked), isTRUE(p$add_mean)))
            sfx <- sprintf("-cycle%skyr", cd_fmt(p$extract))
            lab <- "Extracted cycle"
          } else if (w == "power") {
            r <- WaverideR::extract_power(tracked_period_curve = tm, scalogram = wt, period_up = p$period_up,
                period_down = p$period_down, tracked_cycle_period = p$tracked, extract_cycle_power = p$extract)
            code <- c(code, sprintf(paste0("res <- WaverideR::extract_power(tracked_period_curve = as.matrix(trk), scalogram = wt,\n",
                "    period_up = %s, period_down = %s, tracked_cycle_period = %s, extract_cycle_power = %s)"), cd_fmt(p$period_up),
                cd_fmt(p$period_down), cd_fmt(p$tracked), cd_fmt(p$extract)))
            sfx <- sprintf("-power%skyr", cd_fmt(p$extract))
            lab <- "Spectral power"
          } else {
            cyc <- tm
            cyc[, 2] <- cyc[, 2] * sc
            r <- WaverideR::extract_amplitude(signal = as.matrix(s), cycle = cyc, band = c(p$period_down, p$period_up),
                omega_nr = p$omega_nr, dj = p$dj)
            code <- c(code, sprintf("cyc <- data.frame(trk[, 1], trk[, 2] * %s / %s)", cd_fmt(p$extract), cd_fmt(p$tracked)),
                sprintf("res <- WaverideR::extract_amplitude(signal = as.matrix(dat), cycle = as.matrix(cyc), band = c(%s, %s), omega_nr = %s, dj = %s)",
                    cd_fmt(p$period_down), cd_fmt(p$period_up), cd_fmt(p$omega_nr), cd_fmt(p$dj)))
            sfx <- sprintf("-ampl%skyr", cd_fmt(p$extract))
            lab <- "Amplitude"
          }
          r <- as.data.frame(r)
          out <- data.frame(x = r[, 1], y = r[, 2])
          o <- list(cd_output(cd_out_name(file, sfx), out, var = "out"))
          code <- c(code, "out <- data.frame(res[, 1], res[, 2])")
          if (w == "power") {
            o[[2]] <- cd_output(cd_out_name(file, paste0(sfx, "_rel")), data.frame(x = r[, 1], y = r[, 4]), var = "rel")
            code <- c(code, "rel <- data.frame(res[, 1], res[, 4])")
          }
          list(result = list(data = s, out = out, what = w, lab = lab), outputs = o, code = c(code, cd_code_write(o)),
              text = sprintf("%s of the %s-kyr cycle (tracked %s-kyr cycle, %d track points).", lab, cd_fmt(p$extract),
                  cd_fmt(p$tracked), nrow(trk)))
        },
        plot = function(res, p) {
          if (res$what == "signal") {
            top <- res$data
            if (!isTRUE(p$add_mean)) {
              top$y <- top$y - mean(top$y)
            }
            cd_two_panel(top, res$out, top_lab = "Data", bottom_lab = res$lab, extra_top = res$out, extra_lab = res$lab)
          } else if (res$what == "amplitude") {
            top <- res$data
            top$y <- top$y - mean(top$y)
            cd_two_panel(top, res$out, top_lab = "Data (centred)", bottom_lab = res$lab, extra_top = res$out,
                extra_lab = res$lab)
          } else {
            cd_two_panel(res$data, res$out, top_lab = "Data", bottom_lab = res$lab)
          }
        }),

    cd_tool("wv_dynamic_filter", "Wavelets", "Dynamic filter along a tracked period (2 files)", group = "Cycle extraction",
        inputs = 2L, icon = "sliders",
        info = paste("Second file: the tracked period curve (depth, period). Band-pass filter whose pass band follows the",
            "tracked period (the dynamic filter of Acycle, Thibault & Rizzi / Li), computed in sliding Hann windows."),
        uses = "WaverideR::dynamic_filter_controls / dynamic_filter / plot_dynamic_filter",
        params = list(
          cd_num("rel_bandwidth", "Relative bandwidth of the pass band", 0.3, min = 0.01, max = 1.5, step = 0.05),
          cd_num("n_points", "Number of control points (0 = all track points)", 0, min = 0, step = 5),
          cd_num("window_length", "Window length (depth units)", cd_wv_len(0.2), min = 0),
          cd_num("step_length", "Window step (depth units)", function(d) { signif(5 * cd_dt(d[, 1]), 3) }, min = 0),
          cd_chk("trim", "Drop the ends covered by less than 10% of a window (edge blow-up)", TRUE),
          cd_chk("period", "Show the pass band as period", FALSE)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          trk <- cd_wv_track(d2)
          np <- if (is.finite(p$n_points) && p$n_points > 1) { as.integer(p$n_points) } else { NULL }
          ctrl <- WaverideR::dynamic_filter_controls(tracked = as.matrix(trk), rel_bandwidth = p$rel_bandwidth, n_points = np,
              data = as.matrix(s))
          r <- WaverideR::dynamic_filter(data = as.matrix(s), controls = ctrl, window_length = p$window_length,
              step_length = p$step_length)
          f <- as.data.frame(r$filtered)
          keep <- rep(TRUE, nrow(f))
          if (isTRUE(p$trim)) {
            keep <- f$coverage_weight >= 0.1 * max(f$coverage_weight, na.rm = TRUE)
          }
          out <- data.frame(x = f$depth[keep], y = f$filtered[keep])
          o <- list(cd_output(cd_out_name(file, "-dynfilter"), out, var = "out"))
          code <- c(cd_even_code(d), cd_wv_track_code(file2),
              sprintf("ctrl <- WaverideR::dynamic_filter_controls(tracked = as.matrix(trk), rel_bandwidth = %s, n_points = %s, data = as.matrix(dat))",
                  cd_fmt(p$rel_bandwidth), if (is.null(np)) { "NULL" } else { as.character(np) }),
              sprintf("res <- WaverideR::dynamic_filter(data = as.matrix(dat), controls = ctrl, window_length = %s, step_length = %s)",
                  cd_fmt(p$window_length), cd_fmt(p$step_length)),
              sprintf("WaverideR::plot_dynamic_filter(data = as.matrix(dat), dyn_filter = res, period = %s)", isTRUE(p$period)),
              "f <- as.data.frame(res$filtered)",
              if (isTRUE(p$trim)) { "f <- f[f$coverage_weight >= 0.1 * max(f$coverage_weight), ]" } else { character() },
              "out <- data.frame(f$depth, f$filtered)", cd_code_write(o))
          txt <- c(cd_even_note(d, s), sprintf("%d windows of %s (step %s); %d control points.", nrow(r$windows),
              cd_fmt(r$window_length), cd_fmt(r$step_length), nrow(ctrl)),
              if (isTRUE(p$trim) && any(!keep)) { sprintf("%d end points with little window coverage dropped.", sum(!keep)) } else { character() })
          r$filtered <- r$filtered[keep, , drop = FALSE]
          list(result = list(data = s, dyn = r), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          WaverideR::plot_dynamic_filter(data = as.matrix(res$data), dyn_filter = res$dyn, period = isTRUE(p$period))
        }),

    # Diagnostics and uncertainty ####

    cd_tool("wv_transients", "Wavelets", "Detect hiatuses and transients", group = "Diagnostics and uncertainty",
        icon = "bolt",
        info = paste("Finds depths where energy over many periods arrives at once (phase jumps of all cycles at a hiatus,",
            "erosion surfaces, turbidites, spikes) with the transient-extracting (time-reassigned) wavelet transform."),
        uses = "WaverideR::detect_transients",
        params = list(
          cd_num("lowerPeriod", "Shortest period", cd_wv_lo, min = 0),
          cd_num("upperPeriod", "Longest period", cd_wv_hi, min = 0),
          cd_num("dj", "Period spacing dj (octaves)", 0.05, min = 0.005, step = 0.01),
          cd_num("omega_nr", "Number of cycles (small = sharper in depth)", 6, min = 2, step = 1),
          cd_num("threshold", "Detection threshold (multiple of the running median)", 4, min = 1.1, step = 0.5),
          cd_num("min_distance", "Minimum distance between transients (0 = longest period / 4)", 0, min = 0),
          cd_num("window", "Running-median window (0 = 2 x longest period)", 0, min = 0)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          md <- if (is.finite(p$min_distance) && p$min_distance > 0) { p$min_distance } else { NULL }
          wn <- if (is.finite(p$window) && p$window > 0) { p$window } else { NULL }
          r <- WaverideR::detect_transients(data = as.matrix(s), lowerPeriod = p$lowerPeriod, upperPeriod = p$upperPeriod,
              dj = p$dj, omega_nr = p$omega_nr, threshold = p$threshold, min_distance = md, window = wn)
          prof <- as.data.frame(r$profile)
          tr <- as.data.frame(r$transients)
          o <- list(cd_output(cd_out_name(file, "-transient_ratio"), data.frame(x = prof$depth, y = prof$ratio), var = "prof"))
          if (nrow(tr)) {
            o[[2]] <- cd_output(cd_out_name(file, "-transients"), tr, header = TRUE, var = "res$transients")
          }
          code <- c(sprintf(paste0("res <- WaverideR::detect_transients(data = as.matrix(dat), lowerPeriod = %s, upperPeriod = %s,\n",
              "    dj = %s, omega_nr = %s, threshold = %s, min_distance = %s, window = %s)"), cd_fmt(p$lowerPeriod),
              cd_fmt(p$upperPeriod), cd_fmt(p$dj), cd_fmt(p$omega_nr), cd_fmt(p$threshold),
              if (is.null(md)) { "NULL" } else { cd_fmt(md) }, if (is.null(wn)) { "NULL" } else { cd_fmt(wn) }),
              "prof <- data.frame(res$profile$depth, res$profile$ratio)", cd_code_write(o))
          txt <- if (nrow(tr)) {
            c(sprintf("%d transient(s) above %s x the running median (depth, ratio):", nrow(tr), cd_fmt(p$threshold)),
                paste0("  ", cd_fmt_period(tr$depth), "  (", cd_fmt_period(tr$ratio), ")"))
          } else {
            sprintf("No transient above %s x the running median.", cd_fmt(p$threshold))
          }
          list(result = list(data = s, profile = prof, transients = tr, threshold = p$threshold), outputs = o, code = code,
              text = txt)
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          cd_plot_one(res$data, col = "#7f8c8d", ylab = "Value", xlab = "")
          if (nrow(res$transients)) {
            graphics::abline(v = res$transients$depth, col = "#c0392b", lwd = 1.5)
          }
          cd_par()
          pr <- res$profile
          graphics::plot(pr$depth, pr$ratio, type = "l", col = "#1f4e79", xlab = "Depth / time",
              ylab = "Transient energy / running median")
          graphics::grid(col = "grey90", lty = 1)
          graphics::abline(h = res$threshold, col = "#c0392b", lty = 2)
          if (nrow(res$transients)) {
            graphics::points(res$transients$depth, res$transients$ratio, pch = 25, bg = "#c0392b", col = "#c0392b")
          }
        }),

    cd_tool("wv_uncertainty", "Wavelets", "Uncertainty of a tracked cycle (2 files)", group = "Diagnostics and uncertainty",
        inputs = 2L, icon = "arrows-up-down",
        info = paste("Second file: the tracked period curve (depth, period). Frequency uncertainty of the tracked cycle",
            "from the Gabor uncertainty of the Morlet wavelet or the resolution of an FFT, and the resulting spread of",
            "the time-depth model."),
        uses = "WaverideR::wavelet_uncertainty / fft_uncertainty, WaverideR::analyse_wavelet",
        params = c(list(
          cd_sel("method", "Uncertainty of", c("Morlet wavelet (Gabor)" = "wavelet", "FFT, practical resolution 1/T" = "practical",
              "FFT, Heisenberg-Gabor limit 1/(4 pi T)" = "gabor")),
          cd_num("cycle", "Duration of the tracked cycle (kyr)", 405, min = 0),
          cd_num("multi", "Multiple of the uncertainty", 1, min = 0.1, step = 0.5),
          cd_num("signal_length", "Record or window length for the FFT (0 = length of the track)", 0, min = 0,
              show_if = list(param = "method", values = c("practical", "gabor")))),
          cd_wv_cwt_params(omega = 10, show_if = list(param = "method", values = "wavelet"))),
        compute = function(d, p, d2, file, file2) {
          trk <- cd_wv_track(d2)
          code <- cd_wv_track_code(file2)
          if (p$method == "wavelet") {
            s <- cd_even(d)
            wt <- cd_wv_cwt(s, p)
            r <- WaverideR::wavelet_uncertainty(tracked_cycle = trk, period_of_tracked_cycle = p$cycle, wavelet = wt,
                multi = p$multi)
            code <- c(cd_even_code(d), code, cd_wv_cwt_code(p),
                sprintf("res <- WaverideR::wavelet_uncertainty(tracked_cycle = as.data.frame(trk), period_of_tracked_cycle = %s, wavelet = wt, multi = %s)",
                    cd_fmt(p$cycle), cd_fmt(p$multi)))
          } else {
            sl <- if (is.finite(p$signal_length) && p$signal_length > 0) { p$signal_length } else { NULL }
            r <- WaverideR::fft_uncertainty(tracked_cycle = trk, period_of_tracked_cycle = p$cycle, signal_length = sl,
                type = p$method, multi = p$multi)
            code <- c(code, sprintf("res <- WaverideR::fft_uncertainty(tracked_cycle = trk, period_of_tracked_cycle = %s, signal_length = %s, type = %s, multi = %s)",
                cd_fmt(p$cycle), if (is.null(sl)) { "NULL" } else { cd_fmt(sl) }, cd_dep(p$method), cd_fmt(p$multi)))
          }
          r <- as.data.frame(r)
          nm <- names(r)
          col_of <- function(pat) {
            k <- grep(pat, nm, fixed = TRUE)
            if (!length(k)) {
              stop("unexpected output of the uncertainty function: no column ", pat, call. = FALSE)
            }
            k[1]
          }
          # the first "x_times" column is the frequency uncertainty times multi (same layout for both functions)
          df <- r[[col_of("x_times")]]
          tab <- data.frame(depth = r[[1]], period = r[[2]], frequency = r[[3]], freq_uncertainty = df,
              period_short = 1 / (r[[3]] + df), period_long = 1 / pmax(r[[3]] - df, 1e-12),
              time_mean = r[[col_of("time mean")]], time_short = r[[col_of("time plus")]], time_long = r[[col_of("time min")]])
          o <- list(cd_output(cd_out_name(file, paste0("-uncertainty_", p$method)), tab, header = TRUE, var = "tab"),
              cd_output(cd_out_name(file, sprintf("-agemodel%s_mean", cd_fmt(p$cycle))),
                  data.frame(x = tab$depth, y = tab$time_mean), var = "am"))
          code <- c(code, "nm <- names(res)", "df <- res[[grep(\"x_times\", nm)[1]]]",
              "tab <- data.frame(depth = res[[1]], period = res[[2]], frequency = res[[3]], freq_uncertainty = df,",
              "    period_short = 1 / (res[[3]] + df), period_long = 1 / (res[[3]] - df), time_mean = res[[grep(\"time mean\", nm)]],",
              "    time_short = res[[grep(\"time plus\", nm)]], time_long = res[[grep(\"time min\", nm)]])",
              "am <- data.frame(tab$depth, tab$time_mean)", cd_code_write(o))
          dur <- tab$time_mean[nrow(tab)] - tab$time_mean[1]
          d1 <- abs(tab$time_short[nrow(tab)] - tab$time_short[1])
          d2u <- abs(tab$time_long[nrow(tab)] - tab$time_long[1])
          list(result = tab, outputs = o, code = code,
              text = sprintf("Duration %s kyr (range %s to %s kyr with %s x the uncertainty).", cd_fmt(signif(dur, 4)),
                  cd_fmt(signif(min(d1, d2u), 4)), cd_fmt(signif(max(d1, d2u), 4)), cd_fmt(p$multi)))
        },
        plot = function(res, p) {
          oldpar <- graphics::par(no.readonly = TRUE)
          on.exit(graphics::par(oldpar))
          graphics::layout(matrix(1:2, ncol = 1))
          cd_par()
          yl <- range(c(res$period_short, res$period_long, res$period), finite = TRUE)
          graphics::plot(res$depth, res$period, type = "n", ylim = yl, xlab = "", ylab = "Period")
          graphics::polygon(c(res$depth, rev(res$depth)), c(res$period_short, rev(res$period_long)),
              col = grDevices::adjustcolor("#1f4e79", 0.15), border = NA)
          graphics::lines(res$depth, res$period, col = "#1f4e79", lwd = 1.5)
          graphics::grid(col = "grey90", lty = 1)
          cd_par()
          yl <- range(c(res$time_mean, res$time_short, res$time_long), finite = TRUE)
          graphics::plot(res$depth, res$time_mean, type = "l", col = "#1f4e79", lwd = 1.5, ylim = yl, xlab = "Depth",
              ylab = "Time (kyr)")
          graphics::lines(res$depth, res$time_short, col = "#c0392b", lty = 2)
          graphics::lines(res$depth, res$time_long, col = "#2e8b57", lty = 2)
          graphics::grid(col = "grey90", lty = 1)
          graphics::legend("topleft", c("mean", "shorter periods", "longer periods"), col = c("#1f4e79", "#c0392b", "#2e8b57"),
              lty = c(1, 2, 2), bty = "n", cex = 0.85)
        }),

    cd_tool("wv_resolution", "Wavelets", "Frequency resolution and cycle bands", group = "Diagnostics and uncertainty",
        icon = "ruler",
        info = paste("Frequency uncertainty of a Fourier spectrum of this record (practical 1/T and Heisenberg-Gabor",
            "limit) and the period range a cycle can be told apart from (Rayleigh band), for the periods you give."),
        uses = "WaverideR::fft_frequency_uncertainty / cycle_band",
        params = list(
          cd_txt("periods", "Periods of interest", function(d) { cd_fmt_period(1 / cd_peak_freq(d)) }, numeric = TRUE),
          cd_num("mult", "Number of Rayleigh bandwidths (half-width of the band)", 1, min = 0.1, step = 0.5)),
        compute = function(d, p, d2, file, file2) {
          s <- cd_clean(d)
          tl <- diff(range(s$x))
          per <- p$periods
          per <- per[is.finite(per) & per > 0]
          if (!length(per)) {
            stop("give at least one period of interest", call. = FALSE)
          }
          r <- WaverideR::fft_frequency_uncertainty(T_length = tl, period = per)
          r <- as.data.frame(r)
          bmin <- numeric(length(per))
          bmax <- numeric(length(per))
          for (i in seq_along(per)) {
            cb <- suppressWarnings(WaverideR::cycle_band(per[i], record_length = tl, mult = p$mult))
            bmin[i] <- cb[1]
            bmax[i] <- cb[2]
          }
          r$band_min <- bmin
          r$band_max <- bmax
          o <- list(cd_output(cd_out_name(file, "-resolution"), r, header = TRUE, var = "res"))
          code <- c(sprintf("res <- WaverideR::fft_frequency_uncertainty(T_length = %s, period = %s)", cd_fmt(tl), cd_dep(per)),
              sprintf("bands <- matrix(NA, %d, 2)", length(per)),
              sprintf("per <- %s", cd_dep(per)), "for (i in seq_along(per)) {",
              sprintf("  bands[i, ] <- WaverideR::cycle_band(per[i], record_length = %s, mult = %s)", cd_fmt(tl), cd_fmt(p$mult)),
              "}", "res$band_min <- bands[, 1]", "res$band_max <- bands[, 2]", cd_code_write(o))
          txt <- c(sprintf("Record length %s: practical resolution %s, Heisenberg-Gabor limit %s (cycles per unit).",
              cd_fmt(tl), cd_fmt(signif(r$df_practical[1], 4)), cd_fmt(signif(r$df_gabor[1], 4))),
              paste0("  period ", cd_fmt_period(per), ": band ", cd_fmt_period(bmin), " to ", cd_fmt_period(bmax),
                  " (", cd_fmt(p$mult), " Rayleigh bandwidth)"))
          list(result = list(data = s, tab = r, per = per), outputs = o, code = code, text = txt)
        },
        plot = function(res, p) {
          oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
          on.exit(graphics::par(oldpar))
          cd_par()
          s <- cd_even(res$data)
          sp <- astrochron::periodogram(data.frame(s$x, s$y - mean(s$y)), padfac = 4, output = 1, genplot = FALSE,
              verbose = FALSE)
          fmax <- min(max(sp$Frequency), 3 / min(res$per))
          k <- sp$Frequency <= fmax & sp$Frequency > 0
          graphics::plot(sp$Frequency[k], sp$Power[k], type = "l", col = "#1f4e79", xlab = "Frequency (cycles/unit)",
              ylab = "Power")
          graphics::grid(col = "grey90", lty = 1)
          tb <- res$tab
          for (i in seq_len(nrow(tb))) {
            graphics::rect(1 / tb$band_max[i], 0, 1 / tb$band_min[i], max(sp$Power[k]) * 1.1,
                col = grDevices::adjustcolor("#c0392b", 0.15), border = NA)
            graphics::abline(v = 1 / res$per[i], col = "#c0392b", lty = 2)
          }
        }),

    # Spectra ####

    cd_tool("wv_spectra", "Wavelets", "Bayesian, SWA and average MESA spectra", group = "Spectra (WaverideR)",
        icon = "chart-line",
        info = paste("Bayesian probability spectrum (Gregory 2005, Weedon et al. 2019), periodogram with the Smoothed",
            "Window Average background (Kemp 2016, Weedon 2020), or the average spectrum of an evolutive MESA."),
        uses = "WaverideR::bayes_spectrum / swa_background / mesa_evolutive / mesa_average, astrochron::periodogram",
        params = list(
          cd_sel("method", "Spectrum", c("Bayesian probability spectrum" = "bayes", "Periodogram with SWA background" = "swa",
              "Average of an evolutive MESA" = "mesa")),
          cd_num("win", "MESA window length (depth units)", cd_wv_len(0.15), min = 0,
              show_if = list(param = "method", values = "mesa")),
          cd_num("step", "MESA window step (depth units)", function(d) { signif(5 * cd_dt(d[, 1]), 3) }, min = 0,
              show_if = list(param = "method", values = "mesa")),
          cd_chk("normalise", "Normalise every window before averaging", TRUE, show_if = list(param = "method", values = "mesa")),
          cd_num("fmax", "Maximum frequency shown (cycles/unit)", cd_nyq, min = 0),
          cd_sel("xaxis", "Horizontal axis", c("Frequency" = "frequency", "Period" = "period"))),
        compute = function(d, p, d2, file, file2) {
          s <- cd_even(d)
          m <- p$method
          if (m == "bayes") {
            r <- WaverideR::bayes_spectrum(data = as.matrix(s))
            tab <- data.frame(frequency = r$freq, period = r$period, power = r$power, probability = r$bayes_prob)
            code <- c("res <- WaverideR::bayes_spectrum(data = as.matrix(dat))",
                "tab <- data.frame(frequency = res$freq, period = res$period, power = res$power, probability = res$bayes_prob)")
            txt <- sprintf("Highest probability %s at period %s (lag-1 used for pre-whitening: %s).",
                cd_fmt(signif(r$max_prob, 3)), cd_fmt_period(1 / r$freq_max), cd_fmt(signif(r$rho1, 3)))
          } else if (m == "swa") {
            sp <- as.data.frame(astrochron::periodogram(s, padfac = 1, demean = TRUE, detrend = TRUE, output = 1,
                genplot = FALSE, verbose = FALSE))
            sp <- sp[sp$Frequency > 0 & sp$Power > 0, ]
            bg <- WaverideR::swa_background(freq = sp$Frequency, log_power = log10(sp$Power), n_data = nrow(s))
            tab <- data.frame(frequency = sp$Frequency, power = sp$Power, swa_background = bg$swa,
                ratio = sp$Power / bg$swa)
            code <- c(paste("sp <- astrochron::periodogram(dat, padfac = 1, demean = TRUE, detrend = TRUE, output = 1, genplot = FALSE,",
                "verbose = FALSE)"),
                "sp <- sp[sp$Frequency > 0 & sp$Power > 0, ]",
                "bg <- WaverideR::swa_background(freq = sp$Frequency, log_power = log10(sp$Power), n_data = nrow(dat))",
                "tab <- data.frame(frequency = sp$Frequency, power = sp$Power, swa_background = bg$swa, ratio = sp$Power / bg$swa)")
            txt <- c(sprintf("SWA window %d ordinates, tail model %s.", as.integer(bg$n_windows), bg$diagnostics$tail_model),
                cd_wv_peaks_text(1 / tab$frequency, tab$ratio, "Highest peaks above the background (period)"))
          } else {
            me <- WaverideR::mesa_evolutive(data = as.matrix(s), window = p$win, step = p$step, fmax = p$fmax)
            r <- WaverideR::mesa_average(me, normalise = isTRUE(p$normalise))
            r <- as.data.frame(r)
            tab <- r[r$frequency > 0, c("frequency", "period", "amplitude")]
            code <- c(sprintf("me <- WaverideR::mesa_evolutive(data = as.matrix(dat), window = %s, step = %s, fmax = %s)",
                cd_fmt(p$win), cd_fmt(p$step), cd_fmt(p$fmax)),
                sprintf("tab <- WaverideR::mesa_average(me, normalise = %s)", isTRUE(p$normalise)),
                "tab <- tab[tab$frequency > 0, c(\"frequency\", \"period\", \"amplitude\")]")
            txt <- cd_wv_peaks_text(tab$period, tab$amplitude, "Peaks of the average MESA spectrum (period)")
          }
          o <- list(cd_output(cd_out_name(file, paste0("-", m, "_spectrum")), tab, header = TRUE, var = "tab"))
          list(result = list(tab = tab, method = m), outputs = o, code = c(cd_even_code(d), code, cd_code_write(o)),
              text = c(cd_even_note(d, s), txt))
        },
        plot = function(res, p) {
          tb <- res$tab
          if (res$method == "bayes") {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            graphics::layout(matrix(1:2, ncol = 1))
            k <- tb$frequency > 0 & tb$frequency <= p$fmax
            x <- if (p$xaxis == "period") { tb$period[k] } else { tb$frequency[k] }
            lg <- if (p$xaxis == "period") { "x" } else { "" }
            xl <- if (p$xaxis == "period") { "Period" } else { "Frequency (cycles/unit)" }
            cd_par()
            graphics::plot(x, tb$power[k], type = "l", col = "#1f4e79", log = lg, xlab = "", ylab = "Normalised power")
            graphics::grid(col = "grey90", lty = 1)
            cd_par()
            graphics::plot(x, tb$probability[k], type = "l", col = "#c0392b", log = lg, xlab = xl,
                ylab = "Bayesian probability")
            graphics::grid(col = "grey90", lty = 1)
          } else if (res$method == "swa") {
            oldpar <- graphics::par(no.readonly = TRUE)
            on.exit(graphics::par(oldpar))
            cd_par()
            k <- tb$frequency <= p$fmax
            x <- if (p$xaxis == "period") { 1 / tb$frequency[k] } else { tb$frequency[k] }
            graphics::plot(x, tb$power[k], type = "l", col = "#7f9cbf", log = if (p$xaxis == "period") { "xy" } else { "y" },
                xlab = if (p$xaxis == "period") { "Period" } else { "Frequency (cycles/unit)" }, ylab = "Power")
            graphics::grid(col = "grey90", lty = 1)
            graphics::lines(x, tb$swa_background[k], col = "#c0392b", lwd = 2)
            graphics::legend("topright", c("periodogram", "SWA background"), col = c("#7f9cbf", "#c0392b"), lwd = c(1, 2),
                bty = "n", cex = 0.85)
          } else {
            cd_plot_spectrum(tb$frequency, tb$amplitude, xaxis = p$xaxis, logy = FALSE, fmax = p$fmax,
                ylab = "Average amplitude")
          }
        })
  )
}
