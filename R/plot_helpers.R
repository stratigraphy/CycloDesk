# Plot helpers (base graphics) ####

cd_cols <- function(n) {
  base <- c("#1f4e79", "#c0392b", "#2e8b57", "#8e44ad", "#d35400", "#16a085", "#7f8c8d", "#b7950b")
  rep(base, length.out = n)
}

cd_viridis <- function(n = 100) {
  grDevices::hcl.colors(n, "viridis")
}

cd_par <- function(...) {
  graphics::par(mar = c(4, 4.3, 1.6, 1), mgp = c(2.4, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.9, ...)
}

cd_message_plot <- function(msg) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::text(0.5, 0.5, msg, col = "grey40", cex = 1.1)
}

# Series plot: one or more series on one axis or stacked panels
cd_plot_series <- function(series, labels = NULL, stack = FALSE, standardise = FALSE, type = "l", xlab = "Depth / time",
    ylab = "Value", xlim = NULL, ylim = NULL, rev_x = FALSE, swap = FALSE, main = NULL) {
  n <- length(series)
  if (!n) {
    cd_message_plot("Select one or more data files in the list on the left")
    return(invisible(NULL))
  }
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  cols <- cd_cols(n)
  for (i in seq_len(n)) {
    s <- series[[i]]
    s <- s[stats::complete.cases(s[, 1:2]), 1:2, drop = FALSE]
    if (standardise && nrow(s) > 1) {
      s[, 2] <- (s[, 2] - mean(s[, 2])) / stats::sd(s[, 2])
    }
    series[[i]] <- s
  }
  if (is.null(labels)) {
    labels <- paste("series", seq_len(n))
  }
  if (stack && n > 1) {
    if (swap) {
      graphics::layout(matrix(seq_len(n), nrow = 1))
    } else {
      graphics::layout(matrix(seq_len(n), ncol = 1))
    }
    for (i in seq_len(n)) {
      cd_par()
      s <- series[[i]]
      cd_plot_one(s, col = cols[i], type = type, xlab = xlab, ylab = labels[i], xlim = xlim, rev_x = rev_x,
          swap = swap)
    }
    return(invisible(NULL))
  }
  cd_par()
  allx <- numeric()
  ally <- numeric()
  for (i in seq_len(n)) {
    allx <- c(allx, series[[i]][, 1])
    ally <- c(ally, series[[i]][, 2])
  }
  if (is.null(xlim)) {
    xlim <- range(allx, finite = TRUE)
  }
  if (is.null(ylim)) {
    inx <- allx >= min(xlim) & allx <= max(xlim)
    ylim <- range(ally[inx], finite = TRUE)
  }
  if (rev_x) {
    xlim <- rev(xlim)
  }
  yl <- if (standardise) { "Standardised value" } else if (n == 1) { labels[1] } else { ylab }
  if (swap) {
    graphics::plot(NA, xlim = ylim, ylim = xlim, xlab = yl, ylab = xlab, main = main)
  } else {
    graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = yl, main = main)
  }
  graphics::grid(col = "grey90", lty = 1)
  for (i in seq_len(n)) {
    s <- series[[i]]
    if (swap) {
      graphics::lines(s[, 2], s[, 1], col = cols[i], type = type, pch = 16, cex = 0.5, lwd = 1)
    } else {
      graphics::lines(s[, 1], s[, 2], col = cols[i], type = type, pch = 16, cex = 0.5, lwd = 1)
    }
  }
  if (n > 1) {
    graphics::legend("topright", legend = labels, col = cols, lwd = 2, bty = "n", cex = 0.85)
  }
  invisible(NULL)
}

cd_plot_one <- function(s, col = "#1f4e79", type = "l", xlab = "", ylab = "", xlim = NULL, rev_x = FALSE,
    swap = FALSE, add = FALSE, main = NULL) {
  if (is.null(xlim)) {
    xlim <- range(s[, 1], finite = TRUE)
  }
  inx <- s[, 1] >= min(xlim) & s[, 1] <= max(xlim)
  ylim <- range(s[inx, 2], finite = TRUE)
  if (rev_x) {
    xlim <- rev(xlim)
  }
  if (swap) {
    graphics::plot(s[, 2], s[, 1], type = type, col = col, xlab = ylab, ylab = xlab, ylim = xlim, xlim = ylim,
        pch = 16, cex = 0.5, main = main)
  } else {
    graphics::plot(s[, 1], s[, 2], type = type, col = col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
        pch = 16, cex = 0.5, main = main)
  }
  graphics::grid(col = "grey90", lty = 1)
  if (swap) {
    graphics::lines(s[, 2], s[, 1], col = col, type = type, pch = 16, cex = 0.5)
  } else {
    graphics::lines(s[, 1], s[, 2], col = col, type = type, pch = 16, cex = 0.5)
  }
}

# Spectrum plot with confidence levels and labelled peaks
cd_plot_spectrum <- function(f, pw, cl = NULL, cl_names = NULL, xaxis = "frequency", logy = TRUE, fmax = NULL,
    main = NULL, label_peaks = TRUE, ylab = "Power", peaks_above = NULL) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  cd_par()
  ok <- is.finite(f) & is.finite(pw) & f > 0
  if (!is.null(fmax) && is.finite(fmax)) {
    ok <- ok & f <= fmax
  }
  if (logy) {
    ok <- ok & pw > 0
  }
  f <- f[ok]
  pw <- pw[ok]
  if (!is.null(cl)) {
    cl <- as.matrix(cl)[ok, , drop = FALSE]
  }
  x <- if (xaxis == "period") { 1 / f } else { f }
  yr <- range(c(pw, if (is.null(cl)) { numeric() } else { cl[cl > 0] }), finite = TRUE)
  graphics::plot(x, pw, type = "n", log = paste0(if (logy) { "y" } else { "" }, if (xaxis == "period") { "x" } else { "" }),
      xlab = if (xaxis == "period") { "Period" } else { "Frequency (cycles/unit)" }, ylab = ylab, ylim = yr,
      main = main)
  graphics::grid(col = "grey90", lty = 1)
  lt <- c(2, 1, 3, 4)
  cc <- c("#c0392b", "#d35400", "#8e44ad", "#16a085")
  if (!is.null(cl)) {
    for (j in seq_len(ncol(cl))) {
      graphics::lines(x, cl[, j], col = cc[(j - 1) %% 4 + 1], lty = lt[(j - 1) %% 4 + 1])
    }
  }
  graphics::lines(x, pw, col = "#1f4e79", lwd = 1.2)
  if (!is.null(cl_names) && length(cl_names)) {
    graphics::legend("topright", legend = c("power", cl_names), col = c("#1f4e79", cc[seq_along(cl_names)]),
        lty = c(1, lt[seq_along(cl_names)]), bty = "n", cex = 0.8)
  }
  if (label_peaks && length(pw) > 5) {
    thr <- if (is.null(peaks_above)) { rep(-Inf, length(pw)) } else { peaks_above[ok] }
    pk <- cd_peaks(pw, thr)
    if (length(pk)) {
      pk <- pk[order(-pw[pk])]
      xs <- if (xaxis == "period") { log10(x) } else { x }
      gap <- 0.04 * diff(range(xs))
      keep <- integer()
      for (k in pk) {
        if (length(keep) >= 8) {
          break
        }
        if (!length(keep) || all(abs(xs[keep] - xs[k]) > gap)) {
          keep <- c(keep, k)
        }
      }
      pk <- keep
      graphics::text(x[pk], pw[pk], labels = cd_fmt_period(1 / f[pk]), pos = 3, cex = 0.7, col = "grey20", xpd = NA)
      graphics::points(x[pk], pw[pk], pch = 25, cex = 0.6, bg = "grey20", col = "grey20")
    }
  }
  invisible(NULL)
}

cd_fmt_period <- function(p) {
  out <- character(length(p))
  for (i in seq_along(p)) {
    out[i] <- format(signif(p[i], 3), scientific = FALSE, trim = TRUE)
  }
  out
}

# Local maxima above a threshold
cd_peaks <- function(y, thr = NULL) {
  n <- length(y)
  if (n < 3) {
    return(integer())
  }
  if (is.null(thr)) {
    thr <- rep(-Inf, n)
  }
  i <- 2:(n - 1)
  pk <- i[y[i] > y[i - 1] & y[i] >= y[i + 1] & y[i] > thr[i]]
  pk
}

# Image plot (evolutive spectra) with colour bar
cd_plot_image <- function(x, y, z, xlab = "Depth / time", ylab = "Frequency (cycles/unit)", main = NULL,
    log_power = FALSE, ylog = FALSE, swap = FALSE, zlab = "Power", palette = "viridis|viridis") {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, nrow = 1), widths = c(1, 0.12))
  cd_par()
  if (log_power) {
    z <- log10(pmax(z, max(z, na.rm = TRUE) * 1e-8))
    zlab <- paste0("log10 ", zlab)
  }
  oy <- order(y)
  y <- y[oy]
  z <- z[, oy, drop = FALSE]
  ox <- order(x)
  x <- x[ox]
  z <- z[ox, , drop = FALSE]
  cols <- cd_palette(100, palette)
  reg <- cd_regular(x) && cd_regular(y) && !ylog
  if (swap) {
    graphics::image(y, x, t(z), col = cols, xlab = ylab, ylab = xlab, main = main, log = if (ylog) { "x" } else { "" },
        useRaster = reg, ylim = rev(range(x)))
  } else {
    graphics::image(x, y, z, col = cols, xlab = xlab, ylab = ylab, main = main, log = if (ylog) { "y" } else { "" },
        useRaster = reg)
  }
  graphics::box()
  graphics::par(mar = c(4, 0.5, 1.6, 3.5))
  zr <- range(z, finite = TRUE)
  zz <- seq(zr[1], zr[2], length.out = 100)
  graphics::image(1, zz, matrix(zz, nrow = 1), col = cols, axes = FALSE, xlab = "", ylab = "")
  graphics::axis(4, cex.axis = 0.8)
  graphics::mtext(zlab, side = 4, line = 2.3, cex = 0.8, las = 0)
  graphics::box()
  invisible(NULL)
}

cd_regular <- function(v) {
  dv <- diff(sort(v))
  if (length(dv) < 2) {
    return(TRUE)
  }
  isTRUE(all.equal(dv, rep(dv[1], length(dv))))
}

# Graphical parameters that can be restored without losing the coordinate system of the last panel: shiny reads
# usr, plt and fig after the plot to map clicks and brushes to data coordinates
cd_par_keep_coords <- function(op) {
  # only text and line settings; margins, layout and coordinates are left as drawn (they define the plot region)
  op[intersect(names(op), c("mgp", "las", "tcl", "cex.axis", "cex.lab", "cex.main", "xpd", "bty", "lty",
      "lwd", "col", "bg"))]
}

# Colour palettes of the scalograms and evolutive spectra: "family|name" as used by WaverideR's plot functions
cd_palettes <- function() {
  c("Viridis" = "viridis|viridis", "Magma" = "viridis|magma", "Inferno" = "viridis|inferno", "Plasma" = "viridis|plasma",
      "Cividis (colour-blind safe)" = "viridis|cividis", "Mako" = "viridis|mako", "Rocket" = "viridis|rocket",
      "Turbo (rainbow-like)" = "viridis|turbo", "Jet (MATLAB / Acycle style)" = "colorRamps|matlab.like2",
      "Blue to red" = "colorRamps|blue2red", "Spectral" = "RColorBrewer|Spectral", "Red-yellow-blue" = "RColorBrewer|RdYlBu",
      "Yellow-orange-red" = "RColorBrewer|YlOrRd", "Greys" = "RColorBrewer|Greys", "Zissou" = "grDevices|Zissou 1",
      "Rainbow" = "grDevices|rainbow", "batlowK (scico)" = "scico|batlowK")
}

cd_pal_split <- function(spec) {
  if (is.null(spec) || !nzchar(spec) || !grepl("|", spec, fixed = TRUE)) {
    return(list(family = "viridis", name = "viridis"))
  }
  p <- strsplit(spec, "|", fixed = TRUE)[[1]]
  list(family = p[1], name = p[2])
}

# n colours of a palette, from WaverideR's own palette resolver (viridis when that is not available)
cd_palette <- function(n = 100, spec = "viridis|viridis") {
  p <- cd_pal_split(spec)
  kc <- get0(".wr_key_cols", envir = asNamespace("WaverideR"), inherits = FALSE)
  if (is.function(kc)) {
    out <- try(kc(p$name, p$family, n), silent = TRUE)
    if (!inherits(out, "try-error") && length(out) == n) {
      return(out)
    }
  }
  cd_viridis(n)
}

# Parameters shared by all scalogram tools
cd_scalogram_params <- function() {
  list(
    cd_sel("palette", "Colour palette", cd_palettes()),
    cd_sel("scale", "Colour scale", c("Default (quantiles)" = "default", "Quantiles" = "quantile", "Logarithmic" = "log",
        "Square root" = "sqrt", "Linear" = "linear", "Automatic" = "auto")),
    cd_chk("whiten", "Whiten (equal contrast at every period)", FALSE))
}

cd_scale_arg <- function(s) {
  if (is.null(s) || identical(s, "default")) {
    return(NULL)
  }
  s
}

# Series with a highlighted interval (Select / Remove part): the two movable lines are drawn on top by the browser
cd_plot_interval <- function(d, a, b, keep_inside = TRUE) {
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  cd_par()
  s <- d[stats::complete.cases(d[, 1:2]), 1:2]
  s <- s[order(s[, 1]), ]
  graphics::plot(s[, 1], s[, 2], type = "n", xlab = names(s)[1], ylab = names(s)[2])
  graphics::grid(col = "grey90", lty = 1)
  lo <- min(a, b, na.rm = TRUE)
  hi <- max(a, b, na.rm = TRUE)
  if (is.finite(lo) && is.finite(hi)) {
    graphics::rect(lo, graphics::par("usr")[3], hi, graphics::par("usr")[4],
        col = grDevices::adjustcolor(if (keep_inside) { "#2b5d9b" } else { "#c0392b" }, 0.08), border = NA)
  }
  inside <- s[, 1] >= lo & s[, 1] <= hi
  y_in <- s[, 2]
  y_in[!inside] <- NA
  y_out <- s[, 2]
  y_out[inside] <- NA
  keep_col <- "#1f4e79"
  drop_col <- "#c3cad4"
  graphics::lines(s[, 1], y_out, col = if (keep_inside) { drop_col } else { keep_col })
  graphics::lines(s[, 1], y_in, col = if (keep_inside) { keep_col } else { drop_col })
  graphics::box()
  invisible(NULL)
}

# Periodogram with the pass band of a filter (Filtering, before it has run)
cd_plot_band <- function(d, flow, fhigh) {
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  cd_par()
  s <- cd_even(d)
  sp <- astrochron::periodogram(data.frame(s$x, s$y - mean(s$y)), padfac = 4, output = 1, genplot = FALSE, verbose = FALSE)
  fmax <- max(sp$Frequency)
  if (is.finite(fhigh)) {
    fmax <- min(fmax, max(3 * fhigh, 0.2 * fmax))
  }
  k <- sp$Frequency <= fmax
  graphics::plot(sp$Frequency[k], sp$Power[k], type = "l", col = "#1f4e79", xlab = "Frequency (cycles/unit)", ylab = "Power")
  graphics::grid(col = "grey90", lty = 1)
  if (is.finite(flow) && is.finite(fhigh)) {
    graphics::rect(flow, 0, fhigh, max(sp$Power[k]) * 1.1, col = grDevices::adjustcolor("#c0392b", 0.12), border = NA)
  }
  invisible(NULL)
}
