# Tools: astronomy ####
#
# Deep-time astronomical periods, the precession constant (CTest and related estimators), the Earth-Moon system and
# the geological time scale, all from WaverideR. Most tools are calculators (no data file) that return a table.

# Helpers ####

cd_as_env <- new.env(parent = emptyenv())

# Two-value range c(min, max) from a parsed numeric field, or NULL when it has fewer than two values
cd_as_rng <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) < 2) {
    return(NULL)
  }
  sort(v[1:2])
}

# Age field: one value or c(min, max)
cd_as_age <- function(v, default = 400) {
  v <- v[is.finite(v)]
  if (!length(v)) {
    return(default)
  }
  if (length(v) == 1) {
    return(v)
  }
  sort(v[1:2])
}

# A positive number, or NULL for 0, NA or an empty field
cd_as_pos <- function(x) {
  if (is.null(x) || !length(x) || !is.finite(x[1]) || x[1] <= 0) {
    return(NULL)
  }
  x[1]
}

# Term names typed as a comma separated list
cd_as_terms <- function(s) {
  s <- trimws(strsplit(paste(as.character(s), collapse = ","), "[,;[:space:]]+")[[1]])
  s[nzchar(s)]
}

# Table with row names as a first column
cd_as_rn <- function(m, name = "quantity") {
  m <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
  out <- data.frame(x = rownames(m), m, check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[1] <- name
  rownames(out) <- NULL
  out
}

# Table as text lines for the result window
cd_as_table_text <- function(df, title = NULL, digits = 5, max_rows = 60) {
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

# Value as R code
cd_as_val <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.character(x) && length(x) == 1 && is.null(names(x))) {
    return(cd_dep(x))
  }
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

# Call a WaverideR function with a list of arguments; returns the value and the equivalent line of R code
cd_as_run <- function(fn, args, var = "res") {
  f <- getExportedValue("WaverideR", fn)
  value <- do.call(f, args)
  parts <- character(length(args))
  nms <- names(args)
  for (i in seq_along(args)) {
    parts[i] <- sprintf("%s = %s", nms[i], cd_as_val(args[[i]]))
  }
  list(value = value, code = sprintf("%s <- WaverideR::%s(%s)", var, fn, paste(parts, collapse = ", ")))
}

cd_as_code_write <- function(outputs) {
  out <- character()
  for (o in outputs) {
    out <- c(out, sprintf("CycloDesk::write_series(%s, %s, header = %s)", o$var, cd_dep(o$name),
        if (isTRUE(o$header)) { "TRUE" } else { "FALSE" }))
  }
  out
}

# Precession constant k (kyr) at an age from AstroGeo22, or the user's value
cd_as_k <- function(p, age) {
  if (identical(p$ksrc, "own")) {
    return(p$k)
  }
  WaverideR::astrogeo22(mean(age))$p_kyr[1]
}

cd_as_k_yr <- function(p) {
  if (identical(p$ksrc, "own")) {
    return(p$k * 1000)
  }
  NULL
}

# Colour of an astronomical family
cd_as_fam_col <- function(fam) {
  out <- rep("#7f8c8d", length(fam))
  out[grepl("ecc", fam)] <- "#1f4e79"
  out[grepl("obliq", fam)] <- "#2e8b57"
  out[grepl("prec", fam)] <- "#c0392b"
  out
}

cd_as_ag_curve <- function(tmax = 1000, by = 2) {
  WaverideR::astrogeo22(seq(0, min(4400, tmax), by = by))
}

# WaverideR 0.6.0 astro_mc(domain = "depth") divides the periods by scale_range, i.e. reads it as kyr/m rather than
# m/kyr; this checks the behaviour once so the tool stays right if WaverideR is corrected
cd_as_mc_depth_inverted <- function() {
  if (!is.null(cd_as_env$inv)) {
    return(cd_as_env$inv)
  }
  ok <- tryCatch({
    WaverideR::astro_mc(constraints = list(e405 = c(7.9, 8.4)), age = 100, domain = "depth",
        scale_range = c(0.019, 0.021), nsim = 5, seed = 1, block = 1e4, max_attempts = 1e5, verbose = FALSE)
    TRUE
  }, error = function(e) {
    FALSE
  })
  cd_as_env$inv <- !ok
  cd_as_env$inv
}

# Plot helpers ####

# Stick spectrum of astronomical terms on a logarithmic period axis
cd_as_plot_sticks <- function(per, h, lab, fam, main = NULL, ylab = "Relative weight") {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  cd_par()
  ok <- is.finite(per) & per > 0
  per <- per[ok]
  h <- h[ok]
  lab <- lab[ok]
  fam <- fam[ok]
  if (!length(per)) {
    cd_message_plot("No periods to show")
    return(invisible(NULL))
  }
  cols <- cd_as_fam_col(fam)
  graphics::plot(NA, xlim = range(per) * c(0.8, 1.25), ylim = c(0, 1.45 * max(h)), log = "x", xlab = "Period (kyr)",
      ylab = ylab, main = main)
  graphics::grid(col = "grey90", lty = 1)
  graphics::segments(per, 0, per, h, col = cols, lwd = 2.5)
  graphics::text(per, h, lab, srt = 90, adj = c(-0.08, 0.5), cex = 0.75, col = cols, xpd = TRUE)
  fl <- c("eccentricity", "obliquity", "precession", "other")
  fc <- c("#1f4e79", "#2e8b57", "#c0392b", "#7f8c8d")
  used <- c(any(grepl("ecc", fam)), any(grepl("obliq", fam)), any(grepl("prec", fam)),
      any(!grepl("ecc|obliq|prec", fam)))
  graphics::legend("topright", fl[used], col = fc[used], lwd = 2.5, bty = "n", cex = 0.8)
  invisible(NULL)
}

# One density panel: x is a named list of sample vectors; the first is summarised by its mean and 95% interval
cd_as_dens_panel <- function(x, main = "", xlab = "", ref = NULL, ref_lab = "AstroGeo22", ref2 = NULL,
    ref2_lab = "Zeeden et al. 2023") {
  keep <- list()
  for (nm in names(x)) {
    v <- x[[nm]]
    v <- v[is.finite(v)]
    if (length(v) > 1) {
      keep[[nm]] <- v
    }
  }
  if (!length(keep)) {
    graphics::plot.new()
    graphics::title(main = main)
    graphics::text(0.5, 0.5, "no samples", col = "grey40")
    return(invisible(NULL))
  }
  v1 <- keep[[1]]
  if (stats::sd(v1) <= 1e-9 * max(1, abs(mean(v1)))) {
    graphics::plot.new()
    graphics::title(main = main)
    graphics::text(0.5, 0.5, sprintf("constant: %s %s", cd_fmt(mean(v1)), xlab), col = "grey30")
    return(invisible(NULL))
  }
  dens <- list()
  xr <- numeric()
  yr <- 0
  for (nm in names(keep)) {
    if (stats::sd(keep[[nm]]) > 0) {
      dd <- stats::density(keep[[nm]])
      dens[[nm]] <- dd
      xr <- c(xr, range(dd$x))
      yr <- max(yr, dd$y)
    }
  }
  xr <- range(c(xr, ref, ref2), finite = TRUE)
  cols <- cd_cols(length(dens))
  graphics::plot(NA, xlim = xr, ylim = c(0, yr * 1.12), xlab = xlab, ylab = "Density", main = main)
  graphics::grid(col = "grey90", lty = 1)
  q <- stats::quantile(v1, c(0.025, 0.975))
  graphics::rect(q[1], 0, q[2], yr * 1.12, col = "#1f4e7915", border = NA)
  for (i in seq_along(dens)) {
    graphics::lines(dens[[i]], col = cols[i], lwd = 2)
  }
  graphics::abline(v = mean(v1), col = cols[1], lty = 2)
  lg <- character()
  lc <- character()
  lt <- numeric()
  if (length(dens) > 1) {
    lg <- names(dens)
    lc <- cols
    lt <- rep(1, length(dens))
  }
  if (!is.null(ref) && is.finite(ref)) {
    graphics::abline(v = ref, col = "grey35", lty = 2, lwd = 2)
    lg <- c(lg, ref_lab)
    lc <- c(lc, "grey35")
    lt <- c(lt, 2)
  }
  if (!is.null(ref2) && is.finite(ref2)) {
    graphics::abline(v = ref2, col = "#c0392b", lty = 3, lwd = 2)
    lg <- c(lg, ref2_lab)
    lc <- c(lc, "#c0392b")
    lt <- c(lt, 3)
  }
  if (length(lg)) {
    graphics::legend("topright", lg, col = lc, lty = lt, lwd = 2, bty = "n", cex = 0.72)
  }
  invisible(NULL)
}

# Posterior figure of a Monte Carlo estimate: k, Earth-Moon distance, length of day, days per year and two extras
cd_as_plot_post <- function(post) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  np <- 4 + length(post$extra)
  if (np > 4) {
    graphics::layout(matrix(1:6, nrow = 2, byrow = TRUE))
  } else {
    graphics::layout(matrix(1:4, nrow = 2, byrow = TRUE))
  }
  ref <- post$ref
  dev <- !is.null(post$age) && mean(post$age) >= 359 && mean(post$age) <= 420
  cd_par()
  cd_as_dens_panel(list(k = post$k_kyr), "Precession constant k", "k (kyr)", ref = ref$p_kyr,
      ref2 = if (dev) { 20.70 } else { NULL })
  cd_par()
  cd_as_dens_panel(list(a = post$a_RE), "Earth-Moon distance", "Earth radii", ref = ref$a_RE)
  cd_par()
  cd_as_dens_panel(list(lod = post$lod), "Length of the solar day", "hours", ref = ref$lod_solar_h)
  cd_par()
  cd_as_dens_panel(list(days = post$days), "Days per year", "days", ref = ref$days_per_year)
  for (e in post$extra) {
    cd_par()
    cd_as_dens_panel(e$x, e$main, e$xlab)
  }
  invisible(NULL)
}

# Extra posterior panels from the samples of a ctest / astro_mc object
cd_as_extra_panels <- function(s, depth = TRUE) {
  ex <- list()
  if (depth && !is.null(s$SR)) {
    ex[[1]] <- list(x = list(SR = s$SR * 100), main = "Sedimentation rate", xlab = "cm/kyr")
  } else if (!is.null(s$scale)) {
    ex[[1]] <- list(x = list(scale = s$scale), main = "Time-scale stretch", xlab = "factor")
  }
  pp <- list()
  for (nm in c("obl_ks3", "prec_kg5", "prec_kg2", "prec_kg4", "prec_kg3")) {
    if (!is.null(s[[nm]]) && all(is.finite(s[[nm]]))) {
      pp[[sub("^obl_ks3$", "k+s3", sub("^prec_kg", "k+g", nm))]] <- s[[nm]]
    }
  }
  if (length(pp)) {
    ex[[length(ex) + 1]] <- list(x = pp, main = "Obliquity and precession periods", xlab = "kyr")
  }
  ex
}

# k posterior with the AstroGeo22 curve through time
cd_as_plot_k <- function(res) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, nrow = 1), widths = c(1, 1.15))
  age <- mean(res$age)
  ref <- res$ref
  cd_par()
  cd_as_dens_panel(list(k = res$k_kyr), "Precession constant k", "k (kyr)", ref = ref$p_kyr,
      ref2 = if (age >= 359 && age <= 420) { 20.70 } else { NULL })
  if (!is.null(res$per_term)) {
    pt <- res$per_term
    yl <- graphics::par("usr")[4]
    yy <- yl * seq(0.15, 0.6, length.out = nrow(pt))
    graphics::segments(pt$k_lo95, yy, pt$k_hi95, yy, col = "#d35400", lwd = 2)
    graphics::points(pt$k_kyr, yy, pch = 16, col = "#d35400")
    graphics::text(pt$k_hi95, yy, pt$term, pos = 4, cex = 0.7, col = "#d35400", xpd = TRUE)
  }
  cd_par()
  ag <- cd_as_ag_curve(max(1000, age * 1.4), by = 5)
  q <- stats::quantile(res$k_kyr, c(0.025, 0.5, 0.975), na.rm = TRUE)
  graphics::plot(ag$age_ma, ag$p_kyr, type = "n", xlab = "Age (Ma)", ylab = "k (kyr)",
      ylim = range(c(ag$p_kyr, q), na.rm = TRUE), main = "AstroGeo22 and the estimate")
  graphics::grid(col = "grey90", lty = 1)
  ok <- is.finite(ag$p_min_arcsec) & is.finite(ag$p_max_arcsec)
  graphics::polygon(c(ag$age_ma[ok], rev(ag$age_ma[ok])), c(1296 / ag$p_min_arcsec[ok], rev(1296 / ag$p_max_arcsec[ok])),
      col = "grey85", border = NA)
  graphics::lines(ag$age_ma, ag$p_kyr, col = "grey35", lwd = 2)
  graphics::arrows(age, q[1], age, q[3], angle = 90, code = 3, length = 0.04, col = "#1f4e79", lwd = 2)
  graphics::points(age, q[2], pch = 16, col = "#1f4e79", cex = 1.3)
  graphics::legend("topright", c("AstroGeo22 (Farhat et al. 2022)", "this estimate (median, 95%)"),
      col = c("grey35", "#1f4e79"), lwd = 2, pch = c(NA, 16), bty = "n", cex = 0.75)
  invisible(NULL)
}

# Geological time scale chart: one column per rank
cd_as_plot_gts <- function(tab, t0, t1, mark = NULL) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  ranks <- c("Eon", "Era", "Period", "Epoch", "Age")
  graphics::par(mar = c(3, 4.5, 2, 1), mgp = c(2.6, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.9)
  pad <- 0.01 * (t1 - t0)
  graphics::plot(NA, xlim = c(0, length(ranks)), ylim = c(t1 + pad, t0 - pad), xaxs = "i", yaxs = "i", xaxt = "n",
      xlab = "", ylab = "Age (Ma)")
  graphics::axis(1, at = seq_along(ranks) - 0.5, labels = c("Eon", "Era", "Period", "Epoch", "Stage / age"),
      tick = FALSE, cex.axis = 0.85)
  hpx <- graphics::par("pin")[2] / abs(diff(graphics::par("usr")[3:4]))
  for (i in seq_len(nrow(tab))) {
    col_i <- match(tab$type[i], ranks)
    if (is.na(col_i)) {
      next
    }
    a <- max(tab$top_ma[i], t0)
    b <- min(tab$base_ma[i], t1)
    if (b <= a) {
      next
    }
    graphics::rect(col_i - 1, a, col_i, b, col = tab$colour[i], border = "grey30", lwd = 0.6)
    h_in <- (b - a) * hpx
    if (h_in > 0.11) {
      lab <- gsub("_", " ", tab$name[i])
      cx <- min(0.8, h_in / 0.2)
      graphics::text(col_i - 0.5, (a + b) / 2, lab, cex = cx, srt = if (h_in > 0.9 && col_i <= 2) { 90 } else { 0 })
    }
  }
  if (!is.null(mark)) {
    graphics::abline(h = mark, col = "#c0392b", lwd = 2, lty = 2)
  }
  graphics::box()
  invisible(NULL)
}

# Tools ####

cd_tools_astronomy <- function() {
  menu <- "Astronomy"
  g1 <- "Astronomical periods in deep time"
  g2 <- "Precession constant and CTest"
  g3 <- "Earth-Moon system"
  g4 <- "Geological time scale"
  em_choices <- c("Angular-momentum conservation" = "angular_momentum", "AstroGeo22 curve (Farhat et al. 2022)" = "astrogeo22")
  orb_choices <- c("Uniform within 2 sigma (as in the paper)" = "U", "Normal" = "N")
  s3_choices <- c("As in the published code (s3 fixed)" = "published", "Scaled like g2-g5" = "scaled")
  k_src <- c("AstroGeo22 model at this age" = "model", "My own value" = "own")
  gts <- WaverideR::GTS_info
  gts_ok <- !(gts$name %in% gts$name[duplicated(gts$name)]) & gts$type %in% c("Eon", "Era", "Period", "Epoch", "Age")
  gts_choices <- gts$name[gts_ok]
  names(gts_choices) <- sprintf("%s (%s, %s-%s Ma)", gsub("_", " ", gts$name[gts_ok]), gts$type[gts_ok],
      cd_as_fmt_vec(gts$top[gts_ok]), cd_as_fmt_vec(gts$bot[gts_ok]))
  list(
    cd_as_tool_periods(menu, g1, k_src),
    cd_as_tool_periods_time(menu, g1),
    cd_as_tool_term_search(menu, g1, k_src),
    cd_as_tool_bundling(menu, g1, k_src),
    cd_as_tool_chaos(menu, g1, k_src),
    cd_as_tool_ctest(menu, g2, em_choices, orb_choices, s3_choices),
    cd_as_tool_ctest_tuned(menu, g2, em_choices, orb_choices, s3_choices),
    cd_as_tool_k(menu, g2, em_choices, orb_choices, s3_choices),
    cd_as_tool_k_combine(menu, g2),
    cd_as_tool_test_tuned(menu, g2),
    cd_as_tool_test_check(menu, g2),
    cd_as_tool_compare(menu, g2),
    cd_as_tool_earthmoon(menu, g3, em_choices),
    cd_as_tool_em_relations(menu, g3),
    cd_as_tool_gts(menu, g4, gts_choices)
  )
}

cd_as_fmt_vec <- function(x) {
  out <- character(length(x))
  for (i in seq_along(x)) {
    out[i] <- cd_fmt(x[i])
  }
  out
}

# Astronomical periods at an age ####

cd_as_tool_periods <- function(menu, group, k_src) {
  cd_tool("as_periods", menu, "Astronomical periods at an age", inputs = 0L, group = group, icon = "wave-square",
      info = paste("Eccentricity, obliquity and precession periods at a given age, with the precession constant k from",
          "the AstroGeo22 tidal model (Farhat et al. 2022) or your own k. Use it to know which periods to expect in a",
          "deep-time record."),
      uses = paste("WaverideR::calculate_orbit9, WaverideR::orbit9, WaverideR::astro_terms,",
          "WaverideR::astro_periods_tracking, WaverideR::orbital_periods, WaverideR::astrogeo22"),
      params = list(
        cd_sel("set", "Terms", c("Nine main periods (Acycle ORBIT9)" = "nine", "All named terms with aliases" = "all",
            "Main periods with weights for tracking" = "tracking", "Every gi-gj, k+si and k+gi term" = "orbital")),
        cd_num("age", "Age (Ma)", 400, min = 0, max = 4429),
        cd_sel("ksrc", "Precession constant k", k_src),
        cd_num("k", "Precession constant k (kyr)", 21, min = 1, show_if = list(param = "ksrc", values = "own")),
        cd_chk("inc_ecc", "Include eccentricity", TRUE, show_if = list(param = "set", values = "tracking")),
        cd_chk("inc_obl", "Include obliquity", TRUE, show_if = list(param = "set", values = "tracking")),
        cd_chk("inc_prec", "Include precession", TRUE, show_if = list(param = "set", values = "tracking"))),
      compute = function(d, p, d2, file, file2) {
        k_yr <- cd_as_k_yr(p)
        own <- identical(p$ksrc, "own")
        if (p$set == "nine") {
          if (own) {
            r <- cd_as_run("orbit9", list(k_kyr = p$k))
          } else {
            r <- cd_as_run("calculate_orbit9", list(age_ma = p$age))
          }
          tab <- r$value[, c("term", "group", "freq_arcsec_yr", "period_kyr")]
          names(tab)[2] <- "family"
          pl <- data.frame(label = tab$term, family = tab$family, period = tab$period_kyr, weight = 1)
        } else if (p$set == "all") {
          r <- cd_as_run("astro_terms", list(age_ma = p$age, k_yr = k_yr))
          tab <- r$value[, c("term", "alias", "family", "description", "order", "freq_arcsec_yr", "period_kyr", "carries_k")]
          lab <- ifelse(nzchar(tab$alias), paste0(tab$term, " (", tab$alias, ")"), tab$term)
          pl <- data.frame(label = lab, family = tab$family, period = tab$period_kyr, weight = 1 / tab$order)
        } else if (p$set == "tracking") {
          inc <- c("eccentricity", "obliquity", "precession")[c(isTRUE(p$inc_ecc), isTRUE(p$inc_obl), isTRUE(p$inc_prec))]
          if (!length(inc)) {
            stop("include at least one family of cycles", call. = FALSE)
          }
          r <- cd_as_run("astro_periods_tracking", list(age_ma = p$age, k_yr = k_yr, include = inc))
          tab <- r$value
          names(tab)[names(tab) == "period"] <- "period_kyr"
          pl <- data.frame(label = tab$term, family = tab$family, period = tab$period_kyr, weight = tab$weight)
        } else {
          kk <- if (own) { p$k * 1000 } else { WaverideR::astrogeo22(p$age)$p_kyr[1] * 1000 }
          r <- cd_as_run("orbital_periods", list(k_yr = signif(kk, 8), terms = "all"))
          tab <- r$value
          names(tab)[2] <- "family"
          pl <- data.frame(label = tab$term, family = tab$family, period = tab$period_kyr, weight = 1)
        }
        tab <- as.data.frame(tab)
        rownames(tab) <- NULL
        kshow <- if (own) { p$k } else { WaverideR::astrogeo22(p$age)$p_kyr[1] }
        nm <- sprintf("AstroPeriods_%s_%sMa.txt", p$set, cd_fmt(p$age))
        o <- list(cd_output(nm, tab, header = TRUE, var = "res"))
        code <- r$code
        if (p$set == "nine" && !own) {
          code <- c(code, "res <- res[, c(\"term\", \"group\", \"freq_arcsec_yr\", \"period_kyr\")]")
        }
        txt <- c(sprintf("Age %s Ma; precession constant k = %s kyr (%s)", cd_fmt(p$age), cd_fmt(signif(kshow, 5)),
            if (own) { "your value" } else { "AstroGeo22" }),
            if (p$set == "nine" && !own) { "calculate_orbit9 rounds the age to the nearest whole Ma, as Acycle does." } else { character() },
            cd_as_table_text(tab))
        list(result = list(table = tab, plot = pl, age = p$age, k = kshow, set = p$set), outputs = o,
            code = c(code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        cd_as_plot_sticks(res$plot$period, res$plot$weight, res$plot$label, res$plot$family,
            main = sprintf("%s Ma, k = %s kyr", cd_fmt(res$age), cd_fmt(signif(res$k, 4))),
            ylab = if (res$set == "tracking") { "Weight for tracking" } else if (res$set == "all") { "1 / order of the term" } else { "" })
      })
}

# Astronomical periods through time ####

cd_as_tool_periods_time <- function(menu, group) {
  cd_tool("as_periods_time", menu, "Astronomical periods through time", inputs = 0L, group = group,
      icon = "chart-line",
      info = paste("The nine main astronomical periods over an age range, with the precession constant from AstroGeo22",
          "or the La2004 extrapolation. Shows how the obliquity and precession periods shorten into the past."),
      uses = "WaverideR::astrogeo22, WaverideR::orbit9, WaverideR::orbit9_at_age, WaverideR::la2004_extrap",
      heavy = function(p) {
        identical(p$src, "acycle") && (p$tmax - p$tmin) / p$step > 60
      },
      params = list(
        cd_num("tmin", "Youngest age (Ma)", 0, min = 0, max = 4429),
        cd_num("tmax", "Oldest age (Ma)", 1000, min = 0, max = 4429),
        cd_num("step", "Age step (Ma)", 10, min = 0.1),
        cd_sel("src", "Precession constant from", c("AstroGeo22 (Farhat et al. 2022), interpolated" = "ag",
            "AstroGeo22, nearest 1-Myr row as in Acycle" = "acycle", "La2004 polynomial extrapolation" = "la04"))),
      compute = function(d, p, d2, file, file2) {
        if (p$tmax <= p$tmin || p$step <= 0) {
          stop("the oldest age must be larger than the youngest age, and the step positive", call. = FALSE)
        }
        ages <- seq(p$tmin, p$tmax, by = p$step)
        if (length(ages) > 5000) {
          stop("more than 5000 ages: use a larger step", call. = FALSE)
        }
        if (p$src == "la04") {
          kk <- WaverideR::la2004_extrap(ages)$p_kyr
          code <- sprintf("ages <- seq(%s, %s, by = %s)\nk <- WaverideR::la2004_extrap(ages)$p_kyr", cd_fmt(p$tmin),
              cd_fmt(p$tmax), cd_fmt(p$step))
        } else {
          kk <- WaverideR::astrogeo22(ages, interpolate = p$src == "ag")$p_kyr
          code <- sprintf("ages <- seq(%s, %s, by = %s)\nk <- WaverideR::astrogeo22(ages, interpolate = %s)$p_kyr",
              cd_fmt(p$tmin), cd_fmt(p$tmax), cd_fmt(p$step), p$src == "ag")
        }
        terms <- WaverideR::orbit9(25.7)$term
        per <- matrix(NA_real_, length(ages), length(terms))
        for (i in seq_along(ages)) {
          if (!is.finite(kk[i])) {
            next
          }
          if (p$src == "acycle") {
            per[i, ] <- WaverideR::orbit9_at_age(ages[i])
          } else {
            per[i, ] <- WaverideR::orbit9(kk[i])$period_kyr
          }
        }
        colnames(per) <- terms
        tab <- data.frame(age_ma = ages, k_kyr = kk, per, check.names = FALSE)
        tab <- tab[is.finite(tab$k_kyr), , drop = FALSE]
        if (!nrow(tab)) {
          stop("no precession constant available in this age range", call. = FALSE)
        }
        nm <- sprintf("AstroPeriods_%s_%s-%sMa.txt", p$src, cd_fmt(p$tmin), cd_fmt(p$tmax))
        o <- list(cd_output(nm, tab, header = TRUE))
        loop <- if (p$src == "acycle") { "  per[i, ] <- WaverideR::orbit9_at_age(ages[i])" } else {
          "  per[i, ] <- WaverideR::orbit9(k[i])$period_kyr" }
        code <- c(code, "per <- matrix(NA_real_, length(ages), 9)", "for (i in seq_along(ages)) {",
            "  if (!is.finite(k[i])) {", "    next", "  }", loop, "}",
            "colnames(per) <- WaverideR::orbit9(25.7)$term",
            "res <- data.frame(age_ma = ages, k_kyr = k, per, check.names = FALSE)", "res <- res[is.finite(res$k_kyr), ]",
            cd_as_code_write(o))
        txt <- c(sprintf("%d ages from %s to %s Ma; k from %s kyr to %s kyr", nrow(tab), cd_fmt(min(tab$age_ma)),
            cd_fmt(max(tab$age_ma)), cd_fmt(signif(tab$k_kyr[1], 4)), cd_fmt(signif(tab$k_kyr[nrow(tab)], 4))),
            "The eccentricity periods use the La2004 secular frequencies and do not change with age here.",
            cd_as_table_text(tab, max_rows = 25))
        list(result = list(table = tab, src = p$src), outputs = o, code = code, text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        tab <- res$table
        graphics::layout(matrix(1:2, nrow = 1), widths = c(2, 1))
        cd_par()
        kt <- c("k+s3", "k+g5", "k+g2", "k+g4")
        cols <- c("#2e8b57", "#c0392b", "#d35400", "#8e44ad")
        yr <- range(c(as.matrix(tab[, kt]), tab$k_kyr), na.rm = TRUE)
        graphics::plot(NA, xlim = range(tab$age_ma), ylim = yr, xlab = "Age (Ma)", ylab = "Period (kyr)",
            main = "Obliquity and precession")
        graphics::grid(col = "grey90", lty = 1)
        for (j in seq_along(kt)) {
          graphics::lines(tab$age_ma, tab[[kt[j]]], col = cols[j], lwd = 2)
        }
        graphics::lines(tab$age_ma, tab$k_kyr, col = "grey30", lwd = 1.5, lty = 2)
        graphics::legend("topright", c(kt, "k (axial precession)"), col = c(cols, "grey30"), lwd = 2,
            lty = c(1, 1, 1, 1, 2), bty = "n", cex = 0.8)
        cd_par()
        et <- c("g2-g5", "g3-g2", "g4-g2", "g3-g5", "g4-g5")
        ep <- as.numeric(tab[1, et])
        graphics::plot(rep(1, 5), ep, log = "y", pch = 15, col = "#1f4e79", xaxt = "n", xlab = "", xlim = c(0.5, 2),
            ylab = "Period (kyr)", main = "Eccentricity (constant)")
        graphics::text(rep(1.1, 5), ep, sprintf("%s  %s kyr", et, cd_as_fmt_vec(signif(ep, 4))), adj = 0, cex = 0.75)
      })
}

# Identify a measured period ####

cd_as_tool_term_search <- function(menu, group, k_src) {
  cd_tool("as_term_search", menu, "Identify a measured period", inputs = 0L, group = group, icon = "magnifying-glass",
      info = paste("Which astronomical term, harmonic or combination tone could an unidentified spectral peak be? Lists",
          "the terms within a relative tolerance of each measured period."),
      uses = "WaverideR::astro_term_search",
      params = list(
        cd_txt("periods", "Measured period(s) (kyr)", "35", numeric = TRUE),
        cd_num("age", "Age (Ma)", 400, min = 0, max = 4429),
        cd_num("tol", "Relative tolerance", 0.03, min = 0.001, max = 0.5, step = 0.01),
        cd_sel("max_order", "Highest order", c("Single terms" = "1", "Also harmonics, sums and differences" = "2"),
            value = "2"),
        cd_sel("ksrc", "Precession constant k", k_src),
        cd_num("k", "Precession constant k (kyr)", 21, min = 1, show_if = list(param = "ksrc", values = "own"))),
      compute = function(d, p, d2, file, file2) {
        per <- p$periods[is.finite(p$periods) & p$periods > 0]
        if (!length(per)) {
          stop("give at least one measured period", call. = FALSE)
        }
        tabs <- list()
        code <- character()
        for (i in seq_along(per)) {
          r <- cd_as_run("astro_term_search", list(period_kyr = per[i], age_ma = p$age, tol = p$tol,
              max_order = as.numeric(p$max_order), k_yr = cd_as_k_yr(p)), var = sprintf("r%d", i))
          code <- c(code, r$code)
          if (nrow(r$value)) {
            tabs[[length(tabs) + 1]] <- data.frame(measured_kyr = per[i], r$value, check.names = FALSE)
          }
        }
        tab <- if (length(tabs)) { do.call(rbind, tabs) } else {
          data.frame(measured_kyr = numeric(), term = character(), period_kyr = numeric(), offset_pct = numeric(),
              carries_k = logical(), k_mult = numeric(), order = numeric())
        }
        rownames(tab) <- NULL
        nm <- sprintf("AstroTermSearch_%sMa.txt", cd_fmt(p$age))
        o <- list(cd_output(nm, tab, header = TRUE))
        rv <- sprintf("r%d", seq_along(per))
        code <- c(code, sprintf("res <- rbind(%s)", paste(sprintf("data.frame(measured_kyr = %s, %s)",
            cd_as_fmt_vec(per), rv), collapse = ", ")), cd_as_code_write(o))
        txt <- c(sprintf("%d candidate terms within %s%% (k = %s kyr)", nrow(tab), cd_fmt(100 * p$tol),
            cd_fmt(signif(cd_as_k(p, p$age), 4))), cd_as_table_text(tab))
        list(result = list(table = tab, per = per, tol = p$tol), outputs = o, code = code, text = txt)
      },
      plot = function(res, p) {
        tab <- res$table
        if (!nrow(tab)) {
          cd_message_plot("No term matches within the tolerance")
          return(invisible(NULL))
        }
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        n <- nrow(tab)
        cd_par(mar = c(4, max(8, 0.55 * max(nchar(tab$term)) + 4), 1.6, 1))
        lab <- sprintf("%s (%s)", tab$term, cd_as_fmt_vec(tab$measured_kyr))
        cols <- ifelse(tab$carries_k, "#c0392b", "#1f4e79")
        lim <- 100 * res$tol * 1.1
        graphics::plot(tab$offset_pct, seq_len(n), xlim = c(-lim, lim), ylim = c(n + 0.5, 0.5), yaxt = "n", pch = 16,
            col = cols, xlab = "Offset from the measured period (%)", ylab = "", cex = 1.2)
        graphics::abline(v = 0, col = "grey40", lty = 2)
        graphics::abline(h = seq_len(n), col = "grey92")
        graphics::axis(2, at = seq_len(n), labels = lab, cex.axis = 0.7, las = 1)
        graphics::legend("topright", c("carries k", "no k"), col = c("#c0392b", "#1f4e79"), pch = 16, bty = "n",
            cex = 0.8)
      })
}

# Bundling ratios ####

cd_as_tool_bundling <- function(menu, group, k_src) {
  cd_tool("as_bundling", menu, "Bundling ratios", inputs = 0L, group = group, icon = "layer-group",
      info = paste("Predicted number of precession (or obliquity) cycles per eccentricity cycle at a given age. Compare",
          "with the bundling counted in the record (then use the Precession constant tool, method 'Bundling')."),
      uses = "WaverideR::bundling_ratios",
      params = list(
        cd_num("age", "Age (Ma)", 400, min = 0, max = 4429),
        cd_sel("ksrc", "Precession constant k", k_src),
        cd_num("k", "Precession constant k (kyr)", 21, min = 1, show_if = list(param = "ksrc", values = "own")),
        cd_txt("ecc_terms", "Eccentricity terms (rows)", "g2-g5, g3-g2, g4-g2, g3-g5, g4-g5"),
        cd_txt("prec_terms", "Precession / obliquity terms (columns)", "k+g5, k+g2, k+g3, k+g4, k+s3")),
      compute = function(d, p, d2, file, file2) {
        r <- cd_as_run("bundling_ratios", list(age_ma = p$age, k_yr = cd_as_k_yr(p), ecc_terms = cd_as_terms(p$ecc_terms),
            prec_terms = cd_as_terms(p$prec_terms)))
        m <- r$value
        tab <- cd_as_rn(unclass(m)[seq_len(nrow(m)), , drop = FALSE], "eccentricity")
        nm <- sprintf("BundlingRatios_%sMa.txt", cd_fmt(p$age))
        o <- list(cd_output(nm, tab, header = TRUE, var = "tab"))
        code <- c(r$code, "tab <- data.frame(eccentricity = rownames(res), unclass(res), check.names = FALSE)",
            cd_as_code_write(o))
        txt <- c(sprintf("Precession cycles per eccentricity cycle at %s Ma (k = %s kyr)", cd_fmt(p$age),
            cd_fmt(signif(attr(m, "k_kyr"), 4))), cd_as_table_text(tab, digits = 4))
        list(result = list(m = unclass(m)[seq_len(nrow(m)), , drop = FALSE], age = p$age), outputs = o, code = code,
            text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        m <- res$m
        nr <- nrow(m)
        nc <- ncol(m)
        graphics::par(mar = c(6, 9, 2, 1), las = 1, cex.axis = 0.8)
        z <- t(m[nr:1, , drop = FALSE])
        graphics::image(seq_len(nc), seq_len(nr), z, col = cd_viridis(100), xaxt = "n", yaxt = "n", xlab = "", ylab = "",
            main = sprintf("Cycles per bundle, %s Ma", cd_fmt(res$age)))
        graphics::axis(1, at = seq_len(nc), labels = colnames(m), las = 2)
        graphics::axis(2, at = seq_len(nr), labels = rev(rownames(m)))
        zr <- range(z, finite = TRUE)
        for (i in seq_len(nc)) {
          for (j in seq_len(nr)) {
            light <- (z[i, j] - zr[1]) / max(diff(zr), 1e-9) > 0.6
            graphics::text(i, j, sprintf("%.2f", z[i, j]), col = if (light) { "black" } else { "white" }, cex = 0.85)
          }
        }
      })
}

# Chaotic resonance ####

cd_as_tool_chaos <- function(menu, group, k_src) {
  cd_tool("as_chaos", menu, "Chaotic resonance and sensitivity", inputs = 0L, group = group, icon = "shuffle",
      info = paste("Explore chaos in the inner solar system: shift g4-g3 / s4-s3 and see which cycles move, the",
          "sensitivity of every term to each fundamental frequency, or which resonance state measured long",
          "modulation periods imply."),
      uses = "WaverideR::chaos_scenario, WaverideR::term_sensitivity, WaverideR::resonance_state",
      params = list(
        cd_sel("method", "Analysis", c("Scenario: shift g4-g3 and s4-s3" = "scenario",
            "Sensitivity of every term to each fundamental" = "sensitivity",
            "Resonance state from measured modulation periods" = "resonance")),
        cd_num("g4g3", "g4-g3 period in the scenario (kyr, 0 = La2004)", 1200, min = 0,
            show_if = list(param = "method", values = "scenario")),
        cd_num("s4s3", "s4-s3 period in the scenario (kyr, 0 = La2004)", 0, min = 0,
            show_if = list(param = "method", values = "scenario")),
        cd_sel("hold", "How g3 and g4 move", c("Both, about their mean" = "mean", "Keep g3, move g4" = "g3",
            "Keep g4, move g3" = "g4"), show_if = list(param = "method", values = "scenario")),
        cd_num("delta", "Fractional change of each fundamental", 0.01, step = 0.005,
            show_if = list(param = "method", values = "sensitivity")),
        cd_txt("fundamentals", "Fundamentals", "g1, g2, g3, g4, g5, s1, s3, s4, s6, k",
            show_if = list(param = "method", values = "sensitivity")),
        cd_num("ecc_kyr", "Measured eccentricity modulation, g4-g3 (kyr)", 2400, min = 1,
            show_if = list(param = "method", values = "resonance")),
        cd_num("obl_kyr", "Measured obliquity modulation, s4-s3 (kyr)", 1200, min = 1,
            show_if = list(param = "method", values = "resonance")),
        cd_num("max_ratio", "Largest p and q of the ratio p:q", 4, min = 1, step = 1,
            show_if = list(param = "method", values = "resonance")),
        cd_num("age", "Age (Ma)", 100, min = 0, max = 4429, show_if = list(param = "method", values = c("scenario", "sensitivity"))),
        cd_sel("ksrc", "Precession constant k", k_src, show_if = list(param = "method", values = c("scenario", "sensitivity"))),
        cd_num("k", "Precession constant k (kyr)", 24.5, min = 1, show_if = list(param = "ksrc", values = "own"))),
      compute = function(d, p, d2, file, file2) {
        if (p$method == "scenario") {
          r <- cd_as_run("chaos_scenario", list(g4_g3_kyr = cd_as_pos(p$g4g3), s4_s3_kyr = cd_as_pos(p$s4s3), hold = p$hold,
              age_ma = p$age, k_yr = cd_as_k_yr(p)))
          tab <- as.data.frame(r$value)
          rownames(tab) <- NULL
          nm <- sprintf("ChaosScenario_g4g3_%s_s4s3_%s.txt", cd_fmt(p$g4g3), cd_fmt(p$s4s3))
          moved <- tab[abs(tab$change_pct) > 1e-6, , drop = FALSE]
          txt <- c(sprintf("%d of %d terms change in this scenario", nrow(moved), nrow(tab)), cd_as_table_text(tab))
          code <- r$code
          o <- list(cd_output(nm, tab, header = TRUE))
        } else if (p$method == "sensitivity") {
          r <- cd_as_run("term_sensitivity", list(age_ma = p$age, delta = p$delta, k_yr = cd_as_k_yr(p),
              fundamentals = cd_as_terms(p$fundamentals)))
          m <- unclass(r$value)
          tab <- cd_as_rn(m[seq_len(nrow(m)), , drop = FALSE], "term")
          nm <- sprintf("TermSensitivity_%sMa.txt", cd_fmt(p$age))
          o <- list(cd_output(nm, tab, header = TRUE, var = "tab"))
          code <- c(r$code, "tab <- data.frame(term = rownames(res), unclass(res), check.names = FALSE)")
          txt <- c(sprintf("Percent change of each period for a %s%% change of each fundamental frequency",
              cd_fmt(100 * p$delta)), cd_as_table_text(tab, digits = 3))
        } else {
          r <- cd_as_run("resonance_state", list(ecc_kyr = p$ecc_kyr, obl_kyr = p$obl_kyr, max_ratio = p$max_ratio))
          v <- r$value
          tab <- data.frame(quantity = names(v), value = character(length(v)), stringsAsFactors = FALSE)
          for (i in seq_along(v)) {
            tab$value[i] <- if (is.numeric(v[[i]])) { cd_fmt(signif(v[[i]], 6)) } else { as.character(v[[i]]) }
          }
          nm <- sprintf("ResonanceState_%s_%s.txt", cd_fmt(p$ecc_kyr), cd_fmt(p$obl_kyr))
          o <- list(cd_output(nm, tab, header = TRUE, var = "tab"))
          code <- c(r$code, "tab <- data.frame(quantity = names(res), value = unlist(res))")
          txt <- c(sprintf("Frequency ratio s4-s3 : g4-g3 = %s, nearest simple ratio %s (modern: %s)", cd_fmt(signif(v$ratio, 4)),
              v$nearest, v$modern_state), cd_as_table_text(tab))
          r$value <- v
        }
        list(result = list(method = p$method, table = tab, raw = if (p$method == "resonance") { r$value } else { NULL },
            age = p$age), outputs = o, code = c(code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        tab <- res$table
        if (res$method == "scenario") {
          lab <- ifelse(nzchar(tab$alias), paste0(tab$term, " (", tab$alias, ")"), tab$term)
          n <- nrow(tab)
          graphics::par(mar = c(4, 11, 2, 1), las = 1, cex.axis = 0.7, mgp = c(2.4, 0.6, 0))
          cols <- ifelse(tab$change_pct > 0, "#c0392b", "#1f4e79")
          graphics::barplot(rev(tab$change_pct), horiz = TRUE, names.arg = rev(lab), col = rev(cols), border = NA,
              xlab = "Change of the period (%)", main = "Scenario against La2004")
          graphics::abline(v = 0, col = "grey40")
        } else if (res$method == "sensitivity") {
          m <- as.matrix(tab[, -1, drop = FALSE])
          rownames(m) <- tab$term
          nr <- nrow(m)
          graphics::par(mar = c(4, 14, 2, 1), las = 1, cex.axis = 0.65)
          z <- t(m[nr:1, , drop = FALSE])
          lim <- max(abs(z), na.rm = TRUE)
          graphics::image(seq_len(ncol(m)), seq_len(nr), z, zlim = c(-lim, lim), col = grDevices::hcl.colors(101, "Blue-Red 3"),
              xaxt = "n", yaxt = "n", xlab = "Fundamental frequency changed", ylab = "",
              main = "Percent change of each period")
          graphics::axis(1, at = seq_len(ncol(m)), labels = colnames(m), las = 1)
          graphics::axis(2, at = seq_len(nr), labels = rev(rownames(m)))
        } else {
          v <- res$raw
          cd_par()
          mr <- max(1, round(if (is.null(p$max_ratio)) { 4 } else { p$max_ratio }))
          rr <- numeric()
          rl <- character()
          for (a in 1:mr) {
            for (b in 1:mr) {
              rr <- c(rr, a / b)
              rl <- c(rl, sprintf("%d:%d", a, b))
            }
          }
          keep <- !duplicated(round(rr, 8))
          rr <- rr[keep]
          rl <- rl[keep]
          xr <- range(c(0, rr, v$ratio, v$modern_ratio))
          graphics::plot(NA, xlim = xr, ylim = c(0, 1), yaxt = "n", ylab = "", xlab = "f(s4-s3) / f(g4-g3)",
              main = sprintf("Measured ratio %s, nearest %s", cd_fmt(signif(v$ratio, 3)), v$nearest))
          graphics::segments(rr, 0.1, rr, 0.5, col = "grey60")
          graphics::text(rr, 0.55, rl, cex = 0.7, col = "grey40", srt = 90, adj = 0)
          graphics::abline(v = v$modern_ratio, col = "#2e8b57", lwd = 2, lty = 2)
          graphics::abline(v = v$ratio, col = "#c0392b", lwd = 3)
          graphics::legend("topright", c("measured", "modern (La2004)"), col = c("#c0392b", "#2e8b57"), lwd = 2,
              lty = c(1, 2), bty = "n", cex = 0.8)
        }
      })
}

# CTest from depth-domain spectral peaks ####

cd_as_tool_ctest <- function(menu, group, em_choices, orb_choices, s3_choices) {
  depth <- list(param = "method", values = c("ctest", "ctest_loop"))
  cd_tool("as_ctest", menu, "CTest from depth-domain peaks", inputs = 0L, group = group, icon = "bullseye",
      heavy = TRUE,
      info = paste("CTest (Zeeden et al. 2023): give the wavelength ranges (m) of the eccentricity, obliquity and",
          "precession cycles you identified in a depth-domain spectrum and the age; a Monte Carlo consistency test",
          "returns the sedimentation rate, the precession constant k and the Earth-Moon distance and length of day."),
      uses = paste("WaverideR::astro_from_peaks, WaverideR::ctest_input, WaverideR::ctest, WaverideR::ctest_earthmoon,",
          "WaverideR::astro_mc"),
      params = list(
        cd_sel("method", "Method", c("CTest (Zeeden et al. 2023)" = "ctest",
            "CTest in the published draw order (slow, reproduces the paper's samples)" = "ctest_loop",
            "Flexible Monte Carlo with a sedimentation-rate range" = "mc_depth")),
        cd_txt("age", "Age of the record (Ma): one value or min, max", "380, 390", numeric = TRUE),
        cd_txt("e405", "405-kyr eccentricity, wavelength min, max (m)", "7.9, 8.3", numeric = TRUE),
        cd_txt("e125", "~125-kyr eccentricity (m)", "2.4, 2.7", numeric = TRUE),
        cd_txt("e95", "~95-kyr eccentricity (m)", "1.85, 2.0", numeric = TRUE),
        cd_txt("obl", "Obliquity k+s3 (m, empty = not identified)", "0.57, 0.61", numeric = TRUE),
        cd_txt("p1", "Longest precession k+g5 (m)", "0.38, 0.40", numeric = TRUE),
        cd_txt("p2", "Middle precession k+g2 (m)", "0.36, 0.38", numeric = TRUE),
        cd_txt("p3", "Shortest precession k+g4 and k+g3 (m)", "0.31, 0.335", numeric = TRUE),
        cd_sel("mode", "Cycles used besides eccentricity", c("From the cycles given" = "auto",
            "Precession and obliquity" = "PT", "Obliquity only" = "T", "Precession only" = "P"), show_if = depth),
        cd_txt("sr", "Sedimentation rate range (m/kyr)", "0.01, 0.04", numeric = TRUE,
            show_if = list(param = "method", values = "mc_depth")),
        cd_num("nsim", "Accepted Monte Carlo samples", 1000, min = 50, step = 100),
        cd_txt("ptest", "Range of k sampled (yr)", "10000, 28000", numeric = TRUE),
        cd_sel("sampleorb", "Sampling of g2-g5", orb_choices),
        cd_sel("s3", "Uncertainty of s3", s3_choices),
        cd_sel("em", "Earth-Moon conversion", em_choices),
        cd_num("seed", "Random seed", 1, step = 1)),
      compute = function(d, p, d2, file, file2) {
        age <- cd_as_age(p$age, 385)
        agev <- if (length(age) == 1) { age + c(-5, 5) } else { age }
        e405 <- cd_as_rng(p$e405)
        e125 <- cd_as_rng(p$e125)
        e95 <- cd_as_rng(p$e95)
        obl <- cd_as_rng(p$obl)
        p1 <- cd_as_rng(p$p1)
        p2 <- cd_as_rng(p$p2)
        p3 <- cd_as_rng(p$p3)
        ptest <- cd_as_rng(p$ptest)
        if (is.null(ptest)) {
          ptest <- c(10000, 28000)
        }
        np <- sum(!is.null(p1), !is.null(p2), !is.null(p3))
        code <- character()
        txt <- character()
        if (p$method %in% c("ctest", "ctest_loop")) {
          if (is.null(e405) || is.null(e125) || is.null(e95)) {
            stop("CTest needs the 405-kyr, ~125-kyr and ~95-kyr eccentricity wavelength ranges", call. = FALSE)
          }
          if (np %in% c(1, 2)) {
            stop("give all three precession ranges or none", call. = FALSE)
          }
          if (is.null(obl) && np == 0) {
            stop("give the obliquity range, the three precession ranges, or both", call. = FALSE)
          }
        }
        if (p$method == "ctest") {
          r <- cd_as_run("astro_from_peaks", list(age = age, e405 = e405, e125 = e125, e95 = e95, obl = obl,
              prec_long = p1, prec_mid = p2, prec_short = p3, nsim = p$nsim, ptest = ptest, mode = p$mode,
              sampleorb = p$sampleorb, method = p$em, s3_uncertainty = p$s3, seed = p$seed, verbose = FALSE))
          v <- r$value
          ct <- v$ctest
          em <- v$em
          code <- c(r$code, "print(res)", "ct <- res$ctest", "em <- res$em")
          extra <- list(cd_output("CTest_depth_frequencies.txt", v$frequencies, header = TRUE, var = "res$frequencies"),
              cd_output("CTest_depth_periods.txt", v$periods, header = TRUE, var = "res$periods"))
          txt <- c(utils::capture.output(print(v)))
        } else if (p$method == "ctest_loop") {
          mode <- p$mode
          if (mode == "auto") {
            mode <- if (!is.null(obl) && np == 3) { "PT" } else if (!is.null(obl)) { "T" } else { "P" }
          }
          na2 <- c(NA_real_, NA_real_)
          r1 <- cd_as_run("ctest_input", list(age = agev, e405 = e405, e125 = e125, e95 = e95,
              obl = if (is.null(obl)) { na2 } else { obl }, p1 = if (is.null(p1)) { na2 } else { p1 },
              p2 = if (is.null(p2)) { na2 } else { p2 }, p3 = if (is.null(p3)) { na2 } else { p3 }), var = "inp")
          ct <- WaverideR::ctest(r1$value, nsim = p$nsim, mode = mode, ptest = ptest, sampleorb = p$sampleorb, seed = p$seed,
              verbose = FALSE, engine = "loop", s3_uncertainty = p$s3)
          em <- WaverideR::ctest_earthmoon(ct, method = p$em)
          code <- c(r1$code, sprintf(paste0("ct <- WaverideR::ctest(inp, nsim = %s, mode = %s, ptest = %s, sampleorb = %s,",
              " seed = %s, engine = \"loop\", s3_uncertainty = %s)"), cd_fmt(p$nsim), cd_dep(mode), cd_as_val(ptest),
              cd_dep(p$sampleorb), cd_fmt(p$seed), cd_dep(p$s3)),
              sprintf("em <- WaverideR::ctest_earthmoon(ct, method = %s)", cd_dep(p$em)), "print(ct)")
          extra <- list()
          txt <- utils::capture.output(print(ct))
        } else {
          sr <- cd_as_rng(p$sr)
          if (is.null(sr) || sr[1] <= 0) {
            stop("give the sedimentation rate range in m/kyr", call. = FALSE)
          }
          cons <- list()
          nms <- c("e405", "e125", "e95", "k+s3", "k+g5", "k+g2", "k+g4|k+g3")
          vals <- list(e405, e125, e95, obl, p1, p2, p3)
          for (i in seq_along(nms)) {
            if (!is.null(vals[[i]])) {
              cons[[nms[i]]] <- vals[[i]]
            }
          }
          if (length(cons) < 2) {
            stop("give at least two cycles", call. = FALSE)
          }
          inv <- cd_as_mc_depth_inverted()
          sc <- if (inv) { signif(1 / rev(sr), 10) } else { sr }
          r <- cd_as_run("astro_mc", list(constraints = cons, age = age, domain = "depth", scale_range = sc,
              nsim = p$nsim, ptest = ptest, sampleorb = p$sampleorb, s3_uncertainty = p$s3, seed = p$seed,
              verbose = FALSE), var = "ct")
          ct <- r$value
          if (inv) {
            ct$samples$SR <- 1 / ct$samples$scale
          }
          em <- WaverideR::ctest_earthmoon(ct, method = p$em)
          code <- c(if (inv) { "# WaverideR 0.6.0 reads scale_range as kyr/m in the depth domain: the range is 1 / sedimentation rate" } else {
            character() }, r$code, if (inv) { "ct$samples$SR <- 1 / ct$samples$scale" } else { character() },
              sprintf("em <- WaverideR::ctest_earthmoon(ct, method = %s)", cd_dep(p$em)), "print(ct)")
          extra <- list()
          txt <- utils::capture.output(print(ct))
        }
        s <- ct$samples
        summ <- as.data.frame(ct$summary)
        if (p$method == "mc_depth" && cd_as_mc_depth_inverted()) {
          q <- stats::quantile(s$SR, c(0.025, 0.975))
          summ[1, 2:5] <- c(q[1], q[2], mean(s$SR), stats::sd(s$SR))
          summ[1, 1] <- "sedimentation rate (m/kyr)"
        }
        emt <- cd_as_rn(em$summary)
        k <- s$k
        samp <- data.frame(sample = seq_along(k), k_yr = k)
        o <- c(list(cd_output("CTest_depth_summary.txt", summ, header = TRUE, var = if (p$method == "ctest") { "res$ctest$summary" } else { "ct$summary" }),
            cd_output("CTest_depth_EarthMoon.txt", emt, header = TRUE,
                var = "data.frame(quantity = rownames(em$summary), em$summary, check.names = FALSE)"),
            cd_output("CTest_depth_k_samples.txt", samp, header = TRUE,
                var = "data.frame(sample = seq_along(ct$samples$k), k_yr = ct$samples$k)")), extra)
        if (p$method == "ctest") {
          o[[3]]$var <- "data.frame(sample = seq_along(res$ctest$samples$k), k_yr = res$ctest$samples$k)"
        }
        ref <- WaverideR::astrogeo22(mean(age))
        txt <- c(sprintf("k = %s +/- %s kyr (mean +/- sd), %d accepted samples, acceptance %s%%", cd_fmt(signif(mean(k) / 1000, 4)),
            cd_fmt(signif(stats::sd(k) / 1000, 2)), length(k), cd_fmt(signif(100 * ct$acceptance, 3))),
            sprintf("AstroGeo22 at %s Ma: k = %s kyr", cd_fmt(signif(mean(age), 4)), cd_fmt(signif(ref$p_kyr, 4))),
            "", txt, cd_as_table_text(emt, "Earth-Moon system"))
        keep_s <- s[, intersect(names(s), c("SR", "scale", "k", "obl_ks3", "prec_kg5", "prec_kg2", "prec_kg4", "prec_kg3")),
            drop = FALSE]
        post <- list(k_kyr = em$samples$k / 1000, a_RE = em$samples$a_RE, lod = em$samples$lod_solar_h,
            days = em$samples$days_per_year, extra = cd_as_extra_panels(keep_s, depth = TRUE), ref = ref, age = age)
        list(result = list(post = post, summary = summ, earthmoon = emt), outputs = o,
            code = c(code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        cd_as_plot_post(res$post)
      })
}

# CTest for a tuned record ####

cd_as_parse_extra <- function(s) {
  out <- list()
  s <- trimws(paste(s, collapse = ";"))
  if (!nzchar(s)) {
    return(out)
  }
  parts <- strsplit(s, ";")[[1]]
  for (pt in parts) {
    pt <- trimws(pt)
    if (!nzchar(pt)) {
      next
    }
    kv <- strsplit(pt, "=")[[1]]
    if (length(kv) != 2) {
      stop("other constraints must be written as term = min, max; term = min, max", call. = FALSE)
    }
    v <- cd_as_rng(cd_parse_nums(kv[2]))
    if (is.null(v)) {
      stop("other constraint '", pt, "' needs two values", call. = FALSE)
    }
    out[[trimws(kv[1])]] <- v
  }
  out
}

cd_as_tool_ctest_tuned <- function(menu, group, em_choices, orb_choices, s3_choices) {
  mc <- list(param = "method", values = "mc")
  cd_tool("as_ctest_tuned", menu, "CTest for a tuned record", inputs = 0L, group = group, icon = "crosshairs",
      heavy = TRUE,
      info = paste("Consistency test for a record already tuned to time: give the period ranges (kyr) of the cycles",
          "you resolved; the precession constant k, the secular frequencies and the Earth-Moon state follow, with",
          "the uncertainty of the tuning target propagated."),
      uses = "WaverideR::astro_from_periods, WaverideR::astro_mc, WaverideR::astro_ctest, WaverideR::astro_invert, WaverideR::plot_astro",
      params = list(
        cd_sel("method", "Method", c("Monte Carlo consistency test (astro_from_periods)" = "mc",
            "Zeeden et al. (2023) estimator (astro_ctest)" = "zeeden",
            "Joint inversion of the obliquity and precession terms only (astro_invert)" = "invert")),
        cd_txt("age", "Age of the record (Ma): one value or min, max", "385", numeric = TRUE),
        cd_txt("e405", "405-kyr eccentricity, period min, max (kyr)", "400, 410", numeric = TRUE),
        cd_txt("e125", "~125-kyr eccentricity (kyr, empty = not resolved)", "", numeric = TRUE),
        cd_txt("e95", "~95-kyr eccentricity (kyr)", "", numeric = TRUE),
        cd_txt("obl", "Obliquity k+s3 (kyr)", "29, 30.4", numeric = TRUE),
        cd_txt("kg5", "Precession k+g5 (kyr)", "19, 19.8", numeric = TRUE),
        cd_txt("kg2", "Precession k+g2 (kyr)", "", numeric = TRUE),
        cd_txt("kg4", "Precession k+g4 (kyr)", "", numeric = TRUE),
        cd_txt("kg3", "Precession k+g3 (kyr)", "", numeric = TRUE),
        cd_txt("pshort", "Short precession k+g4 and k+g3 together (kyr)", "", numeric = TRUE, show_if = mc),
        cd_txt("extra", "Other constraints (term = min, max; ...)", "", show_if = mc,
            help = "Any term of 'Astronomical periods at an age', e.g. g4-g3 = 2200, 2600; any:k+g5|k+g2 = 19, 20"),
        cd_num("anchor", "Period assumed for the tuning target (kyr, 0 = free stretch)", 405, min = 0,
            show_if = list(param = "method", values = c("mc", "zeeden"))),
        cd_txt("anchor_term", "Tuning target term", "e405", show_if = mc),
        cd_num("nsim", "Monte Carlo samples", 2000, min = 50, step = 100),
        cd_txt("krange", "Range of k sampled (kyr)", "10, 34", numeric = TRUE),
        cd_sel("sampleorb", "Sampling of g2-g5", orb_choices, show_if = mc),
        cd_sel("s3", "Uncertainty of s3", s3_choices, show_if = mc),
        cd_sel("em", "Earth-Moon conversion", em_choices, show_if = mc),
        cd_num("seed", "Random seed", 1, step = 1)),
      compute = function(d, p, d2, file, file2) {
        age <- cd_as_age(p$age, 385)
        win <- list()
        fields <- c(e405 = "e405", e125 = "e125", e95 = "e95", "k+s3" = "obl", "k+g5" = "kg5", "k+g2" = "kg2",
            "k+g4" = "kg4", "k+g3" = "kg3")
        for (nm in names(fields)) {
          v <- cd_as_rng(p[[fields[[nm]]]])
          if (!is.null(v)) {
            win[[nm]] <- v
          }
        }
        kr <- cd_as_rng(p$krange)
        if (is.null(kr)) {
          kr <- c(10, 34)
        }
        kterms <- c("k+s3", "k+g5", "k+g2", "k+g4", "k+g3")
        ref <- WaverideR::astrogeo22(mean(age))
        if (p$method == "mc") {
          v <- cd_as_rng(p$pshort)
          if (!is.null(v)) {
            win[["k+g4|k+g3"]] <- v
          }
          ex <- cd_as_parse_extra(p$extra)
          for (nm in names(ex)) {
            win[[nm]] <- ex[[nm]]
          }
          if (!length(win)) {
            stop("give at least one cycle", call. = FALSE)
          }
          r <- cd_as_run("astro_from_periods", list(cycles = win, age = age, anchor = cd_as_pos(p$anchor),
              anchor_term = p$anchor_term, nsim = p$nsim, ptest = kr * 1000, method = p$em, s3_uncertainty = p$s3,
              seed = p$seed, verbose = FALSE, sampleorb = p$sampleorb))
          v <- r$value
          em <- v$em
          s <- v$ctest$samples
          emt <- cd_as_rn(v$earthmoon)
          o <- list(cd_output("CTest_tuned_EarthMoon.txt", emt, header = TRUE,
                  var = "data.frame(quantity = rownames(res$earthmoon), res$earthmoon, check.names = FALSE)"),
              cd_output("CTest_tuned_frequencies.txt", v$frequencies, header = TRUE, var = "res$frequencies"),
              cd_output("CTest_tuned_periods.txt", v$periods, header = TRUE, var = "res$periods"),
              cd_output("CTest_tuned_k_samples.txt", data.frame(sample = seq_along(s$k), k_yr = s$k), header = TRUE,
                  var = "data.frame(sample = seq_along(res$ctest$samples$k), k_yr = res$ctest$samples$k)"))
          keep_s <- s[, intersect(names(s), c("scale", "obl_ks3", "prec_kg5", "prec_kg2", "prec_kg4", "prec_kg3")), drop = FALSE]
          post <- list(k_kyr = em$samples$k / 1000, a_RE = em$samples$a_RE, lod = em$samples$lod_solar_h,
              days = em$samples$days_per_year, extra = cd_as_extra_panels(keep_s, depth = FALSE), ref = ref, age = age)
          txt <- c(sprintf("k = %s +/- %s kyr; AstroGeo22 at %s Ma: %s kyr", cd_fmt(signif(mean(s$k) / 1000, 4)),
              cd_fmt(signif(stats::sd(s$k) / 1000, 2)), cd_fmt(signif(mean(age), 4)), cd_fmt(signif(ref$p_kyr, 4))),
              utils::capture.output(print(v)))
          code <- c(r$code, "print(res)")
          res <- list(post = post, fit = NULL)
        } else {
          if (p$method == "zeeden") {
            bad <- setdiff(names(win), c("e405", kterms))
            if (length(bad)) {
              stop("astro_ctest accepts only e405 and the obliquity and precession terms (not ", paste(bad, collapse = ", "),
                  ")", call. = FALSE)
            }
            anc <- cd_as_pos(p$anchor)
            if (is.null(anc)) {
              stop("astro_ctest needs the period assumed for the 405-kyr tuning target", call. = FALSE)
            }
            r <- cd_as_run("astro_ctest", list(windows = win, age_ma = age, nsim = p$nsim, k_range = kr, anchor_kyr = anc,
                seed = p$seed, verbose = FALSE))
          } else {
            win <- win[intersect(names(win), kterms)]
            if (!length(win)) {
              stop("astro_invert needs at least one obliquity or precession window", call. = FALSE)
            }
            r <- cd_as_run("astro_invert", list(windows = win, nsim = p$nsim, obl_deg = signif(ref$obliquity_deg, 8),
                k_range = kr, seed = p$seed))
          }
          v <- r$value
          kt <- data.frame(quantity = "k, precession constant (kyr)", mean = v$k[["mean"]], lo95 = v$k[["2.5%"]],
              hi95 = v$k[["97.5%"]])
          st <- cd_as_rn(v$state)
          names(st) <- c("quantity", "mean", "lo95", "hi95")
          tab <- rbind(kt, st)
          dr <- as.data.frame(v$draws)
          o <- list(cd_output(sprintf("CTest_tuned_%s_summary.txt", p$method), tab, header = TRUE, var = "tab"),
              cd_output(sprintf("CTest_tuned_%s_k_samples.txt", p$method), data.frame(sample = seq_along(dr$k_kyr),
                  k_yr = dr$k_kyr * 1000), header = TRUE,
                  var = "data.frame(sample = seq_along(res$draws$k_kyr), k_yr = res$draws$k_kyr * 1000)"))
          code <- c(r$code, "tab <- rbind(data.frame(quantity = \"k (kyr)\", mean = res$k[[\"mean\"]], lo95 = res$k[[\"2.5%\"]], hi95 = res$k[[\"97.5%\"]]),",
              "    data.frame(quantity = rownames(res$state), mean = res$state[, 1], lo95 = res$state[, 2], hi95 = res$state[, 3]))",
              sprintf("WaverideR::plot_astro(list(k = res$k, state = res$state, draws = res$draws, windows = res$windows[intersect(names(res$windows), %s)]), age_ma = %s)",
                  cd_as_val(kterms), cd_fmt(mean(age))))
          txt <- c(sprintf("k = %s kyr (95%%: %s to %s); AstroGeo22 at %s Ma: %s kyr", cd_fmt(signif(v$k[["mean"]], 4)),
              cd_fmt(signif(v$k[["2.5%"]], 4)), cd_fmt(signif(v$k[["97.5%"]], 4)), cd_fmt(signif(mean(age), 4)),
              cd_fmt(signif(ref$p_kyr, 4))),
              if (p$method == "invert") { "astro_invert uses only the obliquity and precession windows (no age, no eccentricity)." } else { character() },
              cd_as_table_text(tab))
          fit <- list(k = v$k, state = v$state, draws = dr[, intersect(names(dr), c("k_kyr", "a_RE", "lod_solar_h",
              "days_per_year")), drop = FALSE], windows = win[intersect(names(win), kterms)])
          post <- list(k_kyr = dr$k_kyr, a_RE = dr$a_RE, lod = dr$lod_solar_h, days = dr$days_per_year, extra = list(),
              ref = ref, age = age)
          res <- list(post = post, fit = fit)
        }
        res$age <- mean(age)
        list(result = res, outputs = o, code = c(code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        if (!is.null(res$fit) && length(res$fit$windows)) {
          peaks <- numeric()
          for (w in res$fit$windows) {
            peaks <- c(peaks, mean(w))
          }
          WaverideR::plot_astro(res$fit, peaks = peaks, age_ma = res$age)
        } else {
          cd_as_plot_post(res$post)
        }
      })
}

# Precession constant from measured cycles ####

cd_as_tool_k <- function(menu, group, em_choices, orb_choices, s3_choices) {
  sw <- function(...) {
    list(param = "method", values = c(...))
  }
  mc <- c("cycles", "ratio", "bundling")
  cd_tool("as_k", menu, "Precession constant from cycles", inputs = 0L, group = group, icon = "compass",
      heavy = function(p) {
        p$method %in% mc && isTRUE(p$nsim > 5000)
      },
      info = paste("The precession constant k and the Earth-Moon state from measured cycles: single cycles read as",
          "named terms, the ratio of two cycles (works on untuned depth data), a bundling count, every possible",
          "reading of one peak, a quick conversion, or a scan of k against several peaks."),
      uses = paste("WaverideR::k_from_cycles, WaverideR::k_from_ratio, WaverideR::k_from_bundling, WaverideR::k_hypotheses,",
          "WaverideR::k_from_period, WaverideR::k_scan"),
      params = list(
        cd_sel("method", "Method", c("Single cycles read as named terms" = "cycles", "Ratio of two cycles" = "ratio",
            "Precession cycles per eccentricity bundle" = "bundling", "Every reading of one peak (hypotheses)" = "hypotheses",
            "Quick conversion of periods to k" = "quick", "Scan k against measured peaks" = "scan")),
        cd_txt("periods", "Measured periods (kyr)", "29.7, 19.4, 18.5, 16.1", numeric = TRUE,
            show_if = sw("cycles", "hypotheses", "quick", "scan"),
            help = "Hypotheses use the first period only."),
        cd_txt("terms", "Term of each period", "k+s3, k+g5, k+g2, k+g4", show_if = sw("cycles")),
        cd_num("long", "Longer cycle (kyr or m)", 0.59, min = 0, show_if = sw("ratio")),
        cd_num("short", "Shorter cycle (same unit)", 0.388, min = 0, show_if = sw("ratio")),
        cd_txt("term_long", "Term of the longer cycle", "k+s3", show_if = sw("ratio")),
        cd_txt("term_short", "Term of the shorter cycle", "k+g5", show_if = sw("ratio")),
        cd_num("rel_sd", "Relative 1-sigma uncertainty of each cycle", 0.03, min = 0, step = 0.01, show_if = sw("ratio")),
        cd_num("n", "Cycles counted per bundle (may be fractional)", 20.5, min = 1, show_if = sw("bundling")),
        cd_txt("bundle_term", "Eccentricity term of the bundle", "e405", show_if = sw("bundling")),
        cd_txt("prec_term", "Bundled term", "k+g5", show_if = sw("bundling")),
        cd_num("n_sd", "1-sigma counting uncertainty (cycles)", 0.5, min = 0, show_if = sw("bundling")),
        cd_txt("hyp_terms", "Candidate terms", "k+g5, k+g2, k+g4, k+g3, k+s3", show_if = sw("hypotheses")),
        cd_sel("term1", "Term of the periods", c("Obliquity k+s3" = "k+s3", "Precession k+g5" = "k+g5",
            "Precession k+g2" = "k+g2", "Precession k+g4" = "k+g4", "Precession k+g3" = "k+g3"), show_if = sw("quick")),
        cd_txt("scan_terms", "Terms predicted for every k", "k+s3, k+g5, k+g2, k+g4", show_if = sw("scan")),
        cd_num("kmin", "Smallest k (kyr)", 12, min = 1, show_if = sw("scan")),
        cd_num("kmax", "Largest k (kyr)", 40, min = 1, show_if = sw("scan")),
        cd_num("kstep", "Step of k (kyr)", 0.02, min = 0.001, show_if = sw("scan")),
        cd_num("scan_tol", "Relative tolerance of a match", 0.04, min = 0.001, step = 0.01, show_if = sw("scan")),
        cd_txt("age", "Age (Ma): one value or min, max", "385", numeric = TRUE, show_if = sw(mc, "hypotheses")),
        cd_num("reclen", "Record length (kyr or m; 0 = use the tolerance / sd)", 0, min = 0, show_if = sw("cycles", "ratio"),
            help = "With a record length the band of each cycle is its frequency resolution."),
        cd_num("tol", "Relative half-width of each cycle", 0.05, min = 0.001, step = 0.01, show_if = sw("cycles")),
        cd_num("mult", "Multiple of the frequency resolution", 1, min = 0.1, show_if = sw("cycles", "ratio")),
        cd_num("tuned_to", "Period assumed for the tuning target (kyr, 0 = untuned)", 0, min = 0, show_if = sw("cycles")),
        cd_sel("combine", "Combine several k terms by", c("Multiplying the densities" = "multiply",
            "Pooling the samples" = "pool"), show_if = sw("cycles")),
        cd_txt("g4g3", "g4-g3 period range for chaos (kyr, empty = La2004)", "", numeric = TRUE,
            show_if = sw("cycles", "bundling")),
        cd_txt("s4s3", "s4-s3 period range (kyr, empty = La2004)", "", numeric = TRUE, show_if = sw("cycles", "bundling")),
        cd_sel("sampleorb", "Sampling of the secular frequencies", orb_choices, show_if = sw("cycles", "bundling")),
        cd_sel("s3", "Uncertainty of s3", s3_choices, show_if = sw("cycles")),
        cd_num("nsim", "Monte Carlo draws (0 = point estimate)", 20000, min = 0, step = 1000, show_if = sw(mc)),
        cd_sel("em", "Earth-Moon conversion", em_choices, show_if = sw(mc, "hypotheses")),
        cd_num("seed", "Random seed", 1, step = 1, show_if = sw(mc))),
      compute = function(d, p, d2, file, file2) {
        age <- cd_as_age(p$age, 385)
        per <- p$periods[is.finite(p$periods) & p$periods > 0]
        ref <- WaverideR::astrogeo22(mean(age))
        per_term <- NULL
        k_kyr <- NULL
        tag <- p$method
        if (p$method == "cycles") {
          tm <- cd_as_terms(p$terms)
          if (!length(per) || length(tm) != length(per)) {
            stop("give one term for every period", call. = FALSE)
          }
          cyc <- per
          names(cyc) <- tm
          r <- cd_as_run("k_from_cycles", list(cycles = cyc, age_ma = age, record_length_kyr = cd_as_pos(p$reclen),
              tol = p$tol, mult = p$mult, nsim = p$nsim, method = p$em, combine = p$combine, seed = p$seed,
              tuned_to = cd_as_pos(p$tuned_to), anchor_term = "e405", sampleorb = p$sampleorb,
              g4g3_range_kyr = cd_as_rng(p$g4g3), s4s3_range_kyr = cd_as_rng(p$s4s3), s3_uncertainty = p$s3))
          v <- r$value
          per_term <- as.data.frame(v$per_term)
          summ <- cd_as_rn(v$combined)
          k_kyr <- if (length(v$samples) > 1) { v$samples / 1000 } else { NULL }
          o <- list(cd_output("k_cycles_per_term.txt", per_term, header = TRUE, var = "res$per_term"),
              cd_output("k_cycles_EarthMoon.txt", summ, header = TRUE,
                  var = "data.frame(quantity = rownames(res$combined), res$combined, check.names = FALSE)"))
          txt <- c(if (isTRUE(v$point)) { "Point estimate (no Monte Carlo draws)." } else if (isTRUE(v$consistent)) { "The per-term 95% intervals overlap: the readings are consistent." } else {
            "The per-term 95% intervals do NOT all overlap: check the term assignment." },
            cd_as_table_text(per_term, "Per term"), cd_as_table_text(summ, "Combined estimate and Earth-Moon system"))
          if (!is.null(v$secular)) {
            txt <- c(txt, cd_as_table_text(as.data.frame(v$secular), "Secular-frequency terms"))
          }
        } else if (p$method %in% c("ratio", "bundling")) {
          if (p$method == "ratio") {
            if (!isTRUE(p$long > p$short) || !isTRUE(p$short > 0)) {
              stop("the longer cycle must be longer than the shorter one", call. = FALSE)
            }
            r <- cd_as_run("k_from_ratio", list(long = p$long, short = p$short, term_long = p$term_long,
                term_short = p$term_short, age_ma = age, rel_sd = p$rel_sd, record_length = cd_as_pos(p$reclen),
                mult = p$mult, nsim = p$nsim, method = p$em, seed = p$seed))
          } else {
            r <- cd_as_run("k_from_bundling", list(n = p$n, bundle_term = p$bundle_term, prec_term = p$prec_term,
                age_ma = age, n_sd = p$n_sd, nsim = p$nsim, method = p$em, sampleorb = p$sampleorb,
                g4g3_range_kyr = cd_as_rng(p$g4g3), s4s3_range_kyr = cd_as_rng(p$s4s3), seed = p$seed))
          }
          v <- r$value
          summ <- cd_as_rn(v$summary)
          k_kyr <- if (length(v$samples) > 1) { v$samples / 1000 } else { NULL }
          o <- list(cd_output(sprintf("k_%s_EarthMoon.txt", p$method), summ, header = TRUE,
              var = "data.frame(quantity = rownames(res$summary), res$summary, check.names = FALSE)"))
          txt <- c(sprintf("k = %s kyr, sd %s, 95%% %s to %s (terms %s)", cd_fmt(signif(v$k_kyr, 4)),
              cd_fmt(signif(v$k_sd_kyr, 2)), cd_fmt(signif(v$k_ci95_kyr[1], 4)), cd_fmt(signif(v$k_ci95_kyr[2], 4)),
              paste(v$terms, collapse = " / ")),
              if (p$method == "bundling") { sprintf("Predicted count with the AstroGeo22 k: %s", cd_fmt(signif(v$predicted_n_model, 4))) } else {
                sprintf("Ratio %s", cd_fmt(signif(v$ratio, 5))) },
              cd_as_table_text(summ, "Earth-Moon system"))
        } else if (p$method == "hypotheses") {
          if (!length(per)) {
            stop("give the measured period", call. = FALSE)
          }
          r <- cd_as_run("k_hypotheses", list(period_kyr = per[1], age_ma = mean(age), terms = cd_as_terms(p$hyp_terms),
              method = p$em))
          summ <- as.data.frame(r$value)
          rownames(summ) <- NULL
          o <- list(cd_output(sprintf("k_hypotheses_%skyr.txt", cd_fmt(per[1])), summ, header = TRUE))
          txt <- c(sprintf("Reading a %s-kyr cycle as each candidate term (AstroGeo22 k at %s Ma: %s kyr)", cd_fmt(per[1]),
              cd_fmt(mean(age)), cd_fmt(signif(ref$p_kyr, 4))), cd_as_table_text(summ))
        } else if (p$method == "quick") {
          if (!length(per)) {
            stop("give at least one period", call. = FALSE)
          }
          r <- cd_as_run("k_from_period", list(period_kyr = per, term = p$term1), var = "k")
          summ <- data.frame(period_kyr = per, term = p$term1, k_kyr = r$value)
          o <- list(cd_output(sprintf("k_from_%s.txt", gsub("[^A-Za-z0-9]", "", p$term1)), summ, header = TRUE))
          r$code <- c(r$code, sprintf("res <- data.frame(period_kyr = %s, term = %s, k_kyr = k)", cd_as_val(per), cd_dep(p$term1)))
          txt <- cd_as_table_text(summ)
        } else {
          if (!length(per)) {
            stop("give at least one peak period", call. = FALSE)
          }
          if (p$kmax <= p$kmin) {
            stop("the largest k must exceed the smallest", call. = FALSE)
          }
          kg <- seq(p$kmin, p$kmax, by = p$kstep)
          r <- cd_as_run("k_scan", list(peaks = per, terms = cd_as_terms(p$scan_terms), k_grid = kg, tol = p$scan_tol))
          r$code <- sub("k_grid = c\\(.*\\), tol", sprintf("k_grid = seq(%s, %s, by = %s), tol", cd_fmt(p$kmin), cd_fmt(p$kmax),
              cd_fmt(p$kstep)), r$code)
          summ <- as.data.frame(r$value)
          o <- list(cd_output("k_scan.txt", summ, header = TRUE))
          best <- summ$k_kyr[summ$matched == max(summ$matched)]
          txt <- c(sprintf("Most terms matched: %d of %d, for k between %s and %s kyr (AstroGeo22 at %s Ma: %s kyr)",
              as.integer(max(summ$matched)), length(cd_as_terms(p$scan_terms)), cd_fmt(min(best)), cd_fmt(max(best)),
              cd_fmt(mean(age)), cd_fmt(signif(ref$p_kyr, 4))))
        }
        if (!is.null(k_kyr)) {
          o[[length(o) + 1]] <- cd_output(sprintf("k_%s_samples.txt", tag), data.frame(sample = seq_along(k_kyr),
              k_yr = k_kyr * 1000), header = TRUE, var = "data.frame(sample = seq_along(res$samples), k_yr = res$samples)")
        }
        res <- list(method = p$method, table = summ, per_term = per_term, k_kyr = k_kyr, age = age, ref = ref,
            per = per)
        list(result = res, outputs = o, code = c(r$code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        if (!is.null(res$k_kyr)) {
          cd_as_plot_k(res)
          return(invisible(NULL))
        }
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        tab <- res$table
        if (res$method %in% c("cycles", "ratio", "bundling")) {
          cd_message_plot("Point estimate only (no Monte Carlo draws): see the table")
        } else if (res$method == "hypotheses") {
          cd_par(mar = c(4, 5, 2, 1))
          n <- nrow(tab)
          cols <- ifelse(tab$moon_beyond_today, "#7f8c8d", "#c0392b")
          graphics::plot(tab$k_kyr, seq_len(n), ylim = c(n + 0.5, 0.5), pch = 16, cex = 1.4, col = cols, yaxt = "n", ylab = "",
              xlab = "k (kyr)", xlim = range(c(tab$k_kyr, res$ref$p_kyr)) + c(-1, 1),
              main = sprintf("A %s-kyr cycle read as each term", cd_fmt(res$per[1])))
          graphics::axis(2, at = seq_len(n), labels = tab$term, las = 1)
          graphics::abline(v = res$ref$p_kyr, col = "grey35", lty = 2, lwd = 2)
          graphics::legend("topright", c("plausible", "Moon beyond today's distance", "AstroGeo22"), pch = c(16, 16, NA),
              lty = c(NA, NA, 2), col = c("#c0392b", "#7f8c8d", "grey35"), bty = "n", cex = 0.8)
        } else if (res$method == "quick") {
          cd_par()
          kp <- seq(min(res$per) * 0.7, max(res$per) * 1.3, length.out = 300)
          kc <- WaverideR::k_from_period(kp, tab$term[1])
          graphics::plot(kp, kc, type = "l", lwd = 2, col = "#1f4e79", xlab = sprintf("Period of %s (kyr)", tab$term[1]),
              ylab = "k (kyr)", main = "Precession constant implied by the period")
          graphics::grid(col = "grey90", lty = 1)
          graphics::points(tab$period_kyr, tab$k_kyr, pch = 16, col = "#c0392b", cex = 1.3)
        } else {
          cd_par()
          graphics::plot(tab$k_kyr, tab$matched, type = "s", lwd = 1.5, col = "#1f4e79", xlab = "k (kyr)",
              ylab = "Terms matched", main = "Single-k match score")
          graphics::grid(col = "grey90", lty = 1)
          graphics::abline(v = res$ref$p_kyr, col = "grey35", lty = 2, lwd = 2)
          graphics::legend("topright", sprintf("AstroGeo22 at %s Ma", cd_fmt(mean(res$age))), col = "grey35", lty = 2,
              lwd = 2, bty = "n", cex = 0.8)
        }
      })
}

# Combine two k estimates ####

cd_as_tool_k_combine <- function(menu, group) {
  cd_tool("as_k_combine", menu, "Combine two k estimates", inputs = 2L, group = group, icon = "object-group",
      info = paste("Multiplies the densities of two sets of precession-constant samples (e.g. the *_k_samples.txt files",
          "of two CTest runs or two proxies), as in Zeeden et al. (2023). Column 2 of each file is used."),
      uses = "WaverideR::ctest_combine, WaverideR::earthmoon_state",
      params = list(
        cd_num("n", "Grid points of the densities", 1000, min = 50, step = 100),
        cd_num("age", "Age for the Earth-Moon state (Ma, 0 = present-day obliquity)", 385, min = 0)),
      compute = function(d, p, d2, file, file2) {
        x1 <- as.numeric(d[, 2])
        x2 <- as.numeric(d2[, 2])
        x1 <- x1[is.finite(x1)]
        x2 <- x2[is.finite(x2)]
        if (length(x1) < 5 || length(x2) < 5) {
          stop("each file needs at least 5 samples in column 2", call. = FALSE)
        }
        n1 <- tools::file_path_sans_ext(basename(file))
        n2 <- tools::file_path_sans_ext(basename(if (is.null(file2)) { "series2" } else { file2 }))
        lst <- list(x1, x2)
        names(lst) <- c(n1, n2)
        cb <- WaverideR::ctest_combine(lst, n = p$n)
        unit_yr <- stats::median(c(x1, x2)) > 1000
        k_yr <- if (unit_yr) { cb$median } else { cb$median * 1000 }
        ems <- WaverideR::earthmoon_state(k_yr = k_yr, age_ma = cd_as_pos(p$age))
        grid <- data.frame(k = cb$x, density = cb$density)
        o <- list(cd_output(cd_out_name(file, "-combined_k"), grid, header = TRUE, var = "grid"),
            cd_output(cd_out_name(file, "-combined_k_EarthMoon"), ems, header = TRUE, var = "em"))
        code <- c(cd_code_read(if (is.null(file2)) { "series2.txt" } else { file2 }, var = "dat2"), "x1 <- dat[, 2]",
            "x2 <- dat2[, 2]",
            sprintf("cb <- WaverideR::ctest_combine(list(%s = x1, %s = x2), n = %s)", make.names(n1), make.names(n2), cd_fmt(p$n)),
            "grid <- data.frame(k = cb$x, density = cb$density)",
            sprintf("em <- WaverideR::earthmoon_state(k_yr = %s, age_ma = %s)", if (unit_yr) { "cb$median" } else { "cb$median * 1000" },
                cd_as_val(cd_as_pos(p$age))), cd_as_code_write(o))
        u <- if (unit_yr) { "yr" } else { "kyr" }
        txt <- c(sprintf("Combined k: median %s %s, sd (half the inter-quartile span) %s %s", cd_fmt(signif(cb$median, 6)), u,
            cd_fmt(signif(cb$sd, 3)), u),
            sprintf("Quartiles: %s", paste(cd_as_fmt_vec(signif(cb$quantiles, 6)), collapse = ", ")),
            sprintf("Range shared by both estimates: %s to %s %s", cd_fmt(signif(cb$shared_range[1], 6)),
                cd_fmt(signif(cb$shared_range[2], 6)), u),
            cd_as_table_text(ems, "Earth-Moon state at the combined median"))
        list(result = list(x = lst, cb = cb, unit = u), outputs = o, code = code, text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        cd_par()
        cb <- res$cb
        d1 <- stats::density(res$x[[1]])
        d2 <- stats::density(res$x[[2]])
        ym <- max(d1$y, d2$y, cb$density / max(cb$density) * max(d1$y, d2$y))
        graphics::plot(NA, xlim = range(c(d1$x, d2$x)), ylim = c(0, ym * 1.1), xlab = sprintf("k (%s)", res$unit),
            ylab = "Density", main = "Combined precession constant")
        graphics::grid(col = "grey90", lty = 1)
        graphics::lines(d1, col = "#1f4e79", lwd = 2)
        graphics::lines(d2, col = "#2e8b57", lwd = 2)
        graphics::lines(cb$x, cb$density / max(cb$density) * max(d1$y, d2$y), col = "#c0392b", lwd = 3)
        graphics::abline(v = cb$median, col = "#c0392b", lty = 2)
        graphics::legend("topright", c(names(res$x), "combined (scaled)"), col = c("#1f4e79", "#2e8b57", "#c0392b"),
            lwd = c(2, 2, 3), bty = "n", cex = 0.8)
      })
}

# Test a tuned record ####

cd_as_tool_test_tuned <- function(menu, group) {
  cd_tool("as_test_tuned", menu, "Test a tuned record (precession constant)", inputs = 1L, group = group,
      icon = "list-check", heavy = TRUE,
      info = paste("Tests an age model: the significant obliquity and precession peaks of the tuned record must give",
          "one consistent precession constant k, which is compared with AstroGeo22; spectral peaks and amplitude",
          "modulation are tested as well. The data file is the tuned record (time, value)."),
      uses = "WaverideR::test_tuned, WaverideR::plot_test_k",
      params = list(
        cd_sel("unit", "Time unit of the record", c("kyr" = "kyr", "Ma" = "Ma")),
        cd_num("age_ma", "Age of the record (Ma)", 386, min = 0, max = 4429),
        cd_num("tbw", "MTM time-bandwidth product", function(d) {
          if (diff(range(d[, 1], na.rm = TRUE)) >= 3200) { 2 } else { 1.5 }
        }, min = 1.5, step = 0.5, help = paste("k+g5 and k+g2 separate only when tbw <= record length (kyr) / 1600;",
            "values below 1.5 (a single taper) fail in astrochron::mtm.")),
        cd_num("pad", "Padding factor", 5, min = 1, step = 1),
        cd_num("siglevel", "Confidence level of a peak", 0.9, min = 0.5, max = 0.999, step = 0.01),
        cd_chk("ftest", "Also require the harmonic F-test", FALSE),
        cd_num("tol", "Half-width of the search windows (fraction)", 0.04, min = 0.005, step = 0.005),
        cd_chk("scan", "Remove intervals with weak precession first", TRUE),
        cd_txt("scan_band", "Precession band of the pre-scan (kyr)", "15, 23", numeric = TRUE,
            show_if = list(param = "scan", values = "true")),
        cd_num("scan_win", "Running window of the pre-scan (kyr)", 200, min = 10, show_if = list(param = "scan", values = "true")),
        cd_num("scan_thr", "Weak when below this fraction of the median", 0.5, min = 0, step = 0.1,
            show_if = list(param = "scan", values = "true")),
        cd_num("scan_min", "Shortest piece kept or removed (kyr)", 100, min = 0, show_if = list(param = "scan", values = "true")),
        cd_num("n_surr", "Phase-randomised surrogates", 200, min = 2, step = 50),
        cd_num("seed", "Random seed", 42, step = 1),
        cd_num("k_ref", "Reference k (kyr, 0 = AstroGeo22 at the age)", 0, min = 0),
        cd_txt("k_dev", "Plausible range of k for the verdict (kyr)", "20.4, 21.6", numeric = TRUE),
        cd_txt("k_search", "Range of k searched (kyr, empty = around AstroGeo22)", "19.5, 23.5", numeric = TRUE)),
      compute = function(d, p, d2, file, file2) {
        s <- cd_clean(d)
        if (p$unit == "Ma") {
          s$x <- s$x * 1000
        }
        names(s) <- c("time_kyr", "value")
        note <- character()
        tbw <- p$tbw
        if (!is.finite(tbw) || tbw < 1.5) {
          tbw <- 1.5
          note <- "Note: tbw was raised to 1.5; with tbw = 1 (one taper) astrochron::mtm fails."
        }
        args <- list(URAN_time = s, tbw = tbw, pad = p$pad, siglevel = p$siglevel, ftest = isTRUE(p$ftest), tol = p$tol,
            scan = isTRUE(p$scan), scan_band = cd_as_rng(p$scan_band), scan_win = p$scan_win, scan_thr = p$scan_thr,
            scan_min = p$scan_min, n_surr = p$n_surr, seed = p$seed, plot = FALSE, verbose = FALSE, age_ma = p$age_ma,
            k_ref = cd_as_pos(p$k_ref), k_dev = cd_as_rng(p$k_dev), k_search = cd_as_rng(p$k_search))
        if (is.null(args$scan_band)) {
          args$scan_band <- c(15, 23)
        }
        if (is.null(args$k_dev)) {
          args$k_dev <- c(20.4, 21.6)
        }
        r <- NULL
        rep_txt <- utils::capture.output(r <- do.call(WaverideR::test_tuned, args))
        parts <- character()
        for (nm in names(args)[-1]) {
          parts <- c(parts, sprintf("%s = %s", nm, cd_as_val(args[[nm]])))
        }
        tests <- c("precession constant", "spectral peaks", "405 from 100-kyr envelope", "100 from precession envelope")
        vt <- data.frame(test = tests[seq_along(r$verdicts)], verdict = r$verdicts)
        kt <- as.data.frame(r$k_terms)
        o <- list(cd_output(cd_out_name(file, "-test_k_terms"), kt, header = TRUE, var = "res$k_terms"),
            cd_output(cd_out_name(file, "-test_verdicts"), vt, header = TRUE,
                var = "data.frame(test = seq_along(res$verdicts), verdict = res$verdicts)"))
        if (!is.null(r$k_ranges) && nrow(as.data.frame(r$k_ranges))) {
          o[[3]] <- cd_output(cd_out_name(file, "-test_k_ranges"), as.data.frame(r$k_ranges), header = TRUE, var = "res$k_ranges")
        }
        code <- c(if (p$unit == "Ma") { "dat[, 1] <- dat[, 1] * 1000" } else { character() },
            sprintf("res <- WaverideR::test_tuned(URAN_time = dat[, 1:2], %s)", paste(parts, collapse = ", ")),
            "WaverideR::plot_test_k(res)", cd_as_code_write(o))
        list(result = r, outputs = o, code = code, text = c(note, rep_txt))
      },
      plot = function(res, p) {
        WaverideR::plot_test_k(res)
      })
}

cd_as_tool_test_check <- function(menu, group) {
  cd_tool("as_test_check", menu, "Check the k test on synthetic records", inputs = 0L, group = group, icon = "flask",
      heavy = TRUE,
      info = paste("Builds synthetic tuned records with known precession constants (five orbital terms, a 405-kyr",
          "cycle and AR(1) noise) and shows the k recovered by the tuned-record test: a check that the chosen",
          "time-bandwidth product resolves the terms."),
      uses = "WaverideR::test_tuned_check",
      params = list(
        cd_txt("k_true", "Known k of the synthetic records (kyr)", "20.7, 21, 21.2, 21.4", numeric = TRUE),
        cd_num("tbw", "MTM time-bandwidth product", 2, min = 1.5, step = 0.5),
        cd_num("noise", "Noise level (sd multiplier)", 1.5, min = 0, step = 0.1),
        cd_num("len", "Record length (kyr)", 5500, min = 500, step = 500),
        cd_num("age_ma", "Age passed to the test (Ma)", 386, min = 0),
        cd_num("seed", "Random seed", 1, step = 1)),
      compute = function(d, p, d2, file, file2) {
        kt <- p$k_true[is.finite(p$k_true) & p$k_true > 0]
        if (!length(kt)) {
          stop("give at least one precession constant", call. = FALSE)
        }
        if (!is.finite(p$tbw) || p$tbw < 1.5) {
          stop("use a time-bandwidth product of at least 1.5 (astrochron::mtm fails with one taper)", call. = FALSE)
        }
        lines <- utils::capture.output(WaverideR::test_tuned_check(k_true = kt, tbw = p$tbw, noise = p$noise, seed = p$seed,
            len = p$len, age_ma = p$age_ma))
        lines <- lines[grepl("^true k", lines)]
        tab <- data.frame(k_true = numeric(length(lines)), k_obliquity = NA_real_, k_precession = NA_real_,
            k_combined = NA_real_)
        for (i in seq_along(lines)) {
          a <- strsplit(sub("^true k ", "", lines[i]), " -> ")[[1]]
          tab$k_true[i] <- as.numeric(a[1])
          b <- strsplit(a[2], ", ")[[1]]
          vals <- suppressWarnings(as.numeric(sub(".* ", "", b)))
          tab$k_obliquity[i] <- vals[1]
          tab$k_precession[i] <- vals[2]
          tab$k_combined[i] <- vals[3]
        }
        tab$error_kyr <- tab$k_combined - tab$k_true
        o <- list(cd_output(sprintf("TestTunedCheck_tbw%s.txt", cd_fmt(p$tbw)), tab, header = TRUE, var = "tab"))
        code <- c(sprintf("WaverideR::test_tuned_check(k_true = %s, tbw = %s, noise = %s, seed = %s, len = %s, age_ma = %s)",
            cd_as_val(kt), cd_fmt(p$tbw), cd_fmt(p$noise), cd_fmt(p$seed), cd_fmt(p$len), cd_fmt(p$age_ma)))
        txt <- c(lines, cd_as_table_text(tab))
        list(result = tab, outputs = o, code = code, text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        cd_par()
        tab <- res
        r <- range(c(tab$k_true, tab$k_obliquity, tab$k_precession, tab$k_combined), na.rm = TRUE) + c(-0.3, 0.3)
        graphics::plot(tab$k_true, tab$k_combined, xlim = r, ylim = r, pch = 16, cex = 1.4, col = "#1f4e79",
            xlab = "True k (kyr)", ylab = "Recovered k (kyr)", main = "Recovery of k from synthetic records")
        graphics::grid(col = "grey90", lty = 1)
        graphics::abline(0, 1, col = "grey40", lty = 2)
        graphics::points(tab$k_true, tab$k_obliquity, pch = 2, col = "#2e8b57")
        graphics::points(tab$k_true, tab$k_precession, pch = 6, col = "#c0392b")
        graphics::legend("topleft", c("obliquity + precession", "obliquity", "precession", "1:1"), pch = c(16, 2, 6, NA),
            lty = c(NA, NA, NA, 2), col = c("#1f4e79", "#2e8b57", "#c0392b", "grey40"), bty = "n", cex = 0.8)
      })
}

# Compare with published models ####

cd_as_tool_compare <- function(menu, group) {
  cd_tool("as_compare", menu, "Compare k with published models", inputs = 0L, group = group, icon = "scale-balanced",
      info = paste("Compares a measured precession constant with AstroGeo22 (Farhat et al. 2022) and the La2004",
          "extrapolation at the age of the record, or lists the Devonian reference values of Zeeden et al. (2023)."),
      uses = "WaverideR::compare_published, WaverideR::zeeden2023_table1, WaverideR::astrogeo22, WaverideR::la2004_extrap",
      params = list(
        cd_sel("method", "Table", c("Measured k against the published models" = "compare",
            "Zeeden et al. (2023) Table 1, Devonian reference values" = "zeeden")),
        cd_num("age", "Age of the record (Ma)", 386, min = 0, max = 4429),
        cd_num("k", "Measured k (kyr, 0 = none)", 20.7, min = 0, show_if = list(param = "method", values = "compare")),
        cd_num("obl", "Obliquity for the inversion (degrees, 0 = AstroGeo22)", 0, min = 0,
            show_if = list(param = "method", values = "compare"))),
      compute = function(d, p, d2, file, file2) {
        if (p$method == "compare") {
          k_yr <- cd_as_pos(p$k)
          if (!is.null(k_yr)) {
            k_yr <- k_yr * 1000
          }
          r <- cd_as_run("compare_published", list(age_ma = p$age, k_yr = k_yr, obliquity_deg = cd_as_pos(p$obl)))
          nm <- sprintf("ComparePublished_%sMa.txt", cd_fmt(p$age))
        } else {
          r <- cd_as_run("zeeden2023_table1", list())
          nm <- "Zeeden2023_Table1.txt"
        }
        tab <- as.data.frame(r$value)
        rownames(tab) <- NULL
        o <- list(cd_output(nm, tab, header = TRUE))
        list(result = list(table = tab, method = p$method, age = p$age, k = cd_as_pos(p$k)), outputs = o,
            code = c(r$code, cd_as_code_write(o)), text = cd_as_table_text(tab))
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        tab <- res$table
        tmax <- min(4400, max(1000, 1.4 * res$age))
        ag <- cd_as_ag_curve(tmax, by = 5)
        la <- WaverideR::la2004_extrap(ag$age_ma)
        graphics::layout(matrix(1:2, nrow = 1))
        cd_par()
        yl <- range(c(ag$p_kyr, tab$p_kyr), na.rm = TRUE)
        graphics::plot(ag$age_ma, ag$p_kyr, type = "l", lwd = 2, col = "grey35", ylim = yl, xlab = "Age (Ma)",
            ylab = "Precession constant k (kyr)", main = "Precession constant")
        graphics::grid(col = "grey90", lty = 1)
        graphics::lines(la$age_ma, la$p_kyr, col = "#8e44ad", lwd = 2, lty = 2)
        if (res$method == "compare") {
          if (!is.null(res$k)) {
            graphics::points(res$age, res$k, pch = 16, col = "#c0392b", cex = 1.5)
          }
        } else {
          dv <- grepl("Devonian", tab$source)
          graphics::rect(359, tab$p_kyr[dv] - tab$p_sd_kyr[dv], 419, tab$p_kyr[dv] + tab$p_sd_kyr[dv], col = "#c0392b40",
              border = "#c0392b")
          ok <- is.finite(tab$age_ma)
          graphics::points(tab$age_ma[ok], tab$p_kyr[ok], pch = 17, col = "#1f4e79", cex = 1.2)
          graphics::text(tab$age_ma[ok], tab$p_kyr[ok], tab$source[ok], pos = 4, cex = 0.6)
        }
        graphics::legend("topright", c("AstroGeo22", "La2004 extrapolation", if (res$method == "compare") { "measured" } else {
          "Zeeden et al. 2023" }), col = c("grey35", "#8e44ad", "#c0392b"), lwd = c(2, 2, NA), lty = c(1, 2, NA),
          pch = c(NA, NA, 16), bty = "n", cex = 0.75)
        cd_par()
        graphics::plot(ag$age_ma, ag$lod_solar_h, type = "l", lwd = 2, col = "grey35", xlab = "Age (Ma)",
            ylab = "Length of day (h)", main = "Length of day",
            ylim = range(c(ag$lod_solar_h, la$lod_h, tab$lod_solar_h), na.rm = TRUE))
        graphics::grid(col = "grey90", lty = 1)
        graphics::lines(la$age_ma, la$lod_h, col = "#8e44ad", lwd = 2, lty = 2)
        if (res$method == "compare" && !is.null(tab$lod_solar_h)) {
          m <- grepl("^measured", tab$source)
          graphics::points(rep(res$age, sum(m)), tab$lod_solar_h[m], pch = 16, col = "#c0392b", cex = 1.3)
        }
      })
}

# Earth-Moon calculator ####

cd_as_em_extra <- function(st) {
  om <- WaverideR::earth_omega(st$lod_solar_h, kind = "solar")
  st$omega_rad_s <- om
  st$moment_of_inertia_kg_m2 <- WaverideR::earth_moment_of_inertia(om)
  st$angular_momentum_kg_m2_s <- WaverideR::earthmoon_angular_momentum(st$a_RE, om)
  st
}

cd_as_tool_earthmoon <- function(menu, group, em_choices) {
  sw <- function(...) {
    list(param = "known", values = c(...))
  }
  cd_tool("as_earthmoon", menu, "Earth-Moon calculator", inputs = 0L, group = group, icon = "moon",
      info = paste("From any one known quantity (precession constant, Earth-Moon distance, length of day, lunar months",
          "per year, an observed cycle, or an age on the AstroGeo22 model) to the full Earth-Moon state: distance,",
          "solar and sidereal day, days and months per year, obliquity, spin rate and angular momentum."),
      uses = paste("WaverideR::earthmoon_state, WaverideR::earthmoon_from_precession, WaverideR::cycles_to_k,",
          "WaverideR::astrogeo22, WaverideR::earth_omega, WaverideR::earth_moment_of_inertia,",
          "WaverideR::earthmoon_angular_momentum"),
      params = list(
        cd_sel("known", "Known quantity", c("Precession constant k (kyr)" = "k", "Precession frequency (arcsec/yr)" = "p",
            "Earth-Moon distance (Earth radii)" = "a_RE", "Earth-Moon distance (km)" = "a_km",
            "Length of the solar day (h)" = "lod", "Lunar months per year" = "months",
            "Observed obliquity or precession cycle (kyr)" = "cycle", "Age on the AstroGeo22 model (Ma)" = "age")),
        cd_txt("v_k", "Precession constant k (kyr), one or more", "21, 25.7", numeric = TRUE, show_if = sw("k")),
        cd_txt("v_p", "Precession frequency (arcsec/yr)", "61.7", numeric = TRUE, show_if = sw("p")),
        cd_txt("v_a", "Earth-Moon distance (Earth radii)", "57.5", numeric = TRUE, show_if = sw("a_RE")),
        cd_txt("v_km", "Earth-Moon distance (km)", "367000", numeric = TRUE, show_if = sw("a_km")),
        cd_txt("v_lod", "Length of the solar day (h)", "22", numeric = TRUE, show_if = sw("lod")),
        cd_txt("v_months", "Lunar months per year", "13.9", numeric = TRUE, show_if = sw("months")),
        cd_sel("months_kind", "Type of month", c("Sidereal" = "sidereal", "Synodic" = "synodic"), show_if = sw("months")),
        cd_txt("v_cycle", "Observed cycle (kyr)", "29.6", numeric = TRUE, show_if = sw("cycle")),
        cd_sel("term", "The cycle is", c("Obliquity k+s3" = "k+s3", "Precession k+g5" = "k+g5", "Precession k+g2" = "k+g2",
            "Precession k+g4" = "k+g4", "Precession k+g3" = "k+g3"), show_if = sw("cycle")),
        cd_txt("v_age", "Age (Ma)", "0, 100, 400", numeric = TRUE, show_if = sw("age")),
        cd_sel("method", "Inversion of a precession value", em_choices, show_if = sw("k", "p", "cycle")),
        cd_sel("oblsrc", "Obliquity", c("AstroGeo22 at an age" = "age", "Present day (23.25 degrees)" = "today",
            "My own value" = "own"), show_if = sw("k", "p", "a_RE", "a_km", "lod", "months", "cycle")),
        cd_num("obl_age", "Age for the obliquity (Ma)", 400, min = 0, max = 4429, show_if = list(param = "oblsrc", values = "age")),
        cd_num("obl", "Obliquity (degrees)", 23.2545, min = 0, max = 90, show_if = list(param = "oblsrc", values = "own"))),
      compute = function(d, p, d2, file, file2) {
        kn <- p$known
        vals <- switch(kn, k = p$v_k, p = p$v_p, a_RE = p$v_a, a_km = p$v_km, lod = p$v_lod, months = p$v_months,
            cycle = p$v_cycle, age = p$v_age)
        vals <- vals[is.finite(vals) & vals >= 0]
        if (!length(vals)) {
          stop("give at least one value", call. = FALSE)
        }
        crossings <- NULL
        code <- character()
        if (kn == "age") {
          r <- cd_as_run("astrogeo22", list(age_ma = vals), var = "ag")
          ag <- r$value
          st <- data.frame(p_arcsec = ag$p_arcsec, k_kyr = ag$p_kyr, a_RE = ag$a_RE, a_km = ag$a_km,
              lod_solar_h = ag$lod_solar_h, lod_sidereal_h = ag$lod_sidereal_h, days_per_year = ag$days_per_year,
              sidereal_months_per_year = ag$sidereal_months_per_year, synodic_months_per_year = ag$synodic_months_per_year,
              obliquity_deg = ag$obliquity_deg, model_age_ma = ag$age_ma)
          code <- c(r$code, paste0("res <- data.frame(p_arcsec = ag$p_arcsec, k_kyr = ag$p_kyr, a_RE = ag$a_RE, a_km = ag$a_km,",
              " lod_solar_h = ag$lod_solar_h, lod_sidereal_h = ag$lod_sidereal_h, days_per_year = ag$days_per_year,",
              " sidereal_months_per_year = ag$sidereal_months_per_year, synodic_months_per_year = ag$synodic_months_per_year,",
              " obliquity_deg = ag$obliquity_deg, model_age_ma = ag$age_ma)"))
        } else {
          obl <- NULL
          oage <- NULL
          if (p$oblsrc == "own") {
            obl <- p$obl
          } else if (p$oblsrc == "age") {
            oage <- p$obl_age
          }
          args <- list(method = p$method, obliquity_deg = obl, age_ma = oage)
          if (kn == "k") {
            args <- c(list(p_kyr = vals), args)
          } else if (kn == "p") {
            args <- c(list(p_arcsec = vals), args)
          } else if (kn == "a_RE") {
            args <- c(list(a_RE = vals), args)
          } else if (kn == "a_km") {
            args <- c(list(a_km = vals), args)
          } else if (kn == "lod") {
            args <- c(list(lod_h = vals), args)
          } else if (kn == "months") {
            args <- c(list(months_per_year = vals, months_kind = p$months_kind), args)
          } else {
            rk <- cd_as_run("cycles_to_k", list(period_kyr = vals, term = p$term), var = "kc")
            code <- rk$code
            args <- c(list(k_yr = rk$value$k_yr), args)
          }
          r <- cd_as_run("earthmoon_state", args)
          if (kn == "cycle") {
            r$code <- sub("k_yr = c\\([^)]*\\)|k_yr = [0-9.e+]+", "k_yr = kc$k_yr", r$code)
          }
          st <- as.data.frame(r$value)
          code <- c(code, r$code)
          if (kn %in% c("k", "p", "cycle") && p$method == "astrogeo22") {
            pk <- st$k_kyr
            crossings <- WaverideR::earthmoon_from_precession(p_kyr = pk, method = "astrogeo22")
            crossings <- as.data.frame(crossings)
            code <- c(code, sprintf("crossings <- WaverideR::earthmoon_from_precession(p_kyr = res$k_kyr, method = \"astrogeo22\")"))
          }
        }
        st <- cd_as_em_extra(st)
        if (kn != "age") {
          st <- data.frame(input = vals, st, check.names = FALSE)
          names(st)[1] <- switch(kn, k = "k_kyr_in", p = "p_arcsec_in", a_RE = "a_RE_in", a_km = "a_km_in", lod = "lod_h_in",
              months = "months_in", cycle = "cycle_kyr_in")
        }
        code <- c(code, "res$omega_rad_s <- WaverideR::earth_omega(res$lod_solar_h)",
            "res$moment_of_inertia_kg_m2 <- WaverideR::earth_moment_of_inertia(res$omega_rad_s)",
            "res$angular_momentum_kg_m2_s <- WaverideR::earthmoon_angular_momentum(res$a_RE, res$omega_rad_s)")
        o <- list(cd_output(sprintf("EarthMoon_from_%s.txt", kn), st, header = TRUE))
        if (!is.null(crossings) && nrow(crossings)) {
          o[[2]] <- cd_output(sprintf("EarthMoon_from_%s_AstroGeo22_ages.txt", kn), crossings, header = TRUE, var = "crossings")
        }
        tt <- data.frame(quantity = names(st), stringsAsFactors = FALSE)
        for (i in seq_len(nrow(st))) {
          v <- character(ncol(st))
          for (j in seq_len(ncol(st))) {
            v[j] <- if (is.numeric(st[[j]])) { formatC(st[[j]][i], digits = 6, format = "g") } else { as.character(st[[j]][i]) }
          }
          tt[[sprintf("value %d", i)]] <- v
        }
        txt <- c(cd_as_table_text(tt),
            if (!is.null(crossings) && nrow(crossings)) { cd_as_table_text(crossings[, intersect(names(crossings),
                c("p_kyr_requested", "age_ma", "a_RE", "lod_solar_h", "obliquity_deg", "outside_model_range"))],
                "Ages at which the AstroGeo22 curve has this precession constant") } else { character() })
        list(result = list(state = st, known = kn, crossings = crossings), outputs = o,
            code = c(code, cd_as_code_write(o)), text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        st <- res$state
        mage <- st$model_age_ma
        tmax <- max(1000, 1.3 * max(c(mage, 0), na.rm = TRUE))
        ag <- cd_as_ag_curve(tmax, by = 5)
        graphics::layout(matrix(1:4, nrow = 2, byrow = TRUE))
        vars <- c("k_kyr", "a_RE", "lod_solar_h", "days_per_year")
        mv <- c("p_kyr", "a_RE", "lod_solar_h", "days_per_year")
        labs <- c("Precession constant k (kyr)", "Earth-Moon distance (Earth radii)", "Length of the solar day (h)",
            "Days per year")
        cols <- cd_cols(nrow(st))
        for (j in seq_along(vars)) {
          cd_par()
          y <- ag[[mv[j]]]
          graphics::plot(ag$age_ma, y, type = "l", lwd = 2, col = "grey35", xlab = "Age (Ma)", ylab = labs[j],
              ylim = range(c(y, st[[vars[j]]]), na.rm = TRUE), main = if (j == 1) { "AstroGeo22 and the computed state" } else { NULL })
          graphics::grid(col = "grey90", lty = 1)
          for (i in seq_len(nrow(st))) {
            graphics::abline(h = st[[vars[j]]][i], col = cols[i], lty = 2, lwd = 1.5)
            if (!is.null(mage) && is.finite(mage[i])) {
              graphics::points(mage[i], st[[vars[j]]][i], pch = 16, col = cols[i], cex = 1.3)
            }
          }
          if (j == 1) {
            lab <- if (res$known == "age") { sprintf("%s Ma", cd_as_fmt_vec(signif(st$model_age_ma, 5))) } else {
              sprintf("%s = %s", names(st)[1], cd_as_fmt_vec(signif(st[[1]], 5))) }
            graphics::legend("topright", c("AstroGeo22", lab), col = c("grey35", cols), lty = c(1, rep(2, nrow(st))),
                lwd = 2, bty = "n", cex = 0.75)
          }
        }
      })
}

# Earth-Moon relations ####

cd_as_tool_em_relations <- function(menu, group) {
  sw <- function(...) {
    list(param = "xvar", values = c(...))
  }
  cd_tool("as_em_relations", menu, "Earth-Moon relations", inputs = 0L, group = group, icon = "earth-europe",
      info = paste("Curves of the Earth-Moon system with conserved angular momentum over a range of distances, day",
          "lengths, months per year or precession constants, compared with the AstroGeo22 tidal model."),
      uses = paste("WaverideR::omega_from_moon_distance, WaverideR::lod_from_moon_distance, WaverideR::lod_hours,",
          "WaverideR::moon_distance_from_lod, WaverideR::moon_distance_from_months, WaverideR::months_per_year,",
          "WaverideR::days_per_year, WaverideR::precession_frequency, WaverideR::em_state,",
          "WaverideR::earth_moment_of_inertia, WaverideR::earthmoon_angular_momentum"),
      params = list(
        cd_sel("xvar", "Vary", c("Earth-Moon distance (Earth radii)" = "a", "Length of the solar day (h)" = "lod",
            "Lunar months per year" = "months", "Precession constant k (kyr)" = "k")),
        cd_num("a_from", "From (Earth radii)", 50, min = 20, show_if = sw("a")),
        cd_num("a_to", "To (Earth radii)", 60.14, min = 20, show_if = sw("a")),
        cd_num("lod_from", "From (h)", 18, min = 5, show_if = sw("lod")),
        cd_num("lod_to", "To (h)", 24, min = 5, show_if = sw("lod")),
        cd_num("m_from", "From (months per year)", 13.4, min = 12, show_if = sw("months")),
        cd_num("m_to", "To (months per year)", 15, min = 12, show_if = sw("months")),
        cd_sel("months_kind", "Type of month", c("Sidereal" = "sidereal", "Synodic" = "synodic"), show_if = sw("months")),
        cd_num("k_from", "From (kyr)", 16, min = 5, show_if = sw("k")),
        cd_num("k_to", "To (kyr)", 25.7, min = 5, show_if = sw("k")),
        cd_num("npts", "Number of points", 60, min = 5, step = 5),
        cd_num("obl", "Obliquity for the precession (degrees)", 23.2545, min = 0, max = 90),
        cd_chk("model", "Show the AstroGeo22 model (0-1000 Ma)", TRUE)),
      compute = function(d, p, d2, file, file2) {
        rng <- switch(p$xvar, a = c(p$a_from, p$a_to), lod = c(p$lod_from, p$lod_to), months = c(p$m_from, p$m_to),
            k = c(p$k_from, p$k_to))
        if (!all(is.finite(rng)) || rng[1] == rng[2]) {
          stop("give two different limits", call. = FALSE)
        }
        x <- seq(rng[1], rng[2], length.out = max(5, round(p$npts)))
        xs <- sprintf("x <- seq(%s, %s, length.out = %d)", cd_fmt(rng[1]), cd_fmt(rng[2]), length(x))
        if (p$xvar == "a") {
          a <- x
          ca <- "a <- x"
        } else if (p$xvar == "lod") {
          a <- WaverideR::moon_distance_from_lod(x, kind = "solar")
          ca <- "a <- WaverideR::moon_distance_from_lod(x, kind = \"solar\")"
        } else if (p$xvar == "months") {
          a <- WaverideR::moon_distance_from_months(x, kind = p$months_kind)
          ca <- sprintf("a <- WaverideR::moon_distance_from_months(x, kind = %s)", cd_dep(p$months_kind))
        } else {
          a <- WaverideR::em_state(x, obl_deg = p$obl)$a_RE
          ca <- sprintf("a <- WaverideR::em_state(x, obl_deg = %s)$a_RE", cd_fmt(p$obl))
        }
        ok <- is.finite(a)
        if (!any(ok)) {
          stop("no Earth-Moon distance found for this range", call. = FALSE)
        }
        x <- x[ok]
        a <- a[ok]
        om <- WaverideR::omega_from_moon_distance(a)
        lod <- WaverideR::lod_from_moon_distance(a, kind = "solar")
        lods <- WaverideR::lod_hours(om, kind = "sidereal")
        pf <- WaverideR::precession_frequency(a, obliquity_deg = p$obl)
        tab <- data.frame(x = x, a_RE = a, a_km = a * 6378.1366, lod_solar_h = lod, lod_sidereal_h = lods,
            days_per_year = WaverideR::days_per_year(lod), sidereal_months_per_year = WaverideR::months_per_year(a, "sidereal"),
            synodic_months_per_year = WaverideR::months_per_year(a, "synodic"), p_arcsec = pf$p_arcsec, k_kyr = pf$p_kyr,
            omega_rad_s = om, moment_of_inertia_kg_m2 = WaverideR::earth_moment_of_inertia(om),
            angular_momentum_kg_m2_s = WaverideR::earthmoon_angular_momentum(a, om))
        names(tab)[1] <- switch(p$xvar, a = "a_RE_in", lod = "lod_h_in", months = "months_in", k = "k_kyr_in")
        o <- list(cd_output(sprintf("EarthMoon_relations_%s.txt", p$xvar), tab, header = TRUE))
        code <- c(xs, ca, "om <- WaverideR::omega_from_moon_distance(a)",
            "lod <- WaverideR::lod_from_moon_distance(a, kind = \"solar\")",
            sprintf("pf <- WaverideR::precession_frequency(a, obliquity_deg = %s)", cd_fmt(p$obl)),
            paste0("res <- data.frame(x = x, a_RE = a, a_km = a * 6378.1366, lod_solar_h = lod,",
                " lod_sidereal_h = WaverideR::lod_hours(om, kind = \"sidereal\"), days_per_year = WaverideR::days_per_year(lod),",
                " sidereal_months_per_year = WaverideR::months_per_year(a, \"sidereal\"),",
                " synodic_months_per_year = WaverideR::months_per_year(a, \"synodic\"), p_arcsec = pf$p_arcsec, k_kyr = pf$p_kyr,",
                " omega_rad_s = om, moment_of_inertia_kg_m2 = WaverideR::earth_moment_of_inertia(om),",
                " angular_momentum_kg_m2_s = WaverideR::earthmoon_angular_momentum(a, om))"),
            cd_as_code_write(o))
        txt <- c(sprintf("%d points with conserved Earth-Moon angular momentum (%s kg m2/s)", nrow(tab),
            format(signif(tab$angular_momentum_kg_m2_s[1], 5))), cd_as_table_text(tab, max_rows = 20))
        list(result = list(table = tab, xvar = p$xvar, model = isTRUE(p$model)), outputs = o, code = code, text = txt)
      },
      plot = function(res, p) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        tab <- res$table
        xv <- res$xvar
        xlab <- switch(xv, a = "Earth-Moon distance (Earth radii)", lod = "Length of the solar day (h)",
            months = "Lunar months per year", k = "Precession constant k (kyr)")
        ys <- c("lod_solar_h", "days_per_year", "sidereal_months_per_year", "k_kyr")
        yl <- c("Length of the solar day (h)", "Days per year", "Sidereal months per year", "Precession constant k (kyr)")
        rep_i <- switch(xv, a = 0, lod = 1, months = 3, k = 4)
        if (rep_i > 0) {
          ys[rep_i] <- "a_RE"
          yl[rep_i] <- "Earth-Moon distance (Earth radii)"
        }
        ag <- NULL
        if (isTRUE(res$model)) {
          ag <- cd_as_ag_curve(1000, by = 5)
          agx <- switch(xv, a = ag$a_RE, lod = ag$lod_solar_h, months = ag$sidereal_months_per_year, k = ag$p_kyr)
        }
        graphics::layout(matrix(1:4, nrow = 2, byrow = TRUE))
        for (j in 1:4) {
          cd_par()
          y <- tab[[ys[j]]]
          graphics::plot(tab[[1]], y, type = "l", lwd = 2, col = "#1f4e79", xlab = xlab, ylab = yl[j],
              main = if (j == 1) { "Angular momentum conserved" } else { NULL })
          graphics::grid(col = "grey90", lty = 1)
          if (!is.null(ag)) {
            agy <- switch(ys[j], lod_solar_h = ag$lod_solar_h, days_per_year = ag$days_per_year,
                sidereal_months_per_year = ag$sidereal_months_per_year, k_kyr = ag$p_kyr, a_RE = ag$a_RE)
            graphics::lines(agx, agy, col = "grey45", lwd = 1.5, lty = 2)
          }
          if (j == 1 && !is.null(ag)) {
            graphics::legend("topleft", c("angular momentum conserved", "AstroGeo22, 0-1000 Ma"), col = c("#1f4e79", "grey45"),
                lwd = 2, lty = c(1, 2), bty = "n", cex = 0.75)
          }
        }
      })
}

# Geological time scale ####

cd_as_tool_gts <- function(menu, group, gts_choices) {
  cd_tool("as_gts", menu, "Geological time scale", inputs = 0L, group = group, icon = "mountain",
      info = paste("Boundaries, mid-ages and colours of the Geologic Time Scale 2020 units: a unit and its",
          "subdivisions, an age range, or the units at one age, drawn as a time-scale chart; optionally with the",
          "astronomical periods at the mid-age."),
      uses = "WaverideR::GTS_info, WaverideR::geo_loc, WaverideR::geo_col, WaverideR::geo_mid, WaverideR::calculate_orbit9",
      params = list(
        cd_sel("mode", "Show", c("A unit and its subdivisions" = "unit", "An age range" = "range",
            "The units at one age" = "age")),
        cd_sel("name", "Unit", gts_choices, value = "Silurian", show_if = list(param = "mode", values = "unit")),
        cd_num("tmin", "Youngest age (Ma)", 419.2, min = 0, show_if = list(param = "mode", values = "range")),
        cd_num("tmax", "Oldest age (Ma)", 443.8, min = 0, show_if = list(param = "mode", values = "range")),
        cd_num("age", "Age (Ma)", 435, min = 0, show_if = list(param = "mode", values = "age")),
        cd_chk("orbit", "Also give the astronomical periods at the mid-age", TRUE)),
      compute = function(d, p, d2, file, file2) {
        g <- WaverideR::GTS_info
        ranks <- c("Eon", "Era", "Period", "Epoch", "Age")
        g <- g[g$type %in% ranks, , drop = FALSE]
        colour <- grDevices::rgb(g$R, g$G, g$B, maxColorValue = 255)
        units <- data.frame(name = g$name, type = g$type, top_ma = g$top, base_ma = g$bot, mid_ma = (g$top + g$bot) / 2,
            duration_myr = g$bot - g$top, base_uncertainty_myr = g$boterr, colour = colour, stringsAsFactors = FALSE)
        code <- character()
        mark <- NULL
        if (p$mode == "unit") {
          lo <- WaverideR::geo_loc(p$name)
          if (!nrow(lo)) {
            stop("unit not found: ", p$name, call. = FALSE)
          }
          t0 <- as.numeric(lo[1, 1])
          t1 <- as.numeric(lo[1, 3])
          mid <- as.numeric(WaverideR::geo_mid(p$name)[1, 1])
          ucol <- WaverideR::geo_col(p$name)
          code <- sprintf("loc <- WaverideR::geo_loc(%s)\ncol <- WaverideR::geo_col(%s)\nmid <- WaverideR::geo_mid(%s)",
              cd_dep(p$name), cd_dep(p$name), cd_dep(p$name))
          sel <- units$top_ma >= t0 - 1e-9 & units$base_ma <= t1 + 1e-9
          head_txt <- sprintf("%s: %s to %s Ma (mid %s Ma, duration %s Myr), colour %s", gsub("_", " ", p$name), cd_fmt(t0),
              cd_fmt(t1), cd_fmt(signif(mid, 6)), cd_fmt(signif(t1 - t0, 5)), ucol)
        } else if (p$mode == "range") {
          t0 <- min(p$tmin, p$tmax)
          t1 <- max(p$tmin, p$tmax)
          if (t1 <= t0) {
            stop("give two different ages", call. = FALSE)
          }
          mid <- (t0 + t1) / 2
          sel <- units$base_ma > t0 & units$top_ma < t1
          head_txt <- sprintf("Units overlapping %s to %s Ma", cd_fmt(t0), cd_fmt(t1))
        } else {
          sel <- units$top_ma <= p$age & units$base_ma >= p$age
          if (!any(sel)) {
            stop("no unit contains this age", call. = FALSE)
          }
          per <- units[sel & units$type == "Period", , drop = FALSE]
          if (nrow(per)) {
            t0 <- per$top_ma[1]
            t1 <- per$base_ma[1]
          } else {
            t0 <- max(0, p$age - 10)
            t1 <- p$age + 10
          }
          mid <- p$age
          mark <- p$age
          head_txt <- sprintf("Units at %s Ma: %s", cd_fmt(p$age), paste(sprintf("%s %s", units$type[sel],
              gsub("_", " ", units$name[sel])), collapse = ", "))
        }
        tab <- units[sel, , drop = FALSE]
        rownames(tab) <- NULL
        ctx <- units[units$base_ma > t0 & units$top_ma < t1, , drop = FALSE]
        nm <- if (p$mode == "unit") { sprintf("GTS2020_%s.txt", gsub("[^A-Za-z0-9]", "_", p$name)) } else if (p$mode == "range") {
          sprintf("GTS2020_%s-%sMa.txt", cd_fmt(t0), cd_fmt(t1)) } else { sprintf("GTS2020_at_%sMa.txt", cd_fmt(p$age)) }
        o <- list(cd_output(nm, tab, header = TRUE, var = "tab"))
        code <- c(code, "g <- WaverideR::GTS_info",
            "tab <- data.frame(name = g$name, type = g$type, top_ma = g$top, base_ma = g$bot, mid_ma = (g$top + g$bot) / 2,",
            "    colour = rgb(g$R, g$G, g$B, maxColorValue = 255))",
            if (p$mode == "unit") { "tab <- tab[tab$top_ma >= loc[1, 1] & tab$base_ma <= loc[1, 3], ]" } else if (p$mode == "range") {
              sprintf("tab <- tab[tab$base_ma > %s & tab$top_ma < %s, ]", cd_fmt(t0), cd_fmt(t1)) } else {
              sprintf("tab <- tab[tab$top_ma <= %s & tab$base_ma >= %s, ]", cd_fmt(p$age), cd_fmt(p$age)) })
        txt <- c(head_txt, cd_as_table_text(tab[, c("name", "type", "top_ma", "base_ma", "mid_ma", "duration_myr", "colour")]))
        orb <- NULL
        if (isTRUE(p$orbit) && mid <= 4429) {
          orb <- WaverideR::calculate_orbit9(mid)
          orb <- orb[, c("term", "group", "freq_arcsec_yr", "period_kyr")]
          o[[2]] <- cd_output(sub("\\.txt$", "_periods.txt", nm), orb, header = TRUE, var = "orb")
          code <- c(code, sprintf("orb <- WaverideR::calculate_orbit9(%s)", cd_fmt(signif(mid, 8))))
          txt <- c(txt, cd_as_table_text(orb, sprintf("Astronomical periods at %s Ma (AstroGeo22 k)", cd_fmt(signif(mid, 5)))))
        }
        list(result = list(table = ctx, t0 = t0, t1 = t1, mark = mark), outputs = o, code = c(code, cd_as_code_write(o)),
            text = txt)
      },
      plot = function(res, p) {
        cd_as_plot_gts(res$table, res$t0, res$t1, res$mark)
      })
}
