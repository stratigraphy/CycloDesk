# Anchoring a record to an astronomical solution or a metronome ####
#
# The Anchor tab: click a peak in the record (a data file, or the cycle extracted in the Track tab) and the matching
# peak in the target (La2004 / La2010 / La2011, ETP, insolation or a metronome); every pair is a tie point. With one
# tie the floating age model is shifted; with more ties the age model runs through the ties and keeps the floating
# rates beyond the first and last tie. The tuned record is compared with the target as you go.

cd_anchor_tab <- function() {
  shiny::tabPanel("Anchor", value = "anchor",
      shiny::fluidRow(
          shiny::column(4, class = "cd-toolform",
              shiny::tags$h4("Anchor to an astronomical solution"),
              shiny::tags$div(class = "cd-info", "Click a peak in the record (top), then the matching peak in the target",
                  " (middle): each pair is a tie point. The bottom figure shows the tuned record on the target."),
              shiny::tags$div(class = "cd-uses", "astrochron::getLaskar, astrochron::etp, astrochron::tune"),
              cd_section("1. Record",
                  shiny::uiOutput("anc_rec_ui"),
                  shiny::radioButtons("anc_init", "Starting age model", c(
                      "floating model from the Track tab" = "track", "an age-model file (depth, age)" = "file",
                      "a constant sedimentation rate" = "sr"), selected = "track"),
                  shiny::conditionalPanel("input.anc_init == 'file'", shiny::uiOutput("anc_amfile_ui")),
                  shiny::conditionalPanel("input.anc_init == 'sr'",
                      shiny::fluidRow(
                          shiny::column(6, shiny::numericInput("anc_sr", "Sed. rate (cm/kyr)", 2, min = 0)),
                          shiny::column(6, shiny::selectInput("anc_unit", "Depth unit", c("m" = "100", "cm" = "1",
                              "ft" = "30.48"))))),
                  shiny::numericInput("anc_t0", "Age of the top of the record (ka)", 0),
                  shiny::checkboxInput("anc_down", "Age increases with depth", TRUE)),
              cd_section("2. Target",
                  shiny::selectInput("anc_tgt", "Astronomical solution or metronome", c(
                      "La2004 eccentricity" = "la04|ecc", "La2004 obliquity" = "la04|obl", "La2004 precession index" = "la04|prec",
                      "La2004 ETP" = "la04|etp", "Insolation 65N, June solstice (La2004)" = "la04|insol",
                      "La2010b eccentricity" = "la10b|ecc", "La2011 eccentricity" = "la11|ecc", "Metronome" = "metronome")),
                  shiny::conditionalPanel("input.anc_tgt == 'metronome'",
                      shiny::fluidRow(
                          shiny::column(6, shiny::numericInput("anc_mperiod", "Period (kyr)", 405, min = 0)),
                          shiny::column(6, shiny::numericInput("anc_mphase", "Phase (degrees)", 0)))),
                  shiny::checkboxInput("anc_flip", "Flip the target (maxima become minima)", FALSE),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("anc_from", "Show from (ka)", NA)),
                      shiny::column(6, shiny::numericInput("anc_to", "to (ka)", NA))),
                  shiny::tags$div(class = "cd-hint", "Empty: the range of the starting age model plus a margin.")),
              cd_section("3. Tie points",
                  shiny::radioButtons("anc_snap", "A click snaps to", c("the nearest maximum" = "max",
                      "the nearest minimum" = "min", "nothing (exactly where you click)" = "free"), inline = FALSE),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("anc_undo", "Undo last tie", icon = shiny::icon("rotate-left"), class = "btn-sm"),
                      shiny::actionButton("anc_clear", "Clear ties", icon = shiny::icon("eraser"), class = "btn-sm")),
                  shiny::tableOutput("anc_ties")),
              cd_section("4. Results (written to the working folder)",
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("anc_save", "Save ties", icon = shiny::icon("floppy-disk"), class = "btn-sm"),
                      shiny::actionButton("anc_age", "Age model + tuned series", icon = shiny::icon("clock"),
                          class = "btn-sm"))),
              shiny::tags$br()),
          shiny::column(8,
              shiny::plotOutput("anc_rec_plot", height = "200px", click = "anc_rec_click"),
              shiny::plotOutput("anc_tgt_plot", height = "220px", click = "anc_tgt_click"),
              shiny::plotOutput("anc_fit_plot", height = "220px"),
              shiny::verbatimTextOutput("anc_text"))))
}

# Target series on an even time grid
cd_anchor_target <- function(spec, t0, t1, mperiod = 405, mphase = 0) {
  t0 <- max(0, floor(t0))
  t1 <- ceiling(t1)
  if (identical(spec, "metronome")) {
    dt <- max(mperiod / 60, 0.1)
    t <- seq(t0, t1, by = dt)
    return(data.frame(time = t, value = cos(2 * pi * t / mperiod + mphase * pi / 180)))
  }
  p <- strsplit(spec, "|", fixed = TRUE)[[1]]
  sol <- p[1]
  what <- p[2]
  if (what == "insol") {
    r <- cd_insolation(tmin = t0, tmax = t1, lat = 65, lon = 90)
  } else if (what %in% c("prec", "etp")) {
    a <- cd_laskar("la04")
    r <- if (what == "prec") {
      astrochron::etp(tmin = t0, tmax = t1, dt = 1, eWt = 0, oWt = 0, pWt = 1, solution = a, standardize = FALSE,
          genplot = FALSE, verbose = FALSE)
    } else {
      astrochron::etp(tmin = t0, tmax = t1, dt = 1, solution = a, genplot = FALSE, verbose = FALSE)
    }
  } else {
    a <- cd_laskar(sol)
    col <- if (what == "obl") { grep("^tilt", names(a))[1] } else { grep("^ecc", names(a))[1] }
    r <- a[a[, 1] >= t0 & a[, 1] <= t1, c(1, col)]
  }
  r <- as.data.frame(r)
  names(r) <- c("time", "value")
  r
}

# Index of the local extremum nearest to x (within 'win'); NA when there is none
cd_nearest_extreme <- function(xs, ys, x, type = "max", win = Inf) {
  y <- if (type == "min") { -ys } else { ys }
  n <- length(y)
  if (n < 3) {
    return(NA_integer_)
  }
  i <- 2:(n - 1)
  pk <- i[y[i] > y[i - 1] & y[i] >= y[i + 1]]
  pk <- pk[abs(xs[pk] - x) <= win]
  if (!length(pk)) {
    return(NA_integer_)
  }
  pk[which.min(abs(xs[pk] - x))]
}

# Depth -> age with the ties: through the ties, and parallel to the starting model beyond the first and last tie
cd_anchor_model <- function(depth, base_fun, ties) {
  b <- base_fun(depth)
  if (is.null(ties) || !nrow(ties)) {
    return(b)
  }
  if (nrow(ties) == 1) {
    return(b + (ties$time[1] - base_fun(ties$depth[1])))
  }
  o <- order(ties$depth)
  td <- ties$depth[o]
  tt <- ties$time[o]
  out <- stats::approx(td, tt, xout = depth, ties = mean)$y
  lo <- depth < min(td)
  hi <- depth > max(td)
  out[lo] <- tt[1] + (b[lo] - base_fun(td[1]))
  out[hi] <- tt[length(tt)] + (b[hi] - base_fun(td[length(td)]))
  out
}

cd_anchor_server <- function(input, output, session, ctx) {
  rv <- ctx$rv
  av <- shiny::reactiveValues(ties = data.frame(depth = numeric(), time = numeric()), pend_d = NULL, pend_t = NULL)

  output$anc_rec_ui <- shiny::renderUI({
    ctx$shared$ver
    ch <- ctx$data_files()
    if (!is.null(ctx$shared$extract)) {
      ch <- c(stats::setNames("track_extract", paste("Track tab:", ctx$shared$extract_label)),
          stats::setNames("track_data", "Track tab: the data of the tracked record"), stats::setNames(ch, ch))
    }
    sel <- shiny::isolate(ctx$selected_files())
    pick <- if (!is.null(ctx$shared$extract)) { "track_extract" } else if (length(sel)) { sel[1] } else { NULL }
    shiny::selectInput("anc_rec", "Record (depth domain)", choices = ch, selected = pick)
  })
  output$anc_amfile_ui <- shiny::renderUI({
    shiny::selectInput("anc_amfile", NULL, choices = ctx$data_files())
  })

  record <- shiny::reactive({
    ctx$shared$ver
    r <- input$anc_rec
    if (is.null(r) || !nzchar(r)) {
      return(NULL)
    }
    d <- if (r == "track_extract") {
      ctx$shared$extract
    } else if (r == "track_data") {
      ctx$shared$data
    } else {
      try(ctx$read_file(r), silent = TRUE)
    }
    if (is.null(d) || inherits(d, "try-error")) {
      return(NULL)
    }
    d <- cd_clean(d)
    d
  })

  # Starting age model as a function of depth
  base_fun <- shiny::reactive({
    d <- record()
    t0 <- if (is.finite(input$anc_t0)) { input$anc_t0 } else { 0 }
    sgn <- if (isTRUE(input$anc_down)) { 1 } else { -1 }
    if (identical(input$anc_init, "track") && !is.null(ctx$shared$age)) {
      am <- ctx$shared$age
      return(function(z) {
        a <- stats::approx(am[, 1], am[, 2], xout = z, rule = 2)$y
        # beyond the floating model: its mean rate
        rate <- diff(range(am[, 2])) / diff(range(am[, 1]))
        a[z < min(am[, 1])] <- am[1, 2] - (min(am[, 1]) - z[z < min(am[, 1])]) * rate
        a[z > max(am[, 1])] <- am[nrow(am), 2] + (z[z > max(am[, 1])] - max(am[, 1])) * rate
        t0 + sgn * (a - am[1, 2])
      })
    }
    if (identical(input$anc_init, "file") && !is.null(input$anc_amfile) && nzchar(input$anc_amfile)) {
      am <- try(ctx$read_file(input$anc_amfile), silent = TRUE)
      if (!inherits(am, "try-error") && nrow(am) >= 2) {
        am <- am[order(am[, 1]), ]
        return(function(z) {
          stats::approx(am[, 1], am[, 2], xout = z, rule = 2)$y
        })
      }
    }
    sr <- if (is.finite(input$anc_sr) && input$anc_sr > 0) { input$anc_sr } else { 1 }
    u <- as.numeric(input$anc_unit)
    if (!is.finite(u)) {
      u <- 100
    }
    top <- if (is.null(d)) { 0 } else { if (sgn > 0) { min(d$x) } else { max(d$x) } }
    function(z) {
      t0 + sgn * (z - top) * u / sr
    }
  })

  model <- function(z) {
    cd_anchor_model(z, base_fun(), av$ties)
  }

  target <- shiny::reactive({
    d <- record()
    if (is.null(d)) {
      return(NULL)
    }
    bt <- range(base_fun()(d$x), na.rm = TRUE)
    if (nrow(av$ties)) {
      bt <- range(c(bt, model(range(d$x))))
    }
    span <- diff(bt)
    t0 <- if (is.finite(input$anc_from)) { input$anc_from } else { bt[1] - 0.15 * span }
    t1 <- if (is.finite(input$anc_to)) { input$anc_to } else { bt[2] + 0.15 * span }
    tg <- try(cd_anchor_target(input$anc_tgt, max(0, t0), max(t1, t0 + 10), input$anc_mperiod, input$anc_mphase),
        silent = TRUE)
    if (inherits(tg, "try-error")) {
      return(list(error = conditionMessage(attr(tg, "condition"))))
    }
    if (isTRUE(input$anc_flip)) {
      tg$value <- -tg$value
    }
    tg
  })

  snap_type <- function() {
    if (identical(input$anc_snap, "min")) { "min" } else { "max" }
  }

  add_tie_if_ready <- function() {
    if (!is.null(av$pend_d) && !is.null(av$pend_t)) {
      t <- rbind(av$ties, data.frame(depth = av$pend_d, time = av$pend_t))
      av$ties <- t[order(t$depth), ]
      av$pend_d <- NULL
      av$pend_t <- NULL
    }
  }

  shiny::observeEvent(input$anc_rec_click, {
    d <- record()
    x <- input$anc_rec_click$x
    if (is.null(d) || is.null(x)) {
      return()
    }
    if (!identical(input$anc_snap, "free")) {
      i <- cd_nearest_extreme(d$x, d$y, x, snap_type(), win = 0.05 * diff(range(d$x)))
      if (is.finite(i)) {
        x <- d$x[i]
      }
    }
    av$pend_d <- x
    add_tie_if_ready()
  })
  shiny::observeEvent(input$anc_tgt_click, {
    tg <- target()
    x <- input$anc_tgt_click$x
    if (is.null(tg) || !is.null(tg$error) || is.null(x)) {
      return()
    }
    if (!identical(input$anc_snap, "free")) {
      i <- cd_nearest_extreme(tg$time, tg$value, x, snap_type(), win = 0.05 * diff(range(tg$time)))
      if (is.finite(i)) {
        x <- tg$time[i]
      }
    }
    av$pend_t <- x
    add_tie_if_ready()
  })
  shiny::observeEvent(input$anc_undo, {
    if (!is.null(av$pend_d) || !is.null(av$pend_t)) {
      av$pend_d <- NULL
      av$pend_t <- NULL
    } else if (nrow(av$ties)) {
      av$ties <- av$ties[-nrow(av$ties), , drop = FALSE]
    }
  })
  shiny::observeEvent(input$anc_clear, {
    av$ties <- data.frame(depth = numeric(), time = numeric())
    av$pend_d <- NULL
    av$pend_t <- NULL
  })
  shiny::observeEvent(input$anc_rec, {
    av$ties <- data.frame(depth = numeric(), time = numeric())
  }, ignoreInit = TRUE)

  output$anc_ties <- shiny::renderTable({
    t <- av$ties
    if (!nrow(t)) {
      return(NULL)
    }
    dep <- character(nrow(t))
    age <- character(nrow(t))
    for (k in seq_len(nrow(t))) {
      dep[k] <- cd_fmt(t$depth[k])
      age[k] <- cd_fmt(t$time[k])
    }
    data.frame(`#` = seq_len(nrow(t)), Depth = dep, `Age (ka)` = age, check.names = FALSE)
  })

  tie_cols <- function(n) {
    rep(c("#c0392b", "#2e8b57", "#8e44ad", "#d35400", "#16a085", "#1f4e79"), length.out = n)
  }

  output$anc_rec_plot <- shiny::renderPlot({
    d <- record()
    if (is.null(d)) {
      cd_message_plot("Choose a record (section 1)")
      return(invisible(NULL))
    }
    oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
    on.exit(graphics::par(oldpar))
    graphics::par(mar = c(3.6, 4.8, 1.6, 1), mgp = c(2.3, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
    graphics::plot(d$x, d$y, type = "l", col = "#1f4e79", xlab = "Depth", ylab = "Record", xaxs = "i",
        main = "Record: click a peak", cex.main = 0.9, font.main = 1)
    t <- av$ties
    if (nrow(t)) {
      cl <- tie_cols(nrow(t))
      graphics::abline(v = t$depth, col = cl, lwd = 1.5)
      graphics::mtext(seq_len(nrow(t)), side = 3, at = t$depth, line = 0.1, col = cl, cex = 0.75, font = 2)
    }
    if (!is.null(av$pend_d)) {
      graphics::abline(v = av$pend_d, col = "#ffb02e", lwd = 3, lty = 2)
    }
  }, res = 96)

  output$anc_tgt_plot <- shiny::renderPlot({
    tg <- target()
    if (is.null(tg)) {
      cd_message_plot("Choose a record first")
      return(invisible(NULL))
    }
    if (!is.null(tg$error)) {
      cd_message_plot(paste("Target:", tg$error))
      return(invisible(NULL))
    }
    oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
    on.exit(graphics::par(oldpar))
    graphics::par(mar = c(3.6, 4.8, 1.6, 1), mgp = c(2.3, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
    graphics::plot(tg$time, tg$value, type = "l", col = "#2b2b2b", xlab = "Age (ka)", ylab = "Target", xaxs = "i",
        main = "Target: click the matching peak (grey: the record on the current age model)", cex.main = 0.9, font.main = 1)
    d <- record()
    tt <- model(d$x)
    ok <- is.finite(tt) & tt >= min(tg$time) & tt <= max(tg$time)
    if (sum(ok) > 2) {
      yr <- range(tg$value)
      ys <- (d$y[ok] - min(d$y[ok])) / diff(range(d$y[ok])) * diff(yr) + yr[1]
      o <- order(tt[ok])
      graphics::lines(tt[ok][o], ys[o], col = grDevices::adjustcolor("#1f4e79", 0.35))
    }
    t <- av$ties
    if (nrow(t)) {
      cl <- tie_cols(nrow(t))
      graphics::abline(v = t$time, col = cl, lwd = 1.5)
      graphics::mtext(seq_len(nrow(t)), side = 3, at = t$time, line = 0.1, col = cl, cex = 0.75, font = 2)
    }
    if (!is.null(av$pend_t)) {
      graphics::abline(v = av$pend_t, col = "#ffb02e", lwd = 3, lty = 2)
    }
  }, res = 96)

  fit <- shiny::reactive({
    d <- record()
    tg <- target()
    if (is.null(d) || is.null(tg) || !is.null(tg$error)) {
      return(NULL)
    }
    tt <- model(d$x)
    o <- order(tt)
    tuned <- data.frame(time = tt[o], value = d$y[o])
    tv <- stats::approx(tg$time, tg$value, xout = tuned$time)$y
    ok <- is.finite(tv)
    r <- if (sum(ok) > 5) { stats::cor(tuned$value[ok], tv[ok]) } else { NA }
    list(tuned = tuned, target = tg, r = r)
  })

  output$anc_fit_plot <- shiny::renderPlot({
    f <- fit()
    if (is.null(f)) {
      cd_message_plot("The tuned record appears here")
      return(invisible(NULL))
    }
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(mar = c(3.6, 4.8, 1.6, 1), mgp = c(2.3, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
    z <- function(v) {
      (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)
    }
    xr <- range(f$tuned$time, na.rm = TRUE)
    tg <- f$target[f$target$time >= xr[1] & f$target$time <= xr[2], ]
    graphics::plot(tg$time, z(tg$value), type = "l", col = "#2b2b2b", xlim = xr, xaxs = "i", xlab = "Age (ka)",
        ylab = "standardised", ylim = range(c(z(tg$value), z(f$tuned$value)), na.rm = TRUE),
        main = sprintf("Tuned record (blue) on the target (black)%s", if (is.finite(f$r)) { sprintf(":  r = %.3f", f$r) } else { "" }),
        cex.main = 0.9, font.main = 1)
    graphics::lines(f$tuned$time, z(f$tuned$value), col = "#1f4e79", lwd = 1.3)
    if (nrow(av$ties)) {
      graphics::abline(v = av$ties$time, col = tie_cols(nrow(av$ties)), lty = 3)
    }
  }, res = 96)

  output$anc_text <- shiny::renderText({
    t <- av$ties
    txt <- character()
    if (!is.null(av$pend_d)) {
      txt <- c(txt, sprintf("Depth %s picked: now click the matching peak in the target.", cd_fmt(av$pend_d)))
    }
    if (!is.null(av$pend_t)) {
      txt <- c(txt, sprintf("Age %s ka picked: now click the matching peak in the record.", cd_fmt(av$pend_t)))
    }
    if (nrow(t) >= 2) {
      sr <- diff(t$depth) / diff(t$time)
      if (any(sr <= 0)) {
        txt <- c(txt, "Warning: the ties are not in order (age must change in one direction with depth).")
      }
      if (all(is.finite(sr))) {
        u <- if (identical(input$anc_init, "sr")) { as.numeric(input$anc_unit) } else { 100 }
        txt <- c(txt, sprintf("Sed. rate between the ties: %s to %s cm/kyr (for depths in %s).", cd_fmt(min(abs(sr)) * u),
            cd_fmt(max(abs(sr)) * u), if (u == 100) { "m" } else if (u == 1) { "cm" } else { "ft" }))
      }
    }
    f <- fit()
    if (!is.null(f) && is.finite(f$r)) {
      txt <- c(txt, sprintf("%d tie point(s); correlation of the tuned record with the target r = %.3f", nrow(t), f$r))
    }
    paste(txt, collapse = "\n")
  })

  rec_name <- function() {
    r <- input$anc_rec
    if (r %in% c("track_extract", "track_data")) {
      n <- ctx$shared$name
      if (is.null(n)) { "record" } else { paste0(n, if (r == "track_extract") { "-extract" } else { "" }) }
    } else {
      tools::file_path_sans_ext(r)
    }
  }
  shiny::observeEvent(input$anc_save, {
    if (!nrow(av$ties)) {
      shiny::showNotification("No tie points yet.", type = "warning")
      return()
    }
    nm <- paste0(rec_name(), "-ties.txt")
    write_series(av$ties, file.path(rv$folder, nm))
    ctx$say("Tie points written: ", nm)
    shiny::showNotification(paste("Written", nm), type = "message")
    ctx$refresh()
  })
  shiny::observeEvent(input$anc_age, {
    d <- record()
    if (is.null(d) || !nrow(av$ties)) {
      shiny::showNotification("Pick at least one tie point first.", type = "warning")
      return()
    }
    b <- rec_name()
    am <- data.frame(depth = d$x, age_ka = model(d$x))
    tuned <- data.frame(age_ka = am$age_ka, value = d$y)
    tuned <- tuned[order(tuned$age_ka), ]
    lab <- if (identical(input$anc_tgt, "metronome")) { paste0("metronome", cd_fmt(input$anc_mperiod)) } else {
      sub("|", "_", input$anc_tgt, fixed = TRUE) }
    n1 <- sprintf("%s-anchored_%s_age.txt", b, lab)
    n2 <- sprintf("%s-anchored_%s.txt", b, lab)
    n3 <- sprintf("%s-anchored_%s_ties.txt", b, lab)
    write_series(am, file.path(rv$folder, n1))
    write_series(tuned, file.path(rv$folder, n2))
    write_series(av$ties, file.path(rv$folder, n3))
    ties_code <- sprintf("ties <- data.frame(depth = %s,\n    age_ka = %s)", cd_dep(signif(av$ties$depth, 7)),
        cd_dep(signif(av$ties$time, 7)))
    ctx$add_code(c(ties_code, if (nrow(av$ties) >= 2) {
      c("# age model through the ties (CycloDesk extends it beyond the ties parallel to the starting model)",
          "tuned <- astrochron::tune(dat, controlPts = ties, extrapolate = TRUE, genplot = FALSE)")
    } else {
      "# one tie: the starting (floating) age model is shifted so that it passes through the tie"
    }, sprintf("# written by CycloDesk: %s, %s, %s", n1, n2, n3)), paste("Anchor to", input$anc_tgt))
    ctx$say("Anchored age model written: ", paste(c(n1, n2, n3), collapse = ", "))
    shiny::showNotification(paste("Written", n1, n2, n3, sep = "\n"), type = "message", duration = 8)
    ctx$refresh(select = n2)
  })
}
