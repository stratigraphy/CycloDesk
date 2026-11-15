# Tracking a cycle on the wavelet scalogram ####
#
# The Track tab: compute a CWT, click points on the ridge of a cycle (optionally snapped to the power maximum), or
# let WaverideR::track_ridge_hmm() follow the ridge with the clicks as a guide. The tracked period curve is then used
# to extract the cycle (WaverideR::extract_signal), to build an age model (WaverideR::curve2time, curve2sedrate) and to
# tune the series (astrochron::tune), as with WaverideR's track_period_wavelet() workflow.

cd_section <- function(title, ..., open = TRUE) {
  shiny::tags$details(class = "cd-sec", open = if (open) { NA } else { NULL },
      shiny::tags$summary(title), shiny::tags$div(class = "cd-secbody", ...))
}

cd_track_tab <- function() {
  shiny::tabPanel("Track", value = "track",
      shiny::fluidRow(
          shiny::column(4, class = "cd-toolform",
              shiny::tags$h4("Track a cycle on the scalogram"),
              shiny::tags$div(class = "cd-info", "Click points along the band of a cycle, give its duration, and build a",
                  " floating age model. The tuning check compares the amplitude envelope of a shorter cycle with a longer",
                  " cycle extracted directly from the record; move points and watch the check until you are happy."),
              shiny::tags$div(class = "cd-uses", "WaverideR::analyse_wavelet, track_ridge_hmm, extract_signal, curve2time"),
              cd_section("1. Scalogram",
                  shiny::uiOutput("trk_setup"),
                  shiny::selectInput("trk_palette", "Colour palette", cd_palettes()),
                  shiny::actionButton("trk_compute", "Show scalogram", icon = shiny::icon("braille"), class = "btn-primary")),
              cd_section("2. Track the cycle",
                  shiny::radioButtons("trk_mode", "A click on the scalogram", c(
                      "adds a point, snapped to the nearest power peak" = "peaks",
                      "adds a point exactly where you click (free tracking)" = "free",
                      "adds or removes a vertical boundary line" = "bound"), selected = "peaks"),
                  shiny::checkboxInput("trk_showpeaks", "Show the power peaks", TRUE),
                  shiny::conditionalPanel("input.trk_showpeaks",
                      shiny::sliderInput("trk_peakmin", "Peak filter (0: all peaks; to the right: only the strong ones)",
                          min = 0, max = 100, value = 50, step = 1, ticks = FALSE),
                      shiny::tags$div(class = "cd-hint", shiny::textOutput("trk_peakmin_txt", inline = TRUE))),
                  shiny::tags$div(class = "cd-hint", "Click a point to select it (Shift/Ctrl-click: several), click it again",
                      " to deselect; drag a point to move it; Delete or Backspace removes the selected points."),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("trk_undo", "Undo last point", icon = shiny::icon("rotate-left"), class = "btn-sm"),
                      shiny::actionButton("trk_delsel", "Delete selected", icon = shiny::icon("trash-can"), class = "btn-sm"),
                      shiny::actionButton("trk_selall", "Select all", icon = shiny::icon("object-group"), class = "btn-sm"),
                      shiny::actionButton("trk_selnone", "Deselect", icon = shiny::icon("object-ungroup"), class = "btn-sm"),
                      shiny::actionButton("trk_clear", "Clear all", icon = shiny::icon("eraser"), class = "btn-sm")),
                  shiny::numericInput("trk_tol", "Ridge search width around the clicks (fraction)", 0.25, min = 0.05, max = 1,
                      step = 0.05),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("trk_snapall", "Snap the whole curve to the nearest peaks",
                          icon = shiny::icon("magnet"), class = "btn-sm"),
                      shiny::actionButton("trk_hmm", "Follow the ridge (HMM)", icon = shiny::icon("route"), class = "btn-sm")),
                  shiny::tags$div(class = "cd-hint", "Snapping: at every depth the curve goes to the power peak nearest to the",
                      " line through your points (within the search width). HMM: WaverideR::track_ridge_hmm."),
                  shiny::sliderInput("trk_lowess", "Smooth the curve: LOWESS span (0 = off)", min = 0, max = 0.5, value = 0,
                      step = 0.01)),
              cd_section("3. Helper lines",
                  shiny::tags$div(class = "cd-hint", "Set up a group of lines and press Add; add as many groups as you",
                      " like. Tick groups in the list to remove them. On a sedimentation-rate map (eCOCO, eTimeOpt) only",
                      " fixed values and file columns are drawn, read as rates (cm/kyr; use multiplier 1 for a rate column)."),
                  shiny::selectInput("hl_type", "Lines", c(
                      "periods of cycles (kyr), scaled by the current track" = "cyc_track",
                      "periods of cycles (kyr), from an age-model file (depth, age)" = "cyc_file",
                      "fixed periods (depth units, e.g. m)" = "fixed",
                      "a column of a file (depth, value) x multiplier" = "column")),
                  shiny::conditionalPanel("input.hl_type == 'cyc_track' || input.hl_type == 'cyc_file'",
                      shiny::textInput("hl_cycles", "Cycles (kyr)", "", placeholder = "e.g. 405, 125, 95, 41, 21")),
                  shiny::conditionalPanel("input.hl_type == 'fixed'",
                      shiny::textInput("hl_fixed", "Periods (depth units)", "", placeholder = "e.g. 25, 32.5")),
                  shiny::conditionalPanel("input.hl_type == 'cyc_file' || input.hl_type == 'column'",
                      shiny::uiOutput("hl_file_ui")),
                  shiny::conditionalPanel("input.hl_type == 'column'",
                      shiny::uiOutput("hl_cols_ui"),
                      shiny::numericInput("hl_mult", "Multiplier (period = value x multiplier)", 1, step = 0.05),
                      shiny::tags$div(class = "cd-hint", "1 when the column is already a period in depth units; e.g. 4.05",
                          " for a sedimentation rate in cm/kyr gives the 405-kyr period in m (rate x 405 / 100).")),
                  shiny::fluidRow(
                      shiny::column(6, shiny::selectInput("hl_colour", "Colour", cd_helper_colours())),
                      shiny::column(6, shiny::selectInput("hl_lty", "Line", c("dashed" = "2", "solid" = "1", "dotted" = "3")))),
                  shiny::textInput("hl_label", "Label (optional)", ""),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("hl_add", "Add", icon = shiny::icon("plus"), class = "btn-sm"),
                      shiny::actionButton("hl_remove", "Remove ticked", icon = shiny::icon("minus"), class = "btn-sm"),
                      shiny::actionButton("hl_clear", "Remove all", icon = shiny::icon("eraser"), class = "btn-sm")),
                  shiny::uiOutput("hl_list_ui"),
                  shiny::tags$hr(),
                  shiny::textInput("trk_bounds", "Vertical boundary lines at depths", "", placeholder = "e.g. 160, 167.5"),
                  shiny::tags$div(class = "cd-hint", "Boundary lines can also be clicked on the scalogram (third click",
                      " option above); clicking a line again removes it."),
                  shiny::uiOutput("bd_file_ui"),
                  shiny::uiOutput("bd_col_ui"),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("bd_add", "Add depths from the file", icon = shiny::icon("file-import"),
                          class = "btn-sm"),
                      shiny::actionButton("bd_clear", "Clear boundary lines", icon = shiny::icon("eraser"), class = "btn-sm")),
                  open = FALSE),
              cd_section("4. Floating age model and extraction",
                  shiny::numericInput("trk_cycle", "Duration of the tracked cycle (kyr)", 405, min = 0),
                  shiny::numericInput("trk_excycle", "Cycle to extract (kyr; any cycle, scaled from the track)", 405, min = 0),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("trk_down", "Band: lower factor", 0.8, min = 0.1, step = 0.05)),
                      shiny::column(6, shiny::numericInput("trk_up", "upper factor", 1.2, min = 1, step = 0.05))),
                  shiny::checkboxInput("trk_addmean", "Add the mean to the extracted cycle", FALSE),
                  shiny::checkboxGroupInput("trk_show", "Show below the scalogram", c("extracted cycle" = "ext",
                      "age model + tuned series" = "age"), selected = c("ext", "age"), inline = TRUE),
                  shiny::tags$label("Results (written to the working folder)"),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("trk_save", "Save track", icon = shiny::icon("floppy-disk"), class = "btn-sm"),
                      shiny::actionButton("trk_extract", "Extract cycle", icon = shiny::icon("wave-square"), class = "btn-sm"),
                      shiny::actionButton("trk_age", "Age model + tuned series", icon = shiny::icon("clock"), class = "btn-sm"),
                      shiny::actionButton("trk_to_anchor", "Anchor to a solution...", icon = shiny::icon("anchor"),
                          class = "btn-sm"))),
              cd_section("5. Tuning check (amplitude modulation)",
                  shiny::tags$div(class = "cd-hint", "The modulated cycle is extracted with the track and its Hilbert",
                      " envelope is taken. The modulating cycle is then extracted from that envelope (amplitude modulation)",
                      " and, with the track, from the record itself; both are compared on the floating time scale.",
                      " A good age model gives a high correlation."),
                  shiny::numericInput("chk_mod", "Modulated cycle (kyr)", 21, min = 0),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("chk_mod_down", "Band: lower factor", 0.8, min = 0.1, step = 0.05)),
                      shiny::column(6, shiny::numericInput("chk_mod_up", "upper factor", 1.2, min = 1, step = 0.05))),
                  shiny::numericInput("chk_env", "Modulating cycle (kyr)", 100, min = 0),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("chk_env_down", "Band: lower factor", 0.8, min = 0.1, step = 0.05)),
                      shiny::column(6, shiny::numericInput("chk_env_up", "upper factor", 1.2, min = 1, step = 0.05))),
                  shiny::checkboxInput("chk_auto", "Update after every change of the track", TRUE),
                  shiny::checkboxInput("chk_show", "Show the tuning check", TRUE),
                  shiny::actionButton("chk_run", "Run the check", icon = shiny::icon("scale-balanced"), class = "btn-sm"),
                  open = FALSE),
              shiny::tags$br()),
          shiny::column(8,
              shiny::tags$div(class = "cd-trkstack",
                  shiny::plotOutput("trk_data", height = "120px"),
                  shiny::plotOutput("trk_plot", height = "500px", click = "trk_click"),
                  shiny::uiOutput("trk_ext_ui"),
                  shiny::uiOutput("trk_age_ui")),
              shiny::verbatimTextOutput("trk_text"),
              shiny::uiOutput("trk_check_ui"))))
}

# Display version of a scalogram: thinned positions, power as quantiles, log2 period axis
cd_track_display <- function(w, max_cols = 1500) {
  n <- ncol(w$Power)
  idx <- seq(1, n, by = max(1, ceiling(n / max_cols)))
  z <- w$Power[, idx, drop = FALSE]
  q <- rank(z, ties.method = "average") / length(z)
  dim(q) <- dim(z)
  list(x = w$axis.1[idx], lp = log2(w$Period), z = t(q), coi_x = w$coi.1, coi_y = w$coi.2,
      dx = w$x[idx], dy = w$y[idx])
}

# Data panel above the scalogram (a separate plot: clicks on the scalogram must map to a single panel)
cd_plot_track_data <- function(disp, bounds = NULL) {
  # on a short device the default margins give an invalid plot size, which par() refuses to restore
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(0, 4.8, 0.6, 1), mgp = c(2.4, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  graphics::plot(disp$dx, disp$dy, type = "l", col = "#1f4e79", xaxs = "i", xaxt = "n", xlab = "", ylab = "Data",
      xlim = range(disp$x))
  if (length(bounds)) {
    graphics::abline(v = bounds, col = "#d97706", lwd = 1.5, lty = 2)
  }
}

cd_plot_track <- function(disp, pts = NULL, curve = NULL, band = NULL, xlab = "Depth", ylab = "Period",
    palette = "viridis|viridis", peaks = NULL, helper = NULL, bounds = NULL, raw = NULL) {
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(4, 4.8, 0, 1), mgp = c(2.6, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  graphics::image(disp$x, disp$lp, disp$z, col = cd_palette(100, palette), xlab = xlab, ylab = ylab, yaxt = "n", useRaster = TRUE)
  tk <- seq(ceiling(min(disp$lp)), floor(max(disp$lp)))
  graphics::axis(2, at = tk, labels = cd_fmt_period(2^tk))
  if (length(disp$coi_x)) {
    graphics::polygon(c(disp$coi_x, rev(disp$coi_x)), c(disp$coi_y, rep(max(disp$lp) + 10, length(disp$coi_y))),
        col = grDevices::adjustcolor("white", 0.45), border = NA)
  }
  if (!is.null(band)) {
    graphics::lines(band[, 1], log2(band[, 2]), col = "white", lty = 2)
    graphics::lines(band[, 1], log2(band[, 3]), col = "white", lty = 2)
  }
  if (length(helper)) {
    for (h in helper) {
      ok <- is.finite(h$period) & h$period > 0
      if (!any(ok)) {
        next
      }
      hcol <- if (is.null(h$colour)) { "white" } else { h$colour }
      hlty <- if (is.null(h$lty)) { 2 } else { h$lty }
      y <- log2(h$period)
      y[!ok] <- NA
      graphics::lines(h$depth, y, col = hcol, lty = hlty, lwd = 1.5)
      k <- max(which(ok))
      graphics::text(h$depth[k], y[k], h$label, pos = 2, col = hcol, cex = 0.75, font = 2)
    }
  }
  if (length(bounds)) {
    graphics::abline(v = bounds, col = "black", lwd = 3)
    graphics::abline(v = bounds, col = "#ffb02e", lwd = 1.5, lty = 2)
  }
  if (!is.null(raw)) {
    graphics::lines(raw[, 1], log2(raw[, 2]), col = grDevices::adjustcolor("white", 0.7), lwd = 1)
  }
  if (!is.null(curve)) {
    graphics::lines(curve[, 1], log2(curve[, 2]), col = "black", lwd = 4)
    graphics::lines(curve[, 1], log2(curve[, 2]), col = "white", lwd = 2)
  }
  if (!is.null(peaks) && nrow(peaks)) {
    graphics::points(peaks$depth, log2(peaks$period), pch = 16, cex = 0.35, col = grDevices::adjustcolor("white", 0.85))
  }
  if (!is.null(pts) && nrow(pts)) {
    graphics::points(pts$depth, log2(pts$period), pch = 21, bg = "#ff6b4a", col = "white", cex = 1.4, lwd = 1.5)
  }
  graphics::box()
  # where the scalogram lies on the device, read before on.exit() resets the margins
  invisible(list(usr = graphics::par("usr"), plt = graphics::par("plt"), fig = graphics::par("fig")))
}

cd_track_server <- function(input, output, session, ctx) {
  rv <- ctx$rv
  trk <- new.env()
  tv <- shiny::reactiveValues(ver = 0, pts = data.frame(id = integer(), depth = numeric(), period = numeric()), sel = integer(), nextid = 1L, hmm = NULL,
      file = NULL, par = NULL, kind = "cwt", ylab = "Period", msg = "Choose a data file and compute its scalogram.")

  # Scalograms that can be reused: the result in the Tool tab and the saved wavelet results in the folder
  sources <- shiny::reactive({
    rv$tick
    ch <- c("Compute a new CWT" = "new")
    res <- rv$tool_res
    if (!is.null(res) && !is.null(cd_track_kind(res))) {
      ch <- c(stats::setNames("tool", sprintf("Result in the Tool tab (%s, %s)", res$file, cd_get_tool(res$tool)$label)), ch)
    }
    rds <- rv$listing$files[grepl("\\.(cyclodesk|rcycle)\\.rds$", rv$listing$files)]
    if (length(rds)) {
      ch <- c(ch, stats::setNames(paste0("rds|", rds), paste("Saved:", rds)))
    }
    ch
  })

  output$trk_setup <- shiny::renderUI({
    fs <- ctx$data_files()
    sel <- shiny::isolate(ctx$selected_files())
    s1 <- if (length(sel)) { sel[1] } else { fs[1] }
    d <- if (length(s1) && !is.na(s1)) { try(ctx$read_file(s1), silent = TRUE) } else { NULL }
    lo <- 1
    hi <- 100
    dj <- 0.02
    if (!is.null(d) && !inherits(d, "try-error")) {
      lo <- signif(3 * cd_dt(d[, 1]), 3)
      hi <- signif(diff(range(d[, 1])) * 0.33, 3)
      dj <- cd_dj_default(d)
    }
    src <- sources()
    pick <- if ("tool" %in% src) { "tool" } else { "new" }
    if (pick == "new" && length(s1) && !is.na(s1)) {
      mine <- src[startsWith(src, paste0("rds|", tools::file_path_sans_ext(s1), "-wavelet"))]
      if (length(mine)) {
        pick <- mine[1]
      }
    }
    shiny::tagList(
        shiny::selectInput("trk_source", "Scalogram or spectrum map (wavelet, EHA, eTimeOpt, eCOCO, ...)", choices = src,
            selected = pick),
        shiny::conditionalPanel("input.trk_source == 'new'",
            shiny::selectInput("trk_file", "Data file", choices = fs, selected = s1),
            shiny::fluidRow(
                shiny::column(6, shiny::numericInput("trk_lo", "Shortest period", lo, min = 0)),
                shiny::column(6, shiny::numericInput("trk_hi", "Longest period", hi, min = 0))),
            shiny::fluidRow(
                shiny::column(6, shiny::numericInput("trk_omega", "omega_nr", 8, min = 1)),
                shiny::column(6, shiny::numericInput("trk_dj", "dj", dj, min = 0.001, step = 0.005)))))
  })

  # Put a scalogram into the Track tab. w can be a full or a slimmed (saved) result; the full CWT that
  # extract_signal() needs is recomputed with the same settings only when it is missing
  load_scalogram <- function(w, f, par, d, how) {
    s <- cd_even(d)
    names(s) <- names(d)[1:2]
    trk$w_orig <- w
    trk$w <- cd_wt_std(w)
    trk$full <- if (!is.null(w$Wave) && !isTRUE(w$slim) && identical(par$method, "cwt")) { w } else { NULL }
    xcache$prep <- NULL
    trk$s <- s
    trk$disp <- cd_track_display(trk$w)
    # the data panel: from the data when the positions of the map are not the data samples (EHA, eCOCO, ...)
    if (length(w$x) != length(trk$w$axis.1) || length(trk$disp$dx) != length(trk$disp$x)) {
      trk$disp$dx <- s[, 1]
      trk$disp$dy <- s[, 2]
    }
    tv$kind <- if (is.null(par$kind)) { "cwt" } else { par$kind }
    tv$ylab <- if (identical(tv$kind, "sedrate")) { "Sed. rate (cm/kyr)" } else { "Period" }
    tv$file <- f
    tv$par <- par
    tv$pts <- data.frame(id = integer(), depth = numeric(), period = numeric())
    tv$sel <- integer()
    tv$hmm <- NULL
    tv$snap <- NULL
    tv$ver <- tv$ver + 1
    tv$msg <- if (identical(tv$kind, "sedrate")) {
      sprintf(paste("%s (%s): sedimentation rates %s to %s cm/kyr. Click along the ridge of the best rate; the track is",
          "a sedimentation-rate curve (the cycle duration below is then only used to scale extracted cycles)."), f, how,
          cd_fmt(min(trk$w$Period)), cd_fmt(max(trk$w$Period)))
    } else {
      sprintf("%s (%s): %d periods from %s to %s. Click along the band of the cycle.", f, how,
          length(trk$w$Period), cd_fmt(min(trk$w$Period)), cd_fmt(max(trk$w$Period)))
    }
    ctx$say("Track: scalogram of ", f, " (", how, ")")
  }

  # A result of the Wavelet tool (list with result, params, file) into the Track tab
  load_result <- function(res, how) {
    p <- res$params
    f <- res$file
    d <- NULL
    if (!is.null(f) && file.exists(file.path(rv$folder, f))) {
      d <- try(ctx$read_file(f), silent = TRUE)
      if (inherits(d, "try-error")) {
        d <- NULL
      }
    }
    w <- res$result
    kind <- cd_track_kind(res)
    if (is.null(kind)) {
      shiny::showNotification("This result has no scalogram or spectrum map to track on.", type = "error")
      return(invisible(FALSE))
    }
    if (!identical(kind, "cwt")) {
      if (is.null(d)) {
        shiny::showNotification(paste("The data file", f, "is not in this folder."), type = "error")
        return(invisible(FALSE))
      }
      m <- try(cd_track_map(res), silent = TRUE)
      if (inherits(m, "try-error") || is.null(m)) {
        shiny::showNotification("The map of this result could not be read for tracking.", type = "error")
        return(invisible(FALSE))
      }
      m$x <- d[, 1]
      m$y <- d[, 2]
      par <- list(lo = signif(3 * cd_dt(d[, 1]), 3), hi = signif(diff(range(d[, 1])) * 0.33, 3), omega = 8,
          dj = cd_dj_default(d), wavelet = "morlet", method = "cwt", kind = kind, source = res$tool)
      load_scalogram(m, f, par, d, how)
      return(invisible(TRUE))
    }
    if (is.null(d)) {
      if (is.null(w$x) || is.null(w$y)) {
        shiny::showNotification(paste("The data file", f, "is not in this folder."), type = "error")
        return(invisible(FALSE))
      }
      d <- data.frame(x = w$x, y = w$y)
      shiny::showNotification(paste("The data file", f, "is not in this folder: the thinned data stored with the result",
          "are used."), type = "warning", duration = 8)
    }
    par <- list(lo = p$lowerPeriod, hi = p$upperPeriod, omega = if (is.null(p$omega_nr)) { 8 } else { p$omega_nr },
        dj = if (is.null(p$dj)) { cd_dj_default(d) } else { p$dj },
        wavelet = if (is.null(p$wavelet) || identical(p$method, "superlet")) { "morlet" } else { p$wavelet },
        method = if (is.null(p$method)) { "cwt" } else { p$method })
    load_scalogram(w, f, par, d, how)
    invisible(TRUE)
  }

  shiny::observeEvent(input$trk_compute, {
    src <- input$trk_source
    if (identical(src, "tool")) {
      load_result(rv$tool_res, "the result in the Tool tab")
      return()
    }
    if (!is.null(src) && startsWith(src, "rds|")) {
      x <- try(readRDS(file.path(rv$folder, substring(src, 5))), silent = TRUE)
      if (inherits(x, "try-error") || !isTRUE(x$cyclodesk)) {
        shiny::showNotification("Not an CycloDesk result file.", type = "error")
        return()
      }
      load_result(x$result, substring(src, 5))
      return()
    }
    f <- input$trk_file
    if (is.null(f) || !nzchar(f)) {
      shiny::showNotification("Choose a data file.", type = "warning")
      return()
    }
    d <- try(ctx$read_file(f), silent = TRUE)
    if (inherits(d, "try-error")) {
      shiny::showNotification("The file could not be read as a series.", type = "error")
      return()
    }
    s <- cd_even(d)
    par <- list(lo = input$trk_lo, hi = input$trk_hi, omega = input$trk_omega, dj = input$trk_dj, wavelet = "morlet",
        method = "cwt")
    w <- shiny::withProgress(message = "Computing the scalogram ...", value = 0.5, {
      try(WaverideR::analyse_wavelet(data = as.matrix(s), dj = par$dj, lowerPeriod = par$lo, upperPeriod = par$hi,
          omega_nr = par$omega), silent = TRUE)
    })
    if (inherits(w, "try-error")) {
      shiny::showNotification(conditionMessage(attr(w, "condition")), type = "error", duration = 8)
      return()
    }
    load_scalogram(w, f, par, d, "new CWT")
  })

  # "Track a cycle on this scalogram" button in the Tool tab
  shiny::observeEvent(input$tool_to_track, {
    if (!is.null(rv$tool_res) && !is.null(cd_track_kind(rv$tool_res))) {
      if (isTRUE(load_result(rv$tool_res, "the result in the Tool tab"))) {
        shiny::updateTabsetPanel(session, "tabs", selected = "track")
      }
    }
  })

  # Full CWT for extract_signal(), recomputed from the settings when only a slimmed result is loaded
  # The reconstruction matrix of the full CWT, kept for fast extractions (cd_extract)
  xcache <- new.env(parent = emptyenv())
  full_prep <- function() {
    if (is.null(xcache$prep)) {
      xcache$prep <- cd_extract_prep(full_cwt())
    }
    xcache$prep
  }
  full_cwt <- function() {
    if (!is.null(trk$full)) {
      return(trk$full)
    }
    par <- tv$par
    w <- shiny::withProgress(message = "Recomputing the full CWT for the extraction ...", value = 0.5, {
      try(WaverideR::analyse_wavelet(data = as.matrix(trk$s), dj = par$dj, lowerPeriod = par$lo, upperPeriod = par$hi,
          omega_nr = par$omega, wavelet = par$wavelet), silent = TRUE)
    })
    if (inherits(w, "try-error")) {
      stop(conditionMessage(attr(w, "condition")), call. = FALSE)
    }
    trk$full <- w
    xcache$prep <- NULL
    w
  }


  shiny::observeEvent(input$trk_click, {
    if (is.null(trk$w)) {
      return()
    }
    cl <- input$trk_click
    lp <- trk$disp$lp
    if (is.null(cl$x) || is.null(cl$y) || cl$y < min(lp) || cl$y > max(lp)) {
      return()
    }
    if (identical(input$trk_mode, "bound")) {
      b <- bounds()
      tol <- 0.01 * diff(range(trk$w$axis.1))
      near <- which(abs(b - cl$x) < tol)
      b <- if (length(near)) { b[-near] } else { sort(c(b, signif(cl$x, 6))) }
      shiny::updateTextInput(session, "trk_bounds", value = cd_fmt_depths(b))
      return()
    }
    xy <- snap_point(cl$x, 2^cl$y)
    id <- tv$nextid
    tv$nextid <- id + 1L
    pts <- rbind(tv$pts, data.frame(id = id, depth = xy[1], period = xy[2]))
    tv$pts <- pts[order(pts$depth), ]
    tv$sel <- integer()
    tv$hmm <- NULL
    tv$snap <- NULL
  })
  # In snapping mode a point goes to the nearest local maximum of power along the period axis, as in
  # track_period_wavelet(track_peaks = TRUE); in free mode it stays where it was put
  snap_point <- function(x, p) {
    if (identical(input$trk_mode, "peaks")) {
      w <- trk$w
      j <- which.min(abs(w$axis.1 - x))
      pk <- cd_col_peaks(w$Power[, j])
      if (length(pk)) {
        k <- pk[which.min(abs(log2(w$Period[pk]) - log2(p)))]
        p <- w$Period[k]
        x <- w$axis.1[j]
      }
    }
    c(x, p)
  }
  shiny::observeEvent(input$trk_undo, {
    if (nrow(tv$pts)) {
      last <- max(tv$pts$id)
      tv$pts <- tv$pts[tv$pts$id != last, , drop = FALSE]
      tv$sel <- setdiff(tv$sel, last)
      tv$hmm <- NULL
      tv$snap <- NULL
    }
  })
  shiny::observeEvent(input$trk_clear, {
    tv$pts <- data.frame(id = integer(), depth = numeric(), period = numeric())
    tv$sel <- integer()
    tv$hmm <- NULL
    tv$snap <- NULL
  })
  delete_selected <- function() {
    if (length(tv$sel)) {
      tv$pts <- tv$pts[!(tv$pts$id %in% tv$sel), , drop = FALSE]
      tv$sel <- integer()
      tv$hmm <- NULL
      tv$snap <- NULL
    }
  }
  shiny::observeEvent(input$trk_delsel, {
    delete_selected()
  })
  shiny::observeEvent(input$trk_delkey, {
    delete_selected()
  })
  shiny::observeEvent(input$trk_selall, {
    tv$sel <- tv$pts$id
  })
  shiny::observeEvent(input$trk_selnone, {
    tv$sel <- integer()
  })
  # Click on a point: select it (Shift/Ctrl: add to or remove from the selection); click a selected point to deselect
  shiny::observeEvent(input$trk_pt_click, {
    m <- input$trk_pt_click
    id <- as.integer(m$id)
    if (isTRUE(m$add)) {
      tv$sel <- if (id %in% tv$sel) { setdiff(tv$sel, id) } else { c(tv$sel, id) }
    } else if (identical(tv$sel, id)) {
      tv$sel <- integer()
    } else {
      tv$sel <- id
    }
  })
  # A dragged point: its new place (snapped in snapping mode)
  shiny::observeEvent(input$trk_pt_move, {
    m <- input$trk_pt_move
    id <- as.integer(m$id)
    k <- which(tv$pts$id == id)
    if (!length(k) || is.null(trk$w)) {
      return()
    }
    lp <- trk$disp$lp
    y <- min(max(as.numeric(m$y), min(lp)), max(lp))
    xr <- range(trk$w$axis.1)
    x <- min(max(as.numeric(m$x), xr[1]), xr[2])
    xy <- snap_point(x, 2^y)
    pts <- tv$pts
    pts$depth[k] <- xy[1]
    pts$period[k] <- xy[2]
    tv$pts <- pts[order(pts$depth), ]
    tv$sel <- id
    tv$hmm <- NULL
    tv$snap <- NULL
  })
  # Points and selection to the browser, which draws them as draggable markers
  shiny::observe({
    pts <- tv$pts
    sel <- tv$sel
    out <- vector("list", nrow(pts))
    for (k in seq_len(nrow(pts))) {
      out[[k]] <- list(id = pts$id[k], x = pts$depth[k], y = log2(pts$period[k]), sel = pts$id[k] %in% sel)
    }
    session$sendCustomMessage("trk_points", list(pts = out))
  })

  shiny::observeEvent(input$trk_hmm, {
    if (is.null(trk$w)) {
      shiny::showNotification("Compute the scalogram first.", type = "warning")
      return()
    }
    pts <- tv$pts
    if (nrow(pts) < 1) {
      shiny::showNotification("Click at least one point on the band of the cycle first.", type = "warning")
      return()
    }
    tol <- input$trk_tol
    h <- shiny::withProgress(message = "Following the ridge ...", value = 0.5, {
      wh <- if (!identical(tv$kind, "cwt")) {
        trk$w_orig
      } else if (identical(tv$par$method, "superlet")) {
        try(full_cwt(), silent = TRUE)
      } else {
        trk$w_orig
      }
      if (inherits(wh, "try-error")) {
        wh
      } else {
        # on an EHA, eTimeOpt or eCOCO map only that map is the evidence (no CWT of the data mixed in)
        fw <- if (identical(tv$kind, "cwt")) { 0.2 } else { 0 }
        if (nrow(pts) >= 2) {
          try(WaverideR::track_ridge_hmm(wh, guide = pts[, c("depth", "period")], guide_tolerance = tol,
              fallback_weight = fw), silent = TRUE)
        } else {
          try(WaverideR::track_ridge_hmm(wh, period = pts$period[1] * c(1 - tol, 1 + tol), fallback_weight = fw),
              silent = TRUE)
        }
      }
    })
    if (inherits(h, "try-error")) {
      shiny::showNotification(conditionMessage(attr(h, "condition")), type = "error", duration = 8)
      return()
    }
    tv$hmm <- as.data.frame(h)
    tv$snap <- NULL
    ctx$say("Track: ridge followed with track_ridge_hmm")
  })

  # The tracked period at every position of the scalogram
  # The curve through the points, or the snapped / HMM curve
  raw_curve <- shiny::reactive({
    tv$ver
    if (is.null(trk$w)) {
      return(NULL)
    }
    if (!is.null(tv$hmm)) {
      return(data.frame(depth = tv$hmm$depth, period = tv$hmm$period))
    }
    if (!is.null(tv$snap)) {
      return(tv$snap)
    }
    pts <- tv$pts
    if (nrow(pts) < 2) {
      return(NULL)
    }
    x <- trk$w$axis.1
    data.frame(depth = x, period = stats::approx(pts$depth, pts$period, xout = x, rule = 2, ties = mean)$y)
  })
  # ... optionally smoothed with LOWESS to take out jumps; this is the curve that is used
  map_curve <- shiny::reactive({
    cv <- raw_curve()
    span <- input$trk_lowess
    if (is.null(cv) || is.null(span) || !is.finite(span) || span <= 0) {
      return(cv)
    }
    data.frame(depth = cv$depth, period = stats::lowess(cv$depth, cv$period, f = span)$y)
  })
  # ... as the period of the tracked cycle: a sedimentation-rate track (cm/kyr, depth in m) becomes the period of a
  # cycle of the given duration, so the age model, extraction and tuning check work as for a period track
  curve <- shiny::reactive({
    cv <- map_curve()
    if (is.null(cv) || !identical(tv$kind, "sedrate")) {
      return(cv)
    }
    cyc <- input$trk_cycle
    if (!is.finite(cyc) || cyc <= 0) {
      return(NULL)
    }
    data.frame(depth = cv$depth, period = cv$period / 100 * cyc)
  })

  # Snap the whole curve to the nearest power peak at every depth
  shiny::observeEvent(input$trk_snapall, {
    if (is.null(trk$w)) {
      shiny::showNotification("Show a scalogram first.", type = "warning")
      return()
    }
    pts <- tv$pts
    if (nrow(pts) < 1) {
      shiny::showNotification("Click at least one point on the band of the cycle first.", type = "warning")
      return()
    }
    tv$snap <- cd_snap_track(trk$w, pts[, c("depth", "period")], tol = input$trk_tol)
    tv$hmm <- NULL
    ctx$say("Track: curve snapped to the nearest power peaks")
  })

  output$trk_plot <- shiny::renderPlot({
    tv$ver
    if (is.null(trk$disp)) {
      cd_message_plot("Compute a scalogram to start tracking")
      return(invisible(NULL))
    }
    cv <- map_curve()
    band <- NULL
    if (!is.null(tv$hmm) && !is.null(tv$hmm$period_lower)) {
      band <- tv$hmm[, c("depth", "period_lower", "period_upper")]
    }
    pk <- NULL
    if (isTRUE(input$trk_showpeaks)) {
      pk <- cd_track_peaks(trk$w, frac = cd_peak_frac(input$trk_peakmin))
    }
    g <- cd_plot_track(trk$disp, pts = NULL, curve = cv, band = band, xlab = names(trk$s)[1], ylab = tv$ylab,
        palette = input$trk_palette, peaks = pk,
        helper = cd_helper_for_kind(helper_lines(), tv$kind), bounds = bounds(),
        raw = if (is.finite(input$trk_lowess) && input$trk_lowess > 0) { raw_curve() } else { NULL })
    session$sendCustomMessage("trk_geom", g)
  }, res = 96)
  output$trk_data <- shiny::renderPlot({
    tv$ver
    if (is.null(trk$disp)) {
      return(NULL)
    }
    cd_plot_track_data(trk$disp, bounds = bounds())
  }, res = 96)

  output$trk_text <- shiny::renderText({
    cv <- curve()
    txt <- tv$msg
    if (nrow(tv$pts)) {
      txt <- c(txt, sprintf("%d points%s%s%s.", nrow(tv$pts), if (length(tv$sel)) { sprintf(", %d selected", length(tv$sel)) } else { "" },
          if (!is.null(tv$hmm)) { "; ridge followed (HMM)" } else if (!is.null(tv$snap)) { "; curve snapped to the nearest peaks" } else { "" },
          if (is.finite(input$trk_lowess) && input$trk_lowess > 0) { sprintf("; smoothed with LOWESS (span %s)", cd_fmt(input$trk_lowess)) } else { "" }))
    }
    mc <- map_curve()
    if (identical(tv$kind, "sedrate") && !is.null(mc)) {
      txt <- c(txt, sprintf("Tracked sedimentation rate %s to %s cm/kyr: %s kyr in total.", cd_fmt(min(mc$period)),
          cd_fmt(max(mc$period)), cd_fmt(sum(diff(mc$depth) / (mc$period[-1] / 100)))))
    } else if (!is.null(cv) && is.finite(input$trk_cycle) && input$trk_cycle > 0) {
      sr <- 100 * cv$period / input$trk_cycle
      txt <- c(txt, sprintf("Tracked period %s to %s; for a %s-kyr cycle: sed. rate %s to %s cm/kyr, %s kyr in total.",
          cd_fmt(min(cv$period)), cd_fmt(max(cv$period)), cd_fmt(input$trk_cycle), cd_fmt(min(sr)), cd_fmt(max(sr)),
          cd_fmt(sum(diff(cv$depth) / (cv$period[-1] / input$trk_cycle)))))
    }
    if (length(hl()) && !is.null(trk$w)) {
      txt <- c(txt, cd_helper_status(cd_helper_for_kind(helper_lines(), tv$kind), range(trk$w$Period), length(hl())))
    }
    paste(txt, collapse = "\n")
  })

  # The extracted cycle, shown below the scalogram on the same depth axis
  ext_res <- shiny::reactiveVal(NULL)
  shiny::observeEvent(tv$ver, {
    ext_res(NULL)
  })
  output$trk_ext_ui <- shiny::renderUI({
    if (is.null(ext_res()) || !("ext" %in% input$trk_show)) {
      return(NULL)
    }
    shiny::plotOutput("trk_ext_plot", height = "170px")
  })
  output$trk_ext_plot <- shiny::renderPlot({
    e <- ext_res()
    if (is.null(e) || is.null(trk$disp)) {
      return(NULL)
    }
    cd_plot_track_extract(trk$disp, trk$s, e$ex, e$label, bounds = bounds())
  }, res = 96)

  # The floating age model (same depth axis) and the tuned series, below the extracted cycle
  age_res <- shiny::reactiveVal(NULL)
  shiny::observeEvent(tv$ver, {
    age_res(NULL)
  })
  output$trk_age_ui <- shiny::renderUI({
    if (is.null(age_res()) || !("age" %in% input$trk_show)) {
      return(NULL)
    }
    shiny::plotOutput("trk_age_plot", height = "360px")
  })
  output$trk_age_plot <- shiny::renderPlot({
    a <- age_res()
    if (is.null(a) || is.null(trk$disp)) {
      return(NULL)
    }
    cd_plot_track_age(trk$disp, trk$s, a$am, a$tuned, a$cyc, bounds = bounds(), ex = a$ex, exc = a$exc)
  }, res = 96)

  output$trk_peakmin_txt <- shiny::renderText({
    fr <- cd_peak_frac(input$trk_peakmin)
    if (fr <= 0) {
      return("All local power maxima are shown.")
    }
    sprintf("Peaks with at least %s%% of the power of the strongest peak at that position.", cd_fmt(signif(100 * fr, 2)))
  })
  show_part <- function(part) {
    if (!(part %in% input$trk_show)) {
      shiny::updateCheckboxGroupInput(session, "trk_show", selected = c(input$trk_show, part))
    }
  }

  # Columns of a file (all numeric columns), for the file pickers of the helper and boundary lines
  file_columns <- function(f) {
    if (is.null(f) || !nzchar(f)) {
      return(character())
    }
    d <- try(ctx$read_file(f, full = TRUE), silent = TRUE)
    if (inherits(d, "try-error")) {
      return(character())
    }
    names(d)
  }
  column_choices <- function(nm) {
    stats::setNames(as.character(seq_along(nm)), sprintf("%d: %s", seq_along(nm), nm))
  }

  # Helper lines: groups of lines added with Add and removed with Remove
  hl <- shiny::reactiveVal(list())
  hl_counter <- new.env(parent = emptyenv())
  hl_counter$n <- 0L
  output$hl_file_ui <- shiny::renderUI({
    shiny::selectInput("hl_file", "File", choices = ctx$data_files())
  })
  output$hl_cols_ui <- shiny::renderUI({
    nm <- file_columns(input$hl_file)
    if (!length(nm)) {
      return(shiny::tags$div(class = "cd-hint", "Choose a readable file with at least two numeric columns."))
    }
    ch <- column_choices(nm)
    shiny::fluidRow(
        shiny::column(6, shiny::selectInput("hl_xcol", "Depth column", ch, selected = ch[1])),
        shiny::column(6, shiny::selectInput("hl_ycol", "Value column", ch, selected = ch[min(2, length(ch))])))
  })
  shiny::observeEvent(input$hl_add, {
    type <- input$hl_type
    spec <- list(type = type, colour = input$hl_colour, lty = as.integer(input$hl_lty), label = trimws(input$hl_label))
    if (type %in% c("cyc_track", "cyc_file")) {
      spec$values <- cd_parse_nums(input$hl_cycles)
    } else if (type == "fixed") {
      spec$values <- cd_parse_nums(input$hl_fixed)
    }
    if (type %in% c("cyc_file", "column")) {
      spec$file <- input$hl_file
      if (is.null(spec$file) || !nzchar(spec$file)) {
        shiny::showNotification("Choose a file.", type = "warning")
        return()
      }
    }
    if (type == "column") {
      spec$xcol <- as.integer(input$hl_xcol)
      spec$ycol <- as.integer(input$hl_ycol)
      spec$mult <- input$hl_mult
      if (!length(spec$xcol) || !length(spec$ycol) || !is.finite(spec$mult) || spec$mult == 0) {
        shiny::showNotification("Choose the depth and value columns and a multiplier other than 0.", type = "warning")
        return()
      }
      spec$colname <- file_columns(spec$file)[spec$ycol]
    } else if (!length(spec$values)) {
      shiny::showNotification("Give at least one value.", type = "warning")
      return()
    }
    hl_counter$n <- hl_counter$n + 1L
    spec$id <- hl_counter$n
    spec$text <- cd_helper_describe(spec)
    hl(c(hl(), list(spec)))
    if (is.null(trk$w)) {
      shiny::showNotification("Helper lines added; they are drawn once a scalogram is shown.", type = "message")
    } else {
      d <- NULL
      if (type %in% c("cyc_file", "column")) {
        d <- try(ctx$read_file(spec$file, full = TRUE), silent = TRUE)
        if (inherits(d, "try-error")) {
          d <- NULL
        }
      }
      h <- cd_helper_lines(spec, trk$w$axis.1, track = curve(), track_cycle = input$trk_cycle, data = d)
      shiny::showNotification(cd_helper_status(cd_helper_for_kind(h, tv$kind), range(trk$w$Period), 1L), type = "message",
          duration = 8)
    }
  })
  output$hl_list_ui <- shiny::renderUI({
    s <- hl()
    if (!length(s)) {
      return(shiny::tags$div(class = "cd-hint", "No helper lines yet."))
    }
    ids <- character(length(s))
    txt <- character(length(s))
    for (k in seq_along(s)) {
      ids[k] <- as.character(s[[k]]$id)
      txt[k] <- s[[k]]$text
    }
    shiny::checkboxGroupInput("hl_pick", "Helper lines on the scalogram", choiceNames = txt, choiceValues = ids)
  })
  shiny::observeEvent(input$hl_remove, {
    pick <- input$hl_pick
    if (!length(pick)) {
      shiny::showNotification("Tick the groups to remove first.", type = "warning")
      return()
    }
    s <- hl()
    keep <- logical(length(s))
    for (k in seq_along(s)) {
      keep[k] <- !(as.character(s[[k]]$id) %in% pick)
    }
    hl(s[keep])
  })
  shiny::observeEvent(input$hl_clear, {
    hl(list())
  })

  # Boundary lines (depths), typed, clicked or read from a column of a file
  bounds <- shiny::reactive({
    b <- cd_parse_nums(input$trk_bounds)
    if (is.null(b)) { numeric() } else { sort(b) }
  })
  output$bd_file_ui <- shiny::renderUI({
    shiny::selectInput("bd_file", "Depths from a file", choices = c("(choose a file)" = "", ctx$data_files()))
  })
  output$bd_col_ui <- shiny::renderUI({
    nm <- file_columns(input$bd_file)
    if (!length(nm)) {
      return(NULL)
    }
    ch <- column_choices(nm)
    shiny::selectInput("bd_col", "Column with the depths", ch, selected = ch[1])
  })
  shiny::observeEvent(input$bd_add, {
    f <- input$bd_file
    if (is.null(f) || !nzchar(f) || is.null(input$bd_col)) {
      shiny::showNotification("Choose a file and the column with the depths.", type = "warning")
      return()
    }
    d <- try(ctx$read_file(f, full = TRUE), silent = TRUE)
    j <- as.integer(input$bd_col)
    if (inherits(d, "try-error") || j > ncol(d)) {
      shiny::showNotification("The file could not be read.", type = "error")
      return()
    }
    v <- d[[j]]
    v <- v[is.finite(v)]
    b <- sort(unique(signif(c(bounds(), v), 6)))
    shiny::updateTextInput(session, "trk_bounds", value = cd_fmt_depths(b))
    shiny::showNotification(sprintf("%d boundary depths added from %s.", length(v), f), type = "message")
  })
  shiny::observeEvent(input$bd_clear, {
    shiny::updateTextInput(session, "trk_bounds", value = "")
  })

  # The helper lines as curves of period (depth units) against depth
  helper_lines <- shiny::reactive({
    s <- hl()
    if (!length(s) || is.null(trk$w)) {
      return(NULL)
    }
    x <- trk$w$axis.1
    cv <- curve()
    out <- list()
    for (k in seq_along(s)) {
      d <- NULL
      if (s[[k]]$type %in% c("cyc_file", "column")) {
        d <- try(ctx$read_file(s[[k]]$file, full = TRUE), silent = TRUE)
        if (inherits(d, "try-error")) {
          next
        }
      }
      out <- c(out, cd_helper_lines(s[[k]], x, track = cv, track_cycle = input$trk_cycle, data = d))
    }
    out
  })

  # Tuning check: Hilbert envelope of the modulated cycle against the modulating cycle, both extracted with the track
  check_trigger <- shiny::reactive({
    list(curve(), input$chk_mod, input$chk_env, input$trk_cycle, input$chk_mod_down, input$chk_mod_up, input$chk_env_down,
        input$chk_env_up)
  })
  check_debounced <- shiny::debounce(check_trigger, 800)
  check_res <- shiny::reactiveVal(NULL)
  run_check <- function() {
    cv <- curve()
    if (is.null(cv) || is.null(trk$w)) {
      check_res(NULL)
      return()
    }
    r <- shiny::withProgress(message = "Tuning check ...", value = 0.5, {
      try(cd_track_check(full_prep(), cv, input$trk_cycle, input$chk_mod, input$chk_env, mod_down = input$chk_mod_down,
          mod_up = input$chk_mod_up, env_down = input$chk_env_down, env_up = input$chk_env_up), silent = TRUE)
    })
    if (inherits(r, "try-error")) {
      check_res(list(error = conditionMessage(attr(r, "condition"))))
    } else {
      check_res(r)
    }
  }
  shiny::observeEvent(input$chk_run, {
    if (!isTRUE(input$chk_show)) {
      shiny::updateCheckboxInput(session, "chk_show", value = TRUE)
    }
    run_check()
  })
  shiny::observeEvent(check_debounced(), {
    if (isTRUE(input$chk_auto) && !is.null(check_res())) {
      run_check()
    }
  }, ignoreInit = TRUE)
  output$trk_check_ui <- shiny::renderUI({
    if (is.null(check_res()) || !isTRUE(input$chk_show)) {
      return(NULL)
    }
    shiny::plotOutput("trk_check", height = "380px")
  })
  output$trk_check <- shiny::renderPlot({
    r <- check_res()
    if (is.null(r)) {
      return(invisible(NULL))
    }
    if (!is.null(r$error)) {
      cd_message_plot(paste("Tuning check:", r$error))
      return(invisible(NULL))
    }
    cd_plot_track_check(r)
  }, res = 96)

  # The floating model and the extracted cycle go to the Anchor tab
  shiny::observeEvent(input$trk_to_anchor, {
    cv <- need_curve()
    if (is.null(cv)) {
      return()
    }
    cyc <- input$trk_cycle
    exc <- input$trk_excycle
    out <- try({
      am <- as.data.frame(WaverideR::curve2time(tracked_cycle_curve = cv, tracked_cycle_period = cyc))
      ex <- cd_extract(full_prep(), cv, cyc, exc, up = input$trk_up, down = input$trk_down)
      list(am = am, ex = ex)
    }, silent = TRUE)
    if (inherits(out, "try-error")) {
      shiny::showNotification(conditionMessage(attr(out, "condition")), type = "error", duration = 8)
      return()
    }
    ctx$shared$age <- out$am
    ctx$shared$age_label <- sprintf("floating age model of %s (%s-kyr track)", tv$file, cd_fmt(cyc))
    ctx$shared$extract <- out$ex
    ctx$shared$extract_label <- sprintf("%s-kyr cycle extracted from %s", cd_fmt(exc), tv$file)
    ctx$shared$data <- trk$s
    ctx$shared$name <- base_name()
    ctx$shared$ver <- ctx$shared$ver + 1
    shiny::updateTabsetPanel(session, "tabs", selected = "anchor")
  })

  need_curve <- function() {
    cv <- curve()
    if (is.null(cv)) {
      shiny::showNotification("Track the cycle first: click at least two points on its band.", type = "warning")
    }
    cv
  }
  base_name <- function() {
    tools::file_path_sans_ext(tv$file)
  }
  code_head <- function() {
    if (!identical(tv$kind, "cwt")) {
      # a track on an EHA, eTimeOpt, eCOCO, ... map: it is written to a file that the script reads
      mc <- map_curve()
      tf <- paste0(base_name(), if (identical(tv$kind, "sedrate")) { "-track_sedrate.txt" } else { "-track.txt" })
      if (!is.null(mc)) {
        write_out(mc, tf)
      }
      return(c(cd_code_read(tv$file, col = rv$cols[[tv$file]], sheet = rv$sheets[[tv$file]]),
          sprintf("wt <- WaverideR::analyse_wavelet(data = as.matrix(dat), dj = %s, lowerPeriod = %s, upperPeriod = %s, omega_nr = %s)",
              cd_fmt(tv$par$dj), cd_fmt(tv$par$lo), cd_fmt(tv$par$hi), cd_fmt(tv$par$omega)),
          sprintf("# the track drawn in CycloDesk on the %s map (written by CycloDesk)", cd_get_tool(tv$par$source)$label),
          sprintf("track <- CycloDesk::read_series(\"%s\")", tf),
          "names(track) <- c(\"depth\", \"period\")",
          if (identical(tv$kind, "sedrate")) {
            sprintf("track$period <- track$period / 100 * %s  # sed. rate (cm/kyr) to the period (m) of a %s-kyr cycle",
                cd_fmt(input$trk_cycle), cd_fmt(input$trk_cycle))
          } else {
            NULL
          }))
    }
    c(cd_code_read(tv$file, col = rv$cols[[tv$file]], sheet = rv$sheets[[tv$file]]),
        sprintf("wt <- WaverideR::analyse_wavelet(data = as.matrix(dat), dj = %s, lowerPeriod = %s, upperPeriod = %s, omega_nr = %s%s)",
            cd_fmt(tv$par$dj), cd_fmt(tv$par$lo), cd_fmt(tv$par$hi), cd_fmt(tv$par$omega),
            if (identical(tv$par$wavelet, "morlet")) { "" } else { sprintf(", wavelet = %s", cd_dep(tv$par$wavelet)) }),
        sprintf("pts <- data.frame(depth = %s,\n    period = %s)", cd_dep(signif(tv$pts$depth, 6)), cd_dep(signif(tv$pts$period, 5))),
        if (!is.null(tv$hmm)) {
          sprintf("track <- WaverideR::track_ridge_hmm(wt, guide = pts, guide_tolerance = %s)[, c(\"depth\", \"period\")]",
              cd_fmt(input$trk_tol))
        } else if (!is.null(tv$snap)) {
          sprintf("track <- CycloDesk::cd_snap_track(wt, pts, tol = %s)", cd_fmt(input$trk_tol))
        } else {
          "track <- data.frame(depth = wt$axis.1, period = approx(pts$depth, pts$period, xout = wt$axis.1, rule = 2)$y)"
        },
        if (is.finite(input$trk_lowess) && input$trk_lowess > 0) {
          sprintf("track$period <- lowess(track$depth, track$period, f = %s)$y", cd_fmt(input$trk_lowess))
        } else {
          NULL
        })
  }
  write_out <- function(x, name, header = NULL) {
    write_series(x, file.path(rv$folder, name), header = header)
    name
  }

  shiny::observeEvent(input$trk_save, {
    cv <- need_curve()
    if (is.null(cv)) {
      return()
    }
    b <- base_name()
    if (identical(tv$kind, "sedrate")) {
      wr <- c(write_out(map_curve(), paste0(b, "-track_sedrate.txt")),
          write_out(tv$pts[, c("depth", "period")], paste0(b, "-trackpoints_sedrate.txt")))
      ctx$say("Sedimentation-rate track saved: ", paste(wr, collapse = ", "))
      shiny::showNotification(paste("Saved", paste(wr, collapse = ", "), "(depth, sed. rate in cm/kyr; use Time Series >",
          "Sedimentation rate to age model)"), type = "message", duration = 8)
      ctx$refresh()
      return()
    }
    wr <- c(write_out(cv, paste0(b, "-track.txt")), write_out(tv$pts[, c("depth", "period")], paste0(b, "-trackpoints.txt")))
    if (!is.null(tv$hmm)) {
      wr <- c(wr, write_out(tv$hmm, paste0(b, "-trackHMM.txt"), header = TRUE))
    }
    ctx$add_code(c(code_head(), sprintf("CycloDesk::write_series(track, \"%s-track.txt\")", b)), paste("Track:", tv$file))
    ctx$say("Track saved: ", paste(wr, collapse = ", "))
    shiny::showNotification(paste("Saved", paste(wr, collapse = ", ")), type = "message")
    ctx$refresh()
  })

  shiny::observeEvent(input$trk_extract, {
    cv <- need_curve()
    if (is.null(cv)) {
      return()
    }
    cyc <- input$trk_cycle
    exc <- input$trk_excycle
    if (!is.finite(exc) || exc <= 0) {
      exc <- cyc
    }
    ex <- try(cd_extract(full_prep(), cv, cyc, exc, up = input$trk_up, down = input$trk_down,
        add_mean = isTRUE(input$trk_addmean)), silent = TRUE)
    if (inherits(ex, "try-error")) {
      shiny::showNotification(conditionMessage(attr(ex, "condition")), type = "error", duration = 8)
      return()
    }
    nm <- write_out(as.data.frame(ex), sprintf("%s-extract%s.txt", base_name(), cd_fmt(exc)))
    ext_res(list(ex = as.data.frame(ex), label = sprintf("Extracted %s-kyr cycle", cd_fmt(exc))))
    show_part("ext")
    ctx$shared$extract <- as.data.frame(ex)
    ctx$shared$extract_label <- sprintf("%s-kyr cycle extracted from %s", cd_fmt(exc), tv$file)
    ctx$add_code(c(code_head(), sprintf(paste0("ext <- WaverideR::extract_signal(tracked_cycle_curve = track, wavelet = wt,",
        " period_up = %s, period_down = %s,\n    add_mean = %s, tracked_cycle_period = %s, extract_cycle = %s)"),
        cd_fmt(input$trk_up), cd_fmt(input$trk_down), isTRUE(input$trk_addmean), cd_fmt(cyc), cd_fmt(exc)),
        sprintf("CycloDesk::write_series(ext, \"%s\")", nm)), paste("Extract tracked cycle:", tv$file))
    ctx$say("Extracted cycle written: ", nm)
    shiny::showNotification(paste("Written", nm), type = "message")
    ctx$refresh(select = nm)
  })

  shiny::observeEvent(input$trk_age, {
    cv <- need_curve()
    if (is.null(cv)) {
      return()
    }
    cyc <- input$trk_cycle
    out <- try({
      am <- as.data.frame(WaverideR::curve2time(tracked_cycle_curve = cv, tracked_cycle_period = cyc))
      sr <- as.data.frame(WaverideR::curve2sedrate(tracked_cycle_curve = cv, tracked_cycle_period = cyc))
      tuned <- as.data.frame(astrochron::tune(trk$s, controlPts = am, genplot = FALSE, verbose = FALSE))
      exc <- input$trk_excycle
      if (!is.finite(exc) || exc <= 0) {
        exc <- cyc
      }
      ex <- try(cd_extract(full_prep(), cv, cyc, exc, up = input$trk_up, down = input$trk_down), silent = TRUE)
      if (inherits(ex, "try-error")) {
        ex <- NULL
      } else {
        ex <- data.frame(time = stats::approx(am[, 1], am[, 2], xout = ex[, 1], rule = 2)$y, value = ex[, 2])
      }
      list(am = am, sr = sr, tuned = tuned, ex = ex, exc = exc)
    }, silent = TRUE)
    if (inherits(out, "try-error")) {
      shiny::showNotification(conditionMessage(attr(out, "condition")), type = "error", duration = 8)
      return()
    }
    age_res(list(am = out$am, tuned = out$tuned, cyc = cyc, ex = out$ex, exc = out$exc))
    show_part("age")
    ctx$shared$age <- out$am
    ctx$shared$age_label <- sprintf("floating age model of %s (%s-kyr track)", tv$file, cd_fmt(cyc))
    ctx$shared$data <- trk$s
    b <- base_name()
    cs <- cd_fmt(cyc)
    wr <- c(write_out(out$am, sprintf("%s-age%s.txt", b, cs)), write_out(out$sr, sprintf("%s-sedrate%s.txt", b, cs)),
        write_out(out$tuned, sprintf("%s-tuned%s.txt", b, cs)))
    ctx$add_code(c(code_head(),
        sprintf("am <- WaverideR::curve2time(tracked_cycle_curve = track, tracked_cycle_period = %s)", cs),
        sprintf("sr <- WaverideR::curve2sedrate(tracked_cycle_curve = track, tracked_cycle_period = %s)", cs),
        "tuned <- astrochron::tune(dat, controlPts = am, genplot = FALSE)",
        sprintf("CycloDesk::write_series(am, \"%s\")", wr[1]), sprintf("CycloDesk::write_series(sr, \"%s\")", wr[2]),
        sprintf("CycloDesk::write_series(tuned, \"%s\")", wr[3])), paste("Age model from the track:", tv$file))
    ctx$say("Age model written: ", paste(wr, collapse = ", "))
    shiny::showNotification(sprintf("Written %s (%s kyr in total)", paste(wr, collapse = ", "),
        cd_fmt(diff(range(out$am[, 2])))), type = "message", duration = 8)
    ctx$refresh(select = wr[3])
  })
}

# Helper lines ####

# Colours offered for helper lines (they are drawn on the scalogram)
cd_helper_colours <- function() {
  c("white" = "white", "red" = "#ff4d4d", "orange" = "#ffa500", "yellow" = "#ffe14d", "cyan" = "#33d6ff",
      "magenta" = "#ff4dd2", "green" = "#5cd65c", "black" = "black")
}

# Depths (or other values) as text for an input field: 6 significant digits, comma-separated
cd_fmt_depths <- function(v) {
  out <- character(length(v))
  for (i in seq_along(v)) {
    out[i] <- format(signif(v[i], 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  }
  paste(out, collapse = ", ")
}

# Helper lines to draw on the current map: all of them on a scalogram; on a sedimentation-rate map (eCOCO,
# eTimeOpt) only fixed values and file columns, which are then read as rates (cm/kyr), since cycle periods in
# depth units have no meaning there
cd_helper_for_kind <- function(h, kind) {
  if (!identical(kind, "sedrate") || !length(h)) {
    return(h)
  }
  keep <- logical(length(h))
  for (k in seq_along(h)) {
    keep[k] <- !is.null(h[[k]]$type) && h[[k]]$type %in% c("fixed", "column")
  }
  h[keep]
}

# Status text for helper lines: how many there are and which lie outside the period range of the scalogram
cd_helper_status <- function(h, prange, n_groups) {
  if (!length(h)) {
    return(sprintf("Helper lines: %d group(s), nothing to draw (no track yet, or the file gives no values in the depth range).",
        n_groups))
  }
  out <- character(0)
  for (k in seq_along(h)) {
    p <- h[[k]]$period[is.finite(h[[k]]$period) & h[[k]]$period > 0]
    if (!length(p)) {
      out <- c(out, sprintf("'%s' has no positive values", h[[k]]$label))
    } else if (max(p) < prange[1] || min(p) > prange[2]) {
      out <- c(out, sprintf("'%s' (%s to %s) lies outside the periods shown (%s to %s)", h[[k]]$label,
          cd_fmt(signif(min(p), 3)), cd_fmt(signif(max(p), 3)), cd_fmt(signif(prange[1], 3)), cd_fmt(signif(prange[2], 3))))
    }
  }
  txt <- sprintf("Helper lines: %d group(s), %d line(s).", n_groups, length(h))
  if (length(out)) {
    txt <- paste(txt, "Not visible:", paste(out, collapse = "; "),
        "- check the column and the multiplier (an age column is not a period: use 'from an age-model file').")
  }
  txt
}

# One-line description of a group of helper lines, for the list in the Track tab
cd_helper_describe <- function(spec) {
  v <- cd_fmt_depths(spec$values)
  txt <- switch(spec$type,
      cyc_track = sprintf("%s kyr, scaled by the track", v),
      cyc_file = sprintf("%s kyr, age model %s", v, spec$file),
      fixed = sprintf("fixed periods %s", v),
      column = sprintf("%s (%s) x %s", spec$colname, spec$file, cd_fmt(spec$mult)))
  col_name <- names(cd_helper_colours())[match(spec$colour, cd_helper_colours())]
  if (is.na(col_name)) {
    col_name <- spec$colour
  }
  paste0(if (nzchar(spec$label)) { paste0(spec$label, ": ") } else { "" }, txt, " [", col_name, "]")
}

# The curves (depth, period in depth units) of one group of helper lines on the positions x of the scalogram.
#   cyc_track: period = tracked period / tracked cycle x cycle (needs 'track', a data frame depth, period)
#   cyc_file:  period = sedimentation rate of the age model in 'data' (depth, age) x cycle
#   fixed:     constant periods
#   column:    period = column spec$ycol x spec$mult against column spec$xcol of 'data', within the range of x
# Returns a list of list(depth, period, label, colour, lty).
cd_helper_lines <- function(spec, x, track = NULL, track_cycle = NA, data = NULL) {
  lab0 <- if (is.null(spec$label) || !nzchar(spec$label)) { "" } else { paste0(spec$label, " ") }
  colour <- if (is.null(spec$colour)) { "white" } else { spec$colour }
  lty <- if (is.null(spec$lty)) { 2L } else { spec$lty }
  one <- function(depth, period, label) {
    list(depth = depth, period = period, label = trimws(paste0(lab0, label)), colour = colour, lty = lty, type = spec$type)
  }
  out <- list()
  if (spec$type == "column") {
    if (is.null(data) || max(spec$xcol, spec$ycol) > ncol(data)) {
      return(out)
    }
    d <- data.frame(depth = data[[spec$xcol]], v = data[[spec$ycol]] * spec$mult)
    d <- d[is.finite(d$depth) & is.finite(d$v), , drop = FALSE]
    d <- d[order(d$depth), , drop = FALSE]
    d <- d[d$depth >= min(x) & d$depth <= max(x), , drop = FALSE]
    if (nrow(d) < 1) {
      return(out)
    }
    lab <- if (nzchar(lab0)) { "" } else { sprintf("%s x %s", names(data)[spec$ycol], cd_fmt(spec$mult)) }
    if (nrow(d) == 1) {
      out[[1]] <- one(range(x), rep(d$v, 2), lab)
    } else {
      out[[1]] <- one(d$depth, d$v, lab)
    }
    return(out)
  }
  if (spec$type == "fixed") {
    for (k in seq_along(spec$values)) {
      out[[k]] <- one(range(x), rep(spec$values[k], 2), cd_fmt(spec$values[k]))
    }
    return(out)
  }
  # cycles: period per kyr (depth units / kyr) along x
  if (spec$type == "cyc_track") {
    if (is.null(track) || !is.finite(track_cycle) || track_cycle <= 0) {
      return(out)
    }
    srx <- stats::approx(track$depth, track$period, xout = x, rule = 2)$y / track_cycle
  } else {
    if (is.null(data) || ncol(data) < 2 || nrow(data) < 2) {
      return(out)
    }
    am <- data[order(data[[1]]), 1:2]
    sr <- abs(diff(am[[1]]) / diff(am[[2]]))
    mid <- (am[[1]][-1] + am[[1]][-nrow(am)]) / 2
    ok <- is.finite(sr)
    if (sum(ok) < 1) {
      return(out)
    }
    srx <- if (sum(ok) == 1) { rep(sr[ok], length(x)) } else { stats::approx(mid[ok], sr[ok], xout = x, rule = 2)$y }
  }
  for (k in seq_along(spec$values)) {
    out[[k]] <- one(x, srx * spec$values[k], paste(cd_fmt(spec$values[k]), "kyr"))
  }
  out
}

# Scalogram with the power as periods x positions and the periods increasing (CWT, SST and superlet results)
cd_wt_std <- function(w) {
  P <- w$Power
  n <- length(w$axis.1)
  if (nrow(P) == n && ncol(P) == length(w$Period) && ncol(P) != n) {
    P <- t(P)
  }
  o <- order(w$Period)
  list(Power = P[o, , drop = FALSE], Period = w$Period[o], axis.1 = w$axis.1, coi.1 = w$coi.1, coi.2 = w$coi.2,
      x = w$x, y = w$y)
}

# Local maxima of power along the period axis in one column (indices)
cd_col_peaks <- function(v) {
  n <- length(v)
  if (n < 3) {
    return(integer())
  }
  i <- 2:(n - 1)
  i[v[i] > v[i - 1] & v[i] >= v[i + 1]]
}

# Power peaks for display: every local maximum above a fraction of the strongest one at that position, on at most
# 'max_cols' positions so the figure stays readable
cd_track_peaks <- function(w, frac = 0.1, max_cols = 600) {
  n <- length(w$axis.1)
  cols <- seq(1, n, by = max(1, ceiling(n / max_cols)))
  dep <- vector("list", length(cols))
  per <- vector("list", length(cols))
  for (k in seq_along(cols)) {
    v <- w$Power[, cols[k]]
    pk <- cd_col_peaks(v)
    if (length(pk)) {
      pk <- pk[v[pk] >= frac * max(v[pk])]
    }
    dep[[k]] <- rep(w$axis.1[cols[k]], length(pk))
    per[[k]] <- w$Period[pk]
  }
  data.frame(depth = unlist(dep), period = unlist(per))
}

# Tuning check in the floating time domain: the modulated cycle (e.g. precession) is extracted with the tracked curve and
# its Hilbert envelope is taken; the modulating cycle (e.g. eccentricity) is extracted from that envelope (band-pass in
# floating time) and, with the tracked curve, from the record itself, and the two are correlated
cd_track_check <- function(w, curve, cyc, pmod, penv, mod_down = 0.8, mod_up = 1.2, env_down = 0.8, env_up = 1.2) {
  if (!is.finite(pmod) || !is.finite(penv) || pmod <= 0 || penv <= 0) {
    stop("give the durations of both cycles", call. = FALSE)
  }
  fac <- c(mod_down, mod_up, env_down, env_up)
  if (any(!is.finite(fac)) || mod_down <= 0 || env_down <= 0 || mod_down >= mod_up || env_down >= env_up) {
    stop("each band needs a lower factor > 0 that is smaller than the upper factor", call. = FALSE)
  }
  if (!inherits(w, "cd_xprep")) {
    w <- cd_extract_prep(w)
  }
  minp <- min(curve$period) * pmod / cyc * mod_down
  if (minp < min(w$Period)) {
    stop(sprintf("the %s-kyr cycle reaches periods (%s) below the shortest period of the scalogram (%s): lower it",
        cd_fmt(pmod), cd_fmt_period(minp), cd_fmt_period(min(w$Period))), call. = FALSE)
  }
  mod <- cd_extract(w, curve, cyc, pmod, up = mod_up, down = mod_down)
  env_c <- cd_extract(w, curve, cyc, penv, up = env_up, down = env_down)
  hil <- astrochron::hilbert(data.frame(mod[, 1], mod[, 2]), genplot = FALSE, verbose = FALSE)
  am <- as.data.frame(WaverideR::curve2time(tracked_cycle_curve = curve, tracked_cycle_period = cyc))
  t <- stats::approx(am[, 1], am[, 2], xout = mod[, 1], rule = 2)$y
  e <- hil[, 2]
  ecc <- stats::approx(env_c[, 1], env_c[, 2], xout = mod[, 1], rule = 2)$y
  ok <- is.finite(t) & is.finite(e)
  span <- diff(range(t[ok]))
  if (span < 1.5 * penv) {
    stop(sprintf("the floating record spans %s kyr: too short to extract the %s-kyr cycle from the envelope",
        cd_fmt(span), cd_fmt(penv)), call. = FALSE)
  }
  # the envelope on a regular floating time grid, band-passed around the modulating cycle
  dt <- stats::median(diff(t[ok]))
  tg <- seq(min(t[ok]), max(t[ok]), by = dt)
  eg <- stats::approx(t[ok], e[ok], xout = tg, rule = 2, ties = mean)$y
  fl <- astrochron::taner(data.frame(tg, eg), flow = 1 / (penv * env_up), fhigh = 1 / (penv * env_down),
      genplot = FALSE, check = FALSE, verbose = FALSE)
  env_f <- stats::approx(fl[, 1], fl[, 2], xout = t, rule = 2)$y
  ok <- is.finite(env_f) & is.finite(ecc) & is.finite(e)
  r <- stats::cor(env_f[ok], ecc[ok])
  r_raw <- stats::cor(e[ok], ecc[ok])
  list(time = t, mod = mod[, 2], env = e, env_f = env_f, ecc = ecc, r = r, r_raw = r_raw, pmod = pmod, penv = penv,
      mod_band = c(mod_down, mod_up), env_band = c(env_down, env_up))
}

cd_plot_track_check <- function(r) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, ncol = 1))
  graphics::par(mar = c(0.6, 4.8, 2, 1), mgp = c(2.4, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  xr <- range(r$time, na.rm = TRUE)
  graphics::plot(r$time, r$mod, type = "l", col = "#7f8c8d", xaxt = "n", xlab = "", xlim = xr, xaxs = "i",
      ylim = range(c(r$mod, r$env, -r$env), na.rm = TRUE), ylab = paste(cd_fmt(r$pmod), "kyr"),
      main = sprintf("Tuning check: %s kyr (x%s to x%s) and its envelope; %s kyr of the envelope (x%s to x%s)",
      cd_fmt(r$pmod), cd_fmt(r$mod_band[1]), cd_fmt(r$mod_band[2]), cd_fmt(r$penv), cd_fmt(r$env_band[1]),
      cd_fmt(r$env_band[2])), cex.main = 0.85, font.main = 1)
  graphics::lines(r$time, r$env, col = "#e6a39b", lwd = 1.2)
  graphics::lines(r$time, -r$env, col = "#e6a39b", lwd = 1.2)
  graphics::lines(r$time, r$env_f, col = "#c0392b", lwd = 2)
  graphics::par(mar = c(4, 4.8, 2.3, 1))
  z1 <- (r$env_f - mean(r$env_f, na.rm = TRUE)) / stats::sd(r$env_f, na.rm = TRUE)
  z2 <- (r$ecc - mean(r$ecc, na.rm = TRUE)) / stats::sd(r$ecc, na.rm = TRUE)
  graphics::plot(r$time, z1, type = "l", col = "#c0392b", lwd = 1.5, xlim = xr, xaxs = "i",
      ylim = range(c(z1, z2), na.rm = TRUE), xlab = "Floating time (kyr)", ylab = "standardised")
  graphics::lines(r$time, z2, col = "#1f4e79", lwd = 1.5)
  graphics::abline(h = 0, col = "grey80")
  # labels in the top margin, so they never cover the curves
  # the two labels right-aligned next to each other, the r value on the left
  lab2 <- sprintf("%s kyr of the record", cd_fmt(r$penv))
  lab1 <- sprintf("%s kyr of the envelope   ", cd_fmt(r$penv))
  usr <- graphics::par("usr")
  graphics::mtext(lab2, side = 3, line = 0.2, at = usr[2], adj = 1, col = "#1f4e79", cex = 0.75)
  graphics::mtext(lab1, side = 3, line = 0.2, at = usr[2] - graphics::strwidth(lab2, cex = 0.75 * graphics::par("cex")),
      adj = 1, col = "#c0392b", cex = 0.75)
  # a strong negative r is also a good fit: the proxy then responds inversely to the longer cycle
  graphics::mtext(sprintf("r = %.3f%s (raw envelope %.3f)", r$r, if (r$r < 0) { ", anti-phased" } else { "" }, r$r_raw),
      side = 3, line = 1.1, adj = 0, font = 2, cex = 0.8,
      col = if (abs(r$r) > 0.5) { "#2e7d32" } else if (abs(r$r) > 0.2) { "#b7950b" } else { "#c0392b" })
}

#' Snap a tracked curve to the nearest power peaks
#'
#' Completes a tracking: at every position of the scalogram the period is set to the local maximum of power (along the
#' period axis) that is nearest, on a log2 scale, to the line through the tracking points. Positions without a peak
#' within the search width are interpolated.
#'
#' @param wavelet Wavelet result of \code{WaverideR::analyse_wavelet()} (or a superlet / synchrosqueezed result).
#' @param points Tracking points: data frame with depth in column 1 and period in column 2 (one point gives a
#'   constant guide).
#' @param tol Search width around the guide line, as a fraction of the guide period (numeric, > 0). \code{Default=0.25}.
#' @return A data frame with columns \code{depth} and \code{period} at every position of the scalogram.
#' @export
cd_snap_track <- function(wavelet, points, tol = 0.25) {
  w <- cd_wt_std(wavelet)
  x <- w$axis.1
  pts <- as.data.frame(points)[, 1:2]
  pts <- pts[order(pts[, 1]), ]
  g <- if (nrow(pts) >= 2) { stats::approx(pts[, 1], pts[, 2], xout = x, rule = 2, ties = mean)$y } else { rep(pts[1, 2], length(x)) }
  lp <- log2(w$Period)
  out <- rep(NA_real_, length(x))
  for (j in seq_along(x)) {
    pk <- cd_col_peaks(w$Power[, j])
    pk <- pk[w$Period[pk] >= g[j] * (1 - tol) & w$Period[pk] <= g[j] * (1 + tol)]
    if (length(pk)) {
      out[j] <- w$Period[pk[which.min(abs(lp[pk] - log2(g[j])))]]
    }
  }
  ok <- is.finite(out)
  if (sum(ok) >= 2) {
    out <- stats::approx(x[ok], out[ok], xout = x, rule = 2)$y
  } else {
    out <- g
  }
  data.frame(depth = x, period = out)
}

# The extracted cycle below the scalogram: the data (grey, scaled) and the extracted cycle on the same depth axis
cd_plot_track_extract <- function(disp, s, ex, label, bounds = NULL) {
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(3.4, 4.8, 1.4, 1), mgp = c(2.2, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  y <- ex[, 2]
  ds <- (s[, 2] - mean(s[, 2])) / stats::sd(s[, 2]) * stats::sd(y, na.rm = TRUE) + mean(y, na.rm = TRUE)
  graphics::plot(s[, 1], ds, type = "l", col = "grey75", xlim = range(disp$x), xaxs = "i", xlab = names(s)[1],
      ylab = "Extracted", main = label, cex.main = 0.85, font.main = 1, ylim = range(c(y, ds), na.rm = TRUE))
  graphics::lines(ex[, 1], y, col = "#1f4e79", lwd = 1.5)
  if (length(bounds)) {
    graphics::abline(v = bounds, col = "#d97706", lwd = 1.5, lty = 2)
  }
}

# The floating age model below the scalogram (same depth axis) and the tuned series on the floating time scale
cd_plot_track_age <- function(disp, s, am, tuned, cyc, bounds = NULL, ex = NULL, exc = NULL) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::layout(matrix(1:2, ncol = 1))
  graphics::par(mar = c(3.4, 4.8, 1.4, 1), mgp = c(2.2, 0.7, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  graphics::plot(am[, 1], am[, 2], type = "l", col = "#1f4e79", lwd = 1.5, xlim = range(disp$x), xaxs = "i",
      xlab = names(s)[1], ylab = "", cex.main = 0.85, font.main = 1,
      main = sprintf("Floating age model (%s-kyr track): %s kyr in total", cd_fmt(cyc), cd_fmt(round(diff(range(am[, 2]))))))
  graphics::mtext("Time (kyr)", side = 2, line = 3.5, las = 0)
  if (length(bounds)) {
    graphics::abline(v = bounds, col = "#d97706", lwd = 1.5, lty = 2)
  }
  graphics::par(mar = c(3.4, 4.8, 1.8, 1))
  has_ex <- !is.null(ex) && nrow(ex) > 1
  ey <- if (has_ex) { ex[, 2] - mean(ex[, 2], na.rm = TRUE) + mean(tuned[, 2], na.rm = TRUE) } else { NULL }
  graphics::plot(tuned[, 1], tuned[, 2], type = "l", col = if (has_ex) { "grey60" } else { "#2c3e50" }, xaxs = "i",
      xlab = "Floating time (kyr)", ylab = names(s)[2], ylim = range(c(tuned[, 2], ey), na.rm = TRUE),
      main = if (has_ex) { sprintf("Tuned series with the %s-kyr cycle extracted from the record", cd_fmt(exc)) } else {
      "Tuned series" }, cex.main = 0.85, font.main = 1)
  if (has_ex) {
    graphics::lines(ex[, 1], ey, col = "#c0392b", lwd = 1.5)
  }
  if (length(bounds)) {
    graphics::abline(v = stats::approx(am[, 1], am[, 2], xout = bounds)$y, col = "#d97706", lwd = 1.5, lty = 2)
  }
}

# Peak filter: slider position (0-100) to a fraction of the strongest peak, on a log scale (1e-4 to 1); 0 shows all peaks
cd_peak_frac <- function(v) {
  if (is.null(v) || !is.finite(v) || v <= 0) {
    return(0)
  }
  10^(-4 * (1 - min(v, 100) / 100))
}

# Fast extraction of a cycle along a tracked curve, the same computation as WaverideR::extract_signal() (real part of
# the CWT, scale-normalised, summed over the band period * down to period * up at each position, rescaled to the
# standard deviation of the data) but with the cumulative sums over the periods computed once per CWT
cd_extract_prep <- function(w) {
  psi0 <- if (is.null(w$psi0)) { pi^(-1 / 4) } else { w$psi0 }
  R <- Re(w$Wave) / sqrt(w$Scale) * (w$dj * sqrt(w$dt) / psi0)
  R[!is.finite(R)] <- 0
  nr <- nrow(R)
  C <- matrix(0, nr + 1, ncol(R))
  for (k in seq_len(nr)) {
    C[k + 1, ] <- C[k, ] + R[k, ]
  }
  y <- w$y
  structure(list(C = C, Period = w$Period, x = w$x, norm = stats::sd(y) / stats::sd(C[nr + 1, ]), mean = mean(y)),
      class = "cd_xprep")
}

# Index of the nearest value in an increasing vector (the first one on a tie)
cd_nearest_index <- function(p, v) {
  j <- findInterval(v, p, all.inside = TRUE)
  j + (abs(p[j + 1] - v) < abs(p[j] - v))
}

# Linear interpolation with linear extrapolation beyond the ends
cd_approx_extrap <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  keep <- !duplicated(x)
  x <- x[keep]
  y <- y[keep]
  o <- order(x)
  x <- x[o]
  y <- y[o]
  out <- stats::approx(x, y, xout = xout, rule = 2)$y
  n <- length(x)
  if (n >= 2) {
    lo <- xout < x[1]
    out[lo] <- (y[2] - y[1]) / (x[2] - x[1]) * (xout[lo] - x[1]) + y[1]
    hi <- xout > x[n]
    out[hi] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1]) * (xout[hi] - x[n - 1]) + y[n - 1]
  }
  out
}

cd_extract <- function(prep, curve, cyc, exc, up = 1.2, down = 0.8, add_mean = FALSE) {
  if (!inherits(prep, "cd_xprep")) {
    prep <- cd_extract_prep(prep)
  }
  cv <- as.data.frame(curve)
  g <- cd_approx_extrap(cv[, 1], cv[, 2] * exc / cyc, prep$x)
  a <- cd_nearest_index(prep$Period, g * down)
  b <- cd_nearest_index(prep$Period, g * up)
  i <- seq_along(prep$x)
  v <- prep$C[cbind(pmax(a, b) + 1, i)] - prep$C[cbind(pmin(a, b), i)]
  v <- v * prep$norm
  if (add_mean) {
    v <- v + prep$mean
  }
  data.frame(filtered_cycle = prep$x, value = v)
}

# Track tab sources ####
#
# Besides wavelet, superlet and synchrosqueezed scalograms, cycles can be tracked on the maps of the evolutive
# spectra (EHA, windowed FFT, EHA on a log2 axis, evolutive MESA) and sedimentation rates on the maps of eTimeOpt,
# win_timeOpt and eCOCO. These maps are put on a common form: power over positions (columns) and over y values
# (rows) evenly spaced on a log2 scale, so that snapping, the HMM ridge follower and the display work as for a
# scalogram.

# What a result can be tracked as: "cwt" (a scalogram), "map" (a period map), "sedrate" (a sed-rate map) or NULL
cd_track_kind <- function(res) {
  if (is.null(res) || is.null(res$tool)) {
    return(NULL)
  }
  r <- res$result
  if (identical(res$tool, "wavelet")) {
    return("cwt")
  }
  if (identical(res$tool, "evolutive")) {
    return("map")
  }
  if (res$tool %in% c("etimeopt", "ecoco")) {
    return("sedrate")
  }
  if (is.list(r) && !is.null(r$Power) && !is.null(r$Period) && !is.null(r$axis.1)) {
    return("cwt")
  }
  if (is.list(r) && !is.null(r$sr) && !is.null(r$depth) && (is.matrix(r$score) || is.matrix(r$rho))) {
    return("sedrate")
  }
  NULL
}

# A map (y values x positions) resampled to y values evenly spaced on a log2 scale
cd_map_std <- function(pos, yv, M, n_y = 250) {
  M <- as.matrix(M)
  ok <- is.finite(yv) & yv > 0
  yv <- yv[ok]
  M <- M[ok, , drop = FALSE]
  o <- order(yv)
  yv <- yv[o]
  M <- M[o, , drop = FALSE]
  keep <- !duplicated(yv)
  yv <- yv[keep]
  M <- M[keep, , drop = FALSE]
  po <- order(pos)
  pos <- pos[po]
  M <- M[, po, drop = FALSE]
  if (length(yv) < 2 || length(pos) < 2) {
    stop("the map is too small to track on", call. = FALSE)
  }
  lg <- seq(log2(min(yv)), log2(max(yv)), length.out = n_y)
  ly <- log2(yv)
  P <- matrix(0, n_y, length(pos))
  for (j in seq_along(pos)) {
    v <- M[, j]
    f <- is.finite(v)
    if (sum(f) >= 2) {
      col <- stats::approx(ly[f], v[f], xout = lg, rule = 2)$y
      # scores can be negative (eCOCO): shifted so that the lowest value of each position is 0
      if (min(col) < 0) {
        col <- col - min(col)
      }
      P[, j] <- col
    }
  }
  list(Power = P, Period = 2^lg, axis.1 = pos, dt = stats::median(diff(pos)), dj = diff(lg[1:2]))
}

# The map of an evolutive, eTimeOpt or eCOCO result in the common form
cd_track_map <- function(res) {
  r <- res$result
  if (identical(res$tool, "evolutive")) {
    if (identical(r$method, "eha_log2")) {
      return(cd_map_std(r$raw$depth, 2^r$raw$log2_period, r$raw$pwr))
    }
    if (identical(r$method, "mesa")) {
      f <- r$raw$frequency
      k <- f > 0
      return(cd_map_std(r$raw$position, 1 / f[k], r$raw$windows[[1]]$power[k, , drop = FALSE]))
    }
    k <- r$f > 0
    return(cd_map_std(r$x, 1 / r$f[k], t(r$z[, k, drop = FALSE])))
  }
  if (identical(res$tool, "etimeopt")) {
    if (identical(r$method, "win")) {
      return(cd_map_std(r$raw$x, r$raw$sedrate, r$raw$r_2_opt))
    }
    return(cd_map_std(r$x, r$sed, t(r$z)))
  }
  if (!is.null(r$sr) && !is.null(r$depth)) {
    m <- if (is.matrix(r$score)) { r$score } else { r$rho }
    return(cd_map_std(r$depth, r$sr, m))
  }
  NULL
}
