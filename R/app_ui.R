# User interface ####

#' Start the CycloDesk interface
#'
#' Opens the point-and-click interface in the web browser. Pick a working folder on the left; its data files are
#' listed below it. Select one or more files and choose a tool from the menus; results are written as new text files
#' into the same folder (with Acycle-style suffixes such as \code{-rsp0.02} or \code{-rLowess0.35}), and the
#' equivalent R code is collected in the R code tab.
#'
#' @param folder Working folder to start in (character string); \code{NULL} uses the last folder used, or the home
#'   folder. \code{Default=NULL}.
#' @param port Port number of the local web server (integer) or \code{NULL} for a random free port.
#'   \code{Default=NULL}.
#' @param launch.browser Open the interface in the browser (logical). \code{Default=TRUE}.
#' @param stop_on_close Stop R's web server when the browser tab is closed (logical). \code{Default=TRUE}.
#' @return Called for its side effect; returns the value of \code{shiny::runApp()} invisibly.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(folder = NULL, port = NULL, launch.browser = TRUE, stop_on_close = TRUE) {
  oldop <- options(waverider.dev_new = FALSE, shiny.useragg = cd_ragg_ok())
  on.exit(options(oldop), add = TRUE)
  app <- cd_app(folder = folder, stop_on_close = stop_on_close)
  invisible(shiny::runApp(app, port = port, launch.browser = launch.browser))
}

#' The CycloDesk Shiny application object
#'
#' @inheritParams run_app
#' @return A \code{shiny.appobj}, for example to deploy with \code{shiny::runApp()}.
#' @export
cd_app <- function(folder = NULL, stop_on_close = TRUE) {
  www <- system.file("app", "www", package = "CycloDesk")
  if (nzchar(www)) {
    shiny::addResourcePath("cyclodesk", www)
  }
  shiny::shinyApp(ui = cd_ui(), server = cd_server(folder = folder, stop_on_close = stop_on_close))
}

cd_menu <- function(label, items, icon = NULL) {
  shiny::tags$li(class = "dropdown",
      shiny::tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
          if (!is.null(icon)) { shiny::icon(icon) } else { NULL }, label, shiny::tags$span(class = "cd-caret", shiny::icon("chevron-down"))),
      shiny::tags$ul(class = "dropdown-menu", items))
}

cd_item <- function(label, action, icon = "circle", key = NULL) {
  shiny::tags$li(shiny::tags$a(href = "#", class = "cd-act", `data-action` = action,
      shiny::tags$span(class = "cd-ico", shiny::icon(icon)), shiny::tags$span(class = "cd-lab", label),
      if (!is.null(key)) { shiny::tags$span(class = "cd-key", key) } else { NULL }))
}

cd_divider <- function() {
  shiny::tags$li(role = "separator", class = "divider")
}

cd_header <- function(label) {
  shiny::tags$li(class = "dropdown-header", label)
}

# Icon of each tool in the menus (Font Awesome 6 names)
cd_tool_icon <- function(id) {
  ic <- c(sampling = "ruler-horizontal", distribution = "chart-column", astro_solution = "globe", insolation = "sun",
      etp = "wave-square", generator = "shuffle", examples = "database", sort = "arrow-down-a-z",
      interpolate = "arrows-left-right", select = "crop-simple", remove = "scissors", trim = "eraser", clip = "compress",
      transform = "wand-magic-sparkles", derivative = "superscript", smooth = "water", column = "table-columns",
      changepoint = "code-branch", detrend = "arrow-trend-down", prewhiten = "droplet", spectrum = "signal",
      evolutive = "fire", wavelet = "braille", coherence = "link", filter = "filter", amplitude = "wave-square",
      agemodel = "clock", sedrate2age = "gauge", tune = "hourglass-half", timeopt = "bullseye",
      etimeopt = "crosshairs", coco = "chart-line", ecoco = "chart-area", rho1 = "stairs", dynot = "mountain",
      pda = "layer-group", spectral_moments = "heart-pulse", xwavelet = "code-merge", bispectral = "table-cells",
      track_auto = "wand-magic-sparkles")
  if (id %in% names(ic)) {
    return(ic[[id]])
  }
  "circle"
}

cd_tool_items <- function(menu) {
  tl <- cd_tools()
  out <- list()
  last_group <- ""
  for (t in tl) {
    if (t$menu != menu) {
      next
    }
    g <- if (is.null(t$group)) { "" } else { t$group }
    if (!identical(g, last_group) && nzchar(g)) {
      if (length(out)) {
        out[[length(out) + 1]] <- cd_divider()
      }
      out[[length(out) + 1]] <- cd_header(g)
    }
    last_group <- g
    out[[length(out) + 1]] <- cd_item(t$label, paste0("tool:", t$id), icon = cd_tool_icon(t$id))
    if (t$id == "track_auto") {
      out[[length(out) + 1]] <- cd_item("Track a cycle by hand on the scalogram", "cmd:track", icon = "route")
      out[[length(out) + 1]] <- cd_item("Anchor to an astronomical solution or metronome", "cmd:anchor", icon = "anchor")
    }
  }
  out
}

# Small wave logo for the menu bar
cd_logo <- function() {
  shiny::tags$img(src = "cyclodesk/cyclodesk.png", class = "cd-logo", alt = "")
}

cd_ui <- function() {
  menubar <- shiny::tags$nav(class = "navbar navbar-default cd-navbar",
      shiny::tags$div(class = "container-fluid",
          shiny::tags$div(class = "navbar-header", shiny::tags$span(class = "navbar-brand cd-brand", cd_logo(),
              shiny::tags$span(class = "cd-brand-name", "CycloDesk"))),
          shiny::tags$ul(class = "nav navbar-nav",
              cd_menu("File", list(
                cd_item("New folder...", "cmd:newfolder", "folder-plus"),
                cd_item("New data file (paste)...", "cmd:newfile", "file-circle-plus"),
                cd_divider(),
                cd_item("Save current figure...", "cmd:savefig", "image"),
                cd_item("Save R code of this session", "cmd:savecode", "code"),
                cd_divider(),
                cd_item("Open working folder in Explorer", "cmd:explorer", "folder-open"),
                cd_divider(),
                cd_item("Quit CycloDesk", "cmd:quit", "power-off"))),
              cd_menu("Edit", list(
                cd_item("Refresh list", "cmd:refresh", "rotate", key = "F5"),
                cd_item("Rename...", "cmd:rename", "pen"),
                cd_item("Duplicate", "cmd:duplicate", "copy"),
                cd_item("Delete...", "cmd:delete", "trash-can", key = "Del"),
                cd_divider(),
                cd_item("Show all files / data files only", "cmd:showall", "eye"))),
              cd_menu("Plot", c(list(
                cd_item("Plot", "cmd:plot", "chart-line"),
                cd_item("Plot stacked", "cmd:plotstack", "layer-group"),
                cd_item("Plot standardised", "cmd:plotstd", "chart-simple"),
                cd_item("Plot with swapped axes", "cmd:plotswap", "arrows-left-right"),
                cd_item("Statistics summary / data table", "cmd:stats", "table"),
                cd_divider()), cd_tool_items("Plot"))),
              cd_menu("Basic Series", cd_tool_items("Basic Series")),
              cd_menu("Math", cd_tool_items("Math")),
              cd_menu("Time Series", cd_tool_items("Time Series")),
              cd_menu("Help", list(
                cd_item("About CycloDesk and how to cite", "cmd:about", "circle-info"),
                cd_item("Which R function does what", "cmd:functions", "book"),
                cd_item("Tutorial and manual (web)", "cmd:manual", "graduation-cap"),
                cd_divider(),
                cd_item("Report a problem or suggest a feature", "cmd:feedback", "comment-dots"),
                cd_item("Check for updates", "cmd:update", "arrows-rotate")))),
          shiny::tags$button(class = "cd-closebtn", id = "cd_close", title = "Close CycloDesk (stops R)",
              shiny::icon("power-off"), shiny::tags$span("Close")),
          shiny::tags$span(class = "cd-busy", id = "cd_busy", "")))
  left <- shiny::tags$div(class = "cd-left",
      shiny::tags$div(class = "cd-card cd-nav",
          shiny::tags$div(class = "cd-navtop",
              shiny::tags$button(class = "cd-ibtn", id = "cd_up", title = "Up one folder (Backspace)", shiny::icon("arrow-up")),
              shiny::uiOutput("places", inline = TRUE),
              shiny::tags$span(class = "cd-spacer"),
              shiny::tags$button(class = "cd-ibtn", id = "cd_edit", title = "Type a path", shiny::icon("pencil")),
              shiny::tags$button(class = "cd-ibtn", id = "cd_refresh", title = "Refresh (F5)", shiny::icon("rotate"))),
          shiny::uiOutput("drives"),
          shiny::tags$div(id = "cd_crumbs", class = "cd-crumbs"),
          shiny::tags$div(id = "cd_pathbox", class = "cd-pathbox", style = "display:none",
              shiny::textInput("path", NULL, value = "", width = "100%", placeholder = "D:/Phd/data  (Enter to go)"))),
      shiny::tags$div(class = "cd-card cd-browser-card",
          shiny::tags$div(class = "cd-filter",
              shiny::icon("magnifying-glass"),
              shiny::tags$input(id = "cd_filter", type = "text", placeholder = "Filter files", autocomplete = "off")),
          shiny::tags$div(id = "cd_browser", class = "cd-browser", tabindex = "0")),
      shiny::tags$div(class = "cd-card cd-infocard",
          shiny::uiOutput("colpick"),
          shiny::tags$div(class = "cd-fileinfo", shiny::uiOutput("fileinfo"))))
  plot_tab <- shiny::tabPanel("Plot", value = "plot",
      shiny::tags$div(class = "cd-plotopts",
          shiny::checkboxInput("pl_stack", "Stacked", FALSE),
          shiny::checkboxInput("pl_std", "Standardised", FALSE),
          shiny::checkboxInput("pl_pts", "Points", FALSE),
          shiny::checkboxInput("pl_rev", "Reverse depth axis", FALSE),
          shiny::checkboxInput("pl_swap", "Depth vertical", FALSE),
          shiny::actionButton("pl_savefig", "Save figure", icon = shiny::icon("image"), class = "btn-sm"),
          shiny::tags$span(class = "cd-hint", "Drag to zoom, double-click to reset")),
      shiny::plotOutput("mainplot", height = "600px", brush = shiny::brushOpts("pl_brush", direction = "x", resetOnNew = TRUE),
          dblclick = "pl_dbl"))
  tool_tab <- shiny::tabPanel("Tool", value = "tool",
      shiny::fluidRow(
          shiny::column(4, class = "cd-toolform",
              shiny::uiOutput("tool_head"),
              shiny::uiOutput("tool_files"),
              shiny::uiOutput("tool_form"),
              shiny::uiOutput("tool_buttons")),
          shiny::column(8,
              shiny::plotOutput("tool_plot", height = "560px"),
              shiny::verbatimTextOutput("tool_text"))))
  data_tab <- shiny::tabPanel("Data", value = "data",
      shiny::fluidRow(shiny::column(5, shiny::uiOutput("stats")), shiny::column(7, shiny::tableOutput("datatable"))))
  code_tab <- shiny::tabPanel("R code", value = "code",
      shiny::tags$p(class = "cd-hint", "The R code of every operation in this session. It runs in the working folder",
          " (setwd() to it first)."),
      shiny::actionButton("code_save", "Save to working folder", icon = shiny::icon("floppy-disk"), class = "btn-sm"),
      shiny::actionButton("code_clear", "Clear", class = "btn-sm"),
      shiny::tags$div(class = "cd-code", shiny::verbatimTextOutput("code")))
  log_tab <- shiny::tabPanel("Log", value = "log", shiny::tags$div(class = "cd-log", shiny::verbatimTextOutput("log")))
  shiny::fluidPage(
      title = "CycloDesk",
      theme = NULL,
      shiny::tags$head(
          shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
          shiny::tags$link(rel = "icon", type = "image/png", href = "cyclodesk/cyclodesk.png"),
          shiny::tags$link(rel = "stylesheet", href = "cyclodesk/cyclodesk.css"),
          shiny::tags$script(src = "cyclodesk/cyclodesk.js")),
      menubar,
      shiny::tags$div(class = "cd-main",
          left,
          shiny::tags$div(class = "cd-right cd-card",
              shiny::tabsetPanel(id = "tabs", type = "tabs", plot_tab, tool_tab, cd_track_tab(), cd_anchor_tab(), data_tab, code_tab,
                  log_tab))),
      shiny::tags$div(class = "cd-status", shiny::textOutput("status", inline = TRUE)))
}

# shiny draws plots with ragg when it is installed; a broken ragg installation (built for another R version) gives
# "unable to load shared object ... ragg.dll", so then the standard png device is used
cd_ragg_ok <- function() {
  ok <- tryCatch(requireNamespace("ragg", quietly = TRUE), error = function(e) { FALSE }, warning = function(w) { FALSE })
  isTRUE(ok)
}
