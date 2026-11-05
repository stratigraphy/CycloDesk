# Server ####

cd_config_file <- function() {
  file.path(tools::R_user_dir("CycloDesk", "config"), "last_folder.txt")
}

cd_start_folder <- function(folder) {
  if (!is.null(folder) && dir.exists(folder)) {
    return(normalizePath(folder, winslash = "/"))
  }
  f <- cd_config_file()
  if (file.exists(f)) {
    last <- readLines(f, warn = FALSE, n = 1)
    if (length(last) && dir.exists(last)) {
      return(last)
    }
  }
  docs <- file.path(Sys.getenv("USERPROFILE", path.expand("~")), "Documents")
  if (dir.exists(docs)) {
    return(normalizePath(docs, winslash = "/"))
  }
  normalizePath(path.expand("~"), winslash = "/")
}

cd_server <- function(folder = NULL, stop_on_close = TRUE) {
  function(input, output, session) {
    if (stop_on_close) {
      session$onSessionEnded(function() {
        shiny::stopApp()
      })
    }
    rv <- shiny::reactiveValues(
        folder = cd_start_folder(folder),
        listing = list(dirs = character(), files = character()),
        show_all = FALSE,
        cols = list(),
        sheets = list(),
        tool = NULL,
        tool_res = NULL,
        tool_err = NULL,
        job = NULL,
        job_info = NULL,
        code = character(),
        log = character(),
        xlim = NULL,
        form_nonce = 0,
        recent = cd_recent_folders(),
        drives = cd_drives(),
        tick = 0)

    # Update check (once a day, in a background process) ####
    upd <- shiny::reactiveValues(job = NULL, status = NULL, force = FALSE)
    start_update_check <- function(force = FALSE) {
      if (!is.null(shiny::isolate(upd$job))) {
        return(invisible(NULL))
      }
      upd$force <- force
      job <- try(callr::r_bg(cd_update_status, args = list(force = force), package = TRUE), silent = TRUE)
      upd$job <- if (inherits(job, "try-error")) { NULL } else { job }
    }
    if (isTRUE(getOption("CycloDesk.check_updates", TRUE))) {
      start_update_check()
    }
    shiny::observe({
      j <- upd$job
      if (is.null(j)) {
        return()
      }
      if (j$is_alive()) {
        shiny::invalidateLater(1000)
        return()
      }
      st <- try(j$get_result(), silent = TRUE)
      upd$job <- NULL
      force <- shiny::isolate(upd$force)
      if (inherits(st, "try-error")) {
        if (force) {
          shiny::showNotification("The update check failed.", id = "cd_upd", type = "error")
        }
        return()
      }
      upd$status <- st
      cd_update_note(st, force)
    })
    shiny::observeEvent(input$upd_go, {
      st <- upd$status
      if (is.null(st) || !any(st$update)) {
        return()
      }
      if (cd_start_updater(st)) {
        session$sendCustomMessage("cd_close", list(keep = TRUE, title = "CycloDesk is being updated",
            text = "A window shows the progress; CycloDesk opens again in a new tab when the update is installed."))
        shiny::stopApp()
      } else {
        shiny::showModal(shiny::modalDialog(title = "Update CycloDesk", easyClose = TRUE,
            shiny::tags$p("Close CycloDesk, then run this in a fresh R session and start CycloDesk again:"),
            shiny::tags$pre(paste(cd_update_code(st), collapse = "\n"))))
      }
    })

    say <- function(...) {
      msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", paste0(...))
      rv$log <- c(msg, rv$log)
    }
    add_code <- function(lines, title) {
      rv$code <- c(rv$code, "", paste0("# ", title, " (", format(Sys.time(), "%Y-%m-%d %H:%M"), ")"), lines)
    }

    # Working folder and file browser ####
    refresh <- function(select = NULL) {
      if (identical(rv$folder, RC_PC)) {
        rv$drives <- cd_drives()
        items <- list()
        for (k in seq_along(rv$drives)) {
          items[[k]] <- list(v = paste0("p|", rv$drives[[k]]), n = names(rv$drives)[k], t = "drive", i = "", d = "")
        }
        rv$listing <- list(dirs = character(), files = character())
        session$sendCustomMessage("cd_listing", list(items = items, selected = list(),
            crumbs = list(list(l = "This PC", p = RC_PC)), up = FALSE))
        shiny::updateTextInput(session, "path", value = "")
        rv$tick <- rv$tick + 1
        return(invisible(NULL))
      }
      rv$listing <- cd_list_folder(rv$folder, rv$show_all)
      items <- list()
      for (d in rv$listing$dirs) {
        items[[length(items) + 1]] <- list(v = paste0("d|", d), n = d, t = "dir", i = "", d = "")
      }
      fi <- file.info(file.path(rv$folder, rv$listing$files))
      for (k in seq_along(rv$listing$files)) {
        f <- rv$listing$files[k]
        ext <- tolower(tools::file_ext(f))
        tp <- if (ext %in% c("xlsx", "xls", "xlsm")) { "excel" } else if (ext == "las") { "las" } else if (cd_is_data(f)) {
          "data" } else if (ext == "rds") { "rds" } else if (ext == "r") { "code" } else if (ext %in% c("pdf", "png")) { "fig" } else { "other" }
        items[[length(items) + 1]] <- list(v = paste0("f|", f), n = f, t = tp,
            i = cd_size(fi$size[k]), d = format(fi$mtime[k], "%d %b %Y %H:%M"))
      }
      sel <- if (is.null(select)) { as.character(shiny::isolate(input$files)) } else { paste0("f|", select) }
      vals <- character(length(items))
      for (k in seq_along(items)) {
        vals[k] <- items[[k]]$v
      }
      sel <- sel[sel %in% vals]
      has_pc <- length(rv$drives) > 0
      cr <- cd_crumbs(rv$folder)
      if (has_pc) {
        cr <- c(list(list(l = "This PC", p = RC_PC)), cr)
      }
      session$sendCustomMessage("cd_listing", list(items = items, selected = as.list(sel), crumbs = cr,
          up = dirname(rv$folder) != rv$folder || has_pc))
      shiny::updateTextInput(session, "path", value = rv$folder)
      rv$tick <- rv$tick + 1
    }
    set_folder <- function(path) {
      if (identical(path, RC_PC)) {
        rv$folder <- RC_PC
        refresh(select = character())
        return(invisible(TRUE))
      }
      path <- gsub("\\\\", "/", trimws(path))
      if (grepl("^[A-Za-z]:$", path)) {
        path <- paste0(path, "/")
      }
      if (!dir.exists(path)) {
        shiny::showNotification(paste("Folder not found:", path), type = "error")
        shiny::updateTextInput(session, "path", value = rv$folder)
        return(invisible(FALSE))
      }
      rv$folder <- normalizePath(path, winslash = "/")
      rv$xlim <- NULL
      cd_remember_folder(rv$folder)
      rv$recent <- cd_recent_folders()
      refresh(select = character())
      invisible(TRUE)
    }
    session$onFlushed(function() {
      shiny::isolate(refresh())
    }, once = TRUE)
    shiny::observeEvent(input$path_enter, {
      set_folder(input$path)
    })
    shiny::observeEvent(input$nav, {
      p <- input$nav$path
      if (identical(p, "..")) {
        if (identical(rv$folder, RC_PC)) {
          return()
        }
        if (dirname(rv$folder) == rv$folder && length(rv$drives)) {
          set_folder(RC_PC)
        } else {
          set_folder(dirname(rv$folder))
        }
      } else if (startsWith(p, "p|")) {
        set_folder(substring(p, 3))
      } else if (startsWith(p, "d|")) {
        set_folder(file.path(rv$folder, substring(p, 3)))
      } else {
        set_folder(p)
      }
    })
    output$places <- shiny::renderUI({
      rec <- rv$recent
      items <- list(cd_header("This computer"))
      home <- normalizePath(Sys.getenv("USERPROFILE", path.expand("~")), winslash = "/", mustWork = FALSE)
      pl <- c(Home = home, Documents = file.path(home, "Documents"), Desktop = file.path(home, "Desktop"))
      icn <- c(Home = "house", Documents = "folder", Desktop = "desktop")
      for (k in seq_along(pl)) {
        if (dir.exists(pl[[k]])) {
          items[[length(items) + 1]] <- cd_place(names(pl)[k], pl[[k]], icn[[k]])
        }
      }
      dr <- rv$drives
      if (length(dr)) {
        items[[length(items) + 1]] <- cd_place("This PC (all drives)", RC_PC, "desktop")
        for (k in seq_along(dr)) {
          items[[length(items) + 1]] <- cd_place(names(dr)[k], dr[[k]], "hard-drive")
        }
      } else {
        items[[length(items) + 1]] <- cd_place("/", "/", "hard-drive")
      }
      if (length(rec)) {
        items[[length(items) + 1]] <- cd_divider()
        items[[length(items) + 1]] <- cd_header("Recent folders")
        for (r in rec) {
          items[[length(items) + 1]] <- cd_place(basename(r), r, "clock-rotate-left", sub = dirname(r))
        }
      }
      shiny::tags$span(class = "dropdown cd-places",
          shiny::tags$button(class = "cd-ibtn cd-placesbtn dropdown-toggle", `data-toggle` = "dropdown", title = "Places",
              shiny::icon("star"), shiny::tags$span("Places"), shiny::icon("chevron-down")),
          shiny::tags$ul(class = "dropdown-menu", items))
    })
    output$drives <- shiny::renderUI({
      dr <- rv$drives
      if (!length(dr)) {
        return(NULL)
      }
      cur <- toupper(substr(rv$folder, 1, 2))
      chips <- list(shiny::tags$a(href = "#", class = paste("cd-drv cd-place", if (identical(rv$folder, RC_PC)) { "cd-on" } else { "" }),
          `data-path` = RC_PC, title = "This PC: all drives", shiny::icon("desktop")))
      for (k in seq_along(dr)) {
        lab <- names(dr)[k]
        chips[[length(chips) + 1]] <- shiny::tags$a(href = "#", class = paste("cd-drv cd-place",
            if (identical(toupper(substr(dr[[k]], 1, 2)), cur)) { "cd-on" } else { "" }), `data-path` = dr[[k]],
            title = paste("Go to drive", substr(lab, 1, 2)), shiny::icon("hard-drive"), substr(lab, 1, 2))
      }
      shiny::tags$div(class = "cd-drives", chips)
    })
    shiny::observeEvent(input$move_files, {
      m <- input$move_files
      tg <- as.character(m$target)
      if (identical(tg, "..")) {
        dest <- dirname(rv$folder)
      } else if (startsWith(tg, "d|")) {
        dest <- file.path(rv$folder, substring(tg, 3))
      } else if (startsWith(tg, "p|")) {
        dest <- substring(tg, 3)
      } else {
        dest <- tg
      }
      if (identical(dest, RC_PC) || !dir.exists(dest)) {
        shiny::showNotification("Drop the files on a folder.", type = "warning")
        return()
      }
      dest <- normalizePath(dest, winslash = "/")
      if (identical(dest, rv$folder)) {
        return()
      }
      fs <- as.character(unlist(m$files))
      fs <- substring(fs[startsWith(fs, "f|")], 3)
      res <- cd_move_files(file.path(rv$folder, fs), dest, copy = isTRUE(m$copy))
      verb <- if (isTRUE(m$copy)) { "Copied" } else { "Moved" }
      msg <- sprintf("%s %d file(s) to %s", verb, length(res$done), basename(dest))
      if (length(res$skipped)) {
        msg <- paste0(msg, sprintf("; %d skipped (a file with that name exists there): %s", length(res$skipped),
            paste(res$skipped, collapse = ", ")))
      }
      if (length(res$failed)) {
        msg <- paste0(msg, sprintf("; %d failed: %s", length(res$failed), paste(res$failed, collapse = ", ")))
      }
      say(msg)
      shiny::showNotification(msg, type = if (length(res$failed) || length(res$skipped)) { "warning" } else { "message" },
          duration = 6)
      refresh(select = character())
    })
    shiny::observeEvent(input$file_dblclick, {
      v <- input$file_dblclick$value
      if (grepl("\\.rds$", v, ignore.case = TRUE)) {
        open_rds(file.path(rv$folder, substring(v, 3)))
      } else if (grepl("\\.(pdf|png|r)$", v, ignore.case = TRUE)) {
        if (.Platform$OS.type == "windows") {
          shell_exec <- get("shell.exec", envir = baseenv())
          shell_exec(file.path(rv$folder, substring(v, 3)))
        }
      } else {
        shiny::updateTabsetPanel(session, "tabs", selected = "plot")
      }
    })

    selected_files <- shiny::reactive({
      v <- as.character(input$files)
      ord <- as.character(input$files_order)
      ord <- ord[ord %in% v]
      v <- c(ord, setdiff(v, ord))
      v <- v[startsWith(v, "f|")]
      v <- substring(v, 3)
      v[cd_is_data(v)]
    })

    # Reading files (cached by modification time) ####
    cache <- new.env()
    read_file <- function(name, full = FALSE) {
      f <- file.path(rv$folder, name)
      col <- rv$cols[[name]]
      if (is.null(col)) {
        col <- 2L
      }
      sh <- rv$sheets[[name]]
      key <- paste(f, file.mtime(f), full, col, if (is.null(sh)) { "" } else { sh })
      if (!is.null(cache[[key]])) {
        return(cache[[key]])
      }
      d <- read_series(f, sheet = sh)
      if (!full) {
        col <- min(max(2L, col), ncol(d))
        d <- d[, c(1L, col), drop = FALSE]
        d <- d[stats::complete.cases(d), , drop = FALSE]
      }
      assign(key, d, envir = cache)
      d
    }
    sel_data <- shiny::reactive({
      rv$tick
      fs <- selected_files()
      out <- list()
      for (f in fs) {
        d <- try(read_file(f), silent = TRUE)
        if (!inherits(d, "try-error") && ncol(d) >= 2) {
          out[[f]] <- d
        }
      }
      out
    })

    output$colpick <- shiny::renderUI({
      fs <- selected_files()
      if (length(fs) != 1) {
        return(NULL)
      }
      sheet_ui <- NULL
      if (tolower(tools::file_ext(fs)) %in% c("xlsx", "xls", "xlsm")) {
        shs <- cd_excel_sheets(file.path(rv$folder, fs))
        if (length(shs) > 1) {
          cs <- rv$sheets[[fs]]
          sheet_ui <- shiny::selectInput("sheet", "Sheet", choices = shs, selected = if (is.null(cs)) { shs[1] } else { cs })
        }
      }
      d <- try(read_file(fs, full = TRUE), silent = TRUE)
      if (inherits(d, "try-error") || ncol(d) <= 2) {
        return(sheet_ui)
      }
      ch <- seq(2, ncol(d))
      names(ch) <- names(d)[-1]
      cur <- rv$cols[[fs]]
      shiny::tagList(sheet_ui, shiny::selectInput("col", "Data column", choices = ch,
          selected = if (is.null(cur)) { 2 } else { cur }))
    })
    shiny::observeEvent(input$sheet, {
      fs <- shiny::isolate(selected_files())
      if (length(fs) == 1 && !identical(rv$sheets[[fs]], input$sheet)) {
        rv$sheets[[fs]] <- input$sheet
        rv$cols[[fs]] <- 2L
      }
    })
    shiny::observeEvent(input$col, {
      fs <- shiny::isolate(selected_files())
      if (length(fs) == 1) {
        if (!identical(rv$cols[[fs]], as.integer(input$col))) {
          rv$cols[[fs]] <- as.integer(input$col)
        }
      }
    })

    output$fileinfo <- shiny::renderUI({
      fs <- selected_files()
      if (!length(fs)) {
        return(shiny::tags$span(class = "cd-hint", "Click a folder to open it, the arrow or a part of the path above to go",
            " up. Click a data file to plot it; Ctrl/Shift-click selects several. Drag files onto a folder, drive or part of the",
            " path to move them there (hold Ctrl to copy). Arrow keys, Enter and Backspace also work."))
      }
      dl <- sel_data()
      rows <- list()
      for (f in names(dl)) {
        d <- dl[[f]]
        dt <- cd_dt(d[, 1])
        rows[[length(rows) + 1]] <- shiny::tags$tr(shiny::tags$td(shiny::tags$b(f)))
        rows[[length(rows) + 1]] <- shiny::tags$tr(shiny::tags$td(sprintf("%d points, %s to %s, dt %s%s", nrow(d),
            cd_fmt(min(d[, 1])), cd_fmt(max(d[, 1])), cd_fmt(dt), if (cd_is_even(sort(d[, 1]))) { "" } else { " (uneven)" })))
      }
      bad <- setdiff(fs, names(dl))
      if (length(bad)) {
        rows[[length(rows) + 1]] <- shiny::tags$tr(shiny::tags$td(style = "color:#c0392b",
            paste("Not readable as a series:", paste(bad, collapse = ", "))))
      }
      shiny::tags$table(rows)
    })

    output$status <- shiny::renderText({
      rv$tick
      sprintf("Folder: %s   |   %d data files   |   %d selected   |   WaverideR %s, astrochron %s",
          if (identical(rv$folder, RC_PC)) { "This PC" } else { rv$folder },
          length(rv$listing$files), length(selected_files()), as.character(utils::packageVersion("WaverideR")),
          as.character(utils::packageVersion("astrochron")))
    })

    # Plot tab ####
    shiny::observeEvent(input$pl_brush, {
      b <- input$pl_brush
      if (!is.null(b)) {
        if (isTRUE(input$pl_swap)) {
          rv$xlim <- sort(c(b$ymin, b$ymax))
        } else {
          rv$xlim <- sort(c(b$xmin, b$xmax))
        }
      }
    })
    shiny::observeEvent(input$pl_dbl, {
      rv$xlim <- NULL
    })
    main_plot <- function() {
      dl <- sel_data()
      labs <- names(dl)
      if (length(dl) == 1) {
        labs <- names(dl[[1]])[2]
      }
      cd_plot_series(unname(dl), labels = labs, stack = isTRUE(input$pl_stack), standardise = isTRUE(input$pl_std),
          type = if (isTRUE(input$pl_pts)) { "o" } else { "l" }, xlim = rv$xlim, rev_x = isTRUE(input$pl_rev),
          swap = isTRUE(input$pl_swap), xlab = if (length(dl) == 1) { names(dl[[1]])[1] } else { "Depth / time" })
    }
    output$mainplot <- shiny::renderPlot({
      main_plot()
    }, res = 96)

    # Data tab ####
    output$datatable <- shiny::renderTable({
      fs <- selected_files()
      if (!length(fs)) {
        return(NULL)
      }
      d <- try(read_file(fs[1], full = TRUE), silent = TRUE)
      if (inherits(d, "try-error")) {
        return(NULL)
      }
      utils::head(d, 500)
    }, digits = 6)
    output$stats <- shiny::renderUI({
      dl <- sel_data()
      if (!length(dl)) {
        return(shiny::tags$p(class = "cd-hint", "Select a data file."))
      }
      out <- list()
      for (f in names(dl)) {
        d <- dl[[f]]
        y <- d[, 2]
        dx <- diff(sort(d[, 1]))
        st <- c("Points" = nrow(d), "Start" = min(d[, 1]), "End" = max(d[, 1]), "Median interval" = stats::median(dx),
            "Min interval" = min(dx), "Max interval" = max(dx), "Mean" = mean(y), "Median" = stats::median(y),
            "Standard deviation" = stats::sd(y), "Minimum" = min(y), "Maximum" = max(y))
        rows <- list()
        for (k in seq_along(st)) {
          rows[[k]] <- shiny::tags$tr(shiny::tags$td(names(st)[k]), shiny::tags$td(cd_fmt(st[[k]])))
        }
        out[[length(out) + 1]] <- shiny::tags$h5(f)
        out[[length(out) + 1]] <- shiny::tags$table(class = "cd-stats", rows)
      }
      out
    })

    # Code and log tabs ####
    output$code <- shiny::renderText({
      hdr <- c("# R code written by CycloDesk", "library(WaverideR)", "library(astrochron)",
          sprintf("setwd(%s)", cd_dep(rv$folder)))
      paste(c(hdr, rv$code), collapse = "\n")
    })
    output$log <- shiny::renderText({
      paste(rv$log, collapse = "\n")
    })
    save_code <- function() {
      f <- file.path(rv$folder, sprintf("CycloDesk_session_%s.R", format(Sys.time(), "%Y%m%d_%H%M")))
      writeLines(c("# R code written by CycloDesk", "library(WaverideR)", "library(astrochron)",
          sprintf("setwd(%s)", cd_dep(rv$folder)), rv$code), f)
      say("R code saved: ", basename(f))
      shiny::showNotification(paste("Saved", basename(f)), type = "message")
      refresh()
    }
    shiny::observeEvent(input$code_save, {
      save_code()
    })
    shiny::observeEvent(input$code_clear, {
      rv$code <- character()
    })

    # Tools ####
    form_state <- new.env()
    form_state$last <- -1
    shiny::observeEvent(input$tool_reset, {
      rv$form_nonce <- rv$form_nonce + 1
    })
    open_tool <- function(id) {
      rv$form_nonce <- rv$form_nonce + 1
      rv$tool <- cd_get_tool(id)
      rv$tool_res <- NULL
      rv$tool_err <- NULL
      shiny::updateTabsetPanel(session, "tabs", selected = "tool")
    }

    output$tool_head <- shiny::renderUI({
      t <- rv$tool
      if (is.null(t)) {
        return(shiny::tags$p(class = "cd-hint", "Choose a tool from the Basic Series, Math or Time Series menu."))
      }
      shiny::tagList(shiny::tags$h4(t$label), shiny::tags$div(class = "cd-info", t$info),
          shiny::tags$div(class = "cd-uses", t$uses))
    })

    data_files <- shiny::reactive({
      rv$tick
      f <- rv$listing$files
      f[cd_is_data(f)]
    })

    sync <- new.env()
    sync$skip <- FALSE
    shiny::observeEvent(selected_files(), {
      sel <- selected_files()
      # the new result file is selected after a run: keep the tool on its input file (and its settings)
      if (isTRUE(sync$skip)) {
        sync$skip <- FALSE
        return()
      }
      if (length(sel) >= 1) {
        shiny::updateSelectInput(session, "tool_file1", selected = sel[1])
      }
      if (length(sel) >= 2) {
        s1 <- sel[1]
        s2 <- sel[2]
        if (identical(shiny::isolate(rv$tool$id), "tune") && grepl("agemodel", s1) && !grepl("agemodel", s2)) {
          s1 <- sel[2]
          s2 <- sel[1]
        }
        shiny::updateSelectInput(session, "tool_file1", selected = s1)
        shiny::updateSelectInput(session, "tool_file2", selected = s2)
      }
    })

    output$tool_files <- shiny::renderUI({
      t <- rv$tool
      if (is.null(t) || t$inputs < 1) {
        return(NULL)
      }
      fs <- data_files()
      sel <- shiny::isolate(selected_files())
      s1 <- if (length(sel)) { sel[1] } else { fs[1] }
      s2 <- if (length(sel) > 1) { sel[2] } else { fs[min(2, length(fs))] }
      if (t$id == "tune" && length(sel) > 1 && grepl("agemodel", s1) && !grepl("agemodel", s2)) {
        tmp <- s1
        s1 <- s2
        s2 <- tmp
      }
      shiny::tagList(
          shiny::selectInput("tool_file1", if (t$inputs > 1) { "Series 1" } else { "Data file" }, choices = fs, selected = s1),
          if (t$inputs > 1) {
            shiny::selectInput("tool_file2", if (t$id == "tune") { "Age model (depth, age)" } else { "Series 2" },
                choices = fs, selected = s2)
          } else {
            NULL
          })
    })

    tool_data <- function(which = 1) {
      t <- rv$tool
      nm <- if (which == 1) { input$tool_file1 } else { input$tool_file2 }
      if (is.null(nm) || !nzchar(nm)) {
        return(NULL)
      }
      d <- try(read_file(nm, full = isTRUE(t$full_table) && which == 1), silent = TRUE)
      if (inherits(d, "try-error")) {
        return(NULL)
      }
      d
    }

    output$tool_form <- shiny::renderUI({
      t <- rv$tool
      if (is.null(t)) {
        return(NULL)
      }
      d <- if (t$inputs >= 1) { tool_data(1) } else { NULL }
      if (t$inputs >= 1 && is.null(d)) {
        return(shiny::tags$p(class = "cd-hint", "Choose a readable data file."))
      }
      nonce <- rv$form_nonce
      keep <- identical(nonce, form_state$last)
      form_state$last <- nonce
      fnow <- if (t$inputs >= 1) { input$tool_file1 } else { "" }
      same_file <- identical(fnow, form_state$file)
      form_state$file <- fnow
      items <- list()
      for (q in t$params) {
        id <- cd_pid(t$id, q$id)
        v <- cd_default(q$value, d)
        cur <- shiny::isolate(input[[id]])
        if (keep && !is.null(cur) && (same_file || !is.function(q$value))) {
          v <- if (q$type == "nums") { cd_parse_nums(cur) } else { cur }
        }
        w <- switch(q$type,
            num = shiny::numericInput(id, q$label, value = v, min = q$min, max = q$max, step = q$step),
            select = {
              ch <- cd_default(q$choices, d)
              shiny::selectInput(id, q$label, choices = ch, selected = v)
            },
            check = shiny::checkboxInput(id, q$label, value = isTRUE(v)),
            text = shiny::textInput(id, q$label, value = v),
            nums = shiny::textInput(id, q$label, value = paste(v, collapse = ", ")))
        if (!is.null(q$help)) {
          w <- shiny::tagList(w, shiny::tags$div(class = "help-block", q$help))
        }
        if (!is.null(q$show_if)) {
          other <- cd_pid(t$id, q$show_if$param)
          vals <- q$show_if$values
          if (identical(vals, "true")) {
            cond <- sprintf("input['%s'] === true", other)
          } else {
            cond <- paste(sprintf("input['%s'] == '%s'", other, vals), collapse = " || ")
          }
          w <- shiny::conditionalPanel(cond, w)
        }
        items[[length(items) + 1]] <- w
      }
      if (length(t$params)) {
        items[[length(items) + 1]] <- shiny::actionLink("tool_reset", "Reset to the defaults for this file",
            icon = shiny::icon("rotate-left"))
      }
      shiny::tagList(items)
    })

    output$tool_buttons <- shiny::renderUI({
      t <- rv$tool
      if (is.null(t)) {
        return(NULL)
      }
      shiny::tags$div(class = "cd-buttons",
          shiny::checkboxInput("tool_write", "Write result files to the working folder", TRUE),
          if (!isFALSE(t$heavy)) { shiny::checkboxInput("tool_rds", "Save the result for re-plotting later (.rds)", TRUE) } else { NULL },
          shiny::actionButton("tool_run", "Run", icon = shiny::icon("play"), class = "btn-primary"),
          shiny::actionButton("tool_cancel", "Cancel", icon = shiny::icon("stop"), class = "btn-default"),
          shiny::actionButton("tool_savefig", "Save figure", icon = shiny::icon("image"), class = "btn-default"),
          if (t$id == "wavelet") {
            shiny::conditionalPanel("output.tool_has_wavelet == '1'",
                shiny::actionButton("tool_to_track", "Track a cycle on this scalogram", icon = shiny::icon("route"),
                    class = "btn-default"))
          } else {
            NULL
          })
    })

    finish_run <- function(res) {
      t <- cd_get_tool(res$tool)
      rv$tool_res <- res
      rv$tool_err <- NULL
      written <- character()
      write_ok <- isTRUE(shiny::isolate(input$tool_write)) && dir.exists(rv$folder)
      if (write_ok) {
        for (o in res$outputs) {
          f <- file.path(rv$folder, o$name)
          wr <- try(write_series(o$data, f, header = o$header), silent = TRUE)
          if (inherits(wr, "try-error")) {
            say("Could not write ", o$name, ": ", conditionMessage(attr(wr, "condition")))
          } else {
            written <- c(written, o$name)
          }
        }
        if (!isFALSE(t$heavy) && isTRUE(shiny::isolate(input$tool_rds))) {
          base <- if (is.null(res$file)) { t$id } else { tools::file_path_sans_ext(basename(res$file)) }
          rn <- sprintf("%s-%s.cyclodesk.rds", base, t$id)
          saveRDS(list(cyclodesk = TRUE, tool = t$id, params = res$params, result = res), file.path(rv$folder, rn))
          written <- c(written, rn)
        }
      }
      read_line <- if (t$inputs >= 1) { cd_code_read(res$file, col = if (isTRUE(t$full_table)) { NULL } else {
        rv$cols[[res$file]] }, sheet = rv$sheets[[res$file]]) } else { character() }
      add_code(c(read_line, res$code), paste0(t$label, if (is.null(res$file)) { "" } else { paste0(": ", res$file) }))
      say(t$label, " finished", if (length(written)) { paste0("; written: ", paste(written, collapse = ", ")) } else { "" })
      rv$tool_res$written <- written
      if (length(written)) {
        series <- character()
        for (o in res$outputs) {
          if (o$name %in% written && ncol(as.data.frame(o$data)) == 2 && !isTRUE(o$header)) {
            series <- c(series, o$name)
          }
        }
        if (length(series)) {
          if (!identical(shiny::isolate(selected_files()), series[1])) {
            sync$skip <- TRUE
          }
          refresh(select = series[1])
        } else {
          refresh()
        }
      }
    }

    shiny::observeEvent(input$tool_run, {
      t <- rv$tool
      if (is.null(t)) {
        return()
      }
      if (!is.null(rv$job)) {
        shiny::showNotification("A calculation is still running.", type = "warning")
        return()
      }
      p <- cd_read_params(t, input)
      d <- NULL
      d2 <- NULL
      f1 <- NULL
      f2 <- NULL
      if (t$inputs >= 1) {
        d <- tool_data(1)
        f1 <- input$tool_file1
        if (is.null(d)) {
          shiny::showNotification("Choose a readable data file.", type = "error")
          return()
        }
      }
      if (t$inputs >= 2) {
        d2 <- tool_data(2)
        f2 <- input$tool_file2
      }
      say(t$label, " started", if (is.null(f1)) { "" } else { paste0(" on ", f1) })
      heavy <- if (is.function(t$heavy)) { isTRUE(t$heavy(p)) } else { isTRUE(t$heavy) }
      if (heavy) {
        rv$job <- cd_job_start(t$id, d, p, d2, f1, f2)
        rv$job_info <- list(label = t$label, start = Sys.time())
        session$sendCustomMessage("cd_busy", list(text = paste("Running:", t$label, "...")))
        return()
      }
      res <- shiny::withProgress(message = paste(t$label, "..."), value = 0.5, {
        tryCatch(cd_run_tool(t$id, d = d, p = p, d2 = d2, file = f1, file2 = f2), error = function(e) { e })
      })
      if (inherits(res, "error")) {
        rv$tool_err <- conditionMessage(res)
        rv$tool_res <- NULL
        say(t$label, " failed: ", conditionMessage(res))
        shiny::showNotification(conditionMessage(res), type = "error", duration = 8)
        return()
      }
      finish_run(res)
    })

    shiny::observe({
      job <- rv$job
      if (is.null(job)) {
        return()
      }
      shiny::invalidateLater(400)
      if (job$is_alive()) {
        el <- as.numeric(difftime(Sys.time(), rv$job_info$start, units = "secs"))
        session$sendCustomMessage("cd_busy", list(text = sprintf("Running: %s ... %d s", rv$job_info$label, round(el))))
        return()
      }
      shiny::isolate({
        rv$job <- NULL
        session$sendCustomMessage("cd_busy", list(text = ""))
        res <- tryCatch(cd_job_result(job), error = function(e) { e })
        if (inherits(res, "error")) {
          rv$tool_err <- conditionMessage(res)
          rv$tool_res <- NULL
          say(rv$job_info$label, " failed: ", conditionMessage(res))
          shiny::showNotification(conditionMessage(res), type = "error", duration = 8)
        } else {
          finish_run(res)
        }
      })
    })

    shiny::observeEvent(input$tool_cancel, {
      if (!is.null(rv$job)) {
        try(rv$job$kill(), silent = TRUE)
        rv$job <- NULL
        session$sendCustomMessage("cd_busy", list(text = ""))
        say("Cancelled: ", rv$job_info$label)
      }
    })
    session$onSessionEnded(function() {
      j <- shiny::isolate(rv$job)
      if (!is.null(j)) {
        try(j$kill(), silent = TRUE)
      }
    })

    tool_plot <- function() {
      res <- rv$tool_res
      if (!is.null(rv$tool_err)) {
        cd_message_plot(paste("Error:", rv$tool_err))
        return(invisible(NULL))
      }
      if (is.null(res)) {
        t <- rv$tool
        if (!is.null(t) && is.function(t$preview) && is.null(rv$job)) {
          d <- tool_data(1)
          if (!is.null(d)) {
            pv <- try(t$preview(d, cd_read_params(t, input)), silent = TRUE)
            if (!inherits(pv, "try-error")) {
              return(invisible(NULL))
            }
          }
        }
        if (!is.null(rv$job)) {
          cd_message_plot("Calculating...")
        } else {
          cd_message_plot("Set the parameters and press Run")
        }
        return(invisible(NULL))
      }
      t <- cd_get_tool(res$tool)
      if (is.null(t$plot)) {
        cd_message_plot("Done")
        return(invisible(NULL))
      }
      r <- try(t$plot(res$result, res$params), silent = TRUE)
      if (inherits(r, "try-error")) {
        cd_message_plot(paste("Plot failed:", conditionMessage(attr(r, "condition"))))
      }
      invisible(NULL)
    }
    output$tool_plot <- shiny::renderPlot({
      tool_plot()
      # where the last panel lies, so the browser can draw the two movable lines of interval tools on it
      t <- rv$tool
      if (!is.null(t) && length(t$lines) == 2 && is.null(rv$tool_err)) {
        g <- list(usr = graphics::par("usr"), plt = graphics::par("plt"), fig = graphics::par("fig"),
            xlog = graphics::par("xlog"))
        vals <- shiny::isolate(lines_values())
        session$sendCustomMessage("cd_lines", list(show = TRUE, geom = g, a = vals[1], b = vals[2],
            labels = vals_labels(t)))
      } else {
        session$sendCustomMessage("cd_lines", list(show = FALSE))
      }
    }, res = 96)
    lines_values <- shiny::reactive({
      t <- rv$tool
      if (is.null(t) || length(t$lines) != 2) {
        return(c(NA, NA))
      }
      a <- suppressWarnings(as.numeric(input[[cd_pid(t$id, t$lines[1])]]))
      b <- suppressWarnings(as.numeric(input[[cd_pid(t$id, t$lines[2])]]))
      c(if (length(a)) { a } else { NA }, if (length(b)) { b } else { NA })
    })
    vals_labels <- function(t) {
      lab <- character(2)
      for (q in t$params) {
        if (q$id == t$lines[1]) {
          lab[1] <- q$label
        }
        if (q$id == t$lines[2]) {
          lab[2] <- q$label
        }
      }
      lab
    }
    # typed values move the lines
    shiny::observeEvent(lines_values(), {
      v <- lines_values()
      if (all(is.finite(v))) {
        session$sendCustomMessage("cd_lines_pos", list(a = v[1], b = v[2]))
      }
    })
    # dragged lines change the values
    shiny::observeEvent(input$lines_moved, {
      t <- rv$tool
      m <- input$lines_moved
      if (is.null(t) || length(t$lines) != 2) {
        return()
      }
      a <- as.numeric(m$a)
      b <- as.numeric(m$b)
      if (!is.finite(a) || !is.finite(b)) {
        return()
      }
      dg <- 4
      if (t$id %in% c("select", "remove")) {
        # depths: inside the record and on the sampling grid
        d <- tool_data(1)
        if (!is.null(d)) {
          r <- range(d[, 1], na.rm = TRUE)
          dt <- cd_dt(d[, 1])
          a <- min(max(a, r[1]), r[2])
          b <- min(max(b, r[1]), r[2])
          if (is.finite(dt) && dt > 0) {
            a <- r[1] + round((a - r[1]) / dt) * dt
            b <- r[1] + round((b - r[1]) / dt) * dt
          }
          dg <- 7
        }
      }
      shiny::updateNumericInput(session, cd_pid(t$id, t$lines[1]), value = signif(min(a, b), dg))
      shiny::updateNumericInput(session, cd_pid(t$id, t$lines[2]), value = signif(max(a, b), dg))
    })
    output$tool_has_wavelet <- shiny::reactive({
      if (!is.null(rv$tool_res) && identical(rv$tool_res$tool, "wavelet")) { "1" } else { "0" }
    })
    shiny::outputOptions(output, "tool_has_wavelet", suspendWhenHidden = FALSE)
    output$tool_text <- shiny::renderText({
      res <- rv$tool_res
      if (!is.null(rv$tool_err)) {
        return(paste("Error:", rv$tool_err))
      }
      if (is.null(res)) {
        return("")
      }
      w <- res$written
      paste(c(res$text, if (length(w)) { c("", "Written:", paste0("  ", w)) } else { NULL }), collapse = "\n")
    })

    open_rds <- function(f) {
      x <- try(readRDS(f), silent = TRUE)
      if (inherits(x, "try-error") || !isTRUE(x$cyclodesk)) {
        shiny::showNotification("Not an CycloDesk result file.", type = "error")
        return()
      }
      rv$tool <- cd_get_tool(x$tool)
      rv$tool_err <- NULL
      rv$tool_res <- x$result
      rv$tool_res$written <- character()
      shiny::updateTabsetPanel(session, "tabs", selected = "tool")
      say("Opened result ", basename(f))
    }

    shared <- shiny::reactiveValues(ver = 0, extract = NULL, extract_label = NULL, age = NULL, age_label = NULL,
        data = NULL, name = NULL)
    tctx <- list(rv = rv, read_file = read_file, refresh = refresh, say = say, add_code = add_code,
        data_files = data_files, selected_files = selected_files, shared = shared)
    cd_track_server(input, output, session, tctx)
    cd_anchor_server(input, output, session, tctx)

    # Figures ####
    fig_modal <- function(source) {
      base <- if (source == "tool" && !is.null(rv$tool_res)) {
        b <- if (is.null(rv$tool_res$file)) { rv$tool_res$tool } else { tools::file_path_sans_ext(rv$tool_res$file) }
        paste0(b, "-", rv$tool_res$tool)
      } else {
        fs <- selected_files()
        if (length(fs)) { tools::file_path_sans_ext(fs[1]) } else { "figure" }
      }
      shiny::showModal(shiny::modalDialog(title = "Save figure",
          shiny::textInput("fig_name", "File name (without extension)", base),
          shiny::selectInput("fig_fmt", "Format", c("PDF (editable in Illustrator)" = "pdf", "PNG (300 dpi)" = "png")),
          shiny::numericInput("fig_w", "Width (inches)", 10, min = 2),
          shiny::numericInput("fig_h", "Height (inches)", 6, min = 2),
          shiny::tags$input(type = "hidden", id = "fig_src", value = source),
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(paste0("fig_ok_", source), "Save"))))
    }
    save_fig <- function(source) {
      nm <- gsub("[^A-Za-z0-9_.-]", "_", input$fig_name)
      f <- file.path(rv$folder, paste0(nm, ".", input$fig_fmt))
      if (file.exists(f) && !file.remove(f)) {
        shiny::showNotification(paste(basename(f), "is open in another program"), type = "error")
        return()
      }
      if (input$fig_fmt == "pdf") {
        grDevices::pdf(f, width = input$fig_w, height = input$fig_h, onefile = FALSE, bg = "white", family = "Helvetica",
            encoding = "WinAnsi", useDingbats = FALSE)
      } else {
        grDevices::png(f, width = input$fig_w, height = input$fig_h, units = "in", res = 300, family = "sans", bg = "white")
      }
      ok <- try({
        if (source == "tool") {
          tool_plot()
        } else {
          main_plot()
        }
      }, silent = TRUE)
      grDevices::dev.off()
      shiny::removeModal()
      if (inherits(ok, "try-error")) {
        shiny::showNotification("Figure failed", type = "error")
      } else {
        say("Figure saved: ", basename(f))
        shiny::showNotification(paste("Saved", basename(f)), type = "message")
        refresh()
      }
    }
    shiny::observeEvent(input$tool_savefig, {
      fig_modal("tool")
    })
    shiny::observeEvent(input$pl_savefig, {
      fig_modal("plot")
    })
    shiny::observeEvent(input$fig_ok_tool, {
      save_fig("tool")
    })
    shiny::observeEvent(input$fig_ok_plot, {
      save_fig("plot")
    })

    # File and Edit commands ####
    one_file <- function() {
      fs <- as.character(input$files)
      fs <- fs[startsWith(fs, "f|") | startsWith(fs, "d|")]
      if (length(fs) != 1) {
        shiny::showNotification("Select exactly one file or folder.", type = "warning")
        return(NULL)
      }
      substring(fs, 3)
    }
    shiny::observeEvent(input$menu_action, {
      a <- input$menu_action$action
      if (startsWith(a, "tool:")) {
        open_tool(substring(a, 6))
        return()
      }
      cmd <- substring(a, 5)
      if (cmd == "refresh") {
        refresh()
      } else if (cmd == "showall") {
        rv$show_all <- !rv$show_all
        refresh()
      } else if (cmd %in% c("plot", "plotstack", "plotstd", "plotswap")) {
        shiny::updateCheckboxInput(session, "pl_stack", value = cmd == "plotstack")
        shiny::updateCheckboxInput(session, "pl_std", value = cmd == "plotstd")
        shiny::updateCheckboxInput(session, "pl_swap", value = cmd == "plotswap")
        shiny::updateTabsetPanel(session, "tabs", selected = "plot")
      } else if (cmd == "track") {
        shiny::updateTabsetPanel(session, "tabs", selected = "track")
      } else if (cmd == "anchor") {
        shiny::updateTabsetPanel(session, "tabs", selected = "anchor")
      } else if (cmd == "stats") {
        shiny::updateTabsetPanel(session, "tabs", selected = "data")
      } else if (cmd == "newfolder") {
        shiny::showModal(shiny::modalDialog(title = "New folder", shiny::textInput("nf_name", "Name", "new_folder"),
            footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("nf_ok", "Create"))))
      } else if (cmd == "newfile") {
        shiny::showModal(shiny::modalDialog(title = "New data file", size = "l",
            shiny::textInput("nd_name", "File name", "new_data.txt"),
            shiny::textAreaInput("nd_text", "Paste two (or more) columns: depth/time and values (from Excel or a text file)",
                rows = 14, width = "100%"),
            footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("nd_ok", "Save"))))
      } else if (cmd == "savefig") {
        if (identical(input$tabs, "tool")) {
          fig_modal("tool")
        } else {
          fig_modal("plot")
        }
      } else if (cmd == "savecode") {
        save_code()
      } else if (cmd == "explorer") {
        if (.Platform$OS.type == "windows") {
          shell_exec <- get("shell.exec", envir = baseenv())
          shell_exec(rv$folder)
        } else {
          shiny::showNotification(rv$folder, type = "message")
        }
      } else if (cmd == "quit") {
        session$sendCustomMessage("cd_close", list())
        shiny::stopApp()
      } else if (cmd == "rename") {
        f <- one_file()
        if (!is.null(f)) {
          shiny::showModal(shiny::modalDialog(title = "Rename", shiny::textInput("rn_name", "New name", f),
              shiny::tags$input(type = "hidden", id = "rn_old", value = f),
              footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("rn_ok", "Rename"))))
          rv$rename_from <- f
        }
      } else if (cmd == "duplicate") {
        f <- one_file()
        if (!is.null(f)) {
          b <- tools::file_path_sans_ext(f)
          e <- tools::file_ext(f)
          n <- file.path(rv$folder, sprintf("%s-copy.%s", b, e))
          k <- 2
          while (file.exists(n)) {
            n <- file.path(rv$folder, sprintf("%s-copy%d.%s", b, k, e))
            k <- k + 1
          }
          file.copy(file.path(rv$folder, f), n)
          say("Duplicated ", f, " as ", basename(n))
          refresh(select = basename(n))
        }
      } else if (cmd == "delete") {
        fs <- as.character(input$files)
        fs <- substring(fs[startsWith(fs, "f|")], 3)
        if (length(fs)) {
          shiny::showModal(shiny::modalDialog(title = "Delete files",
              shiny::tags$p(sprintf("Delete %d file(s) permanently?", length(fs))),
              shiny::tags$ul(cd_li_tags(fs)),
              footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("del_ok", "Delete", class = "btn-danger"))))
        }
      } else if (cmd == "about") {
        cd_about_modal()
      } else if (cmd == "manual") {
        session$sendCustomMessage("cd_open", list(url = "https://stratigraphy.github.io/CycloDesk/"))
      } else if (cmd == "feedback") {
        session$sendCustomMessage("cd_open", list(url = "https://github.com/stratigraphy/CycloDesk/issues/new/choose"))
      } else if (cmd == "update") {
        shiny::showNotification("Checking GitHub for updates ...", id = "cd_upd", duration = 4)
        start_update_check(force = TRUE)
      } else if (cmd == "functions") {
        cd_functions_modal()
      }
    })
    shiny::observeEvent(input$nf_ok, {
      nm <- gsub("[\\\\/:*?\"<>|]", "_", input$nf_name)
      dir.create(file.path(rv$folder, nm), showWarnings = FALSE)
      shiny::removeModal()
      say("New folder ", nm)
      refresh()
    })
    shiny::observeEvent(input$nd_ok, {
      nm <- gsub("[\\\\/:*?\"<>|]", "_", input$nd_name)
      if (!grepl("\\.", nm)) {
        nm <- paste0(nm, ".txt")
      }
      txt <- strsplit(input$nd_text, "\r?\n")[[1]]
      txt <- gsub(",", ".", gsub("[ \t;]+", "\t", trimws(txt)))
      f <- file.path(rv$folder, nm)
      if (file.exists(f)) {
        shiny::showNotification(paste(nm, "already exists"), type = "error")
        return()
      }
      writeLines(txt[nzchar(txt)], f)
      shiny::removeModal()
      say("New data file ", nm)
      refresh(select = nm)
    })
    shiny::observeEvent(input$rn_ok, {
      old <- rv$rename_from
      nm <- gsub("[\\\\/:*?\"<>|]", "_", input$rn_name)
      if (!is.null(old) && nzchar(nm) && !file.exists(file.path(rv$folder, nm))) {
        file.rename(file.path(rv$folder, old), file.path(rv$folder, nm))
        say("Renamed ", old, " to ", nm)
        shiny::removeModal()
        refresh(select = nm)
      } else {
        shiny::showNotification("That name exists already.", type = "error")
      }
    })
    shiny::observeEvent(input$del_ok, {
      fs <- as.character(input$files)
      fs <- substring(fs[startsWith(fs, "f|")], 3)
      for (f in fs) {
        file.remove(file.path(rv$folder, f))
      }
      say("Deleted ", paste(fs, collapse = ", "))
      shiny::removeModal()
      refresh(select = character())
    })
  }
}

cd_li_tags <- function(x) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- shiny::tags$li(x[i])
  }
  out
}

# Notification after an update check
cd_update_note <- function(st, force = FALSE) {
  if (any(st$update)) {
    what <- paste(st$package[st$update], collapse = " and ")
    shiny::showNotification(id = "cd_upd", duration = NULL, type = "warning",
        ui = shiny::tags$span(sprintf("A newer version of %s is available on GitHub.", what)),
        action = shiny::actionLink("upd_go", "Update and restart"))
    return(invisible(TRUE))
  }
  if (force) {
    txt <- character()
    for (i in seq_len(nrow(st))) {
      txt <- c(txt, sprintf("%s %s: %s", st$package[i], st$version[i], if (identical(st$origin[i], "local")) {
        "development install (not checked)" } else if (is.na(st$latest[i])) { "GitHub could not be reached" } else {
        "up to date" }))
    }
    shiny::showNotification(paste(txt, collapse = "; "), id = "cd_upd", type = "message", duration = 8)
  }
  invisible(FALSE)
}

cd_about_modal <- function() {
  link <- function(href, txt) {
    shiny::tags$a(href = href, target = "_blank", txt)
  }
  shiny::showModal(shiny::modalDialog(title = "About CycloDesk", size = "l", easyClose = TRUE, shiny::tags$div(class = "cd-about",
      shiny::tags$p(shiny::tags$b(sprintf("CycloDesk %s (alpha)", as.character(utils::packageVersion("CycloDesk")))),
          " is a point-and-click workbench for cyclostratigraphy by Michiel Arts. All calculations are done by the R",
          " packages WaverideR and astrochron; the R code of every step is in the R code tab."),
      shiny::tags$p("Source code, manual and feedback: ", link("https://github.com/stratigraphy/CycloDesk",
          "github.com/stratigraphy/CycloDesk"), ". Something wrong or missing? ",
          link("https://github.com/stratigraphy/CycloDesk/issues/new/choose", "Tell us"), "."),
      shiny::tags$p("When you publish results, please cite CycloDesk and the packages that did the work:"),
      shiny::tags$ul(
          shiny::tags$li(sprintf(paste0("Arts, M. (2026). CycloDesk: Graphical Interface for Cyclostratigraphy with WaverideR",
              " and astrochron. R package version %s. https://github.com/stratigraphy/CycloDesk"),
              as.character(utils::packageVersion("CycloDesk")))),
          shiny::tags$li("Arts, M., et al. (2024). Frontiers in Earth Science 12, 1357751. doi:10.3389/feart.2024.1357751",
              " (the paper that introduced WaverideR)."),
          shiny::tags$li("Arts, M. (2023). WaverideR: Extracting Signals from Wavelet Spectra. R package,",
              " https://CRAN.R-project.org/package=WaverideR (see citation(\"WaverideR\"))."),
          shiny::tags$li("Meyers, S.R. (2014). astrochron: An R Package for Astrochronology.",
              " https://CRAN.R-project.org/package=astrochron (see citation(\"astrochron\"))."),
          shiny::tags$li("The original method papers named in the help of each function (Tool tab, purple line).")),
      shiny::tags$p("The menu layout and working-folder workflow are inspired by Acycle: Li, M., Hinnov, L.A., Kump, L.R.",
          " (2019). Acycle: Time-series analysis software for paleoclimate research and education. Computers &",
          " Geosciences 127, 12-22. doi:10.1016/j.cageo.2019.02.011"))))
}

cd_functions_modal <- function() {
  tl <- cd_tools()
  rows <- vector("list", length(tl))
  for (i in seq_along(tl)) {
    rows[[i]] <- shiny::tags$tr(shiny::tags$td(tl[[i]]$menu), shiny::tags$td(tl[[i]]$label),
        shiny::tags$td(shiny::tags$code(tl[[i]]$uses)))
  }
  shiny::showModal(shiny::modalDialog(title = "Which R function does what", size = "l", easyClose = TRUE,
      shiny::tags$table(class = "table table-condensed", shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Menu"),
          shiny::tags$th("Tool"), shiny::tags$th("Function(s)"))), shiny::tags$tbody(rows))))
}

cd_place <- function(label, path, icon, sub = NULL) {
  shiny::tags$li(shiny::tags$a(href = "#", class = "cd-place", `data-path` = path, title = path,
      shiny::tags$span(class = "cd-ico", shiny::icon(icon)), shiny::tags$span(class = "cd-lab", label),
      if (!is.null(sub)) { shiny::tags$span(class = "cd-key", sub) } else { NULL }))
}

# Breadcrumb parts of a path: label and full path of every level
cd_crumbs <- function(path) {
  path <- gsub("\\\\", "/", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  out <- list()
  if (startsWith(path, "/")) {
    out[[1]] <- list(l = "/", p = "/")
    parts <- parts[nzchar(parts)]
    acc <- ""
    for (x in parts) {
      acc <- paste0(acc, "/", x)
      out[[length(out) + 1]] <- list(l = x, p = acc)
    }
    return(out)
  }
  parts <- parts[nzchar(parts)]
  acc <- ""
  for (k in seq_along(parts)) {
    acc <- if (k == 1) { paste0(parts[1], "/") } else { paste0(sub("/$", "", acc), "/", parts[k]) }
    out[[length(out) + 1]] <- list(l = parts[k], p = acc)
  }
  out
}

cd_size <- function(b) {
  if (is.na(b)) {
    return("")
  }
  if (b < 1024) {
    return(paste(b, "B"))
  }
  if (b < 1024^2) {
    return(sprintf("%.0f kB", b / 1024))
  }
  sprintf("%.1f MB", b / 1024^2)
}

cd_recent_file <- function() {
  file.path(tools::R_user_dir("CycloDesk", "config"), "recent_folders.txt")
}

cd_recent_folders <- function() {
  f <- cd_recent_file()
  if (!file.exists(f)) {
    return(character())
  }
  r <- readLines(f, warn = FALSE)
  r[dir.exists(r)]
}

cd_remember_folder <- function(path) {
  cf <- cd_config_file()
  dir.create(dirname(cf), recursive = TRUE, showWarnings = FALSE)
  try(writeLines(path, cf), silent = TRUE)
  r <- unique(c(path, cd_recent_folders()))
  try(writeLines(utils::head(r, 10), cd_recent_file()), silent = TRUE)
}

# Name of the virtual folder that lists all drives
RC_PC <- "::pc"

# Drives of this computer: named character vector, label = "C:  (label)", value = "C:/"
cd_drives <- function() {
  test <- getOption("CycloDesk.test_drives")
  if (!is.null(test)) {
    return(test)
  }
  if (.Platform$OS.type != "windows") {
    return(character())
  }
  out <- character()
  for (L in LETTERS[3:26]) {
    dr <- paste0(L, ":/")
    if (dir.exists(dr)) {
      out[paste0(L, ":")] <- dr
    }
  }
  if (!length(out)) {
    return(out)
  }
  nm <- names(out)
  for (k in seq_along(out)) {
    v <- try(suppressWarnings(system2("cmd", c("/c", "vol", nm[k]), stdout = TRUE, stderr = FALSE)), silent = TRUE)
    if (!inherits(v, "try-error") && length(v)) {
      m <- regmatches(v[1], regexec(" is (.+)$", v[1]))[[1]]
      if (length(m) == 2 && nzchar(trimws(m[2]))) {
        nm[k] <- paste0(nm[k], "  ", trimws(m[2]))
      }
    }
  }
  names(out) <- nm
  out
}

# Move (or copy) files into a folder; never overwrites
cd_move_files <- function(files, dest, copy = FALSE) {
  done <- character()
  skipped <- character()
  failed <- character()
  for (f in files) {
    to <- file.path(dest, basename(f))
    if (file.exists(to)) {
      skipped <- c(skipped, basename(f))
      next
    }
    ok <- FALSE
    if (copy) {
      ok <- file.copy(f, to, copy.date = TRUE)
    } else {
      ok <- suppressWarnings(file.rename(f, to))
      if (!ok) {
        # another drive: copy, then remove the original only when the copy is complete
        ok <- file.copy(f, to, copy.date = TRUE) && isTRUE(file.size(to) == file.size(f))
        if (ok) {
          ok <- file.remove(f)
        }
      }
    }
    if (ok) {
      done <- c(done, basename(f))
    } else {
      failed <- c(failed, basename(f))
    }
  }
  list(done = done, skipped = skipped, failed = failed)
}
