# Tool framework: parameter specifications and the tool registry ####
#
# Every menu item that opens a dialog is a tool. A tool has parameters (shown as a form), a compute function that
# returns the result, the files to write and the equivalent R code, and a plot function that draws the result on the
# current graphics device. Tools are collected in cd_tools(); adding an Acycle menu item means adding one tool.

# Parameter constructors
cd_num <- function(id, label, value, min = NA, max = NA, step = NA, help = NULL, show_if = NULL) {
  list(id = id, label = label, type = "num", value = value, min = min, max = max, step = step, help = help,
      show_if = show_if)
}
cd_sel <- function(id, label, choices, value = NULL, help = NULL, show_if = NULL) {
  if (is.null(value) && !is.function(choices)) {
    value <- unname(choices)[1]
  }
  list(id = id, label = label, type = "select", choices = choices, value = value, help = help, show_if = show_if)
}
cd_chk <- function(id, label, value = FALSE, help = NULL, show_if = NULL) {
  list(id = id, label = label, type = "check", value = value, help = help, show_if = show_if)
}
cd_txt <- function(id, label, value = "", help = NULL, show_if = NULL, numeric = FALSE) {
  list(id = id, label = label, type = if (numeric) { "nums" } else { "text" }, value = value, help = help,
      show_if = show_if)
}

# Tool constructor
# lines: ids of two numeric parameters shown as two movable vertical lines on the figure (e.g. an interval);
# preview: function(d, p) that draws the figure before the tool has run
cd_tool <- function(id, menu, label, compute, plot = NULL, params = list(), inputs = 1L, heavy = FALSE,
    info = "", group = NULL, full_table = FALSE, uses = "", lines = NULL, preview = NULL) {
  list(id = id, menu = menu, group = group, label = label, compute = compute, plot = plot, params = params,
      inputs = as.integer(inputs), heavy = heavy, info = info, full_table = full_table, uses = uses, lines = lines,
      preview = preview)
}

# Input id of a parameter in the Shiny form
cd_pid <- function(tool_id, param_id) {
  paste0("p__", tool_id, "__", param_id)
}

# Defaults that depend on the data are given as functions of the data frame
cd_default <- function(v, d) {
  if (is.function(v)) {
    out <- try(v(d), silent = TRUE)
    if (inherits(out, "try-error")) {
      return(NA)
    }
    return(out)
  }
  v
}

# Default parameter list of a tool (used by tests and scripts)
cd_defaults <- function(tool, d = NULL) {
  p <- list()
  for (q in tool$params) {
    v <- cd_default(q$value, d)
    if (q$type == "nums") {
      v <- cd_parse_nums(v)
    }
    p[q$id] <- list(v)
  }
  p
}

cd_parse_nums <- function(s) {
  if (is.numeric(s)) {
    return(s)
  }
  s <- trimws(as.character(s))
  if (!nzchar(s)) {
    return(NULL)
  }
  v <- suppressWarnings(as.numeric(strsplit(s, "[,; ]+")[[1]]))
  v[!is.na(v)]
}

# Read the parameter values from Shiny input
cd_read_params <- function(tool, input) {
  p <- list()
  for (q in tool$params) {
    v <- input[[cd_pid(tool$id, q$id)]]
    if (q$type == "num") {
      v <- suppressWarnings(as.numeric(v))
    } else if (q$type == "check") {
      v <- isTRUE(v)
    } else if (q$type == "nums") {
      v <- cd_parse_nums(v)
    } else if (is.null(v)) {
      v <- ""
    }
    p[q$id] <- list(v)
  }
  p
}

# Run one tool: returns list(result, outputs, text, code, tool, params)
cd_run_tool <- function(tool_id, d = NULL, p = list(), d2 = NULL, file = NULL, file2 = NULL) {
  tool <- cd_get_tool(tool_id)
  if (tool$inputs >= 1 && is.null(d)) {
    stop("select a data file first", call. = FALSE)
  }
  if (tool$inputs >= 2 && is.null(d2)) {
    stop("select a second data file", call. = FALSE)
  }
  base <- if (is.null(file)) { "series" } else { file }
  out <- tool$compute(d = d, p = p, d2 = d2, file = base, file2 = file2)
  if (is.null(out$outputs)) {
    out$outputs <- list()
  }
  if (is.null(out$text)) {
    out$text <- character()
  }
  if (is.null(out$code)) {
    out$code <- character()
  }
  out$tool <- tool_id
  out$params <- p
  out$file <- file
  out$file2 <- file2
  out
}

cd_get_tool <- function(id) {
  tl <- cd_tools()
  for (t in tl) {
    if (identical(t$id, id)) {
      return(t)
    }
  }
  stop("unknown tool: ", id, call. = FALSE)
}

# Registry of all tools, in menu order
cd_tools <- function() {
  c(cd_tools_plot(), cd_tools_basic(), cd_tools_math(), cd_tools_timeseries())
}

# Output description helper
cd_output <- function(name, data, header = NULL, var = "res") {
  list(name = name, data = data, header = header, var = var)
}

# R code helpers: an input line and the call
cd_code_read <- function(file, col = NULL, var = "dat", sheet = NULL) {
  if (is.null(file)) {
    return(character())
  }
  sprintf("%s <- CycloDesk::read_series(%s%s%s)", var, cd_dep(basename(file)),
      if (is.null(col)) { "" } else { sprintf(", col = %d", as.integer(col)) },
      if (is.null(sheet)) { "" } else { sprintf(", sheet = %s", cd_dep(sheet)) })
}
cd_code_write <- function(outputs) {
  out <- character()
  for (o in outputs) {
    out <- c(out, sprintf("CycloDesk::write_series(%s, %s)", o$var, cd_dep(o$name)))
  }
  out
}
