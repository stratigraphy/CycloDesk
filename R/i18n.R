# Languages ####
#
# The interface is written in English; a translation is a text file inst/i18n/<code>.tsv with two tab-separated
# columns, the English text and its translation (UTF-8; "\n" stands for a line break, lines that start with # are
# comments, and the comment line "# language: <name>" gives the name shown in the Help > Language menu). A text that
# is not in the file stays English, so a translation can be partial, and a new language is added by adding a file.
#
# Texts are translated where they are shown, not where they are written: cd_tr_tags() walks through an interface
# element (a shiny tag) and replaces every text, tooltip and placeholder it finds in the dictionary. The whole page
# (cd_ui), every renderUI (cd_render_ui), every dialog (cd_show_modal) and every notification (cd_notify) pass through
# it, so tools need no code for translation. Figures, result texts, the R code and the log stay English.

cd_i18n_dir <- function() {
  d <- system.file("i18n", package = "CycloDesk")
  if (!nzchar(d)) {
    d <- file.path("inst", "i18n")
  }
  d
}

# Languages with a translation file, as c(code = "name"); English is always first
cd_languages <- function() {
  out <- c(en = "English")
  files <- sort(list.files(cd_i18n_dir(), pattern = "^[a-z]{2}\\.tsv$", full.names = TRUE))
  for (f in files) {
    code <- sub("\\.tsv$", "", basename(f))
    head <- readLines(f, n = 5, warn = FALSE, encoding = "UTF-8")
    nm <- grep("^# language:", head, value = TRUE)
    out[[code]] <- if (length(nm)) { trimws(sub("^# language:", "", nm[1])) } else { code }
  }
  out
}

cd_lang_file <- function() {
  file.path(tools::R_user_dir("CycloDesk", "config"), "language.txt")
}

# The language of the computer (from the locale), when CycloDesk has a translation for it
cd_system_lang <- function(locale = NULL) {
  if (is.null(locale)) {
    locale <- c(Sys.getenv("LANGUAGE"), Sys.getenv("LC_ALL"), Sys.getenv("LANG"),
        suppressWarnings(tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) { "" })))
  }
  names_win <- c(french = "fr", dutch = "nl", german = "de", spanish = "es", chinese = "zh", portuguese = "pt",
      italian = "it", english = "en")
  have <- names(cd_languages())
  for (loc in locale[nzchar(locale)]) {
    low <- tolower(loc)
    code <- substr(low, 1, 2)
    if (grepl("^[a-z]{2}([_.@-]|$)", low) && code %in% have) {
      return(code)
    }
    for (n in names(names_win)) {
      if (startsWith(low, n) && names_win[[n]] %in% have) {
        return(names_win[[n]])
      }
    }
  }
  "en"
}

# The language in use: chosen in Help > Language (remembered), else the language of the computer
cd_lang <- function() {
  if (!is.null(cd_state$lang)) {
    return(cd_state$lang)
  }
  lang <- getOption("CycloDesk.lang")
  if (is.null(lang)) {
    f <- cd_lang_file()
    if (file.exists(f)) {
      lang <- trimws(readLines(f, n = 1, warn = FALSE))
    }
  }
  if (is.null(lang) || !length(lang) || !(lang %in% names(cd_languages()))) {
    lang <- cd_system_lang()
  }
  cd_state$lang <- lang
  lang
}

cd_set_lang <- function(lang, remember = TRUE) {
  if (!(lang %in% names(cd_languages()))) {
    lang <- "en"
  }
  cd_state$lang <- lang
  if (remember) {
    f <- cd_lang_file()
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    try(writeLines(lang, f), silent = TRUE)
  }
  invisible(lang)
}

# Dictionary of a language: an environment with the English text (white space squeezed) as key
cd_dictionary <- function(lang) {
  if (is.null(cd_state$dict)) {
    cd_state$dict <- list()
  }
  if (!is.null(cd_state$dict[[lang]])) {
    return(cd_state$dict[[lang]])
  }
  env <- new.env(hash = TRUE, parent = emptyenv())
  f <- file.path(cd_i18n_dir(), paste0(lang, ".tsv"))
  if (file.exists(f)) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    x <- x[nzchar(x) & !startsWith(x, "#")]
    for (ln in x) {
      p <- regexpr("\t", ln, fixed = TRUE)
      if (p < 2 || p >= nchar(ln)) {
        next
      }
      key <- cd_i18n_key(gsub("\\n", "\n", substr(ln, 1, p - 1), fixed = TRUE))
      val <- gsub("\\n", "\n", substr(ln, p + 1, nchar(ln)), fixed = TRUE)
      if (nzchar(key) && nzchar(val)) {
        assign(key, val, envir = env)
      }
    }
  }
  cd_state$dict[[lang]] <- env
  env
}

cd_i18n_key <- function(x) {
  trimws(gsub("[ \t]+", " ", x))
}

# With options(CycloDesk.i18n_collect = "<file>") every text that passes through the translation is added to that
# file (once): this is how the list of texts for a new translation is made
cd_i18n_seen <- function(keys) {
  f <- getOption("CycloDesk.i18n_collect")
  if (is.null(f)) {
    return(invisible(NULL))
  }
  if (is.null(cd_state$seen)) {
    cd_state$seen <- new.env(hash = TRUE, parent = emptyenv())
  }
  for (k in keys) {
    if (nzchar(k) && grepl("[A-Za-z]{2}", k) && is.null(cd_state$seen[[k]])) {
      assign(k, TRUE, envir = cd_state$seen)
      cat(gsub("\n", "\\n", k, fixed = TRUE), "\n", sep = "", file = f, append = TRUE)
    }
  }
  invisible(NULL)
}

# Translation of texts (a character vector); texts without a translation are returned as they are
cd_tr <- function(x, lang = cd_lang()) {
  if (!is.character(x) || !length(x)) {
    return(x)
  }
  collect <- !is.null(getOption("CycloDesk.i18n_collect"))
  if (identical(lang, "en") && !collect) {
    return(x)
  }
  dict <- cd_dictionary(lang)
  for (i in seq_along(x)) {
    s <- x[i]
    if (is.na(s) || !nzchar(s)) {
      next
    }
    key <- cd_i18n_key(s)
    if (collect) {
      cd_i18n_seen(key)
    }
    if (!nzchar(key) || nchar(key) > 4000) {
      next
    }
    val <- dict[[key]]
    if (!is.null(val)) {
      lead <- regmatches(s, regexpr("^[ \t\n]*", s))
      trail <- regmatches(s, regexpr("[ \t\n]*$", s))
      x[i] <- paste0(lead, val, trail)
    }
  }
  x
}

# A translated text with %s and %d replaced by the values, in order
cd_trf <- function(fmt, ...) {
  out <- cd_tr(fmt)
  # the values are put in by hand: sprintf() would turn the accented letters of a translation into codes in a session
  # that does not use UTF-8
  for (a in list(...)) {
    p <- regexpr("%[sd]", out)
    if (p < 0) {
      break
    }
    out <- paste0(substr(out, 1, p - 1), as.character(a), substr(out, p + 2, nchar(out)))
  }
  gsub("%%", "%", out, fixed = TRUE)
}

# Text in HTML that shiny has already written: the entities back to characters, and characters to entities
cd_html_unesc <- function(s) {
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  s <- gsub("&#39;", "'", s, fixed = TRUE)
  gsub("&amp;", "&", s, fixed = TRUE)
}
cd_html_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

# The labels of radio buttons and check-box groups: shiny writes each one as a piece of HTML that holds text only
cd_tr_html_text <- function(html, lang) {
  out <- html
  for (i in seq_along(html)) {
    if (!is.na(html[i]) && !grepl("<", html[i], fixed = TRUE)) {
      en <- cd_html_unesc(html[i])
      tr <- cd_tr(en, lang)
      if (!identical(tr, en)) {
        out[i] <- cd_html_esc(tr)
      }
    }
  }
  attributes(out) <- attributes(html)
  out
}

# Labels of the <option> and <optgroup> elements in the HTML that shiny writes for a select input
cd_tr_options <- function(html, lang) {
  unesc <- cd_html_unesc
  esc <- cd_html_esc
  out <- html
  for (i in seq_along(html)) {
    h <- html[i]
    m <- gregexpr("(<option[^>]*>)([^<]*)(</option>)", h)
    parts <- regmatches(h, m)[[1]]
    if (length(parts)) {
      new <- parts
      for (k in seq_along(parts)) {
        lab <- sub("^<option[^>]*>([^<]*)</option>$", "\\1", parts[k])
        tr <- cd_tr(unesc(lab), lang)
        if (!identical(tr, unesc(lab))) {
          new[k] <- paste0(sub("^(<option[^>]*>).*$", "\\1", parts[k]), esc(tr), "</option>")
        }
      }
      regmatches(h, m) <- list(new)
    }
    m <- gregexpr("<optgroup label=\"[^\"]*\"", h)
    parts <- regmatches(h, m)[[1]]
    if (length(parts)) {
      new <- parts
      for (k in seq_along(parts)) {
        lab <- sub("^<optgroup label=\"([^\"]*)\"$", "\\1", parts[k])
        tr <- cd_tr(unesc(lab), lang)
        new[k] <- paste0("<optgroup label=\"", gsub("\"", "&quot;", esc(tr), fixed = TRUE), "\"")
      }
      regmatches(h, m) <- list(new)
    }
    out[i] <- h
  }
  attributes(out) <- attributes(html)
  out
}

# Translates the texts, tooltips and placeholders of an interface element. Elements of the classes cd-notr and
# cd-uses (file names, R function names), scripts, styles and code are left as they are.
cd_tr_tags <- function(x, lang = cd_lang()) {
  if (identical(lang, "en") && is.null(getOption("CycloDesk.i18n_collect"))) {
    return(x)
  }
  cd_tr_walk(x, lang)
}

cd_tr_walk <- function(x, lang) {
  if (is.null(x) || is.function(x)) {
    return(x)
  }
  if (is.character(x)) {
    if (inherits(x, "html")) {
      if (any(grepl("<option", x, fixed = TRUE))) {
        return(cd_tr_options(x, lang))
      }
      return(cd_tr_html_text(x, lang))
    }
    at <- attributes(x)
    x <- cd_tr(x, lang)
    attributes(x) <- at
    return(x)
  }
  if (inherits(x, "shiny.tag")) {
    if (x$name %in% c("script", "style", "pre", "code", "head", "option")) {
      return(x)
    }
    cls <- paste(unlist(x$attribs[names(x$attribs) == "class"]), collapse = " ")
    if (grepl("(^| )(cd-notr|cd-uses|shiny-text-output|shiny-html-output)( |$)", cls)) {
      return(x)
    }
    for (a in which(names(x$attribs) %in% c("title", "placeholder", "data-placeholder"))) {
      v <- x$attribs[[a]]
      if (is.character(v) && length(v) == 1) {
        x$attribs[[a]] <- cd_tr(v, lang)
      }
    }
    kids <- x$children
    for (i in seq_along(kids)) {
      kids[i] <- list(cd_tr_walk(kids[[i]], lang))
    }
    x$children <- kids
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(cd_tr_walk(x[[i]], lang))
    }
    return(x)
  }
  x
}

# shiny::renderUI() whose result is translated
cd_render_ui <- function(expr, env = parent.frame()) {
  fun <- NULL
  shiny::installExprFunction(substitute(expr), "fun", env, quoted = TRUE)
  shiny::renderUI({
    cd_tr_tags(fun())
  })
}

# shiny::showModal() of a translated dialog
cd_show_modal <- function(ui, session = shiny::getDefaultReactiveDomain()) {
  shiny::showModal(cd_tr_tags(ui), session = session)
}

# shiny::showNotification() of a translated message
cd_notify <- function(ui, ...) {
  shiny::showNotification(if (is.character(ui)) { cd_tr(ui) } else { cd_tr_tags(ui) }, ...)
}

# Texts used by the script of the page (file list, right-click menu, closing page), translated
cd_js_texts <- function() {
  en <- c("No data files here.", "Open another folder or drive above.", "Up one level", "Plot", "Rename...", "Duplicate",
      "Delete...", "Delete %d files...", "%d files", "Open the folder", "Files", "CycloDesk is closed",
      "R has stopped. You can close this window.", "Start CycloDesk again from its icon.")
  out <- as.list(cd_tr(en))
  names(out) <- en
  out
}
