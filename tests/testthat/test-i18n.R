# Translations: the dictionary files are well formed and texts are translated where they are shown
test_that("every language file is well formed", {
  dir <- CycloDesk:::cd_i18n_dir()
  files <- list.files(dir, pattern = "^[a-z]{2}\\.tsv$", full.names = TRUE)
  expect_true(length(files) >= 5)
  langs <- CycloDesk:::cd_languages()
  expect_identical(names(langs)[1], "en")
  expect_true(all(c("fr", "nl", "de", "es", "zh") %in% names(langs)))
  count <- function(s, pat) {
    lengths(regmatches(s, gregexpr(pat, s, fixed = TRUE)))
  }
  for (f in files) {
    x <- readLines(f, encoding = "UTF-8", warn = FALSE)
    expect_true(any(grepl("^# language: .+", x[1:3])), info = f)
    x <- x[nzchar(x) & !startsWith(x, "#")]
    expect_true(length(x) > 1500, info = f)
    expect_true(all(count(x, "\t") == 1), info = f)
    p <- regexpr("\t", x, fixed = TRUE)
    en <- substr(x, 1, p - 1)
    tr <- substr(x, p + 1, nchar(x))
    expect_true(all(nzchar(en)) && all(nzchar(tr)), info = f)
    expect_false(anyDuplicated(en) > 0, info = f)
    expect_true(all(count(en, "%s") == count(tr, "%s")), info = f)
    expect_true(all(count(en, "%d") == count(tr, "%d")), info = f)
  }
})

test_that("texts, tags and select options are translated; unknown texts stay English", {
  old <- CycloDesk:::cd_state$lang
  on.exit(assign("lang", old, envir = CycloDesk:::cd_state), add = TRUE)
  CycloDesk:::cd_set_lang("fr", remember = FALSE)
  expect_identical(CycloDesk:::cd_lang(), "fr")
  expect_identical(CycloDesk:::cd_tr("Cancel"), "Annuler")
  expect_identical(CycloDesk:::cd_tr("  Cancel "), "  Annuler ")
  expect_identical(CycloDesk:::cd_tr("no such text in CycloDesk 123"), "no such text in CycloDesk 123")
  expect_identical(CycloDesk:::cd_tr(c("Cancel", "", NA)), c("Annuler", "", NA))
  ui <- shiny::tags$div(title = "Cancel", shiny::tags$button("Cancel"), shiny::tags$span(class = "cd-notr", "Cancel"),
      shiny::tags$pre("Cancel"), shiny::selectInput("x", "Cancel", c("Cancel" = "a", "zzz" = "b")))
  html <- as.character(CycloDesk:::cd_tr_tags(ui))
  expect_match(html, "title=\"Annuler\"", fixed = TRUE)
  expect_match(html, "<button>Annuler</button>", fixed = TRUE)
  expect_match(html, "<span class=\"cd-notr\">Cancel</span>", fixed = TRUE)
  expect_match(html, "<pre>Cancel</pre>", fixed = TRUE)
  expect_match(html, "<option value=\"a\" selected>Annuler</option>", fixed = TRUE)
  expect_match(html, "<option value=\"b\">zzz</option>", fixed = TRUE)
  txt <- CycloDesk:::cd_trf("Delete %d file(s) from the working folder? This cannot be undone.", 3L)
  expect_match(txt, "3", fixed = TRUE)
  expect_false(grepl("Delete", txt, fixed = TRUE))
  page <- as.character(CycloDesk:::cd_tr_tags(CycloDesk:::cd_ui()))
  expect_match(page, "Fichier", fixed = TRUE)
  expect_match(page, "data-action=\"tool:spectrum\"", fixed = TRUE)
  CycloDesk:::cd_set_lang("en", remember = FALSE)
  expect_identical(CycloDesk:::cd_tr("Cancel"), "Cancel")
  expect_identical(CycloDesk:::cd_tr_tags(ui), ui)
})

test_that("the language of the computer is recognised", {
  expect_identical(CycloDesk:::cd_system_lang("fr_FR.UTF-8"), "fr")
  expect_identical(CycloDesk:::cd_system_lang("French_France.utf8"), "fr")
  expect_identical(CycloDesk:::cd_system_lang("Dutch_Netherlands.1252"), "nl")
  expect_identical(CycloDesk:::cd_system_lang("zh_CN.UTF-8"), "zh")
  expect_identical(CycloDesk:::cd_system_lang("C"), "en")
  expect_identical(CycloDesk:::cd_system_lang("pl_PL.UTF-8"), "en")
})

test_that("the choices of radio buttons and tick-box groups are translated", {
  old <- CycloDesk:::cd_state$lang
  on.exit(assign("lang", old, envir = CycloDesk:::cd_state), add = TRUE)
  CycloDesk:::cd_set_lang("fr", remember = FALSE)
  rb <- shiny::radioButtons("x", "Starting age model", c("a constant sedimentation rate" = "sr", "zzz & <b>" = "z"))
  html <- as.character(CycloDesk:::cd_tr_tags(rb))
  expect_match(html, "un taux de sédimentation constant", fixed = TRUE)
  expect_false(grepl("a constant sedimentation rate", html, fixed = TRUE))
  expect_match(html, "zzz &amp; &lt;b&gt;", fixed = TRUE)
  cb <- shiny::checkboxGroupInput("y", "Show below the scalogram", c("extracted cycle" = "ext"))
  expect_match(as.character(CycloDesk:::cd_tr_tags(cb)), "cycle extrait", fixed = TRUE)
  page <- as.character(CycloDesk:::cd_tr_tags(CycloDesk:::cd_ui()))
  expect_match(page, "Analyse et modèles d'âge", fixed = TRUE)
  expect_match(page, "Code R et journal", fixed = TRUE)
  expect_false(grepl("the nearest maximum", page, fixed = TRUE))
})
