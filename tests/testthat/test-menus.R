# The menu layout lists every tool once, and the interface builds from it
test_that("every tool has one place in the menu layout", {
  raw <- c(CycloDesk:::cd_tools_plot(), CycloDesk:::cd_tools_basic(), CycloDesk:::cd_tools_math(),
      CycloDesk:::cd_tools_math_extra(), CycloDesk:::cd_tools_timeseries(), CycloDesk:::cd_tools_wavelets(),
      CycloDesk:::cd_tools_sedrate(), CycloDesk:::cd_tools_agemodels(), CycloDesk:::cd_tools_astronomy(),
      CycloDesk:::cd_tools_astrochron_basic(), CycloDesk:::cd_tools_astrochron_testing(),
      CycloDesk:::cd_tools_decompose())
  ids <- character()
  for (t in raw) {
    ids <- c(ids, t$id)
  }
  tab <- CycloDesk:::cd_menu_table()
  lay <- tab$id[!startsWith(tab$id, "cmd:")]
  expect_false(anyDuplicated(lay) > 0)
  expect_setequal(lay, ids)
  expect_true(all(tab$id[startsWith(tab$id, "cmd:")] %in% names(CycloDesk:::cd_menu_commands())))
  tl <- CycloDesk:::cd_tools()
  expect_length(tl, length(ids))
  got <- character()
  for (t in tl) {
    got <- c(got, t$id)
    k <- match(t$id, tab$id)
    expect_identical(t$menu, tab$menu[k])
  }
  expect_identical(got, lay)
})

test_that("the menus, the tool index and the interface build", {
  for (m in CycloDesk:::cd_tool_menus()) {
    expect_true(length(CycloDesk:::cd_tool_items(m)) >= 1, info = m)
  }
  idx <- as.character(CycloDesk:::cd_tool_index())
  expect_match(idx, "tool:spectrum", fixed = TRUE)
  expect_match(idx, "cmd:track", fixed = TRUE)
  ui <- as.character(CycloDesk:::cd_ui())
  expect_match(ui, "cd-submenu", fixed = TRUE)
  expect_match(ui, "Analysis &amp; age models", fixed = TRUE)
  astro <- paste(as.character(shiny::tagList(CycloDesk:::cd_tool_items("Astronomy"))), collapse = "")
  expect_false(grepl("Geological time scale", astro, fixed = TRUE))
  expect_match(astro, "Earth-Moon", fixed = TRUE)
})

test_that("spectrum tables are recognised and plotted; series plot on a log axis", {
  d <- data.frame(frequency = seq(0.01, 0.5, by = 0.01), power = exp(-seq(0.01, 0.5, by = 0.01) * 5),
      AR1_fit = 0.5, AR1_95_power = 0.8)
  expect_true(CycloDesk:::cd_is_spectrum_table(d))
  expect_false(CycloDesk:::cd_is_spectrum_table(data.frame(depth = 1:10, value = 1:10)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(CycloDesk:::cd_plot_spectrum_table(d, logy = TRUE))
  expect_no_error(CycloDesk:::cd_plot_spectrum_table(d, logy = FALSE))
  s <- data.frame(x = 1:50, y = c(0, exp(seq(0, 3, length.out = 49))))
  expect_no_error(CycloDesk:::cd_plot_series(list(s), logy = TRUE))
  expect_no_error(CycloDesk:::cd_plot_series(list(s, s), stack = TRUE, logy = TRUE))
  expect_no_error(CycloDesk:::cd_plot_series(list(s), swap = TRUE, logy = TRUE))
})

test_that("the file-panel state is read back as written", {
  old <- Sys.getenv("R_USER_CONFIG_DIR", NA)
  tmp <- tempfile("cdconf")
  dir.create(tmp)
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  on.exit(if (is.na(old)) { Sys.unsetenv("R_USER_CONFIG_DIR") } else { Sys.setenv(R_USER_CONFIG_DIR = old) }, add = TRUE)
  expect_equal(CycloDesk:::cd_ui_state_read(), list(width = 340, collapsed = FALSE))
  CycloDesk:::cd_ui_state_write(512.4, TRUE)
  expect_equal(CycloDesk:::cd_ui_state_read(), list(width = 512, collapsed = TRUE))
})

test_that("the five tabs: every menu and panel has its work tab", {
  st <- CycloDesk:::cd_stages()
  expect_identical(names(st), c("proc", "ana"))
  menus <- names(CycloDesk:::cd_menu_layout())
  staged <- c(st$proc$menus, st$ana$menus)
  expect_setequal(staged, menus)
  expect_false(anyDuplicated(staged) > 0)
  expect_identical(CycloDesk:::cd_menu_stage("Processing"), "proc")
  expect_identical(CycloDesk:::cd_menu_stage("Basic Series"), "proc")
  expect_identical(CycloDesk:::cd_menu_stage("Time Series"), "ana")
  expect_identical(CycloDesk:::cd_menu_stage("Age models"), "ana")
  expect_identical(CycloDesk:::cd_panel_stage("data"), "data")
  expect_identical(CycloDesk:::cd_panel_stage("plot"), "plot")
  expect_identical(CycloDesk:::cd_panel_stage("code"), "code")
  expect_identical(CycloDesk:::cd_panel_stage("image"), "proc")
  expect_identical(CycloDesk:::cd_panel_stage("track"), "ana")
  expect_identical(CycloDesk:::cd_panel_stage("anchor"), "ana")
  expect_identical(CycloDesk:::cd_panel_stage("tool", "ana"), "ana")
  expect_identical(CycloDesk:::cd_panel_stage("tool", "proc"), "proc")
  expect_identical(CycloDesk:::cd_panel_stage(NULL), "plot")
  proc <- as.character(CycloDesk:::cd_tool_index(stage = "proc"))
  ana <- as.character(CycloDesk:::cd_tool_index(stage = "ana"))
  expect_match(proc, "tool:detrend", fixed = TRUE)
  expect_match(proc, "cmd:image", fixed = TRUE)
  expect_false(grepl("tool:spectrum", proc, fixed = TRUE))
  expect_match(ana, "tool:spectrum", fixed = TRUE)
  expect_match(ana, "cmd:track", fixed = TRUE)
  expect_match(ana, "cmd:anchor", fixed = TRUE)
  expect_false(grepl("tool:detrend\"", ana, fixed = TRUE))
  ui <- as.character(CycloDesk:::cd_ui())
  tabs <- regmatches(ui, gregexpr("data-action=\"stage:[a-z]+\"", ui))[[1]]
  expect_identical(tabs, sprintf("data-action=\"stage:%s\"", c("data", "plot", "proc", "ana", "code")))
  expect_match(ui, "R code &amp; log", fixed = TRUE)
  expect_false(grepl("Process &amp; analyse", ui, fixed = TRUE))
  for (v in c("view:proc:tool", "view:proc:image", "view:ana:tool", "view:ana:track", "view:ana:anchor")) {
    expect_match(ui, sprintf("data-action=\"%s\"", v), fixed = TRUE)
  }
  for (p in c("data", "plot", "tool", "track", "anchor", "image", "code")) {
    expect_match(ui, sprintf("data-value=\"%s\"", p), fixed = TRUE)
  }
})
