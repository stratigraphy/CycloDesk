# Menu layout ####
#
# The menus follow the order of the work: make or load a series (Basic Series), prepare it (Processing), analyse
# it (Time Series), estimate sedimentation rates (Sed. rate), build and check age models (Age models), and the
# astronomy behind it (Astronomy). Every tool is listed here once, by its id; cd_tools() takes each tool's menu and
# group from this list and returns the tools in this order. Entries that start with "cmd:" are commands of the
# interface (the Track, Anchor and Image views). A group named " " is shown directly in the menu, not as a submenu.
#
# The tabs follow the same order: Data, Plot, Processing (the menus Basic Series and Processing, and the colour
# record from an image), Analysis & age models (the other menus, with Track and Anchor) and R code & log.
# cd_stages() says which menus belong to which of the two work tabs.

cd_menu_layout <- function() {
  list(
      "Plot" = list(
          " " = c("sampling", "distribution")),
      "Basic Series" = list(
          "Astronomical solutions and insolation" = c("astro_solution", "insolation", "etp", "ab_insoseries",
              "ab_insomap", "ab_imbrie"),
          "Synthetic records" = c("generator", "ab_synth", "mx_noise", "ab_sedmodel", "am_sedramp", "ab_synthstrat",
              "ab_bioturb", "at_impulse"),
          " " = c("examples", "cmd:image")),
      "Processing" = list(
          "Clean and select" = c("sort", "select", "remove", "trim", "clip", "column", "ab_clean"),
          "Interpolate and resample" = c("interpolate", "ab_lh13", "ab_resample", "mx_lithlog", "ab_rank"),
          "Transform" = c("transform", "ab_transform", "derivative", "smooth", "ab_taper", "ab_accum"),
          "Detrend and pre-whiten" = c("detrend", "ab_lowpass", "prewhiten"),
          "Statistics" = c("ab_strats", "ab_mwstats", "ab_mwcor", "mx_lag1", "ab_xplot", "ab_pca", "mx_extremes",
              "ab_peaks"),
          "Change points" = c("changepoint", "mx_bcp_r2")),
      "Time Series" = list(
          "Spectra" = c("spectrum", "wv_spectra", "dc_lomb", "at_spectest", "at_testbg"),
          "Evolutive spectra" = c("evolutive", "at_bandpower", "wv_chirp_eha"),
          "Wavelets and superlets" = c("wavelet", "wv_transform", "wv_chirp_wavelet", "wv_dechirped",
              "wv_resolution"),
          "Significance and surrogates" = c("wv_significance", "wv_sst_significance", "wv_band_significance",
              "wv_surrogates", "at_surrogates"),
          "Filtering and cycle extraction" = c("filter", "amplitude", "wv_extract", "wv_extract_track",
              "wv_dynamic_filter"),
          "Two series: coherence and phase" = c("coherence", "xwavelet", "wv_xsuperlet", "dc_xspec", "at_correlate"),
          "Decomposition (EMD, SSA)" = c("dc_emd", "dc_ssa", "dc_pulse", "dc_ratio", "dc_precision"),
          "Nonlinear analysis" = c("bispectral", "at_bicoherence", "dc_recurrence"),
          "Sedimentary noise and power" = c("rho1", "dynot", "pda", "sr_pda_legacy", "spectral_moments")),
      "Sed. rate" = list(
          "Track a cycle or a rate" = c("cmd:track", "track_auto", "sr_hmm", "sr_chirp", "sr_mesa", "sr_flmw",
              "at_freq2sedrate"),
          "TimeOpt" = c("timeopt", "etimeopt", "sr_wto_multi", "at_timeopt_sim", "at_timeopt_template", "at_timeoptb",
              "at_timeopt_mcmc", "at_etimeopt_track"),
          "COCO" = c("coco", "ecoco", "sr_coco", "sr_ecoco", "sr_coco_series"),
          "Other astronomical tests" = c("at_asm", "sr_astro_test", "sr_astro_multi")),
      "Age models" = list(
          "Build an age model" = c("agemodel", "am_mintune", "sedrate2age", "am_convert", "tune"),
          "Anchor to absolute ages" = c("cmd:anchor", "at_anchor", "at_wtmean", "as_gts"),
          "Dated levels (Bayesian)" = c("am_bchron", "am_astrobchron"),
          "Uncertainty (Monte Carlo)" = c("am_retrack", "am_unc", "am_gaps", "wv_uncertainty"),
          "Check an age model" = c("am_check", "at_am_test", "wv_transients"),
          "Correlate two records" = c("dc_dtw")),
      "Astronomy" = list(
          "Astronomical periods" = c("as_periods", "as_periods_time", "at_periods", "as_term_search", "as_bundling",
              "at_tones", "as_chaos", "at_priors"),
          "Precession constant" = c("as_ctest", "as_ctest_tuned", "as_k", "as_k_combine", "as_test_tuned",
              "as_test_check", "as_compare"),
          "Earth-Moon system" = c("as_earthmoon", "as_em_relations", "at_alod")))
}

# Commands of the interface that appear among the tools of a menu: label and icon
cd_menu_commands <- function() {
  list("cmd:track" = list(label = "Track a cycle by hand", icon = "route"),
      "cmd:anchor" = list(label = "Anchor to an astronomical solution or metronome", icon = "anchor"),
      "cmd:image" = list(label = "Colour record from an image (outcrop, core, tree rings)...", icon = "image"))
}

# The two work tabs: their menus and the views that open in them (the tool view and the interactive views)
cd_stages <- function() {
  list(proc = list(label = "Processing", menus = c("Plot", "Basic Series", "Processing"),
          views = c(tool = "Tools", image = "Image")),
      ana = list(label = "Analysis & age models", menus = c("Time Series", "Sed. rate", "Age models", "Astronomy"),
          views = c(tool = "Tools", track = "Track", anchor = "Anchor")))
}

# The work tab of a menu ("proc" or "ana")
cd_menu_stage <- function(menu) {
  st <- cd_stages()
  for (s in names(st)) {
    if (menu %in% st[[s]]$menus) {
      return(s)
    }
  }
  "ana"
}

# The tab that shows a panel of the interface: its own tab, or the work tab it opens in. tool_stage is the work
# tab the tool view was last used in.
cd_panel_stage <- function(panel, tool_stage = "proc") {
  if (is.null(panel) || !nzchar(panel)) {
    return("plot")
  }
  if (identical(panel, "tool")) {
    return(tool_stage)
  }
  st <- cd_stages()
  for (s in names(st)) {
    if (panel %in% names(st[[s]]$views)) {
      return(s)
    }
  }
  panel
}

# The menus in the order of the menu bar
cd_tool_menus <- function() {
  setdiff(names(cd_menu_layout()), "Plot")
}

# Menu and group of every tool id: a data frame with the columns id, menu, group and pos (the order)
cd_menu_table <- function() {
  lay <- cd_menu_layout()
  id <- character()
  menu <- character()
  group <- character()
  for (m in names(lay)) {
    for (g in names(lay[[m]])) {
      ids <- lay[[m]][[g]]
      id <- c(id, ids)
      menu <- c(menu, rep(m, length(ids)))
      group <- c(group, rep(trimws(g), length(ids)))
    }
  }
  data.frame(id = id, menu = menu, group = group, pos = seq_along(id), stringsAsFactors = FALSE)
}

# Gives every tool its menu and group from the layout and sorts the tools in the order of the layout; a tool that
# is not in the layout keeps its own menu and group and comes last
cd_apply_menu_layout <- function(tl) {
  tab <- cd_menu_table()
  pos <- numeric(length(tl))
  for (i in seq_along(tl)) {
    k <- match(tl[[i]]$id, tab$id)
    if (is.na(k)) {
      pos[i] <- nrow(tab) + i
    } else {
      tl[[i]]$menu <- tab$menu[k]
      tl[[i]]$group <- if (nzchar(tab$group[k])) { tab$group[k] } else { NULL }
      pos[i] <- tab$pos[k]
    }
  }
  tl[order(pos)]
}

# Start page of a work tab: its tools by menu and group, with a search box ####
#
# stage = "proc" or "ana" gives the tools of that tab; NULL gives all tools.
cd_tool_index <- function(tl = cd_tools(), stage = NULL) {
  lay <- cd_menu_layout()
  cmds <- cd_menu_commands()
  what <- c("Basic Series" = "Make or load a series", "Processing" = "Prepare the data",
      "Time Series" = "Spectra, wavelets, filters", "Sed. rate" = "Sedimentation rates from astronomical cycles",
      "Age models" = "Depth to time, anchoring, uncertainty", "Astronomy" = "Periods, precession constant, Earth-Moon")
  cols <- list()
  menus <- cd_tool_menus()
  if (!is.null(stage)) {
    menus <- intersect(menus, cd_stages()[[stage]]$menus)
  }
  for (m in menus) {
    blocks <- list()
    for (g in names(lay[[m]])) {
      links <- list()
      for (id in lay[[m]][[g]]) {
        lab <- NULL
        act <- NULL
        if (startsWith(id, "cmd:")) {
          lab <- cmds[[id]]$label
          act <- id
        } else {
          for (t in tl) {
            if (identical(t$id, id)) {
              lab <- t$label
              act <- paste0("tool:", id)
            }
          }
        }
        if (!is.null(lab)) {
          links[[length(links) + 1]] <- shiny::tags$a(href = "#", class = "cd-act cd-idx-tool", `data-action` = act, lab)
        }
      }
      blocks[[length(blocks) + 1]] <- shiny::tags$div(class = "cd-idx-group",
          if (nzchar(trimws(g))) { shiny::tags$div(class = "cd-idx-gname", g) } else { NULL }, links)
    }
    cols[[length(cols) + 1]] <- shiny::tags$div(class = "cd-idx-menu",
        shiny::tags$div(class = "cd-idx-mname", m), shiny::tags$div(class = "cd-idx-what", what[[m]]), blocks)
  }
  shiny::tags$div(class = "cd-idx",
      shiny::tags$div(class = "cd-idx-top",
          shiny::tags$div(class = "cd-idx-title", "Choose a tool"),
          shiny::tags$div(class = "cd-hint", "Every tool of the menus, in the order of the work. Its settings open here on",
              " the left and its result on the right; the result files are written to the working folder."),
          shiny::tags$div(class = "cd-idx-search", shiny::icon("magnifying-glass"),
              shiny::tags$input(id = "cd_idx_filter", type = "text", autocomplete = "off",
                  placeholder = "Find a tool, e.g. wavelet, detrend, TimeOpt, tuning"))),
      shiny::tags$div(class = "cd-idx-cols", cols),
      shiny::tags$div(class = "cd-idx-none cd-hint", style = "display:none", "No tool with these words."))
}
