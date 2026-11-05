# CycloDesk 0.1.0 (alpha)

First public release, for testing and feedback. Menus, defaults and file names may still change.

* Working-folder workflow: file browser with drives, places, filter, drag-and-drop (Ctrl = copy); text, csv
  (also with semicolons and decimal commas), Excel and LAS files.
* Plot, Basic Series, Math and Time Series menus with about 40 tools, all calling WaverideR or astrochron; every
  result is written next to its input with an Acycle-style suffix, and the R code of each step is collected in
  the R code tab.
* Spectral analysis (MTM, LOWSPEC, MESA, AMTM, SWA, ...), evolutive spectra, wavelets (22 mother wavelets),
  superlets and synchrosqueezing with 17 colour palettes, coherence, cross wavelet and bispectral analysis.
* Track tab: track a cycle by hand on the scalogram (snap to peaks, free points, HMM ridge following, snapping
  the whole curve to the nearest peaks, LOWESS smoothing), helper lines for other cycles, boundary lines,
  floating age model, extraction of any cycle, and a tuning check with the amplitude modulation.
* Anchor tab: tie a floating record to La2004/La2010/La2011, insolation or a metronome by clicking matching
  peaks.
* Filtering, age models, tuning, TimeOpt/eTimeOpt, COCO/eCOCO, rho1, DYNOT, PDA and spectral moments.
* Windows launcher (SETUP.bat) with a desktop shortcut; checks once a day for new versions of CycloDesk and
  WaverideR on GitHub.
