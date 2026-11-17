# CycloDesk 0.2.3 (alpha)

* **Windows installer: installing a new version over an earlier one works in one go.** The installer used to refuse
  the folder of the earlier CycloDesk ("Directory ... is not empty", again at every click on Next), so the old
  version had to be uninstalled and the installer started a second time. It now asks once whether to replace the
  earlier CycloDesk, runs its uninstaller and goes on; if CycloDesk is still running it asks to close it first.
  Data files are not affected: CycloDesk does not keep them in its own folder.

# CycloDesk 0.2.2 (alpha)

* **Menus in the order of the work**, with submenus by subject: Basic Series (make or load a series), Processing
  (was Math; now also detrending and pre-whitening), Time Series (spectra, evolutive spectra, wavelets, significance,
  filtering, two series, decomposition, nonlinear analysis, noise), Sed. rate (tracking, TimeOpt, COCO, other tests),
  Age models (building, anchoring, dated levels, uncertainty, checks, correlation) and Astronomy. The separate
  Wavelets menu is now part of Time Series, and the geological time scale moved from Astronomy to Age models >
  Anchor to absolute ages. The place of every tool is set in one list (`R/menus.R`).
* **Five tabs in the order of the work** (were eight): Data, Plot, Processing, Analysis & age models, R code & log.
  Processing holds the tools of the menus Basic Series and Processing and the colour record from an image;
  Analysis & age models holds the tools of Time Series, Sed. rate, Age models and Astronomy, with Track and Anchor.
  Track, Anchor and Image are views inside these two tabs (a small row under the tabs), no longer tabs of their
  own. Each of the two tabs starts with a list of its tools and a search box, and comes back to the view it showed
  last. The R code and the log are side by side in one tab.
* **The Plot tab shows every kind of file:** data series, spectra written by the spectrum tools (power with its
  confidence levels and labelled peaks, on a log axis) and saved results (`.cyclodesk.rds`, for example a
  scalogram). New option "Log value axis".
* **File panel:** drag its edge to make it wider (full file names) or collapse it to a ribbon for a larger work
  area; the width is remembered. A delete button and a right-click menu (plot, rename, duplicate, delete, open the
  folder) next to Edit > Delete and the Del key.
* **Languages:** the interface (menus, tabs, tool names, settings, help texts, dialogs) is available in English,
  French, Dutch, German, Spanish and Simplified Chinese. CycloDesk starts in the language of the computer when it
  has that language; change it under Help > Language. Each language is one text file (`inst/i18n/<code>.tsv`), so
  corrections and new languages are easy to add (see `inst/i18n/README.md`). Figures, result texts, the R code and
  the log stay English. The choices of radio buttons and tick-box groups are translated as well.
* **Geological time scale** (Age models > Anchor to absolute ages): shows the table of the installed WaverideR, which
  now follows the International Chronostratigraphic Chart 2026/06 (it was the chart of 2016, with some errors). The
  result files are named `GTS_...` (were `GTS2020_...`).
* PDF, PNG and R files and the working folder open with the system's program on macOS and Linux as well.

# CycloDesk 0.2.1 (alpha)

* **CycloDesk is a desktop program:** the installers start it in its own window (a small native program,
  `installer/shell`), with its own icon in the taskbar or Dock; closing the window closes CycloDesk. Links to other
  sites open in the web browser.
* Windows installer: CycloDesk crashed at start-up on some computers ("Mingw-w64 runtime failure: 32 bit pseudo
  relocation ... out of range"), depending on where Windows placed the packages spam and fields in memory. These are
  now built from source; the installer test checks every package for this problem.
* The installed CycloDesk ignores the settings and packages of another R on the same computer.

# CycloDesk 0.2.0 (alpha)

* **Installers for Windows, macOS and Linux** (`installer/`, GitHub workflow `installers`): one file per system
  (`.exe`, `.pkg` for Apple silicon and Intel, `.sh`) that brings its own R (conda-forge) with all packages,
  installs without administrator rights and adds a desktop/menu icon. Built and tested on GitHub for every release
  and attached to it. "Update and restart" now also works in these installed copies on all three systems.
* Four new menus give access to most of WaverideR (about 50 more tools):
  * **Wavelets:** adaptive wavelet, superlet and chirplet synchrosqueezing, ConceFT, chirp-matched EHA and wavelet,
    de-chirped scalograms, wavelet/SST/band significance and surrogates, extraction of signal, power and amplitude
    (stable band or along a track), dynamic filtering, transients, Bayesian/SWA/MESA spectra, frequency and
    wavelet uncertainty.
  * **Sed. rate:** astronomical HMM and iterative tracking, chirp-steered tracking and rate-grams, FLMW, MESA
    tracking, cross-validated and adaptive (e)COCO with ridge tracking, windowed TimeOpt with several fits, the
    astronomical sedimentation-rate and tuning tests, legacy PDA.
  * **Age models:** minimal tuning, conversion with a track, sedimentation rate or anchor points, Monte Carlo
    re-tracking and age-model uncertainty (floating or anchored), gap durations, modified Bchron and astroMCBchron,
    age-model check against the de-chirped record, exponential sedimentation ramps.
  * **Astronomy:** astronomical periods at any age and through time, term identification, bundling ratios, chaotic
    resonance, CTest and the precession constant, tests of tuned records, the Earth-Moon system, the geological time
    scale.
  * **Math > More (WaverideR):** maxima/minima, lag-1 autocorrelation, litholog discretisation, change-point R2,
    coloured noise.
* About 50 tools for the astrochron functions that were not yet reachable, and for DecomposeR, lomb and dtw:
  * **astrochron:** cleaning, tapers, resampling at the levels of a second file, Laepple-Huybers interpolation,
    peaks, moving-window statistics and correlation, summary statistics, cross-plots, stratPCA, accumulation rates,
    rank series (Math > More (astrochron)); lowpass/highpass/kernel detrending; synthetic series, insolation
    series and maps, the Imbrie ice-volume model, sedimentation-rate ramps and differential accumulation, synthetic
    stratigraphy, bioturbation (Basic Series); spectral peak tests with multiple-testing corrections, testBackground,
    EHA band power, WOSA bicoherence, surrogates, surrogateCor/slideCor, the bioturbation impulse response (Time
    Series); TimeOpt with Monte Carlo significance, timeOptTemplate, Bayesian TimeOpt and MCMC, eTimeOptTrack,
    ASM/eASM, testPrecession/testTilt, freq2sedrate (Sed. rate); anchorTime, wtMean/stepHeat (Age models);
    bergerPeriods/calcPeriods, hoangPriors, alodfromk, tones (Astronomy).
  * **DecomposeR** (Time Series > Empirical mode decomposition): EMD and ensemble EMD, singular spectrum analysis,
    instantaneous frequency and amplitude of the modes, ratios between modes, precision of the Hilbert frequency.
  * **Other packages:** Lomb-Scargle periodogram for uneven sampling (lomb), Fourier cross-spectral coherence and
    phase, recurrence plots, correlation of two records by dynamic time warping (dtw, Age models).
  * DecomposeR, lomb and dtw are suggested packages: the launcher installs them, and a tool that needs a missing one
    says how to install it.
* Menus keep the tools of one group together, with the astrochron and other-package groups next to the CycloDesk
  group of the same name.
* **Track > Helper lines:** groups of lines are added and removed in a list, each with its own colour, line
  type and label. Line types: periods of cycles (kyr) scaled by the current track or by an age-model file; fixed
  periods in depth units; or any column of a file against a depth column, times a multiplier (e.g. 4.05 turns a
  sedimentation rate in cm/kyr into the 405-kyr period in m). Boundary lines can be read from a column of a file and
  cleared with one button; typed or clicked boundaries keep 6 significant digits (were 3, which rounded depths
  above 999). After Add, and below the scalogram, CycloDesk says how many helper lines there are and which lie
  outside the period range shown (for example an age column picked as a period). On a sedimentation-rate map
  (eCOCO, eTimeOpt) fixed values and file columns are drawn as rates (cm/kyr).
* **Image tab** (File > Colour record from an image): click a route on an outcrop photograph, core scan, tree-ring
  image or satellite image, set the scale with a scale bar, and get grey value, L\*a\*b\*, RGB and HSV averaged over a
  swath along the route (`cd_image_profile()`).
* **Track tab** works on every map: wavelet, superlet and synchrosqueezed scalograms, EHA, windowed FFT, EHA on a
  log2 axis and evolutive MESA (period tracks), and eTimeOpt, win_timeOpt and eCOCO (sedimentation-rate tracks,
  converted directly to an age model). "Track on this map" in the Tool tab opens any of them.
* **Zoom in the Tool tab:** drag a box on the figure or type the x and y limits; double-click resets. Spectra label
  the peaks inside the zoomed range, so a peak can be read off and used for a filter band.
* **Padding** of sliding-window analyses (EHA, eTimeOpt, eCOCO): mirror or mean padding by half a window, so the
  result spans the whole record (`cd_pad_ends()`).
* Citation: "Arts, M. (2026). CycloDesk: Graphical Interface for Cyclostratigraphy. R package version 0.2.0.
  https://github.com/stratigraphy/CycloDesk".
* Fixes: evolutive figures failed with "figure margins too large" in narrow windows; a clearer message when eTimeOpt
  cannot test the chosen rates; long menus scroll.

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
