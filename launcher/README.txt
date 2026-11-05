CycloDesk - point-and-click cyclostratigraphy with WaverideR and astrochron (Windows launcher)
================================================================================================

Install
  1. Install R from https://cloud.r-project.org/bin/windows/base/ (default options are fine).
  2. Unzip it somewhere permanent, e.g. into Documents\Apps (not in Downloads); this gives the folder CycloDesk.
  3. Double-click SETUP.bat. It installs CycloDesk, WaverideR and the packages they need (a few minutes the
     first time) and puts the shortcut "CycloDesk" on the desktop.

Start
  Double-click "CycloDesk" on the desktop. The interface opens in your browser at http://127.0.0.1:7792; it runs
  on your own computer and your data stay there. Closing the browser tab (or File > Quit CycloDesk) stops R.

Updates
  Once a day CycloDesk checks GitHub for newer versions of CycloDesk and WaverideR and offers
  "Update and restart". Help > Check for updates checks at once. Running SETUP.bat again also updates.

Problems, questions, ideas
  https://github.com/stratigraphy/CycloDesk/issues   (state/setup_log.txt and state/update_log.txt help)
  Manual and tutorial: https://stratigraphy.github.io/CycloDesk/

Remove
  Delete the desktop shortcut and this folder; in R: remove.packages("CycloDesk").
