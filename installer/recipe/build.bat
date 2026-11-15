@echo off
rem Installs the R source packages listed in install_order.txt (in that order), the menu files and the CycloDesk
rem window program (built by build_installer.R into shell_bin\)
setlocal enabledelayedexpansion
set "RCMD=%PREFIX%\Lib\R\bin\x64\R.exe"
if not exist "%RCMD%" set "RCMD=%PREFIX%\Scripts\R.exe"
for /f "usebackq delims=" %%f in ("%SRC_DIR%\install_order.txt") do (
  echo Installing %%f
  "%RCMD%" CMD INSTALL --no-test-load --library="%PREFIX%\Lib\R\library" "%SRC_DIR%\%%f"
  if errorlevel 1 exit /b 1
)
if not exist "%PREFIX%\Menu" mkdir "%PREFIX%\Menu"
copy /Y "%RECIPE_DIR%\menu\cyclodesk*" "%PREFIX%\Menu\" >nul
if errorlevel 1 exit /b 1
del "%PREFIX%\Menu\cyclodesk.sh"
copy /Y "%RECIPE_DIR%\shell_bin\CycloDesk.exe" "%PREFIX%\CycloDesk.exe" >nul
if errorlevel 1 exit /b 1
