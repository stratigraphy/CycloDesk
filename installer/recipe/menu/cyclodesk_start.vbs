' cyclodesk_start.vbs: starts CycloDesk without a console window (Windows desktop and Start-menu icon)
' The installed R lives one folder up (<install folder>\Lib\R); its DLL folders are put on the PATH first.
' Settings for another R on this computer (R_HOME, R_LIBS, R_LIBS_USER, .Renviron, .Rprofile) are kept away from
' it: packages built for another R version can crash this one. Messages before CycloDesk's own log starts go to
' %TEMP%\cyclodesk_console.txt.
Set fso = CreateObject("Scripting.FileSystemObject")
Set sh = CreateObject("WScript.Shell")
menuDir = fso.GetParentFolderName(WScript.ScriptFullName)
prefix = fso.GetParentFolderName(menuDir)
Set env = sh.Environment("Process")
env("PATH") = prefix & ";" & prefix & "\Library\mingw-w64\bin;" & prefix & "\Library\usr\bin;" & _
  prefix & "\Library\bin;" & prefix & "\Scripts;" & prefix & "\bin;" & env("PATH")
env("CONDA_PREFIX") = prefix
For Each v In Array("R_HOME", "R_LIBS", "R_LIBS_SITE", "R_PROFILE", "R_ENVIRON", "R_ARCH", "R_SHARE_DIR", _
    "R_INCLUDE_DIR", "R_DOC_DIR")
  On Error Resume Next
  env.Remove(v)
  On Error GoTo 0
Next
' a folder that does not exist: no user library of another R version
env("R_LIBS_USER") = prefix & "\Lib\R\no-user-library"
env("R_ENVIRON_USER") = menuDir & "\none.Renviron"
env("R_PROFILE_USER") = menuDir & "\none.Rprofile"
rscript = prefix & "\Lib\R\bin\x64\Rscript.exe"
If Not fso.FileExists(rscript) Then
  rscript = prefix & "\Scripts\Rscript.exe"
End If
logFile = env("TEMP") & "\cyclodesk_console.txt"
sh.Run "cmd /c """"" & rscript & """ """ & menuDir & "\cyclodesk_start.R"" > """ & logFile & """ 2>&1""", 0, False
