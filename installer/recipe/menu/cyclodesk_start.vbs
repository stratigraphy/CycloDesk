' cyclodesk_start.vbs: starts CycloDesk without a console window (Windows desktop and Start-menu icon)
' The installed R lives two folders up (<install folder>\Lib\R); its DLL folders are put on the PATH first.
Set fso = CreateObject("Scripting.FileSystemObject")
Set sh = CreateObject("WScript.Shell")
menuDir = fso.GetParentFolderName(WScript.ScriptFullName)
prefix = fso.GetParentFolderName(menuDir)
Set env = sh.Environment("Process")
env("PATH") = prefix & ";" & prefix & "\Library\mingw-w64\bin;" & prefix & "\Library\usr\bin;" & _
  prefix & "\Library\bin;" & prefix & "\Scripts;" & prefix & "\bin;" & env("PATH")
env("CONDA_PREFIX") = prefix
rscript = prefix & "\Lib\R\bin\x64\Rscript.exe"
If Not fso.FileExists(rscript) Then
  rscript = prefix & "\Scripts\Rscript.exe"
End If
sh.Run """" & rscript & """ """ & menuDir & "\cyclodesk_start.R""", 0, False
