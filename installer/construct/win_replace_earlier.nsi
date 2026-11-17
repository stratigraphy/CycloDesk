    # CycloDesk: a CycloDesk that is already installed in the chosen folder is replaced (its own uninstaller runs
    # first), so that a new version installs in one go. Without this, constructor refuses a folder that is not empty
    # and the user has to uninstall and run the installer again. build_installer.R inserts these lines at the start of
    # OnDirectoryLeave in constructor's template.
    ${If} ${FileExists} "$INSTDIR\Uninstall-${NAME}.exe"
    ${AndIf} ${FileExists} "$INSTDIR\conda-meta\history"
        MessageBox MB_YESNO|MB_ICONQUESTION \
            "${NAME} is already installed in '$INSTDIR'.$\n$\n\
            Replace it with ${NAME} ${VERSION}?$\n$\n\
            Your data files are not stored in this folder and stay where they are." \
            /SD IDYES IDYES cd_replace_earlier
        Abort
        cd_replace_earlier:
        # a running CycloDesk (its window program or R) keeps its files locked
        StrCpy $1 0
        ${If} ${FileExists} "$INSTDIR\CycloDesk.exe"
            ClearErrors
            FileOpen $0 "$INSTDIR\CycloDesk.exe" a
            ${If} ${Errors}
                StrCpy $1 1
            ${Else}
                FileClose $0
            ${EndIf}
        ${EndIf}
        ${If} ${FileExists} "$INSTDIR\Lib\R\bin\x64\R.dll"
            ClearErrors
            FileOpen $0 "$INSTDIR\Lib\R\bin\x64\R.dll" a
            ${If} ${Errors}
                StrCpy $1 1
            ${Else}
                FileClose $0
            ${EndIf}
        ${EndIf}
        ${If} $1 = 1
            MessageBox MB_OK|MB_ICONEXCLAMATION \
                "${NAME} is still running.$\n$\nClose it (the Close button at the top right of CycloDesk) and click Next again." \
                /SD IDOK
            Abort
        ${EndIf}
        ${Print} "Removing the earlier ${NAME} in $INSTDIR"
        ExecWait '"$INSTDIR\Uninstall-${NAME}.exe" /S _?=$INSTDIR' $0
        RMDir /r "$INSTDIR"
        ${If} ${FileExists} "$INSTDIR\*.*"
            MessageBox MB_OK|MB_ICONEXCLAMATION \
                "The earlier ${NAME} in '$INSTDIR' could not be removed completely.$\n$\n\
                Close ${NAME} and any R window, then click Next again." \
                /SD IDOK
            Abort
        ${EndIf}
    ${EndIf}

