PROCEDURE pCodeFile:
    DEFINE INPUT-OUTPUT PARAMETER ipidx   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcText AS CHARACTER NO-UNDO.

    CREATE ttFileCode.
    ASSIGN
        ipidx = ipidx + 1
        ttFileCode.ln = ipidx
        ttFileCode.lnText = ipcText
        .
END PROCEDURE.
