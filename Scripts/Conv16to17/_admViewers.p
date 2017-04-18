/* _admViewers.p */

{_base.i}

DEFINE VARIABLE viewerFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sViewerFile.

INPUT STREAM sViewerFile FROM {&datLocation}\admViewers{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sViewerFile viewerFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + viewerFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        IF cFile EQ "~&Scoped-define WINDOW-NAME CURRENT-WINDOW" THEN DO:
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = fAddComment("~{Advantzware\WinKit\admViewersUsing.i}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + viewerFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sViewerFile CLOSE.

{_pCodeFile.i}
