/* _dontValidatePanels.p */

{_base.i}

DEFINE VARIABLE panelFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx       AS INTEGER     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sPanelFile.

INPUT STREAM sPanelFile FROM {&datLocation}\dontValidatePanels{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sPanelFile panelFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + panelFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile BEGINS "/* ***************************  Definitions" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = fAddComment("~&SCOPED-DEFINE DontValidateError").
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + panelFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sPanelFile CLOSE.

{_pCodeFile.i}
