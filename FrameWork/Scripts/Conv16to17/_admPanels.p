/* _admPanels.p */

{_base.i}

DEFINE VARIABLE panelFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx       AS INTEGER     NO-UNDO.
DEFINE VARIABLE done      AS LOGICAL     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sPanelFile.

INPUT STREAM sPanelFile FROM {&datLocation}\admPanels{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sPanelFile panelFile.
    IF panelFile BEGINS "//" THEN NEXT.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + panelFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile EQ "~&Scoped-define PROCEDURE-TYPE SmartViewer" THEN
        cFile = REPLACE(cFile,"SmartViewer","SmartPanel").
        ELSE IF cFile EQ "/* ************************* Included-Libraries *********************** */" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~{Advantzware/WinKit/winkit-panel.i}".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            IF cFile EQ "~{src/adm/method/viewer.i}" THEN
            cFile = "~{src/adm/method/panel.i}".
        END.
        ELSE IF cFile BEGINS "ON CHOOSE OF" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            done = FALSE.
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile EQ "/* _UIB-CODE-BLOCK-END */" THEN DO:
                    FIND FIRST ttFileCode
                         WHERE ttFileCode.ln EQ idx - 1
                         NO-ERROR.
                    IF AVAILABLE ttFileCode THEN
                    ttFileCode.lnText = fAddComment("  ~{Advantzware/WinKit/winkit-panel-triggerend.i}")
                                      + CHR(10) + ttFileCode.lnText.
                    done = TRUE.
                END.
                IF done THEN LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
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
