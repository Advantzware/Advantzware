/* _admTransPanels.p */

{_base.i}

DEFINE VARIABLE panelFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx       AS INTEGER     NO-UNDO.
DEFINE VARIABLE btnAdd    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnCancel AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnCopy   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnDelete AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnReset  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnSave   AS LOGICAL     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sPanelFile.

INPUT STREAM sPanelFile FROM {&datLocation}\admTransPanels{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sPanelFile panelFile.
    EMPTY TEMP-TABLE ttFileCode.
    ASSIGN
        btnAdd = NO
        btnCancel = NO
        btnCopy = NO
        btnDelete = NO
        btnReset = NO
        btnSave = NO
        .
    INPUT FROM VALUE("{&v16}" + panelFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        cFile = LEFT-TRIM(cFile).
        CASE cFile:
            WHEN "DEFINE BUTTON Btn-Add" THEN
            btnAdd = YES.
            WHEN "DEFINE BUTTON Btn-Cancel" THEN
            btnCancel = YES.
            WHEN "DEFINE BUTTON Btn-Copy" THEN
            btnCopy = YES.
            WHEN "DEFINE BUTTON Btn-Delete" THEN
            btnDelete = YES.
            WHEN "DEFINE BUTTON Btn-Reset" THEN
            btnReset = YES.
            WHEN "DEFINE BUTTON Btn-Save" THEN
            btnSave = YES.
        END CASE.
        IF cFile EQ "RUN notify ('add-record':U)." THEN DO:
            cFile = fAddComment("  ~{methods/setButton.i Btn-Save ~"Save~"}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "RUN notify ('cancel-record':U)." THEN DO:
            cFile = fAddComment("      ~{methods/setButton.i Btn-Save ~"Update~"}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "RUN notify ('copy-record':U)." THEN DO:
            cFile = fAddComment("   ~{methods/setButton.i Btn-Save ~"Save~"}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "RUN notify ('update-record':U)." THEN DO:
            cFile = fAddComment("           ~{methods/setButton.i Btn-Save ~"Update~"}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "RUN new-state('update-begin':U)." THEN DO:
            cFile = fAddComment("           ~{methods/setButton.i Btn-Save ~"Save~"}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "RUN dispatch IN THIS-PROCEDURE ('initialize':U)." THEN DO:
            IMPORT UNFORMATTED cFile.
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IF btnSave THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Save ~"Update~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            IF btnReset THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Reset ~"Reset~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            IF btnAdd THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Add ~"Add~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            IF btnCopy THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Copy ~"Copy~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            IF btnDelete THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Delete ~"Delete~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            IF btnCancel THEN DO:
                cFile = fAddComment("  ~{methods/setButton.i Btn-Cancel ~"Cancel~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "IF (RETURN-VALUE = 'Multiple-Records':U) AND add-active THEN" THEN DO:
            REPEAT:
                IMPORT UNFORMATTED cFile.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                cFile = LEFT-TRIM(cFile).
                IF cFile EQ "ELSE" THEN LEAVE.
            END.
        END.
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
