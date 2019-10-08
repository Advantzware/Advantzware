/* _nonAdm1.p */

{_base.i}

DEFINE VARIABLE nonAdm AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER     NO-UNDO.
DEFINE VARIABLE done   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE done1  AS LOGICAL     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sNonAdm.

INPUT STREAM sNonAdm FROM {&datLocation}\nonAdm1{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sNonAdm nonAdm.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + nonAdm) NO-ECHO.
    done = FALSE.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        done1 = FALSE.
        IF cFile EQ "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile EQ "" THEN DO:
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                    cFile = "~{Advantzware/WinKit/embedwindow-nonadm.i}".
                    done = TRUE.
                END.
                IF done THEN LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
        ELSE IF cFile BEGINS "ON CHOOSE OF" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile EQ "/* _UIB-CODE-BLOCK-END */" THEN DO:
                    FIND FIRST ttFileCode
                         WHERE ttFileCode.ln EQ idx - 1
                         NO-ERROR.
                    IF AVAILABLE ttFileCode THEN
                    ttFileCode.lnText = FILL(" ",LENGTH(ttFileCode.lnText))
                                      + fAddComment("~{Advantzware/WinKit/winkit-panel-triggerend.i}")
                                      + CHR(10) + ttFileCode.lnText.
                    done1 = TRUE.
                END.
                IF done1 THEN LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
        ELSE IF cFile EQ "/* ***********  Runtime Attributes and AppBuilder Settings  *********** */" AND NOT done THEN DO:
            cFile = "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win ".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "/* ************************* Included-Libraries *********************** */".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~{Advantzware/WinKit/embedwindow-nonadm.i}".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "/* _UIB-CODE-BLOCK-END */".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~&ANALYZE-RESUME".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "/* ***********  Runtime Attributes and AppBuilder Settings  *********** */".
        END.
        ELSE IF cFile EQ "ON CLOSE OF THIS-PROCEDURE" THEN DO:
            cFile = "ON CLOSE OF THIS-PROCEDURE DO:".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = fAddComment("   ~{Advantzware/WinKit/closewindow-nonadm.i}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "END.".
        END.
        ELSE IF cFile EQ "ON CLOSE OF THIS-PROCEDURE DO:" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile EQ "END." THEN DO:
                    cFile1 = fAddComment("   ~{Advantzware/WinKit/closewindow-nonadm.i}").
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    done1 = TRUE.
                END.
                IF done1 THEN LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
        ELSE IF TRIM(cFile) EQ "IF NOT THIS-PROCEDURE:PERSISTENT THEN" THEN DO:
            cFile1 = fAddComment("    ~{Advantzware/WinKit/embedfinalize-nonadm.i}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
        END.
        ELSE IF cFile EQ "~&IF '~{~&WINDOW-SYSTEM}' NE 'TTY' ~&THEN" THEN DO:
            IMPORT UNFORMATTED ^.
            IMPORT UNFORMATTED ^.
            IMPORT UNFORMATTED ^.
            IMPORT UNFORMATTED ^.
            IMPORT UNFORMATTED cFile.
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + nonAdm).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sNonAdm CLOSE.

{_pCodeFile.i}
