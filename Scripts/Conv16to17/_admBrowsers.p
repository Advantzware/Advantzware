/* _admBrowsers.p */

{_base.i}

DEF VAR browserFile AS CHAR NO-UNDO.
DEF VAR cFile       AS CHAR NO-UNDO.
DEF VAR cFile1      AS CHAR NO-UNDO.
DEF VAR idx         AS INT  NO-UNDO.
DEF VAR done        AS LOG  NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sBrowserFile.

INPUT STREAM sBrowserFile FROM {&datLocation}\admBrowsers{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sBrowserFile browserFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + browserFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        IF cFile EQ "~&Scoped-define WINDOW-NAME CURRENT-WINDOW" THEN DO:
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = fAddComment("~{Advantzware\WinKit\admBrowserUsing.i}").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "".
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
        END.
        ELSE IF cFile EQ "/* ************************* Included-Libraries *********************** */" THEN DO:
            done = FALSE.
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile EQ "/* _UIB-CODE-BLOCK-END */" THEN DO:
                    cFile1 = "~{Advantzware/WinKit/dataGridProc.i}".
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    cFile1 = "".
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    done = TRUE.
                END.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                IF done THEN LEAVE.
            END.
        END.
        ELSE IF cFile BEGINS "PROCEDURE local-initialize" THEN DO:
            done = FALSE.
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF INDEX(cFile,"RUN dispatch") NE 0 THEN DO:
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                    cFile1 = fAddComment("  RUN pDataGridInit.").
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    done = TRUE.
                END.
                IF done THEN LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + browserFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sBrowserFile CLOSE.

{_pCodeFile.i}
