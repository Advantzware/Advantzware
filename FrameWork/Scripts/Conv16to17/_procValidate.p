/* _procValidate.p.p */

{_base.i}

DEFINE VARIABLE procFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
DEFINE VARIABLE done     AS LOGICAL   NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sProcFile.

INPUT STREAM sProcFile FROM {&datLocation}\procValidate{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sProcFile procFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + procFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile BEGINS "PROCEDURE valid" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            done = NO.
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF TRIM(cFile) BEGINS "~/*--------" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "Purpose:" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "Parameters:" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "Notes:" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "----------" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "DEF" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) EQ "" THEN
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                ELSE IF TRIM(cFile) BEGINS "END PROCEDURE" THEN DO:
                    cFile1 = "  ~{methods/lValidateError.i NO}".
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    LEAVE.
                END.
                ELSE IF NOT done THEN DO:
                    cFile1 = "  ~{methods/lValidateError.i YES}".
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                    done = YES.
                END.
                ELSE RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + procFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sProcFile CLOSE.

{_pCodeFile.i}
