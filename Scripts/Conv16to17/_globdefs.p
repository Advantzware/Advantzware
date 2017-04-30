/* _globdefs.p */

{_base.i}

DEFINE VARIABLE winFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sWinFile.

INPUT STREAM sWinFile FROM {&datLocation}\globdefs{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sWinFile winFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + winFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile BEGINS "/* Local Variable Definitions ---" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = fAddComment("~{custom/globdefs.i}").
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + winFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sWinFile CLOSE.

{_pCodeFile.i}
