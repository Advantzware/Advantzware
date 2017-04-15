/* _admWindows.p */

{_base.i}

DEFINE VARIABLE winFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sWinFile.

INPUT STREAM sWinFile FROM {&datLocation}\admWindows{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sWinFile winFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + winFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile EQ "/* ************************* Included-Libraries *********************** */" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile = "~{Advantzware/WinKit/embedwindow.i}".
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
