/* _caps.p */

{_base.i}

DEFINE VARIABLE capsFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER     NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sCapsFile.

INPUT STREAM sCapsFile FROM {&datLocation}\caps{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sCapsFile capsFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + capsFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile BEGINS "ON VALUE-CHANGED OF" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile BEGINS "/* _UIB-CODE-BLOCK-END */" THEN
                LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                IF INDEX(cFile,"CAPS(") NE 0 THEN DO:
                    cFile = fAddComment("  ~{&SELF-NAME}:CURSOR-OFFSET = LENGTH(~{&SELF-NAME}:SCREEN-VALUE) + 1.").
                    RUN pCodeFile (INPUT-OUTPUT idx, cFile).
                END.
            END.
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + capsFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sCapsFile CLOSE.

{_pCodeFile.i}
