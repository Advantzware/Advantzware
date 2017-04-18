/* _lookups.p */

{_base.i}

DEFINE VARIABLE lookupFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE idx    AS INTEGER       NO-UNDO.
DEFINE VARIABLE jdx    AS DECIMAL       NO-UNDO.

{_ttFileCode.i}

DEFINE STREAM sLookup.

INPUT STREAM sLookup FROM {&datLocation}\lookups{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sLookup lookupFile.
    EMPTY TEMP-TABLE ttFileCode.
    INPUT FROM VALUE("{&v16}" + lookupFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        jdx = 0.
        IF cFile EQ "~"NOSWEAT ~" ~~" THEN
        cFile = REPLACE(cFile,"NOSWEAT","ASI").
        ELSE IF cFile EQ "~"EMPTRACK ~" ~~" THEN
        cFile = REPLACE(cFile,"EMPTRACK","ASI").
        ELSE IF cFile EQ "~&Scoped-define lookup-db NOSWEAT." THEN
        cFile = REPLACE(cFile,"NOSWEAT","ASI").
        ELSE IF cFile EQ "~&Scoped-define lookup-db EMPTRACK." THEN
        cFile = REPLACE(cFile,"EMPTRACK","ASI").
        ELSE IF INDEX(cFile,"NOSWEAT.notes") NE 0 THEN
        cFile = REPLACE(cFile,"NOSWEAT","ASI").
        ELSE IF INDEX(cFile,"jobs.jobs.") NE 0 THEN
        cFile = REPLACE(cFile,"jobs.jobs.","jobs.").
        ELSE IF INDEX(cFile,"EMPTRACK.") NE 0 THEN
        cFile = REPLACE(cFile,"EMPTRACK.","").
        ELSE IF INDEX(cFile,"RFQ.") NE 0 THEN
        cFile = REPLACE(cFile,"RFQ.","").
        ELSE IF cFile BEGINS "~&Scoped-define window-size" THEN
        ASSIGN
            jdx = DECIMAL(ENTRY(3,cFile," ")) + .7
            ENTRY(3,cFile," ") = STRING(jdx)
            .
        ELSE IF cFile BEGINS "~&Scoped-define btn-row" THEN
        ASSIGN
            jdx = DECIMAL(ENTRY(3,cFile," ")) - .07
            ENTRY(3,cFile," ") = STRING(jdx)
            .
        ELSE IF cFile BEGINS "~&Scoped-define btn-ok-col" THEN
        ASSIGN
            jdx = DECIMAL(ENTRY(3,cFile," ")) + 2
            ENTRY(3,cFile," ") = STRING(jdx)
            .
        ELSE IF cFile BEGINS "~&Scoped-define btn-cancel-col" THEN
        ASSIGN
            jdx = DECIMAL(ENTRY(3,cFile," ")) + 6
            ENTRY(3,cFile," ") = STRING(jdx)
            .
        ELSE IF cFile BEGINS "~&Scoped-define auto-find-row" THEN
        ASSIGN
            jdx = DECIMAL(ENTRY(3,cFile," ")) + .75
            ENTRY(3,cFile," ") = STRING(jdx)
            .
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + lookupFile).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sLookup CLOSE.

{_pCodeFile.i}
