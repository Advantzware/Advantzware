/*---------------------------------------------------------------------------*/
/*  File:           getDbConnections.p                                                   */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    creates multiple db conn .pf files from input            */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:  prodict/load.df                                      */
/*  External files:     READ nosweat.pf or SESSION:PARAMETER                 */
/*                      WRITE misc .pf files                                 */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/

DEF VAR cPfName AS CHAR NO-UNDO.
DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cTest AS CHAR NO-UNDO.
DEF VAR i AS INT.

DEF TEMP-TABLE ttDbConn 
    FIELD cDbName AS CHAR
    FIELD cHost AS CHAR
    FIELD cService AS CHAR
    FIELD lSingle AS LOG
    FIELD cLdName AS CHAR.

cPfName = SEARCH("nosweat.pf").

IF SESSION:PARAMETER > "" THEN ASSIGN
    cPfName = SESSION:PARAMETER.

INPUT FROM VALUE(cPfName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "#" THEN NEXT.
    ASSIGN 
        cLine = TRIM(cLine)
        cLine = REPLACE(cLine,CHR(011)," ")
        cLine = REPLACE(cLine,"|"," ")
        cLine = REPLACE(cLine,"  "," ")
        cLine = REPLACE(cLine," ",",").
    CREATE ttDbConn.
    ASSIGN
        ttDbConn.cLdName = ""
        ttDbConn.lSingle = FALSE.
    DO i = 1 TO NUM-ENTRIES(cLine):
        ASSIGN
            cTest = ENTRY(i,cLine).
        CASE cTest:
            WHEN "-db" THEN ASSIGN
                ttDbConn.cDbName = ENTRY((i + 1),cLine)
                ttDbConn.cDbName = REPLACE(ttDbConn.cDbName,"/","\").
            WHEN "-H" THEN ASSIGN
                ttDbConn.cHost = ENTRY((i + 1),cLine).
            WHEN "-S" THEN ASSIGN
                ttDbConn.cService = ENTRY((i + 1),cLine).
            WHEN "-1" THEN ASSIGN
                ttDbConn.lSingle = TRUE.
            WHEN "-ld" THEN ASSIGN
                ttDbConn.cLdName = ENTRY((i + 1),cLine).
        END CASE.
    END.
END.

FOR EACH ttDbConn:
    IF INDEX(ttDbConn.cDbName,"\") <> 0 THEN ASSIGN
        ttDbConn.cDbName = SUBSTRING(ttDbConn.cDbName,(R-INDEX(ttDbConn.cDbName,"\") + 1)).
    OUTPUT TO VALUE(ttDbConn.cDbName + ".pf").
    IF ttDbConn.lSingle THEN PUT UNFORMATTED
        "-DB " + ttDbConn.cDbName + CHR(10) + 
        "-1" + CHR(10) + 
        IF ttDbConn.cLdName <> "" THEN "-ld " + ttDbConn.cLdName + CHR(10) ELSE "".
    ELSE PUT UNFORMATTED 
        "-DB " + ttDbConn.cDbName + CHR(10) +
        "-H " + ttDbConn.cHost + CHR(10) +
        "-S " + ttDbConn.cService + CHR(10) +
        IF ttDbConn.cLdName <> "" THEN "-ld " + ttDbConn.cLdName + CHR(10) ELSE "".
        .
    OUTPUT CLOSE.
END.

