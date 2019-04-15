/*---------------------------------------------------------------------------*/
/*  File:           admin\envadmin\convusr.p                                 */
/*  Copyright:      (c)2018 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Convert old-style advantzware.usr file to new style      */
/*                                                                           */
/*  Included files:     none                                                 */
/*                                                                           */
/*  External RUN/CALL:  none                                                 */
/*                                                                           */
/*  External files:     READ admin/advantzware.usr                           */
/*                      COPY admin/advantzware.usr.old                       */
/*                      WRITE admin/advantzware.usr                          */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      06/14/18    MYT     xxxxx   Original Version         */
/*---------------------------------------------------------------------------*/

DEF TEMP-TABLE ttUsers
    FIELD ttfuserid AS CHAR
    FIELD ttfdbname AS CHAR
    FIELD ttfalias AS CHAR
    FIELD ttfenvlist AS CHAR
    FIELD ttfdblist AS CHAR
    FIELD ttfmodelist AS CHAR.
 
DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cOutline AS CHAR NO-UNDO.
DEF VAR cuserlist AS CHAR NO-UNDO.
DEF VAR cenvlist AS CHAR NO-UNDO.
DEF VAR cdblist AS CHAR NO-UNDO.
DEF VAR calias AS CHAR NO-UNDO.
DEF VAR cenv AS CHAR NO-UNDO.
DEF VAR cdb AS CHAR NO-UNDO.
DEF VAR cmode AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 
DEF VAR k AS INT NO-UNDO. 
DEF VAR ctr AS INT NO-UNDO.
DEF VAR lContinue AS LOG INITIAL TRUE NO-UNDO.

INPUT FROM N:\admin\advantzware.usr.
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF ENTRY(2,cLine,"|") EQ "*" THEN DO:
        ASSIGN
            lContinue = FALSE.
        RETURN.
    END.
    
    CREATE ttUsers.
    ASSIGN
        ttfUserid = ENTRY(3,cLine,"|")
        ttfdbname = ENTRY(1,cLine,"|")
        ttfalias = ENTRY(2,cLine,"|")
        ttfenvlist = ENTRY(4,cLine,"|")
        ttfdblist = ENTRY(5,cLine,"|")
        ttfmodelist = ENTRY(6,cLine,"|").
END.
INPUT CLOSE.

IF lContinue EQ FALSE THEN RETURN.

OS-COPY N:\admin\advantzware.usr N:\admin\advantzware.usr.old.

FOR EACH ttUsers:
    IF INDEX(cuserlist,ttfuserid) EQ 0 THEN ASSIGN
        cuserlist = cuserlist + ttfuserid + ",".
    DO i = 1 TO NUM-ENTRIES(ttfenvlist):
        IF INDEX(cenvlist,ENTRY(i,ttfenvlist)) EQ 0 THEN ASSIGN
            cenvlist = cenvlist + ENTRY(i,ttfenvlist) + ",".
    END.
    IF INDEX(cdblist,ttfDbName) EQ 0 THEN ASSIGN
        cdblist = cdblist + ttfDbName + ",".
END.

ASSIGN
    cuserlist = TRIM(cuserlist,",")
    cdblist = TRIM(cdblist,",")
    cenvlist = TRIM(cenvlist,",").

OUTPUT TO N:\admin\advantzware.usr.
DO i = 1 TO NUM-ENTRIES(cuserlist):
    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfalias NE ""
        NO-ERROR.
    ASSIGN
        calias = IF AVAIL ttusers THEN ttfalias ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfenvlist NE ""
        NO-ERROR.
    ASSIGN
        cenv = IF AVAIL ttusers THEN ttfenvlist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfdblist NE ""
        NO-ERROR.
    ASSIGN
        cdb = IF AVAIL ttusers THEN ttfdblist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfmodelist NE ""
        NO-ERROR.
    ASSIGN
        cmode = IF AVAIL ttusers THEN ttfmodelist ELSE "".

    ASSIGN
        cOutline = ENTRY(i,cuserlist) + "|*|" +
                   calias + "|" +
                   cenv + "|" +
                   cdb + "|".
    PUT UNFORMATTED coutline + CHR(10).
END.
    
DO j = 1 TO NUM-ENTRIES(cdblist):
    ASSIGN
        cdb = ENTRY(j,cdblist).
    DO i = 1 TO NUM-ENTRIES(cuserlist):
        FIND FIRST ttusers WHERE
            ttfuserid = ENTRY(i,cuserlist) AND
            ttfmodelist NE ""
            NO-ERROR.
        ASSIGN
            cmode = IF AVAIL ttusers THEN ttfmodelist ELSE ""
            cOutline = ENTRY(i,cuserlist) + "|" +
                       cdb + "|" +
                       "" + "|" +
                       "" + "|" +
                       "" + "|" +
                       cmode.
        PUT unformatted coutline + CHR(10).
    END.
END.
OUTPUT CLOSE.  
 
 
