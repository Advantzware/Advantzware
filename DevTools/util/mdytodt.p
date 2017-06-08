/* util/mdytodt.p                                                                        */
/* Converts STRING IN FORM "mm/dd/yy" TO FORM specified BY PROGRESS session:DATE-FORMAT. */

DEF INPUT PARAMETER v-startdate AS CHAR NO-UNDO.

IF v-startdate = ?
OR v-startdate = "" 
THEN DO:
    v-startdate = STRING(TODAY).
    RETURN.
END.

DEF VAR adate AS CHAR EXTENT 3 NO-UNDO.
DEF VAR ii AS INT NO-UNDO.
DEF VAR cc AS CHAR NO-UNDO.
DEF VAR cdelm AS CHAR NO-UNDO.

cdelm = ?.
DO ii = 1 TO LENGTH(v-startdate).
    IF INDEX("0123456789",SUBSTRING(v-startdate,ii,1)) = 0
    THEN DO:
        cdelm = SUBSTRING(v-startdate,ii,1).
        LEAVE.
    END.
END.
IF cdelm = ? THEN RETURN ERROR.
ASSIGN
    adate[1] = TRIM(ENTRY(1, v-startdate, cdelm))
    adate[2] = TRIM(ENTRY(2, v-startdate, cdelm))
    adate[3] = TRIM(ENTRY(3, v-startdate, cdelm)).

DO ii = 1 TO 3:
    IF LENGTH(adate[ii]) < 2 THEN adate[ii] = "0" + adate[ii].
    IF LENGTH(adate[ii]) > 2 THEN adate[ii] = SUBSTRING(adate[ii],LENGTH(adate[ii]) - 1, 2).
END.

/*get 1st 2 chars of date*/
cc = SUBSTRING(SESSION:DATE-FORMAT,1,1).
ii = INDEX("MDY",cc).
v-startdate = adate[ii] + "/".

/*get middle 2 chars of date*/
cc = SUBSTRING(SESSION:DATE-FORMAT,2,1).
ii = INDEX("MDY",cc).
v-startdate = v-startdate + adate[ii] + "/".

/*get last 2 chars of date*/
IF adate[3] > "" 
THEN DO:
    cc = SUBSTRING(SESSION:DATE-FORMAT,3,1).
    ii = INDEX("MDY",cc).
    v-startdate = v-startdate + adate[ii].
END.
RETURN v-startdate.
