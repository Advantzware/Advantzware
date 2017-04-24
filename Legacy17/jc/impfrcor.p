/*impfrcor.p  DISPLAY CORRUGATOR PRODUCTION - SOURCE CORRLINK*/

DEF VAR A AS C FORMAT "X(304)" no-undo.
 
DEF temp-TABLE CLDATA
    FIELD CL-ORDER-ID AS C FORMAT "X(20)"
    FIELD CL-CUST-NAME AS C FORMAT "X(24)"
    FIELD CL-COMPLETION-DATE AS C FORMAT "X(14)"
    FIELD CL-COMPLETION-TIME AS C FORMAT "X(10)"
    FIELD CL-TOTAL-START-BLANKS AS C FORMAT "X(7)"
    field CL-TOTAL-NUM-UP as c form "x(4)"
    .
    
DEF VAR TIME1 AS I.
DEF VAR TIME2 AS C FORMAT "XX".
DEF VAR TIME3 AS C FORMAT "XX".
DEF VAR TIMEDISP AS C.
DEF VAR DATEDISP AS C FORMAT "X(8)".

STATUS INPUT OFF.

/*DOS SILENT
Q:\DLC91B\BIN\QUOTER.EXE 
\\alan_chu\cti\dataxfer\CLHIST.DAT /*schedule.100*/
> c:\cltrans.
*/

DOS SILENT QUOTER.EXE 
\\alan_chu\cti\dataxfer\CLHIST.DAT /*schedule.100*/
> c:\cltrans.


INPUT FROM C:\CLTRANS.

REPEAT:
    IMPORT unformatted A.
    IF SUBSTRING(A, 2, 1) = "A" THEN NEXT.
    
    /* message "input: " substring(a,1,10) "|" substring(a,218,8) */
/*        substring(a,1,1)  "|"  substring(a,1,1) = "A" */
/*        skip */
/*        substring(a,226,6)  skip */
/*        substring(a,234,7) view-as alert-box. */

    CREATE CLDATA.
    
    CL-ORDER-ID = SUBSTRING(A, 2, 20).
    CL-CUST-NAME = SUBSTRING(A, 22, 20).
    CL-COMPLETION-DATE = SUBSTRING(A, 218, 8).
    CL-COMPLETION-TIME = SUBSTRING(A, 226, 6).
    CL-TOTAL-Num-Up = substring(a,234,4).
    CL-TOTAL-START-BLANKS = SUBSTRING(A, 238, 7).

    TIME3 = "AM".
    TIME1 = INTEGER(SUBSTRING(CL-COMPLETION-TIME,1,2)).
    TIME2 = SUBSTRING(CL-COMPLETION-TIME,3,2).
    IF TIME1 GT 12 THEN DO:
        TIME1 = TIME1 - 12.
        TIME3 = "PM".
    END.
    
    TIMEDISP = " ".
    IF TIME1 NE 0 AND TIME2 NE "" THEN
        TIMEDISP = STRING(TIME1,">9" ) + ":" + STRING(TIME2,">9") + TIME3.
    
    /*CL-COMPLETION-TIME = TIMEDISP.*/
    
    DATEDISP = " ".
    IF CL-COMPLETION-DATE NE "00000000" THEN
        DATEDISP = SUBSTRING(CL-COMPLETION-DATE,5,2) + "/"
                   + SUBSTRING(CL-COMPLETION-DATE,7,2) + "/"
                   + SUBSTRING(CL-COMPLETION-DATE,1,4).
    CL-COMPLETION-DATE = DATEDISP.    
END.

/*

MESSAGE 
"                           PRESS ESC WHEN DONE                              ".
DEFINE QUERY Q1 FOR CLDATA.

DEFINE BROWSE B1 QUERY Q1
DISP
    CL-ORDER-ID FORMAT "XXXXXXXXXXXX" COLUMN-LABEL "ORDER!NUMBER"
    CL-CUST-NAME COLUMN-LABEL "CUSTOMER!NAME"
    CL-COMPLETION-DATE COLUMN-LABEL "DATE"
    CL-COMPLETION-TIME COLUMN-LABEL "TIME"
    CL-TOTAL-START-BLANKS COLUMN-LABEL "SHEETS"
                WITH 20 DOWN ROW 5 NO-BOX.

DEFINE FRAME F1 B1.
OPEN QUERY Q1 FOR EACH CLDATA.
ENABLE ALL WITH FRAME F1.


/***********************************GC 120202****************************
APPLY "END" TO BROWSE B1.
***********************************/


WAIT-FOR DEFAULT-ACTION OF B1.
HIDE ALL.

*/

for each cldata:

   create corr2asi.
   assign corr2asi.company = "001"
          corr2asi.cl-order-id = cldata.cl-order-id
          corr2asi.cl-cust-name = cldata.cl-cust-name
          corr2asi.cl-completion-date = date(cldata.cl-completion-date)
          corr2asi.cl-completion-time = int(SUBSTRING(cldata.cl-completion-time,1,2)) * 3600 +
                                        int(SUBSTRING(cldata.cl-completion-time,3,2)) * 60 +
                                        int(SUBSTRING(cldata.cl-completion-time,5,2)) 
          corr2asi.cl-total-start-blanks = (cldata.cl-total-start-blanks)
          corr2asi.import-date = today
          corr2asi.import-time = time
          corr2asi.import-user = userid('nosweat')
          .
end.

