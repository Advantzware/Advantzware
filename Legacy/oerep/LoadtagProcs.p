
/* **********************  Internal Procedures  *********************** */
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

{oerep/r-loadtg.i }
{custom/xprint.i}

{sys/inc/var.i shared}
{sys/form/r-top3.f}

 DEFINE SHARED TEMP-TABLE tt-word-print LIKE w-ord 
   FIELD tag-no AS CHARACTER .

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".


PROCEDURE pPrintView:
DEFINE INPUT PARAMETER ipcCasLabel AS CHARACTER .
DEFINE INPUT PARAMETER iplPrintView AS LOGICAL .

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    IF iplPrintView THEN DO:
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
    END.
    ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.

   
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
            FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ cocode
             AND loadtag.item-type EQ NO
                AND loadtag.tag-no EQ TRIM(tt-word-print.tag-no)
                USE-INDEX tag NO-ERROR.
                                
           IF ipcCasLabel EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag2.xpr" THEN DO:
               {oe/rep/lodxprnt2.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i} 
           END.
           ELSE IF ipcCasLabel EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.
