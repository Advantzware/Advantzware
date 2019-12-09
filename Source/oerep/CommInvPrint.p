/*------------------------------------------------------------------------

  File              : oerep\CommInvPrint.p

  Description       : Commercial Invoice User Input

------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
DEFINE INPUT PARAM icInvoiceForm   AS CHAR NO-UNDO.
DEFINE INPUT PARAM ip-bol AS INT NO-UNDO.
DEFINE INPUT PARAM ip-total-pallets AS INT NO-UNDO.


DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iCountPallet AS INTEGER NO-UNDO .

{custom/xprint.i}

{sys/inc/var.i shared}
{sys/form/r-top3.f}
{oe/rep/invoice.i NEW}
 
RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  

RUN pPrintView .


PROCEDURE pPrintView:

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    
      IF NOT lBussFormModle THEN
          PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
      ELSE
          PUT "<PREVIEW></PROGRESS>" FORM "x(50)".

        IF icInvoiceForm EQ "InvComm20" THEN do:
            RUN oe/rep/CommInv20.p(ip-bol,cocode) .
        END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.
