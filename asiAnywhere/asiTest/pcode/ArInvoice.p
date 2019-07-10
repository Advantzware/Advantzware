/*------------------------------------------------------------------------
    File        : ArInvoice.p
    Purpose     : Invoice Inquiry

    Syntax      :

    Description : Return a Dataset of all AR Invoice Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Feb 06 2008
    Notes       : Dynamic query
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ArInvoice.i}
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmInvoice   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmPart      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustPo    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBOL       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOpen       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPaid       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsArInvoice.

DEFINE VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE qh AS WIDGET-HANDLE. 
DEF VAR v-q-string AS CHAR NO-UNDO.

IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmInvoice = ? THEN ASSIGN prmInvoice = "".
IF prmCust = ? THEN ASSIGN prmCust = "".
IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmPart = ? THEN ASSIGN prmPart = "".
IF prmCustPo = ? THEN ASSIGN prmCustPo = "".
IF prmBOL = ? THEN ASSIGN prmBOL = "".
IF prmEstimate = ? THEN ASSIGN prmEstimate = "".
IF prmDate = ? THEN ASSIGN prmDate = "".
IF prmOpen = ? THEN ASSIGN prmOpen = "".
IF prmPaid = ? THEN ASSIGN prmPaid = "".  
IF prmUser = ? THEN ASSIGN prmUser = "".  
/* ********************  Preprocessor Definitions  ******************** */

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

CREATE QUERY qh.
qh:SET-BUFFERS(BUFFER usercust:HANDLE,BUFFER ar-invl:HANDLE,BUFFER ar-inv:HANDLE,BUFFER cust:HANDLE).

IF prmAction = "select" THEN DO:
    
   v-q-string = v-q-string + " FOR EACH usercust WHERE usercust.user_id = "
                           + QUOTER(prmUser)
                           + " AND usercust.company = "
                           + QUOTER(prmComp) + " NO-LOCK, " .
    
   v-q-string = v-q-string + " EACH ar-invl WHERE ar-invl.company = " + QUOTER(prmComp).

    v-q-string = v-q-string + " AND ar-invl.cust-no = usercust.cust-no ".

   IF prmCust NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.cust-no = " + QUOTER(prmCust).


   v-q-string = v-q-string
              + " AND ar-invl.posted EQ YES NO-LOCK, FIRST ar-inv WHERE "
              + "ar-inv.x-no = ar-invl.x-no AND "
              + "ar-inv.due GT 0 NO-LOCK, FIRST cust WHERE cust.company EQ "
              + QUOTER(prmComp) + " AND cust.cust-no EQ ar-inv.cust-no NO-LOCK".
          
   qh:QUERY-PREPARE(v-q-string).
   qh:QUERY-OPEN.

   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.

      create ttArInvoice.
      assign 
          ttArInvoice.Arinv-no         = ar-invl.inv-no
          ttArInvoice.Arbol-no         = ar-invl.bol-no
          ttArInvoice.Arcust-no        = ar-invl.cust-no
          ttArInvoice.Arinv-date       = ar-inv.inv-date
          ttArInvoice.Aract-num        = ar-invl.actnum
          ttArInvoice.Ari-no           = ar-invl.i-no
          ttArInvoice.Arpart-no        = ar-invl.part-no
          ttArInvoice.Arord-no         = ar-invl.ord-no
          ttArInvoice.Arpo-no          = ar-invl.po-no
          ttArInvoice.Arest-no         = ar-invl.est-no
          ttArInvoice.Arname           = ar-invl.i-name
          ttArInvoice.Arcosts          = ar-invl.cost.

      FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = ar-invl.cust-no NO-LOCK NO-ERROR.
      IF AVAIL cust THEN DO:
          ASSIGN
               ttArInvoice.rec_key_cust = cust.rec_key .
      END.
   
  END.
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
ELSE
IF prmAction = "Search" THEN DO:
   ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) .

    v-q-string = v-q-string + " FOR EACH usercust WHERE usercust.user_id = "
                           + QUOTER(prmUser)
                           + " AND usercust.company = "
                           + QUOTER(prmComp) + " NO-LOCK, " .

  v-q-string = v-q-string + " EACH ar-invl NO-LOCK WHERE ar-invl.company = " + QUOTER(prmComp).

  v-q-string = v-q-string + " AND ar-invl.cust-no = usercust.cust-no ".

   IF prmInvoice NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.inv-no = int(" + QUOTER(prmInvoice) + ")".

   IF prmCust NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.cust-no BEGINS " + QUOTER(prmCust).

   IF prmItem NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.i-no BEGINS " + QUOTER(prmItem).

   IF prmPart NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.part-no BEGINS " + QUOTER(prmPart).

   IF prmCustPO NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.po-no BEGINS " + QUOTER(prmCustPO).

   IF prmBOL NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.bol-no = int(" + QUOTER(prmBOL) + ")".

   IF vEst NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.est-no BEGINS " + QUOTER(vEst).

   IF prmOrderNum NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-invl.ord-no = int(" + STRING(prmOrderNum) + ")".

   v-q-string = v-q-string
              + " AND ar-invl.posted EQ YES, first ar-inv where ar-inv.x-no = ar-invl.x-no ".

   IF prmDate NE "" THEN
      v-q-string = v-q-string
                 + " AND ar-inv.inv-date GE date(" + QUOTER(prmDate) + ")".

   /*if both yes, don't look at due*/

   IF prmOpen = "yes" AND prmPaid = "no" THEN
      v-q-string = v-q-string
                 + " AND ar-inv.due GT 0".
   ELSE
   IF prmOpen = "no" AND prmPaid = "yes" THEN
      v-q-string = v-q-string
                 + " AND ar-inv.due LE 0".

   ELSE /*should return no records*/
   IF prmOpen = "no" AND prmPaid = "no" THEN
      v-q-string = v-q-string
                 + " AND ar-inv.due GT 0 and ar-inv.due LE 0".

   v-q-string = v-q-string
              + " NO-LOCK, FIRST cust WHERE cust.company eq "
              + QUOTER(prmComp)
              + " AND cust.cust-no EQ ar-inv.cust-no NO-LOCK".

   qh:QUERY-PREPARE(v-q-string).
   qh:QUERY-OPEN.

   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.

      create ttArInvoice.
      assign 
          ttArInvoice.Arinv-no         = ar-invl.inv-no
          ttArInvoice.Arbol-no         = ar-invl.bol-no
          ttArInvoice.Arcust-no        = ar-invl.cust-no
          ttArInvoice.Arinv-date       = ar-inv.inv-date
          ttArInvoice.Aract-num        = ar-invl.actnum
          ttArInvoice.Ari-no           = ar-invl.i-no
          ttArInvoice.Arpart-no        = ar-invl.part-no
          ttArInvoice.Arord-no         = ar-invl.ord-no
          ttArInvoice.Arpo-no          = ar-invl.po-no
          ttArInvoice.Arest-no         = ar-invl.est-no
          ttArInvoice.Arname           = ar-invl.i-name
          ttArInvoice.Arcosts          = ar-invl.cost.
   END.
END.   /*if prmAction = "Search"*/

qh:QUERY-CLOSE().
DELETE OBJECT qh.
/*************************************************/
