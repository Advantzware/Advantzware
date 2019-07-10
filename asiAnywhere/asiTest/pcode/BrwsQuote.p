



/*------------------------------------------------------------------------
    File        : BrwsQuote.p
    Purpose     : Quote Maintenance

    Syntax      :

    Description : Return a Dataset of Quote Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BrwsQuote.i}
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     as INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate      AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmContact   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRfq       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPart      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBrowseQte.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE prmComp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE q-noValue1  AS INT NO-UNDO.
DEFINE VARIABLE q-noValue2  AS INT NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

IF prmUser = ? THEN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmAction = "" THEN ASSIGN prmAction = "Select".
IF prmQuote = ? THEN ASSIGN prmQuote = 0.

IF prmCustomer = ? THEN ASSIGN prmCustomer = "".
IF prmContact = ? THEN ASSIGN prmContact = "".
IF prmEstimate = ? THEN ASSIGN prmEstimate = "".
IF prmRfq = ? THEN ASSIGN prmRfq = "".
IF prmPart = ? THEN ASSIGN prmPart = "".
/* ********************  Preprocessor Definitions  ******************** */

FOR EACH ttBrowseQte NO-LOCK:
    DELETE ttBrowseQte.
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FUNCTION display-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :

  IF AVAIL quoteitm THEN
     FIND FIRST quoteqty WHERE ASI.quoteqty.company = ASI.quoteitm.company
          AND ASI.quoteqty.loc = ASI.quoteitm.loc
          AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
          AND ASI.quoteqty.line = ASI.quoteitm.line  USE-INDEX qt-qty NO-LOCK NO-ERROR.
  IF AVAIL quoteqty THEN RETURN int(quoteqty.qty).
  ELSE IF AVAIL quoteitm THEN RETURN int(quoteitm.qty).
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.


IF prmAction = "Select" THEN DO:
       v-count = 0.
       MAIN-LOOP:
       FOR EACH quotehd WHERE quotehd.company = prmComp AND LOOKUP(quotehd.cust-no, custcount ) NE 0 
            ,EACH quoteitm OF quotehd NO-LOCK BY quotehd.q-no DESC  :
            create ttBrowseQte.
            assign
                ttBrowseQte.vQuote        = quotehd.q-no
                ttBrowseQte.vDate         = quotehd.quo-date
                ttBrowseQte.vCust         = quotehd.cust-no
                ttBrowseQte.vContact      = quotehd.contact
                ttBrowseQte.vEstimate     = quotehd.est-no
                ttBrowseQte.vRfq          = quotehd.rfq 
                ttBrowseQte.VPart         = quoteitm.part-no
                ttBrowseQte.vLine         = quoteitm.LINE
                ttBrowseQte.vQty         = display-qty()
               .
              v-count = v-count + 1.
                 IF v-count = 100 THEN LEAVE MAIN-LOOP.
    END. /* For Each quotehd*/
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

    
IF prmAction = "Search" THEN DO:
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) .
    
    v-count = 0.
    MAIN-LOOP:
    FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote or prmQuote = 0 )
                           AND (quotehd.cust-no BEGINS prmCustomer or prmCustomer = "" )
                          AND (quotehd.quo-date = prmDate or prmDate = ? OR prmDate = 01/01/001 )
                           AND (quotehd.contact BEGINS prmContact OR prmContact = "" )
                           AND (quotehd.est-no  = vEst or vEst = "" )
                               AND (quotehd.rfq BEGINS prmRfq or prmRfq = "" ) NO-LOCK,
              EACH quoteitm OF quotehd WHERE (quoteitm.part-no BEGINS prmPart OR  prmPart = "") NO-LOCK  BY quotehd.q-no DESC :
                          
            create ttBrowseQte.
            assign
                ttBrowseQte.vQuote        = quotehd.q-no
                ttBrowseQte.vDate         = quotehd.quo-date
                ttBrowseQte.vCust         = quotehd.cust-no
                ttBrowseQte.vContact      = quotehd.contact
                ttBrowseQte.vEstimate     = quotehd.est-no
                ttBrowseQte.vRfq          = quotehd.rfq
                ttBrowseQte.VPart         = quoteitm.part-no
                ttBrowseQte.vLine         = quoteitm.LINE
                ttBrowseQte.vQty         = display-qty()
               .
               v-count = v-count + 1.
                      IF v-count = 200 THEN LEAVE MAIN-LOOP.
             
    END. /* For Each oe-ordl*/
END.   /*if prmAction = "Search"*/

/*************************************************/
                   

