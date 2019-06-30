
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Hist.i}



DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsHist.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = int(prmOrderNum)  NO-LOCK,
    FIRST oe-ord OF oe-ordl no-lock :
    
    create ttHist.
    assign  
    ttHist.Item = oe-ordl.i-no
    ttHist.Date = oe-ord.ord-date
    ttHist.Name= oe-ordl.i-name
    ttHist.Dscr = oe-ordl.i-dscr
    ttHist.Cost = oe-ordl.cost
/*               ttHist.Sell = */
    ttHist.Uom = oe-ordl.pr-uom
    ttHist.Qty = oe-ordl.qty
    .

END.   /*FOR EACH oe-ordl*/
