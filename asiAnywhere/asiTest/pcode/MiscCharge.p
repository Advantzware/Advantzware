
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : sewa
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MiscCharge.i}


DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER vLine as CHAR no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMiscCharge.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */

FOR EACH oe-ord WHERE
    oe-ord.company EQ prmComp AND
    oe-ord.ord-no = INT(prmOrderNum)  no-lock: 
    
    for each oe-ordm  WHERE
        oe-ordm.company EQ prmComp AND
        oe-ordm.ord-no = oe-ord.ord-no  no-lock :
        create ttMiscCharge.
        assign  
            ttMiscCharge.Charge   = oe-ordm.charge
            ttMiscCharge.Sell     = oe-ordm.amt
            ttMiscCharge.Account  = oe-ordm.actnum
            ttMiscCharge.Dscr     = oe-ordm.dscr
            ttMiscCharge.Po       = oe-ordm.po-no
            ttMiscCharge.Job      = oe-ordm.ord-i-no
            ttMiscCharge.Job2     = oe-ordm.ord-line
            ttMiscCharge.VenPo    = oe-ordm.po-no-po.


        FIND FIRST oe-ordl WHERE
             oe-ordl.company EQ prmComp AND 
             oe-ordl.ord-no = oe-ordm.ord-no  AND
             oe-ordl.LINE   = int(vLine) 
            /* oe-ordl.est-no = oe-ordm.est-no*/
             NO-LOCK.
        
        IF oe-ordm.tax  = TRUE THEN
           ASSIGN
             ttMiscCharge.Tax      = "Y".
        ELSE 
           ASSIGN
             ttMiscCharge.Tax      = "N".

        ASSIGN
            ttMiscCharge.vItem    = oe-ordl.i-no
            ttMiscCharge.Bill     = oe-ordm.bill
            ttMiscCharge.Weight   = oe-ord.t-weight
           /* ttMiscCharge.Tax1     = oe-ordl.tax*/
            ttMiscCharge.Freight  = oe-ord.t-freight
            ttMiscCharge.BillFre  = oe-ord.f-bill
            ttMiscCharge.Total    = oe-ord.t-revenue
            ttMiscCharge.Cost     = oe-ord.t-cost
            ttMiscCharge.Comm     = oe-ord.t-comm.
        
    END. /*for each oe-ordm OF oe-ordl no-lock :*/
      
 END.   /*FOR EACH oe-ord*/

 
