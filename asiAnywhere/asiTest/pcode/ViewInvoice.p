



/*------------------------------------------------------------------------
    File        : ViewInvoice.p
    Purpose     : ViewInvoice

    Syntax      :

    Description : Return a Dataset of all View Invoice

    Author(s)   : Jyoti Bajaj
    Created     : dec 19 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */




{ViewInvoice.i}
    
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmInv       as INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewInv.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmInv      = ? THEN ASSIGN prmInv      = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */

   
        FOR EACH ar-invl where ar-invl.ord-no  = int(prmOrderNum) AND ar-invl.inv-no = prmInv
                           AND ar-invl.company = prmComp NO-LOCK
                           BY ar-invl.LINE:
            FIND FIRST ttViewInv WHERE ttViewInv.LINE = ar-invl.LINE NO-LOCK NO-ERROR.
            IF AVAIL ttViewInv THEN NEXT.
            CREATE ttViewInv.
            ASSIGN
                ttViewInv.LINE         = ar-invl.LINE
                ttViewInv.actnum       = ar-invl.actnum
                ttViewInv.act-dscr     = get-actdscr()
                ttViewInv.i-name       = ar-invl.i-name
                ttViewInv.i-dscr       = get-i-dscr()
                ttViewInv.inv-qty      = ar-invl.inv-qty
                ttViewInv.cons-uom     = ar-invl.cons-uom
                ttViewInv.sf-sht       = ar-invl.sf-sht
                ttViewInv.unit-pr      = ar-invl.unit-pr
                ttViewInv.pr-qty-uom   = ar-invl.pr-qty-uom
                ttViewInv.amt          = ar-invl.amt
                ttViewInv.amt-msf      = ar-invl.amt-msf .
        FIND FIRST ar-inv WHERE ar-inv.x-no = ar-invl.x-no
            AND ar-inv.cust-no = ar-invl.cust-no 
            AND ar-invl.inv-no = ar-invl.inv-no NO-LOCK NO-ERROR.
        
        IF AVAIL ar-inv THEN DO:                          
            assign 
                ttViewInv.cust-no      = ar-inv.cust-no
                ttViewInv.cust-name    = ar-inv.cust-name
                ttViewInv.ship-id      = ar-inv.ship-id
                ttViewInv.inv-no       = ar-inv.inv-no
                ttViewInv.tax-code     = ar-inv.tax-code
                ttViewInv.terms        = ar-inv.terms
                ttViewInv.terms-d      = ar-inv.terms-d
                ttViewInv.inv-date     = ar-inv.inv-date
                ttViewInv.due-date     = ar-inv.due-date
                ttViewInv.ord-no       = ar-invl.ord-no
                ttViewInv.disc-%       = ar-inv.disc-% 
                ttViewInv.disc-days    = ar-inv.disc-days
                ttViewInv.carrier      = ar-inv.carrier
                ttViewInv.cost         = ar-invl.cost
                ttViewInv.gross        = ar-inv.gross
                ttViewInv.freight      = ar-inv.freight
                ttViewInv.tax-amt      = ar-inv.tax-amt
                ttViewInv.disc-taken   = ar-inv.disc-taken
                ttViewInv.paid         = ar-inv.paid
                ttViewInv.due          = ar-inv.due
                ttViewInv.po-no        = ar-inv.po-no
               
                ttViewInv.vRowid       = recid(ar-invl).
        END.   /*if avail ar-inv*/
        END.    /*FOR EACH ar-invl*/
/*********************************************************************************/

