

/*------------------------------------------------------------------------
    File        : ViewItem.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewItem.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItem.

DEFINE VARIABLE v-use-rel      AS LOG NO-UNDO.
DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE VARIABLE li-onh AS DECIMAL.
DEFINE VARIABLE li-ono AS DECIMAL.
DEFINE VARIABLE li-all AS DECIMAL.
DEFINE VARIABLE li-bor AS DECIMAL.
DEFINE VARIABLE li-ava AS DECIMAL.
DEFINE VARIABLE li-reo AS DECIMAL.
 
DEF VAR v-type AS CHAR NO-UNDO.
DEF VARIABLE op-alloc AS DECIMAL NO-UNDO.
DEF var oereordr-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF var oereordr-cha LIKE sys-ctrl.char-fld NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
        

FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = int(prmOrderNum) NO-LOCK :
  /* FOR EACH itemfg where itemfg.company = oe-ordl.company AND itemfg.i-no = oe-ordl.i-no no-lock :*/
        create ttViewItem.
        assign 
            ttViewItem.OrdNo          = oe-ordl.ord-no
            ttViewItem.vLine          = oe-ordl.LINE
            ttViewItem.est-no         = oe-ordl.est-no
            ttViewItem.Item1          = oe-ordl.i-no
            ttViewItem.CustPart       = oe-ordl.part-no
            ttViewItem.quantity       = oe-ordl.qty
            ttViewItem.Name1          = oe-ordl.i-name
            ttViewItem.Dscr           = string(oe-ordl.part-dscr1 + ", " + oe-ordl.part-dscr2 + ", " + oe-ordl.part-dscr3)
            ttViewItem.price          = oe-ordl.price
            ttViewItem.uom            = oe-ordl.pr-uom
            ttViewItem.taxable        = oe-ordl.tax
            ttViewItem.custpo         = oe-ordl.po-no
            ttViewItem.job-no         = oe-ordl.job-no
            ttViewItem.discount       = oe-ordl.disc
            ttViewItem.requested      = oe-ordl.req-code
            ttViewItem.requestdate    = oe-ordl.req-date
            ttViewItem.extprice       = oe-ordl.t-price
            ttViewItem.promised       = oe-ordl.prom-code
            ttViewItem.promisdate     = oe-ordl.prom-date
            ttViewItem.shipqty        = oe-ordl.ship-qty
            ttViewItem.counter        = oe-ordl.cas-cnt
            ttViewItem.vReckey        = oe-ordl.rec_key
            ttViewItem.over        = oe-ordl.over-pct
            ttViewItem.under        = oe-ordl.under-pct

        .
              /* END.  /*FOR EACH  itemfg*/*/
        
    END.   /*FOR EACH oe-ordl*/
   
    



