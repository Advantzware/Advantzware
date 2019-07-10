

/*------------------------------------------------------------------------
    File        : Inventory.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewItem.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItem.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
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

DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

/* ********************  Preprocessor Definitions  ******************** */
        

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum) NO-LOCK :
  /* FOR EACH itemfg where itemfg.company = oe-ordl.company AND itemfg.i-no = oe-ordl.i-no no-lock :*/
        create ttViewItem.
        assign 
            ttViewItem.OrdNo       = oe-ordl.ord-no
            ttViewItem.vLine       = oe-ordl.LINE
            ttViewItem.CustPart       = oe-ordl.part-no
            ttViewItem.Item1          = oe-ordl.i-no
            ttViewItem.Name1          = oe-ordl.i-name
            ttViewItem.Dscr           = string(oe-ordl.part-dscr1 + ", " + oe-ordl.part-dscr2 + ", " + oe-ordl.part-dscr3)
            ttViewItem.quantity       = oe-ordl.qty
            ttViewItem.price          = oe-ordl.price
            ttViewItem.uom            = oe-ordl.pr-uom
            ttViewItem.counter        = oe-ordl.cas-cnt
            ttViewItem.custpo         = oe-ordl.po-no
            ttViewItem.taxable        = oe-ordl.tax
            ttViewItem.discount       = oe-ordl.disc
            ttViewItem.requested      = oe-ordl.req-code
            ttViewItem.requestdate    = oe-ordl.req-date
            ttViewItem.extprice       = oe-ordl.t-price
            ttViewItem.promised       = oe-ordl.prom-code
            ttViewItem.promisdate     = oe-ordl.prom-date
            ttViewItem.shipqty        = oe-ordl.ship-qty
        .
              /* END.  /*FOR EACH  itemfg*/*/
        
    END.   /*FOR EACH oe-ordl*/
   
    



