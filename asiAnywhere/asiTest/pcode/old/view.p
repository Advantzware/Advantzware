

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
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItem.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        
        RUN build-qry .
        
                
        DATASET dsViewItem:FILL().
    END.
   END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  no-lock: 
for each itemfg where itemfg.i-no = oe-ordl.i-no no-lock :
      create ttViewItem.
      assign 
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
          ttViewItem.alloc          = itemfg.alloc
          /*ttViewItem.ord-level      = itemfg.ord-level*/
          ttViewItem.q-ono          = itemfg.q-ono
          ttViewItem.q-onh          = itemfg.q-onh
          ttViewItem.q-alloc        = itemfg.q-alloc
          ttViewItem.q-avail        = itemfg.q-back
          ttViewItem.q-back         = itemfg.q-avail
          .
END.   /*FOR EACH Itemfg*/
END.   /*FOR EACH oe-ordl*/
END PROCEDURE.

