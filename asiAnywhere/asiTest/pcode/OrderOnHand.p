
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
{OrderOnHand.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum   as character no-undo.
DEFINE INPUT PARAMETER prmOrderOnHand as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderFg.
DEFINE VARIABLE vbol-qty as int format "->>>,>>9" .
DEFINE VARIABLE vbol-qty1 as int format "->>>,>>9" .

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction   = ?  THEN ASSIGN prmAction   = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum   = ? THEN ASSIGN prmItemNum   = "".
IF prmOrderOnHand = ? THEN ASSIGN prmOrderOnHand = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

CASE prmAction:
    WHEN "select" THEN DO:
   
    FIND FIRST oe-ordl where
         oe-ordl.company EQ prmComp AND
         oe-ordl.ord-no = int(prmOrderNum) AND
         oe-ordl.i-no = prmItemNum
         NO-LOCK NO-ERROR.

    FIND FIRST itemfg WHERE
         itemfg.company EQ prmComp AND
         itemfg.i-no = oe-ordl.i-no AND
         itemfg.q-onh = int(prmOrderOnHand)
         NO-LOCK NO-ERROR.

    FOR EACH fg-bin where
        fg-bin.company eq itemfg.company and
        fg-bin.i-no    eq itemfg.i-no
        BREAK BY fg-bin.job-no BY fg-bin.job-no2 :

    create ttOrderFg.
    assign 
        ttOrderFg.job-no        = fg-bin.job-no
        ttOrderFg.job-no2       = String(fg-bin.job-no2)
        ttOrderFg.i-no          = itemfg.i-no
        ttOrderFg.loc           = fg-bin.loc
        ttOrderFg.loc-bin       = fg-bin.loc-bin
        ttOrderFg.tag           = fg-bin.tag
        ttOrderFg.cust-no       = fg-bin.cust-no
        ttOrderFg.cases         = string(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
        ttOrderFg.case-count    = string(fg-bin.case-count)
        ttOrderFg.cases-unit    = string(fg-bin.cases-unit)
        ttOrderFg.partial-count = string(fg-bin.partial-count)
        ttOrderFg.qty           = fg-bin.qty
        ttOrderFg.std-tot-cost  = fg-bin.std-tot-cost
        ttOrderFg.std-mat-cost  = fg-bin.std-mat-cost
        ttOrderFg.std-lab-cost  = fg-bin.std-lab-cost
        ttOrderFg.std-var-cost  = fg-bin.std-var-cost
        ttOrderFg.std-fix-cost  = fg-bin.std-fix-cost
        ttOrderFg.last-cost     = fg-bin.last-cost
        ttOrderFg.sell-uom      = fg-bin.pur-uom
        ttOrderFg.q-onh         = itemfg.q-onh
        ttOrderFg.q-ono         = itemfg.q-ono
        ttOrderFg.q-alloc       = itemfg.q-alloc
        ttOrderFg.q-back        = itemfg.q-back
        ttOrderFg.q-avail       = (itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc)
        ttOrderFg.reorder       = itemfg.ord-level
        ttOrderFg.reordermin    = itemfg.ord-min
        ttOrderFg.reordermax    = itemfg.ord-max
        .
find first job-hdr where job-hdr.company eq fg-bin.company
    and job-hdr.i-no    eq fg-bin.i-no
    and job-hdr.job-no  eq fg-bin.job-no
    and job-hdr.job-no2 eq fg-bin.job-no2
    use-index i-no no-lock no-error.
if avail job-hdr then
    assign 
    ttOrderFg.j-no = string(job-hdr.j-no).
    



    FOR EACH oe-relh NO-LOCK
          WHERE oe-relh.company EQ itemfg.company
                AND oe-relh.posted  EQ NO,
          EACH oe-rell NO-LOCK
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND oe-rell.i-no    EQ fg-bin.i-no
            AND oe-rell.loc     EQ fg-bin.loc
            AND oe-rell.loc-bin EQ fg-bin.loc-bin
            AND oe-rell.tag     EQ fg-bin.tag
            AND oe-rell.cust-no EQ fg-bin.cust-no:
        ASSIGN
      ttOrderFg.bol-qty =ttOrderFg.bol-qty + oe-rell.qty.
      END. 

   FOR EACH oe-bolh NO-LOCK
          WHERE oe-bolh.company EQ itemfg.company
            AND oe-bolh.posted  EQ NO,
          EACH oe-boll NO-LOCK
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            AND oe-boll.i-no    EQ fg-bin.i-no
            AND oe-boll.loc     EQ fg-bin.loc
            AND oe-boll.loc-bin EQ fg-bin.loc-bin
            AND oe-boll.tag     EQ fg-bin.tag
            AND oe-boll.cust-no EQ fg-bin.cust-no:
        ttOrderFg.bol-qty = ttOrderFg.bol-qty + oe-boll.qty.
      END.
      FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.
      IF NOT oe-ctrl.u-inv THEN
      FOR EACH inv-line
          WHERE inv-line.company EQ itemfg.company
            AND inv-line.i-no    EQ fg-bin.i-no,
          EACH oe-boll
          WHERE oe-boll.company EQ inv-line.company
            AND oe-boll.b-no    EQ inv-line.b-no
            AND oe-boll.ord-no  EQ inv-line.ord-no
            AND oe-boll.i-no    EQ inv-line.i-no
            AND oe-boll.po-no   EQ inv-line.po-no
            AND oe-boll.loc     EQ fg-bin.loc
            AND oe-boll.loc-bin EQ fg-bin.loc-bin
            AND oe-boll.tag     EQ fg-bin.tag
            AND oe-boll.cust-no EQ fg-bin.cust-no:
        ttOrderFg.bol-qty = ttOrderFg.bol-qty + oe-boll.qty.
      END.

      ttOrderFg.avl-qty = ttOrderFg.qty - ttOrderFg.rel-qty - ttOrderFg.bol-qty.
END. /*IF available fg-bin*/

    END.
END CASE.
