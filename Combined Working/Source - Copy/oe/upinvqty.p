/* ---------------------------------------------------oe/upinvqty.p 3/94 RM   */
/*  Update Inventory Quantities, Put Qty Back Before Update  - o/e module     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER fil_id AS RECID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xitemfg for itemfg.
DEF BUFFER xitemfg-loc FOR itemfg-loc.

def var v-save-id as recid.
def var v-part-qty as dec.

{fg/fullset.i NEW}


find oe-ordl where recid(oe-ordl) = fil_id no-lock no-error.
IF AVAIL oe-ordl THEN FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
if not avail oe-ord then return.

find first itemfg where itemfg.company = cocode and
        itemfg.i-no = oe-ordl.i-no no-error.
RUN fg/chkfgloc.p (INPUT oe-ordl.i-no, INPUT oe-ord.loc).
FIND FIRST itemfg-loc 
    WHERE itemfg-loc.company EQ cocode
      AND itemfg-loc.i-no    EQ oe-ordl.i-no
      AND itemfg-loc.loc     EQ oe-ord.loc
    EXCLUSIVE-LOCK NO-ERROR.

 if avail itemfg then
 do:
   IF oe-ord.type NE "T" THEN itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
   IF oe-ord.type NE "T" AND AVAIL(itemfg-loc) THEN 
       itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-ordl.qty.
   IF itemfg.q-alloc LT 0 THEN DO:
       itemfg.q-alloc = 0.
       IF AVAIL itemfg-loc THEN
       itemfg-loc.q-alloc = 0.
   END.

   assign itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
          itemfg.q-ptd = itemfg.q-ptd - oe-ordl.qty
          itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.

   IF AVAIL(itemfg-loc) THEN
     assign itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
            itemfg-loc.q-ptd = itemfg-loc.q-ptd - oe-ordl.qty
            itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - oe-ordl.qty.
   FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
   find first itemfg where itemfg.company = cocode and
         itemfg.i-no = oe-ordl.i-no no-lock no-error.

   IF itemfg.isaset THEN DO:     /** Update Set Parts */
     RUN fg/fullset.p (ROWID(itemfg)).

     FOR EACH tt-fg-set,
         FIRST xitemfg
         WHERE xitemfg.company EQ tt-fg-set.company
           AND xitemfg.i-no    EQ tt-fg-set.part-no:
       RUN fg/chkfgloc.p (INPUT tt-fg-set.part-no, INPUT oe-ord.loc).
       FIND FIRST xitemfg-loc 
           WHERE xitemfg-loc.company EQ cocode
             AND xitemfg-loc.i-no    EQ tt-fg-set.part-no
             AND xitemfg-loc.loc     EQ oe-ord.loc
           EXCLUSIVE-LOCK NO-ERROR.       

       IF oe-ord.type NE "T" THEN
         xitemfg.q-alloc = xitemfg.q-alloc -
                           (oe-ordl.qty * tt-fg-set.part-qty-dec).
        IF oe-ord.type NE "T" AND AVAIL xitemfg-loc THEN
         xitemfg-loc.q-alloc = xitemfg-loc.q-alloc -
                           (oe-ordl.qty * tt-fg-set.part-qty-dec).
       IF xitemfg.q-alloc LT 0 THEN DO:
           xitemfg.q-alloc = 0.
           IF AVAIL xitemfg-loc THEN
           xitemfg-loc.q-alloc = 0.
       END.

       ASSIGN
        xitemfg.q-avail   = xitemfg.q-onh + xitemfg.q-ono - xitemfg.q-alloc
        xitemfg.q-ptd     = xitemfg.q-ptd -
                            (oe-ordl.qty * tt-fg-set.part-qty-dec)
        xitemfg.q-ord-ytd = xitemfg.q-ord-ytd -
                            (oe-ordl.qty * tt-fg-set.part-qty-dec).

       IF AVAIL(xitemfg-loc) THEN
       ASSIGN
        xitemfg-loc.q-avail   = xitemfg-loc.q-onh + xitemfg-loc.q-ono - xitemfg-loc.q-alloc
        xitemfg-loc.q-ptd     = xitemfg-loc.q-ptd -
                            (oe-ordl.qty * tt-fg-set.part-qty-dec)
        xitemfg-loc.q-ord-ytd = xitemfg-loc.q-ord-ytd -
                            (oe-ordl.qty * tt-fg-set.part-qty-dec).
       FIND CURRENT xitemfg-loc NO-LOCK NO-ERROR.
     END.
   END. /* isaset */
 end.
