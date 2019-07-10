/* -------------------------------------------------- oe/invpost4.p 02/99 JLF */
/* Relieve FG Inventory                                                       */
/* -------------------------------------------------------------------------- */

def input parameter v-recid  as recid.
def input parameter v-factor as int.

{sys/inc/var.i shared}

def buffer b-itemfg for itemfg.
def buffer b-oe-ordl for oe-ordl.

def var v-shp-qty   like inv-line.ship-qty.
def var v-part-qty  as   dec.

{fg/fullset.i NEW}


find inv-line where recid(inv-line) eq v-recid no-lock.
find inv-head where inv-head.r-no   eq inv-line.r-no NO-LOCK NO-ERROR.
v-shp-qty = inv-line.ship-qty * v-factor.

find first itemfg
    where (itemfg.company  = cocode )
      and itemfg.i-no eq inv-line.i-no
    no-error.

find first oe-ordl
    where oe-ordl.company eq inv-line.company
      and oe-ordl.ord-no  eq inv-line.ord-no
      and oe-ordl.line    eq inv-line.line
      and oe-ordl.i-no    eq inv-line.i-no
    use-index ord-no no-lock no-error.

if v-factor gt 0 and
   inv-line.qty - (if avail oe-ordl then oe-ordl.t-ship-qty else 0 +
                   v-shp-qty) lt 0 then
  itemfg.q-alloc = itemfg.q-alloc -
                   (v-shp-qty + (inv-line.qty -
                              (if avail oe-ordl then oe-ordl.t-ship-qty else 0 +
                               v-shp-qty))).
else
  itemfg.q-alloc = itemfg.q-alloc - v-shp-qty.

if itemfg.q-alloc lt 0 then itemfg.q-alloc = 0.

IF itemfg.isaset                                                    AND
   itemfg.alloc                                                     AND
   AVAIL oe-ordl                                                    AND
   CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN
 v-shp-qty = 0.

IF (v-shp-qty GT 0 AND v-factor EQ 1)          OR
   (v-shp-qty LT 0 AND v-factor EQ -1)         OR
   NOT CAN-FIND(FIRST sys-ctrl
                WHERE sys-ctrl.company EQ itemfg.company
                  AND sys-ctrl.name    EQ "BOLPOST"
                  AND sys-ctrl.log-fld EQ YES) THEN DO:

  ASSIGN
   itemfg.q-onh   = itemfg.q-onh - v-shp-qty
   itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.

  IF itemfg.isaset AND itemfg.alloc                                         AND
     (NOT AVAIL oe-ordl OR
      NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})) THEN DO:
    RUN fg/fullset.p (ROWID(itemfg)).

    FOR EACH tt-fg-set,
     
        FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
          AND b-itemfg.i-no    EQ tt-fg-set.part-no:

      b-itemfg.q-alloc = b-itemfg.q-alloc - (v-shp-qty * tt-fg-set.part-qty-dec).

      IF b-itemfg.q-alloc LT 0 THEN b-itemfg.q-alloc = 0.

      IF itemfg.alloc THEN
        b-itemfg.q-onh = b-itemfg.q-onh - (v-shp-qty * tt-fg-set.part-qty-dec).
    
      b-itemfg.q-avail = b-itemfg.q-onh + b-itemfg.q-ono - b-itemfg.q-alloc.
    END.
  END.
END.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
