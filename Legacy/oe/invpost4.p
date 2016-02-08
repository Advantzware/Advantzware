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
DEF VAR v-calc-loc  AS   CHAR NO-UNDO.

{fg/fullset.i NEW}


find inv-line where recid(inv-line) eq v-recid no-lock.
find inv-head where inv-head.r-no   eq inv-line.r-no NO-LOCK NO-ERROR.
v-shp-qty = inv-line.ship-qty * v-factor.

find first itemfg
    {sys/look/itemfgrlW.i}
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


  FIND FIRST oe-boll
    where oe-boll.company eq itemfg.company
      and oe-boll.b-no    eq inv-line.b-no
      and oe-boll.ord-no  eq inv-line.ord-no
      and oe-boll.i-no    eq inv-line.i-no
      and oe-boll.line    eq inv-line.line
      and oe-boll.po-no   eq inv-line.po-no
    NO-LOCK NO-ERROR.
  IF AVAIL oe-boll THEN
    v-calc-loc = oe-boll.loc.
  ELSE
    v-calc-loc = itemfg.loc.
  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT v-calc-loc).
  FIND FIRST itemfg-loc 
         WHERE itemfg-loc.company EQ itemfg.company
           AND itemfg-loc.i-no    EQ inv-line.i-no
           AND itemfg-loc.loc     EQ v-calc-loc
         EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL itemfg-loc THEN
  ASSIGN itemfg-loc.q-onh = itemfg-loc.q-onh - v-shp-qty
         itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
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

      RUN fg/chkfgloc.p (INPUT b-itemfg.i-no, INPUT v-calc-loc).
      FIND FIRST itemfg-loc 
             WHERE itemfg-loc.company EQ b-itemfg.company
               AND itemfg-loc.i-no    EQ b-itemfg.i-no
               AND itemfg-loc.loc     EQ v-calc-loc
             EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL itemfg-loc THEN
      ASSIGN itemfg-loc.q-onh = itemfg-loc.q-onh - (v-shp-qty * tt-fg-set.part-qty-dec)
             itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.

    END.
  END.
END.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
