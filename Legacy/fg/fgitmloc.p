DEF INPUT PARAMETER ip-i-no AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-row AS ROWID NO-UNDO. /* oe-ordl or oe-rel */

DEF VAR v-rel-qty AS INT NO-UNDO.
DEF VAR v-qty-to-assign AS INT NO-UNDO.
DEF VAR v-first-one AS LOG NO-UNDO.
DEF VAR v-tot-alloc AS DEC NO-UNDO.
/* Case 1: Change of location on an oe-rel */
/* Quantity represented by this oe-rel may or may not equal oe-rel.qty */
/* So can't just take this quantity out of one itemfg-loc and put it  */
/* In another. So, have to start over with this order line and examine */
/* All releases so that we distribute the quantities on oe-rel to the */
/* itemfg-loc records without going over the original quantity on the */
/* oe-ordl                                                            */

/* Case 2: Order quantity was updated */
/* This will change the total q-alloc for this item, so have to do the */
/* same rebalancing as in case 1 */

/* Find the oe-ordl */
FIND FIRST oe-rel WHERE ROWID(oe-rel) EQ ip-row NO-LOCK NO-ERROR.
IF AVAIL oe-rel THEN
    FIND FIRST oe-ordl 
           WHERE oe-ordl.company EQ oe-rel.company
             AND oe-ordl.ord-no  EQ oe-rel.ord-no
             AND oe-ordl.LINE    EQ oe-rel.LINE
             AND oe-ordl.i-no    EQ oe-rel.i-no
           NO-LOCK NO-ERROR.
ELSE
    FIND FIRST oe-ordl
           WHERE ROWID(oe-ordl) EQ ip-row NO-LOCK NO-ERROR.

IF NOT AVAIL oe-ordl THEN
    RETURN. /* Nothing to work with */

/* WFK - 7/31/13 - removed old code and started over */
def var v-q-back like itemfg.q-back no-undo.
find itemfg where itemfg.company eq oe-ordl.company
  and itemfg.i-no eq oe-ordl.i-no
  no-lock no-error.
for each oe-rel where oe-rel.company eq oe-ordl.company
 and oe-rel.ord-no eq oe-ordl.ord-no
 and oe-rel.line  eq oe-ordl.line
 no-lock
 BREAK BY oe-rel.spare-char-1:
  IF FIRST-OF(oe-rel.spare-char-1) THEN DO:
  
      FIND itemfg-loc WHERE itemfg-loc.company EQ oe-ordl.company
          AND itemfg-loc.i-no EQ oe-ordl.i-no
          AND itemfg-loc.loc EQ oe-rel.spare-char-1
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF AVAIL itemfg-loc THEN
      RUN fg/calcqabl.p (ROWID(itemfg), oe-rel.spare-char-1, OUTPUT itemfg-loc.q-alloc,     OUTPUT v-q-back).

  END.
end.
