
def input parameter v-recid as recid no-undo.

def buffer xoe-rell for oe-rell.
def buffer xoe-relh for oe-relh.
DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.

find oe-rell where recid(oe-rell) eq v-recid no-lock no-error.
if not avail oe-rell then return.

find oe-relh where oe-relh.r-no eq oe-rell.r-no no-lock no-error.
if not avail oe-relh then return.

find first oe-ordl
    where oe-ordl.company eq oe-rell.company
      and oe-ordl.ord-no  eq oe-relh.ord-no
      and oe-ordl.line    eq oe-rell.line
    no-error.

if avail oe-ordl then do:
  oe-ordl.rel-stat = no.
  
  /** Check to see if any other releases are against this item **/
  for each xoe-relh
      where xoe-relh.company eq oe-rell.company
        and xoe-relh.ord-no  eq oe-ordl.ord-no
      use-index order no-lock,
      each xoe-rell
      where xoe-rell.company eq oe-rell.company
        and xoe-rell.r-no    eq xoe-relh.r-no
        and xoe-rell.i-no    eq oe-ordl.i-no
        and xoe-rell.line    eq oe-ordl.line
        and recid(xoe-rell)  ne recid(oe-rell)
      USE-INDEX r-no no-lock:
    oe-ordl.rel-stat = yes.
    leave.
  end.
end.

if oe-relh.b-ord-no gt 0 then do:
  find first itemfg
      where itemfg.company eq oe-rell.company
        and itemfg.i-no    eq oe-rell.i-no
      no-error.
  if avail itemfg then do:
    RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
    FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company EQ oe-rell.company
        AND itemfg-loc.i-no    EQ oe-rell.i-no
        AND itemfg-loc.loc     EQ oe-rell.loc
      EXCLUSIVE-LOCK NO-ERROR.

    itemfg.q-back = itemfg.q-back - oe-rell.qty.
    IF AVAIL itemfg-loc THEN
      itemfg-loc.q-back = itemfg-loc.q-back - oe-rell.qty.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    IF itemfg.q-back LT 0 THEN DO:
        itemfg.q-back = 0.
        IF AVAIL itemfg-loc THEN
          itemfg-loc.q-back = 0.
    END.

  END.
        
  find first oe-rel where oe-rel.r-no eq oe-rell.link-no 
      use-index seq-no no-error.
  if avail oe-rel then DO: 
    FIND FIRST itemfg-loc
           WHERE itemfg-loc.company EQ oe-rel.company
             AND itemfg-loc.i-no    EQ oe-rel.i-no
             AND itemfg-loc.loc     EQ oe-rel.spare-char-1
           EXCLUSIVE-LOCK NO-ERROR.
    FIND itemfg WHERE itemfg.company EQ oe-rel.company
       AND itemfg.i-no EQ oe-rel.i-no
       NO-LOCK NO-ERROR.
    IF AVAIL itemfg AND AVAIL(itemfg-loc) THEN
       RUN fg/calcqabl.p (ROWID(itemfg), oe-rel.spare-char-1, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
      delete oe-rel.
  END.
end.
