
def input parameter v-recid as recid no-undo.

def buffer xoe-rell for oe-rell.
def buffer xoe-relh for oe-relh.


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
    itemfg.q-back = itemfg.q-back - oe-rell.qty.
    if itemfg.q-back lt 0 then itemfg.q-back = 0.
  end.
        
  find first oe-rel where oe-rel.r-no eq oe-rell.link-no 
      use-index seq-no no-error.
  if avail oe-rel then delete oe-rel.
end.
