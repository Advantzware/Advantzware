/* ---------------------------------------------------- oe/relbol.p 01/98 JLF */
/* order entry - Create actual release for automatic BOL creation             */
/* -------------------------------------------------------------------------- */

def input parameter ip-recid as recid.

{sys/inc/var.i shared}

def shared buffer xoe-ordl for oe-ordl.

def shared var out-recid as recid no-undo.
def shared var relh-recid as recid no-undo.
def shared var v-auto    as log no-undo.
DEF VAR v-merge-prompt AS LOG INIT ? NO-UNDO.
def var choice as log no-undo.
/*DEF VAR v-email AS LOG NO-UNDO.*/

{oe/relemail.i}

find xoe-ordl where recid(xoe-ordl) eq ip-recid no-lock.
find oe-ordl where recid(oe-ordl) eq ip-recid no-lock.
find oe-rel where recid(oe-rel) eq out-recid no-lock no-error.

relh-recid = ?.

if not avail oe-rel then find first oe-rel 
use-index item
where (oe-rel.company eq cocode
  and  oe-rel.ord-no  eq xoe-ordl.ord-no
  and  oe-rel.i-no    eq xoe-ordl.i-no
  and  oe-rel.line    eq xoe-ordl.line) no-lock.

if oe-rel.link-no eq 0 then do:
  {oe/actrel.i}

    leave rel-block.
  end.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

