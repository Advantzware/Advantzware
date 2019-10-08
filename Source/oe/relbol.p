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
DEF VAR v-email AS LOG NO-UNDO.
DEF VAR vrRelh AS ROWID NO-UNDO.
DEF VAR iocPrompt AS CHAR NO-UNDO.

{oe/relemail.i}
{oe/chkordl.i NEW}

find xoe-ordl where recid(xoe-ordl) eq ip-recid no-lock.
find oe-ordl where recid(oe-ordl) eq ip-recid no-lock.
find oe-rel where recid(oe-rel) eq out-recid no-lock no-error.

relh-recid = ?.

if not avail oe-rel then find first oe-rel {oe/oe-relW.i} no-lock.
if oe-rel.link-no eq 0 then do:
  /* {oe/actrel.i} - wfk 03011305 */
  RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "CREATE", 
                       INPUT-OUTPUT iocPrompt, OUTPUT vrRelh).
/*     leave rel-block. */
/*   end.               */
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

