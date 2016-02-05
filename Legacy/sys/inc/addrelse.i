
def var v-do-bol as log init no.
def var v-do-def as log init no.
def var v-do-chk as log init no.
def var addrelse-cha as char INIT "ASI" no-undo.
def var addrelse-dec as dec no-undo.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ADDRELSE"
    no-lock no-error.

assign
 v-do-bol     = avail sys-ctrl and sys-ctrl.log-fld and oe-ctrl.ship-from
 v-do-def     = avail sys-ctrl and sys-ctrl.int-fld eq 1
 v-do-chk     = avail sys-ctrl and sys-ctrl.dec-fld ne 0.

IF AVAIL sys-ctrl THEN
  ASSIGN
   addrelse-cha = sys-ctrl.char-fld
   addrelse-dec = sys-ctrl.dec-fld.
