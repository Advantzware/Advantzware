
def var v-fg-copy like sys-ctrl.log-fld.
DEF VAR cecopy-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cecopy-int LIKE sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CECOPY"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CECOPY"
   sys-ctrl.log-fld = no
   sys-ctrl.descrip = "Allow copy FG item blank info when adding estimates?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
ASSIGN
 v-fg-copy  = sys-ctrl.log-fld
 cecopy-cha = sys-ctrl.char-fld
 cecopy-int = sys-ctrl.int-fld.
