
def var v-rmissue like sys-ctrl.char-fld.
DEF VAR rmissue-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR rmissue-int LIKE sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RMISSUE"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RMISSUE"
   sys-ctrl.char-fld = "Gross"
   sys-ctrl.descrip  = "Use Net or Gross Sheet L&W for RM Issue quantity?".
   
 /* message sys-ctrl.descrip update sys-ctrl.char-fld. */
end.

ASSIGN
 v-rmissue   = sys-ctrl.char-fld
 rmissue-log = sys-ctrl.log-fld
 rmissue-int = sys-ctrl.int-fld.

