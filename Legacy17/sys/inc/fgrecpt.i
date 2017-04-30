
def var fgrecpt like sys-ctrl.log-fld NO-UNDO.
def var fgrecpt-int like sys-ctrl.int-fld NO-UNDO.
DEF VAR fgrecpt-char LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR fgrecpt-dec LIKE sys-ctrl.dec-fld NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGRECPT"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGRECPT"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Is Job# or PO# mandatory on FG Receipts?".
   
 
end.
ASSIGN
 fgrecpt      = sys-ctrl.log-fld
 fgrecpt-int  = sys-ctrl.int-fld
 fgrecpt-char = sys-ctrl.char-fld
 fgrecpt-dec  = sys-ctrl.dec-fld.

