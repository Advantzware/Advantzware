/*sys/inc/sstransf.i */
def var sstransf-log like sys-ctrl.log-fld no-undo.
def var sstransf-char like sys-ctrl.char-fld no-undo.
def var sstransf-int like sys-ctrl.int-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "SSTRANSF"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "SSTRANSF"
   sys-ctrl.descrip = "Sharp Shooter FG Item Transfer Prompt?".

 
end.
ASSIGN sstransf-log = sys-ctrl.log-fld
       sstransf-char = sys-ctrl.char-fld
       sstransf-int = sys-ctrl.int-fld
       .
