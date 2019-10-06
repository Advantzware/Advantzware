/*sys/inc/oeexport.i */
def var oeexport-log like sys-ctrl.log-fld no-undo.
def var oeexport-char like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEEXPORT"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEEXPORT"
   sys-ctrl.descrip = "Batch export of open order line data?"
   sys-ctrl.char-fld = "C:\tmp\FCORINTG.DAT"   .

  /*MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
end.
ASSIGN oeexport-log = sys-ctrl.log-fld
       oeexport-char = sys-ctrl.char-fld
       .
