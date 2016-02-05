/* sys/inc/SelRptCol.i       SelectRptColumn => Reports */

def var SelectRptColumn-log like sys-ctrl.log-fld no-undo.
def var SelectRptColumn-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "Reports" /*"SelectRptColumn"*/
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "Reports" /*SelectRptColumn"*/
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Selectable Report Columns".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 SelectRptColumn-log = sys-ctrl.log-fld
 SelectRptColumn-cha = sys-ctrl.char-fld.

FIND FIRST sys-ctrl-shipto OF sys-ctrl 
    WHERE sys-ctrl-shipto.char-fld = "{1}" NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl-shipto THEN do:   /* Task 01201407  */

      create sys-ctrl-shipto.
  assign
   sys-ctrl-shipto.company  = cocode
   sys-ctrl-shipto.name     = "Reports" /*SelectRptColumn"*/
   sys-ctrl-shipto.log-fld  = no
   sys-ctrl-shipto.char-fld = "{1}"
   sys-ctrl-shipto.descrip  = "Selectable Report Columns".

  END.

IF AVAIL sys-ctrl-shipto THEN
   assign
 SelectRptColumn-log = sys-ctrl-ship.log-fld
 SelectRptColumn-cha = sys-ctrl.char-fld.
