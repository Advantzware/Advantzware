/*sys/inc/quoimage.i */
def var quoimage-log like sys-ctrl.log-fld no-undo.
def var quoimage-cha like sys-ctrl.char-fld no-undo.

find first sys-ctrl where sys-ctrl.company eq g_company
                      and sys-ctrl.name    eq "QUOIMAGE"
                      no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "QUOIMAGE"
   sys-ctrl.descrip = "Quote Image File Name"
   sys-ctrl.char-fld = "C:/tmp/quote.jpg"   .
  /*MESSAGE sys-ctrl.char-fld
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
end.
ASSIGN quoimage-log = sys-ctrl.log-fld
       quoimage-cha = sys-ctrl.char-fld
       .
