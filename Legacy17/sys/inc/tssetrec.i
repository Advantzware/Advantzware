/*tssetrec.i  02/24/05   YSK*/
def var tssetrec-log like sys-ctrl.log-fld no-undo.
DEF VAR tssetrec-char AS cha NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "TSSETREC" no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "TSSETREC"
   sys-ctrl.descrip = "Create Set Header Recipts via Touch Screen?".

 
end.
tssetrec-log = sys-ctrl.log-fld.
tssetrec-char = sys-ctrl.char-fld.
