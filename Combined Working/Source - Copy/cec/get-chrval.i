/* cec/get-chrval.i */
def var orderxfer like sys-ctrl.log-fld no-undo.
DEF VAR v-ordxchr-val AS CHAR NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                    and sys-ctrl.name    eq "ORDERXFER"
                    no-lock no-error.
if avail sys-ctrl then do:
  orderxfer = sys-ctrl.log-fld.
end.

if orderxfer then
DO:
   IF sys-ctrl.char-fld NE "" THEN
    ASSIGN v-ordxchr-val = sys-ctrl.char-fld.       
END.

