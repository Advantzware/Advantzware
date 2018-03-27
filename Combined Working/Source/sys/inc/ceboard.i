/* sys/inc/ceprint.i*/
def var ceboard-log AS LOG NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "CEBOARD"
     no-lock no-error.
if not avail sys-ctrl then DO:
  create sys-ctrl.
  ASSIGN sys-ctrl.company  = cocode
         sys-ctrl.name     = "CEBOARD"
         sys-ctrl.log-fld  = no
         sys-ctrl.descrip  = "Prompt to Override Board Cost and Use Vendor Cost?".
end.
ASSIGN ceboard-log = sys-ctrl.log-fld.
       

