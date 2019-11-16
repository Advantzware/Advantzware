/* sys/inc/cecpurwas.i*/
def var cecpurwaste-log AS LOG NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "CECPURWAS"
     no-lock no-error.
if not avail sys-ctrl then DO:
  create sys-ctrl.
  ASSIGN sys-ctrl.company  = cocode
         sys-ctrl.name     = "CECPURWAS"
         sys-ctrl.log-fld  = no
         sys-ctrl.descrip  = "Remove Waste when Searching Purchased FARM Folder Costs?".
end.
ASSIGN cecpurwaste-log  = sys-ctrl.log-fld.
       

