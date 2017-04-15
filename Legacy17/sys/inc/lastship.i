
def var lastship-cha like sys-ctrl.char-fld no-undo.
def var lastship-int like sys-ctrl.int-fld  no-undo.
def var lastship-dec like sys-ctrl.dec-fld  no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "LASTSHIP"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "LASTSHIP"
   sys-ctrl.descrip  = "Method to calculate Due Date and Last Ship Date"
   sys-ctrl.char-fld = "ASI".
   
 /* message sys-ctrl.descrip "(ASI/Fibre)" update sys-ctrl.char-fld. */
          
  if sys-ctrl.char-fld eq "Fibre" then do:
    message "Manufacturing Days:" update sys-ctrl.int-fld.
    message "Folding Factor:"     update sys-ctrl.dec-fld.
  end.
  
  else
  if sys-ctrl.char-fld ne "ASI" then sys-ctrl.char-fld = "ASI".
end.

assign
 lastship-cha = sys-ctrl.char-fld
 lastship-int = sys-ctrl.int-fld
 lastship-dec = sys-ctrl.dec-fld.
 
if lastship-dec eq 0 then lastship-dec = 1.

