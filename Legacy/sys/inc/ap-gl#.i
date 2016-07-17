
def var ap-gl#-log like sys-ctrl.log-fld  no-undo.
def var ap-gl#-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AP GL#"
    no-error.
do transaction:
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "AP GL#"
     sys-ctrl.descrip  = "Default GL# from Purchasing?"
     sys-ctrl.log-fld  = no.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  
  if sys-ctrl.char-fld eq "" then sys-ctrl.char-fld = "Asset".
end.

assign
 ap-gl#-log = sys-ctrl.log-fld
 ap-gl#-cha = sys-ctrl.char-fld.
 
