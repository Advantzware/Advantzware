
DEFINE VARIABLE ap-gl#-log LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE ap-gl#-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AP GL#"
    no-error.
do transaction:
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "AP GL#"
     sys-ctrl.descrip  = "Default GL# from Purchasing?"
     sys-ctrl.log-fld  = no.
       
  end.
  
  if sys-ctrl.char-fld eq "" then sys-ctrl.char-fld = "Asset".
    FIND CURRENT sys-ctrl NO-LOCK.
end.

assign
 ap-gl#-log = sys-ctrl.log-fld
 ap-gl#-cha = sys-ctrl.char-fld.
 
