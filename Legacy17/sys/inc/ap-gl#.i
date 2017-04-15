
DEFINE VARIABLE ap-gl#-log LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE ap-gl#-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "AP GL#"
    NO-ERROR.
DO TRANSACTION:
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "AP GL#"
            sys-ctrl.descrip = "Default GL# from Purchasing?"
            sys-ctrl.log-fld = NO.
       
    END.
  
    IF sys-ctrl.char-fld EQ "" THEN sys-ctrl.char-fld = "Asset".
    FIND CURRENT sys-ctrl NO-LOCK.
END.

ASSIGN
    ap-gl#-log = sys-ctrl.log-fld
    ap-gl#-cha = sys-ctrl.char-fld.
 
