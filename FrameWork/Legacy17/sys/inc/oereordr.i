
DEF var oereordr-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF var oereordr-cha LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
      AND sys-ctrl.name    eq "OEREORDR"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEREORDR"
   sys-ctrl.descrip = "Use Actual Releases to calculate Qty Allocated in OE?"
   sys-ctrl.log-fld = NO.
   
  
END.
ASSIGN
 oereordr-log = sys-ctrl.log-fld
 oereordr-cha = sys-ctrl.char-fld.
