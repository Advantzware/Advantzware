
DEF VAR cecomm-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR cecomm-dec LIKE sys-ctrl.dec-fld NO-UNDO.
DEF VAR cecomm-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cecomm-log LIKE sys-ctrl.log-fld NO-UNDO.
    

FIND FIRST sys-ctrl
    where sys-ctrl.company EQ cocode
      and sys-ctrl.name    EQ "CECOMM"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CECOMM"
   sys-ctrl.descrip = "Include Sales Rep Commission in CE Sell Price Calculation?"
   sys-ctrl.log-fld = YES.
  
END.

ASSIGN
 cecomm-int = sys-ctrl.int-fld
 cecomm-dec = sys-ctrl.dec-fld
 cecomm-cha = sys-ctrl.char-fld
 cecomm-log = sys-ctrl.log-fld.

