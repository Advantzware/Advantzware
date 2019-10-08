
DEF VAR tskey-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name    EQ "TSKEYBOARD"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = g_company
   sys-ctrl.name    = "TSKEYBOARD"
   sys-ctrl.descrip = "Display Touch Screen On Screen Keyboard?"
   sys-ctrl.log-fld = YES.
end.

tskey-log = sys-ctrl.log-fld.
