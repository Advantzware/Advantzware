
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "LoadTagLimit"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "LoadTagLimit"
   sys-ctrl.module  = "OU7"
   sys-ctrl.descrip = "Limit the maximum number of load tags to print"
   sys-ctrl.int-fld = 120
   sys-ctrl.dec-fld = 50 .
end.

