DEF VAR ecbrowse-chr AS CHAR NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ECBROWSE"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
   CREATE sys-ctrl.
   ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "ECBROWSE"
      sys-ctrl.descrip = "Corrugated Labels".
END.

ecbrowse-chr = sys-ctrl.char-fld.
 
