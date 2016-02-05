DEF VAR cepartition-int AS INT NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEPARTITION"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
   CREATE sys-ctrl.
   ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "CEPARTITION"
      sys-ctrl.descrip = "Default Rev. Grain for Assembled Partitions".
END.

cepartition-int = sys-ctrl.int-fld.
 
