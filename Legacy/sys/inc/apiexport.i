FIND FIRST sys-ctrl
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "APIExport"
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "APIExport"
        sys-ctrl.module   = ""
        sys-ctrl.log-fld  = TRUE
        sys-ctrl.descrip  = "Path to export the Outbound Events data"
        sys-ctrl.char-fld = "C:\BA\Label\"
        .
END.
