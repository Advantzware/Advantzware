FIND FIRST sys-ctrl
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "APIConfig"
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "APIConfig"
        sys-ctrl.module   = ""
        sys-ctrl.log-fld  = TRUE
        sys-ctrl.descrip  = "Outbound API Configuration"
        sys-ctrl.char-fld = "Premier"
        sys-ctrl.int-fld  = 1
        .
END.
