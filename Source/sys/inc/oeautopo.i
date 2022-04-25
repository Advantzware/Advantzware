FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "OEAUTOPO"
    NO-LOCK NO-ERROR.
            
IF NOT AVAILABLE sys-ctrl THEN 
DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "OEAUTOPO"
        sys-ctrl.descrip  = "Auto/Manual PO Creation from Order Entry? Multiple Jobs per PO?"
        sys-ctrl.char-fld = "Manual"
        sys-ctrl.log-fld  = NO.
                
    RUN po/d-oepo.w (OUTPUT sys-ctrl.char-fld).
    MESSAGE "Create PO with Multiple Jobs?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
    