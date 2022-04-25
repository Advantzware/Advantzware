FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "POCOST"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "POCOST"
        sys-ctrl.descrip  = "Default Raw Material Cost via?   Hold Overprices?"
        sys-ctrl.char-fld = "JobFile"
        sys-ctrl.log-fld  = NO.
    /*message "Default Raw Material Cost via?  (JobFile,Vendor)"
        update sys-ctrl.char-fld.
    */
    RUN sys/ref/d-pocost.w  (OUTPUT sys-ctrl.char-fld).
    MESSAGE "Hold Overprices?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
