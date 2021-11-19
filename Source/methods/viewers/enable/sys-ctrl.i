/* sys-ctrl.i */

&IF '{&post-enable}' NE '' &THEN
    RUN {&post-enable}.
&ENDIF
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        sys-ctrl.isPassword:SENSITIVE = lSuperAdmin
        sys-ctrl.isActive:SENSITIVE   = lSuperAdmin
        .
END.
