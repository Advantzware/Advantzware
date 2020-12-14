/* sys-ctrl.i */

&IF '{&post-enable}' NE '' &THEN
    RUN {&post-enable}.
&ENDIF
sys-ctrl.isPassword:SENSITIVE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("sfIsUserSuperAdmin").
