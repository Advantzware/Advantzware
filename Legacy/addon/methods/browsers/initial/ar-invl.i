DEF VAR char-hdl AS CHAR NO-UNDO.
  

RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, "dont-enable-source", OUTPUT char-hdl).
                      
IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RETURN.
