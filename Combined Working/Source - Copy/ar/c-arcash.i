/* ar/c-arcash.i  */
DEF VAR char-hdl AS cha NO-UNDO.


IF ld-not-applied <> 0 THEN DO:
   MESSAGE "Apply all check amount first please." VIEW-AS ALERT-BOX ERROR.
   RETURN.     
END.

RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"invhead-source", OUTPUT char-hdl).
IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  RUN undo-added IN WIDGET-HANDLE(char-hdl).
