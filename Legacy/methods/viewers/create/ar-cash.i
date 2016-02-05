/* ar-cash.i */
DEF VAR char-hdl AS cha NO-UNDO.


RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"invhead-source", OUTPUT char-hdl).
IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  RUN undo-added IN WIDGET-HANDLE(char-hdl).
