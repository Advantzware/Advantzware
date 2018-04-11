/* ar-inv.i */
DEF VAR char-hdl AS cha NO-UNDO.
define variable lCError as logical no-undo .

RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"invhead-source", OUTPUT char-hdl).
IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  RUN undo-added IN WIDGET-HANDLE(char-hdl).


&IF DEFINED(create-more) > 0  &THEN
   RUN CheckCreate(output lCError) .
if lCError then return no-apply .
&ENDIF
