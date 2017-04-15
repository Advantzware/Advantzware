/* ar-inv.i */
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR op-delete-choice AS LOG NO-UNDO.

RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"clear-target",OUTPUT char-hdl).

IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
DO:
   RUN undo-added IN WIDGET-HANDLE(char-hdl) (OUTPUT op-delete-choice).
   IF op-delete-choice = NO THEN
      RETURN NO-APPLY.
END.

IF scr-manual-check-no:HIDDEN IN FRAME {&FRAME-NAME} EQ NO THEN
   scr-manual-check-no:SCREEN-VALUE = "".
