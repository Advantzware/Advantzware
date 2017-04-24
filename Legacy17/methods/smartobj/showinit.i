/* showinit.i */

DEFINE VARIABLE notes-init AS LOGICAL NO-UNDO.
DEFINE VARIABLE miscflds-init AS LOGICAL NO-UNDO.
DEFINE VARIABLE addresses-init AS LOGICAL NO-UNDO.
DEFINE VARIABLE phones-init AS LOGICAL NO-UNDO.

{methods/run_link.i "CONTAINER-SOURCE" "Init-Show-Parameters"
    "(OUTPUT notes-init,OUTPUT miscflds-init,OUTPUT addresses-init,OUTPUT phones-init)"}
DO WITH FRAME {&FRAME-NAME}:
  DISPLAY {&LIST-1}.
  ASSIGN
    show-notes:SENSITIVE = notes-init
    show-misc-field-values:SENSITIVE = miscflds-init
    show-addresses:SENSITIVE = addresses-init
    show-phones:SENSITIVE = phones-init.
END.
