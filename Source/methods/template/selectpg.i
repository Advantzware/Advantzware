/* selectpg.i */

DEFINE VARIABLE test-integer AS INTEGER NO-UNDO.

test-integer = INTEGER({&BROWSE-NAME}:PRIVATE-DATA) NO-ERROR.
IF ERROR-STATUS:ERROR OR {&BROWSE-NAME}:PRIVATE-DATA = ? THEN
DO:
  MESSAGE "Invalid or No Page Number Found in Browser's Private Data!"
      VIEW-AS ALERT-BOX WARNING.
  RETURN NO-APPLY.
END.
{&autofind}
{methods/run_link.i "CONTAINER-SOURCE" "SELECT-PAGE"
    "(INTEGER({&BROWSE-NAME}:PRIVATE-DATA))"}
