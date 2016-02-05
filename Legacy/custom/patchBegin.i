/* patchBegin.i */

&SCOPED-DEFINE patch {1}
&SCOPED-DEFINE version '{2}'
&SCOPED-DEFINE seq '{3}'
&SCOPED-DEFINE utility {4}

FUNCTION dependancyOK RETURNS LOGICAL
  (ipPatch AS INTEGER,ipVersion AS CHARACTER,ipSeq AS CHARACTER,ipDependancy AS CHARACTER):
  
  DEFINE VARIABLE runOrder AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  j = NUM-ENTRIES(ipDependancy).
  DO i = 1 TO j:
    runOrder = INTEGER(ENTRY(i,ipDependancy)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    IF CAN-FIND(FIRST patchhst
                WHERE patchhst.patch EQ ipPatch
                  AND patchhst.version EQ ipVersion
                  AND patchhst.seq EQ ipSeq
                  AND patchhst.run-order EQ runOrder
                  AND patchhst.completed EQ NO) THEN
    RETURN FALSE.
  END.
  RETURN TRUE.
END FUNCTION.

FIND FIRST patchhst NO-LOCK WHERE patchhst.patch EQ {&patch}
                              AND patchhst.version EQ {&version}
                              AND patchhst.seq EQ {&seq}
                              AND patchhst.utility EQ '{&utility}' NO-ERROR.
IF NOT AVAILABLE patchhst THEN DO:
  MESSAGE 'Patch History record:' SKIP(1)
    'Patch: ' {&patch} SKIP
    'Version: ' {&version} SKIP
    'Seq: ' {&seq} SKIP
    'Utility: ' '{&utility}' SKIP(1)
    'Does not exist, unable to execute this program.'
      VIEW-AS ALERT-BOX ERROR TITLE '{&utility}'.
  RETURN.
END.

IF patchhst.completed THEN DO:
  IF patchhst.run-once THEN DO:
    MESSAGE 'Patch History indicates this process has already' SKIP
            'been run, unable to execute this program.'
        VIEW-AS ALERT-BOX ERROR TITLE '{&utility}'.
    RETURN.
  END.
  ELSE DO:
    MESSAGE 'Patch History indicates this process has already' SKIP
            'been run, execute this program again?.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE '{&utility}'
        UPDATE continue AS LOGICAL.
    IF NOT continue THEN RETURN.
  END.
END.

IF NOT dependancyOK({&patch},{&version},{&seq},patchhst.dependancy) THEN DO:
  MESSAGE 'Patch History indicates prior dependancies have' SKIP
          'not been run, unable to execute this program.'
      VIEW-AS ALERT-BOX ERROR TITLE '{&utility}'.
  RETURN.
END.
