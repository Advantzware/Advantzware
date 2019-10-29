/* run_link.i */

&IF "{&IAMWHAT}" = "" &THEN
IF VALID-HANDLE(adm-broker-hdl) THEN DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'{1}':U,OUTPUT char-hdl).
  IF NUM-ENTRIES(char-hdl) LT 2 THEN DO:
      phandle = WIDGET-HANDLE(char-hdl).
      IF VALID-HANDLE(phandle) THEN
      RUN {2} IN phandle {3} NO-ERROR.
      IF (ERROR-STATUS:ERROR OR NOT VALID-HANDLE(phandle)) AND "{2}" EQ "pCallAudit" THEN
      MESSAGE 
          THIS-PROCEDURE:NAME "failed to RUN pCallAudit." SKIP(1)
          "Please notify Advantzware Development Team"
      VIEW-AS ALERT-BOX ERROR.
  END. /* if num-entries */
END. /* if valid-handle */
&ENDIF
