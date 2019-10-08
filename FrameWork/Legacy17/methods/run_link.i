/* run_link.i */

&IF "{&IAMWHAT}" = "" &THEN
IF VALID-HANDLE(adm-broker-hdl) THEN DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'{1}':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  IF VALID-HANDLE(phandle) THEN
  RUN {2} IN phandle {3} NO-ERROR.
END.
&ENDIF
