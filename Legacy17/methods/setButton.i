/* setButton.i - rstark - 2.22.2017 */

&SCOPED-DEFINE imageType .png

{methods/buttonImage.i {1} "{2}" {3}}

DO:
    &IF DEFINED(DontValidateError) EQ 0 &THEN
    IF VALID-HANDLE(adm-broker-hdl) THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"TableIO-Target":U,OUTPUT char-hdl).
      pHandle = WIDGET-HANDLE(char-hdl).
      IF VALID-HANDLE(pHandle) AND CAN-DO (pHandle:INTERNAL-ENTRIES, "pValidateError") THEN
      RUN pValidateError IN pHandle (OUTPUT lValidateError).
    END.
    IF lValidateError EQ NO THEN
    &ENDIF 
    DO:
        IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
            {methods/loadImageLabel.i {1} "{2}"}
        END. /* if winkitactive */
        IF {1}:LABEL NE "" THEN 
        {1}:LABEL = (IF {1}:LABEL BEGINS "~&" THEN "~&" ELSE "") + "{2}".
    END. /* if slerror */
END. /* do */
