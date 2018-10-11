/* est.i - window local-view - rstark 9.30.2018 */

&IF "{&winViewPrgmName}" EQ "w-est" &THEN
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN entry-to-frame IN WIDGET-HANDLE(char-hdl) .
&ENDIF
