/* nextRFID.i - rstark 11.15.2011 */

&IF DEFINED(company) EQ 0 &THEN
&SCOPED-DEFINE company g_company
&ENDIF

FUNCTION dec2Hex RETURNS CHARACTER (ipDec AS DECIMAL):
  DEFINE VARIABLE vHEX AS CHARACTER NO-UNDO.
  
  DO WHILE TRUE ON STOP UNDO:
    CASE INTEGER(ipDec - (16 * (TRUNCATE(ipDec / 16, 0)))):
      WHEN 10 THEN vHEX = 'a' + vHEX.
      WHEN 11 THEN vHEX = 'b' + vHEX.
      WHEN 12 THEN vHEX = 'c' + vHEX.
      WHEN 13 THEN vHEX = 'd' + vHEX.
      WHEN 14 THEN vHEX = 'e' + vHEX.
      WHEN 15 THEN vHEX = 'f' + vHEX.
      OTHERWISE vHEX = STRING(ipDec - (16 * (TRUNCATE(ipDec / 16, 0)))) + vHEX.
    END CASE.
    ipDec = TRUNCATE(ipDec / 16, 0).
    IF ipDec EQ 0 THEN STOP.
  END.
  vHEX = FILL('0',24 - LENGTH(vHEX)) + vHEX.
  RETURN vHEX.
END FUNCTION.

FUNCTION nextRFID RETURNS CHARACTER:
  DEFINE VARIABLE nextRFID AS CHARACTER NO-UNDO.

  DEFINE BUFFER bf-oe-ctrl FOR oe-ctrl.
  DEFINE BUFFER bf-sys-ctrl FOR sys-ctrl.
  
  FIND FIRST bf-sys-ctrl NO-LOCK
       WHERE bf-sys-ctrl.company EQ {&company}
         AND bf-sys-ctrl.name EQ 'RFIDTAG' NO-ERROR.
  IF NOT AVAILABLE bf-sys-ctrl THEN DO:
    CREATE bf-sys-ctrl.
    ASSIGN
      bf-sys-ctrl.company = {&company}
      bf-sys-ctrl.name = 'RFIDTAG'
      bf-sys-ctrl.descrip = 'RFID Location of Text File to Import'
      bf-sys-ctrl.log-fld = NO.
  END. /* not avail bf-sys-ctrl */

  IF bf-sys-ctrl.log-fld THEN DO:
    FIND FIRST bf-oe-ctrl EXCLUSIVE-LOCK.
    ASSIGN
      bf-oe-ctrl.n-rfid = bf-oe-ctrl.n-rfid + 1
      nextRFID = dec2Hex(bf-oe-ctrl.n-rfid).
  END.

  RETURN nextRFID.
END FUNCTION.
