/* C:\asi_gui10\pco1010\dataLoad\Vendor.p */

DEFINE VARIABLE dataField AS CHARACTER NO-UNDO EXTENT 15.

MESSAGE 'Clear Table "vend" Before Load?'
  VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL
  UPDATE clearTable AS LOGICAL.
CASE clearTable:
  WHEN YES THEN
  FOR EACH vend EXCLUSIVE-LOCK:
    DELETE vend.
  END.
  WHEN ? THEN RETURN.
END CASE.

INPUT FROM "C:\asi_gui10\pco1010\dataLoad\Vendor.txt" NO-ECHO.
IMPORT ^. /* skip header line */
REPEAT:
  IMPORT DELIMITER "~t"
    dataField[1]
    dataField[2]
    dataField[3]
    dataField[4]
    dataField[5]
    dataField[6]
    dataField[7]
    dataField[8]
    dataField[9]
    dataField[10]
    dataField[11]
    dataField[12]
    dataField[13]
    dataField[14]
    dataField[15]
    .
  CREATE vend.
  ASSIGN
    vend.vend-no = dataField[1]
    vend.name = dataField[2]
    vend.terms = dataField[3]
    vend.disc-% = DECIMAL(dataField[4])
    vend.add1 = dataField[5]
    vend.add2 = dataField[6]
    vend.city = dataField[7]
    vend.state = dataField[8]
    vend.Postal = dataField[9]
    vend.phone = dataField[10]
    vend.contact = dataField[11]
    vend.fax = dataField[12]
    vend.actnum = dataField[13]
    vend.tax-id = dataField[14]
    vend.code-1099 = dataField[15]
    .
END.
INPUT CLOSE.
MESSAGE "Program: C:\asi_gui10\pco1010\dataLoad\Vendor.p - Run Complete" VIEW-AS ALERT-BOX.
