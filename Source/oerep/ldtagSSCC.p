/*------------------------------------------------------------------------
    File        : oerep/ldtagSSCC.p
    Purpose     : generate/maintain load tag SSCC values
    Syntax      : run oerep/loadTagSSCC.p (input company,
                                           input customer-no, 
                                          output sscc)
    Description : generate/maintain load tag SSCC values
    Author(s)   : Ron Stark
    Created     : 1.23.2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipCustNo AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opSSCC AS CHARACTER NO-UNDO.

DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE checkDigit AS INTEGER NO-UNDO.
  
/* ***************************  Main Block  *************************** */

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ ipCompany
       AND sys-ctrl.name EQ 'LoadTagSSCC'
       AND sys-ctrl.log-fld EQ YES
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN RETURN.
/*
IF NOT CAN-FIND(FIRST sys-ctrl-shipto
                WHERE sys-ctrl-shipto.company EQ sys-ctrl.company
                  AND sys-ctrl-shipto.name EQ sys-ctrl.name
                  AND sys-ctrl-shipto.cust-vend EQ YES
                  AND sys-ctrl-shipto.cust-vend-no EQ ipCustNo
                  AND sys-ctrl-shipto.log-fld EQ YES) THEN RETURN.
*/
opSSCC = '00'
       + STRING(sys-ctrl.dec-fld,'9')
       + sys-ctrl.char-fld
       + STRING(sys-ctrl.int-fld,'99999')
       .
DO idx = 3 TO LENGTH(opSSCC):
  checkDigit = checkDigit + INT(SUBSTR(opSSCC,idx,1)) * (idx MOD 2 * 2 + 1).
END. /* do idx */

FIND CURRENT sys-ctrl EXCLUSIVE-LOCK.
ASSIGN
  checkDigit = IF checkDigit MOD 10 EQ 0 THEN 0 ELSE 10 - checkDigit MOD 10
  opSSCC = opSSCC + STRING(checkDigit)
  sys-ctrl.int-fld = sys-ctrl.int-fld + 1
  .
IF sys-ctrl.int-fld GT 99999 THEN
ASSIGN
  sys-ctrl.dec-fld = sys-ctrl.dec-fld + 1
  sys-ctrl.int-fld = 0
  .
IF sys-ctrl.dec-fld GT 9 THEN
ASSIGN sys-ctrl.dec-fld = 0.
FIND CURRENT sys-ctrl NO-LOCK.
