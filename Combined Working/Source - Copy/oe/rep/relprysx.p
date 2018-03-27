/* ---------------------------------------------- oe/rep/relprysx.p  */
/* Print oe Release/Picking tickets for Prystup Xprint              */
/* ------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relprysx.i "oe-relh.r-no"}
PROCEDURE right-just:
DEFINE INPUT  PARAMETER ip-total-len AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ip-first-len AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ip-first-value AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ip-2nd-value AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER op-final-result AS CHARACTER   NO-UNDO.

DEF VAR i AS INT.
DEF VAR j AS INT.
DEF VAR i-2nd-len AS INT.

ip-first-value = ip-first-value + " ".
i-2nd-len = ip-total-len - ip-first-len.

i = LENGTH(ip-first-value).
j = LENGTH(ip-2nd-value).
IF i LT ip-first-len THEN
    ip-first-value = FILL(" ", ip-first-len - i) + ip-first-value.
OVERLAY(op-final-result, 1, ip-first-len) = ip-first-value.
IF j LT i-2nd-len THEN
    ip-2nd-value = FILL(" ", i-2nd-len - j) + ip-2nd-value.
OVERLAY(op-final-result, ip-first-len + 1, i-2nd-len) = ip-2nd-value.


END PROCEDURE.
