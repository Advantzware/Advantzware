/* company.i - window local-view - rstark 9.30.2018 */

DEFINE VARIABLE hPgmSecurity AS HANDLE  NO-UNDO.
DEFINE VARIABLE lResult      AS LOGICAL NO-UNDO.

RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("windows/company.w", "", OUTPUT lResult).
DELETE OBJECT hPgmSecurity.
IF NOT lResult THEN 
RUN disable-add-button IN h_f-add.
