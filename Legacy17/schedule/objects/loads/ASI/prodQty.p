/* prodQty.p */

DEFINE INPUT PARAMETER ipCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipMachine     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipJobNumber   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipJobSub      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipFormNumber  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipBlankNumber AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipPass        AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER opProdQty AS INTEGER NO-UNDO.

FOR EACH machtran NO-LOCK
    WHERE machtran.company       EQ ipCompany
      AND machtran.machine       EQ ipMachine
      AND machtran.job_number    EQ ipJobNumber
      AND machtran.job_sub       EQ ipJobSub
      AND machtran.form_number   EQ ipFormNumber
      AND machtran.blank_number  EQ ipBlankNumber
      AND machtran.pass_sequence EQ ipPass,
    FIRST job-code NO-LOCK
    WHERE job-code.code EQ machtran.charge_code
      AND job-code.cat  EQ 'RUN':
  opProdQty = opProdQty + machtran.run_qty.
END. /* each machtran */
