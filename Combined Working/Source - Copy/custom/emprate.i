/* emprate.i */

PROCEDURE Employee-Rate:
  DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-employee AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-shift AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-machine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-rate_usage AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ip-ratetype AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-rate AS DECIMAL NO-UNDO.

  DEFINE BUFFER buf-rate FOR rate.

  IF ip-rate_usage THEN
  ip-machine = ''.
  FIND buf-rate WHERE buf-rate.company = ip-company
                  AND buf-rate.employee = ip-employee
                  AND buf-rate.shift = ip-shift
                  AND buf-rate.machine = ip-machine
                  AND buf-rate.ratetype = 'Standard'
             NO-LOCK NO-ERROR.
  IF NOT AVAILABLE buf-rate THEN
  RETURN.
  op-rate = buf-rate.rate.
  FIND buf-rate WHERE buf-rate.company = ip-company
                  AND buf-rate.employee = ip-employee
                  AND buf-rate.shift = ip-shift
                  AND buf-rate.machine = ip-machine
                  AND buf-rate.ratetype = ip-ratetype
             NO-LOCK NO-ERROR.
  IF AVAILABLE buf-rate THEN
  CASE buf-rate.factortype:
    WHEN 'Straight' THEN
    op-rate = buf-rate.rate.
    WHEN 'Additional' THEN
    op-rate = op-rate + buf-rate.rate.
    WHEN 'Multiply' THEN
    op-rate = op-rate * buf-rate.rate.
    OTHERWISE
    op-rate = 0.
  END CASE.
END PROCEDURE.
