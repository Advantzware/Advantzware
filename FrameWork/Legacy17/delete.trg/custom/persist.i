/* persist.i - Persistent Internal Procedures */

/* new procedures should be added alphabetically */

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_period AS INTEGER NO-UNDO.

PROCEDURE Get-Company:
  DEFINE OUTPUT PARAMETER op-company AS CHARACTER NO-UNDO.
  op-company = g_company.
END PROCEDURE.

PROCEDURE Get-Location:
  DEFINE OUTPUT PARAMETER op-location AS CHARACTER NO-UNDO.
  op-location = g_loc.
END PROCEDURE.

PROCEDURE Get-Period:
  DEFINE OUTPUT PARAMETER op-period AS INTEGER NO-UNDO.
  op-period = g_period.
END PROCEDURE.
