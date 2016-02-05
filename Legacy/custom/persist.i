/* persist.i - Persistent Internal Procedures */

/* new procedures should be added alphabetically */

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEF VAR lv-reckey2 AS cha NO-UNDO.

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

PROCEDURE get-reckey2:
  DEFINE OUTPUT PARAMETER op-reckey2 AS cha NO-UNDO.
  op-reckey2 = lv-reckey2 .  
END PROCEDURE.

PROCEDURE set-reckey2:
  DEFINE INPUT PARAMETER ip-reckey2 AS cha NO-UNDO.
  lv-reckey2 = ip-reckey2  .  
END PROCEDURE.


