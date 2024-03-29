/* persist.i - Persistent Internal Procedures */

/* new procedures should be added alphabetically */

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_loc     AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_period  AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE g_sysdate AS DATE      NO-UNDO.

DEFINE VARIABLE lv-reckey2 AS CHARACTER NO-UNDO.

{addon/custom/emprate.i}
{addon/custom/shftproc.i}

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
  DEFINE OUTPUT PARAMETER op-reckey2 AS CHARACTER NO-UNDO.
  op-reckey2 = lv-reckey2.  
END PROCEDURE.

PROCEDURE Set-AdvantzWare-Values:
  DEFINE INPUT PARAMETER h_mainmenu AS WIDGET-HANDLE NO-UNDO.

  FIND FIRST usr NO-LOCK
       WHERE usr.uid EQ USERID("NOSWEAT")
       NO-ERROR.
  IF AVAILABLE usr THEN DO:
    FIND FIRST company NO-LOCK
         WHERE company.company EQ usr.company
         NO-ERROR.
    IF NOT AVAILABLE company THEN
    FIND FIRST company NO-LOCK NO-ERROR.

    IF AVAILABLE usr AND AVAILABLE company THEN DO:
      FIND FIRST loc NO-LOCK
           WHERE loc.company EQ company.company 
             AND loc.loc EQ usr.loc
           NO-ERROR.
      IF NOT AVAILABLE loc THEN
      FIND FIRST loc NO-LOCK
           WHERE loc.company EQ company.company
           NO-ERROR.
      IF AVAILABLE loc THEN DO:
        ASSIGN
          g_company = company.company
          g_loc = loc.loc
          g_sysdate = TODAY
          .
        FIND FIRST period NO-LOCK
             WHERE period.company EQ g_company 
               AND period.pstat EQ TRUE   
               AND period.pst LE g_sysdate   
               AND period.pend GE g_sysdate 
               NO-ERROR.
        IF NOT AVAILABLE period THEN
        FIND LAST period NO-LOCK
             WHERE period.company EQ g_company
               AND period.pstat EQ TRUE
             NO-ERROR.
        IF AVAILABLE period THEN
        g_period = period.pnum.
      END.
    END.   
  END.
  IF VALID-HANDLE(h_mainmenu) THEN
  RUN Set-comp_loc IN h_mainmenu
     (g_company,
      IF AVAILABLE company THEN company.name ELSE '',
      g_loc,
      IF AVAILABLE loc THEN loc.dscr ELSE ''
     ).
END PROCEDURE.

PROCEDURE set-reckey2:
  DEFINE INPUT PARAMETER ip-reckey2 AS CHARACTER NO-UNDO.
  lv-reckey2 = ip-reckey2.  
END PROCEDURE.
