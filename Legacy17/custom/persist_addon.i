/* persist.i - Persistent Internal Procedures */

/* new procedures should be added alphabetically */

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE g_sysdate AS DATE NO-UNDO.

{custom/emprate.i}

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

PROCEDURE Set-AdvantzWare-Values:
  DEFINE INPUT PARAMETER h_mainmenu AS WIDGET-HANDLE NO-UNDO.

  FIND FIRST usr WHERE usr.uid EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
  IF AVAILABLE usr THEN
  DO:
    FIND FIRST company WHERE company.company EQ usr.company NO-LOCK NO-ERROR.
    IF NOT AVAILABLE company THEN
    FIND FIRST company NO-LOCK NO-ERROR.

    IF AVAILABLE usr AND AVAILABLE company THEN
    DO:
      FIND FIRST loc WHERE loc.company EQ company.company 
                       AND loc.loc EQ usr.loc NO-LOCK NO-ERROR.
      IF NOT AVAILABLE loc THEN
      FIND FIRST loc WHERE loc.company EQ company.company NO-LOCK NO-ERROR.
      IF AVAILABLE loc THEN
      DO:
	 ASSIGN
	   g_company = company.company
	   g_loc = loc.loc
          g_sysdate = TODAY.

        FIND FIRST period WHERE period.company EQ g_company 
                            AND period.pstat EQ TRUE   
                            AND period.pst LE g_sysdate   
                            AND period.pend GE g_sysdate NO-LOCK NO-ERROR.
        IF NOT AVAILABLE period THEN
        FIND LAST period WHERE period.company EQ g_company AND
                               period.pstat EQ TRUE NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN
        g_period = period.pnum.
      END.
    END.   
  END.
  IF VALID-HANDLE(h_mainmenu) THEN
  RUN Set-comp_loc IN h_mainmenu
     (g_company,IF AVAILABLE company THEN company.name ELSE '',
      g_loc,IF AVAILABLE loc THEN loc.dscr ELSE '').
END PROCEDURE.

{custom/shftproc.i}
