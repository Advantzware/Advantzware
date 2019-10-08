/* company_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME company_.
&Scoped-define LISTORDER Company,Name
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY company-query FOR company.

DEFINE VARIABLE begin_company AS CHARACTER FORMAT "X(3)" LABEL "Beginning  Company" NO-UNDO.
DEFINE VARIABLE begin_company_name AS CHARACTER FORMAT "X(30)" LABEL "Beginning Name" NO-UNDO.
DEFINE VARIABLE end_company AS CHARACTER FORMAT "X(3)" LABEL "Ending Company" NO-UNDO.
DEFINE VARIABLE end_company_name AS CHARACTER FORMAT "X(30)" LABEL "Ending Name" NO-UNDO.
DEFINE VARIABLE show-open-periods AS LOGICAL FORMAT "yes/no" LABEL "Show Open Periods" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY company-query FOR EACH company NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        company.company GE begin_company AND
        company.company LE end_company.
    WHEN 2 THEN
    OPEN QUERY company-query FOR EACH company NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        company.name GE begin_company_name AND
        company.name LE end_company_name.
  END CASE.
  GET FIRST company-query.
  DO WHILE AVAILABLE(company)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/company_.i}
    DOWN.
    GET NEXT company-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_company.
  IMPORT begin_company_name.
  IMPORT end_company.
  IMPORT end_company_name.
  IMPORT show-open-periods.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_company COLON 40
    begin_company_name COLON 40
    end_company COLON 40
    end_company_name COLON 40
    show-open-periods COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
