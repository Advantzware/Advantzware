/* stax_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME stax_.
&Scoped-define LISTORDER Sales Tax Group
&Scoped-define WHERE-STATEMENT stax.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY stax-query FOR stax.

DEFINE VARIABLE begin_tax-group AS CHARACTER FORMAT "X(10)" LABEL "Beginning  Tax Group" NO-UNDO.
DEFINE VARIABLE end_tax-group AS CHARACTER FORMAT "X(10)" LABEL "Ending Tax Group" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY stax-query FOR EACH stax NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        stax.tax-group GE begin_tax-group AND
        stax.tax-group LE end_tax-group.
  END CASE.
  GET FIRST stax-query.
  DO WHILE AVAILABLE(stax)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/stax_.i}
    DOWN.
    GET NEXT stax-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_tax-group.
  IMPORT end_tax-group.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_tax-group COLON 40
    end_tax-group COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
