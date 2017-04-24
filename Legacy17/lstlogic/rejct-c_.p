/* rejct-c_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME rejct-c_.
&Scoped-define LISTORDER Rejection Code,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY rejct-cd-query FOR rejct-cd.

DEFINE VARIABLE begin_rejct-cd AS CHARACTER FORMAT "X(2)" LABEL "Beginning  Rejection Code" NO-UNDO.
DEFINE VARIABLE begin_rejct-cd_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_rejct-cd AS CHARACTER FORMAT "X(2)" LABEL "Ending Rejection Code" NO-UNDO.
DEFINE VARIABLE end_rejct-cd_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY rejct-cd-query FOR EACH rejct-cd NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        rejct-cd.code GE begin_rejct-cd AND
        rejct-cd.code LE end_rejct-cd.
    WHEN 2 THEN
    OPEN QUERY rejct-cd-query FOR EACH rejct-cd NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        rejct-cd.dscr GE begin_rejct-cd_dscr AND
        rejct-cd.dscr LE end_rejct-cd_dscr.
  END CASE.
  GET FIRST rejct-cd-query.
  DO WHILE AVAILABLE(rejct-cd)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/rejct-c_.i}
    DOWN.
    GET NEXT rejct-cd-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_rejct-cd.
  IMPORT begin_rejct-cd_dscr.
  IMPORT end_rejct-cd.
  IMPORT end_rejct-cd_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_rejct-cd COLON 40
    begin_rejct-cd_dscr COLON 40
    end_rejct-cd COLON 40
    end_rejct-cd_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
