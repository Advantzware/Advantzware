/* stateco_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME stateco_.
&Scoped-define LISTORDER State Abbreviation,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY statecod-query FOR statecod.

DEFINE VARIABLE begin_statecod AS CHARACTER FORMAT "X(10)" LABEL "Beginning  State Abbreviation" NO-UNDO.
DEFINE VARIABLE begin_statecod_description AS CHARACTER FORMAT "X(30)" LABEL "Beginning State Name" NO-UNDO.
DEFINE VARIABLE end_statecod AS CHARACTER FORMAT "X(10)" LABEL "Ending State Abbreviation" NO-UNDO.
DEFINE VARIABLE end_statecod_description AS CHARACTER FORMAT "X(30)" LABEL "Ending State Name" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY statecod-query FOR EACH statecod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        statecod.statecod GE begin_statecod AND
        statecod.statecod LE end_statecod.
    WHEN 2 THEN
    OPEN QUERY statecod-query FOR EACH statecod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        statecod.description GE begin_statecod_description AND
        statecod.description LE end_statecod_description.
  END CASE.
  GET FIRST statecod-query.
  DO WHILE AVAILABLE(statecod)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/stateco_.i}
    DOWN.
    GET NEXT statecod-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_statecod.
  IMPORT begin_statecod_description.
  IMPORT end_statecod.
  IMPORT end_statecod_description.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_statecod COLON 40
    begin_statecod_description COLON 40
    end_statecod COLON 40
    end_statecod_description COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
