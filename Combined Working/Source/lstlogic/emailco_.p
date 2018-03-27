/* emailco_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME emailco_.
&Scoped-define LISTORDER Title Code,Description
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY emailcod-query FOR emailcod.

DEFINE VARIABLE begin_emailcod AS CHARACTER FORMAT "X(10)" LABEL "Beginning  Title Code" NO-UNDO.
DEFINE VARIABLE begin_emailcod_description AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_emailcod AS CHARACTER FORMAT "X(10)" LABEL "Ending Title Code" NO-UNDO.
DEFINE VARIABLE end_emailcod_description AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY emailcod-query FOR EACH emailcod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        emailcod.emailcod GE begin_emailcod AND
        emailcod.emailcod LE end_emailcod.
    WHEN 2 THEN
    OPEN QUERY emailcod-query FOR EACH emailcod NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        emailcod.description GE begin_emailcod_description AND
        emailcod.description LE end_emailcod_description.
  END CASE.
  GET FIRST emailcod-query.
  DO WHILE AVAILABLE(emailcod)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/emailco_.i}
    DOWN.
    GET NEXT emailcod-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_emailcod.
  IMPORT begin_emailcod_description.
  IMPORT end_emailcod.
  IMPORT end_emailcod_description.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_emailcod COLON 40
    begin_emailcod_description COLON 40
    end_emailcod COLON 40
    end_emailcod_description COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
