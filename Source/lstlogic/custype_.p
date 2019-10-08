/* custype_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME custype_.
&Scoped-define LISTORDER Type,Description
&Scoped-define WHERE-STATEMENT stax.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY custype-query FOR custype.

DEFINE VARIABLE begin_custype AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Customer Type" NO-UNDO.
DEFINE VARIABLE begin_custype_dscr AS CHARACTER FORMAT "X(20)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_custype AS CHARACTER FORMAT "X(8)" LABEL "Ending Customer Type" NO-UNDO.
DEFINE VARIABLE end_custype_dscr AS CHARACTER FORMAT "X(20)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY custype-query FOR EACH custype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        custype.custype GE begin_custype AND
        custype.custype LE end_custype.
    WHEN 2 THEN
    OPEN QUERY custype-query FOR EACH custype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        custype.dscr GE begin_custype_dscr AND
        custype.dscr LE end_custype_dscr.
  END CASE.
  GET FIRST custype-query.
  DO WHILE AVAILABLE(custype)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/custype_.i}
    DOWN.
    GET NEXT custype-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_custype.
  IMPORT begin_custype_dscr.
  IMPORT end_custype.
  IMPORT end_custype_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_custype COLON 40
    begin_custype_dscr COLON 40
    end_custype COLON 40
    end_custype_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
