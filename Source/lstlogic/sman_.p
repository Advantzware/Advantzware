/* sman_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME sman_.
&Scoped-define LISTORDER Sales Rep,Sales Rep Name
&Scoped-define WHERE-STATEMENT sman.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY sman-query FOR sman.

DEFINE VARIABLE begin_sman AS CHARACTER FORMAT "X(3)" LABEL "Beginning Sales Rep" NO-UNDO.
DEFINE VARIABLE begin_sman_sname AS CHARACTER FORMAT "X(20)" LABEL "Beginning Name" NO-UNDO.
DEFINE VARIABLE end_sman AS CHARACTER FORMAT "X(3)" LABEL "Ending Sales Rep" NO-UNDO.
DEFINE VARIABLE end_sman_sname AS CHARACTER FORMAT "X(20)" LABEL "Ending Name" NO-UNDO.
DEFINE VARIABLE tb_matrix AS LOGICAL FORMAT "yes/no" LABEL "Print Commission Matrix?" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY sman-query FOR EACH sman NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        sman.sman GE begin_sman AND
        sman.sman LE end_sman.
    WHEN 2 THEN
    OPEN QUERY sman-query FOR EACH sman NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        sman.sname GE begin_sman_sname AND
        sman.sname LE end_sman_sname.
  END CASE.
  GET FIRST sman-query.
  DO WHILE AVAILABLE(sman)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/sman_.i}
    DOWN.
    GET NEXT sman-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_sman.
  IMPORT begin_sman_sname.
  IMPORT end_sman.
  IMPORT end_sman_sname.
  IMPORT tb_matrix.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_sman LABEL "Beginning Sales Rep" COLON 40
    begin_sman_sname COLON 40
    end_sman LABEL "Ending Sales Rep" COLON 40
    end_sman_sname COLON 40
    tb_matrix COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
