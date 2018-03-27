/* costtyp_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME costtyp_.
&Scoped-define LISTORDER Cost Type,Description
&Scoped-define WHERE-STATEMENT costtype.company = gcompany AND costtype.loc = gloc
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY costtype-query FOR costtype.

DEFINE VARIABLE begin_cost-type AS CHARACTER FORMAT "X(3)" LABEL "Beginning  Cost Type" NO-UNDO.
DEFINE VARIABLE begin_costtype_descr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_cost-type AS CHARACTER FORMAT "X(3)" LABEL "Ending Cost Type" NO-UNDO.
DEFINE VARIABLE end_costtype_descr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY costtype-query FOR EACH costtype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        costtype.cost-type GE begin_cost-type AND
        costtype.cost-type LE end_cost-type.
    WHEN 2 THEN
    OPEN QUERY costtype-query FOR EACH costtype NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        costtype.descr GE begin_costtype_descr AND
        costtype.descr LE end_costtype_descr.
  END CASE.
  GET FIRST costtype-query.
  DO WHILE AVAILABLE(costtype)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/costtyp_.i}
    DOWN.
    GET NEXT costtype-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_cost-type.
  IMPORT begin_costtype_descr.
  IMPORT end_cost-type.
  IMPORT end_costtype_descr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_cost-type COLON 40
    begin_costtype_descr COLON 40
    end_cost-type COLON 40
    end_costtype_descr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
