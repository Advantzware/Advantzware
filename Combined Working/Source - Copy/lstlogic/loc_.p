/* loc_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME loc_.
&Scoped-define LISTORDER Location,Description
&Scoped-define WHERE-STATEMENT loc.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY loc-query FOR loc.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)" LABEL "Beginning  Location" NO-UNDO.
DEFINE VARIABLE begin_loc_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)" LABEL "Ending Location" NO-UNDO.
DEFINE VARIABLE end_loc_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.
DEFINE VARIABLE show-fg-bins AS LOGICAL FORMAT "yes/no" LABEL "Show Finished Goods' Bins" NO-UNDO.
DEFINE VARIABLE show-rm-bins AS LOGICAL FORMAT "yes/no" LABEL "Show Raw Materials' Bins" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY loc-query FOR EACH loc NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        loc.loc GE begin_loc AND
        loc.loc LE end_loc.
    WHEN 2 THEN
    OPEN QUERY loc-query FOR EACH loc NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        loc.dscr GE begin_loc_dscr AND
        loc.dscr LE end_loc_dscr.
  END CASE.
  GET FIRST loc-query.
  DO WHILE AVAILABLE(loc)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/loc_.i}
    DOWN.
    GET NEXT loc-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_loc.
  IMPORT begin_loc_dscr.
  IMPORT end_loc.
  IMPORT end_loc_dscr.
  IMPORT show-fg-bins.
  IMPORT show-rm-bins.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_loc COLON 40
    begin_loc_dscr COLON 40
    end_loc COLON 40
    end_loc_dscr COLON 40
    show-fg-bins COLON 40
    show-rm-bins COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
