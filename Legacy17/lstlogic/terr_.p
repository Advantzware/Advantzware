/* terr_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME terr_.
&Scoped-define LISTORDER Territory,Description
&Scoped-define WHERE-STATEMENT stax.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY terr-query FOR terr.

DEFINE VARIABLE begin_terr AS CHARACTER FORMAT "X(3)" LABEL "Beginning Territory" NO-UNDO.
DEFINE VARIABLE begin_terr_dscr AS CHARACTER FORMAT "X(20)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_terr AS CHARACTER FORMAT "X(3)" LABEL "Ending Territory" NO-UNDO.
DEFINE VARIABLE end_terr_dscr AS CHARACTER FORMAT "X(20)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY terr-query FOR EACH terr NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        terr.terr GE begin_terr AND
        terr.terr LE end_terr.
    WHEN 2 THEN
    OPEN QUERY terr-query FOR EACH terr NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        terr.dscr GE begin_terr_dscr AND
        terr.dscr LE end_terr_dscr.
  END CASE.
  GET FIRST terr-query.
  DO WHILE AVAILABLE(terr)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/terr_.i}
    DOWN.
    GET NEXT terr-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_terr.
  IMPORT begin_terr_dscr.
  IMPORT end_terr.
  IMPORT end_terr_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_terr COLON 40
    begin_terr_dscr COLON 40
    end_terr COLON 40
    end_terr_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
