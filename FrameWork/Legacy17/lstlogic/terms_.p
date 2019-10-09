/* terms_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME terms_.
&Scoped-define LISTORDER Terms,Description
&Scoped-define WHERE-STATEMENT stax.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY terms-query FOR terms.

DEFINE VARIABLE begin_t-code AS CHARACTER FORMAT "X(5)" LABEL "Beginning Terms" NO-UNDO.
DEFINE VARIABLE begin_terms_dscr AS CHARACTER FORMAT "X(30)" LABEL "Beginning Description" NO-UNDO.
DEFINE VARIABLE end_t-code AS CHARACTER FORMAT "X(5)" LABEL "Ending Terms" NO-UNDO.
DEFINE VARIABLE end_terms_dscr AS CHARACTER FORMAT "X(30)" LABEL "Ending Description" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY terms-query FOR EACH terms NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        terms.t-code GE begin_t-code AND
        terms.t-code LE end_t-code.
    WHEN 2 THEN
    OPEN QUERY terms-query FOR EACH terms NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        terms.dscr GE begin_terms_dscr AND
        terms.dscr LE end_terms_dscr.
  END CASE.
  GET FIRST terms-query.
  DO WHILE AVAILABLE(terms)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/terms_.i}
    DOWN.
    GET NEXT terms-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_t-code.
  IMPORT begin_terms_dscr.
  IMPORT end_t-code.
  IMPORT end_terms_dscr.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_t-code COLON 40
    begin_terms_dscr COLON 40
    end_t-code COLON 40
    end_terms_dscr COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
