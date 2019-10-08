/* bank_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME bank_.
&Scoped-define LISTORDER Bank,Bank Name
&Scoped-define WHERE-STATEMENT bank.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY bank-query FOR bank.

DEFINE VARIABLE begin_bank-code AS CHARACTER FORMAT "X(10)" LABEL "Beginning Bank" NO-UNDO.
DEFINE VARIABLE begin_bank_bank-name AS CHARACTER FORMAT "X(30)" LABEL "Beginning Bank Name" NO-UNDO.
DEFINE VARIABLE end_bank-code AS CHARACTER FORMAT "X(10)" LABEL "Ending Bank" NO-UNDO.
DEFINE VARIABLE end_bank_bank-name AS CHARACTER FORMAT "X(30)" LABEL "Ending Bank Name" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY bank-query FOR EACH bank NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        bank.bank-code GE begin_bank-code AND
        bank.bank-code LE end_bank-code.
    WHEN 2 THEN
    OPEN QUERY bank-query FOR EACH bank NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        bank.bank-name GE begin_bank_bank-name AND
        bank.bank-name LE end_bank_bank-name.
  END CASE.
  GET FIRST bank-query.
  DO WHILE AVAILABLE(bank)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/bank_.i}
    DOWN.
    GET NEXT bank-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_bank-code.
  IMPORT begin_bank_bank-name.
  IMPORT end_bank-code.
  IMPORT end_bank_bank-name.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_bank-code COLON 40
    begin_bank_bank-name COLON 40
    end_bank-code COLON 40
    end_bank_bank-name COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
