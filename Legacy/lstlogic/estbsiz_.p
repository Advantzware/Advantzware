/* estbsiz_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME estbsiz_.
&Scoped-define LISTORDER Cust. #
&Scoped-define WHERE-STATEMENT cust.company = gcompany
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY eb-query FOR eb.

DEFINE VARIABLE rd-industry AS CHARACTER FORMAT "X(40)" LABEL "?" init "B" NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Customer"  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Ending Customer" init "zzzzz" NO-UNDO.
DEFINE VARIABLE begin_eb_style AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Style" NO-UNDO.
DEFINE VARIABLE end_eb_style AS CHARACTER FORMAT "X(8)" LABEL "Ending Style" init "zzzzz" NO-UNDO.
DEFINE VARIABLE begin_eb_flute AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Flute" NO-UNDO.
DEFINE VARIABLE end_eb_flute AS CHARACTER FORMAT "X(8)" LABEL "Ending Flute" init "zzzzz" NO-UNDO.
DEFINE VARIABLE begin_eb_test AS CHARACTER FORMAT "X(8)" LABEL "Beginning Test" NO-UNDO.
DEFINE VARIABLE end_eb_test AS CHARACTER FORMAT "X(8)" LABEL "Ending Test" init "zzzzz" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  {methods/lstlogic/custom/estbsiz_.i}
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT rd-industry.
  IMPORT begin_cust-no.
  IMPORT end_cust-no.
  IMPORT begin_eb_style.
  IMPORT end_eb_style.
  IMPORT begin_eb_flute.
  IMPORT end_eb_flute.
  IMPORT begin_eb_test.
  IMPORT end_eb_test.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    rd-industry COLON 40
    begin_cust-no COLON 40
    end_cust-no COLON 40
    begin_eb_style COLON 40
    end_eb_style COLON 40
    begin_eb_flute COLON 40
    end_eb_flute COLON 40
    begin_eb_test COLON 40
    end_eb_test COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
