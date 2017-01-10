/* usergrp_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME usergrp_.
&Scoped-define LISTORDER User Group
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY usergrps-query FOR usergrps.

DEFINE VARIABLE begin_usergrps AS CHARACTER FORMAT "X(10)" LABEL "Beginning  User Groups" NO-UNDO.
DEFINE VARIABLE end_usergrps AS CHARACTER FORMAT "X(10)" LABEL "Ending User Groups" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY usergrps-query FOR EACH usergrps NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        usergrps.usergrps GE begin_usergrps AND
        usergrps.usergrps LE end_usergrps.
  END CASE.
  GET FIRST usergrps-query.
  DO WHILE AVAILABLE(usergrps)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 320 STREAM-IO DOWN:
    {methods/lstlogic/custom/usergrp_.i}
    DOWN.
    GET NEXT usergrps-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_usergrps.
  IMPORT end_usergrps.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_usergrps COLON 40
    end_usergrps COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 182 STREAM-IO.
END PROCEDURE.
