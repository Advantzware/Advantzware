/* parmfil_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME parmfil_.
&Scoped-define LISTORDER Parameter File
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY parmfile-query FOR parmfile.

DEFINE VARIABLE begin_parmfile AS CHARACTER FORMAT "X(40)" LABEL "Beginning  Parameter File" NO-UNDO.
DEFINE VARIABLE end_parmfile AS CHARACTER FORMAT "X(40)" LABEL "Ending Parameter File" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY parmfile-query FOR EACH parmfile NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        parmfile.parmfile GE begin_parmfile AND
        parmfile.parmfile LE end_parmfile.
  END CASE.
  GET FIRST parmfile-query.
  DO WHILE AVAILABLE(parmfile)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/parmfil_.i}
    DOWN.
    GET NEXT parmfile-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_parmfile.
  IMPORT end_parmfile.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_parmfile COLON 40
    end_parmfile COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
