/* track_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME track_.
&Scoped-define LISTORDER 
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE VARIABLE m_prgmname AS CHARACTER FORMAT "X(12)" LABEL "Program Name" NO-UNDO.
DEFINE VARIABLE m_user_id AS CHARACTER FORMAT "X(12)" LABEL "User ID" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" LABEL "Beginning Date" NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" LABEL "Ending Date" NO-UNDO.
DEFINE VARIABLE date-time AS CHARACTER FORMAT "X(40)" LABEL "Date and Time" NO-UNDO.
DEFINE VARIABLE program AS CHARACTER FORMAT "X(40)" LABEL "Program" NO-UNDO.
DEFINE VARIABLE user-id AS CHARACTER FORMAT "X(40)" LABEL "User" NO-UNDO.
DEFINE VARIABLE m_purge AS LOGICAL FORMAT "yes/no" LABEL "Purge Data" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  {methods/lstlogic/custom/track_.i}
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT m_prgmname.
  IMPORT m_user_id.
  IMPORT begin_date.
  IMPORT end_date.
  IMPORT date-time.
  IMPORT program.
  IMPORT user-id.
  IMPORT m_purge.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    m_prgmname COLON 40
    m_user_id COLON 40
    begin_date COLON 40
    end_date COLON 40
    date-time COLON 40
    program COLON 40
    user-id COLON 40
    m_purge COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
