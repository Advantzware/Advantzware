/* audit_.p */

&Scoped-define PROGNAME audit_.
&Scoped-define LISTORDER 
&Scoped-define SHOWNOTES no
&Scoped-define SHOWMISCFLDS no
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE VARIABLE m_action_table AS CHARACTER FORMAT "X(40)" LABEL "Audit Action" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" LABEL "Beginning Date" NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" LABEL "Ending Date" NO-UNDO.
DEFINE VARIABLE m_user_id AS CHARACTER FORMAT "X(12)" LABEL "User ID" NO-UNDO.
DEFINE VARIABLE m_db_table AS CHARACTER FORMAT "X(12)" LABEL "DB.Table Name" NO-UNDO.
DEFINE VARIABLE m_field_changed AS LOGICAL FORMAT "yes/no" LABEL "Show Only Fields with Changed Values" NO-UNDO.
DEFINE VARIABLE m_show_index AS LOGICAL FORMAT "yes/no" LABEL "Show Index Fields and Values" NO-UNDO.
DEFINE VARIABLE m_purge AS LOGICAL FORMAT "yes/no" LABEL "Purge Audit Transactions by Date Range" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  {methods/lstlogic/custom/audit_.i}
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT m_action_table.
  IMPORT begin_date.
  IMPORT end_date.
  IMPORT m_user_id.
  IMPORT m_db_table.
  IMPORT m_field_changed.
  IMPORT m_show_index.
  IMPORT m_purge.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    m_action_table COLON 40
    begin_date COLON 40
    end_date COLON 40
    m_user_id COLON 40
    m_db_table COLON 40
    m_field_changed COLON 40
    m_show_index COLON 40
    m_purge COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
