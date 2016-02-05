/* users_.p */
/* remove this comment to prevent auto creating, saving custom changes */

&Scoped-define PROGNAME users_.
&Scoped-define LISTORDER User ID's,User Name
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY users-query FOR users.

DEFINE VARIABLE begin_user_id AS CHARACTER FORMAT "X(10)" LABEL "Beginning  User ID" NO-UNDO.
DEFINE VARIABLE begin_users_user_name AS CHARACTER FORMAT "X(30)" LABEL "Beginning User Name" NO-UNDO.
DEFINE VARIABLE end_user_id AS CHARACTER FORMAT "X(10)" LABEL "Ending User ID" NO-UNDO.
DEFINE VARIABLE end_users_user_name AS CHARACTER FORMAT "X(30)" LABEL "Ending User Name" NO-UNDO.

{methods/lstlogic/lstlogic.i}

PROCEDURE List-Logic:
  CASE list-order:
    WHEN 1 THEN
    OPEN QUERY users-query FOR EACH users NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        users.user_id GE begin_user_id AND
        users.user_id LE end_user_id.
    WHEN 2 THEN
    OPEN QUERY users-query FOR EACH users NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        users.user_name GE begin_users_user_name AND
        users.user_name LE end_users_user_name.
  END CASE.
  GET FIRST users-query.
  DO WHILE AVAILABLE(users)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/users_.i}
    DOWN.
    GET NEXT users-query.
  END.
END PROCEDURE. /* List-Logic */

PROCEDURE Import-Values:
  IMPORT begin_user_id.
  IMPORT begin_users_user_name.
  IMPORT end_user_id.
  IMPORT end_users_user_name.
END PROCEDURE.

PROCEDURE Show-Selections:
  DISPLAY
    begin_user_id COLON 40
    begin_users_user_name COLON 40
    end_user_id COLON 40
    end_users_user_name COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.
