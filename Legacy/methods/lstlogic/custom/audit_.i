/* audit_.i */

FIND FIRST config NO-LOCK.
IF CAN-FIND(FIRST audit) THEN
FOR EACH audit EXCLUSIVE:
  OUTPUT STREAM s-audit TO VALUE(config.audit_dir + "~/" + audit.audit_key) APPEND.
  PUT STREAM s-audit UNFORMATTED audit.audit_data SKIP.
  OUTPUT STREAM s-audit CLOSE.
  DELETE audit.
END.

RUN Get_Procedure IN Persistent-Handle ("audit.",OUTPUT run-proc,no).
IF run-proc NE "" THEN
RUN VALUE(run-proc)
  (m_action_table,
   m_user_id,
   m_db_table,
   m_field_changed,
   m_show_index,
   m_purge,
   begin_date,
   end_date).

DEFINE VARIABLE logline AS CHARACTER NO-UNDO.

INPUT FROM VALUE(config.logs_dir + "~/audit.log") NO-ECHO.
REPEAT:
  IMPORT UNFORMATTED logline.
  IF ASC(SUBSTR(logline,1,1)) = 12 THEN
  DO:
    PAGE.
    logline = SUBSTR(logline,2).
  END.
  IF logline NE "" THEN
  PUT UNFORMATTED logline SKIP.
  ELSE
  PUT UNFORMATTED SKIP(1).
  logline = "".
END.
INPUT CLOSE.
