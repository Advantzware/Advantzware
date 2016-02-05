/* audit.p */

DEFINE STREAM s_audit_dir.

DEFINE VARIABLE v_audit_file AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE v_attr_list AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

DEFINE WORK-TABLE ttbl_files NO-UNDO
  FIELD audit-file-name AS CHARACTER.

{methods/defines/audit.i &NEW="NEW"}

FIND FIRST config NO-LOCK.
CREATE ttbl_header.
INPUT STREAM s_audit_dir FROM OS-DIR(config.audit_dir) NO-ECHO.
REPEAT:
  SET STREAM s_audit_dir v_audit_file ^ v_attr_list.
  IF v_attr_list NE "F" THEN
  NEXT.
  IF SUBSTR(v_audit_file,1,8) LT
     STRING(YEAR(v_begin_date),"9999") +
     STRING(MONTH(v_begin_date),"99") +
     STRING(DAY(v_begin_date),"99") OR
     SUBSTR(v_audit_file,1,8) GT
     STRING(YEAR(v_end_date),"9999") +
     STRING(MONTH(v_end_date),"99") +
     STRING(DAY(v_end_date),"99") THEN
  NEXT.
  CREATE ttbl_files.
  ttbl_files.audit-file-name = v_audit_file.
END.
OUTPUT STREAM s-audit-rpt TO VALUE(config.logs_dir + "/audit.log").
FOR EACH ttbl_files BY ttbl_files.audit-file-name:
  INPUT STREAM s_audit_file FROM
      VALUE(config.audit_dir + "/" + ttbl_files.audit-file-name).
  REPEAT:
    IMPORT STREAM s_audit_file ttbl_header.
    IF CAN-DO(v_action_taken,ttbl_header.v_action) AND
    (ttbl_header.v_userid = v_user_id OR v_user_id = "") AND
    (ttbl_header.v_db = v_db_table OR v_db_table = "") THEN
    CASE ttbl_header.v_file:
      {custom/audit.i}
    END CASE.
    ELSE
    IMPORT STREAM s_audit_file ^.
  END.
  INPUT STREAM s_audit_file CLOSE.
  IF v_purge THEN
  OS-DELETE SILENT
      VALUE(config.audit_dir + "/" + ttbl_files.audit-file-name).
END.
OUTPUT STREAM s-audit-rpt CLOSE.
INPUT STREAM s_audit_dir CLOSE.
ldummy = SESSION:SET-WAIT-STATE("").
