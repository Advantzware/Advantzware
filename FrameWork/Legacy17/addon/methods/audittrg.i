/* audittrg.i */

&Scoped-define UNIX UNIX

DEFINE VARIABLE auditname AS CHARACTER NO-UNDO.
DEFINE VARIABLE auditout AS CHARACTER NO-UNDO.
DEFINE VARIABLE auditdata AS CHARACTER NO-UNDO.
     
IF CAN-FIND(FIRST config WHERE CAN-DO(config.audit_tables,"{&TABLENAME}")) THEN
DO:
  FIND FIRST config NO-LOCK NO-ERROR.
  ASSIGN
    auditname = STRING(YEAR(TODAY),"9999") +
               STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + "." +
               STRING(TIME,"99999")
    auditout = 
&IF "{&OPSYS}" = "{&UNIX}" &THEN
               (IF config.audit_dir_unix = "" THEN "."
                ELSE config.audit_dir_unix)
&ELSE
               config.audit_dir
&ENDIF
               + "~/" + auditname.
  OUTPUT TO VALUE(auditout).
  &IF "{&ACTION}" = "UPDATE" &THEN
  IF old-{&TABLENAME}.rec_key NE "" THEN
  DO:
    EXPORT "{&ACTION}" {&DBNAME} "{&TABLENAME}" USERID("NOSWEAT") TODAY TIME.
    EXPORT old-{&TABLENAME}.
  END.
  ELSE
    EXPORT "CREATE" {&DBNAME} "{&TABLENAME}" USERID("NOSWEAT") TODAY TIME.
  &ELSE
    EXPORT "{&ACTION}" {&DBNAME} "{&TABLENAME}" USERID("NOSWEAT") TODAY TIME.
  &ENDIF
  EXPORT {&TABLENAME}.
  OUTPUT CLOSE.
&IF "{&OPSYS}" = "{&UNIX}" &THEN
  IF auditout BEGINS "." THEN
  DO:
    INPUT FROM VALUE(auditout) NO-ECHO.
    REPEAT:
      IMPORT UNFORMATTED auditdata.
      CREATE audit.
      ASSIGN
        audit.audit_key = auditname
        audit.audit_data = auditdata.
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(auditout).
  END.
&ENDIF
END.
