/* audittrg.i */

&SCOPED-DEFINE UNIX UNIX

DEFINE VARIABLE auditname AS CHARACTER NO-UNDO.
DEFINE VARIABLE auditout AS CHARACTER NO-UNDO.
DEFINE VARIABLE auditdata AS CHARACTER NO-UNDO.

IF CAN-FIND(FIRST ASI.config WHERE CAN-DO(ASI.config.audit_tables,"{&TABLENAME}")) THEN DO:
  FIND FIRST ASI.config NO-LOCK NO-ERROR.
  ASSIGN
    auditname = STRING(YEAR(TODAY),"9999") +
               STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + "." +
               STRING(TIME,"99999")
    auditout = 
&IF "{&OPSYS}" = "{&UNIX}" &THEN
               (IF ASI.config.audit_dir_unix = "" THEN "."
                ELSE ASI.config.audit_dir_unix)
&ELSE
               ASI.config.audit_dir
&ENDIF
               + "~/" + auditname.
  OUTPUT TO VALUE(auditout) APPEND.
  &IF "{&ACTION}" = "UPDATE" &THEN
  IF old-{&TABLENAME}.rec_key NE "" THEN DO:
    EXPORT "{&ACTION}" {&DBNAME} "{&TABLENAME}" USERID("ASI") TODAY TIME.
    EXPORT old-{&TABLENAME}.
  END.
  ELSE
  EXPORT "CREATE" {&DBNAME} "{&TABLENAME}" USERID("ASI") TODAY TIME.
  &ELSE
  EXPORT "{&ACTION}" {&DBNAME} "{&TABLENAME}" USERID("ASI") TODAY TIME.
  &ENDIF
  EXPORT {&TABLENAME}.
  OUTPUT CLOSE.
&IF "{&OPSYS}" = "{&UNIX}" &THEN
  IF auditout BEGINS "." THEN DO:
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
