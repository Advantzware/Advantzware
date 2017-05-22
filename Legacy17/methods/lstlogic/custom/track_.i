/* track_.i */

DEFINE VARIABLE title-line-1 AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE title-line-2 AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE detail-line-1 AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE detail-line-2 AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE i_prgmname AS CHARACTER NO-UNDO.
DEFINE VARIABLE i_user_id AS CHARACTER NO-UNDO.
DEFINE VARIABLE i_date AS DATE NO-UNDO.
DEFINE VARIABLE i_time AS INTEGER NO-UNDO.
DEFINE VARIABLE v-prgtitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-user-name AS CHARACTER NO-UNDO.

ASSIGN
  title-line-1 = IF date-time = "1" THEN "Date/Time"
                 ELSE IF program = "1" THEN "Program/Program Title"
                 ELSE IF user-id = "1" THEN "User ID/User Name"
                 ELSE ""
  title-line-2 = IF date-time = "1" THEN FILL("-",10)
                 ELSE IF program = "1" THEN FILL("-",32)
                 ELSE IF user-id = "1" THEN FILL("-",32)
                 ELSE ""
  SUBSTR(title-line-1,LENGTH(title-line-2) + 2) = 
                 IF date-time = "2" THEN "Date/Time"
                 ELSE IF program = "2" THEN "Program/Program Title"
                 ELSE IF user-id = "2" THEN "User ID/User Name"
                 ELSE ""
  SUBSTR(title-line-2,LENGTH(title-line-2) + 2) = 
                 IF date-time = "2" THEN FILL("-",10)
                 ELSE IF program = "2" THEN FILL("-",32)
                 ELSE IF user-id = "2" THEN FILL("-",32)
                 ELSE ""
  SUBSTR(title-line-1,LENGTH(title-line-2) + 2) = 
                 IF date-time = "3" THEN "Date/Time"
                 ELSE IF program = "3" THEN "Program/Program Title"
                 ELSE IF user-id = "3" THEN "User ID/User Name"
                 ELSE ""
  SUBSTR(title-line-2,LENGTH(title-line-2) + 2) = 
                 IF date-time = "3" THEN FILL("-",10)
                 ELSE IF program = "3" THEN FILL("-",32)
                 ELSE IF user-id = "3" THEN FILL("-",32)
                 ELSE "".

FIND FIRST config NO-LOCK.
IF m_purge THEN
OUTPUT STREAM s-purge TO VALUE(config.logs_dir + "/trackuse.tmp").
IF SEARCH(config.logs_dir + "/trackuse.log") NE ? THEN
DO:
  INPUT FROM VALUE(config.logs_dir + "/trackuse.log") NO-ECHO.
  REPEAT:
    IMPORT i_user_id i_prgmname i_date i_time.
    IF (m_user_id NE "" AND m_user_id NE i_user_id) OR
       (m_prgmname NE "" AND m_prgmname NE i_prgmname) OR
       (i_date LT begin_date OR i_date GT end_date) THEN
    DO:
      IF m_purge THEN
      EXPORT STREAM s-purge i_user_id i_prgmname i_date i_time.
      NEXT.
    END.
    FIND prgrms WHERE prgrms.prgmname = i_prgmname NO-LOCK NO-ERROR.
    IF AVAILABLE prgrms THEN
    v-prgtitle = prgrms.prgtitle.
    FIND users WHERE users.user_id = i_user_id NO-LOCK NO-ERROR.
    IF AVAILABLE users THEN
    v-user-name = users.user_name.
    ASSIGN
      detail-line-1 = IF date-time = "1" THEN STRING(i_date,"99/99/9999")
                      ELSE IF program = "1" THEN i_prgmname
                      ELSE IF user-id = "1" THEN i_user_id
                      ELSE ""
      detail-line-2 = IF date-time = "1" THEN "  " + STRING(i_time,"HH:MM:SS")
                      ELSE IF program = "1" THEN "  " + v-prgtitle
                      ELSE IF user-id = "1" THEN "  " + v-user-name
                      ELSE ""
      SUBSTR(detail-line-1,INDEX(title-line-2," ") + 1) =
                      IF date-time = "2" THEN STRING(i_date,"99/99/9999")
                      ELSE IF program = "2" THEN i_prgmname
                      ELSE IF user-id = "2" THEN i_user_id
                      ELSE ""
      SUBSTR(detail-line-2,INDEX(title-line-2," ") + 1) =
                      IF date-time = "2" THEN "  " + STRING(i_time,"HH:MM:SS")
                      ELSE IF program = "2" THEN "  " + v-prgtitle
                      ELSE IF user-id = "2" THEN "  " + v-user-name
                      ELSE ""
      SUBSTR(detail-line-1,R-INDEX(title-line-2," ") + 1) =
                      IF date-time = "3" THEN STRING(i_date,"99/99/9999")
                      ELSE IF program = "3" THEN i_prgmname
                      ELSE IF user-id = "3" THEN i_user_id
                      ELSE ""
      SUBSTR(detail-line-2,R-INDEX(title-line-2," ") + 1) =
                      IF date-time = "3" THEN "  " + STRING(i_time,"HH:MM:SS")
                      ELSE IF program = "3" THEN "  " + v-prgtitle
                      ELSE IF user-id = "3" THEN "  " + v-user-name
                      ELSE "".
    CREATE ttbl-detail.
    ASSIGN
      ttbl-detail.line-1 = detail-line-1
      ttbl-detail.line-2 = detail-line-2.
  END.
  INPUT CLOSE.
END.
IF m_purge THEN
DO:
  OUTPUT STREAM s-purge CLOSE.
  OS-DELETE VALUE(config.logs_dir + "/trackuse.log").
  OS-RENAME VALUE(config.logs_dir + "/trackuse.tmp")
            VALUE(config.logs_dir + "/trackuse.log").
END.
FOR EACH ttbl-detail:
  IF LINE-COUNTER = 1 THEN
  PUT UNFORMATTED title-line-1 AT 1 title-line-2 AT 1.
  PUT UNFORMATTED ttbl-detail.line-1 AT 1 ttbl-detail.line-2 AT 1.
END.
