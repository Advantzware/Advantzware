/* db_list.p */

DEFINE OUTPUT PARAMETER m_db_list AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO i = 1 TO NUM-DBS:
  m_db_list = IF m_db_list = "" THEN LDBNAME(i)
              ELSE m_db_list + "," + LDBNAME(i).
END.
ldummy = SESSION:SET-WAIT-STATE("").
