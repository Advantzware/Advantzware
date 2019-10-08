
DEF INPUT PARAM  ip-date AS DATE NO-UNDO.
DEF OUTPUT PARAM op-date AS CHAR NO-UNDO.

DEF VAR lv-ll AS CHAR NO-UNDO.
DEF VAR lv-ds AS CHAR NO-UNDO.
DEF VAR lv-dd AS CHAR NO-UNDO.
DEF VAR lv-mm AS CHAR NO-UNDO.
DEF VAR lv-yy AS CHAR NO-UNDO.


ASSIGN
 lv-ll = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"
 lv-ds = "-"
 lv-dd = STRING(DAY(ip-date),"99")
 lv-yy = SUBSTR(STRING(YEAR(ip-date),"9999"),3,2)
 lv-mm = ENTRY(MONTH(ip-date),lv-ll)
 NO-ERROR.

IF ERROR-STATUS:ERROR THEN
  ASSIGN
   lv-mm = STRING(MONTH(ip-date),"99")
   lv-ds = "/".

op-date = TRIM(lv-dd) + lv-ds +
          TRIM(lv-mm) + lv-ds +
          TRIM(lv-yy).

