/* custom/is-wkday.p IS weekday ? */
DEF INPUT PARAM ip-check-date AS DATE NO-UNDO.
DEF OUTPUT PARAM op-is-weekday AS LOG NO-UNDO.

op-is-weekday =  WEEKDAY(ip-check-date) > 1 AND WEEKDAY(ip-check-date) < 7.
