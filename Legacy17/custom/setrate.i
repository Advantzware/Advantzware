/* custom/setrate.i  get table's record for ex-rate */

FIND {1} WHERE RECID({1}) = {2} NO-ERROR.
IF AVAIL {1} THEN ASSIGN {1}.ex-rate = lv-exrate.
 
