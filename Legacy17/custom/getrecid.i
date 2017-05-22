/* custom/getrecid.i  get table's record for ex-rate */

FIND {1} WHERE RECID({1}) = {2} NO-LOCK NO-ERROR.
IF AVAIL {1} THEN ASSIGN lv-curr-code = {1}.curr-code[1]
                         lv-exrate = {1}.ex-rate.
 
