/* shftdata.i */

FIND {&file} WHERE {&where} {&file}.shift = ip-shift NO-LOCK NO-ERROR.
IF AVAILABLE {&file} THEN
DO:
  ASSIGN
    op-starttime = {&file}.start_time
    op-endtime = {&file}.end_time.
  RETURN.
END.
