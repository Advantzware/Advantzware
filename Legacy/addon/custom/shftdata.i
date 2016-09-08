 /* shftdata.i */

FIND FIRST {&file} NO-LOCK
     WHERE {&where} {&file}.shift EQ ip-shift
     NO-ERROR.
IF AVAILABLE {&file} THEN DO:
  ASSIGN
    op-starttime = {&file}.start_time
    op-endtime = {&file}.end_time.
  RETURN.
END.
