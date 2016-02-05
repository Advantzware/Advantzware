/* rec_key.i */

g_rec_key = "".
CASE prgmxref.table_name:
  /*
  WHEN "deptcode" THEN
  DO:
    FIND deptcode WHERE deptcode.rec_key = notes.rec_key NO-LOCK NO-ERROR.
    IF NOT AVAILABLE deptcode THEN
    RETURN.
    FIND compcode OF deptcode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE compcode THEN
    RETURN.
    ASSIGN
      g_rec_key = deptcode.rec_key + "," +
                  compcode.rec_key
      g_pageno = prgmxref.pageno.
  END.
  */
  OTHERWISE
  ASSIGN
    g_rec_key = notes.rec_key
    g_pageno = prgmxref.pageno.
END CASE.
