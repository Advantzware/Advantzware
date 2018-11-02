/* rec_key.i */

def var g_pageno as int no-undo.
def var g_rec_key as char no-undo.
find first note no-lock.
find first elements
   where applctns = "cq" and
         recordid = "q1" no-lock.
find prgmxref where table_name = "elements" no-lock.

PROCEDURE Set-g_rec_key:
  ASSIGN
    g_rec_key = ""
    p_pageno = 0.
  CASE prgmxref.table_name:
    WHEN "deptcode" THEN
    DO:
      FIND deptcode WHERE deptcode.rec_key = note.rec_key NO-LOCK.
      IF NOT AVAILABLE deptcode THEN
      RETURN.
      FIND compcode OF deptcode NO-LOCK.
      IF NOT AVAILABLE compcode THEN
      RETURN.
      g_rec_key = deptcode.rec_key + "," + compcode.rec_key.
    END.
    WHEN "deptoga" THEN
    DO:
      FIND deptogaWHERE deptoga.rec_key = note.rec_key NO-LOCK.
      IF NOT AVAILABLE deptoga THEN
      RETURN.
      FIND deptcode OF deptoga NO-LOCK.
      IF NOT AVAILABLE deptcode THEN
      RETURN.
      FIND compcode OF deptcode NO-LOCK.
      IF NOT AVAILABLE compcode THEN
      RETURN.
      g_rec_key = deptoga.rec_key + "," +
                  deptcode.rec_key + "," +
                  compcode.rec_key.
    END.
    WHEN "elements" THEN
    DO:
      FIND elements WHERE elements.rec_key = note.rec_key NO-LOCK NO-ERROR.
      IF NOT AVAILABLE elements THEN
      RETURN.
      FIND recordid OF elements NO-LOCK NO-ERROR.
      IF NOT AVAILABLE recordid THEN
      RETURN.
      FIND applctns OF recordid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE applctns THEN
      RETURN.
      g_rec_key = elements.rec_key + "," +
                  recordid.rec_key + "," +
                  applctns.rec_key.
    END.
    WHEN "recordid" THEN
    DO:
      FIND recordid WHERE recordid.rec_key = note.rec_key NO-LOCK NO-ERROR.
      IF NOT AVAILABLE recordid THEN
      RETURN.
      FIND applctns OF recordid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE applctns THEN
      RETURN.
      g_rec_key = elements.rec_key + "," + recordid.rec_key.
    END.
    OTHERWISE
    ASSIGN
      g_rec_key = note.rec_key
      p_pageno = prgmxref.pageno.
  END CASE.
END PROCEDURE.
