/* showmisc.i */

/*
&db_table = DATABASE.table-name
&col = column where misc fields begin
&frame-name = frame for misc fields

NOTE: this include uses the logical variable 'show-misc-fields'
*/

IF show-misc-fields THEN
DO:
  IF NOT AVAILABLE mfdata THEN
  FIND FIRST mfdata NO-LOCK NO-ERROR.
  FOR EACH mfvalues WHERE mfvalues.rec_key = {&db_table}.rec_key NO-LOCK:
    misc-label = "".
    DO cnt = 1 TO NUM-ENTRIES(mfdata.miscflds_data,";") - 1:
      IF ENTRY(2,ENTRY(cnt,mfdata.miscflds_data,";"),"~"") NE mfvalues.mf_id THEN
      NEXT.
      misc-label = ENTRY(8,ENTRY(cnt,mfdata.miscflds_data,";"),"~"") + " ".
      LEAVE.
    END.
    misc-label = misc-label + "(" +  mfvalues.mf_id + "): " + mfvalues.mf_value.
    PUT UNFORMATTED misc-label AT {&col}.
  END.
END.
