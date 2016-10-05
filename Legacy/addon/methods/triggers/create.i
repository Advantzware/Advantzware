/* create.i */

{&TABLENAME}.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999").

CREATE rec_key.
ASSIGN
  rec_key.rec_key = {&TABLENAME}.rec_key
  rec_key.table_name = "{&TABLENAME}".

&IF '{&TABLENAME}' EQ 'job-mch' &THEN
/* override schema setting these dates to TODAY - res - 10.17.2005 */
ASSIGN
  job-mch.start-date-su = ?
  job-mch.end-date-su = ?
  job-mch.start-date = ?
  job-mch.end-date = ?.
&ENDIF
