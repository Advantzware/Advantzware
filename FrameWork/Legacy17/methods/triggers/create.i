/* create.i */

{custom/rec_key.i {&TABLENAME}}

&IF '{&TABLENAME}' EQ 'job-mch' &THEN
/* override schema setting these dates to TODAY - res - 10.17.2005 */
ASSIGN
  job-mch.start-date-su = ?
  job-mch.end-date-su = ?
  job-mch.start-date = ?
  job-mch.end-date = ?.
&ENDIF
