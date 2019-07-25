/* emprpt1_.i 
   modified   02/26/01  YSK  sort by start_date & start_time, display job#   
*/

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'emprpt1_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="employee" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="employee" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="employee" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="employee" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DEFINE VARIABLE line-cnt AS INTEGER FORMAT '>9' LABEL 'Ln' NO-UNDO.
DEFINE VARIABLE total-time AS CHARACTER FORMAT 'X(5)' LABEL 'Time' NO-UNDO.
DEFINE VARIABLE grand-time AS DECIMAL FORMAT '>>>.99' LABEL 'Total' NO-UNDO.
DEFINE VARIABLE grand-total-time AS DECIMAL NO-UNDO.
DEFINE VARIABLE emp-time AS INTEGER NO-UNDO.
DEFINE VARIABLE op-base LIKE rate.rate NO-UNDO.
DEFINE VARIABLE op-rate LIKE rate.rate NO-UNDO.

FOR EACH employee WHERE
    employee.company = cocode AND
    employee.employee GE begin_employee AND
    employee.employee LE end_employee NO-LOCK,
    EACH machemp NO-LOCK
    WHERE machemp.employee EQ employee.employee
      AND machemp.start_date GE begin-date
      AND machemp.start_date LE end-date,
    FIRST machtran WHERE machtran.rec_key = machemp.table_rec_key
                   AND machtran.job_number GE begin_job-no
                   AND machtran.job_number LE end_job-no
                   AND machtran.job_sub GE int(begin_job-no2)
                   AND machtran.job_sub LE int(end_job-no2)
     NO-LOCK
    BREAK BY machemp.employee by machemp.start_date by machemp.start_time
    WITH FRAME daysum STREAM-IO NO-BOX WIDTH 150 :

  IF FIRST-OF(machemp.employee) THEN
  DO:
    DISPLAY
      machemp.employee label "Emp!ID"
      employee.first_name + ' ' + employee.last_name LABEL 'Name' FORMAT 'X(30)'.
    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED 
       machemp.employee ","
       employee.first_name ' ' employee.last_name ",".
    ASSIGN
      emp-time = 0
      line-cnt = 0.
  END.
  ELSE IF tb_excel THEN PUT STREAM excel UNFORMATTED ",,". /* to get empty columns for id/name */
  ASSIGN
    emp-time = emp-time + machemp.total_time
    line-cnt = line-cnt + 1.
  FIND empmach OF employee WHERE empmach.machine = machtran.machine NO-LOCK NO-ERROR.
  RUN Employee-Rate IN Persistent-Handle
     (employee.company,employee.employee,machemp.shift,
      machtran.machine,machemp.rate_usage,'Standard',OUTPUT op-rate).
  DISPLAY
    line-cnt label "Cnt" form ">>9"
    machtran.machine  form "x(10)"
    machtran.job_number  /* ysk 02/26/01*/
    empmach.gl_account WHEN AVAILABLE(empmach)  column-label "G/L!Account" form "x(10)"
    machemp.start_date   label "Started!Date" /* ysk */
    STRING(machemp.start_time,'HH:MM am') column-LABEL 'Started!Time'
    machemp.end_date /* ysk */  column-label "End!Date"
    STRING(machemp.end_time,'HH:MM am') column-LABEL 'Ended!Time'
    machemp.shift LABEL 'S'  /*Shift*/
    op-rate column-LABEL 'Base!Rate'
    machemp.rate
    total-time
    STRING(machemp.total_time,'HH:MM') @ total-time
    grand-time
   (machemp.total_time / 3600) FORMAT '->>>.99' @ grand-time
   .
   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED 
          string(line-cnt,">>9") ","
          machtran.machine ","
          machtran.job_number  ","
          IF AVAILABLE empmach THEN empmach.gl_account ELSE ""  ","
          machemp.start_date  ","
          trim(STRING(machemp.start_time,'HH:MM am'))  ","
          machemp.end_date  ","
          trim(STRING(machemp.end_time,'HH:MM am'))  ","
          machemp.shift  ","
          op-rate  ","
          machemp.rate  ","
          trim(STRING(machemp.total_time,'HH:MM'))  ","
          STRING(machemp.total_time / 3600,'->>>.99')  ","
          SKIP.
  IF LAST-OF(machemp.employee) THEN
  DO:
    DOWN.
    DISPLAY STRING(emp-time,'HH:MM') @ total-time
      emp-time / 3600 @ grand-time.
    IF tb_excel THEN PUT STREAM excel ",,,,,,,,,,,,," /* to place under total time */
       trim(STRING(emp-time,'HH:MM')) "," string(emp-time / 3600,"->>>.99") SKIP.
    PUT UNFORMATTED SKIP FILL('-',132) SKIP.
    grand-total-time = grand-total-time + emp-time.
    IF tb_excel THEN PUT STREAM excel UNFORMATTED SKIP FILL('-',132) SKIP.
  END.
  IF LAST(machemp.employee) THEN DO:
      PUT UNFORMATTED SKIP FILL('=',132) SKIP
          'Grand Total: ' AT 123 STRING(grand-total-time / 3600,'>>,>>>.99') TO 146.
      IF tb_excel THEN PUT STREAM excel UNFORMATTED SKIP FILL('=',132) SKIP
         FILL(",",14) 'Grand Total:,' STRING(grand-total-time / 3600,'>>>>>.99') SKIP.
  END.
END. /* each machemp */
