/* machtra_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'machtra_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="machtran" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="machtran" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="machtran" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="machtran" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

IF machtran.company = selected-company THEN
DO:
  DISPLAY
    machtran.machine
    machtran.job_number
    machtran.job_sub
    machtran.form_number
    machtran.blank_number
    machtran.pass_sequence
    machtran.charge_code
    machtran.start_date
    STRING(machtran.start_time,'HH:MM am') LABEL 'Log In'
    machtran.end_date
    STRING(machtran.end_time,'HH:MM am') LABEL 'Log Out'
    machtran.shift
    STRING(machtran.total_time,'HH:MM') LABEL 'Total'
    machtran.run_qty
    machtran.waste_qty.
  {methods/lstlogic/shownote.i &db_table="machtran" &col="5" &frame-name="f-notes"}
  {methods/lstlogic/showmisc.i &db_table="machtran" &col="5" &frame-name="f-miscflds"}
  IF show-employees THEN
  FOR EACH machemp NO-LOCK WHERE machemp.table_rec_key = machtran.rec_key
      WITH FRAME machemp STREAM-IO NO-BOX COLUMN 9 WIDTH 132:
    FIND employee WHERE employee.company = machtran.company
                    AND employee.employee = machemp.employee NO-LOCK NO-ERROR.
    DISPLAY
      machemp.employee
      employee.first_name + ' ' + employee.last_name FORMAT 'X(30)' LABEL 'Name'
      machemp.start_date
      STRING(machemp.start_time,'HH:MM am') LABEL 'Started'
      machemp.end_date
      STRING(machemp.end_time,'HH:MM am') LABEL 'Ended'
      STRING(machemp.total_time,'HH:MM') LABEL 'Total'
      machemp.shift
      machemp.rate_usage LABEL 'Usage'
      machemp.ratetype
      machemp.rate.
  END.
END.

