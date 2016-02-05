/* employe_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'employe_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="employee" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="employee" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="employee" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="employee" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  employee.employee
  employee.last_name LABEL 'Employee Name' FORMAT 'X(25)'
  employee.last_name + ', ' + employee.first_name + ' ' + employee.middle_name @
  employee.last_name
  employee.soc_sec
  employee.emp_type LABEL 'Type'
  employee.actnum
  employee.start_date
  employee.ref_no
  employee.rate_usage.
IF show-rates THEN
FOR EACH rate OF employee NO-LOCK WITH STREAM-IO TITLE '---- Rates ----' COL 5:
  DISPLAY
    rate.ratetype
    rate.shift
    rate.machine
    rate.rate_usage
    rate.rate
    rate.factortype.
END.
IF show-login-logout THEN
FOR EACH emplogin OF employee NO-LOCK WITH STREAM-IO TITLE '---- Login/Logout ----' COL 5:
  DISPLAY
    emplogin.start_date
    STRING(emplogin.start_time,'HH:MM am') LABEL 'Login'
    emplogin.machine
    emplogin.end_date
    emplogin.end_time LABEL 'Logout'
    STRING(emplogin.end_time,'HH:MM am') WHEN emplogin.end_time NE 0 @ emplogin.end_time
    '' WHEN emplogin.end_time = 0 @ emplogin.end_time
    emplogin.shift
    STRING(emplogin.total_time,'HH:MM') LABEL 'Total'.
END.
IF show-machines THEN 
FOR EACH empmach OF employee NO-LOCK WITH STREAM-IO TITLE '---- Assigned Machines ----' COL 5:
  FIND mach WHERE mach.company = empmach.company
              AND mach.m-code = empmach.machine
            NO-LOCK NO-ERROR.
  DISPLAY
    empmach.machine
    mach.m-dscr WHEN AVAILABLE(mach)
    empmach.gl_account.
END.
IF show-emp-notes THEN do :    
FOR EACH note where note.rec_key = employee.rec_key  NO-LOCK WITH down STREAM-IO TITLE '---- Notes ----' COL 5
          by note.note_date:
  DISPLAY "Employee   " label "Note Type"
    note.note_date 
    note.note_title with width 132.
END.
for each emplogin where emplogin.employee = employee.employee no-lock:
    for each note where note.rec_key = emplogin.rec_key /*and
                            note.note_date = machemp.start_date */
                       no-lock           by note.note_date :
        DISPLAY   "Login      " at 5
                  note.note_date
                  note.note_title with stream-io width 132 no-box no-label.
      end.
end.    
for each machemp where machemp.employee = employee.employee no-lock:
    for each note where note.rec_key = machemp.rec_key /*and
                          note.note_date = machemp.start_date */
                      no-lock           by note.note_date :
        DISPLAY  "Transaction" at 5 
                  note.note_date
                 note.note_title with stream-io width 132 no-box no-labels .
    end.
end.    
      
end.  /* show-emp-notes */
put skip(1).

{methods/lstlogic/shownote.i &db_table="employee" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="employee" &col="5" &frame-name="f-miscflds"}
{methods/lstlogic/showaddr.i &db_table="employee" &col="5" &frame-name="f-addresses"}
{methods/lstlogic/showphon.i &db_table="employee" &col="5" &frame-name="f-phones"}
