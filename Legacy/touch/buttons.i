/* buttons.i */

&IF '{&BUTTON-INCLUDE}' = 'BLANKS' &THEN
ASSIGN
  idummy = INDEX(SELF:LABEL,':')
  blank_number = SUBSTR(SELF:LABEL,idummy + 2)
  item_number = ''.
IF INDEX(blank_number,'(') NE 0 THEN
ASSIGN
  idummy = INDEX(blank_number,'(')
  blank_number = SUBSTR(blank_number,1,idummy - 2)
  idummy = INDEX(SELF:LABEL,'(')
  item_number = REPLACE(SUBSTR(SELF:LABEL,idummy + 1),')','').
{methods/run_link.i "CONTAINER" "Set_Value" "('blank_number',blank_number)"}
{methods/run_link.i "CONTAINER" "Set_Value" "('item_number',item_number)"}
RUN valid-blank NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
{methods/run_link.i "CONTAINER" "Change_Page" "(12)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'EMPLOYEES' &THEN
ASSIGN
  idummy = INDEX(SELF:LABEL,'(')
  employee_code = REPLACE(SUBSTR(SELF:LABEL,idummy + 1),')','')
  employee_name = SUBSTR(SELF:LABEL,1,idummy - 2).
{methods/run_link.i "CONTAINER" "Set_Value" "('employee_code',employee_code)"}
{methods/run_link.i "CONTAINER" "Set_Value" "('employee_name',employee_name)"}
{methods/run_link.i "CONTAINER" "Change_Page" "(4)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'FORMS' &THEN
ASSIGN
  idummy = INDEX(SELF:LABEL,':')
  form_number = SUBSTR(SELF:LABEL,idummy + 2).
{methods/run_link.i "CONTAINER" "Set_Value" "('form_number',form_number)"}
{methods/run_link.i "CONTAINER" "Change_Page" "(11)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'JOBS' &THEN
job# = SELF:LABEL.
{methods/run_link.i "CONTAINER" "Set_Value" "('job#',job#)"}
RUN check-job-status NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
{methods/run_link.i "CONTAINER" "Change_Page" "(10)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'JOBSEQ' &THEN
ASSIGN
  job_sequence = SELF:LABEL
  idummy = R-INDEX(SELF:LABEL,'(')
  charge_code = SUBSTR(SELF:LABEL,idummy + 1)
  idummy = R-INDEX(charge_code,')')
  charge_code = SUBSTR(charge_code,1,idummy - 1).
{methods/run_link.i "CONTAINER" "Set_Value" "('job_sequence',job_sequence)"}
{methods/run_link.i "CONTAINER" "Set_Value" "('charge_code',charge_code)"}
{methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(machtran-rowid)"}
{methods/run_link.i "CONTAINER" "Change_Page" "(14)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'MACHINES' &THEN
ASSIGN
  idummy = INDEX(SELF:LABEL,'(')
  machine_code = SUBSTR(SELF:LABEL,1,idummy - 2).
{methods/run_link.i "CONTAINER" "Set_Value" "('machine_code',machine_code)"}
{methods/run_link.i "CONTAINER" "Change_Page" "(6)"}

&ELSEIF '{&BUTTON-INCLUDE}' EQ 'PASS' &THEN
ASSIGN
  idummy = INDEX(SELF:LABEL,':')
  pass_sequence = SUBSTR(SELF:LABEL,idummy + 2).
{methods/run_link.i "CONTAINER" "Set_Value" "('pass_sequence',pass_sequence)"}
{methods/run_link.i "CONTAINER" "Change_Page" "(13)"}

&ENDIF
