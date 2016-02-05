/* postdisp.i */

&IF '{&DISPLAY}' = 'YES' &THEN
  &IF '{1}' = '1' &THEN
    OUTPUT TO 'touch/post-out.i'.
    DISP cmpltjob.company cmpltjob.machine cmpltjob.job_number cmpltjob.job_sub
        WITH STREAM-IO WIDTH 132 COL 25.
  &ELSEIF '{1}' = '2' &THEN
      DISPLAY
        machtran.charge_code
        machtran.form_number
        machtran.blank_number
        machtran.pass_sequence
        machtran.start_date
        STRING(machtran.start_time,'HH:MM am') LABEL 'Started'
        machtran.end_date
        STRING(machtran.end_time,'HH:MM am') LABEL 'Ended'
        STRING(machtran.total_time,'HH:MM') LABEL 'Total'
        machtran.run_qty
        machtran.waste_qty
        machshifts
        realshifts
      WITH STREAM-IO WIDTH 132.
  &ELSEIF '{1}' = '3' &THEN
        DISPLAY
          ENTRY(i,machshifts) LABEL 'Shift' FORMAT 'XX'
          STRING(machtran.start_time,'HH:MM am') LABEL 'Started'
          STRING(machtran.end_time,'HH:MM am') LABEL 'Ended'
          machtran.total_time
          STRING(machstarttime,'HH:MM am') LABEL 'MachStart'
          STRING(machendtime,'HH:MM am') LABEL 'MachEnd'
          machtotaltime
          shiftpct
        WITH STREAM-IO WIDTH 132 COL 5 FRAME F1 DOWN.
        DOWN WITH FRAME F1.
  &ELSEIF '{1}' = '4' &THEN
          DISPLAY
            emplogin.employee
            emplogin.start_date
            STRING(emplogin.start_time,'HH:MM am') LABEL 'Started'
            emplogin.end_date
            STRING(emplogin.end_time,'HH:MM am') LABEL 'Ended'
            STRING(emplogin.total_time,'HH:MM') LABEL 'Total'
            empshifts
            realshifts
            employee.rate_usage
          WITH STREAM-IO WIDTH 132 COL 5.
  &ELSEIF '{1}' = '5' &THEN
            DISPLAY
              rate.employee
              rate.shift
              rate.machine
              STRING(starttime,'HH:MM am') LABEL 'Start'
              STRING(endtime,'HH:MM am') LABEL 'End'
              totaltime
              STRING(empstarttime,'HH:MM am') LABEL 'EmpStart'
              STRING(empendtime,'HH:MM am') LABEL 'EmpEnd'
              emptotaltime
              rate.rate
              rate.factortype
            WITH STREAM-IO WIDTH 132 COL 10.
  &ELSEIF '{1}' = '6' &THEN
FOR EACH ttbl_pc-prdh NO-LOCK WITH STREAM-IO:
  DISPLAY postok.
  DISPLAY ttbl_pc-prdh.
  FOR EACH ttbl_pc-prdd NO-LOCK
      WHERE ttbl_pc-prdd.company = ttbl_pc-prdh.company
        AND ttbl_pc-prdd.m-code = ttbl_pc-prdh.m-code
        AND ttbl_pc-prdd.op-date = ttbl_pc-prdh.trans-date
        AND ttbl_pc-prdd.shift = ttbl_pc-prdh.shift
      WITH STREAM-IO WIDTH 132 COL 5 3 COLUMN:
    DISPLAY
      ttbl_pc-prdd.blank-no
      ttbl_pc-prdd.code
      ttbl_pc-prdd.company
      ttbl_pc-prdd.complete
      ttbl_pc-prdd.crew
      ttbl_pc-prdd.dept
      ttbl_pc-prdd.frm
      ttbl_pc-prdd.hours
      ttbl_pc-prdd.i-name
      ttbl_pc-prdd.i-no
      ttbl_pc-prdd.j-no
      ttbl_pc-prdd.job
      ttbl_pc-prdd.job-no
      ttbl_pc-prdd.job-no2
      ttbl_pc-prdd.m-code
      ttbl_pc-prdd.op-date
      ttbl_pc-prdd.pass
      ttbl_pc-prdd.qty
      ttbl_pc-prdd.shift
      ttbl_pc-prdd.speed
      ttbl_pc-prdd.start
      ttbl_pc-prdd.stopp
      ttbl_pc-prdd.waste.
    DO i = 1 TO INTEGER(ttbl_pc-prdd.crew) WITH FRAME emp DOWN STREAM-IO COL 19:
      DISPLAY
        ttbl_pc-prdd.emp-id[i]
        ttbl_pc-prdd.rate[i].
      DOWN.
    END.
  END.
END.
  &ENDIF
&ENDIF
