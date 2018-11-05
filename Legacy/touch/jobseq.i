/* jobseq.i */

IF NOT CAN-FIND(FIRST jobseq) THEN
DO:
  MESSAGE 'Job Sequence(s) do not exist.' SKIP
          'This process can not continue, please setup Job Sequence(s).'
      VIEW-AS ALERT-BOX ERROR.
  {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
  RETURN.
END.

&IF '{&SEQUENCE-TYPE}' = 'JOB' &THEN
&Scoped-define SEQTABLE jobseq
&Scoped-define WHERE-PHRASE WHERE jobseq.charge_code = machtran.charge_code
&Scoped-define WHERE-PHRASE-X WHERE TRUE
&Scoped-define WHERE-PHRASE-Y 
&Scoped-define CHARGECODETABLE job-code
&Scoped-define CHARGECODE code
&ELSEIF '{&SEQUENCE-TYPE}' = 'MACHINE' &THEN
&Scoped-define SEQTABLE machseq
&Scoped-define WHERE-PHRASE WHERE machseq.company = company_code ~
AND machseq.machine = machine_code ~
AND machseq.charge_code = machtran.charge_code
&Scoped-define WHERE-PHRASE-X WHERE machseq.company = company_code ~
AND machseq.machine = machine_code
&Scoped-define WHERE-PHRASE-Y WHERE machchrg.company = company_code ~
AND machchrg.machine = machine_code
&Scoped-define CHARGECODETABLE machchrg
&Scoped-define CHARGECODE charge_code
&ENDIF

&IF '{&SEQUENCE-TYPE}' = 'JOB' &THEN
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  IF CAN-FIND(FIRST machseq WHERE machseq.company = company_code
                              AND machseq.machine = machine_code) THEN
  DO:
    RUN Get_Machine_Sequence.
    RETURN.
  END.
&ENDIF
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
  ASSIGN
    itemlist = ''
    button_item = 1
    itemlist = 'END '.
  RELEASE {&SEQTABLE}.
  FIND FIRST machtran
       WHERE machtran.company = company_code
         AND machtran.machine = machine_code
         AND machtran.job_number = job_number
         AND machtran.job_sub = INTEGER(job_sub)
         AND machtran.form_number = INTEGER(form_number)
         AND machtran.blank_number = INTEGER(blank_number)
         AND machtran.pass_sequence = INTEGER(pass_sequence)
         AND machtran.end_time = 0
         AND machtran.total_time = 0
         AND machtran.END_date EQ ?
       NO-LOCK NO-ERROR.

  IF NOT AVAILABLE machtran THEN
  DO:
    FIND LAST machtran
         WHERE machtran.company = company_code
           AND machtran.machine = machine_code
           AND machtran.job_number = job_number
           AND machtran.job_sub = INTEGER(job_sub)
           AND machtran.form_number = INTEGER(form_number)
           AND machtran.blank_number = INTEGER(blank_number)
           AND machtran.pass_sequence = INTEGER(pass_sequence)
           AND (machtran.end_time NE 0
            OR machtran.total_time NE 0)
         NO-LOCK NO-ERROR.
    DO WHILE TRUE:
      IF AVAILABLE machtran THEN
      DO:
        FIND {&SEQTABLE} {&WHERE-PHRASE} NO-LOCK NO-ERROR.
        IF AVAILABLE {&SEQTABLE} THEN
        DO:
          FIND NEXT {&SEQTABLE} NO-LOCK NO-ERROR.
          LEAVE.
        END.
      END.
      ELSE
      LEAVE.
      FIND PREV machtran
           WHERE machtran.company = company_code
             AND machtran.machine = machine_code
             AND machtran.job_number = job_number
             AND machtran.job_sub = INTEGER(job_sub)
             AND machtran.form_number = INTEGER(form_number)
             AND machtran.blank_number = INTEGER(blank_number)
             AND machtran.pass_sequence = INTEGER(pass_sequence)
             AND (machtran.end_time NE 0
              OR machtran.total_time NE 0)
           NO-LOCK NO-ERROR.
    END. /* do while true */
    IF NOT AVAILABLE {&SEQTABLE} THEN
    FIND FIRST {&SEQTABLE} {&WHERE-PHRASE-X} NO-LOCK NO-ERROR.
    itemlist = 'START '.
  END. /* if not avail machtran */
  IF AVAILABLE {&SEQTABLE} THEN
  FIND job-code WHERE job-code.code = {&SEQTABLE}.charge_code NO-LOCK NO-ERROR.
  ELSE
  FIND job-code WHERE job-code.code = machtran.charge_code NO-LOCK NO-ERROR.
  IF AVAILABLE job-code THEN
  itemlist = itemlist + CAPS(job-code.dscr) + ' (' + CAPS(job-code.code) + ')'.
  ELSE
  itemlist = itemlist + CAPS({&SEQTABLE}.charge_code) + ' (' + CAPS({&SEQTABLE}.charge_code) + ')'.
  machtran-rowid = ROWID(machtran).
  IF itemlist BEGINS 'START' THEN
  FOR EACH {&CHARGECODETABLE} {&WHERE-PHRASE-Y} NO-LOCK:
    /* uncomment if sequence codes are to be excluded from selection
    IF CAN-FIND({&SEQTABLE}
       {&WHERE-PHRASE-X} AND
       {&SEQTABLE}.charge_code = {&CHARGECODETABLE}.{&CHARGECODE}) THEN
    NEXT.
    */
&IF '{&SEQUENCE-TYPE}' = 'MACHINE' &THEN
    FIND job-code WHERE job-code.code = {&CHARGECODETABLE}.{&CHARGECODE} NO-LOCK NO-ERROR.
    IF NOT AVAILABLE job-code THEN
    NEXT.
&ENDIF
    itemlist = IF itemlist = '' THEN 'START ' + LC(job-code.dscr) + ' (' + CAPS(job-code.code) + ')'
               ELSE itemlist + '@' + 'START ' + LC(job-code.dscr) + ' (' + CAPS(job-code.code) + ')'.
  END.
  RUN Button_Labels (INPUT-OUTPUT button_item).
  IF button_item = 2 OR itemlist BEGINS 'END' THEN
  APPLY 'CHOOSE' TO Btn_Button-1 IN FRAME {&FRAME-NAME}.
