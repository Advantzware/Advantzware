/* post_.i 
   Modified   YSK 04/03/2001   Not update machtran.posted when user runs report */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'post_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="machtran" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="machtran" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="machtran" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="machtran" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
DEF VAR ll-invalid AS LOG NO-UNDO.
def var cINo as char no-undo.
def var cIName as char no-undo.

FOR EACH b-mach FIELDS(m-code) WHERE
    b-mach.company = g_company AND
    b-mach.m-code GE begin_machine AND
    b-mach.m-code LE end_machine
    NO-LOCK,
    EACH machtran
    WHERE machtran.company      EQ g_company
      AND machtran.machine      EQ b-mach.m-code
      AND machtran.posted       EQ YES
      AND machtran.job_number   GE begin_job-no
      AND machtran.job_number   LE END_job-no
      AND machtran.job_sub      GE begin_job-no2
      AND machtran.job_sub      LE END_job-no2  
      AND ((machtran.end_date   GE begin_date AND
            machtran.end_date   LE end_date AND
            (machtran.shift GE TRIM(STRING(begin_shift,">>")) AND
             machtran.shift LE TRIM(STRING(end_shift,">>")))) OR
           CAN-FIND(FIRST mach
                    WHERE mach.company  EQ machtran.company
                      AND mach.m-code   EQ machtran.machine
                      AND mach.industry EQ "X")):

  ll-invalid = NO.

  IF machtran.start_date EQ machtran.end_date THEN
    ll-invalid = CAN-FIND(FIRST pc-prdd
                          WHERE pc-prdd.company EQ machtran.company
                            AND pc-prdd.m-code  EQ machtran.machine
                            AND pc-prdd.op-date EQ machtran.end_date
                            AND pc-prdd.start   EQ machtran.start_time
                            AND pc-prdd.stopp   EQ machtran.end_time)         OR
                 CAN-FIND(FIRST mch-act
                          WHERE mch-act.company EQ machtran.company
                            AND mch-act.m-code  EQ machtran.machine
                            AND mch-act.op-date EQ machtran.end_date
                            AND mch-act.start   EQ machtran.start_time
                            AND mch-act.stopp   EQ machtran.end_time).

  ELSE
    ll-invalid = CAN-FIND(FIRST pc-prdd
                          WHERE pc-prdd.company EQ machtran.company
                            AND pc-prdd.m-code  EQ machtran.machine
                            AND pc-prdd.op-date EQ machtran.start_date
                            AND pc-prdd.start   EQ machtran.start_time
                            AND pc-prdd.stopp   EQ 86340)                     OR
                 CAN-FIND(FIRST pc-prdd
                          WHERE pc-prdd.company EQ machtran.company
                            AND pc-prdd.m-code  EQ machtran.machine
                            AND pc-prdd.op-date EQ machtran.end_date
                            AND pc-prdd.start   EQ 0
                            AND pc-prdd.stopp   EQ machtran.end_time - 86400) OR
                 CAN-FIND(FIRST mch-act
                          WHERE mch-act.company EQ machtran.company
                            AND mch-act.m-code  EQ machtran.machine
                            AND mch-act.op-date EQ machtran.start_date
                            AND mch-act.start   EQ machtran.start_time
                            AND mch-act.stopp   EQ 86340)                     OR
                 CAN-FIND(FIRST mch-act
                          WHERE mch-act.company EQ machtran.company
                            AND mch-act.m-code  EQ machtran.machine
                            AND mch-act.op-date EQ machtran.end_date
                            AND mch-act.start   EQ 0
                            AND mch-act.stopp   EQ machtran.end_time - 86400).

  IF NOT ll-invalid THEN DO:
    /* don't post to ASI ony set posted flag to yes 
       07/30/01  YSK                                  */
    find first mach where mach.company = machtran.company and
                          mach.m-code = machtran.machine and
                          mach.industry = "X" 
                          no-lock no-error.
/*
    if machtran.machine = "w/h" or machtran.machine = "coindr" or machtran.machine = "foindr"
       or machtran.machine = "maintc"
    then do:
*/
    if avail mach and mach.industry = "X" then do:
       next.
    end.    
    /* ======================================= */
    
   lv-valid-to-post = YES.

   IF machtran.start_date eq machtran.end_date THEN
    DO:
      IF NOT CAN-FIND(ttbl_pc-prdh
              WHERE ttbl_pc-prdh.company = machtran.company
                AND ttbl_pc-prdh.m-code = machtran.machine
                AND ttbl_pc-prdh.shift = INTEGER(machtran.shift)
                AND ttbl_pc-prdh.trans-date = machtran.end_date) THEN
      DO:
        CREATE ttbl_pc-prdh.
        ASSIGN
          ttbl_pc-prdh.company = machtran.company
          ttbl_pc-prdh.m-code = machtran.machine
          ttbl_pc-prdh.shift = INTEGER(machtran.shift)
          ttbl_pc-prdh.trans-date = machtran.end_date
          ttbl_pc-prdh.user-id = USERID('NOSWEAT')
          ttbl_pc-prdh.dept = b-mach.dept[1].
      END.
      RUN jc/GetItemFromJob.p (INPUT machtran.company,
                            INPUT machtran.job_number,
                            INPUT machtran.job_sub,
                            INPUT machtran.form_number,
                            INPUT machtran.blank_number,
                            INPUT mach.m-code,
                            OUTPUT cINo,
                            OUTPUT cIName).
      FIND FIRST job WHERE job.company = machtran.company
                       AND job.job-no = machtran.job_number
                       AND job.job-no2 = machtran.job_sub
                     NO-LOCK NO-ERROR.
      FIND FIRST mach WHERE mach.company = machtran.company
                        AND mach.m-code = machtran.machine
                      NO-LOCK NO-ERROR.
      FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                           AND job-hdr.job-no = machtran.job_number
                           AND job-hdr.job-no2 = machtran.job_sub
                           AND job-hdr.frm = machtran.form_number
                           AND job-hdr.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                             AND job-hdr.job-no = machtran.job_number
                             AND job-hdr.job-no2 = machtran.job_sub
                             AND job-hdr.frm = machtran.form_number
                        /*     AND job-hdr.blank-no = machtran.blank_number */
                         NO-LOCK NO-ERROR.
      FIND FIRST job-mch WHERE job-mch.company = machtran.company
                           AND job-mch.job-no = machtran.job_number
                           AND job-mch.job-no2 = machtran.job_sub
                           AND job-mch.frm = machtran.form_number
                           AND job-mch.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mch THEN
         FIND FIRST job-mch WHERE job-mch.company = machtran.company
                            AND job-mch.job-no = machtran.job_number
                            AND job-mch.job-no2 = machtran.job_sub
                            AND job-mch.frm = machtran.form_number
                            NO-LOCK NO-ERROR.
      IF AVAILABLE job-hdr AND NOT AVAIL job-mch THEN
      DO:
        FIND FIRST job-mch WHERE job-mch.company = machtran.company
                             AND job-mch.j-no = job-hdr.j-no
                             AND job-mch.i-no = job-hdr.i-no
                             AND job-mch.m-code = machtran.machine
                           NO-LOCK NO-ERROR.
      END.
      FIND FIRST ttbl_pc-prdd
           WHERE ttbl_pc-prdd.company = machtran.company
             AND ttbl_pc-prdd.m-code = machtran.machine
             AND ttbl_pc-prdd.job-no = machtran.job_number
             AND ttbl_pc-prdd.job-no2 = machtran.job_sub
             AND ttbl_pc-prdd.frm = machtran.form_number
             AND ttbl_pc-prdd.blank-no = machtran.blank_number
             AND ttbl_pc-prdd.pass = machtran.pass_sequence
             AND ttbl_pc-prdd.i-no = cINo
/*              AND ttbl_pc-prdd.i-no = (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')                                     */
             AND ttbl_pc-prdd.code = machtran.charge_code
             AND ttbl_pc-prdd.op-date = machtran.end_date
             AND ttbl_pc-prdd.start = machtran.start_time
             AND ttbl_pc-prdd.stopp = machtran.end_time
             AND ttbl_pc-prdd.shift = INTEGER(machtran.shift)
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE ttbl_pc-prdd THEN
      DO:
        CREATE ttbl_pc-prdd.
        ASSIGN
          ttbl_pc-prdd.company = machtran.company
          ttbl_pc-prdd.m-code = machtran.machine
          ttbl_pc-prdd.job-no = machtran.job_number
          ttbl_pc-prdd.job-no2 = machtran.job_sub
          ttbl_pc-prdd.frm = machtran.form_number
          ttbl_pc-prdd.blank-no = machtran.blank_number
          ttbl_pc-prdd.pass = machtran.pass_sequence
          ttbl_pc-prdd.i-no = cINo
          ttbl_pc-prdd.i-name = cIName
/*           ttbl_pc-prdd.i-no = (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')                              */
          ttbl_pc-prdd.code = machtran.charge_code
          ttbl_pc-prdd.op-date = machtran.end_date
          ttbl_pc-prdd.start = machtran.start_time
          ttbl_pc-prdd.stopp = machtran.end_time
          ttbl_pc-prdd.startx = SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
          ttbl_pc-prdd.stopx =  SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
          ttbl_pc-prdd.shift = INTEGER(machtran.shift)
          ttbl_pc-prdd.complete = machtran.complete /* ??????????????????????????????????? */
          ttbl_pc-prdd.dept = IF AVAILABLE mach THEN mach.dept[1] ELSE ''
          ttbl_pc-prdd.hours = machtran.total_time / 3600
          ttbl_pc-prdd.j-no = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
          ttbl_pc-prdd.job = job.job
          ttbl_pc-prdd.speed = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
          machtotaltime = machtotaltime + machtran.total_time.
          
        CREATE ttbl_rowid.
        ASSIGN
          ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
          ttbl_rowid.total_time = machtran.total_time.
      END.

      IF v-tspost AND v-tspost-val = "Actual" THEN DO:
         ASSIGN ttbl_pc-prdd.crew = 0
                ttbl_pc-prdd.complete = if v-autopost THEN YES ELSE NO /*tspostfg-log*/ .
         FOR EACH machemp WHERE machemp.table_rec_key = machtran.rec_key 
               /* AND machemp.START_date = machtran.START_date
                AND machemp.START_time = machtran.START_time
                AND machemp.end_date = machtran.end_date
                AND machemp.end_time = machtran.end_time */ NO-LOCK:
            ASSIGN ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
            ASSIGN ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                   ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate . 
            
         END.  
         IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
      END.
      ELSE DO:
          ASSIGN ttbl_pc-prdd.crew = 0.
          FOR EACH machemp NO-LOCK WHERE machemp.table_rec_key = machtran.rec_key :
              ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
          END.
          IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
          ELSE DO:
            find job-code where job-code.code = ttbl_pc-prdd.CODE no-lock no-error.
            if available job-code AND job-code.cat = "MR" 
               THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate . 
            ELSE if available job-code AND job-code.cat = "RUN"
               THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate . 
          END.
      END.
/*
      ASSIGN
        ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
/*      ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1
        ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee
        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate. */  */
    END.
    ELSE  /* Start Date and End Date are Different.  Need two records in Advantzware */
    DO:
      IF NOT CAN-FIND(ttbl_pc-prdh
              WHERE ttbl_pc-prdh.company = machtran.company
                AND ttbl_pc-prdh.m-code = machtran.machine
                AND ttbl_pc-prdh.shift = INTEGER(machtran.shift)
                AND ttbl_pc-prdh.trans-date = machtran.start_date) THEN
      DO:
        CREATE ttbl_pc-prdh.
        ASSIGN
          ttbl_pc-prdh.company = machtran.company
          ttbl_pc-prdh.m-code = machtran.machine
          ttbl_pc-prdh.shift = INTEGER(machtran.shift)
          ttbl_pc-prdh.trans-date = machtran.start_date
          ttbl_pc-prdh.user-id = USERID('NOSWEAT')
          ttbl_pc-prdh.dept = b-mach.dept[1].
      END.
      RUN jc/GetItemFromJob.p (INPUT machtran.company,
                            INPUT machtran.job_number,
                            INPUT machtran.job_sub,
                            INPUT machtran.form_number,
                            INPUT machtran.blank_number,
                            INPUT mach.m-code,
                            OUTPUT cINo,
                            OUTPUT cIName).
      FIND FIRST job WHERE job.company = machtran.company
                       AND job.job-no = machtran.job_number
                       AND job.job-no2 = machtran.job_sub
                     NO-LOCK NO-ERROR.
      FIND FIRST mach WHERE mach.company = machtran.company
                        AND mach.m-code = machtran.machine
                      NO-LOCK NO-ERROR.
      FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                           AND job-hdr.job-no = machtran.job_number
                           AND job-hdr.job-no2 = machtran.job_sub
                           AND job-hdr.frm = machtran.form_number
                           AND job-hdr.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                             AND job-hdr.job-no = machtran.job_number
                             AND job-hdr.job-no2 = machtran.job_sub
                             AND job-hdr.frm = machtran.form_number
                        /*     AND job-hdr.blank-no = machtran.blank_number */
                         NO-LOCK NO-ERROR.
      FIND FIRST job-mch WHERE job-mch.company = machtran.company
                           AND job-mch.job-no = machtran.job_number
                           AND job-mch.job-no2 = machtran.job_sub
                           AND job-mch.frm = machtran.form_number
                           AND job-mch.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mch THEN
         FIND FIRST job-mch WHERE job-mch.company = machtran.company
                            AND job-mch.job-no = machtran.job_number
                            AND job-mch.job-no2 = machtran.job_sub
                            AND job-mch.frm = machtran.form_number
                            NO-LOCK NO-ERROR.
      IF AVAILABLE job-hdr AND NOT AVAIL job-mch THEN
      DO:
            FIND FIRST job-mch WHERE job-mch.company = machtran.company
                             AND job-mch.j-no = job-hdr.j-no
                             AND job-mch.i-no = job-hdr.i-no
                             AND job-mch.m-code = machtran.machine
                           NO-LOCK NO-ERROR.
      END.
      FIND FIRST ttbl_pc-prdd
           WHERE ttbl_pc-prdd.company = machtran.company
             AND ttbl_pc-prdd.m-code = machtran.machine
             AND ttbl_pc-prdd.job-no = machtran.job_number
             AND ttbl_pc-prdd.job-no2 = machtran.job_sub
             AND ttbl_pc-prdd.frm = machtran.form_number
             AND ttbl_pc-prdd.blank-no = machtran.blank_number
             AND ttbl_pc-prdd.pass = machtran.pass_sequence
             AND ttbl_pc-prdd.i-no = cINo
/*           (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')          */
             AND ttbl_pc-prdd.code = machtran.charge_code
             AND ttbl_pc-prdd.op-date = machtran.start_date
             AND ttbl_pc-prdd.start = machtran.start_time
/*           AND ttbl_pc-prdd.stopp = machtran.end_time  */
             AND ttbl_pc-prdd.stopp = 86340
             AND ttbl_pc-prdd.shift = INTEGER(machtran.shift)
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE ttbl_pc-prdd THEN
      DO:
        CREATE ttbl_pc-prdd.
        ASSIGN
          ttbl_pc-prdd.company = machtran.company
          ttbl_pc-prdd.m-code = machtran.machine
          ttbl_pc-prdd.job-no = machtran.job_number
          ttbl_pc-prdd.job-no2 = machtran.job_sub
          ttbl_pc-prdd.frm = machtran.form_number
          ttbl_pc-prdd.blank-no = machtran.blank_number
          ttbl_pc-prdd.pass = machtran.pass_sequence
          ttbl_pc-prdd.i-no = cINo
          ttbl_pc-prdd.i-name = cIName
/*             (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')            */
          ttbl_pc-prdd.code = machtran.charge_code
          ttbl_pc-prdd.op-date = machtran.start_date
          ttbl_pc-prdd.start = machtran.start_time
          ttbl_pc-prdd.stopp = 86400
          ttbl_pc-prdd.startx = SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
          ttbl_pc-prdd.stopx =  SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
          ttbl_pc-prdd.shift = INTEGER(machtran.shift)
          ttbl_pc-prdd.complete = machtran.COMPLETE /* ??????????????????????????????????? */
          ttbl_pc-prdd.dept = IF AVAILABLE mach THEN mach.dept[1] ELSE ''
/*        ttbl_pc-prdd.hours = machemp.total_time / 3600 */
          ttbl_pc-prdd.hours = ((86400 - machtran.start_time) / 3600)
          ttbl_pc-prdd.j-no = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
          ttbl_pc-prdd.job = job.job
          ttbl_pc-prdd.speed = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
/*        machtotaltime = machtotaltime + machemp.total_time. */
          machtotaltime = machtotaltime + machtran.total_time.
          
        CREATE ttbl_rowid.
        ASSIGN
          ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
/*        ttbl_rowid.total_time = machemp.total_time. */
          ttbl_rowid.total_time = 86400 - machtran.start_time.
      END.

      IF v-tspost AND v-tspost-val = "Actual" THEN DO:
         ASSIGN ttbl_pc-prdd.crew = 0
                ttbl_pc-prdd.complete = if v-autopost THEN YES ELSE NO /*tspostfg-log*/ .
         FOR EACH machemp WHERE machemp.table_rec_key = machtran.rec_key 
               /* AND machemp.START_date = machtran.START_date
                AND machemp.START_time = machtran.START_time
                AND machemp.end_date = machtran.end_date
                AND machemp.end_time = machtran.end_time */ NO-LOCK:
            ASSIGN ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
            ASSIGN ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                   ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate . 
            
         END.             
         IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
      END.
      ELSE DO:
          ASSIGN ttbl_pc-prdd.crew = 0.
          FOR EACH machemp NO-LOCK WHERE machemp.table_rec_key = machtran.rec_key :
              ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
          END.
          IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
          ELSE DO:
            find job-code where job-code.code = ttbl_pc-prdd.CODE no-lock no-error.
            if available job-code AND job-code.cat = "MR" 
               THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate . 
            ELSE if available job-code AND job-code.cat = "RUN"
               THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate . 
          END.
      END.
      /*ASSIGN
        ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
        ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee
        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate. */

      IF NOT CAN-FIND(ttbl_pc-prdh
              WHERE ttbl_pc-prdh.company = machtran.company
                AND ttbl_pc-prdh.m-code = machtran.machine
                AND ttbl_pc-prdh.shift = INTEGER(machtran.shift)
                AND ttbl_pc-prdh.trans-date = machtran.end_date) THEN
      DO:
        CREATE ttbl_pc-prdh.
        ASSIGN
          ttbl_pc-prdh.company = machtran.company
          ttbl_pc-prdh.m-code = machtran.machine
          ttbl_pc-prdh.shift = INTEGER(machtran.shift)
          ttbl_pc-prdh.trans-date = machtran.end_date
          ttbl_pc-prdh.user-id = USERID('NOSWEAT')
          ttbl_pc-prdh.dept = b-mach.dept[1].
      END.
      RUN jc/GetItemFromJob.p (INPUT machtran.company,
                            INPUT machtran.job_number,
                            INPUT machtran.job_sub,
                            INPUT machtran.form_number,
                            INPUT machtran.blank_number,
                            INPUT mach.m-code,
                            OUTPUT cINo,
                            OUTPUT cIName).
      FIND FIRST job WHERE job.company = machtran.company
                       AND job.job-no = machtran.job_number
                       AND job.job-no2 = machtran.job_sub
                     NO-LOCK NO-ERROR.
      FIND FIRST mach WHERE mach.company = machtran.company
                        AND mach.m-code = machtran.machine
                      NO-LOCK NO-ERROR.
      FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                           AND job-hdr.job-no = machtran.job_number
                           AND job-hdr.job-no2 = machtran.job_sub
                           AND job-hdr.frm = machtran.form_number
                           AND job-hdr.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE job-hdr.company = machtran.company
                             AND job-hdr.job-no = machtran.job_number
                             AND job-hdr.job-no2 = machtran.job_sub
                             AND job-hdr.frm = machtran.form_number
                        /*     AND job-hdr.blank-no = machtran.blank_number */
                         NO-LOCK NO-ERROR.
      FIND FIRST job-mch WHERE job-mch.company = machtran.company
                           AND job-mch.job-no = machtran.job_number
                           AND job-mch.job-no2 = machtran.job_sub
                           AND job-mch.frm = machtran.form_number
                           AND job-mch.blank-no = machtran.blank_number
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL job-mch THEN
         FIND FIRST job-mch WHERE job-mch.company = machtran.company
                            AND job-mch.job-no = machtran.job_number
                            AND job-mch.job-no2 = machtran.job_sub
                            AND job-mch.frm = machtran.form_number
                            NO-LOCK NO-ERROR.
      IF AVAILABLE job-hdr AND NOT AVAIL job-mch THEN
      DO:
            FIND FIRST job-mch WHERE job-mch.company = machtran.company
                             AND job-mch.j-no = job-hdr.j-no
                             AND job-mch.i-no = job-hdr.i-no
                             AND job-mch.m-code = machtran.machine
                           NO-LOCK NO-ERROR.
      END.
      FIND FIRST ttbl_pc-prdd
           WHERE ttbl_pc-prdd.company = machtran.company
             AND ttbl_pc-prdd.m-code = machtran.machine
             AND ttbl_pc-prdd.job-no = machtran.job_number
             AND ttbl_pc-prdd.job-no2 = machtran.job_sub
             AND ttbl_pc-prdd.frm = machtran.form_number
             AND ttbl_pc-prdd.blank-no = machtran.blank_number
             AND ttbl_pc-prdd.pass = machtran.pass_sequence
             AND ttbl_pc-prdd.i-no = cINo
/*           (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')          */
             AND ttbl_pc-prdd.code = machtran.charge_code
             AND ttbl_pc-prdd.op-date = machtran.end_date
             AND ttbl_pc-prdd.start = 0
             AND ttbl_pc-prdd.stopp = machtran.end_time - 86400
             AND ttbl_pc-prdd.shift = INTEGER(machtran.shift)
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE ttbl_pc-prdd THEN
      DO:
        CREATE ttbl_pc-prdd.
        ASSIGN
          ttbl_pc-prdd.company = machtran.company
          ttbl_pc-prdd.m-code = machtran.machine
          ttbl_pc-prdd.job-no = machtran.job_number
          ttbl_pc-prdd.job-no2 = machtran.job_sub
          ttbl_pc-prdd.frm = machtran.form_number
          ttbl_pc-prdd.blank-no = machtran.blank_number
          ttbl_pc-prdd.pass = machtran.pass_sequence
          ttbl_pc-prdd.i-no = cINo
          ttbl_pc-prdd.i-no = cIName
/*             (IF AVAILABLE job-mch AND job-mch.i-no NE '' THEN job-mch.i-no ELSE */
/*                     IF  AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')            */
          ttbl_pc-prdd.code = machtran.charge_code
          ttbl_pc-prdd.op-date = machtran.end_date
          ttbl_pc-prdd.start = 0
          ttbl_pc-prdd.stopp = machtran.end_time - 86400
          ttbl_pc-prdd.startx = SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.start,"hh:mm"),4,2)
          ttbl_pc-prdd.stopx =  SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),1,2) +
                                SUBstring(STRING(ttbl_pc-prdd.stopp,"hh:mm"),4,2)
          ttbl_pc-prdd.shift = INTEGER(machtran.shift)
          ttbl_pc-prdd.complete = machtran.COMPLETE /* ??????????????????????????????????? */
          ttbl_pc-prdd.dept = IF AVAILABLE mach THEN mach.dept[1] ELSE ''
/*        ttbl_pc-prdd.hours = machemp.total_time / 3600 */
          ttbl_pc-prdd.hours = machtran.end_time / 3600
          ttbl_pc-prdd.j-no = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
          ttbl_pc-prdd.job = job.job
          ttbl_pc-prdd.speed = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0.
        CREATE ttbl_rowid.
        ASSIGN
          ttbl_rowid.pc-prdd_rowid = ROWID(ttbl_pc-prdd)
/*        ttbl_rowid.total_time = machemp.total_time. */
          ttbl_rowid.total_time = machtran.end_time.
      END.

      IF v-tspost AND v-tspost-val = "Actual" THEN DO:
         ASSIGN ttbl_pc-prdd.crew = 0
                ttbl_pc-prdd.complete = if v-autopost THEN YES ELSE NO /*tspostfg-log*/ .
         FOR EACH machemp WHERE machemp.table_rec_key = machtran.rec_key 
               /* AND machemp.START_date = machtran.START_date
                AND machemp.START_time = machtran.START_time
                AND machemp.end_date = machtran.end_date
                AND machemp.end_time = machtran.end_time */ NO-LOCK:
            ASSIGN ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1
                 ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee 
                 ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate . 
            
         END.             
         IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
      END.
      ELSE DO:
          ASSIGN ttbl_pc-prdd.crew = 0.
          FOR EACH machemp NO-LOCK WHERE machemp.table_rec_key = machtran.rec_key :
              ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
          END.
          IF ttbl_pc-prdd.crew = 0 THEN
             PUT "No crew is entered for machine: " ttbl_pc-prdd.m-code " job#: " 
                 ttbl_pc-prdd.job-no "-" ttbl_pc-prdd.job-no2 
                 "charge code: " ttbl_pc-prdd.CODE SKIP.
          ELSE DO:
             find job-code where job-code.code = ttbl_pc-prdd.CODE no-lock no-error.
             if available job-code AND job-code.cat = "MR" 
                THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.mr-rate . 
             ELSE if available job-code AND job-code.cat = "RUN"
                THEN ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = mach.run-rate . 
          END.             
      /*ASSIGN
        ttbl_pc-prdd.crew = ttbl_pc-prdd.crew + 1.
        ttbl_pc-prdd.emp-id[INTEGER(ttbl_pc-prdd.crew)] = machemp.employee
        ttbl_pc-prdd.rate[INTEGER(ttbl_pc-prdd.crew)] = machemp.rate. */
      END.
    END.  /* else start_date = end_date*/

    FOR EACH ttbl_rowid EXCLUSIVE-LOCK:
      FIND ttbl_pc-prdd WHERE ROWID(ttbl_pc-prdd) = ttbl_rowid.pc-prdd_rowid EXCLUSIVE-LOCK.
      ASSIGN
        shiftpct = IF ttbl_rowid.total_time / machtotaltime EQ ? THEN 0
                   ELSE ttbl_rowid.total_time / machtotaltime
        ttbl_pc-prdd.waste = machtran.waste_qty * shiftpct
        ttbl_pc-prdd.qty = machtran.run_qty * shiftpct.

      DELETE ttbl_rowid.
    END. /* each ttbl_rowid */
    assign machtotaltime = 0
        /* machtran.posted = if post then yes else no*/.
  END.   /* ll-valid */
END. /* each machtran */

FOR EACH ttbl_pc-prdh NO-LOCK WHERE ttbl_pc-prdh.company = selected-company,
    EACH ttbl_pc-prdd NO-LOCK
         WHERE ttbl_pc-prdd.company = ttbl_pc-prdh.company
           AND ttbl_pc-prdd.m-code = ttbl_pc-prdh.m-code
           AND ttbl_pc-prdd.op-date = ttbl_pc-prdh.trans-date
           AND ttbl_pc-prdd.shift = ttbl_pc-prdh.shift
         BREAK BY ttbl_pc-prdh.m-code
               BY ttbl_pc-prdd.dept
               BY ttbl_pc-prdh.trans-date BY ttbl_pc-prdd.START
         WITH FRAME pc-prdh STREAM-IO NO-BOX WIDTH 200 DOWN:
  IF FIRST-OF(ttbl_pc-prdh.m-code) THEN
  DO:
    DISPLAY ttbl_pc-prdd.m-code FORMAT 'X(20)' COLUMN-LABEL 'Mach / Description'.
    FIND FIRST mach WHERE mach.company = ttbl_pc-prdd.company
                      AND mach.m-code = ttbl_pc-prdd.m-code
                    NO-LOCK NO-ERROR.
    ASSIGN
      machtotaltime = 0
      waste-qty = 0
      run-qty = 0.
  END.
  IF FIRST-OF(ttbl_pc-prdd.dept) THEN
  DISPLAY ttbl_pc-prdd.dept COLUMN-LABEL 'DP'.
  IF FIRST-OF(ttbl_pc-prdh.trans-date) THEN
  DISPLAY ttbl_pc-prdh.trans-date COLUMN-LABEL 'Date'.
  DISPLAY
    ttbl_pc-prdd.shift COLUMN-LABEL 'Sh'
    ttbl_pc-prdd.job-no + '-' + STRING(ttbl_pc-prdd.job-no2) FORMAT 'X(9)' LABEL 'Job No.'
    ttbl_pc-prdd.frm COLUMN-LABEL 'Frm'
    ttbl_pc-prdd.blank-no COLUMN-LABEL 'Blk'
    ttbl_pc-prdd.i-no COLUMN-LABEL 'Item / Description'
    ttbl_pc-prdd.code
    ttbl_pc-prdd.hours COLUMN-LABEL 'Hours'
    INTEGER(ttbl_pc-prdd.crew) FORMAT '>9' COLUMN-LABEL 'Crew'
    STRING(ttbl_pc-prdd.start,'HH:MM') FORMAT 'X(5)' LABEL 'Start'
    STRING(ttbl_pc-prdd.stopp,'HH:MM') FORMAT 'X(5)' LABEL 'Stop'
    ttbl_pc-prdd.qty COLUMN-LABEL 'Run Qty'
    ttbl_pc-prdd.waste
    ttbl_pc-prdd.complete COLUMN-LABEL 'C'
    ttbl_pc-prdd.emp-id[1] COLUMN-LABEL 'EmpID'
    ttbl_pc-prdd.rate[1]  WHEN t-prt-rate COLUMN-LABEL 'Rate'
    .
  DOWN.
  IF FIRST-OF(ttbl_pc-prdh.m-code) THEN
     DISPLAY mach.m-dscr WHEN AVAILABLE(mach) @ ttbl_pc-prdd.m-code.
  IF ttbl_pc-prdd.i-no <> "" THEN DISPLAY ttbl_pc-prdd.i-name @ ttbl_pc-prdd.i-no.
  IF ttbl_pc-prdd.crew GT 1 THEN
  DO i = 2 TO INTEGER(ttbl_pc-prdd.crew):
    IF i GT 2 THEN
    DOWN.
    DISPLAY
      ttbl_pc-prdd.emp-id[i] @ ttbl_pc-prdd.emp-id[1]
      ttbl_pc-prdd.rate[i] WHEN t-prt-rate @ ttbl_pc-prdd.rate[1].
  END.
  ASSIGN
    machtotaltime = machtotaltime + ttbl_pc-prdd.hours
    waste-qty = waste-qty + ttbl_pc-prdd.waste
    run-qty = run-qty + ttbl_pc-prdd.qty.
  IF LAST-OF(ttbl_pc-prdh.m-code) THEN
  DO:
    UNDERLINE ttbl_pc-prdd.hours ttbl_pc-prdd.qty ttbl_pc-prdd.waste.
    DOWN.
    DISPLAY
      '   Machine Totals:' @ ttbl_pc-prdd.i-no
      machtotaltime @ ttbl_pc-prdd.hours
      waste-qty @ ttbl_pc-prdd.waste
      run-qty @ ttbl_pc-prdd.qty.
  END.
END. /* each ttbl_pc-prdh */
