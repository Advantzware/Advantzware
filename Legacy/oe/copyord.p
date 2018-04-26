/* replacement for copyOrder.i */
/* Attempt to improve problem with mixup in order number with other order */

DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER fil_id AS RECID.
DEFINE INPUT-OUTPUT PARAMETER v-qty-mod AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nufile AS LOG NO-UNDO.

DEF VAR cocode AS CHAR NO-UNDO.
cocode = ipToCompany.

RUN copyOrder (
    INPUT ipFromCompany,
    INPUT ipToCompany,
    INPUT ipFromOrdNo,
    INPUT ipToOrdNo).

PROCEDURE copyOrder:
  DEFINE INPUT PARAMETER ipFromCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFromOrdNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipToOrdNo AS INTEGER NO-UNDO.    

  DEF BUFFER b-oe-ord  FOR oe-ord.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ordm FOR oe-ordm.
  DEF BUFFER b-oe-rel  FOR oe-rel.
  DEF BUFFER b-notes   FOR notes.
  DEF BUFFER b-reftable FOR reftable.

  DEF VAR lv-rec_key LIKE oe-ord.rec_key NO-UNDO.
  DEF VAR lv-r-no LIKE oe-rel.r-no NO-UNDO.
  DEF VAR n-est-no LIKE est.est-no NO-UNDO.

  FIND FIRST oe-ord
      WHERE oe-ord.company EQ ipFromCompany
        AND oe-ord.ord-no  EQ ipFromOrdNo
      NO-LOCK NO-ERROR.

  lv-rec_key = STRING(TODAY,"99999999") +
               STRING(NEXT-VALUE(rec_key_seq,nosweat),"99999999").
  CREATE rec_key.
  ASSIGN
   rec_key.rec_key    = lv-rec_key
   rec_key.table_name = "oe-ord".
      
  FIND b-oe-ord 
    WHERE b-oe-ord.company EQ ipToCompany
      AND b-oe-ord.ord-no  EQ ipToOrdNo
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL b-oe-ord THEN DO:
      MESSAGE "Internal Error - Please notify ASI" SKIP
          "company: " iptocompany SKIP
          "order: " ipToOrdNo SKIP
          VIEW-AS ALERT-BOX.
            RETURN ERROR.
  END.

                
  BUFFER-COPY oe-ord EXCEPT rec_key job-no job-no2 ord-no company TO b-oe-ord.


  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no
      EXCLUSIVE-LOCK:

    CREATE b-oe-ordl.
    BUFFER-COPY oe-ordl EXCEPT rec_key job-no job-no2  TO b-oe-ordl
    ASSIGN
     b-oe-ordl.company = b-oe-ord.company
     b-oe-ordl.ord-no  = b-oe-ord.ord-no.  
     ASSIGN  n-est-no = oe-ordl.est-no . 
   
    ASSIGN
      fil_id    = RECID(oe-ordl).

    IF oe-ordl.est-no <> "" THEN do:
    RUN copyJob (ipFromCompany,ipToCompany,n-est-no,b-oe-ord.ord-no,b-oe-ord.loc).
    ASSIGN b-oe-ord.job-no = STRING(b-oe-ord.ord-no) .
    END.

  END.
  
  FOR EACH oe-ordm
      WHERE oe-ordm.company EQ oe-ord.company
        AND oe-ordm.ord-no EQ oe-ord.ord-no
      NO-LOCK:

    CREATE b-oe-ordm.
    BUFFER-COPY oe-ordm EXCEPT rec_key TO b-oe-ordm
    ASSIGN
     b-oe-ordm.company = b-oe-ord.company
     b-oe-ordm.ord-no  = b-oe-ord.ord-no.
  END.
    
  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ord.company
        AND oe-rel.ord-no    EQ oe-ord.ord-no
      NO-LOCK:

      /* FIND FIRST b-oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR. */
      /* lv-r-no = (IF AVAIL b-oe-rel THEN b-oe-rel.r-no ELSE 0) + 1.  */
      RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT lv-r-no).
      CREATE b-oe-rel.
      BUFFER-COPY oe-rel EXCEPT rec_key TO b-oe-rel

      ASSIGN
       b-oe-rel.company = b-oe-ord.company
       b-oe-rel.r-no    = lv-r-no
       b-oe-rel.ord-no  = b-oe-ord.ord-no
       b-oe-rel.link-no = 0
       b-oe-rel.opened  = YES
       b-oe-rel.rel-no  = 0
       b-oe-rel.ship-date = ?
       b-oe-rel.stat    = "S".
          
      

      
  END.

  FOR EACH notes WHERE notes.rec_key EQ oe-ord.rec_key NO-LOCK:
    CREATE b-notes.
    BUFFER-COPY notes EXCEPT rec_key TO b-notes
    ASSIGN b-notes.rec_key = lv-rec_key.
  END.
   
  
END PROCEDURE.


/***********************************************************************************/
PROCEDURE copyJob:
    DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
    DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.        
    
    DEFINE VARIABLE v-job-no       LIKE oe-ord.job-no  NO-UNDO.
    DEFINE VARIABLE v-job-no2      LIKE oe-ord.job-no2 NO-UNDO.
    DEFINE VARIABLE v-prod-cat     AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE lv-job-recid   AS RECID          NO-UNDO.
    DEFINE VARIABLE choice         AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE hld-id         AS RECID          NO-UNDO.
    DEFINE VARIABLE hld-stat       LIKE job.stat     NO-UNDO.
    DEFINE VARIABLE hld-nufile     AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE v-run-schedule AS LOGICAL        NO-UNDO.

    FIND CURRENT oe-ord.
    DEF BUFFER b-oe-ordl1 FOR oe-ordl.
    DEF BUFFER b-oe-ord1 FOR oe-ord.   

    FIND FIRST eb WHERE eb.est-no = ipEstno 
                  AND eb.company = ipToCompany NO-LOCK NO-ERROR.

   IF AVAIL eb THEN ASSIGN
       v-prod-cat = eb.procat.

    cocode     = ipToCompany.

    v-job-no = fill(" ",6 - length(trim(string(ipOrdno)))) + string(ipOrdno).
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat).
         
    IF v-job-no EQ "" THEN
      v-job-no = fill(" ",6 - length(trim(ipEstno))) + trim(ipEstno).

    IF v-job-no NE "" THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ ipToCompany
                AND job.job-no  EQ v-job-no
                AND job.job-no2 EQ v-job-no2
              NO-ERROR.
         
          IF AVAIL job AND TRIM(job.est-no) NE TRIM(ipEstno) THEN
            IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          AND job-hdr.ord-no  NE ipOrdno) OR
               CAN-FIND(FIRST b-oe-ord1
                        WHERE b-oe-ord1.company EQ job.company
                          AND b-oe-ord1.job-no  EQ job.job-no
                          AND b-oe-ord1.job-no2 EQ job.job-no2
                          AND b-oe-ord1.est-no  EQ job.est-no)   OR
               CAN-FIND(FIRST b-oe-ordl1
                        WHERE b-oe-ordl1.company EQ job.company
                          AND b-oe-ordl1.job-no  EQ job.job-no
                          AND b-oe-ordl1.job-no2 EQ job.job-no2
                          AND b-oe-ordl1.est-no  EQ job.est-no)  THEN RELEASE job.
            ELSE
            DO TRANSACTION:
              FIND CURRENT job NO-ERROR.
              IF AVAIL job THEN DELETE job.
            END.
         
          IF NOT AVAIL job THEN DO:
            RUN create-ord-job (ipFrmCompany,ipToCompany,ipEstno,ipOrdno,v-job-no,v-job-no2,ipLoc,OUTPUT lv-job-recid).
            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
          END.        

          v-qty-mod = YES.

          IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
            /*IF NOT v-qty-mod THEN
               RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).*/
         
            IF v-qty-mod OR job.stat EQ "P" THEN DO:
              RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
              IF NOT choice THEN DO:
                ASSIGN hld-id     = fil_id
                       hld-nufile = nufile 
                       hld-stat   = job.stat
                       nufile     = YES.
         
                RUN jc/jc-calc.p(RECID(job), NO).
                ASSIGN fil_id   = hld-id
                       nufile   = hld-nufile.
               
                IF hld-stat NE "P" THEN DO TRANSACTION:
                  FIND CURRENT job EXCLUSIVE.
                  job.stat = hld-stat.
                  FIND CURRENT job NO-LOCK.
                END.
              END.
            END.
          END.
                
          find first sys-ctrl where
               sys-ctrl.company eq cocode AND
               sys-ctrl.name    eq "SCHEDULE"
               no-lock no-error.

          v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                           ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                           ELSE NO.

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ ipOrdno
                AND oe-ordl.is-a-component EQ NO
         
              BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:
         
            IF LAST-OF(oe-ordl.job-no2) THEN DO:
              ASSIGN
               hld-id     = fil_id
               hld-nufile = nufile
               fil_id     = RECID(oe-ordl).
             
              RUN po/doPo.p (YES).
              /* check oe-ordl.due-date and calc promised date and job's start-date */

              IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.
              
              ASSIGN
               fil_id = hld-id
               nufile = hld-nufile.
            END. /* last of job-no2 */
          END. /* each oe-ordl */
    END. /* if v-job ne "" */
END PROCEDURE. /* copy job */

PROCEDURE set-lock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-form-no LIKE ef.form-no NO-UNDO.
  DEF INPUT PARAMETER ip-op-lock LIKE ef.op-lock NO-UNDO.
  

  FIND FIRST ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND ef.form-no EQ ip-form-no
      NO-ERROR.
  IF avail ef THEN DO:
    ef.op-lock = ip-op-lock.
    RELEASE ef.
  END. 
END PROCEDURE.

/*------------------------------------------------------------------------------*/
PROCEDURE create-ord-job :
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipJobno like oe-ord.job-no no-undo.
  DEFINE INPUT PARAMETER ipJobno2 like oe-ord.job-no2 no-undo.
  DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.
  
  FIND CURRENT oe-ord.
  def output param op-recid as recid no-undo.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  DEFINE VARIABLE v-job-job LIKE job.job     NO-UNDO.
  DEFINE VARIABLE v-job-no  LIKE job.job-no  NO-UNDO.
  DEFINE VARIABLE v-job-no2 LIKE job.job-no2 NO-UNDO.
  DEFINE VARIABLE li-j-no   AS INTEGER     NO-UNDO.
    
  /* === from oe/oe-ord1.p  ============= */
         
  find last job where job.company eq ipToCompany no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.

  ASSIGN
   v-job-no  = ipJobno
   v-job-no2 = ipJobno2.

  FOR EACH job
      WHERE job.company EQ ipToCompany
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  CREATE job.
  ASSIGN job.job        = v-job-job
         job.company    = ipToCompany
         job.loc        = ipLoc
         job.est-no     = ipEstno
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid       = recid(job).

  FOR EACH oe-ordl WHERE oe-ordl.company EQ ipToCompany
                     AND oe-ordl.ord-no  EQ ipOrdno EXCLUSIVE:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ ipToCompany
            AND job-hdr.job-no  EQ oe-ord.job-no
            AND job-hdr.job-no2 EQ oe-ord.job-no2
            AND job-hdr.ord-no  EQ ipOrdno
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-ERROR.

      IF NOT avail job-hdr THEN DO:
         FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
                             AND itemfg.i-no    EQ oe-ordl.i-no
                             NO-LOCK NO-ERROR.   
         
         CREATE job-hdr.
         ASSIGN job-hdr.company      = ipToCompany
                job-hdr.loc          = ipLoc
                job-hdr.est-no       = ipEstno
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         IF avail itemfg THEN
              ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         ASSIGN job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      END. /* not avail job-hdr */

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END. /* avail job hdr */

      ASSIGN job-hdr.est-no  = ipEstno
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2             
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no    = job-hdr.j-no.
      IF ipFrmCompany NE ipToCompany THEN
          oe-ordl.est-no  = job-hdr.est-no.

      FIND CURRENT job-hdr NO-LOCK.
  end. /* each oe-ordl */

  FIND CURRENT job NO-LOCK.
   
END PROCEDURE.


/********************************************************************/

PROCEDURE update-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-update-job-stdate AS LOG  NO-UNDO.
 DEF VAR lv-prom-date AS DATE NO-UNDO.
 DEFINE VARIABLE v-run-schedule AS LOGICAL NO-UNDO.

 IF oe-ordl.job-no = "" THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

  FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
  END. /* each bx-ordl */

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT no-undo.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          ASSIGN
             lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                             truncate(bf-mch.mr-hr,0) * 3600 +
                           ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                             truncate(bf-mch.run-hr,0) * 3600 +
                           ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
             lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END. /* each bf-hdr */
  
  ASSIGN
     lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN truncate(lv-job-time / 3600,0) + 1
                 ELSE truncate(lv-job-time / 3600,0)
     lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day. /*- 1. */

  /*  get from mach-calendar 
  lv-chk-date = lv-start-date.
  li-num-of-wkend = 0.
  DO i = 1 TO lv-first-due-date - lv-start-date:
     IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 THEN li-num-of-wkend = li-num-of-wkend + 1.
     lv-chk-date = lv-chk-date + 1.
  END.
  lv-start-date = lv-start-date - li-num-of-wkend.
  */
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY  /* ip-type = "Update-2" is from v-ord.w*/
  THEN DO:
     lv-update-job-stdate = NO.
     /*MESSAGE "JOB CANNOT BE COMPLETED BEFORE REQUESTED DUE DATE DUE TO TOTAL MACHINE HOURS."
         SKIP
         "PROMISED DATE WILL BE   " lv-prom-date SKIP
         "UPDATE JOB's START DATE & DUE DATE?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    */
     MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
     /*IF lv-update-job-stdate THEN .
     ELSE DO:
         bx-ordl.prom-date = lv-prom-date.           
         return.
     END. */
     lv-start-date = TODAY.
  END. /* lv-start-date < today */
  
  v-run-schedule = NOT CAN-FIND(FIRST sys-ctrl
                                WHERE sys-ctrl.company EQ oe-ord.company
                                  AND sys-ctrl.name EQ 'SCHEDULE'
                                  AND sys-ctrl.char-fld EQ 'NoDate'
                                  AND sys-ctrl.log-fld EQ YES).
  IF v-run-schedule THEN DO: /* run if above does not exist */
  
  /* === reset start-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0.
  
      FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                        AND bf-hdr.job-no = oe-ordl.job-no
                        AND bf-hdr.job-no2 = oe-ordl.job-no2,
          EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2
                        AND NOT bf-mch.anchored
                   BREAK BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:
    
              FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                                AND mach-calendar.m-code = bf-mch.m-code
                                AND mach-calendar.m-date = lv-start-date
                                NO-LOCK NO-ERROR.
              lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                          ELSE 28800. /* 8 HRs*/
              IF lv-m-time LT 0 THEN lv-m-time = 28800.
              lv-maccum-time = lv-maccum-time + lv-m-time.
              IF FIRST(bf-mch.frm) THEN DO:
                 FIND FIRST bf-job OF bf-hdr.
                 ASSIGN
                    bf-job.start-date = lv-start-date
                    lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
              END. /* first (bf-mch.frm) */
              IF FIRST-OF(bf-mch.frm) THEN
                    bf-hdr.start-date = job.start-date.
          
              ASSIGN
              lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
              lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
              bf-mch.seq-no = 0                 
              bf-mch.start-time-su = lv-wrk-st-time
              bf-mch.start-time = lv-wrk-st-time + lv-mr-time
              bf-mch.start-date-su = lv-start-date
              lv-start-date-fr = lv-start-date
              lv-job-time = lv-job-time + lv-mr-time
              lv-start-date = lv-start-date + 
                              IF lv-mr-time > lv-m-time AND
                                 lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                              ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                              ELSE 0
              lv-start-date-fr = lv-start-date.
              IF lv-m-time <> lv-maccum-time THEN DO:
                 lv-start-date = lv-start-date + 
                              IF lv-job-time > lv-maccum-time AND
                                 lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                              ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                              ELSE 0.
              END. /* lv-m-time <> lv-maccum-time  */
              ASSIGN
              lv-start-date-fr = lv-start-date
              bf-mch.end-date-su = lv-start-date
              bf-mch.start-date = lv-start-date
              lv-job-time = lv-job-time + lv-run-time
              lv-start-date = lv-start-date + 
                              IF lv-run-time > lv-m-time AND
                                 lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                              ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                              ELSE 0
              lv-start-date-fr = lv-start-date.
    
              IF lv-m-time <> lv-maccum-time THEN
                 lv-start-date = lv-start-date + 
                              IF lv-job-time > lv-maccum-time AND
                                 lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                              ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                              ELSE 0.
              
              ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                     bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                     bf-mch.end-date = lv-start-date           
                     lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
      END. /* each bf-hdr */
  END. /* if v-run-schedule*/
  
  ASSIGN
     bx-ordl.prom-date = lv-prom-date
     bx-ordl.req-date  = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.
