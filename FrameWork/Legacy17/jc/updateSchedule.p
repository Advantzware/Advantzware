  
  DEFINE INPUT PARAMETER ipdtStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER iprJobRowID AS ROWID NO-UNDO.

  DEF BUFFER bfJob FOR job.
  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.

  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT no-undo.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-job2-time  AS INT NO-UNDO.  /* job time if job needs more than a day */
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-got-st-time AS LOG NO-UNDO.
  DEF VAR lv-prev-end-time AS INT NO-UNDO.
  DEF VAR lv-day-time AS INT NO-UNDO.  /* mach.start-time - previous mach's end-time */
  DEF VAR lv-lap-time AS INT NO-UNDO.

  FIND FIRST bfJob WHERE ROWID(bfJob) EQ iprJobRowID EXCLUSIVE-LOCK.
  bfJob.start-date = ipdtStartDate.
  FIND CURRENT bfJob NO-LOCK.

  FOR EACH bf-hdr OF bfJob:
      bf-hdr.start-date = bfJob.start-date.
  END.
  ASSIGN
    lv-start-date = bfJob.start-date
    lv-job-time = 0.
  FOR EACH bf-mch OF bfjob /*WHERE NOT bf-mch.anchored */
      BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:

      FIND FIRST mach-calendar WHERE mach-calendar.company = bfJob.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date
                        NO-LOCK NO-ERROR.
      lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                  ELSE 28800. /* 8 HRs*/
      IF lv-m-time LT 0 THEN lv-m-time = 28800.
      
      IF AVAIL mach-calendar AND NOT lv-got-st-time THEN
                      ASSIGN lv-start-time = mach-calendar.start-time
                             lv-got-st-time = YES.

      lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                  truncate(bf-mch.mr-hr,0) * 3600 +
                ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
      lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                  truncate(bf-mch.run-hr,0) * 3600 +
                ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

      ASSIGN bf-mch.seq-no = 0                 
             bf-mch.start-time-su = lv-start-time + lv-job-time + lv-day-time.        
             bf-mch.start-date-su = lv-start-date               .

      lv-job-time = lv-job-time + lv-mr-time + lv-run-time.
      
      lv-start-date = lv-start-date + 
                      IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0)
                      ELSE 0.
      
      IF lv-mr-time > lv-m-time THEN DO:
         lv-job2-time = lv-mr-time - lv-m-time.
         lv-lap-time = bf-mch.start-time-su - lv-start-time.
         FIND FIRST mach-calendar WHERE mach-calendar.company = bfJob.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date 
                        NO-LOCK NO-ERROR.
         lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                     ELSE 28800. /* 8 HRs*/.
         IF lv-m-time LT 0 THEN lv-m-time = 28800.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
         ASSIGN bf-mch.end-time-su = lv-start-time + lv-job2-time + lv-lap-time
                bf-mch.start-time = lv-start-time + lv-job2-time + lv-lap-time
                bf-mch.end-date-su = lv-start-date
                bf-mch.start-date = lv-start-date 
                lv-day-time = lv-start-time - lv-prev-end-time + 86400 .       
      END.
      ELSE ASSIGN bf-mch.end-time-su = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                  bf-mch.start-time = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                  bf-mch.end-date-su = lv-start-date
                  bf-mch.start-date = lv-start-date 
                  lv-lap-time = 0.

      lv-start-date = lv-start-date + 
                      IF (lv-run-time ) > lv-m-time THEN TRUNCATE((lv-run-time) / lv-m-time,0)
                      ELSE 0.

      IF (lv-run-time) > lv-m-time THEN DO:
         lv-job2-time = lv-mr-time + lv-run-time - lv-m-time.
         lv-lap-time = bf-mch.start-time - lv-start-time.
         FIND FIRST mach-calendar WHERE mach-calendar.company = bfJob.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date 
                        NO-LOCK NO-ERROR.
         lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                     ELSE 28800. /* 8 HRs*/.
         IF lv-m-time LT 0 THEN lv-m-time = 28800.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
         ASSIGN bf-mch.end-time = lv-start-time + lv-job2-time + lv-lap-time            
                bf-mch.end-date = lv-start-date
                lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400 .       
      END.
      ELSE ASSIGN bf-mch.end-time = /*lv-start-time + lv-job-time */
                                    bf-mch.start-time + lv-run-time
                  bf-mch.end-date = lv-start-date
                  lv-lap-time = 0.
      
      lv-prev-end-time = IF AVAIL mach-calendar THEN mach-calendar.end-time ELSE 86400. /* 24 HRs*/

      IF string(bf-mch.end-time,"hh:mm:ss") > string(lv-prev-end-time,"hh:mm:ss") THEN DO:
                        lv-start-date = lv-start-date + 1.
                        lv-lap-time = bf-mch.end-time - lv-prev-end-time.
                        FIND FIRST mach-calendar WHERE mach-calendar.company = bfJob.company
                                   AND mach-calendar.m-code = bf-mch.m-code
                                   AND mach-calendar.m-date = lv-start-date 
                                   NO-LOCK NO-ERROR.
                        lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                                ELSE 28800. /* 8 HRs*/.
                        IF lv-m-time LT 0 THEN lv-m-time = 28800.
                        lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
                        ASSIGN bf-mch.end-time = lv-start-time + lv-lap-time             
                               bf-mch.end-date = lv-start-date
                               lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400.
     END.
  END.
  
  


