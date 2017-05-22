DEF BUFFER bf-job-hdr FOR job-hdr.
DEF VAR v-next-job LIKE job-hdr.j-no NO-UNDO.
DEF VAR v-job-no LIKE job.job-no NO-UNDO.
DEF VAR v-job-no2 LIKE job.job-no2 NO-UNDO.
DEF VAR v-company LIKE job.company NO-UNDO.


DISABLE TRIGGERS FOR LOAD of job-hdr.

v-company = '001'.
UPDATE v-job-no LABEL "Job #" v-job-no2 LABEL "Seq" v-company LABEL "Company"
    WITH 1 COLUMN TITLE "Press F2 to continue".
FIND job WHERE job.job-no = v-job-no
           AND job.job-no2 = v-job-no2
           AND job.company = v-company
         NO-LOCK NO-ERROR.

IF AVAIL job THEN DO:
    FIND LAST bf-job-hdr WHERE bf-job-hdr.company = bf-job-hdr.company
                   USE-INDEX j-no
                   NO-LOCK NO-ERROR.
    IF AVAIL(bf-job-hdr) AND bf-job-hdr.j-no > 1 THEN DO:
      v-next-job = bf-job-hdr.j-no + 1.
    END.
    ELSE DO:
       FIND LAST bf-job-hdr WHERE bf-job-hdr.company = '001'
                               AND bf-job-hdr.j-no >= 8888888
                             NO-LOCK NO-ERROR.
       IF AVAIL bf-job-hdr THEN
          v-next-job = bf-job-hdr.j-no + 1.
       ELSE
          v-next-job = 8888888.
    END.
    CREATE job-hdr.
    ASSIGN
      job-hdr.company = job.company
      job-hdr.j-no = v-next-job
      job-hdr.job     = job.job
      job-hdr.job-no  = job.job-no
      job-hdr.job-no2 = job.job-no2
      job-hdr.opened  = YES.

    RELEASE job-hdr.
    MESSAGE "Problem corrected." VIEW-AS ALERT-BOX.
END.
ELSE 
    MESSAGE "Job-hdr not found." VIEW-AS ALERT-BOX.
