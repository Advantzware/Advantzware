
FOR EACH company NO-LOCK,
    EACH job
    WHERE job.company EQ company.company
      AND job.opened  EQ YES
      AND (CAN-FIND(FIRST job-mat
                    WHERE job-mat.company EQ job.company
                      AND job-mat.job     EQ job.job
                      AND job-mat.job-no  EQ job.job-no
                      AND job-mat.job-no2 EQ job.job-no2
                      AND job-mat.qty     LT 0) OR
           CAN-FIND(FIRST job-mch
                    WHERE job-mch.company EQ job.company
                      AND job-mch.job     EQ job.job
                      AND job-mch.job-no  EQ job.job-no
                      AND job-mch.job-no2 EQ job.job-no2
                      AND job-mch.run-qty LT 0)):

  DISPLAY job.company       FORMAT "x(10)"      COLUMN-LABEL "Company"
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")
                            FORMAT "x(10)"      COLUMN-LABEL "Job#".  
END.

MESSAGE "Program complete..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

HIDE ALL NO-PAUSE.
