/*sys/util/j-reckey.p */

 FOR EACH job :
    FIND FIRST est WHERE est.company = job.company
                     AND est.est-no = job.est-no NO-LOCK NO-ERROR.
    IF AVAIL est AND est.rec_key <> job.rec_key
         THEN
        job.rec_key = est.rec_key.

    /*    DISP est.rec_key job.rec_key est.est-no 
             est.est-date est.est-type.
             */
END.

FOR EACH pc-prdd :
    /* for notes - same as job and estimate */
    FIND FIRST job WHERE job.company = pc-prdd.company
                     AND job.job-no = pc-prdd.job-no
                     AND job.job-no2 = pc-prdd.job-no2
                     NO-LOCK NO-ERROR.
    IF AVAIL job THEN pc-prdd.rec_key = job.rec_key.


END.
