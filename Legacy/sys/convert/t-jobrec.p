/* need to run to reset job.rec_key for notes */

OUTPUT TO c:\tmp\job_rec.data.
PUT "**** Job table recid and rec_key value ********" SKIP.
FOR EACH job NO-LOCK:
   PUT RECID(job) " " job.rec_key SKIP.
END.
OUTPUT CLOSE.

FOR EACH job WHERE est-no <> "".
    DISP job.rec_key est-no.
    FIND FIRST est WHERE est.company = job.company AND
                        est.est-no = job.est-no
           NO-LOCK NO-ERROR.
    IF AVAIL est THEN do:
       DISP est.rec_key.
       job.rec_key = est.rec_key.
    END.
    FIND FIRST notes WHERE notes.rec_key = job.rec_key NO-LOCK NO-ERROR.
    IF AVAIL notes THEN DISP note_title FORM "x(10)".
    PAUSE 0.
END.



