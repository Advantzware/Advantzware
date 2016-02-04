FIND FIRST po-ord NO-LOCK
     WHERE po-ord.company EQ '001'
       AND po-ord.po-no EQ 100301 NO-ERROR.
FOR EACH po-ordl WHERE
    po-ordl.company EQ '001' AND
    po-ordl.po-no EQ po-ord.po-no NO-LOCK:
  IF NOT po-ordl.item-type THEN NEXT.
  FIND FIRST item OF po-ordl NO-LOCK NO-ERROR.
  IF NOT AVAILABLE item OR item.mat-type NE 'B' THEN NEXT.
  FIND FIRST job NO-LOCK WHERE job.company EQ po-ordl.company
                           AND job.job-no EQ po-ordl.job-no
                           AND job.job-no2 EQ po-ordl.job-no2 NO-ERROR.
  IF AVAILABLE job THEN DO:
    FIND FIRST est NO-LOCK WHERE est.company EQ job.company
                             AND est.est-no EQ job.est-no NO-ERROR.
    IF AVAILABLE est AND est.est-type GE 5 THEN DO:
      FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ job.company
                                 AND job-mat.job EQ job.job
                                 AND job-mat.job-no EQ job.job-no
                                 AND job-mat.job-no2 EQ job.job-no2
                                 AND job-mat.i-no EQ po-ordl.i-no
                                 AND job-mat.frm EQ po-ordl.s-num:
        DISP job-mat WITH 1 COL.
      END. /* each job-mat */
    END. /* avail est */
  END. /* avail job */
END. /* each po-ordl */
