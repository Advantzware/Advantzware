/* copyjob.p */

DEFINE INPUT PARAMETER ipCopyRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipJobRowID AS ROWID NO-UNDO.

DEFINE OUTPUT PARAMETER opJobHdrRowID AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE VARIABLE jobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE jNo AS INTEGER NO-UNDO.
DEFINE VARIABLE noDate AS LOGICAL NO-UNDO.

DEFINE BUFFER b1Job FOR job.
DEFINE BUFFER b2Job FOR job.
DEFINE BUFFER bJobHdr FOR job-hdr.
DEFINE BUFFER bJobMch FOR job-mch.
DEFINE BUFFER bJobMat FOR job-mat.
DEFINE BUFFER bJobPrep FOR job-prep.

noDate = CAN-FIND(FIRST sys-ctrl
                  WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name EQ 'Schedule'
                    AND sys-ctrl.char-fld EQ 'NoDate'
                    AND sys-ctrl.log-fld EQ YES).

FIND b1Job NO-LOCK WHERE ROWID(b1Job) EQ ipCopyRowID NO-ERROR.
IF NOT AVAILABLE b1Job THEN RETURN.
FIND job EXCLUSIVE-LOCK WHERE ROWID(job) EQ ipJobRowID NO-ERROR.
IF NOT AVAILABLE job THEN RETURN.

FIND LAST b2Job NO-LOCK WHERE b2Job.company EQ b1Job.company
                          AND b2Job.job-no EQ b1Job.job-no
                          AND ROWID(b2job) NE ROWID(job).
jobNo2 = (IF AVAIL b2job THEN b2Job.job-no2 ELSE 0) + 1.
BUFFER-COPY b1Job EXCEPT job start-date due-date TO job
  ASSIGN job.job-no = b1Job.job-no
         job.job-no2 = jobNo2
         job.stat = "R".

FOR EACH bJobHdr OF b1Job NO-LOCK:
  CREATE job-hdr.
  BUFFER-COPY bJobHdr EXCEPT job job-no job-no2 j-no rec_key TO job-hdr
    ASSIGN job-hdr.job = job.job
           job-hdr.job-no = job.job-no
           job-hdr.job-no2 = job.job-no2.
  IF job.due-date EQ ? AND noDate THEN
  job-hdr.due-date = ?.
  RUN util/upditmfg.p (ROWID(job-hdr),1).
  opJobHdrRowID = ROWID(job-hdr).
END. /* each bjobhdr */

FOR EACH bJobMch NO-LOCK
    WHERE bJobMch.company EQ b1Job.company
      AND bJobMch.job EQ b1Job.job
      AND bJobMch.job-no EQ b1Job.job-no
      AND bJobMch.job-no2 EQ b1Job.job-no2:
  CREATE job-mch.
  BUFFER-COPY bJobMch EXCEPT rec_key TO job-mch
    ASSIGN job-mch.job = job.job
           job-mch.job-no = job.job-no
           job-mch.job-no2 = job.job-no2
           job-mch.j-no = 0.
  IF job.start-date EQ ? AND noDate THEN
  ASSIGN
    job-mch.end-date = ?
    job-mch.end-date-su = ?
    job-mch.end-time = 0
    job-mch.end-time-su = 0
    job-mch.start-date = ?
    job-mch.start-date-su = ?
    job-mch.start-time = 0
    job-mch.start-time-su = 0
    job-mch.start-seq-no = 0.
END.

FOR EACH bJobMat NO-LOCK
    WHERE bJobMat.company EQ b1Job.company
      AND bJobMat.job EQ b1Job.job
      AND bJobMat.job-no EQ b1Job.job-no
      AND bJobMat.job-no2 EQ b1Job.job-no2:
  CREATE job-mat.
  BUFFER-COPY bJobMat EXCEPT rec_key TO job-mat
    ASSIGN job-mat.job = job.job
           job-mat.job-no = job.job-no
           job-mat.job-no2 = job.job-no2
           job-mat.qty-all = job-mat.qty
           job-mat.all-flg = NO
           job-mat.j-no = 0.
END.

FOR EACH bJobPrep NO-LOCK
    WHERE bJobPrep.company EQ b1Job.company
      AND bJobPrep.job EQ b1Job.job
      AND bJobPrep.job-no EQ b1Job.job-no
      AND bJobPrep.job-no2 EQ b1Job.job-no2:
  CREATE job-prep.
  BUFFER-COPY bJobPrep EXCEPT rec_key TO job-prep
    ASSIGN job-prep.job = job.job
           job-prep.job-no = job.job-no
           job-prep.job-no2 = job.job-no2.
END.

