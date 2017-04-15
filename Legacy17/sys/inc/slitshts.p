
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-form-no LIKE mch-act.frm NO-UNDO.
DEF OUTPUT PARAM op-qty AS INT NO-UNDO.


{sys/inc/var.i SHARED}

FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.
    
IF AVAIL job THEN
FOR EACH mch-act
    WHERE mch-act.company EQ cocode
      AND mch-act.job     EQ job.job
      AND mch-act.job-no  EQ job.job-no
      AND mch-act.job-no2 EQ job.job-no2
      AND mch-act.frm     EQ ip-form-no
      AND mch-act.dept    EQ "RC"
      AND mch-act.pass    LE 1
    NO-LOCK:

  op-qty = op-qty + (mch-act.waste + mch-act.qty).
END.
