&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.
/* not delete if estimate exists */
FIND est WHERE est.rec_key = {&TABLENAME}.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
    {methods/triggers/delete.i}
END.
    


{sys/inc/var.i NEW SHARED}
ASSIGN
 cocode = {&TABLENAME}.company
 locode = {&TABLENAME}.loc.

/* moved to jc/v-{&TABLENAME}.w 
DEF VAR lv-msg AS CHAR NO-UNDO.


lv-msg = "".

IF {&TABLENAME}.stat EQ "A" THEN
  lv-msg = "This job has material allocated".

ELSE
IF {&TABLENAME}.stat EQ "W" then
  lv-msg = "This job has work-in-process".

ELSE
IF INDEX("CXZ", {&TABLENAME}.stat) GT 0 THEN
  lv-msg = "This job has been closed".

ELSE DO:
  FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ {&TABLENAME}.company
        AND oe-ordl.job-no  EQ {&TABLENAME}.job-no
        AND oe-ordl.job-no2 EQ {&TABLENAME}.job-no2
      NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN lv-msg = "An order exists with this Job#".
END.

IF lv-msg NE "" THEN DO:
  MESSAGE lv-msg + ", cannot delete..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.
*/
{jc/jc-dall.i}
               
FOR EACH job-hdr
    WHERE job-hdr.company EQ {&TABLENAME}.company
      AND job-hdr.job     EQ {&TABLENAME}.job
      AND job-hdr.job-no  EQ {&TABLENAME}.job-no
      AND job-hdr.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:            
  {util/dljobkey.i}

  DELETE job-hdr.
END.

FOR EACH job-mch
    WHERE job-mch.company EQ {&TABLENAME}.company
      AND job-mch.job     EQ {&TABLENAME}.job
      AND job-mch.job-no  EQ {&TABLENAME}.job-no
      AND job-mch.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:
  DELETE job-mch.
END.

FOR EACH job-mat
    WHERE job-mat.company EQ {&TABLENAME}.company
      AND job-mat.job     EQ {&TABLENAME}.job
      AND job-mat.job-no  EQ {&TABLENAME}.job-no
      AND job-mat.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:
  DELETE job-mat.
END.

FOR EACH job-prep
    WHERE job-prep.company EQ {&TABLENAME}.company
      AND job-prep.job     EQ {&TABLENAME}.job
      AND job-prep.job-no  EQ {&TABLENAME}.job-no
      AND job-prep.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:
  DELETE job-prep.
END.

FOR EACH job-farm-rctd 
    WHERE job-farm-rctd.company EQ {&TABLENAME}.company      
      AND job-farm-rctd.job-no  EQ {&TABLENAME}.job-no
      AND job-farm-rctd.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:
  DELETE job-farm-rctd.
END.

FOR EACH job-farm
      WHERE job-farm.company EQ {&TABLENAME}.company      
      AND job-farm.job-no  EQ {&TABLENAME}.job-no
      AND job-farm.job-no2 EQ {&TABLENAME}.job-no2
    EXCLUSIVE-LOCK:

END.

FOR EACH reftable
    WHERE reftable.reftable EQ "jc/jc-calc.p"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ STRING({&TABLENAME}.job,"999999999")
    EXCLUSIVE-LOCK:
  DELETE reftable.
END.



FOR EACH reftable
    WHERE reftable.reftable EQ "job.qty-changed"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ STRING({&TABLENAME}.job,"9999999999")
    EXCLUSIVE-LOCK:
  DELETE reftable.
END.

         
IF {&TABLENAME}.exported THEN DO:
  {&TABLENAME}.stat = "X".
  {jc/kiwiexp4.i}
END.
