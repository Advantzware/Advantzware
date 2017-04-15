
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR oeprep-char AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/VAR.i new shared}

assign
 cocode = gcompany.

DO TRANSACTION:
   {sys/inc/oeprep.i}
END.

IF oeprep-char NE "OE & DC" THEN
   LEAVE.

FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job AND job.start-date NE ? AND INT(job.est-no) NE 0 THEN
FOR EACH eb
    WHERE eb.company EQ job.company
      AND eb.est-no  EQ job.est-no
      AND eb.form-no NE 0
      AND (eb.die-no NE "" OR eb.plate-no NE "")
    NO-LOCK:

  IF eb.die-no NE "" THEN
  FOR EACH prep
      WHERE prep.company EQ eb.company
        AND prep.code    EQ eb.die-no
      NO-LOCK:
    RUN update-prep.
  END.

  IF eb.plate-no NE "" THEN
  FOR EACH prep
      WHERE prep.company EQ eb.company
        AND prep.code    EQ eb.plate-no
      NO-LOCK:
    RUN update-prep.
  END.
END.

RETURN.

PROCEDURE update-prep.
  DEF BUFFER upd-prep FOR prep.
   
  FIND FIRST upd-prep WHERE ROWID(upd-prep) EQ ROWID(prep)
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

  IF AVAIL upd-prep THEN DO:
     upd-prep.last-date = job.start-date.

     /*FIND FIRST reftable WHERE
          reftable.reftable EQ "PREPLASTJOB" AND
          reftable.company  EQ upd-prep.company AND
          reftable.loc      EQ upd-prep.loc AND
          reftable.code     EQ upd-prep.CODE
          EXCLUSIVE-LOCK NO-ERROR.

     IF NOT AVAILABLE reftable THEN
     DO:
        CREATE reftable.
        ASSIGN
          reftable.reftable = "PREPLASTJOB"
          reftable.company  = upd-prep.company
          reftable.loc      = upd-prep.loc
          reftable.code     = upd-prep.CODE.
     END.

     ASSIGN
       reftable.code2    = job.job-no
       reftable.val[1]   = job.job-no2.

     RELEASE reftable.*/

     FIND CURRENT upd-prep NO-LOCK NO-ERROR.
  END.
END.

