/* oe-miscflds.i */

DEFINE VARIABLE miscFlds AS LOGICAL NO-UNDO.

RUN sysCtrlMiscFlds (OUTPUT miscFlds).
/*IF NOT miscFlds THEN
RUN hideMiscFlds IN h_miscflds.*/

PROCEDURE selectMiscFlds:

  /* If not available order line then abort. */
  IF NOT AVAILABLE oe-ordl THEN DO:
      MESSAGE "No order line is available."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  /* If order line job number is blank, then abort. */
  IF oe-ordl.job-no = "" THEN DO:
      MESSAGE "No job is available"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  /* Find the job of the order line. */
  FIND FIRST job NO-LOCK WHERE
      job.job-no = oe-ordl.job-no AND
      job.job-no2 = oe-ordl.job-no2 NO-ERROR.
  /* If job not found, then abort. */
  IF NOT AVAILABLE job THEN DO:
      MESSAGE "No job is available"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  RUN sysCtrlMiscFlds (OUTPUT miscFlds).
  IF NOT miscFlds THEN RETURN.

  FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
  IF AVAIL job-hdr THEN
  ASSIGN
      {&mfRecKey} = job-hdr.company + "|jh" + STRING(job-hdr.j-no)
      {&mfHeader} = " Job: " + job-hdr.job-no + "-" + STRING(job-hdr.job-no2) + " Item#: " + job-hdr.i-no.

  RUN nosweat/mfvalns.p (sys-ctrl.char-fld,{&mfRecKey},{&mfHeader},?).
END PROCEDURE.

PROCEDURE sysCtrlMiscFlds:
  DEFINE OUTPUT PARAMETER opMiscFlds AS LOGICAL NO-UNDO.

  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ {&sysCtrlCompany}
         AND sys-ctrl.name EQ '{&sysCtrlName}' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = {&sysCtrlCompany}
      sys-ctrl.name = '{&sysCtrlName}'
      sys-ctrl.log-fld = YES.

    IF sys-ctrl.NAME EQ "MISCJOBCL" THEN
       ASSIGN
       sys-ctrl.char-fld = "ASIJobCL"
       sys-ctrl.descrip = 'Job Checklist Misc Fields Group'.
  END.
  opMiscFlds = sys-ctrl.log-fld.
END PROCEDURE.
