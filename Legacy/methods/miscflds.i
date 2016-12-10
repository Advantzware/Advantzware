/* miscflds.i */

DEFINE VARIABLE miscFlds AS LOGICAL NO-UNDO.

RUN sysCtrlMiscFlds (OUTPUT miscFlds).
/*IF NOT miscFlds THEN
RUN hideMiscFlds IN h_miscflds.*/

PROCEDURE selectMiscFlds:
  DEFINE VARIABLE isRunning AS LOGICAL NO-UNDO.

  IF NOT AVAILABLE job THEN DO:
      MESSAGE "No job is available"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  RUN sysCtrlMiscFlds (OUTPUT miscFlds).
  IF NOT miscFlds THEN RETURN.
  
  RUN Running_Procedures IN Persistent-Handle ("mfvalues.",OUTPUT isRunning).
  RUN UDF/mfvalues.w PERSISTENT (sys-ctrl.char-fld,{&mfRecKey},{&mfHeader},?).
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
