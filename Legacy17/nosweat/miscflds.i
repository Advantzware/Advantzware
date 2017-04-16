/* miscflds.i */

PROCEDURE selectMiscFlds :
  IF NOT AVAILABLE job THEN RETURN.

  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ {&sysCtrlCompany}
         AND sys-ctrl.name EQ '{&sysCtrlName}' NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = {&sysCtrlCompany}
      sys-ctrl.name = '{&sysCtrlName}'.

      IF sys-ctrl.name EQ "MISCJOBCL" THEN
      sys-ctrl.descrip = 'Job Checklist Misc Fields Group'.
    
    RETURN.
  END.
  IF NOT sys-ctrl.log-fld THEN RETURN.
  RUN nosweat/mfvalns.p(sys-ctrl.char-fld,{&mfRecKey},{&mfHeader}).
  
END PROCEDURE.
