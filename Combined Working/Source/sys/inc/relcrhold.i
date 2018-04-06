/* ----------------------------------------- sys/inc/relcrhold.i 03/11/09 GDM */
/* CHECK CREDIT HOLD                                                          */
/* -------------------------------------------------------------------------- */

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
  IF AVAIL sys-ctrl THEN DO:
      
      ASSIGN v-chkflg = sys-ctrl.log-fld.

  END.
