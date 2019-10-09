/* ----------------------------------------------- salrep/SlsMgmt.i   */
/*  ORIGINAL TASK 03090905                                            */
/* CREATES AN N-K-1 PARAMTER - SalesMgmt - TO GET ACCESS TO           */
/* MANAGEMENT REPORTS. THIS IS USED IN 5 DIFF PROGRAMS                */
/* ------------------------------------------------------------------ */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "SalesMgmt" NO-ERROR.
/*IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "SalesMgmt"
        sys-ctrl.log-fld = NO
        sys-ctrl.descrip = "Management Reports".
END.  */
/* gdm - 04/21 - taken out per request 03090905 - soluition tab */
/* ASSIGN v-runflg = sys-ctrl.log-fld.                          */
   ASSIGN v-runflg = YES.

RELEASE sys-ctrl.
