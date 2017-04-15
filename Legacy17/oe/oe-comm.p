/* ---------------------------------------------------- oe/oe-comm.p 3/96 fwk */
/* commission calculation                                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/VAR.i SHARED}

DEF VAR v-tot-cost LIKE oe-ord.t-cost.
DEF VAR f AS INT NO-UNDO.
DEF VAR v-assign-comm AS LOG INIT NO NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis NO-UNDO.

DEF SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.

DEF SHARED BUFFER xoe-ord FOR oe-ord.
DEF SHARED BUFFER xest    FOR est.
DEF SHARED BUFFER xef     FOR ef.
DEF SHARED BUFFER xeb     FOR eb.


FIND FIRST oe-ord WHERE ROWID(oe-ord) EQ ROWID(xoe-ord) EXCLUSIVE.

IF AVAIL oe-ord THEN DO:
  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ oe-ord.company NO-LOCK NO-ERROR.

  FIND FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK NO-ERROR.

  oe-ord.t-comm = 0.

  FOR EACH oe-ordl OF oe-ord NO-LOCK:
    v-tot-cost = (oe-ordl.qty / 1000) * oe-ordl.cost.

    FIND FIRST itemfg
        WHERE itemfg.company EQ oe-ordl.company
          AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.
    DO k = 1 TO 3:
      IF oe-ordl.s-man[k] NE "" THEN DO:
	    RUN custom/combasis.p (oe-ord.company, oe-ordl.s-man[k],
                               (IF AVAIL cust THEN cust.TYPE ELSE ""),
                               (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                               (IF AVAIL cust THEN cust.cust-no ELSE ""),
                               OUTPUT v-basis).

        oe-ord.t-comm = oe-ord.t-comm +
			            ROUND((oe-ordl.t-price - IF v-basis EQ "G" THEN v-tot-cost ELSE 0) *
			                  (oe-ordl.s-pct[k] / 100) * (oe-ordl.s-comm[k] / 100),2).
      END.
    END.
  END.

  IF oe-ctrl.prep-comm THEN
  FOR EACH oe-ordm OF oe-ord NO-LOCK:
    FIND FIRST prep
        WHERE prep.company EQ oe-ordm.company
          AND prep.code    EQ oe-ordm.charge
        NO-LOCK NO-ERROR.
    DO k = 1 TO 3:
      IF oe-ordm.s-man[k] NE "" THEN DO:
	    RUN custom/combasis.p (oe-ord.company, oe-ordm.s-man[k],
                               (IF AVAIL cust THEN cust.TYPE ELSE ""),
                               (IF AVAIL prep THEN prep.fgcat ELSE ""), 0,
                               (IF AVAIL cust THEN cust.cust-no ELSE ""),
                               OUTPUT v-basis).

        oe-ord.t-comm = oe-ord.t-comm +
			            ROUND((oe-ordm.amt - IF v-basis EQ "G" THEN oe-ordm.cost ELSE 0) *
			                  (oe-ordm.s-pct[k] / 100) * (oe-ordm.s-comm[k] / 100),2).
      END.
    END.
  END.
END.

FIND CURRENT xoe-ord NO-LOCK NO-ERROR.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

