
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


SESSION:SET-WAIT-STATE("general").

ASSIGN
 cocode = g_company
 locode = g_loc.

FOR EACH oe-ord WHERE oe-ord.company EQ cocode,

    FIRST cust
    WHERE cust.company EQ oe-ord.company
      AND cust.cust-no EQ oe-ord.cust-no
    NO-LOCK:

  RELEASE eb.

  ASSIGN
   oe-ord.sman[1]   = cust.sman
   oe-ord.s-pct[1]  = 0
   oe-ord.s-comm[1] = 0
   oe-ord.sman[2]   = ""
   oe-ord.s-pct[2]  = 0
   oe-ord.s-comm[2] = 0
   oe-ord.sman[3]   = ""
   oe-ord.s-pct[3]  = 0
   oe-ord.s-comm[3] = 0.

  IF oe-ord.est-no NE "" THEN
  FIND FIRST eb
      WHERE eb.company EQ oe-ord.company
        AND eb.est-no  EQ oe-ord.est-no
        AND eb.cust-no EQ oe-ord.cust-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.
  IF AVAIL eb THEN
    ASSIGN
     oe-ord.sman[1]   = eb.sman
     oe-ord.s-comm[1] = eb.comm.

  FIND FIRST sman
      WHERE sman.company EQ oe-ord.company
        AND sman.sman    EQ oe-ord.sman[1]
      NO-LOCK NO-ERROR.
  IF AVAIL sman THEN DO:
    oe-ord.sname[1] = sman.sname.
    IF NOT AVAIL eb THEN oe-ord.s-comm[1] = sman.scomm.
  END.

  IF oe-ord.sman[1] NE "" THEN oe-ord.s-pct[1] = 100.00.

  FOR EACH oe-ordl OF oe-ord,
      FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK:

    RELEASE eb.

    DO i = 1 TO 3:
      ASSIGN
       oe-ordl.s-man[i]  = oe-ord.sman[i]
       oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
       oe-ordl.s-comm[i] = oe-ord.s-comm[i].
    END.

    IF oe-ordl.est-no NE "" AND oe-ord.est-no EQ "" THEN
    FIND FIRST eb
        WHERE eb.company EQ cocode
          AND eb.est-no  EQ oe-ordl.est-no
          AND eb.cust-no EQ oe-ord.cust-no
          AND eb.form-no NE 0
        NO-LOCK NO-ERROR.

    IF AVAIL eb THEN
      ASSIGN
       oe-ordl.s-man[1]  = eb.sman
       oe-ordl.s-comm[1] = eb.comm.

    IF oe-ordl.s-man[1] NE "" THEN oe-ordl.s-pct[1] = 100.00.
  END.
END.

FOR EACH ar-inv WHERE ar-inv.company EQ cocode,
    FIRST cust
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no
    NO-LOCK:

  ar-inv.t-comm = 0.

  FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no,
      FIRST itemfg
      WHERE itemfg.company EQ ar-invl.company
        AND itemfg.i-no    EQ ar-invl.i-no
      NO-LOCK:

    IF ar-invl.ord-no NE 0 THEN
    FOR FIRST oe-ordl
        WHERE oe-ordl.company EQ ar-invl.company
          AND oe-ordl.ord-no  EQ ar-invl.ord-no
          AND oe-ordl.i-no    EQ ar-invl.i-no
        NO-LOCK:
      
      ASSIGN
       ar-invl.sman[1]   = oe-ordl.s-man[1]
       ar-invl.sman[2]   = oe-ordl.s-man[2]
       ar-invl.sman[3]   = oe-ordl.s-man[3]
       ar-invl.s-pct[1]  = oe-ordl.s-pct[1]
       ar-invl.s-pct[2]  = oe-ordl.s-pct[2]
       ar-invl.s-pct[3]  = oe-ordl.s-pct[3]
       ar-invl.s-comm[1] = oe-ordl.s-comm[1]
       ar-invl.s-comm[2] = oe-ordl.s-comm[2]
       ar-invl.s-comm[3] = oe-ordl.s-comm[3]
       ar-invl.sname[1]  = ""
       ar-invl.sname[2]  = ""
       ar-invl.sname[3]  = "".
    END.

    DO i = 1 TO 3:
      FIND FIRST sman
          WHERE sman.company EQ ar-invl.company
            AND sman.sman    EQ ar-invl.sman[i]
          NO-LOCK NO-ERROR.

      IF AVAIL sman AND ar-invl.sman[i] NE "" THEN DO:
        ar-invl.sname[i] = sman.sname.

        RUN custom/combasis.p (cocode, ar-invl.sman[i], cust.type, itemfg.procat, 0,
                               cust.cust-no,
                               OUTPUT ar-invl.s-commbasis[i]).

        IF ar-invl.s-commbasis[i] EQ "G" THEN
          ar-inv.t-comm = ar-inv.t-comm +
                          ROUND(((ar-invl.amt - ar-invl.t-cost) * ar-invl.s-comm[i]) / 100,2).

        ELSE
          ar-inv.t-comm = ar-inv.t-comm +
                          ROUND((((ar-invl.amt * ar-invl.s-pct[i]) / 100) * ar-invl.s-comm[i]) / 100,2).
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE("").
