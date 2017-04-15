
DEF PARAM BUFFER io-probe FOR probe.

DEF BUFFER b-probemk FOR reftable.
DEF BUFFER probe-ref FOR reftable.
    

IF AVAIL io-probe THEN DO:
  FIND FIRST probe-ref
      WHERE probe-ref.reftable EQ "probe-ref"
        AND probe-ref.company  EQ io-probe.company
        AND probe-ref.loc      EQ ""
        AND probe-ref.code     EQ io-probe.est-no
        AND probe-ref.code2    EQ STRING(io-probe.line,"9999999999")
      NO-ERROR.
  IF NOT AVAIL probe-ref THEN DO:
    CREATE probe-ref.
    ASSIGN
     probe-ref.reftable = "probe-ref"
     probe-ref.company  = io-probe.company
     probe-ref.loc      = ""
     probe-ref.code     = io-probe.est-no
     probe-ref.code2    = STRING(io-probe.line,"9999999999").
  END.
  ASSIGN
   probe-ref.val[6] = 0
   probe-ref.val[7] = 0
   probe-ref.val[8] = 0
   probe-ref.val[9] = 0.

  IF CAN-FIND(FIRST probeit
              WHERE probeit.company EQ io-probe.company
                AND probeit.est-no  EQ io-probe.est-no
                AND probeit.line    EQ io-probe.line) THEN
  FOR EACH probeit
      WHERE probeit.company EQ io-probe.company
        AND probeit.est-no  EQ io-probe.est-no
        AND probeit.line    EQ io-probe.line,
      FIRST b-probemk
      WHERE b-probemk.reftable EQ "ce/com/probemk.p"
        AND b-probemk.company  EQ probeit.company
        AND b-probemk.loc      EQ probeit.est-no
        AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
        AND b-probemk.code2    EQ probeit.part-no:

    ASSIGN
     b-probemk.val[6] = probeit.sell-price * b-probemk.val[2] / 100 *
                        (probeit.yld-qty / 1000)
     b-probemk.val[7] = probeit.sell-price * b-probemk.val[3] / 100 *
                        (probeit.yld-qty / 1000)
     b-probemk.val[8] = probeit.sell-price * b-probemk.val[4] / 100 *
                        (probeit.yld-qty / 1000)
     b-probemk.val[9] = probeit.sell-price * b-probemk.val[5] / 100 *
                        (probeit.yld-qty / 1000)

     probe-ref.val[6] = probe-ref.val[6] + b-probemk.val[6]
     probe-ref.val[7] = probe-ref.val[7] + b-probemk.val[7]
     probe-ref.val[8] = probe-ref.val[8] + b-probemk.val[8]
     probe-ref.val[9] = probe-ref.val[9] + b-probemk.val[9].
  END.

  ELSE
    ASSIGN
     probe-ref.val[6] = io-probe.sell-price * probe-ref.val[2] / 10000000 *
                        (io-probe.est-qty / 1000)
     probe-ref.val[7] = io-probe.sell-price * probe-ref.val[3] / 10000000 *
                        (io-probe.est-qty / 1000)
     probe-ref.val[8] = io-probe.sell-price * probe-ref.val[4] / 10000000 *
                        (io-probe.est-qty / 1000)
     probe-ref.val[9] = io-probe.sell-price * probe-ref.val[5] / 10000000 *
                        (io-probe.est-qty / 1000).
END.
