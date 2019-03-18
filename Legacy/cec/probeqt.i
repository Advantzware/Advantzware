  /*cec/probeqt.i  Section Editor Size Limit */
  IF est.est-type EQ 8 THEN
  /*FOR EACH probeit
      WHERE probeit.company  EQ probe.company
        AND probeit.est-no   EQ probe.est-no
        AND probeit.line     EQ probe.line
      NO-LOCK,
      FIRST xprobe
      WHERE xprobe.company EQ probeit.company
        AND xprobe.est-no  EQ probeit.est-no
        AND xprobe.line    EQ probeit.line
        AND xprobe.do-quote
      BREAK BY probeit.part-no:

    IF LAST(probeit.part-no) THEN xprobe.do-quote = NO.*/
  FOR EACH xprobe
      WHERE xprobe.company EQ probe.company
        AND xprobe.est-no  EQ probe.est-no
        AND xprobe.do-quote,
      EACH probeit
      WHERE probeit.company  EQ xprobe.company
        AND probeit.est-no   EQ xprobe.est-no
        AND probeit.line     EQ xprobe.line
      NO-LOCK 
      BREAK BY xprobe.line
            BY probeit.part-no:

    IF LAST-OF(probeit.part-no) THEN xprobe.do-quote = NO.

    CREATE w-probeit.
    BUFFER-COPY probeit TO w-probeit.    

    FOR EACH xjob WHERE xjob.i-no EQ probeit.part-no,
        FIRST bf-eb
        WHERE bf-eb.company  EQ est.company
          AND bf-eb.est-no   EQ est.est-no
          AND bf-eb.form-no  EQ xjob.form-no
          AND bf-eb.blank-no EQ xjob.blank-no:

      ASSIGN
       w-probeit.mat-cost = w-probeit.mat-cost +
                            (xjob.mat * (bf-eb.bl-qty / 1000))
       w-probeit.lab-cost = w-probeit.lab-cost +
                            (xjob.lab * (bf-eb.bl-qty / 1000))
       w-probeit.fo-cost  = w-probeit.fo-cost  +
                            (xjob.foh * (bf-eb.bl-qty / 1000))
       w-probeit.vo-cost  = w-probeit.vo-cost  +
                            (xjob.voh * (bf-eb.bl-qty / 1000)).
    END.

    ASSIGN
     w-probeit.prof-on    = xprobe.prof-on
     w-probeit.mat-cost   = w-probeit.mat-cost / (w-probeit.bl-qty / 1000)
     w-probeit.lab-cost   = w-probeit.lab-cost / (w-probeit.bl-qty / 1000)
     w-probeit.vo-cost    = w-probeit.vo-cost  / (w-probeit.bl-qty / 1000)
     w-probeit.fo-cost    = w-probeit.fo-cost  / (w-probeit.bl-qty / 1000)
     w-probeit.tot-lbs    = xprobe.tot-lbs
     w-probeit.probe-date = xprobe.probe-date
     w-probeit.freight = IF AVAIL probeit THEN probeit.releaseCount
                                            ELSE xprobe.freight.



  END.

  ELSE
  FOR EACH xprobe
      WHERE xprobe.company  EQ est.company
        AND xprobe.est-no   EQ est.est-no
        AND xprobe.do-quote:

    ASSIGN
     xprobe.do-quote = NO
     i               = i + 1.

    FIND FIRST bf-eb
        WHERE bf-eb.company   EQ est.company 
          AND bf-eb.est-no    EQ est.est-no
          AND (bf-eb.form-no  NE 0 OR
               (bf-eb.form-no EQ 0 AND est.est-type EQ 6))
          AND bf-eb.blank-no  EQ IF est.est-type EQ 5 THEN 1
                                 ELSE
                                 IF est.est-type EQ 6 THEN 0
                                 ELSE i
        NO-LOCK NO-ERROR.

    IF AVAIL bf-eb THEN DO:
      CREATE w-probeit.
      ASSIGN
       w-probeit.company      = bf-eb.company
       w-probeit.est-no       = bf-eb.est-no
       w-probeit.cust-no      = bf-eb.cust-no
       w-probeit.part-no      = bf-eb.part-no
       w-probeit.bl-qty       = xprobe.est-qty
       w-probeit.sell-price   = xprobe.sell-price
       w-probeit.prof-on      = xprobe.prof-on
       w-probeit.net-profit   = xprobe.net-profit
       w-probeit.gross-profit = xprobe.gross-profit
       w-probeit.mat-cost     = xprobe.mat-cost
       w-probeit.lab-cost     = xprobe.lab-cost
       w-probeit.vo-cost      = xprobe.vo-cost
       w-probeit.fo-cost      = xprobe.fo-cost
       w-probeit.tot-lbs      = xprobe.tot-lbs
       w-probeit.freight      = xprobe.freight
       w-probeit.probe-date   = xprobe.probe-date.
    END.
  END.
