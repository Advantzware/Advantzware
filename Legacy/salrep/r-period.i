  
  DEF VAR ld AS DATE EXTENT 3 NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER b-tt-report FOR tt-report.

  ASSIGN
   li = YEAR(as-of-date)
   ld = as-of-date.

  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE as-of-date
        AND period.pend    GE as-of-date
      NO-LOCK NO-ERROR.
  IF AVAIL period THEN
    ASSIGN               
     li    = period.yr
     ld[1] = period.pst
     ld[2] = period.pst.

  IF tb_ytd THEN
  FOR EACH period
      WHERE period.company EQ cocode
        AND period.yr      EQ li
      NO-LOCK
      BY period.pst:
    ld[1] = period.pst.
    LEAVE.
  END.

  if slct-by-inv then do:
    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.posted   eq yes
          and ar-inv.inv-date ge ld[1]
          and ar-inv.inv-date le ld[3]
          and (ar-inv.type    ne "FC" or v-inc-fc)
        use-index inv-date no-lock,

        FIRST cust
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK,

        each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
          and (ar-invl.billable or not ar-invl.misc)
        no-lock:
          
      do i = 1 to 3:
        v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                    else ar-invl.sman[i].

        if v-sman-no  lt fsman                          or
           v-sman-no  gt tsman                          or
           (i ne 1 and
            (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

        create tt-report.
        assign
         tt-report.term-id  = ""
         tt-report.key-08   = v-sman-no
         tt-report.key-09   = cust.cust-no
         tt-report.key-10   = "ar-invl"
         tt-report.rec-id   = recid(ar-invl)
         tt-report.ytd-only = ar-inv.inv-date LT ld[2].
      end.
    end.

    for each cust where cust.company eq cocode no-lock,

        each ar-cash
        where ar-cash.company    eq cocode
          and ar-cash.cust-no    eq cust.cust-no
          and ar-cash.posted     eq yes
          and ar-cash.check-date ge ld[1]
          and ar-cash.check-date le ld[3]
        use-index ar-cash no-lock,

        EACH ar-cashl
        WHERE ar-cashl.c-no    EQ ar-cash.c-no
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.memo    EQ YES
          AND CAN-FIND(FIRST account
                       WHERE account.company EQ ar-cashl.company
                         AND account.actnum  EQ ar-cashl.actnum
                         AND account.type    EQ "R")
        NO-LOCK:

      RELEASE tt-report.
      RELEASE ar-invl.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      if avail oe-retl then 
      find first ar-invl
          where ar-invl.company eq ar-cashl.company
            and ar-invl.cust-no eq cust.cust-no
            and ar-invl.inv-no  eq ar-cashl.inv-no
            and ar-invl.i-no    eq oe-retl.i-no
            and (ar-invl.billable or not ar-invl.misc)
          USE-INDEX inv-no no-lock no-error.

      IF ar-cashl.inv-no NE 0                                                       AND
         (AVAIL ar-invl                             OR
          (NOT AVAIL reftable AND
           NOT ar-cashl.dscr MATCHES "*oe return*") OR
          SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
      FOR EACH b-ar-invl
          WHERE b-ar-invl.company EQ ar-cashl.company
            AND b-ar-invl.cust-no EQ cust.cust-no
            AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
            AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
            AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
          USE-INDEX inv-no NO-LOCK:

        do i = 1 to 3:
          v-sman-no = if b-ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                      else b-ar-invl.sman[i].
             
          if v-sman-no  lt fsman                          or
             v-sman-no  gt tsman                          or
             (i ne 1 and
              (v-sman-no eq "" or b-ar-invl.s-pct[i] eq 0)) then next.

          create tt-report.
          assign
           tt-report.term-id  = ""
           tt-report.key-08   = v-sman-no
           tt-report.key-09   = cust.cust-no
           tt-report.key-10   = "ar-cashl"
           tt-report.rec-id   = recid(ar-cashl)
           tt-report.row-id   = ROWID(b-ar-invl)
           tt-report.ytd-only = ar-cash.check-date LT ld[2].
        end.
      END.

      else
      if cust.sman ge fsman and
         cust.sman le tsman then do:

        create tt-report.
        assign
         tt-report.term-id  = ""
         tt-report.key-08   = cust.sman
         tt-report.key-09   = cust.cust-no
         tt-report.key-10   = "ar-cashl"
         tt-report.rec-id   = recid(ar-cashl)
         tt-report.ytd-only = ar-cash.check-date LT ld[2].
      end.
    end.
  end.

  else do:
    for each cust where cust.company eq cocode no-lock:
      for each ar-inv
          where ar-inv.company  eq cocode
            and ar-inv.posted   eq yes
            and ar-inv.cust-no  eq cust.cust-no
          no-lock,

          first ar-ledger
          where ar-ledger.company  eq cocode
            and ar-ledger.cust-no  eq ar-inv.cust-no
            and ar-ledger.ref-date eq ar-inv.inv-date
            and ar-ledger.ref-num  eq "INV# " + string(ar-inv.inv-no)
            and ar-ledger.tr-date  ge ld[1]
            and ar-ledger.tr-date  le ld[3]
          no-lock,

          each ar-invl
          where ar-invl.x-no eq ar-inv.x-no
            and (ar-invl.billable or not ar-invl.misc)
          no-lock:
          
        do i = 1 to 3:
          v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                      else ar-invl.sman[i].

          if v-sman-no  lt fsman                          or
             v-sman-no  gt tsman                          or
             (i ne 1 and
              (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

          create tt-report.
          assign
           tt-report.term-id  = ""
           tt-report.key-08   = v-sman-no
           tt-report.key-09   = cust.cust-no
           tt-report.key-10   = "ar-invl"
           tt-report.rec-id   = recid(ar-invl)
           tt-report.ytd-only = ar-ledger.tr-date LT ld[2].
        end.
      end.

      for each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.posted     eq yes
          no-lock,

          first ar-ledger
          where ar-ledger.company  eq cocode
            and ar-ledger.cust-no  eq ar-cash.cust-no
            and ar-ledger.ref-date eq ar-cash.check-date
            and ar-ledger.ref-num  eq "Memo#" +
                                    string(ar-cash.check-no,"99999999") + "A/R"
            and ar-ledger.tr-date  ge ld[1]
            and ar-ledger.tr-date  le ld[3]
          no-lock,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
                         WHERE account.company EQ ar-cashl.company
                           AND account.actnum  EQ ar-cashl.actnum
                           AND account.type    EQ "R")
          NO-LOCK:

        RELEASE tt-report.
        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        if avail oe-retl then 
        find first ar-invl
            where ar-invl.company eq ar-cashl.company
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            USE-INDEX inv-status no-lock no-error.

        IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
            (NOT AVAIL reftable AND
             NOT ar-cashl.dscr MATCHES "*oe return*") OR
            SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
        FOR EACH b-ar-invl
            WHERE b-ar-invl.company EQ ar-cashl.company
              AND b-ar-invl.cust-no EQ cust.cust-no
              AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
              AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
              AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
            USE-INDEX inv-status NO-LOCK:

          do i = 1 to 3:
            v-sman-no = if b-ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else b-ar-invl.sman[i].

            if v-sman-no  lt fsman                          or
               v-sman-no  gt tsman                          or
               (i ne 1 and
                (v-sman-no eq "" or b-ar-invl.s-pct[i] eq 0)) then next.

            create tt-report.
            assign
             tt-report.term-id  = ""
             tt-report.key-08   = v-sman-no
             tt-report.key-09   = cust.cust-no
             tt-report.key-10   = "ar-cashl"
             tt-report.rec-id   = recid(ar-cashl)
             tt-report.row-id   = ROWID(b-ar-invl)
             tt-report.ytd-only = ar-ledger.tr-date LT ld[2].
          end.
        END.

        else
        if cust.sman ge fsman and
           cust.sman le tsman then do:

          create tt-report.
          assign
           tt-report.term-id  = ""
           tt-report.key-08   = cust.sman
           tt-report.key-09   = cust.cust-no
           tt-report.key-10   = "ar-cashl"
           tt-report.rec-id   = recid(ar-cashl)
           tt-report.ytd-only = ar-ledger.tr-date LT ld[2].
        end.
      end.
    end.
  end.

  for each tt-report
      where tt-report.term-id eq ""
        and tt-report.key-01  eq ""
        and tt-report.key-02  eq ""
        and tt-report.key-03  eq ""
        and tt-report.key-04  eq ""
        and tt-report.key-05  eq ""
        and tt-report.key-06  eq ""
        and tt-report.key-07  eq "":

    if tt-report.key-10 eq "ar-invl" then do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
      find ar-inv  where ar-inv.x-no    eq ar-invl.x-no  no-lock.
      {sa/sa-mtdsl.i "ar-invl" "ar-inv.inv-date"}
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
      {sa/sa-mtdsl.i "ar-cashl" "ar-cash.check-date"}
    end.
  end.

  if not v-summ then
  for each tt-report
      where tt-report.term-id  EQ ""
        AND tt-report.ytd-only EQ NO,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock
      
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04

      with STREAM-IO width 132:
      
    find first w-inv no-error.  
      
    if not avail w-inv then do:
      create w-inv.
      assign
       w-inv.cust-no = cust.cust-no
       w-inv.name    = cust.name.
    end.
    
    if tt-report.key-10 eq "ar-invl" then do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
      find ar-inv  where ar-inv.x-no    eq ar-invl.x-no  no-lock.

      find first ar-ledger
          where ar-ledger.company  eq cocode
            and ar-ledger.cust-no  eq ar-inv.cust-no
            and ar-ledger.ref-date eq ar-inv.inv-date
            and ar-ledger.ref-num  eq "INV# " + string(ar-inv.inv-no)
          no-lock no-error.

      assign
       w-inv.inv-no   = ar-inv.inv-no
       w-inv.inv-date = ar-inv.inv-date
       w-inv.pst-date = if avail ar-ledger then ar-ledger.tr-date
                        else ar-inv.inv-date.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          no-lock no-error.
          
      assign
       v-pct  = 1
       v-amt  = ar-invl.amt
       v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                else
                if avail itemfg then
                  (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

      if v-sqft eq ? then v-sqft = 0.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-08 then
          assign
           v-pct = ar-invl.s-pct[i] / 100
           i     = 3.
      end.

      if v-pct eq 0 then
      do i = 1 to 3:
        if i eq 1 then j = 0.
        if ar-invl.sman[i] ne "" then j = j + 1.
        if i eq 3 then v-pct = 1 / j.
      end.

      if v-pct le 0 or v-pct eq ? then v-pct = 1.

      IF FIRST-OF(tt-report.key-04) THEN
          dPerSales = v-pct .
      ELSE dPerSales = dPerSales + v-pct .
      
      IF LAST-OF(tt-report.key-04) THEN DO:
          IF dPerSales GT 1 THEN
              ASSIGN dPerSales = 1 .
       w-inv.amt = w-inv.amt + (v-amt  * dPerSales) .
       w-inv.msf = w-inv.msf + (v-sqft * dPerSales).
       END.

     /* assign
       w-inv.amt = w-inv.amt + (v-amt  * v-pct)
       w-inv.msf = w-inv.msf + (v-sqft * v-pct).*/
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      find first ar-ledger
          where ar-ledger.company  eq cocode
            and ar-ledger.cust-no  eq ar-cash.cust-no
            and ar-ledger.ref-date eq ar-cash.check-date
            and ar-ledger.ref-num  eq "Memo#" +
                                    string(ar-cash.check-no,"99999999") + "A/R"
          no-lock no-error.

      assign
       w-inv.inv-no   = ar-cashl.inv-no
       w-inv.inv-date = ar-cash.check-date
       w-inv.pst-date = if avail ar-ledger then ar-ledger.tr-date
                        else ar-cash.check-date.
       
      assign
       v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
       v-sqft = 0
       v-pct  = 1.

      RELEASE ar-invl.
      RELEASE oe-retl.

      FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

      /*IF NOT AVAIL ar-invl THEN*/
      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      IF AVAIL oe-retl THEN DO:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq oe-retl.i-no
            no-lock no-error.

        v-sqft = IF AVAIL itemfg THEN
                   (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                 ELSE 0.

        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq ar-cash.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
            no-lock no-error.

        if avail ar-invl and ar-invl.amt-msf ne 0 then v-sqft = ar-invl.amt-msf.      
      END.
      ELSE
      IF AVAIL ar-invl AND
        /* gdm - 07060907 */
         TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)) NE "ITEMS"
         THEN DO:
        
        ld-inv-pct = 0.
        FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
          ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
          ACCUMULATE 1 (TOTAL).
        END.

        ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                        (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                     ELSE (ACCUM TOTAL 1))
                     ELSE (ar-invl.amt / ld-inv-pct).

        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.
        
        v-amt = v-amt * ld-inv-pct.

        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report.key-08 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

        if v-pct eq 0 then
        do i = 1 to 3:
          if i eq 1 then j = 0.
          if ar-invl.sman[i] ne "" then j = j + 1.
          if i eq 3 then v-pct = 1 / j.
        end.

        if v-pct le 0 or v-pct eq ? then v-pct = 1.
      end.

      assign
       w-inv.amt = w-inv.amt + (v-amt  * v-pct)
       w-inv.msf = w-inv.msf - (v-sqft * v-pct).
    end.

    form xskip no-label.

    if last-of(tt-report.key-03) then do:
      ld-amt-msf = w-inv.amt / w-inv.msf.
      IF ld-amt-msf EQ ? THEN ld-amt-msf = 0.

      display w-inv.inv-no label "Inv #"
              w-inv.inv-date
              w-inv.pst-date
              w-inv.cust-no
              w-inv.name
              w-inv.amt
              w-inv.msf
              ld-amt-msf label "$/MSF" format "->,>>>,>>9.99".

     IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              w-inv.inv-no 
              w-inv.inv-date 
              w-inv.pst-date 
              w-inv.cust-no 
              w-inv.NAME  
              w-inv.amt 
              w-inv.msf 
              ld-amt-msf  
              SKIP.    
      assign
       v-tot = v-tot + w-inv.amt
       v-msf = v-msf + w-inv.msf.

      if sort-by-inv then do:
        xskip = "".
        if linv = 0 then linv = w-inv.inv-no.
        else if linv + 1 ne w-inv.inv-no then xskip = "*".
        display xskip.
        linv = w-inv.inv-no.
      end.

      delete w-inv.
    end.   
  end. 

  ld-amt-msf = v-tot / v-msf.
  IF ld-amt-msf EQ ? THEN ld-amt-msf = 0.

  if not v-summ then
    put "-------------" to 86
        "-------------" to 100
        "-------------" to 114
        skip

        "Total"       to 61
        v-tot         to 86
        v-msf         to 100
        ld-amt-msf    to 114 format "->>,>>>,>>9.99"
        skip(2).

  RELEASE tt-report.

  for each tt-report
      where tt-report.term-id eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock:

    tt-report.key-01 = cust.name.

  end.

  RELEASE tt-report.

  for each tt-report
      where tt-report.term-id eq ""
        AND CAN-FIND(FIRST b-tt-report
                     WHERE b-tt-report.term-id  EQ ""
                       AND b-tt-report.key-09   EQ tt-report.key-09
                       AND b-tt-report.ytd-only EQ NO
                     USE-INDEX ytd-only)

      break by tt-report.key-01
            by tt-report.key-03
            by tt-report.key-04

      with title "SUMMARY":

    find first w-sum no-error.

    if not avail w-sum then do:
      create w-sum.
      assign
       w-sum.cust-no = tt-report.key-09
       w-sum.name    = tt-report.key-01.
    end.

    if tt-report.key-10 eq "ar-invl" then do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
      find ar-inv  where ar-inv.x-no    eq ar-invl.x-no  no-lock.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          no-lock no-error.
          
      assign
       v-pct  = 1
       v-amt  = ar-invl.amt
       v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                else
                if avail itemfg then
                  (itemfg.t-sqft * ar-invl.ship-qty / 1000)
                else 0
       v-wght = if ar-invl.t-weight ne 0 then ar-invl.t-weight
                else
                if avail itemfg then
                  (ar-invl.ship-qty * itemfg.weight-100 / 100)
                else 0.

      if v-sqft eq ? then v-sqft = 0.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-08 then
          assign
           v-pct = ar-invl.s-pct[i] / 100
           i     = 3.
      end.

      if v-pct eq 0 then
      do i = 1 to 3:
        if i eq 1 then j = 0.
        if ar-invl.sman[i] ne "" then j = j + 1.
        if i eq 3 then v-pct = 1 / j.
      end.

      if v-pct le 0 or v-pct eq ? then v-pct = 1.

      IF FIRST-OF(tt-report.key-04) THEN
          dPerSales = v-pct .
      ELSE dPerSales = dPerSales + v-pct .
      
        IF LAST-OF(tt-report.key-04) THEN DO:
            IF dPerSales GT 1 THEN
                ASSIGN dPerSales = 1 .
           w-sum.amt-y = w-sum.amt-y + (v-amt * dPerSales).
          IF tt-report.ytd-only EQ NO THEN DO:
            assign
                w-sum.amt = w-sum.amt + (v-amt  * dPerSales)
                w-sum.msf = w-sum.msf + (v-sqft * dPerSales)
                w-sum.wgt = w-sum.wgt + (v-wght * dPerSales).
          END.
      END.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
      
      assign
       v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
       v-sqft = 0
       v-pct  = 1.

      RELEASE ar-invl.
      RELEASE oe-retl.

      FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

      /*IF NOT AVAIL ar-invl THEN*/
        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      IF AVAIL oe-retl THEN DO:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq oe-retl.i-no
            no-lock no-error.

        assign
         v-sqft = if avail itemfg then
                    (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                  else 0
         v-wght = if avail itemfg then
                    (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                  else 0.

        if v-sqft eq ? then v-sqft = 0.
        if v-wght eq ? then v-wght = 0.

        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq ar-cash.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
            USE-INDEX inv-no no-lock no-error.

        if avail ar-invl then do:
          if ar-invl.amt-msf ne 0 then v-sqft = ar-invl.amt-msf.
          if ar-invl.t-weight ne 0 then v-wght = ar-invl.t-weight.
        end.
      END.

      ELSE
      IF AVAIL ar-invl THEN DO:
        ld-inv-pct = 0.
        FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
          ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
          ACCUMULATE 1 (TOTAL).
        END.
        ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                        (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                     ELSE (ACCUM TOTAL 1))
                     ELSE (ar-invl.amt / ld-inv-pct).

        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

        v-amt = v-amt * ld-inv-pct.

        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report.key-08 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

        if v-pct eq 0 then
        do i = 1 to 3:
          if i eq 1 then j = 0.
          if ar-invl.sman[i] ne "" then j = j + 1.
          if i eq 3 then v-pct = 1 / j.
        end.

        if v-pct le 0 or v-pct eq ? then v-pct = 1.
      end.

      w-sum.amt-y = w-sum.amt-y + (v-amt * v-pct).

      IF tt-report.ytd-only EQ NO THEN
         ASSIGN
          w-sum.amt = w-sum.amt + (v-amt  * v-pct)
          w-sum.msf = w-sum.msf - (v-sqft * v-pct)
          w-sum.wgt = w-sum.wgt - (v-wght * v-pct).
    end.

    if last-of(tt-report.key-01) then do:
      ASSIGN
       ld-amt-msf = w-sum.amt / w-sum.msf
       ld-wgt-msf = w-sum.wgt / w-sum.msf.

      IF ld-amt-msf EQ ? THEN ld-amt-msf = 0.
      IF ld-wgt-msf EQ ? THEN ld-wgt-msf = 0.

      if tb_ytd then
        display w-sum.name w-sum.cust-no w-sum.amt w-sum.msf
                ld-amt-msf format "->,>>>,>>9.99" label "$/MSF"
                ld-wgt-msf format "->,>>>,>>9.99" label "Wgt/MSF"
                w-sum.amt-y

            with no-box no-attr-space STREAM-IO width 132 frame summary1 down.

      else
        display w-sum.name w-sum.cust-no w-sum.amt w-sum.msf
                ld-amt-msf format "->,>>>,>>9.99" label "$/MSF"
                ld-wgt-msf format "->,>>>,>>9.99" label "Wgt/MSF"

            with no-box no-attr-space STREAM-IO width 132 frame summary2 down.
    
    IF tb_excel THEN DO: 
        IF tb_ytd THEN
            EXPORT STREAM excel-2 DELIMITER ","
            w-sum.NAME 
            w-sum.cust-no 
            w-sum.amt 
            w-sum.msf 
            ld-amt-msf 
            ld-wgt-msf
            w-sum.amt-y
            SKIP.
        ELSE
            EXPORT STREAM excel-2 DELIMITER ","
            w-sum.NAME 
            w-sum.cust-no 
            w-sum.amt 
            w-sum.msf 
            ld-amt-msf 
            ld-wgt-msf
            SKIP.

    END.

      assign
       v-tot-sum-tot = v-tot-sum-tot + w-sum.amt
       v-tot-sum-msf = v-tot-sum-msf + w-sum.msf
       v-tot-sum-wgt = v-tot-sum-wgt + w-sum.wgt
       v-tot-sum-tot-y = v-tot-sum-tot-y + w-sum.amt-y.

      delete w-sum.
    end.

    delete tt-report.
  end.

  put "-------------"    to 54
      "-------------"    to 68
      "-------------"    to 82
      "-------------"    to 96.

  if tb_ytd then put "-----------------" to 114.

  ASSIGN
   ld-amt-msf = v-tot-sum-tot / v-tot-sum-msf
   ld-wgt-msf = v-tot-sum-wgt / v-tot-sum-msf.

  IF ld-amt-msf EQ ? THEN ld-amt-msf = 0.
  IF ld-wgt-msf EQ ? THEN ld-wgt-msf = 0.

  PUT SKIP
      "TOTALS"      to 20
      v-tot-sum-tot to 54
      v-tot-sum-msf to 68
      ld-amt-msf    to 82 format "->>,>>>,>>9.99"
      ld-wgt-msf    to 96 format "->>,>>>,>>9.99".

  if tb_ytd then put v-tot-sum-tot-y to 114.

  put skip.

