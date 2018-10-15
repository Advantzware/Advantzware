  
  DEF VAR lv-quotes AS CHAR NO-UNDO.
  DEF VAR v-loop-date AS DATE NO-UNDO.
  DEF VAR v-tot-retn AS INT INIT 0 NO-UNDO.
  DEF BUFFER io-oe-retl FOR oe-retl.

  lv-quotes = CHR(34).

  DEF BUFFER b-invl FOR ar-invl.
  
  IF fcust NE tcust AND fitem EQ titem THEN
  DO:
    for EACH ar-invl WHERE
         ar-invl.company EQ cocode AND
         ar-invl.i-no EQ fitem
         NO-LOCK,
         FIRST ar-inv WHERE
               ar-inv.x-no EQ ar-invl.x-no AND
               ar-inv.posted   eq YES AND
               ar-inv.cust-no  GE fcust and
               ar-inv.cust-no  LE tcust and 
               (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no and
               ttCustList.log-fld no-lock) else true) and

               ar-inv.inv-date GE fdate AND
               ar-inv.inv-date LE tdate
               AND NOT (NOT v-inc-fc AND ar-inv.type EQ "FC")
               NO-LOCK
               BREAK BY ar-invl.x-no:
/* (Need to include this in the 'for each' to do a break by */
/*         IF NOT v-inc-fc AND ar-inv.type EQ "FC" THEN
            NEXT.    */

         /* Records per item created in section below */
         IF FIRST-OF(ar-invl.x-no) THEN DO:
           create tt-report.
           assign
            tt-report.key-09  = ar-inv.cust-no
            tt-report.key-10  = "ar-inv"
            tt-report.rec-id  = recid(ar-inv).
           RELEASE tt-report.
         END.
     END. /*each ar-invl*/
     for each cust WHERE
         cust.company eq cocode 
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)         
         NO-LOCK,
         each ar-cash FIELDS(c-no cust-no) WHERE
             ar-cash.company    eq cocode AND
             ar-cash.cust-no    eq cust.cust-no AND
             ar-cash.check-date GE fdate AND
             ar-cash.check-date LE tdate AND
             ar-cash.posted     eq yes
             no-lock
             USE-INDEX ar-cash,
             EACH ar-cashl WHERE
                  ar-cashl.c-no    EQ ar-cash.c-no AND
                  ar-cashl.posted  EQ YES AND
                  ar-cashl.memo    EQ YES AND
                  CAN-FIND(FIRST account
                           WHERE account.company EQ ar-cashl.company
                             AND account.actnum  EQ ar-cashl.actnum
                             AND account.type    EQ "R")
             NO-LOCK:
      
             create tt-report.
             assign
             tt-report.key-09  = ar-cash.cust-no
             tt-report.key-10  = "ar-cashl"
             tt-report.rec-id  = recid(ar-cashl).
             RELEASE tt-report.
     END. /*each cust*/
  END. /*end fcust NE tcust AND fitem EQ titem*/

  ELSE IF fcust NE tcust THEN
  DO:
     IF fship EQ "" AND tship BEGINS "zzzzz" AND
        not(fitem EQ "" AND titem BEGINS "zzzzzzzz") THEN
        DO:
           for EACH itemfg FIELDS(i-no) WHERE
               itemfg.company EQ cocode AND
               itemfg.i-no GE fitem AND
               itemfg.i-no LE titem
               NO-LOCK,
               EACH ar-invl WHERE
               ar-invl.company EQ cocode AND
               ar-invl.i-no EQ itemfg.i-no
               NO-LOCK,
               FIRST ar-inv WHERE
                     ar-inv.x-no     EQ ar-invl.x-no AND
                     ar-inv.posted   eq YES AND
                     ar-inv.cust-no  GE fcust and
                     ar-inv.cust-no  LE tcust and
                     (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no and 
                     ttCustList.log-fld no-lock) else true) and
                     ar-inv.inv-date GE fdate AND
                     ar-inv.inv-date LE tdate AND
                     NOT (NOT v-inc-fc AND ar-inv.type EQ "FC")
                     NO-LOCK
                     BREAK BY ar-invl.x-no:

               {custom/statusMsg.i " 'Processing Customer#  '  + ar-inv.cust-no "}

               IF NOT v-inc-fc AND ar-inv.type EQ "FC" THEN
                  NEXT.
               IF FIRST-OF(ar-invl.x-no) THEN DO:
                 /* This gets split out to items in a section below */
                 create tt-report.
                 assign
                   tt-report.key-09  = ar-inv.cust-no
                   tt-report.key-10  = "ar-inv"
                   tt-report.rec-id  = recid(ar-inv).
                 RELEASE tt-report.
               END.
           END. /*end each itemfg*/
         
           for each cust WHERE
               cust.company eq cocode 
               AND cust.cust-no GE fcust
               AND cust.cust-no LE tcust
               AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
               AND ttCustList.log-fld no-lock) else true)               
               NO-LOCK,
               each ar-cash FIELDS(c-no cust-no) WHERE
                   ar-cash.company    eq cocode AND
                   ar-cash.cust-no    eq cust.cust-no AND
                   ar-cash.check-date GE fdate AND
                   ar-cash.check-date LE tdate AND
                   ar-cash.posted     eq yes
                   no-lock
                   USE-INDEX ar-cash,
               EACH ar-cashl WHERE
                    ar-cashl.c-no    EQ ar-cash.c-no AND
                    ar-cashl.posted  EQ YES AND
                    ar-cashl.memo    EQ YES AND
                    CAN-FIND(FIRST account WHERE
                    account.company EQ ar-cashl.company AND
                    account.actnum  EQ ar-cashl.actnum AND
                    account.type    EQ "R")
                    NO-LOCK:          
                    CREATE tt-report.
                    assign
                     tt-report.key-09  = ar-cash.cust-no
                     tt-report.key-10  = "ar-cashl"
                     tt-report.rec-id  = recid(ar-cashl).
                    RELEASE tt-report.
           END. /*end cust*/
        END.
     ELSE
     DO:
        DO v-loop-date = fdate TO tdate:
           for each ar-inv
               where ar-inv.company  eq cocode
                 and ar-inv.posted   eq yes
                 AND ar-inv.cust-no  GE fcust
                 AND ar-inv.cust-no  LE tcust
                 AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
                 AND ttCustList.log-fld no-lock) else true)
                 and ar-inv.inv-date EQ v-loop-date
                 AND NOT (NOT v-inc-fc AND ar-inv.type EQ "FC")
                 USE-INDEX inv-date
               NO-LOCK
               BREAK BY ar-inv.X-no:
          
/*               IF NOT v-inc-fc AND ar-inv.type EQ "FC" THEN
                  NEXT. */
               IF FIRST-OF(ar-inv.x-no) THEN DO:            
                 create tt-report.
                 assign
                  tt-report.key-09  = ar-inv.cust-no
                  tt-report.key-10  = "ar-inv"
                  tt-report.rec-id  = recid(ar-inv).
                 RELEASE tt-report.
               END.

           end.
        END.
        for each cust
            where cust.company eq cocode
              AND cust.cust-no GE fcust
              AND cust.cust-no LE tcust
              AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
              AND ttCustList.log-fld no-lock) else true)
            NO-LOCK,
            each ar-cash FIELDS(c-no cust-no) WHERE
                ar-cash.company    eq cocode AND
                ar-cash.cust-no    eq cust.cust-no AND
                ar-cash.check-date GE fdate AND
                ar-cash.check-date LE tdate AND
                ar-cash.posted     eq yes
                no-lock
                USE-INDEX ar-cash,
                EACH ar-cashl WHERE
                     ar-cashl.c-no    EQ ar-cash.c-no AND
                     ar-cashl.posted  EQ YES AND
                     ar-cashl.memo    EQ YES AND
                CAN-FIND(FIRST account WHERE
                account.company EQ ar-cashl.company AND
                account.actnum  EQ ar-cashl.actnum AND
                account.type    EQ "R")
                NO-LOCK:     
                {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
                create tt-report.
                assign
                tt-report.key-09  = ar-cash.cust-no
                tt-report.key-10  = "ar-cashl"
                tt-report.rec-id  = recid(ar-cashl).
                RELEASE tt-report.
        end. /*end cust*/
     END.
  END. /*fcust NE tcust*/

  ELSE /*fcust eq tcust*/
  DO:
     DO v-loop-date = fdate TO tdate:
        for each ar-inv
            where ar-inv.company  eq cocode
              and ar-inv.posted   eq yes
              AND ar-inv.cust-no  GE fcust
              AND ar-inv.cust-no  LE tcust
              AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
              AND ttCustList.log-fld no-lock) else true)
              and ar-inv.inv-date EQ v-loop-date
              AND NOT (NOT v-inc-fc AND ar-inv.type EQ "FC")
            no-lock
            USE-INDEX inv-date
            BREAK BY ar-inv.x-no:
       
/*            IF NOT v-inc-fc AND ar-inv.type EQ "FC" THEN
               NEXT.                                      */            
            IF FIRST-OF(ar-inv.x-no) THEN DO:
              create tt-report.
              assign
               tt-report.key-09  = ar-inv.cust-no
               tt-report.key-10  = "ar-inv"
               tt-report.rec-id  = recid(ar-inv).
              RELEASE tt-report.
            END.
        end. /*end each ar-inv*/
     END. /*end do v-loop-date*/
       
     for each ar-cash FIELDS(cust-no c-no) WHERE
         ar-cash.company    eq cocode AND
         ar-cash.cust-no  GE fcust and
         ar-cash.cust-no  LE tcust and 
         (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-cash.cust-no and
         ttCustList.log-fld no-lock) else true) and 
         ar-cash.check-date GE fdate AND
         ar-cash.check-date LE tdate AND
         ar-cash.posted     eq yes
         no-lock
         USE-INDEX ar-cash,
         EACH ar-cashl WHERE
              ar-cashl.c-no    EQ ar-cash.c-no AND
              ar-cashl.posted  EQ YES AND
              ar-cashl.memo    EQ YES AND
              CAN-FIND(FIRST account WHERE
                       account.company EQ ar-cashl.company AND
                       account.actnum  EQ ar-cashl.actnum AND
                       account.type    EQ "R")
         NO-LOCK:   
         create tt-report.
         assign
         tt-report.key-09  = ar-cash.cust-no
         tt-report.key-10  = "ar-cashl"
         tt-report.rec-id  = recid(ar-cashl).
         RELEASE tt-report.
     end. /*each ar-cash*/
     
  END. /*end fcust eq tcust*/

  for each tt-report
      where tt-report.term-id EQ ""
        AND tt-report.key-01  eq ""
        and tt-report.key-02  eq ""
        and tt-report.key-03  eq ""
        and tt-report.key-04  eq ""
        and tt-report.key-05  eq ""
        and tt-report.key-05  eq ""
        and tt-report.key-07  eq ""
        and tt-report.key-08  eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    if tt-report.key-10 eq "ar-inv" then do:
       find FIRST ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.
      
       v-ship = if ar-inv.ship-id ne "" then ar-inv.ship-id else
                if ar-inv.sold-id ne "" then ar-inv.sold-id else
                                             ar-inv.cust-no.
      
       if v-ship ge fship and
          v-ship le tship then
       for each ar-invl
           where ar-invl.x-no    eq ar-inv.x-no
             and ar-invl.i-no    ge fitem
             and ar-invl.i-no    le titem
           no-lock:
         
         IF NOT(ar-invl.billable or not ar-invl.misc) THEN NEXT.
      
         do i = 1 to 3:
           v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                       else ar-invl.sman[i].
      
           if v-sman-no  lt fsman                          or
              v-sman-no  gt tsman                          or
              (i ne 1 and
               (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
      
           IF tgChooseSalesReps AND LOOKUP(v-sman-no, cSlsList) EQ 0 THEN
             NEXT.
           create xtt-report.
      
           assign
            xtt-report.rec-id  = recid(ar-invl)
            xtt-report.key-01  = if ar-invl.misc then ar-invl.i-name else
                                 if ar-invl.i-no ne "" then ar-invl.i-no else
                                 "AR SALE"
            xtt-report.key-07 =  if ar-invl.i-no ne "" then ar-invl.i-no ELSE "AR SALE"
            xtt-report.key-02  = tt-report.key-09
            xtt-report.key-03  = v-ship
            xtt-report.key-04  = string(ar-invl.inv-no,"999999")
            xtt-report.key-05  = v-sman-no
            xtt-report.key-09  = tt-report.key-09.          
           if not v-sort1 then do:
             find first itemfg
                 where itemfg.company eq cocode
                   and itemfg.i-no    eq xtt-report.key-01
                 no-lock no-error.
             assign
              xtt-report.key-02 = if avail itemfg then itemfg.part-no
                                                  else xtt-report.key-01
              xtt-report.key-01 = xtt-report.key-09.
           end.
           LEAVE.
         end.
       end.
      
       delete tt-report.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find FIRST ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find FIRST ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
      assign
       v-exc            = yes
       tt-report.key-01 = "MEMO"
       tt-report.key-02 = tt-report.key-09
       tt-report.key-03 = tt-report.key-09
       tt-report.key-04 = string(ar-cashl.inv-no,"999999")
       tt-report.key-05 = cust.sman.

      RELEASE oe-reth.
      RELEASE ar-inv.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

      ASSIGN
       lv-r-no = 0
       lv-type = "".
          
      IF AVAIL reftable THEN
        ASSIGN
         lv-r-no = reftable.val[1]
         lv-type = reftable.dscr.

      IF lv-r-no EQ 0 AND ar-cashl.dscr MATCHES "*OE RETURN*" THEN
        ASSIGN
         lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
         lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

      IF lv-r-no NE 0 THEN
      FIND FIRST oe-reth
          WHERE oe-reth.company EQ cocode
            AND oe-reth.r-no    EQ lv-r-no
          NO-LOCK NO-ERROR.       

      lv-inv-no = IF AVAIL oe-reth THEN oe-reth.inv-no ELSE ar-cashl.inv-no.

      IF lv-inv-no NE 0 THEN
      find first ar-inv
           where ar-inv.company eq cocode
             and ar-inv.cust-no eq ar-cash.cust-no
             and ar-inv.inv-no  eq lv-inv-no
           no-lock no-error.

      v-ship = IF AVAIL ar-inv then
                 if ar-inv.ship-id ne "" then ar-inv.ship-id else
                 if ar-inv.sold-id ne "" then ar-inv.sold-id else
                                              ar-inv.cust-no
               ELSE ar-cash.cust-no.

      IF lv-type NE "" OR lv-inv-no NE 0       AND
         (v-ship GE fship AND v-ship LE tship) THEN DO:

        IF lv-type EQ "items"                 OR
           (lv-inv-no NE 0 AND lv-type EQ "") THEN DO:
          RELEASE oe-retl.

          IF AVAIL oe-reth THEN
          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq oe-reth.r-no
                and oe-retl.line    eq ar-cashl.line
                and oe-retl.i-no    ge fitem
                and oe-retl.i-no    le titem
              no-lock no-error.
          
          FOR EACH ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq lv-inv-no
                and ((AVAIL oe-retl AND
                      ar-invl.i-no  EQ oe-retl.i-no) OR
                     (NOT AVAIL oe-retl AND
                      ar-invl.i-no  GE fitem AND
                      ar-invl.i-no  LE titem))
                and (ar-invl.billable or not ar-invl.misc)
              no-lock
              BREAK BY ar-invl.i-no:
            do i = 1 to 3:
              v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                          else ar-invl.sman[i].

              if v-sman-no  lt fsman                          or
                 v-sman-no  gt tsman                          or
                 (i ne 1 and
                  (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
              
              IF tgChooseSalesReps AND LOOKUP(v-sman-no, cSlsList) EQ 0 THEN
                NEXT.
              create xtt-report.

              assign
               v-exc              = no
               xtt-report.rec-id  = recid(ar-cashl)
               xtt-report.rec-id2 = recid(ar-invl)
               xtt-report.key-01  = if ar-invl.misc then ar-invl.i-name else
                                    if ar-invl.i-no ne "" then ar-invl.i-no else
                                    "AR SALE"
               xtt-report.key-07  = if ar-invl.i-no ne "" then ar-invl.i-no ELSE "AR SALE" 
               xtt-report.key-02  = tt-report.key-09
               xtt-report.key-03  = v-ship       
               xtt-report.key-04  = tt-report.key-04
               xtt-report.key-05  = v-sman-no
               xtt-report.key-09  = tt-report.key-09.
               
              if not v-sort1 then do:
                find first itemfg
                    where itemfg.company eq cocode
                      and itemfg.i-no    eq xtt-report.key-01
                    no-lock no-error.
                assign
                 xtt-report.key-02 = if avail itemfg then itemfg.part-no
                                                     else xtt-report.key-01
                 xtt-report.key-01 = xtt-report.key-09.
              end.
              LEAVE.
            end.

            IF LAST(ar-invl.i-no) AND AVAIL tt-report THEN delete tt-report.
          end.
        end.

        else
        if lv-type   eq "freight"                  and
           "freight" ge fitem                      and
           "freight" le titem                      and
           cust.sman ge fsman                      and
           cust.sman le tsman                      then
          assign
           v-exc         = no
           tt-report.key-01 = "FREIGHT"
           tt-report.key-03 = v-ship.

        else
        if lv-type   eq "tax"                  and
           "tax"     ge fitem                  and
           "tax"     le titem                  and
           cust.sman ge fsman                  and
           cust.sman le tsman                  then
          assign
           v-exc         = no
           tt-report.key-01 = "TAX"
           tt-report.key-03 = v-ship.

        else
        if ""        ge fitem and
           ""        le titem and
           cust.sman ge fsman and
           cust.sman le tsman then v-exc = no.
      end.

      else
      if ""               ge fitem and
         ""               le titem and
         cust.sman        ge fsman and
         cust.sman        le tsman and
         ar-cashl.cust-no ge fship and
         ar-cashl.cust-no le tship then v-exc = no.
         
      if v-exc AND AVAIL tt-report then delete tt-report.
      
      else
      if avail tt-report and not v-sort1 then do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq tt-report.key-01
            no-lock no-error.
        assign
         tt-report.key-02 = if avail itemfg then itemfg.part-no
                                            else tt-report.key-01
         tt-report.key-01 = tt-report.key-09 .
      end.
    end.
  end.

  for each tt-report,
      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      NO-LOCK,
      FIRST itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq tt-report.key-07
          NO-LOCK

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05

      with frame itemx down:
    {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    create w-data.
    assign
     w-data.i-no    = if v-sort1 then tt-report.key-01 else tt-report.key-02
     w-data.inv-no  = int(tt-report.key-04)
     w-data.rec-id  = tt-report.rec-id.
     
    find first ar-invl
        where recid(ar-invl) eq w-data.rec-id
        no-lock no-error.

    if avail ar-invl then do:
      RELEASE oe-ordl.
      IF ar-invl.ord-no NE 0 THEN
      FIND FIRST oe-ordl NO-LOCK 
          WHERE oe-ordl.company EQ ar-invl.company
            AND oe-ordl.ord-no  EQ ar-invl.ord-no
            AND oe-ordl.i-no    EQ ar-invl.i-no
          NO-ERROR.

      FIND FIRST ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

      assign
       v-date   = ar-inv.inv-date
       v-ord    = ar-invl.ord-no
       v-est    = if avail oe-ordl then oe-ordl.est-no else ar-invl.est-no
       v-bol    = ar-invl.bol-no
       v-pric   = ar-invl.unit-pr
       v-uom    = ar-invl.pr-uom
       v-qty[1] = ar-invl.ship-qty
       v-amt[1] = ar-invl.amt
       v-pct    = 1.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-05 then
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

      v-amt[1] = v-amt[1] * v-pct.
    end.

    else do:
      find first ar-cashl
          where recid(ar-cashl) eq w-data.rec-id
          no-lock no-error.

      if avail ar-cashl then do:
        find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.
        FIND FIRST ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id2 NO-LOCK NO-ERROR.

        assign
         v-date   = ar-cash.check-date
         v-ord    = 0
         v-est    = ""
         v-bol    = 0
         v-pric   = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-tot-retn = 0     .

        if AVAIL ar-invl then do:
          RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

          IF AVAIL oe-retl THEN
          FOR EACH io-oe-retl
              WHERE io-oe-retl.company EQ ar-cashl.company
              AND io-oe-retl.r-no    EQ oe-retl.r-no
              /*  AND io-oe-retl.line    EQ ar-cashl.line*/
              NO-LOCK:

              v-tot-retn = v-tot-retn + io-oe-retl.tot-qty-return .
          END.

          IF AVAIL oe-retl THEN
            v-qty[1] = - v-tot-retn /*oe-retl.tot-qty-return*/ .

          ELSE DO:
            FIND FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK NO-ERROR.
            lv-tot-amt = IF AVAIL ar-inv THEN
                           (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0) + ar-inv.tax-amt
                         ELSE 0.

            FOR EACH b-invl FIELDS(amt billable misc company cust-no inv-no)
                WHERE b-invl.company EQ cocode
                  AND b-invl.cust-no EQ ar-cash.cust-no
                  AND b-invl.inv-no  EQ ar-cashl.inv-no
                NO-LOCK:

              IF NOT(b-invl.billable OR NOT b-invl.misc) THEN NEXT.

              lv-tot-amt = lv-tot-amt + b-invl.amt.
            END.
            v-amt[1] = v-amt[1] * (ar-invl.amt / lv-tot-amt).
          END.

          RELEASE oe-ordl.
          IF ar-invl.ord-no NE 0 THEN
          FIND FIRST oe-ordl NO-LOCK 
              WHERE oe-ordl.company EQ ar-invl.company
                AND oe-ordl.ord-no  EQ ar-invl.ord-no
                AND oe-ordl.i-no    EQ ar-invl.i-no
              NO-ERROR.
                
          /* Added for decimal problem */
          assign
           v-ord  = ar-invl.ord-no
           v-est  = if avail oe-ordl then oe-ordl.est-no else ar-invl.est-no
           v-bol  = ar-invl.bol-no
           v-pric = ar-invl.unit-pr
           v-uom  = IF AVAIL oe-retl THEN oe-retl.uom ELSE ar-invl.pr-qty-uom.
              
          do i = 1 to 3:
            if ar-invl.sman[i] eq tt-report.key-05 then
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

          v-amt[1] = v-amt[1] * v-pct.
        end.
      end.
    end.

    if v-print1 then v-name = cust.name.

    else do:
      /*find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.*/
      v-name = if avail itemfg then itemfg.i-name else w-data.i-no.
    end.

    if v-det then do:
      display w-data.i-no   when first-of(tt-report.key-01) or not v-sort1
              cust.cust-no        
              v-name        when ((not v-print1) and first-of(tt-report.key-01))
                                 or v-print1
              tt-report.key-03
              w-data.inv-no
              v-date
              v-ord
              v-est
                STRING(v-bol,">>>>>>>>") WHEN rd_show2 EQ "BOL#" @ v-est
              v-qty[1]
              v-pric
              v-uom
              v-amt[1].

      down.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' IF first-of(tt-report.key-01) or not v-sort1 THEN
                    REPLACE(w-data.i-no,lv-quotes,"") ELSE ""                            '",'
             '"' cust.cust-no                                      '",'
             '"' IF ((not v-print1) and first-of(tt-report.key-01))
                    or v-print1 THEN v-name ELSE ""                '",'
             '"' tt-report.key-03                                  '",'
             '"' w-data.inv-no                                     '",'
             '"' STRING(v-date)                                    '",'
             '"' STRING(v-ord,">>>>>>")                            '",'
             '"' IF rd_show2 EQ "BOL#" THEN STRING(v-bol,">>>>>>>>")
                 ELSE v-est                                        '",'
             '"' STRING(v-qty[1],"->>>,>>>,>>>")                   '",'
             '"' STRING(v-pric,"->,>>>,>>9.99<<")                  '",'
             '"' v-uom                                             '",'
             '"' STRING(v-amt[1],"->,>>>,>>>,>>9.99")              '",'
             SKIP.
    end.

    else
    if (first-of(tt-report.key-01) and v-sort1)     or
       (first-of(tt-report.key-02) and not v-sort1) then
    DO:
      display w-data.i-no
              v-name .

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' w-data.i-no                    '",'
             '"' v-name                         '",' .
    END.

    assign
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1].

    if last-of(tt-report.key-03) then do:
      if v-det then do:  
        IF NOT FIRST-OF(tt-report.key-03) THEN do:
        underline v-name v-qty[1] v-amt[1].

        display "      SHIP-TO TOTALS"  @ v-name
                v-qty[2] @ v-qty[1]
                v-amt[2] @ v-amt[1].

        down.
        put skip(1).

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' "SHIP-TO TOTALS"                     '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[2],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[2],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).
        END.  /* NOT FIRST-OF(tt-report.key-03)*/
      end.
      
      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-amt[3] = v-amt[3] + v-amt[2]

       v-qty[2] = 0
       v-amt[2] = 0.
    end.

    if v-det then do:
        IF FIRST-OF(tt-report.key-02) AND LAST-OF(tt-report.key-02)  AND NOT LAST(tt-report.key-01) THEN
            PUT SKIP(1) .
    END.

    if last-of(tt-report.key-02) then do:
      if v-det then do:
        IF NOT FIRST-OF(tt-report.key-02) THEN do:
        underline v-name v-qty[1] v-amt[1].

        display "     CUSTOMER TOTALS"                  @ v-name
                "      PART NO TOTALS" when not v-sort1 @ v-name
                v-qty[3] @ v-qty[1]
                v-amt[3] @ v-amt[1].

        down.
        put skip(1).

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' IF not v-sort1 THEN "PART NO TOTALS"
                   ELSE "CUSTOMER TOTALS"               '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[3],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[3],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).
        END.  /* NOT FIRST-OF(tt-report.key-02)*/
      end.
      
      else
      if not v-sort1 then do:
        display v-qty[3] @ v-qty[1]
                v-amt[3] @ v-amt[1].
        down.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[3],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[3],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).
      end.

      assign
       v-qty[4] = v-qty[4] + v-qty[3]
       v-amt[4] = v-amt[4] + v-amt[3]

       v-qty[3] = 0
       v-amt[3] = 0.
    end.

    if last-of(tt-report.key-01) then do:
      if v-det then do:
        IF NOT FIRST-OF(tt-report.key-01) THEN DO:
        underline v-name v-qty[1] v-amt[1].

        display "         ITEM TOTALS"                  @ v-name
                "     CUSTOMER TOTALS" when not v-sort1 @ v-name
                v-qty[4] @ v-qty[1]
                v-amt[4] @ v-amt[1].

        down.
        put skip(1).

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' IF not v-sort1 THEN "CUSTOMER TOTALS"
                   ELSE "ITEM TOTALS"                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[4],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[4],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).
        END. /* NOT FIRST-OF(tt-report.key-01) */
      end.

      else
      if v-sort1 then do:
        display v-qty[4] @ v-qty[1]
                v-amt[4] @ v-amt[1].
        down.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[4],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[4],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).
      end.

      assign
       v-qty[5] = v-qty[5] + v-qty[4]
       v-amt[5] = v-amt[5] + v-amt[4]

       v-qty[4] = 0
       v-amt[4] = 0.
    end.

    if last(tt-report.key-01) then do:
      put skip(1).

      underline v-name v-qty[1] v-amt[1].

      display "         GRAND TOTALS" @ v-name
              v-qty[5] @ v-qty[1]
              v-amt[5] @ v-amt[1].

      IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' "GRAND TOTALS"                       '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-qty[5],"->>>,>>>,>>9")      '",'
               '"' ""                                   '",'
               '"' ""                                   '",'
               '"' STRING(v-amt[5],"->,>>>,>>>,>>9.99") '",'.
    end.

    delete w-data.
    IF AVAIL tt-report THEN delete tt-report.
  end.

