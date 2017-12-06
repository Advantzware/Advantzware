    
    for each cust
        where cust.company eq cocode
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
        no-lock:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

      {sa/sa-sls04.i "fdate" "tdate"}
    end.

    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq ""
          and tt-report.key-02  eq ""
          and tt-report.key-03  eq ""
          and tt-report.key-04  eq ""
          and tt-report.key-05  eq ""
          and tt-report.key-06  eq ""
          and tt-report.key-07  eq ""
          and tt-report.key-08  eq "",
          
        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report.key-09
        no-lock:
      
      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        IF tb_freight AND ar-inv.f-bill AND ar-inv.freight NE 0 THEN DO:
          RUN salrep/invfrate.p (ROWID(ar-inv), fsman, tsman).

          FOR EACH tt-report2
              WHERE tt-report2.key-10 EQ "ar-invf"
                AND tt-report2.inv-no EQ ar-inv.inv-no
              BREAK BY tt-report2.sman:

            ACCUMULATE tt-report2.freight (TOTAL BY tt-report2.sman).

            IF LAST-OF(tt-report2.sman) THEN DO:
              ASSIGN
               tt-report2.key-01  = IF v-sort EQ "Sales Rep" THEN tt-report2.sman
                                    ELSE ""
               tt-report2.key-02  = STRING(YEAR(ar-inv.inv-date),"9999") +
                                    STRING(MONTH(ar-inv.inv-date),"99")  +
                                    STRING(DAY(ar-inv.inv-date),"99")
               tt-report2.key-03  = STRING(ar-inv.inv-no,"9999999999")
               tt-report2.key-04  = "FREIGHT"
               tt-report2.key-05  = tt-report2.sman  
               tt-report2.rec-id  = RECID(ar-inv)
               tt-report2.freight = ACCUM TOTAL BY tt-report2.sman tt-report2.freight
               tt-report2.key-06   = ar-inv.cust-no .
            END.
            ELSE DELETE tt-report2.
          END.
        END.

        for each ar-invl
            where ar-invl.x-no eq ar-inv.x-no
              and ar-invl.i-no ge fitem
              and ar-invl.i-no le titem
              and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
            no-lock:

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
                and itemfg.procat  ge fpcat
                and itemfg.procat  le tpcat
              no-lock no-error.

          IF AVAIL itemfg OR ("" GE fpcat AND "" LE tpcat) THEN
          do i = 1 to 3:
            v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else ar-invl.sman[i].

            if v-sman-no  lt fsman                          or
               v-sman-no  gt tsman                          or
               (i ne 1 and
                (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
          
            create tt-report2.

            FIND FIRST b-itemfg WHERE
                 b-itemfg.company EQ cocode AND
                 b-itemfg.i-no EQ ar-invl.i-no
                 NO-LOCK NO-ERROR.

            assign
             tt-report2.term-id = ""
             tt-report2.rec-id  = recid(ar-invl)
             tt-report2.key-01  = if v-sort EQ "Sales Rep" THEN v-sman-no
                                  ELSE IF v-sort EQ "Category" AND AVAIL b-itemfg THEN b-itemfg.procat
                                  else ""
             tt-report2.key-02  = string(year(ar-inv.inv-date),"9999") +
                                  string(month(ar-inv.inv-date),"99")  +
                                  string(day(ar-inv.inv-date),"99")
             tt-report2.key-03  = string(ar-inv.inv-no,"9999999999")
             tt-report2.key-04  = ar-invl.i-no
             tt-report2.key-05  = v-sman-no
             tt-report2.key-09  = tt-report.key-09
             tt-report2.key-10  = "ar-invl" .
          ASSIGN
             tt-report2.key-06   = ar-inv.cust-no
             tt-report2.key-07   = ar-invl.i-no  .

            RELEASE b-itemfg.
            LEAVE.
          END.
        end.

        DELETE tt-report.
      end.
      
      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        
        assign
         v-exc            = yes
         tt-report.key-02 = string(year(ar-cash.check-date),"9999") +
                            string(month(ar-cash.check-date),"99")  +
                            string(day(ar-cash.check-date),"99")
         tt-report.key-03 = string(ar-cashl.inv-no,"9999999999").

        release ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
         lv-r-no = 0
         lv-type = "".
          
        IF AVAIL reftable THEN
          ASSIGN
           lv-r-no = reftable.val[1]
           lv-type = reftable.dscr.
        ELSE
        IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
          ASSIGN
           lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
           lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN DO:
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq lv-r-no
              no-lock no-error.
          if avail oe-reth then
          find first ar-inv
               where ar-inv.company eq cocode
                 and ar-inv.cust-no eq oe-reth.cust-no
                 and ar-inv.inv-no  eq oe-reth.inv-no
               no-lock no-error.

          if lv-type eq "items" then do:
            release ar-invl.
            find first oe-retl
                where oe-retl.company eq cocode
                  and oe-retl.r-no    eq oe-reth.r-no
                  and oe-retl.line    eq ar-cashl.line
                  and oe-retl.i-no    ge fitem
                  and oe-retl.i-no    le titem
                no-lock no-error.
            if avail oe-retl then
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                  and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
                no-lock no-error.
            if avail ar-invl then do:
              find first itemfg
                  where itemfg.company eq cocode
                    and itemfg.i-no    eq ar-invl.i-no
                    and itemfg.procat  ge fpcat
                    and itemfg.procat  le tpcat
                  no-lock no-error.

              IF AVAIL itemfg OR ("" GE fpcat AND "" LE tpcat) THEN
              do i = 1 to 3:
                v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                            else ar-invl.sman[i].

                if v-sman-no  lt fsman                          or
                   v-sman-no  gt tsman                          or
                   (i ne 1 and
                    (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
              

                create tt-report2.

                assign
                 v-exc              = no
                 tt-report2.term-id = ""
                 tt-report2.rec-id  = recid(ar-cashl)
                 tt-report2.key-01  = if v-sort EQ "Sales Rep" THEN v-sman-no
                                      ELSE IF v-sort EQ "Category" AND AVAIL itemfg THEN itemfg.procat
                                      else ""
                 tt-report2.key-02  = tt-report.key-02
                 tt-report2.key-03  = tt-report.key-03
                 tt-report2.key-04  = oe-retl.i-no
                 tt-report2.key-05  = v-sman-no
                 tt-report2.key-09  = tt-report.key-09
                 tt-report2.key-10  = tt-report.key-10.
                 LEAVE.
            END.

              DELETE tt-report.
            end.
          end.

          else
          if lv-type   eq "freight"                  and
             tb_freight                              and
             cust.sman ge fsman                      and
             cust.sman le tsman                      then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort EQ "Sales Rep" then cust.sman else ""
             tt-report.key-04 = "FREIGHT".

          else
          if lv-type   eq "tax"                  and
             "tax"     ge fitem                  and
             "tax"     le titem                  and
             "tax"     ge fpcat                  and
             "tax"     le tpcat                  and
             cust.sman ge fsman                  and
             cust.sman le tsman                  then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort EQ "Sales Rep" then cust.sman else ""
             tt-report.key-04 = "TAX".

          else
          if ""        ge fitem and
             ""        le titem and
             ""        ge fpcat and
             ""        le fpcat and
             cust.sman ge fsman and
             cust.sman le tsman then
            assign
             v-exc            = no
             tt-report.key-01 = if v-sort EQ "Sales Rep" then cust.sman else "".
        end.

        else
        if ""        ge fitem and
           ""        le titem and
           ""        ge fpcat and
           ""        le fpcat and
           cust.sman ge fsman and
           cust.sman le tsman then
          assign
           v-exc            = no
           tt-report.key-01 = if v-sort EQ "Sales Rep" then cust.sman else "".
           
        IF AVAIL tt-report AND v-exc THEN DELETE tt-report.
      end.
    end.


IF NOT v-smr THEN
    for each tt-report2
        where tt-report2.term-id eq ""

        break by tt-report2.key-01
              by tt-report2.key-02
              by tt-report2.key-03
              by tt-report2.key-04

        with frame itemx DOWN:
        
      if first-of(tt-report2.key-01) /*AND v-sort EQ "Sales Rep"*/ then do:
         
         page.
      end.
     find first sman
             where sman.company eq cocode
               and sman.sman    eq tt-report2.key-05
             no-lock no-error.

         v-slsmn-hdr = trim(tt-report2.key-05) .
         v-slsmn-hdr2 = (if avail sman then sman.sname else "Not on file").

      create w-data.
      assign
       w-data.inv-no = int(tt-report2.key-03)
       w-data.i-no   = tt-report2.key-04
       w-data.rec-id = tt-report2.rec-id.

      assign
       v-job-no   = ""
       v-job-no2  = 0
       v-msf[1]   = 0
       v-cst[1]   = 0
       v-po-no-po = 0
       v-ord-no   = 0
       v-pct      = 1.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.

      if tt-report2.key-10 eq "ar-invl" then do:
        find first ar-invl where recid(ar-invl) eq w-data.rec-id no-lock.

        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
        assign
         v-cust-no  = ar-inv.cust-no
         v-date     = ar-inv.inv-date
         v-custpo = ar-invl.po-no
         v-amt[1]   = ar-invl.unit-pr
         v-uom      = ar-invl.pr-uom
         v-job-no   = ar-invl.job-no
         v-job-no2  = ar-invl.job-no2
         v-po-no-po = ar-invl.po-no-po
         v-ord-no   = ar-invl.ord-no
         v-qty[1]   = ar-invl.inv-qty /* ship-qty */
         v-amt[1]   = ar-invl.amt
         v-msf[1]   = ar-invl.amt-msf.
         item-name = ar-invl.i-name .
         IF item-name = "" AND AVAIL itemfg THEN
             item-name = itemfg.i-name .

        IF v-cost1 NE "N" THEN
          RUN salrep/salecost.p ( IF v-cost1 EQ "B" THEN 4 ELSE LOOKUP(v-cost1,"B,O,I"),
                                 ROWID(ar-invl),
                                 v-job-no,
                                 v-job-no2,
                                 v-qty[1],
                                 OUTPUT v-cst[1]).
        
        if v-sort EQ "Sales Rep" then
        do i = 1 to 3:
           if ar-invl.sman[i] eq tt-report2.key-01 then
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

      ELSE
      IF tt-report2.key-10 EQ "ar-invf" THEN DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ w-data.rec-id NO-LOCK.

        ASSIGN
         v-cust-no = ar-inv.cust-no
         v-date    = ar-inv.inv-date
         v-amt[1]  = tt-report2.freight
         v-uom     = ""
         v-qty[1]  = 0
         v-cst[1]  = 0.
      END.

      else
      if tt-report2.key-10 eq "ar-cashl" then do:
        find first ar-cashl where recid(ar-cashl) eq w-data.rec-id no-lock.
        find first ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        assign
         v-cust-no = ar-cash.cust-no
         v-date    = ar-cash.check-date
         v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom     = ""
         v-qty[1]  = 0
         v-cst[1]  = 0.

        release ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
         lv-r-no = 0
         lv-type = "".
          
        IF AVAIL reftable THEN
          ASSIGN
           lv-r-no = reftable.val[1]
           lv-type = reftable.dscr.
        ELSE
        IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
          ASSIGN
           lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
           lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN DO:
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq lv-r-no
              no-lock no-error.
          if avail oe-reth then
          for each ar-inv
              where ar-inv.company eq cocode
                and ar-inv.cust-no eq oe-reth.cust-no
                and ar-inv.inv-no  eq oe-reth.inv-no
              no-lock,
              each ar-invl
              where ar-invl.x-no eq ar-inv.x-no
                and ar-invl.i-no eq w-data.i-no
              no-lock:
            v-po-no-po = ar-invl.po-no-po.
            v-ord-no   = ar-invl.ord-no .
            leave.
          end.

          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq int(substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 25,12))
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.

          if avail oe-retl then do:
            assign
             v-uom     = oe-retl.uom
             v-job-no  = oe-retl.job-no
             v-job-no2 = oe-retl.job-no2
             v-qty[1]  = - oe-retl.tot-qty-return.
             
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                no-lock no-error.
            if avail ar-invl then do:
              if v-sort EQ "Sales Rep" then
              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report2.key-01 then
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

            IF v-cost1 NE "N" THEN
              RUN salrep/salecost.p ( IF v-cost1 EQ "B" THEN 4 ELSE LOOKUP(v-cost1,"B,O,I"),
                                     ROWID(ar-invl),
                                     v-job-no,
                                     v-job-no2,
                                     v-qty[1],
                                     OUTPUT v-cst[1]).
          end.
        end.
      end.

      if v-msf[1] eq 0 and avail itemfg then
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).
        
      assign
       v-qty[1] = v-qty[1] * v-pct
       v-amt[1] = v-amt[1] * v-pct
       v-msf[1] = v-msf[1] * v-pct
       v-cst[1] = v-cst[1] * v-pct
       
       v-qty[2] = v-qty[1]
       v-amt[2] = v-amt[1]
       v-msf[2] = v-msf[1]
       v-cst[2] = v-cst[1].

      IF v-qty[2] EQ ? THEN v-qty[2] = 0.
      IF v-msf[2] EQ ? THEN v-msf[2] = 0.
      IF v-cst[2] EQ ? THEN v-cst[2] = 0.
      IF v-amt[2] EQ ? THEN v-amt[2] = 0.
       
      assign
       v-brdc = v-cst[2]
       v-marg = v-amt[2] - v-cst[2]
       v-$msf = v-amt[2] / v-msf[2].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-$msf eq ? then v-$msf = 0.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq v-cust-no
          no-lock no-error.

      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

      FIND FIRST shipto where shipto.company eq cocode
          and shipto.cust-no eq cust.cust-no NO-LOCK NO-ERROR  .

    

       ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = string(tt-report2.key-05) .
                 WHEN "rep-name" THEN cVarValue = string(v-slsmn-hdr2) .
                 WHEN "name" THEN cVarValue = string(item-name,"x(15)") .
                 WHEN "cust" THEN cVarValue = STRING(v-cust-no).
                 WHEN "custname" THEN cVarValue = IF AVAIL cust THEN STRING(cust.name,"x(15)") ELSE "".
                 WHEN "inv-no" THEN cVarValue = STRING(w-data.inv-no).
                 WHEN "inv-date" THEN cVarValue = STRING(v-date,"99/99/99").
                 WHEN "cust-po" THEN cVarValue = STRING(v-custpo,"x(15)").
                 WHEN "pur-ord" THEN cVarValue = STRING(v-ord-no,">>>>>>>>").
                 WHEN "fg" THEN cVarValue = string(w-data.i-no).
                 WHEN "cat" THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.procat) ELSE "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[2],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[2],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[2],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = IF AVAIL cust THEN STRING(cust.spare-char-2,"x(8)") ELSE "".
                 WHEN "mbr-no" THEN cVarValue = IF AVAIL shipto THEN STRING(shipto.spare-char-5,"x(10)") ELSE "".                   
                 WHEN "inv-uom" THEN cVarValue = STRING(v-uom,"x(3)").
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED cDisplay SKIP.
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
   END.

      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-msf[3] = v-msf[3] + v-msf[2]
       v-cst[3] = v-cst[3] + v-cst[2]
       v-amt[3] = v-amt[3] + v-amt[2]
         
       v-qty[2] = 0
       v-msf[2] = 0
       v-cst[2] = 0
       v-amt[2] = 0.

      if last-of(tt-report2.key-02) then do:
     /*   underline v-qty[2] v-msf[2] with frame itemx.

        if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx. */

        assign
         v-brdc = v-cst[3]
         v-marg = v-amt[3] - v-cst[3]
         v-$msf = v-amt[3] / v-msf[3].

        if v-brdc eq ? then v-brdc = 0.
        if v-marg eq ? then v-marg = 0.
        if v-$msf eq ? then v-$msf = 0.

     

        PUT    SKIP  str-line SKIP .
        ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = "".
                 WHEN "rep-name" THEN cVarValue = "".
                 WHEN "name" THEN cVarValue = "" .
                 WHEN "cust" THEN cVarValue = "".
                 WHEN "custname" THEN cVarValue = "".
                 WHEN "inv-no" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "cust-po" THEN cVarValue = "" .
                 WHEN "pur-ord" THEN cVarValue = "".
                 WHEN "fg" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[3],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[3],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[3],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = "".
                 WHEN "mbr-no" THEN cVarValue = "". 
                 WHEN "inv-uom" THEN cVarValue =  "".
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED  "        DATE TOTALS FOR "+ substr(tt-report2.key-02,5,2) + "/" +
                                         substr(tt-report2.key-02,7,2) + "/" +
                                         substr(tt-report2.key-02,1,4)" " substring(cDisplay,36,300) SKIP.
        

        assign
         v-qty[4] = v-qty[4] + v-qty[3]
         v-msf[4] = v-msf[4] + v-msf[3]
         v-cst[4] = v-cst[4] + v-cst[3]
         v-amt[4] = v-amt[4] + v-amt[3]

         v-qty[3] = 0
         v-msf[3] = 0
         v-cst[3] = 0
         v-amt[3] = 0.
      end.

      if last-of(tt-report2.key-01) then do:
      
        if v-sort NE "Date" then do:
         /* underline v-qty[2] v-msf[2] with frame itemx. */

      /*    if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx. */

          assign
           v-brdc = v-cst[4]
           v-marg = v-amt[4] - v-cst[4]
           v-$msf = v-amt[4] / v-msf[4].

          if v-brdc eq ? then v-brdc = 0.
          if v-marg eq ? then v-marg = 0.
          if v-$msf eq ? then v-$msf = 0.

       

          PUT    SKIP  str-line SKIP .
        ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = "".
                 WHEN "rep-name" THEN cVarValue = "" .
                 WHEN "name" THEN cVarValue = "" .
                 WHEN "cust" THEN cVarValue = "".
                 WHEN "custname" THEN cVarValue = "".
                 WHEN "inv-no" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "cust-po" THEN cVarValue = "" .
                 WHEN "pur-ord" THEN cVarValue = "".
                 WHEN "fg" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[4],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[4],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[4],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = "".
                 WHEN "mbr-no" THEN cVarValue = "".
                 WHEN "inv-uom" THEN cVarValue = "".
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED  "    " v-total-label substring(cDisplay,25,300) SKIP.
          
          
        end.

        assign
         v-qty[5] = v-qty[5] + v-qty[4]
         v-msf[5] = v-msf[5] + v-msf[4]
         v-cst[5] = v-cst[5] + v-cst[4]
         v-amt[5] = v-amt[5] + v-amt[4]

         v-qty[4] = 0
         v-msf[4] = 0
         v-cst[4] = 0
         v-amt[4] = 0.
      end.
      
      delete w-data.
    end.

    /* display final totals */
    if v-qty[5] ne 0 or v-cst[5] ne 0 or v-amt[5] ne 0 then do:
      put skip(1).

    /*  underline v-qty[2] v-msf[2] with frame itemx.

      if v-cost2 then underline v-brdc v-marg v-amt[2] with frame itemx. */

      assign
       v-brdc = v-cst[5]
       v-marg = v-amt[5] - v-cst[5]
       v-$msf = v-amt[5] / v-msf[5].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-$msf eq ? then v-$msf = 0.

    

       PUT    SKIP  str-line SKIP .
        ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = "".
                 WHEN "rep-name" THEN cVarValue = "".
                 WHEN "name" THEN cVarValue = "" .
                 WHEN "cust" THEN cVarValue = "".
                 WHEN "custname" THEN cVarValue = "".
                 WHEN "inv-no" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "cust-po" THEN cVarValue = "" .
                 WHEN "pur-ord" THEN cVarValue = "".
                 WHEN "fg" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[5],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[5],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[5],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = "".
                 WHEN "mbr-no" THEN cVarValue = "". 
                 WHEN "inv-uom" THEN cVarValue = "".
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED  "        GRAND TOTALS" substring(cDisplay,21,300) SKIP.
    end.

    ELSE DO: /* summ = yes */

        for each tt-report2
        where tt-report2.term-id eq ""

        break by tt-report2.key-06
              by tt-report2.key-07
             /* by tt-report2.key-01
              by tt-report2.key-02
              by tt-report2.key-03*/
              /*by tt-report2.key-04*/

        with frame itemx DOWN:
        
      /*if first-of(tt-report2.key-01) /*AND v-sort EQ "Sales Rep"*/ then do: */
         find first sman
             where sman.company eq cocode
               and sman.sman    eq tt-report2.key-05
             no-lock no-error.
             
       /*  v-slsmn-hdr = "SalesRep: " + trim(tt-report2.key-01) + " " +
                       (if avail sman then sman.sname else "Not on file"). */

         v-slsmn-hdr = trim(tt-report2.key-05) .
         v-slsmn-hdr2 = (if avail sman then sman.sname else "Not on file").
        
     

      create w-data.
      assign
       w-data.inv-no = int(tt-report2.key-03)
       w-data.i-no   = tt-report2.key-04
       w-data.rec-id = tt-report2.rec-id.

      assign
       v-job-no   = ""
       v-job-no2  = 0
       v-msf[1]   = 0
       v-cst[1]   = 0
       v-po-no-po = 0
       v-ord-no   = 0
       v-pct      = 1.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.

      if tt-report2.key-10 eq "ar-invl" then do:
        find first ar-invl where recid(ar-invl) eq w-data.rec-id no-lock.

        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
        assign
         v-cust-no  = ar-inv.cust-no
         v-date     = ar-inv.inv-date
         v-custpo = ar-invl.po-no
         v-amt[1]   = ar-invl.unit-pr
         v-uom      = ar-invl.pr-uom
         v-job-no   = ar-invl.job-no
         v-job-no2  = ar-invl.job-no2
         v-po-no-po = ar-invl.po-no-po
         v-ord-no   = ar-invl.ord-no
         v-qty[1]   = ar-invl.inv-qty /* ship-qty */
         v-amt[1]   = ar-invl.amt
         v-msf[1]   = ar-invl.amt-msf.
         item-name = ar-invl.i-name .
         IF item-name = "" AND AVAIL itemfg THEN
             item-name = itemfg.i-name .

        IF v-cost1 NE "N" THEN
          RUN salrep/salecost.p ( IF v-cost1 EQ "B" THEN 4 ELSE LOOKUP(v-cost1,"B,O,I"),
                                 ROWID(ar-invl),
                                 v-job-no,
                                 v-job-no2,
                                 v-qty[1],
                                 OUTPUT v-cst[1]).
        
        if v-sort EQ "Sales Rep" then
        do i = 1 to 3:
           if ar-invl.sman[i] eq tt-report2.key-01 then
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

      ELSE
      IF tt-report2.key-10 EQ "ar-invf" THEN DO:
        FIND ar-inv WHERE RECID(ar-inv) EQ w-data.rec-id NO-LOCK.

        ASSIGN
         v-cust-no = ar-inv.cust-no
         v-date    = ar-inv.inv-date
         v-amt[1]  = tt-report2.freight
         v-uom     = ""
         v-qty[1]  = 0
         v-cst[1]  = 0.
      END.

      else
      if tt-report2.key-10 eq "ar-cashl" then do:
        find first ar-cashl where recid(ar-cashl) eq w-data.rec-id no-lock.
        find first ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        assign
         v-cust-no = ar-cash.cust-no
         v-date    = ar-cash.check-date
         v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom     = ""
         v-qty[1]  = 0
         v-cst[1]  = 0.

        release ar-inv.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
         lv-r-no = 0
         lv-type = "".
          
        IF AVAIL reftable THEN
          ASSIGN
           lv-r-no = reftable.val[1]
           lv-type = reftable.dscr.
        ELSE
        IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
          ASSIGN
           lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
           lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN DO:
          find first oe-reth
              where oe-reth.company eq cocode
                and oe-reth.r-no    eq lv-r-no
              no-lock no-error.
          if avail oe-reth then
          for each ar-inv
              where ar-inv.company eq cocode
                and ar-inv.cust-no eq oe-reth.cust-no
                and ar-inv.inv-no  eq oe-reth.inv-no
              no-lock,
              each ar-invl
              where ar-invl.x-no eq ar-inv.x-no
                and ar-invl.i-no eq w-data.i-no
              no-lock:
            v-po-no-po = ar-invl.po-no-po.
            v-ord-no   = ar-invl.ord-no .
            leave.
          end.

          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq int(substr(ar-cashl.dscr,index(ar-cashl.dscr,"oe return") + 25,12))
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.

          if avail oe-retl then do:
            assign
             v-uom     = oe-retl.uom
             v-job-no  = oe-retl.job-no
             v-job-no2 = oe-retl.job-no2
             v-qty[1]  = - oe-retl.tot-qty-return.
             
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                no-lock no-error.
            if avail ar-invl then do:
              if v-sort EQ "Sales Rep" then
              do i = 1 to 3:
                if ar-invl.sman[i] eq tt-report2.key-01 then
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

            IF v-cost1 NE "N" THEN
              RUN salrep/salecost.p ( IF v-cost1 EQ "B" THEN 4 ELSE LOOKUP(v-cost1,"B,O,I"),
                                     ROWID(ar-invl),
                                     v-job-no,
                                     v-job-no2,
                                     v-qty[1],
                                     OUTPUT v-cst[1]).
          end.
        end.
      end.

      if v-msf[1] eq 0 and avail itemfg then
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).
        
      assign
       v-qty[1] = v-qty[1] * v-pct
       v-amt[1] = v-amt[1] * v-pct
       v-msf[1] = v-msf[1] * v-pct
       v-cst[1] = v-cst[1] * v-pct
       
       v-qty[2] = v-qty[1]
       v-amt[2] = v-amt[1]
       v-msf[2] = v-msf[1]
       v-cst[2] = v-cst[1].

      IF v-qty[2] EQ ? THEN v-qty[2] = 0.
      IF v-msf[2] EQ ? THEN v-msf[2] = 0.
      IF v-cst[2] EQ ? THEN v-cst[2] = 0.
      IF v-amt[2] EQ ? THEN v-amt[2] = 0.
       
      assign
       v-brdc = v-cst[2]
       v-marg = v-amt[2] - v-cst[2]
       v-$msf = v-amt[2] / v-msf[2].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-$msf eq ? then v-$msf = 0.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq v-cust-no
          no-lock no-error.

      FIND FIRST shipto where shipto.company eq cocode
          and shipto.cust-no eq cust.cust-no NO-LOCK NO-ERROR  .

      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-msf[3] = v-msf[3] + v-msf[2]
       v-cst[3] = v-cst[3] + v-cst[2]
       v-amt[3] = v-amt[3] + v-amt[2]
         
       v-qty[2] = 0
       v-msf[2] = 0
       v-cst[2] = 0
       v-amt[2] = 0.

    IF LAST-OF(tt-report2.key-07) THEN do:

       ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = STRING(tt-report2.key-05) .
                 WHEN "rep-name" THEN cVarValue = string(v-slsmn-hdr2) .
                 WHEN "name" THEN cVarValue = string(item-name,"x(15)") .
                 WHEN "cust" THEN cVarValue = STRING(v-cust-no).
                 WHEN "custname" THEN cVarValue = IF AVAIL cust THEN STRING(cust.name,"x(15)") ELSE "".
                 WHEN "inv-no" THEN cVarValue = STRING(w-data.inv-no).
                 WHEN "inv-date" THEN cVarValue = STRING(v-date,"99/99/99").
                 WHEN "cust-po" THEN cVarValue = string(v-custpo,"x(15)") . 
                 WHEN "pur-ord" THEN cVarValue = STRING(v-ord-no,">>>>>>>>").
                 WHEN "fg" THEN cVarValue = string(w-data.i-no).
                 WHEN "cat" THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.procat) ELSE "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[3],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[3],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[3],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = IF AVAIL cust THEN STRING(cust.spare-char-2,"x(8)") ELSE "".
                 WHEN "mbr-no" THEN cVarValue = IF AVAIL shipto THEN STRING(shipto.spare-char-5,"x(10)") ELSE "".                   
                 WHEN "inv-uom" THEN cVarValue = STRING(v-uom,"x(3)").
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED cDisplay SKIP.
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
   END.

   assign
         v-qty[4] = v-qty[4] + v-qty[3]
         v-msf[4] = v-msf[4] + v-msf[3]
         v-cst[4] = v-cst[4] + v-cst[3]
         v-amt[4] = v-amt[4] + v-amt[3]

         v-qty[3] = 0
         v-msf[3] = 0
         v-cst[3] = 0
         v-amt[3] = 0.

    END.

     delete w-data.
    end.

    /* display final totals */
    if v-qty[4] ne 0 or v-cst[4] ne 0 or v-amt[4] ne 0 then do:
      put skip(1).

    assign
       v-brdc = v-cst[4]
       v-marg = v-amt[4] - v-cst[4]
       v-$msf = v-amt[4] / v-msf[4].

      if v-brdc eq ? then v-brdc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-$msf eq ? then v-$msf = 0.

    
       PUT    SKIP  str-line SKIP .
        ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?.

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:               
                 WHEN "rep" THEN cVarValue = "".
                 WHEN "rep-name" THEN cVarValue = "" .
                 WHEN "name" THEN cVarValue = "" .
                 WHEN "cust" THEN cVarValue = "".
                 WHEN "custname" THEN cVarValue = "".
                 WHEN "inv-no" THEN cVarValue = "".
                 WHEN "inv-date" THEN cVarValue = "".
                 WHEN "cust-po" THEN cVarValue = "" . 
                 WHEN "pur-ord" THEN cVarValue = "".
                 WHEN "fg" THEN cVarValue = "".
                 WHEN "cat" THEN cVarValue = "".
                 WHEN "qty" THEN cVarValue = STRING(v-qty[4],"->>>,>>>,>>>").
                 WHEN "ttl-msf" THEN cVarValue = STRING(v-msf[4],"->>>>9.99").
                 WHEN "msf" THEN cVarValue = string(v-$msf,"->>>9.99").
                 WHEN "sal-amt" THEN cVarValue = STRING(v-amt[4],"->>>,>>>,>>9.99<<") .
                 WHEN "ful-cst" THEN cVarValue = IF v-cost2 THEN STRING(v-brdc,"->>>,>>9.99") ELSE "".
                 WHEN "proft" THEN cVarValue = IF v-cost2 THEN STRING(v-marg,"->>>,>>9.99") ELSE "".
                 WHEN "grp-no" THEN cVarValue = "".
                 WHEN "mbr-no" THEN cVarValue = "". 
                 WHEN "inv-uom" THEN cVarValue = "".
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       
   END.
   PUT UNFORMATTED  "        GRAND TOTALS" substring(cDisplay,21,300) SKIP.
    end.


    END.
