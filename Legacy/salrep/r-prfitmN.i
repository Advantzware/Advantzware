

    ASSIGN
     v-qty = 0
     v-msf = 0
     v-amt = 0
     v-cst = 0
     v-cst1 = 0
     v-cst2 = 0
     v-style = ""
     v-test = ""
     v-flute = ""
     v-est-no = ""
     v-len = 0
     v-wid = 0
     v-dep = 0
     v-cust-part-no = ""
     v-cust-part-no2 = ""
     v-date = "" 
     v-order-date = "".

    FOR each cust
        where cust.company eq cocode
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
        no-lock:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
      {sa/sa-sls03.i "fdate" "tdate"}
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
          and tt-report.key-08  eq ""

        transaction:

      if tt-report.key-10 eq "ar-inv" then do:
        find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
              and ar-invl.i-no    ge fitem
              and ar-invl.i-no    le titem
              and ((tb_prep AND ar-invl.billable) or not ar-invl.misc)
            no-lock:

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq ar-invl.i-no
                and itemfg.procat  ge fpcat
                and itemfg.procat  le tpcat
              no-lock no-error.
          
          if ("" lt fpcat or "" gt tpcat) and (not avail itemfg) then next.

          create xtt-report.

          assign
           xtt-report.term-id = ""
           xtt-report.rec-id  = recid(ar-invl)
           xtt-report.key-01  = if sort-by-cust then tt-report.key-09
                                else if avail itemfg then itemfg.procat else ""
           xtt-report.key-02  = ar-invl.i-no
           xtt-report.key-03  = string(ar-invl.inv-no,"999999")
           xtt-report.key-09  = tt-report.key-09
           xtt-report.key-10  = "ar-invl".
        end.

        delete tt-report.
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
        find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

        assign
         v-exc         = yes
         tt-report.key-01 = if sort-by-cust then tt-report.key-09 else ""
         tt-report.key-02 = ""
         tt-report.key-03 = string(ar-cashl.inv-no,"999999").

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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
          if lv-type eq "items" then do:
            release ar-invl.
            find first oe-retl
                where oe-retl.company eq cocode
                  and oe-retl.r-no    eq lv-r-no
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

              create xtt-report.
              assign
               v-exc          = no
               tt-report.key-01  = if sort-by-cust then tt-report.key-09
                                else if avail itemfg then itemfg.procat else ""
               tt-report.key-02  = oe-retl.i-no.
            end.
          end.

          else
          if lv-type   eq "freight"                  and
             "freight" ge fitem                      and
             "freight" le titem                      and
             "frght"   ge fpcat                      and
             "frght"   le tpcat                      then
            assign
             v-exc         = no
             tt-report.key-01 = "FRGHT"
             tt-report.key-02 = "FREIGHT".

          else
          if lv-type eq "tax"                    and
             "tax" ge fitem                      and
             "tax" le titem                      and
             "tax" ge fpcat                      and
             "tax" le tpcat                      then
            assign
             v-exc         = no
             tt-report.key-01 = "TAX"
             tt-report.key-02 = "TAX".

          else
          if "" ge fitem and
             "" le titem then v-exc = no.
        end.

        else
        if "" ge fitem and
           "" le titem then v-exc = no.

        if v-exc then delete tt-report.
      end.
    end.

    for each tt-report
        where tt-report.term-id eq ""

        break by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03

        with frame itemx down

        transaction:

      create w-data.
      assign
       w-data.i-no   = tt-report.key-02
       w-data.inv-no = int(tt-report.key-03)
       w-data.rec-id = tt-report.rec-id.

      assign
         v-job-no   = ""
         v-job-no2  = 0
         v-msf[1]   = 0
         v-cst[1]   = 0
         v-cst1[1]   = 0
         v-cst2[1]   = 0
         v-po-no-po = 0
         v-style = ""
         v-test = ""
         v-flute = ""
         v-est-no = ""
         v-len = 0
         v-wid = 0
         v-dep = 0
         v-date = "" 
         v-order-date = "".

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.

      v-color = 0.
          
          FOR EACH itemfg-ink OF itemfg WHERE 
                   itemfg-ink.i-no EQ itemfg.i-no NO-LOCK, 
             EACH item WHERE item.company eq itemfg-ink.company and 
                  item.i-no eq itemfg-ink.rm-i-no 
                NO-LOCK: 

               IF AVAILABLE (itemfg-ink) THEN DO:
                 v-color   = v-color + itemfg-ink.occurs.
               END.  
          END.

      IF AVAILABLE(itemfg) THEN DO:
       /* IF RS_fgitem-cust-part-no = "Cust Part no" THEN */
           ASSIGN
              v-cust-part-no = itemfg.part-no.
       /* ELSE */
           ASSIGN
              v-cust-part-no2 = w-data.i-no.
       
     /*  IF TB_style-flute-test-lwd = YES THEN DO:*/
            ASSIGN
               v-style = itemfg.style
               v-len   = itemfg.l-score[50]
               v-wid   = itemfg.w-score[50]
               v-dep   = itemfg.d-score[50].
           END.
           FOR EACH eb FIELDS(test flute est-no) WHERE eb.company  EQ cocode
                                                   AND eb.est-no   EQ itemfg.est-no
                                                   AND eb.stock-no EQ itemfg.i-no NO-LOCK:
                ASSIGN 
                  v-test   = eb.test
                  v-flute  = eb.flute.
                LEAVE.
           END.
   /*   END.*/
            
      if tt-report.key-10 eq "ar-invl" then do:
        find first ar-invl where recid(ar-invl) eq w-data.rec-id no-lock.
        find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
        assign
         v-cust-no   = ar-inv.cust-no
         v-date      = string(ar-inv.inv-date)
         v-pric      = ar-invl.unit-pr
         v-uom       = ar-invl.pr-uom
         v-job-no    = ar-invl.job-no
         v-job-no2   = ar-invl.job-no2
         v-po-no-po  = ar-invl.po-no-po
         v-qty[1]    = IF ar-invl.ship-qty GT 0 THEN ar-invl.ship-qty ELSE ar-invl.inv-qty
         v-amt[1]    = ar-invl.amt
         v-msf[1]    = ar-invl.amt-msf.

        FIND FIRST oe-ord WHERE oe-ord.company = cocode 
                            AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN ASSIGN v-order-date = string(oe-ord.ord-date) .
        
        FIND FIRST job-hdr WHERE job-hdr.company = cocode
                             AND job-hdr.job-no  = ar-invl.job-no
                             AND job-hdr.job-no2 = ar-invl.job-no2
                             AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN
           v-est-no = TRIM(job-hdr.est-no).
        ELSE
           v-est-no = TRIM(ar-invl.est-no).
        IF v-est-no = "" THEN do:
           IF AVAIL itemfg THEN
              v-est-no = trim(itemfg.est-no) .
        END.
            
        RUN salrep/salecost.p ("4",
                               ROWID(ar-invl),
                               v-job-no,
                               v-job-no2,
                               v-qty[1],
                               OUTPUT v-cst[1]).

        RUN salrep/salecost.p ("2",
                               ROWID(ar-invl),
                               v-job-no,
                               v-job-no2,
                               v-qty[1],
                               OUTPUT v-cst1[1]).

        RUN salrep/salecost.p ("3",
                               ROWID(ar-invl),
                               v-job-no,
                               v-job-no2,
                               v-qty[1],
                               OUTPUT v-cst2[1]).
      end.

      else
      if tt-report.key-10 eq "ar-cashl" then do:
        find first ar-cashl where recid(ar-cashl) eq w-data.rec-id no-lock.
        find first ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.
        assign
         v-cust-no = ar-cash.cust-no
         v-date    = string(ar-cash.check-date)
         v-pric    = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom     = ""
         v-qty[1]  = 0
         v-cst[1]  = 0
         v-cst1[1]  = 0
         v-cst2[1]  = 0
         v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

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

             FIND FIRST oe-ord WHERE oe-ord.company = cocode 
                                 AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
             IF AVAIL oe-ord THEN ASSIGN v-order-date = string(oe-ord.ord-date) .
            leave.
          end.

          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq lv-r-no
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.

          if avail oe-retl then do:
            assign
             v-pric    = oe-retl.unit-pr
             v-uom     = oe-retl.uom
             v-job-no  = oe-retl.job-no
             v-job-no2 = oe-retl.job-no2
             v-qty[1]  = - oe-retl.tot-qty-return
             v-cst[1]  = oe-retl.cost * v-qty[1] / 1000.
             v-cst1[1] = oe-retl.cost * v-qty[1] / 1000.
             v-cst2[1] = oe-retl.cost * v-qty[1] / 1000.

          FIND FIRST job-hdr WHERE job-hdr.company = cocode
                               AND job-hdr.job-no  = oe-retl.job-no
                               AND job-hdr.job-no2 = oe-retl.job-no2 
                               AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
          IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN 
            v-est-no = TRIM(job-hdr.est-no).
          ELSE do:
           IF AVAIL itemfg THEN
              v-est-no = trim(itemfg.est-no) .
          END.

            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                no-lock no-error.
            if avail ar-invl then do:
              v-pric = ar-invl.unit-pr.

              RUN salrep/salecost.p ("4",
                                     ROWID(ar-invl),
                                     v-job-no,
                                     v-job-no2,
                                     v-qty[1],
                                     OUTPUT v-cst[1]).
              RUN salrep/salecost.p ("2",
                                     ROWID(ar-invl),
                                     v-job-no,
                                     v-job-no2,
                                     v-qty[1],
                                     OUTPUT v-cst1[2]).
              RUN salrep/salecost.p ("3",
                                     ROWID(ar-invl),
                                     v-job-no,
                                     v-job-no2,
                                     v-qty[1],
                                     OUTPUT v-cst2[3]).
              END.
            end.
          end.
        end.


      if v-msf[1] eq 0 and avail itemfg then
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

      if v-msf[1] eq 0 and avail itemfg then
        v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

   /*   if v-unit eq "E" then
        assign
         v-brdc = v-cst[1]
         v-marg = v-amt[1] - v-cst[1].
         
      else do:*/
      v-brdc = v-cst[1] / (v-qty[1] / 1000).
      v-ordc = v-cst1[1] / (v-qty[1] / 1000).
      v-invc = v-cst2[1] / (v-qty[1] / 1000).
        
    /*    if v-unit eq "U" then
          v-marg = (v-amt[1] - v-cst[1]) / (v-qty[1] / 1000).
        else*/
          v-marg = v-cst[1] / v-msf[1].
   /*   end.*/

   /*   if v-cost3 eq "C" then
        v-brdp = v-cst[1] / v-amt[1] * 100.
      else*/
        v-brdp = (v-amt[1] - v-cst[1]) / v-amt[1] * 100.
       
      v-$msf = v-amt[1] / v-msf[1].

      if v-brdc eq ? then v-brdc = 0.
      if v-ordc eq ? then v-ordc = 0.
      if v-invc eq ? then v-invc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-brdp eq ? then v-brdp = 0.
      if v-$msf eq ? then v-$msf = 0.

     /* IF tl_color = YES THEN */
         x-v-color = string(v-color).
     /* ELSE
         x-v-color = "". */
      
    /*  IF TB_style-flute-test-lwd = YES THEN DO:
         DISPLAY  
            v-cust-no
            v-order-date
            v-date
            w-data.inv-no
            w-data.i-no
            v-cust-part-no    WHEN RS_fgitem-cust-part-no = "Cust Part no" @ w-data.i-no
            itemfg.procat     WHEN AVAIL itemfg
            v-qty[1]
            itemfg.t-sqft     WHEN AVAIL itemfg
            v-msf[1]
            v-$msf
            v-est-no
            v-style
            v-flute
            v-test
            v-len
            v-wid
            v-dep
            x-v-color         
         WITH FRAME itemb NO-ERROR.
         DOWN WITH FRAME itemb. */
         {custom/statusMsg.i " 'Processing Customer#  '  + v-cust-no "}

         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                        WHEN "cust"        THEN cVarValue = string(v-cust-no) .
                        WHEN "inv"         THEN cVarValue = STRING(w-data.inv-no) .
                        WHEN "ino"         THEN cVarValue = REPLACE(v-cust-part-no2, '"', "") .
                        WHEN "cust-part"   THEN cVarValue = REPLACE(v-cust-part-no, '"', "") .
                        WHEN "cat"         THEN cVarValue = IF AVAIL itemfg THEN itemfg.procat ELSE "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[1],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.t-sqft,">>>9.999<<") ELSE "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[1],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue = STRING(v-pric,"->>>,>>>,>>9.99") .
                        WHEN "uom"         THEN cVarValue = v-uom .
                        WHEN "brdcst-m"       THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "ordcst-m"       THEN cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                        WHEN "invcst-m"       THEN cVarValue = STRING(v-invc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99") .
                        WHEN "cst"         THEN cVarValue = STRING(v-brdp,"->>>,>>9.99") .
                        WHEN "colr"        THEN cVarValue = x-v-color .
                        WHEN "ord-dt"      THEN cVarValue = v-order-date .
                        WHEN "shp-dt"      THEN cVarValue = v-date .
                        WHEN "inv-amt"     THEN cVarValue = STRING(v-amt[1],"->>>,>>>,>>9.99") .
                        WHEN "est"         THEN cVarValue = v-est-no .
                        WHEN "styl"        THEN cVarValue = v-style  .
                        WHEN "flut"        THEN cVarValue = STRING(v-flute) .
                        WHEN "tst"         THEN cVarValue = STRING(v-test)   .  
                        WHEN "lnth"        THEN cVarValue = STRING(v-len)  .   
                        WHEN "wdth"        THEN cVarValue = STRING(v-wid)  .   
                        WHEN "dpth"        THEN cVarValue = STRING(v-dep)  .   
                                                              
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
    /*  END.
      ELSE DO:
         /*DISPLAY 
            v-cust-no
            w-data.inv-no
            w-data.i-no       
            v-cust-part-no    WHEN RS_fgitem-cust-part-no = "Cust Part no" @ w-data.i-no
            itemfg.procat     when avail itemfg
            v-qty[1]
            itemfg.t-sqft     when avail itemfg
            v-msf[1]
            v-$msf
            v-pric
            v-amt[1]          when v-unit eq "E" @ v-pric
            v-uom             when v-unit ne "E"
            v-brdc            when v-cost2 
            v-marg            when v-cost2
            v-brdp            when v-cost2 
            x-v-color.           
         DOWN. */
             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "cust"        THEN cVarValue = string(v-cust-no) .
                        WHEN "inv"         THEN cVarValue = STRING(w-data.inv-no) .
                        WHEN "ino"         THEN cVarValue = REPLACE(v-cust-part-no2, '"', "") .
                        WHEN "cust-part"   THEN cVarValue = REPLACE(v-cust-part-no, '"', "") .
                        WHEN "cat"         THEN cVarValue = IF AVAIL itemfg THEN itemfg.procat ELSE "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[1],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.t-sqft,">>>9.999<<") ELSE "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[1],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue = IF v-unit = "E" THEN string(v-amt[1],"->>>,>>>,>>9.99") ELSE string(v-pric,"->>>,>>>,>>9.99") .
                        WHEN "uom"         THEN cVarValue = IF v-unit NE "E" THEN v-uom else "" .
                        WHEN "cst-m"        THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99") .
                        WHEN "cst"            THEN cVarValue = STRING(v-brdp,"->>>,>>9.99") .
                        WHEN "colr"        THEN cVarValue = x-v-color .
                        WHEN "ord-dt"      THEN cVarValue = "".
                        WHEN "shp-dt"      THEN cVarValue = "".
                        WHEN "inv-amt"     THEN cVarValue = "".
                        WHEN "est"         THEN cVarValue = "".
                        WHEN "styl"        THEN cVarValue = "".
                        WHEN "flut"        THEN cVarValue = "".
                        WHEN "tst"         THEN cVarValue = "".
                        WHEN "lnth"        THEN cVarValue = "".
                        WHEN "wdth"        THEN cVarValue = "".
                        WHEN "dpth"        THEN cVarValue = "".
                                                              
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
      END.*/

     
      

    /*  IF tb_excel THEN DO: 
         IF TB_style-flute-test-lwd = YES THEN
            PUT STREAM excel
               '"' v-cust-no                                      '",'  
               '"' v-order-date                                   '",'
               '"' v-date                                         '",'  
               '"' w-data.inv-no                                  '",'
               '"' REPLACE(v-cust-part-no, '"', "") FORMAT "X(15)"                 '",'  
               '"' (IF AVAIL itemfg THEN itemfg.procat ELSE "")   '",' 
               '"' v-qty[1]                                       '",'
               '"' (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0)    '",'
               '"' v-msf[1]                                       '",'
               '"' v-$msf                                         '",'
               '"' v-pric format "->>>,>>>,>>9.99<<"              '",'
               '"' v-uom                                          '",'
               '"' v-amt[1] format "->>>,>>>,>>9.99<<"            '",'
               '"' v-est-no                                       '",'
               '"' v-style                                        '",'
               '"' v-flute                                        '",'
               '"' v-test                                         '",'
               '"' v-len                                          '",'
               '"' v-wid                                          '",'
               '"' v-dep                                          '",'
               '"' x-v-color                                        '",'
               SKIP.
         ELSE
            PUT STREAM excel 
               '"' v-cust-no                                      '",'
               '"' w-data.inv-no                                  '",'
               '"' REPLACE(v-cust-part-no, '"', "")  FORMAT "X(15)"                '",'  
               '"' (IF AVAIL itemfg THEN itemfg.procat ELSE "")   '",' 
               '"' v-qty[1]                                       '",'
               '"' (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0)    '",'
               '"' v-msf[1]                                       '",'
               '"' v-$msf                                         '",'
               '"' (IF v-unit = "E" THEN v-amt[1] ELSE v-pric)    '",'
               '"' (IF v-unit NE "E" THEN v-uom else "")          '",'
               '"' v-brdc                                         '",'
               '"' v-marg                                         '",'
               '"' v-brdp                                         '",'
               '"' x-v-color                                        '",'
               SKIP.
      END. */
      assign
       v-qty[2] = v-qty[2] + v-qty[1]
       v-msf[2] = v-msf[2] + v-msf[1]
       v-cst[2] = v-cst[2] + IF v-cst[1] EQ ? THEN 0 ELSE v-cst[1]
       v-cst1[2] = v-cst1[2] + IF v-cst1[1] EQ ? THEN 0 ELSE v-cst1[1]
       v-cst2[2] = v-cst2[2] + IF v-cst2[1] EQ ? THEN 0 ELSE v-cst2[1]
       v-amt[2] = v-amt[2] + v-amt[1].

      if last-of(tt-report.key-02) then do:
       /* underline v-qty[1] v-msf[1] with frame itemx.

        if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.

        if v-unit eq "E" AND TB_style-flute-test-lwd = NO then underline v-pric with frame itemx. */

        if (not first-of(tt-report.key-02)) then do:
        /*  if v-unit eq "E" then
            assign
             v-brdc = v-cst[2]
             v-marg = v-amt[2] - v-cst[2].
             
          else do:*/
            v-brdc = v-cst[2] / (v-qty[2] / 1000)  .
            v-ordc = v-cst1[2] / (v-qty[2] / 1000)  .
            v-invc = v-cst2[2] / (v-qty[2] / 1000)  .
        
        /*    if v-unit eq "U" then
              v-marg = (v-amt[2] - v-cst[2]) / (v-qty[2] / 1000).
            else */
              v-marg = v-cst[2] / v-msf[2].
       /*   end.*/

      /*    if v-cost3 eq "C" then
            v-brdp = v-cst[2] / v-amt[2] * 100.
          else*/
            v-brdp = (v-amt[2] - v-cst[2]) / v-amt[2] * 100.
       
          v-$msf = v-amt[2] / v-msf[2].

          if v-brdc eq ? then v-brdc = 0.
          if v-ordc eq ? then v-ordc = 0.
          if v-invc eq ? then v-invc = 0.
          if v-marg eq ? then v-marg = 0.
          if v-brdp eq ? then v-brdp = 0.
          if v-$msf eq ? then v-$msf = 0.

         /* display "    ITEM" @ v-cust-no
                  "TOTALS"   @ w-data.i-no
                  v-qty[2]   @ v-qty[1]
                  v-msf[2]   @ v-msf[1]
                  v-$msf
                  v-amt[2]      when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
                  v-brdc        when v-cost2 AND TB_style-flute-test-lwd = NO
                  v-marg        when v-cost2 AND TB_style-flute-test-lwd = NO
                  v-brdp        when v-cost2 AND TB_style-flute-test-lwd = NO

              with frame itemx.

          down with frame itemx. */
          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "inv"         THEN cVarValue = "" .
                        WHEN "ino"         THEN cVarValue = "" .
                        WHEN "cust-part"   THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[2],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[2],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue = "" .
                        WHEN "uom"         THEN cVarValue = "" .
                        WHEN "brdcst-m"       THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "ordcst-m"       THEN cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                        WHEN "invcst-m"       THEN cVarValue = STRING(v-invc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99")  .
                        WHEN "cst"         THEN cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
                        WHEN "colr"        THEN cVarValue = "" .
                        WHEN "ord-dt"      THEN cVarValue = "".
                        WHEN "shp-dt"      THEN cVarValue = "".
                        WHEN "inv-amt"     THEN cVarValue = STRING(v-amt[2],"->>>,>>>,>>9.99") .
                        WHEN "est"         THEN cVarValue = "".
                        WHEN "styl"        THEN cVarValue = "".
                        WHEN "flut"        THEN cVarValue = "".
                        WHEN "tst"         THEN cVarValue = "".
                        WHEN "lnth"        THEN cVarValue = "".
                        WHEN "wdth"        THEN cVarValue = "".
                        WHEN "dpth"        THEN cVarValue = "".
                                                              
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " ITEM TOTALS"  substring(cDisplay,13,350) SKIP.
            put skip(1).
        end.

        assign
         v-qty[3] = v-qty[3] + v-qty[2]
         v-msf[3] = v-msf[3] + v-msf[2]
         v-cst[3] = v-cst[3] + v-cst[2]
         v-cst1[3] = v-cst1[3] + v-cst1[2]
         v-cst2[3] = v-cst2[3] + v-cst2[2]
         v-amt[3] = v-amt[3] + v-amt[2]

         v-qty[2] = 0
         v-msf[2] = 0
         v-cst[2] = 0
         v-cst1[2] = 0
         v-cst2[2] = 0
         v-amt[2] = 0.
      end.

      if last-of(tt-report.key-01) then do:
       /* underline v-qty[1] v-msf[1] with frame itemx.

        if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.

        if v-unit eq "E" AND TB_style-flute-test-lwd = NO then underline v-pric with frame itemx. */

        if (not first-of(tt-report.key-01)) then do:
        /*  if v-unit eq "E" then
            assign
             v-brdc = v-cst[3]
             v-marg = v-amt[3] - v-cst[3].
             
          else do:*/
            v-brdc = v-cst[3] / (v-qty[3] / 1000)  .
            v-ordc = v-cst1[3] / (v-qty[3] / 1000)  .
            v-invc = v-cst2[3] / (v-qty[3] / 1000)  .
        
          /*  if v-unit eq "U" then
              v-marg = (v-amt[3] - v-cst[3]) / (v-qty[3] / 1000).
            else */
              v-marg = v-cst[3] / v-msf[3].
       /*   end. */

     /*     if v-cost3 eq "C" then
            v-brdp = v-cst[3] / v-amt[3] * 100.
          else*/
            v-brdp = (v-amt[3] - v-cst[3]) / v-amt[3] * 100.
       
          v-$msf = v-amt[3] / v-msf[3].

          if v-brdc eq ? then v-brdc = 0.
          if v-ordc eq ? then v-ordc = 0.
          if v-invc eq ? then v-invc = 0.
          if v-marg eq ? then v-marg = 0.
          if v-brdp eq ? then v-brdp = 0.
          if v-$msf eq ? then v-$msf = 0.

         /* display "PROD CAT" @ v-cust-no
                  "CUSTOMER" when sort-by-cust @ v-cust-no
                  "TOTALS"   @ w-data.i-no
                  v-qty[3]   @ v-qty[1]
                  v-msf[3]   @ v-msf[1]
                  v-$msf
                  v-amt[3]      when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
                  v-brdc        when v-cost2 AND TB_style-flute-test-lwd = NO
                  v-marg        when v-cost2 AND TB_style-flute-test-lwd = NO
                  v-brdp        when v-cost2 AND TB_style-flute-test-lwd = NO

              with frame itemx.

          down with frame itemx. */

          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "inv"         THEN cVarValue = "" .
                        WHEN "ino"         THEN cVarValue = "" .
                        WHEN "cust-part"   THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[3],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[3],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue =   "" .
                        WHEN "uom"         THEN cVarValue = "" .
                        WHEN "brdcst-m"       THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "ordcst-m"       THEN cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                        WHEN "invcst-m"       THEN cVarValue = STRING(v-invc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99")  .
                        WHEN "cst"         THEN cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
                        WHEN "colr"        THEN cVarValue = "" .
                        WHEN "ord-dt"      THEN cVarValue = "".
                        WHEN "shp-dt"      THEN cVarValue = "".
                        WHEN "inv-amt"     THEN cVarValue = STRING(v-amt[3],"->>>,>>>,>>9.99") .
                        WHEN "est"         THEN cVarValue = "".
                        WHEN "styl"        THEN cVarValue = "".
                        WHEN "flut"        THEN cVarValue = "".
                        WHEN "tst"         THEN cVarValue = "".
                        WHEN "lnth"        THEN cVarValue = "".
                        WHEN "wdth"        THEN cVarValue = "".
                        WHEN "dpth"        THEN cVarValue = "".
                                                              
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " PROD CAT CUSTOMER TOTALS"  substring(cDisplay,26,350) SKIP.
            
          put skip(1).
        END.                
                                 
        assign
         v-qty[4] = v-qty[4] + v-qty[3]
         v-msf[4] = v-msf[4] + v-msf[3]
         v-cst[4] = v-cst[4] + v-cst[3]
         v-cst1[4] = v-cst1[4] + v-cst1[3]
         v-cst2[4] = v-cst2[4] + v-cst2[3]
         v-amt[4] = v-amt[4] + v-amt[3]

         v-qty[3] = 0
         v-msf[3] = 0
         v-cst[3] = 0
         v-cst1[3] = 0
         v-cst2[3] = 0
         v-amt[3] = 0.
      end.

      delete w-data.
      delete tt-report.
    end.

    /* display final totals */
    if v-qty[4] ne 0 or v-cst[4] ne 0 OR v-cst1[4] ne 0 OR v-cst2[4] ne 0 or v-amt[4] ne 0 then do:
      put skip(1).

     /* underline v-qty[1] v-msf[1] with frame itemx.

      if v-cost2 AND TB_style-flute-test-lwd = NO then underline v-brdc v-marg v-brdp with frame itemx.*/

   /*   if v-unit eq "E" AND TB_style-flute-test-lwd = NO then do:
       /* underline v-pric with frame itemx.  */
        
        assign
         v-brdc = v-cst[4]
         v-marg = v-amt[4] - v-cst[4].
      end.
      
      else do:*/
        v-brdc = v-cst[4] / (v-qty[4] / 1000) .
        v-ordc = v-cst1[4] / (v-qty[4] / 1000) .
        v-invc = v-cst2[4] / (v-qty[4] / 1000) .
        
     /*   if v-unit eq "U" then
          v-marg = (v-amt[4] - v-cst[4]) / (v-qty[4] / 1000).
        else*/
          v-marg = v-cst[4] / v-msf[4].
     /* end.*/

    /*  if v-cost3 eq "C" then
        v-brdp = v-cst[4] / v-amt[4] * 100.
      else*/
        v-brdp = (v-amt[4] - v-cst[4]) / v-amt[4] * 100.
       
      v-$msf = v-amt[4] / v-msf[4].

      if v-brdc eq ? then v-brdc = 0.
      if v-ordc eq ? then v-ordc = 0.
      if v-invc eq ? then v-invc = 0.
      if v-marg eq ? then v-marg = 0.
      if v-brdp eq ? then v-brdp = 0.
      if v-$msf eq ? then v-$msf = 0.

     /* display "   GRAND" @ v-cust-no
              "TOTALS"   @ w-data.i-no
              v-qty[4]   @ v-qty[1]
              v-msf[4]   @ v-msf[1]
              v-$msf
              v-amt[4]          when v-unit eq "E" AND TB_style-flute-test-lwd = NO @ v-pric
              v-brdc            when v-cost2 AND TB_style-flute-test-lwd = NO
              v-marg            when v-cost2 AND TB_style-flute-test-lwd = NO
              v-brdp            when v-cost2 AND TB_style-flute-test-lwd = NO
          with frame itemx. */
      PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "inv"         THEN cVarValue = "" .
                        WHEN "ino"         THEN cVarValue = "" .
                        WHEN "cust-part"   THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = "" .
                        WHEN "qty-shp"     THEN cVarValue = STRING(v-qty[4],"->>>,>>>,>>>") .
                        WHEN "i-sq"        THEN cVarValue = "" .
                        WHEN "ttl-msf"     THEN cVarValue = STRING(v-msf[4],"->>9.99") .
                        WHEN "msf"         THEN cVarValue = STRING(v-$msf,"->>>9") .
                        WHEN "unt-prc"     THEN cVarValue = ""  .
                        WHEN "uom"         THEN cVarValue = "" .
                        WHEN "brdcst-m"       THEN cVarValue = STRING(v-brdc,"->>>,>>9.99") .
                        WHEN "ordcst-m"       THEN cVarValue = STRING(v-ordc,"->>>,>>9.99") .
                        WHEN "invcst-m"       THEN cVarValue = STRING(v-invc,"->>>,>>9.99") .
                        WHEN "mar-m"       THEN cVarValue = STRING(v-marg,"->>>,>>9.99")  .
                        WHEN "cst"         THEN cVarValue = STRING(v-brdp,"->>>,>>9.99")  .
                        WHEN "colr"        THEN cVarValue = "" .
                        WHEN "ord-dt"      THEN cVarValue = "".
                        WHEN "shp-dt"      THEN cVarValue = "".
                        WHEN "inv-amt"     THEN cVarValue = STRING(v-amt[4],"->>>,>>>,>>9.99").
                        WHEN "est"         THEN cVarValue = "".
                        WHEN "styl"        THEN cVarValue = "".
                        WHEN "flut"        THEN cVarValue = "".
                        WHEN "tst"         THEN cVarValue = "".
                        WHEN "lnth"        THEN cVarValue = "".
                        WHEN "wdth"        THEN cVarValue = "".
                        WHEN "dpth"        THEN cVarValue = "".
                                                              
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED " GRAND TOTALS"  substring(cDisplay,14,350) SKIP.
            
     end.
