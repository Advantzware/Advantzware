/*****************************************************************************
PROGRAM: fg/rep/fg-costs.i 
PURPOSE:

 AUTHOR:
  NOTES:
 ****************************************************************************/
 
    first itemfg where rowid(itemfg) eq tt-itemfg.row-id no-lock

      break by {1}
            by {2}
            {3}:

    IF FIRST-OF({2}) THEN
      /*STATUS DEFAULT TRIM(STRING({1})) + "/" + TRIM(STRING({2})). */
         {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"} 

    if first-of({1}) then v-first[2] = yes.

    if first-of({2}) then do:
      assign
       v-first[1] = yes
       v-tot-sum  = 0
       v-ext-sum  = 0
       v-gsl-sum  = 0
       v-gsm-sum  = 0
       v-qoh      = 0.

      if v-sort-by-cust eq "Wh" then v-first[2] = yes.
    end.

    assign
     v-procat = itemfg.procat
     v-bin    = no.

    EMPTY TEMP-TABLE tt-rdtlh.

    for each tt-fg-bin
        where tt-fg-bin.company eq cocode
          and tt-fg-bin.i-no    eq tt-itemfg.i-no
          and tt-fg-bin.job-no  eq tt-itemfg.job-no
          and tt-fg-bin.job-no2 eq tt-itemfg.job-no2
          and tt-fg-bin.loc     eq tt-itemfg.loc
          and tt-fg-bin.loc-bin eq tt-itemfg.loc-bin
          and tt-fg-bin.tag     eq tt-itemfg.tag
        use-index co-ino NO-LOCK:

      IF vdat GE TODAY THEN DO:
        CREATE tt-rdtlh.
        ASSIGN
         tt-rdtlh.rita-code = "R"
         tt-rdtlh.job-no    = tt-fg-bin.job-no
         tt-rdtlh.job-no2   = tt-fg-bin.job-no2
         tt-rdtlh.loc       = tt-fg-bin.loc
         tt-rdtlh.loc-bin   = tt-fg-bin.loc-bin 
         tt-rdtlh.tag       = tt-fg-bin.tag
         tt-rdtlh.qty       = tt-fg-bin.qty
         tt-rdtlh.cust-no   = tt-fg-bin.cust-no.
      END.

      ELSE
      IF tt-fg-bin.tag EQ "" THEN
      FOR EACH fg-rcpth
          WHERE fg-rcpth.company    EQ cocode
            AND fg-rcpth.i-no       EQ tt-fg-bin.i-no
            AND fg-rcpth.job-no     EQ tt-fg-bin.job-no
            AND fg-rcpth.job-no2    EQ tt-fg-bin.job-no2
            AND fg-rcpth.trans-date LE vdat
          USE-INDEX tran NO-LOCK,

          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.loc       EQ tt-fg-bin.loc
            AND fg-rdtlh.loc-bin   EQ tt-fg-bin.loc-bin
            AND fg-rdtlh.tag       EQ tt-fg-bin.tag
            AND fg-rdtlh.cust-no   EQ tt-fg-bin.cust-no
          USE-INDEX rm-rdtl NO-LOCK:

        CREATE tt-rdtlh.
        BUFFER-COPY fg-rdtlh TO tt-rdtlh
        ASSIGN
         tt-rdtlh.trans-date = fg-rcpth.trans-date
         tt-rdtlh.job-no     = fg-rcpth.job-no
         tt-rdtlh.job-no2    = fg-rcpth.job-no2.
      END.

      ELSE
      FOR EACH fg-rdtlh
          WHERE fg-rdtlh.company EQ cocode
            AND fg-rdtlh.loc     EQ tt-fg-bin.loc
            AND fg-rdtlh.tag     EQ tt-fg-bin.tag
            AND fg-rdtlh.loc-bin EQ tt-fg-bin.loc-bin
            AND fg-rdtlh.cust-no EQ tt-fg-bin.cust-no
          USE-INDEX tag NO-LOCK,
          
          EACH fg-rcpth
          WHERE fg-rcpth.r-no       EQ fg-rdtlh.r-no
            AND fg-rcpth.rita-code  EQ fg-rdtlh.rita-code
            AND fg-rcpth.i-no       EQ tt-fg-bin.i-no
            AND fg-rcpth.job-no     EQ tt-fg-bin.job-no
            AND fg-rcpth.job-no2    EQ tt-fg-bin.job-no2
            AND fg-rcpth.trans-date LE vdat
          USE-INDEX r-no NO-LOCK:

        CREATE tt-rdtlh.
        BUFFER-COPY fg-rdtlh TO tt-rdtlh
        ASSIGN
         tt-rdtlh.trans-date = fg-rcpth.trans-date
         tt-rdtlh.job-no     = fg-rcpth.job-no
         tt-rdtlh.job-no2    = fg-rcpth.job-no2.
      END.
    END.

    RELEASE tt-rdtlh.

    FOR EACH tt-rdtlh USE-INDEX tt-rdtlh,

        FIRST tt-fg-bin
        WHERE tt-fg-bin.company EQ cocode
          AND tt-fg-bin.i-no    EQ tt-itemfg.i-no
          AND tt-fg-bin.job-no  EQ tt-rdtlh.job-no
          AND tt-fg-bin.job-no2 EQ tt-rdtlh.job-no2
          AND tt-fg-bin.loc     EQ tt-rdtlh.loc
          AND tt-fg-bin.loc-bin EQ tt-rdtlh.loc-bin
          AND tt-fg-bin.tag     EQ tt-rdtlh.tag
          AND tt-fg-bin.cust-no EQ tt-rdtlh.cust-no
        USE-INDEX co-ino NO-LOCK

        BREAK BY tt-rdtlh.job-no
              BY tt-rdtlh.job-no2
              BY tt-rdtlh.loc
              BY tt-rdtlh.loc-bin
              BY tt-rdtlh.tag
              BY tt-rdtlh.trans-date
              BY tt-rdtlh.r-no
              BY tt-rdtlh.rec_key:

      if v-sort-by-cust eq "Pr" and v-first[2] then page.

      if index("RATE",tt-rdtlh.rita-code) ne 0 then
        v-binqty = v-binqty + tt-rdtlh.qty.

      else
      if tt-rdtlh.rita-code eq "C" then
        v-binqty = tt-rdtlh.qty.

      else
      if tt-rdtlh.rita-code eq "S" then
        v-binqty = v-binqty - tt-rdtlh.qty.

      if last-of(tt-rdtlh.tag) then
        assign
         v-qty    = v-qty + v-binqty
         v-binqty = 0.

      assign
       v-costl = tt-fg-bin.std-lab-cost * v-qty
       v-costm = tt-fg-bin.std-mat-cost * v-qty 
       v-cost1 = if v-dl-mat then (tt-fg-bin.std-lab-cost + tt-fg-bin.std-mat-cost)
                             else tt-fg-bin.std-tot-cost
       v-cost  = v-cost1             * v-qty.

                                                  /* Calculate Cost */
      if tt-fg-bin.pur-uom eq "CS" and tt-fg-bin.case-count ne 0 then
        assign
         v-costl = v-costl / tt-fg-bin.case-count
         v-costm = v-costm / tt-fg-bin.case-count
         v-cost  = v-cost  / tt-fg-bin.case-count.
      else
      if tt-fg-bin.pur-uom eq "L" then
        assign
         v-costl = v-costl / v-qty
         v-costm = v-costm / v-qty
         v-cost  = v-costm / v-qty.
      else do:
        find first uom
            where uom.uom  eq itemfg.prod-uom
              and uom.mult ne 0
            no-lock no-error.
        if avail uom then
          assign
           v-costl = v-costl / uom.mult
           v-costm = v-costm / uom.mult
           v-cost  = v-cost  / uom.mult.
        else
          assign
           v-costl = v-costl / 1000
           v-costm = v-costm / 1000
           v-cost  = v-cost  / 1000.
      end.

      ASSIGN
       lv-sell-price = itemfg.sell-price
       lv-sell-uom   = itemfg.sell-uom
       lv-case-count = itemfg.case-count.

      IF TRIM(tt-fg-bin.job-no) NE "" AND NOT v-fgprice THEN
      FOR EACH job-hdr
          WHERE job-hdr.company EQ tt-fg-bin.company
            AND job-hdr.job-no  EQ tt-fg-bin.job-no
            AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
            AND job-hdr.i-no    EQ tt-fg-bin.i-no
            AND job-hdr.ord-no  NE 0
          USE-INDEX job-no NO-LOCK,
          FIRST oe-ordl
          WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.i-no    EQ job-hdr.i-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
          USE-INDEX item-ord NO-LOCK
          BY job-hdr.ord-no DESC:
        ASSIGN
         lv-sell-price = oe-ordl.price
         lv-sell-uom   = oe-ordl.pr-uom
         lv-case-count = oe-ordl.cas-cnt.
        LEAVE.
      END.
                                         /* Calculate Selling Price */
      if lv-sell-uom eq "CS" and lv-case-count ne 0 then
        v-ext = (v-qty * lv-sell-price) / lv-case-count.
      else do:
        find first uom
            where uom.uom  eq lv-sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-ext = v-qty * lv-sell-price /
                (if avail uom then uom.mult else 1000).
      end.

      if itemfg.sell-uom eq "L" then
        if v-qty le 0 then v-ext = 0.
        else v-ext = lv-sell-price.
        
      v-ext = round(v-ext,2).  

      if v-costl eq ? then v-costl = 0.
      if v-costm eq ? then v-costm = 0.
      if v-cost  eq ? then v-cost  = 0.
      if v-ext   eq ? then v-ext   = 0.

      assign
       v-qoh     = v-qty
       v-tot-sum = if v-dl-mat then v-costl else v-cost
       v-ext-sum = if v-dl-mat then v-costm else v-ext.
       
      /*IF v-prt-msf THEN*/ v-qoh = v-qoh * itemfg.t-sqft / 1000.

      assign
       v-tot-qty[1] = v-tot-qty[1] + v-qty
       v-tot-msf[1] = v-tot-msf[1] + v-qoh
       v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
       v-tot-ext[1] = v-tot-ext[1] + v-ext-sum.

      if last-of(tt-rdtlh.tag) /*and (zbal or v-qoh ne 0)*/ then do:
        if tt-fg-bin.job-no ne "" then
          v-job-no = trim(tt-fg-bin.job-no) + "-" + string(tt-fg-bin.job-no2,"99").
        else
          v-job-no = "".

        if v-prt-po then do:
          find first job-hdr
              where job-hdr.company eq cocode
                and job-hdr.job-no  eq tt-fg-bin.job-no
                and job-hdr.job-no2 eq tt-fg-bin.job-no2
                and job-hdr.i-no    eq tt-fg-bin.i-no
              USE-INDEX job-no no-lock no-error.
          if avail job-hdr then do:
            if v-po-type eq "L" then do:
              find first oe-ordl
                  where oe-ordl.company eq cocode
                    and oe-ordl.ord-no  eq job-hdr.ord-no
                    and oe-ordl.i-no    eq job-hdr.i-no
                  no-lock no-error.
              if avail oe-ordl then v-po-no = oe-ordl.po-no.
            end.
           
            else do:
              find first oe-ord
                  where oe-ord.company eq cocode
                    and oe-ord.ord-no  eq job-hdr.ord-no
                  no-lock no-error.
              if avail oe-ord then v-po-no = oe-ord.po-no.
            end.
          end.
         
          else v-po-no = "".
        end.

        v-qoh-s = STRING(v-qoh,v-qoh-f).

        v-arq = 0.
        IF v-prt-arqty THEN
        FOR EACH oe-relh
            WHERE oe-relh.company EQ tt-fg-bin.company
              AND oe-relh.posted  EQ NO
            USE-INDEX post NO-LOCK,
            EACH oe-rell
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
              AND oe-rell.i-no    EQ tt-fg-bin.i-no
              AND oe-rell.loc     EQ tt-fg-bin.loc
              AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin
              AND oe-rell.tag     EQ tt-fg-bin.tag
            USE-INDEX r-no NO-LOCK:
          v-arq = v-arq + oe-rell.qty.
        END.

        find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

        assign
         v-gsl-sum = 0
         v-gsm-sum = 0.

        /* lab */
        do i = 1 TO 6:
          v-gsl-sum = v-tot-sum * ce-ctrl.lab-pct[i] / 100.
          if ce-ctrl.lab-cost[i] gt v-tot-sum then leave.
        end.

        /* mat */
        do i = 1 TO 6:
          v-gsm-sum = v-ext-sum * ce-ctrl.mat-pct[i] / 100.
          if ce-ctrl.mat-cost[i] gt v-ext-sum then leave.
        end.

        assign
         v-all-sum    = v-tot-sum + v-ext-sum + v-gsl-sum + v-gsm-sum
         v-tot-gsl[1] = v-tot-gsl[1] + v-gsl-sum
         v-tot-gsm[1] = v-tot-gsm[1] + v-gsm-sum.

        IF SUBSTR(tt-fg-bin.tag,1 , 15) EQ tt-fg-bin.i-no THEN
              v-tag-no = SUBSTR(tt-fg-bin.tag, 16, 8).

                ASSIGN cDisplay = ""
                       cTmpField = ""
                       cVarValue = ""
                       cExcelDisplay = ""
                       cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                        CASE cTmpField:             
                             WHEN "cust"     THEN cVarValue = IF v-first[2] THEN STRING(itemfg.cust-no) ELSE "".
                             WHEN "i-no"     THEN cVarValue = IF v-first[1] THEN string(itemfg.i-no) ELSE "" .
                             WHEN "cust-prt" THEN cVarValue = IF v-first[1] THEN STRING(itemfg.part-no,"x(15)") ELSE "" .
                             WHEN "dscr"     THEN cVarValue = IF v-first[1] THEN STRING(itemfg.i-name,"x(25)") ELSE "" .
                             WHEN "whse"     THEN cVarValue = string(tt-fg-bin.loc).
                             WHEN "bin"      THEN cVarValue = STRING(tt-fg-bin.loc-bin)  .
                             WHEN "tag"      THEN cVarValue = STRING(v-tag-no).
                             WHEN "job"      THEN cVarValue = STRING(v-job-no).
                             WHEN "msf-oh"   THEN cVarValue = string(v-qoh,"->>>,>>9.999") .
                             WHEN "qty-oh"   THEN cVarValue = string(v-qty,"->>>,>>9")  .
                             WHEN "cst-uom"  THEN cVarValue = string(tt-fg-bin.pur-uom)  .
                            /* WHEN "uom-cst"  THEN cVarValue = IF v-prt-c AND v-cost1 NE ? THEN string(v-cost1,"->>>>9.9<<") ELSE "". */
                             WHEN "dl-cst"   THEN cVarValue = IF v-prt-c THEN string(v-tot-sum,"->>>,>>9.99") ELSE "". 
                             WHEN "mat-cst"  THEN cVarValue = IF v-prt-p THEN string(v-ext-sum,"->>>,>>9.99") ELSE "". 
                             WHEN "gs-lbr"   THEN cVarValue = IF v-prt-c THEN string(v-gsl-sum,"->>>,>>9.99") ELSE "".  
                             WHEN "gs-mat"   THEN cVarValue = IF v-prt-c THEN string(v-gsm-sum,"->>>,>>9.99") ELSE "".  
                             WHEN "ttl-cst"  THEN cVarValue = IF v-prt-c THEN string(v-all-sum,"->>>,>>9.99") ELSE "". 
                             
                        END CASE.
                          
                        cExcelVarValue = cVarValue.
                        cDisplay = cDisplay + cVarValue +
                                   FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
              
                PUT UNFORMATTED cDisplay SKIP.
                IF logExcelDump THEN DO:
                     PUT STREAM excel UNFORMATTED  
                           cExcelDisplay SKIP.
                 END.
        
       /* if v-prt-cpn then do:

          display itemfg.cust-no when v-first[2]
                  itemfg.i-no    when v-first[1]
                  itemfg.part-no when v-first[1] format "x(15)"  
                  itemfg.i-name  when v-first[1] format "x(25)"
                  tt-fg-bin.loc
                  tt-fg-bin.loc-bin
                  tt-fg-bin.tag
                    SUBSTR(tt-fg-bin.tag,16,8)
                                 WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                 @ tt-fg-bin.tag
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom
                  v-cost1   when v-prt-c
                  v-tot-sum when v-prt-c
                  v-ext-sum when v-prt-p
                  v-gsl-sum when v-prt-c
                  v-gsm-sum when v-prt-c
                  v-all-sum when v-prt-c
              with frame item-cost1 no-labels no-box down stream-io width 183.
          down with frame item-cost1.

          IF logExcelDump THEN DO: /* rdb 02/05/07  01090713 */
            ASSIGN
              chrCust = ""
              chrINo  = ""
              chrPart = "" 
              chrName = ""
              chrTag  = ""
              decCost = 0
              decTot  = 0
              decExt  = 0
              decGsl  = 0
              decGsm  = 0
              decAll  = 0.

            IF v-first[2] THEN
              chrCust = itemfg.cust-no.

            IF v-first[1] THEN
              ASSIGN
                chrINo  = itemfg.i-no
                chrPart = itemfg.part-no 
                chrName = itemfg.i-name.

            IF SUBSTR(tt-fg-bin.tag,1 , 15) EQ tt-fg-bin.i-no THEN
              chrTag = SUBSTR(tt-fg-bin.tag, 16, 8).

            IF v-prt-p THEN
              decExt  = v-ext-sum.

            IF v-prt-c THEN
              ASSIGN
                decCost = v-cost1
                decTot  = v-tot-sum
                decGsl  = v-gsl-sum
                decGsm  = v-gsm-sum
                decAll  = v-all-sum.

            EXPORT STREAM excel DELIMITER "," 
               chrCust chrINo chrPart chrName tt-fg-bin.loc tt-fg-bin.loc-bin
               chrTag v-job-no v-qoh-s tt-fg-bin.pur-uom decCost decTot decExt 
               decGsl decGsm decAll.
          END.

        end.
        else do:
          display itemfg.cust-no when v-first[2]
                  itemfg.i-no    when v-first[1]
                  itemfg.i-name  when v-first[1] format "x(25)"
                  tt-fg-bin.loc
                  tt-fg-bin.loc-bin
                  tt-fg-bin.tag
                    SUBSTR(tt-fg-bin.tag,16,8)
                                 WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                 @ tt-fg-bin.tag
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom
                  v-cost1   when v-prt-c
                  v-tot-sum when v-prt-c
                  v-ext-sum when v-prt-p
                  v-gsl-sum when v-prt-c
                  v-gsm-sum when v-prt-c
                  v-all-sum when v-prt-c
              with frame item-cost2 no-labels no-box down stream-io width 183.
          down with frame item-cost2.

          IF logExcelDump THEN DO: /* rdb 02/05/07  01090713 */
            ASSIGN
              chrCust = ""
              chrINo  = ""
              chrPart = "" 
              chrName = ""
              chrTag  = ""
              decCost = 0
              decTot  = 0
              decExt  = 0
              decGsl  = 0
              decGsm  = 0
              decAll  = 0.

            IF v-first[2] THEN
              chrCust = itemfg.cust-no.

            IF v-first[1] THEN
              ASSIGN
                chrINo  = itemfg.i-no
                chrPart = itemfg.part-no.
                

            IF SUBSTR(tt-fg-bin.tag,1 , 15) EQ tt-fg-bin.i-no THEN
              chrTag = SUBSTR(tt-fg-bin.tag, 16, 8).

            IF v-prt-p THEN
              decExt  = v-ext-sum.

            IF v-prt-c THEN
              ASSIGN
                decCost = v-cost1
                decTot  = v-tot-sum
                decGsl  = v-gsl-sum
                decGsm  = v-gsm-sum
                decAll  = v-all-sum.

            EXPORT STREAM excel DELIMITER "," 
               chrCust chrINo chrName tt-fg-bin.loc tt-fg-bin.loc-bin
               chrTag v-job-no v-qoh-s tt-fg-bin.pur-uom decCost decTot decExt 
               decGsl decGsm decAll.
          
          END.
        end.*/
        
        assign
         v-prnt  = yes
         v-first = no
         v-bin   = yes
         v-qty   = 0.
      end.
    end.

    if last-of({2}) then do:
      assign
       v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
       v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
       v-tot-cst[2] = v-tot-cst[2] + v-tot-cst[1]
       v-tot-ext[2] = v-tot-ext[2] + v-tot-ext[1]
       v-tot-gsl[2] = v-tot-gsl[2] + v-tot-gsl[1]
       v-tot-gsm[2] = v-tot-gsm[2] + v-tot-gsm[1]

       v-tot-qty[1] = 0
       v-tot-msf[1] = 0
       v-tot-cst[1] = 0
       v-tot-ext[1] = 0
       v-tot-gsl[1] = 0
       v-tot-gsm[1] = 0
       v-prnt[1]    = no.
    end.

    if last-of({1}) then do:
      assign
       v-tot-qty[3] = v-tot-qty[3] + v-tot-qty[2]
       v-tot-msf[3] = v-tot-msf[3] + v-tot-msf[2]
       v-tot-cst[3] = v-tot-cst[3] + v-tot-cst[2]
       v-tot-ext[3] = v-tot-ext[3] + v-tot-ext[2]
       v-tot-gsl[3] = v-tot-gsl[3] + v-tot-gsl[2]
       v-tot-gsm[3] = v-tot-gsm[3] + v-tot-gsm[2]
       
       v-tot-qty[2] = 0
       v-tot-msf[2] = 0
       v-tot-cst[2] = 0
       v-tot-ext[2] = 0
       v-tot-gsl[2] = 0
       v-tot-gsm[2] = 0
       v-prnt[2]    = no.
    end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

