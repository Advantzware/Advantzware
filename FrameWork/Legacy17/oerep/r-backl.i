/***************************************************************
     Program: oerep/r-backl.i
     Purpose:
 
      Author: ASI
 Maintenance: Updated by RDB 01/22/07 - Export to excel spreadsheet
 
       Notes: Called by oerep/r-backl.w
  ****************************************************************/
       
    /*do i = 1 to 3:
      assign
       fstat = tstat
       tstat = if i eq 1 then "C" else
               if i eq 2 then "D" else "Z".*/

      for each xoe-ord
          where xoe-ord.company  eq cocode
            and xoe-ord.opened   eq yes
            and xoe-ord.ord-no   ge v-ford-no[1]
            and xoe-ord.ord-no   le v-ford-no[2]
            and xoe-ord.cust-no  ge v-fcust[1]
            and xoe-ord.cust-no  le v-fcust[2]
            and xoe-ord.sman[1]  ge v-fslsm[1]
            and xoe-ord.sman[1]  le v-fslsm[2]
            AND xoe-ord.user-id  GE begin_user
            AND xoe-ord.user-id  LE end_user
          use-index opened no-lock,

          each oe-ordl
          where oe-ordl.company   eq cocode
            and oe-ordl.ord-no    eq xoe-ord.ord-no
            AND oe-ordl.stat      NE "C"
            and oe-ordl.i-no      ge v-fitem[1]
            and oe-ordl.i-no      le v-fitem[2]
            and oe-ordl.req-date  ge v-fdate[1]
            and oe-ordl.req-date  le v-fdate[2]
          no-lock:

	{custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

        IF oe-ordl.ship-qty EQ oe-ordl.t-ship-qty THEN
          li-qty[2] = oe-ordl.ship-qty.
        ELSE
          RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT li-qty[1], OUTPUT li-qty[2]).


        IF li-qty[2] LT oe-ordl.qty THEN DO:

          create tt-report.
          assign
           tt-report.key-01  = string(if v-sort eq "D" then
                                        string(year(oe-ordl.req-date),"9999") +
                                        string(month(oe-ordl.req-date),"99")  +
                                        string(day(oe-ordl.req-date),"99")
                                      else "xxxxxxxx") +
                               string(if v-sort eq "S" then xoe-ord.sman[1]
                                                       else "","xxx") +
                               xoe-ord.cust-no
           tt-report.key-02  = if v-sumdet then oe-ordl.i-no
                               else string(xoe-ord.ord-no,"9999999999")
           tt-report.key-03  = oe-ordl.i-no
           tt-report.rec-id  = recid(oe-ordl).
        END.
      end.
    /*end.*/
    
    if v-jobs and 0 ge v-ford-no[1] and 0 le v-ford-no[2] then
    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.opened  eq yes
          and job-hdr.i-no    ge v-fitem[1]
          and job-hdr.i-no    le v-fitem[2]
          and job-hdr.cust-no ge v-fcust[1]
          and job-hdr.cust-no le v-fcust[2]
          and job-hdr.ord-no  eq 0
        use-index opened no-lock,
        
        first job
        where job.company    eq cocode
          and job.job        eq job-hdr.job
          and job.job-no     eq job-hdr.job-no
          and job.job-no2    eq job-hdr.job-no2
          and job.start-date ge v-fdate[1]
          and job.start-date le v-fdate[2]
          and job.user-id    ge begin_user
          and job.user-id    le end_user
        no-lock,  
        
        first cust
        where cust.company eq cocode
          and cust.cust-no eq job-hdr.cust-no
          and cust.sman    ge v-fslsm[1]
          and cust.sman    le v-fslsm[2]
        no-lock,
        
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq job-hdr.i-no
        no-lock  
        
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              by job-hdr.i-no:

      v-qty[1] = v-qty[1] + job-hdr.qty.
      
      if last-of(job-hdr.i-no) then do:
        for each fg-act where fg-act.company eq cocode
              and fg-act.job     eq job-hdr.job
              and fg-act.job-no  eq job-hdr.job-no
              and fg-act.job-no2 eq job-hdr.job-no2
              and fg-act.i-no    eq job-hdr.i-no
            use-index job-idx no-lock:
            v-qty[2] = v-qty[2] + fg-act.qty.  
        end.
        
        if v-qty[2] lt v-qty[1] then do:
          create tt-report.
          assign
           tt-report.key-01  = string(if v-sort eq "D" then
                                        string(year(job.start-date),"9999") +
                                        string(month(job.start-date),"99")  +
                                        string(day(job.start-date),"99")
                                   ELSE "xxxxxxxx") +
                            STRING(if v-sort eq "S" then cust.sman
                                   else "","xxx") +
                            cust.cust-no
           tt-report.key-02  = if v-sumdet then job-hdr.i-no
                            else (trim(job.job-no) + "-" +
                                  string(job.job-no2,"99"))
           tt-report.key-03  = job-hdr.i-no
           tt-report.key-04  = string(v-qty[1] - v-qty[2],"9999999999")
           tt-report.key-05  = job-hdr.cust-no
           tt-report.rec-id  = recid(job).
        end.
        v-qty = 0.
      end.      
    end.
    
    if v-all and today ge v-fdate[1] and today le v-fdate[2] then
/*     for each itemfg                                                               */
/*         where itemfg.company eq cocode                                            */
/*           and itemfg.i-no    ge v-fitem[1]                                        */
/*           and itemfg.i-no    le v-fitem[2]                                        */
/*           and itemfg.cust-no ge v-fcust[1]                                        */
/*           and itemfg.cust-no le v-fcust[2]                                        */
/*           and (itemfg.q-onh  gt 0 or itemfg.q-ono gt 0)                           */
/*           and not can-find(first tt-report where tt-report.key-03 eq itemfg.i-no) */
/*         no-lock,                                                                  */

      for each xoe-ord
          where xoe-ord.company  eq cocode
            and xoe-ord.opened   eq yes
            and xoe-ord.ord-no   ge v-ford-no[1]
            and xoe-ord.ord-no   le v-ford-no[2]
            and xoe-ord.cust-no  ge v-fcust[1]
            and xoe-ord.cust-no  le v-fcust[2]
            and xoe-ord.sman[1]  ge v-fslsm[1]
            and xoe-ord.sman[1]  le v-fslsm[2]
            AND xoe-ord.user-id  GE begin_user
            AND xoe-ord.user-id  LE end_user
          use-index opened no-lock,

          each oe-ordl
          where oe-ordl.company   eq cocode
            and oe-ordl.ord-no    eq xoe-ord.ord-no
            AND oe-ordl.stat      NE "C"
            and oe-ordl.i-no      ge v-fitem[1]
            and oe-ordl.i-no      le v-fitem[2]
            and oe-ordl.req-date  ge v-fdate[1]
            and oe-ordl.req-date  le v-fdate[2]
          NO-LOCK,
      FIRST itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    EQ oe-ordl.i-no
          and itemfg.cust-no EQ xoe-ord.cust-no
          and (itemfg.q-onh  gt 0 or itemfg.q-ono gt 0)
        no-lock,        
        first cust
        where cust.company eq cocode
          and cust.cust-no eq itemfg.cust-no
          and cust.sman    ge v-fslsm[1]
          and cust.sman    le v-fslsm[2]
        no-lock:
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"}
      create tt-report.
      assign
       tt-report.key-01  = string(if v-sort eq "D" then
                                 string(year(today),"9999") +
                                 string(month(today),"99")  +
                                 string(day(today),"99")
                               else "xxxxxxxx") +
                        string(if v-sort eq "S" then cust.sman
                               else "","xxx") +
                        cust.cust-no
       tt-report.key-02  = if v-sumdet then itemfg.i-no else ""
       tt-report.rec-id  = recid(itemfg)
       tt-report.key-03  = string(xoe-ord.ord-no)    .
    end.

    IF tb_excel THEN 
    DO:
        excelheader = "".
        IF v-sumdet THEN
        DO:
           IF v-ponum THEN
           DO:
               excelheader = "Sales/Cust,Due Date,Order Date,".
               excelheader = IF v-ord-job 
                             THEN excelheader + "Order Number,"
                             ELSE excelheader + "Job Number,".
               excelheader = excelheader + "PO Number,Description,Item Number,".
               excelheader = excelheader + "Qty OH, Qty Due,".


           END.
           ELSE
           DO:
               excelheader = "Sales/Cust,Due Date,Order Date,".
               excelheader = IF v-ord-job 
                             THEN excelheader + "Order Number,"
                             ELSE excelheader + "Job Number,".
               excelheader = excelheader + "Description,Item Number,".
               excelheader = excelheader + "Qty OH, Qty Due,".
           END.

           CASE v-price:
               WHEN 1 THEN
                 excelheader = excelheader + "Price,Sales,$,".
               WHEN 2 THEN
               DO:
                 excelheader = excelheader + "Status,".

                 IF v-date EQ "Rel" THEN
                    excelheader = excelheader + "Rel Date,".
                 ELSE
                    excelheader = excelheader + "Last Ship Date,".
                     
                 excelheader = excelheader + "Stat,".
               END.
               OTHERWISE
                 excelheader = excelheader + "PO Qty Received,".
           END CASE.
           excelheader = excelheader + "Last ShipTo".
        END.
        ELSE
        DO: /* not v-sumdet */
            excelheader = IF v-ponum 
                          THEN "Order No.,Date,Customer,Name,Sales Rep,PO #," 
                          ELSE "Order No.,Date,Customer,Name,Sales Rep,".
            excelheader = excelheader + "Description,Estimate#,Qty Due,Status,".
            excelheader = excelheader + "Std. Cost,Sales,GP Dollars,GP%".
            excelheader = excelheader + ",Last Ship ID".
        END.
        
        IF excelheader NE "" THEN
          PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
        
    END.

    for each tt-report
        break by tt-report.key-01 by tt-report.key-02:

        {oe/rep/backlog2.i}
        {custom/statusMsg.i "'Processing Order # ' + string(w-ord.ord-no)"}
        ASSIGN
          v-uom    = "/" + if w-ord.uom ne "" then caps(w-ord.uom) else "EA"
          v-job-no = if v-ord-job 
                     then string(w-ord.ord-no) 
                     else (w-ord.job-no + "-" + string(w-ord.job-no2,"99"))
          v-job-no = if v-job-no eq "-00" then "" else trim(v-job-no).

        if v-sumdet then do:
            if first-of(tt-report.key-01) then do:
                if v-sort eq "S" then put "SalesRep: " w-ord.sman space(4).
                put "Customer: " w-ord.cust-no " - " w-ord.cust-name skip.
            end.

            IF tb_excel THEN 
            DO:
                chrSalesCustAndName = "".
                IF v-sort EQ "S" THEN  
                  chrSalesCustAndName = "SalesRep: " + TRIM(w-ord.sman) + " ".
                chrSalesCustAndName = chrSalesCustAndName + "Customer: " + 
                                      TRIM(w-ord.cust-no) +  " - " + 
                                      TRIM(w-ord.cust-name).
            END.
            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                                 AND oe-ordl.ord-no  = integer(w-ord.ord-no)
                                 AND oe-ordl.i-no    = w-ord.i-no
                                 AND oe-ordl.part-no = w-ord.part-no
                               NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN
                v-last-shipid = get-last-shipto().
            ELSE
                v-last-shipid = "".

            if v-ponum then do:
                IF v-price EQ 1 THEN 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.po-num
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.price    when security-flag
                        v-uom          when security-flag
                        w-ord.t-price  when security-flag
                        v-last-shipid
                      with frame ordhead-po.
                    down with frame ordhead-po.

                    IF tb_excel THEN 
                    DO:
                        IF security-flag THEN
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.price
                            v-uom 
                            w-ord.t-price
                            v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            chrDummy
                            chrDummy
                            chrDummy
                            v-last-shipid.
                    END.
                END.
                ELSE IF v-price EQ 2 THEN 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.po-num
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.stat
                        w-ord.rel-date FORMAT "99/99/99" when w-ord.rel-date ne ?
                        w-ord.rel-stat
                        v-last-shipid
                      with frame ordhead-po-s.
                    down with frame ordhead-po-s.

                    IF tb_excel THEN 
                    DO:
                        IF w-ord.rel-date NE ? THEN
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.stat
                            w-ord.rel-date 
                            w-ord.rel-stat
                            v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.stat
                            chrDummy
                            w-ord.rel-stat
                            v-last-shipid.
                    END.
                END.
                ELSE 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.po-num
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.po-received
                        v-last-shipid
                      with frame ordhead-po-q.
                    down with frame ordhead-po-q.

                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.po-received
                            v-last-shipid.
                END.
            end. /* v-ponum */
            else
            do:
                IF v-price EQ 1 THEN 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.price   when security-flag
                        v-uom
                        w-ord.t-price when security-flag                        
                        v-last-shipid
                      with frame ordhead.
                    down with frame ordhead.

                    IF tb_excel THEN 
                    DO:
                        IF security-flag THEN
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.price
                            v-uom 
                            w-ord.t-price
                            v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            chrDummy
                            v-uom 
                            chrDummy
                            v-last-shipid.
                    END.
                END.
                ELSE IF v-price EQ 2 THEN 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.stat
                        w-ord.rel-date FORMAT "99/99/99" when w-ord.rel-date ne ?
                        w-ord.rel-stat
                        v-last-shipid
                      with frame ordhead-s.
                    down with frame ordhead-s.

                    IF tb_excel THEN 
                    DO:
                        IF w-ord.rel-date NE ? THEN
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.stat
                            w-ord.rel-date 
                            w-ord.rel-stat
                            v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.stat
                            chrDummy
                            w-ord.rel-stat
                            v-last-shipid.
                    END.
                END.
                ELSE 
                DO:
                    display 
                        w-ord.due-date FORMAT "99/99/99"
                        w-ord.ord-date FORMAT "99/99/99"
                        v-job-no
                        w-ord.i-name
                        w-ord.i-no
                        w-ord.qty-onh
                        w-ord.qty-due
                        w-ord.po-received
                        v-last-shipid
                      with frame ordhead-q.
                    down with frame ordhead-q.

                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER "," 
                            chrSalesCustAndName
                            w-ord.due-date 
                            w-ord.ord-date 
                            v-job-no
                            w-ord.i-name
                            w-ord.i-no
                            w-ord.qty-onh
                            w-ord.qty-due
                            w-ord.po-received
                            v-last-shipid.
                END.
            end. /* not v-ponum */
        end. /* v-sumdet */
        else
        do:
            if first-of(tt-report.key-02) then
            do:
                if v-ponum then
                  display 
                      skip(1)
                      v-job-no
                      w-ord.due-date FORMAT "99/99/99"
                      w-ord.cust-no
                      w-ord.cust-name
                      w-ord.sman
                      w-ord.po-num
                    with frame detailhead-po.
                else
                  display 
                      skip(1)
                      v-job-no
                      w-ord.due-date FORMAT "99/99/99"
                      w-ord.cust-no
                      w-ord.cust-name
                      w-ord.sman
                    with frame detailhead.
            end.

            assign
              v-gpdollar = (w-ord.t-price - w-ord.cost)
              v-gp       = round(v-gpdollar / w-ord.t-price * 100,2).

            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                                 AND oe-ordl.ord-no  = integer(w-ord.ord-no)
                                 AND oe-ordl.i-no    = w-ord.i-no
                                 AND oe-ordl.part-no = w-ord.part-no
                               NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN
                v-last-shipid = get-last-shipto().
            ELSE
                v-last-shipid = "".
            display 
                w-ord.i-name
                w-ord.est-no
                w-ord.qty-due
                w-ord.stat
                w-ord.cost when v-profit and security-flag
                w-ord.t-price when security-flag
                v-gpdollar when v-profit and security-flag
                v-gp       when v-profit and security-flag
                v-last-shipid COLUMN-LABEL "Last!ShipId"
              with frame ordline.
            down with frame ordline.

            IF tb_excel THEN 
            DO:
                IF v-ponum THEN 
                DO:
                    IF v-profit THEN
                    DO:
                       IF security-flag THEN
                         EXPORT STREAM excel DELIMITER "," 
                            v-job-no
                            w-ord.due-date 
                            w-ord.cust-no
                            w-ord.cust-name
                            w-ord.sman
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.est-no
                            w-ord.qty-due
                            w-ord.stat
                            w-ord.cost 
                            w-ord.t-price 
                            v-gpdollar 
                            v-gp
                            v-last-shipid.
                       ELSE
                         EXPORT STREAM excel DELIMITER "," 
                            v-job-no
                            w-ord.due-date 
                            w-ord.cust-no
                            w-ord.cust-name
                            w-ord.sman
                            w-ord.po-num
                            w-ord.i-name
                            w-ord.est-no
                            w-ord.qty-due
                            w-ord.stat
                            chrDummy
                            w-ord.t-price 
                            chrDummy
                            chrDummy
                            v-last-shipid.
                    END.
                    ELSE
                    DO: /* NOT v-profit */
                        IF security-flag THEN
                          EXPORT STREAM excel DELIMITER "," 
                              v-job-no
                              w-ord.due-date 
                              w-ord.cust-no
                              w-ord.cust-name
                              w-ord.sman
                              w-ord.po-num
                              w-ord.i-name
                              w-ord.est-no
                              w-ord.qty-due
                              w-ord.stat
                              chrDummy
                              w-ord.t-price 
                              chrDummy
                              chrDummy
                              v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                              v-job-no
                              w-ord.due-date 
                              w-ord.cust-no
                              w-ord.cust-name
                              w-ord.sman
                              w-ord.po-num
                              w-ord.i-name
                              w-ord.est-no
                              w-ord.qty-due
                              w-ord.stat
                              chrDummy
                              chrDummy
                              chrDummy
                              chrDummy
                              v-last-shipid.
                    END.
                END.
                ELSE /* NOT v-ponum */
                DO:
                    IF v-profit THEN
                    DO:
                       IF security-flag THEN
                         EXPORT STREAM excel DELIMITER "," 
                            v-job-no
                            w-ord.due-date 
                            w-ord.cust-no
                            w-ord.cust-name
                            w-ord.sman
                            w-ord.i-name
                            w-ord.est-no
                            w-ord.qty-due
                            w-ord.stat
                            w-ord.cost 
                            w-ord.t-price 
                            v-gpdollar 
                            v-gp
                            v-last-shipid.
                       ELSE
                         EXPORT STREAM excel DELIMITER "," 
                            v-job-no
                            w-ord.due-date 
                            w-ord.cust-no
                            w-ord.cust-name
                            w-ord.sman
                            w-ord.i-name
                            w-ord.est-no
                            w-ord.qty-due
                            w-ord.stat
                            chrDummy
                            w-ord.t-price 
                            chrDummy
                            chrDummy
                            v-last-shipid.
                    END.
                    ELSE
                    DO: /* NOT v-profit */
                        IF security-flag THEN
                          EXPORT STREAM excel DELIMITER "," 
                              v-job-no
                              w-ord.due-date 
                              w-ord.cust-no
                              w-ord.cust-name
                              w-ord.sman
                              w-ord.i-name
                              w-ord.est-no
                              w-ord.qty-due
                              w-ord.stat
                              chrDummy
                              w-ord.t-price 
                              chrDummy
                              chrDummy
                              v-last-shipid.
                        ELSE
                          EXPORT STREAM excel DELIMITER "," 
                              v-job-no
                              w-ord.due-date 
                              w-ord.cust-no
                              w-ord.cust-name
                              w-ord.sman
                              w-ord.i-name
                              w-ord.est-no
                              w-ord.qty-due
                              w-ord.stat
                              chrDummy
                              chrDummy
                              chrDummy
                              chrDummy
                              v-last-shipid.
                    END.
                END.
            END.
        end. /* not v-sumdet */

        assign
          v-tot-qty[1]   = v-tot-qty[1] + w-ord.qty
          v-tot-cost[1]  = v-tot-cost[1] + w-ord.cost
          v-tot-sales[1] = v-tot-sales[1] + w-ord.t-price.

        if last-of(tt-report.key-02) then do:
            if v-sumdet and v-sub-item and v-sort ne "D" then do:
                if line-counter + 4 gt page-size then page.

                if v-tot-cost[1] eq ? then v-tot-cost[1] = 0.
                v-tot-pct = if v-tot-sales[1] ne 0 
                            THEN (v-tot-sales[1] - v-tot-cost[1]) / v-tot-sales[1] * 100 
                            ELSE 0.

                put skip(1)
                    "Order Qty"    to 42
                    "Cost"         to 58
                    "Sales"        to 74
                    "GP%"          to 84 skip
                    "ITEM TOTALS:" to 21
                    v-tot-qty[1]   to 42.

                if v-profit then put v-tot-cost[1] to 58.
                IF security-flag THEN PUT v-tot-sales[1] to 74.
                if v-profit AND security-flag THEN put v-tot-pct to 84.
                put skip(1).
            end.

            assign
              v-tot-qty[2]   = v-tot-qty[2] + v-tot-qty[1]
              v-tot-cost[2]  = v-tot-cost[2] + v-tot-cost[1]
              v-tot-sales[2] = v-tot-sales[2] + v-tot-sales[1]
              v-tot-qty[1]   = 0
              v-tot-cost[1]  = 0
              v-tot-sales[1] = 0.
        end.

      if last-of(tt-report.key-01) and
         (v-sumdet or last(tt-report.key-01)) then put skip(1).

      if last(tt-report.key-01) then do:
        if line-counter + 4 gt page-size then page.

        if v-tot-cost[2] eq ? then v-tot-cost[2] = 0.
        v-tot-pct = if v-tot-sales[2] ne 0 then
                      (v-tot-sales[2] - v-tot-cost[2]) /
                       v-tot-sales[2] * 100 else 0.
        put skip(1)
            "Order Qty"     to 42
            "Cost"          to 58
            "Sales"         to 74
            "GP%"           to 84 skip
            "GRAND TOTALS:" to 21
            v-tot-qty[2]    to 42.

        if v-profit then put v-tot-cost[2] to 58.
        IF security-flag  THEN PUT v-tot-sales[2] to 74.
        if v-profit AND security-flag then put v-tot-pct to 84.
        put skip(1).
      end.
      delete w-ord.
    end.
