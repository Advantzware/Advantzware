/***************************************************************
     Program: oerep/r-backlN.i
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
          /* tt-report.key-02  = if v-sumdet then oe-ordl.i-no
                               else string(xoe-ord.ord-no,"9999999999") */
           tt-report.key-02  = string(xoe-ord.ord-no,"9999999999")
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
           /*tt-report.key-02  = if v-sumdet then job-hdr.i-no
                            else (trim(job.job-no) + "-" +
                                  string(job.job-no2,"99"))*/
           tt-report.key-02  = (trim(job.job-no) + "-" +
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
       /*tt-report.key-02  = if v-sumdet then itemfg.i-no else ""*/
       tt-report.key-02  = "" 
       tt-report.rec-id  = recid(itemfg)
       tt-report.key-03  = string(xoe-ord.ord-no)    .
    end.

   /* IF tb_excel THEN 
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
        
    END.*/

    for each tt-report
        break by tt-report.key-01 by tt-report.key-02:
         
        {oe/rep/backlog2.i}
            {custom/statusMsg.i "'Processing Order # ' + string(w-ord.ord-no)"} 

        ASSIGN
          v-uom    = "/" + if w-ord.uom ne "" then caps(w-ord.uom) else "EA"
          v-job-no = /*if v-ord-job 
                     then string(w-ord.ord-no) 
                     else*/ (w-ord.job-no + "-" + string(w-ord.job-no2,"99"))
          v-job-no = if v-job-no eq "-00" then "" else trim(v-job-no).

      

          assign
              v-gpdollar = (w-ord.t-price - w-ord.cost)
              v-gp       = round(v-gpdollar / w-ord.t-price * 100,2).
          IF v-gp = ?  THEN ASSIGN v-gp = 0.
    
              ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
           
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "ord"        THEN cVarValue = STRING(w-ord.ord-no) .
                           WHEN "job"        THEN cVarValue = STRING(v-job-no) .
                           WHEN "due-dt"     THEN cVarValue = IF w-ord.due-date NE ? THEN STRING(w-ord.due-date) ELSE "" .
                           WHEN "cust"       THEN cVarValue = STRING(w-ord.cust-no) .
                           WHEN "name"       THEN cVarValue = STRING(w-ord.cust-name) .
                           WHEN "rep"        THEN cVarValue = STRING(w-ord.sman) .
                           WHEN "po"         THEN cVarValue = STRING(w-ord.po-num) .
                           WHEN "dscr"       THEN cVarValue = STRING(w-ord.i-name) .
                           WHEN "est"        THEN cVarValue = STRING(w-ord.est-no) .
                           WHEN "qty-due"    THEN cVarValue = STRING(w-ord.qty-due,"->>>,>>>,>>9") .
                           WHEN "status"     THEN cVarValue = STRING(w-ord.stat) .
                           WHEN "std-cst"    THEN cVarValue = IF v-profit and security-flag THEN STRING(w-ord.cost,"->>>>,>>9.99") ELSE "".
                           WHEN "uom"        THEN cVarValue = IF security-flag THEN STRING(v-uom) ELSE "".
                           WHEN "gp-dolr"    THEN cVarValue = IF v-profit and security-flag THEN STRING(v-gpdollar,"->>>>,>>9.99") ELSE "".
                           WHEN "gp"         THEN cVarValue = IF v-profit and security-flag THEN STRING(v-gp,"->>>>>>") ELSE "".
                           WHEN "lst-shp"    THEN cVarValue = IF v-last-shipid NE ? THEN STRING(v-last-shipid) ELSE "".
                           WHEN "qty-oh"     THEN cVarValue = STRING(w-ord.qty-onh,"->,>>>,>>9") .
                           WHEN "ord-dt"     THEN cVarValue = IF w-ord.ord-date NE ? THEN STRING(w-ord.ord-date) ELSE "" .
                           WHEN "itm-no"     THEN cVarValue = STRING(w-ord.i-no) .
                           WHEN "rel-dt"     THEN cVarValue = IF w-ord.rel-date NE ? THEN STRING(w-ord.rel-date) ELSE "" .
                           WHEN "prc-sal"    THEN cVarValue = IF security-flag THEN STRING(w-ord.price,">>,>>9.99") ELSE "" .
                           WHEN "stat"       THEN cVarValue = STRING(w-ord.rel-stat,"X(4)") .
                           WHEN "po-qty-rec" THEN cVarValue = STRING(w-ord.po-received,"->>,>>>,>>>") .
                           WHEN "t-pric"     THEN cVarValue = IF security-flag THEN STRING(w-ord.t-price,">>>,>>9.99") ELSE "" .
                           WHEN "ord-qty"     THEN cVarValue =  STRING(w-ord.qty,">>>,>>>,>>9").
                                                              
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
          v-tot-qty[1]   = v-tot-qty[1] + w-ord.qty
          v-tot-cost[1]  = v-tot-cost[1] + w-ord.cost
          v-tot-sales[1] = v-tot-sales[1] + w-ord.t-price.

        if last-of(tt-report.key-02) then do:
            if v-sub-item and v-sort ne "D" then do:
                if line-counter + 4 gt page-size then page.

                if v-tot-cost[1] eq ? then v-tot-cost[1] = 0.
                v-tot-pct = if v-tot-sales[1] ne 0 
                            THEN (v-tot-sales[1] - v-tot-cost[1]) / v-tot-sales[1] * 100 
                            ELSE 0.
                IF v-tot-pct = ?  THEN v-tot-pct = 0.
               /* put skip(1)
                    "Order Qty"    to 42
                    "Cost"         to 58
                    "Sales"        to 74
                    "GP%"          to 84 skip
                    "ITEM TOTALS:" to 21
                    v-tot-qty[1]   to 42.

                if v-profit then put v-tot-cost[1] to 58.
                IF security-flag THEN PUT v-tot-sales[1] to 74.
                if v-profit AND security-flag THEN put v-tot-pct to 84.
                put skip(1).*/
                PUT str-line SKIP.
              ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
           
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "ord"        THEN cVarValue = "" .
                           WHEN "job"        THEN cVarValue = "" .
                           WHEN "due-dt"     THEN cVarValue = "" .
                           WHEN "cust"       THEN cVarValue = "" .
                           WHEN "name"       THEN cVarValue = "" .
                           WHEN "rep"        THEN cVarValue = "" .
                           WHEN "po"         THEN cVarValue = "" .
                           WHEN "dscr"       THEN cVarValue = "" .
                           WHEN "est"        THEN cVarValue = "" .
                           WHEN "qty-due"    THEN cVarValue = "" .
                           WHEN "status"     THEN cVarValue = "" .
                           WHEN "std-cst"    THEN cVarValue = IF v-profit and security-flag THEN STRING(v-tot-cost[1],"->>>>,>>9.99") ELSE "".
                           WHEN "uom"        THEN cVarValue =   "".
                           WHEN "gp-dolr"    THEN cVarValue = "".
                           WHEN "gp"         THEN cVarValue = IF v-profit and security-flag THEN STRING(v-tot-pct,"->>>>>>") ELSE "".
                           WHEN "lst-shp"    THEN cVarValue =   "".
                           WHEN "qty-oh"     THEN cVarValue =   "" .
                           WHEN "ord-dt"     THEN cVarValue =   "" .
                           WHEN "itm-no"     THEN cVarValue =   "" .
                           WHEN "rel-dt"     THEN cVarValue =   "" .
                           WHEN "prc-sal"    THEN cVarValue =   "" .
                           WHEN "stat"       THEN cVarValue =   "" .
                           WHEN "po-qty-rec" THEN cVarValue =   "" .
                           WHEN "t-pric"     THEN cVarValue =   IF v-profit and security-flag THEN STRING(v-tot-sales[1],">>>,>>9.99") ELSE "" .
                            WHEN "ord-qty"     THEN cVarValue =  STRING(v-tot-qty[1],">>>,>>>,>>9").
                                                              
                      END CASE.
                        
                      cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                                 FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
              END.
           
              PUT UNFORMATTED "     ITEM TOTALS: " +  substring(cDisplay,19,300) SKIP(1).
            end.

            assign
              v-tot-qty[2]   = v-tot-qty[2] + v-tot-qty[1]
              v-tot-cost[2]  = v-tot-cost[2] + v-tot-cost[1]
              v-tot-sales[2] = v-tot-sales[2] + v-tot-sales[1]
              v-tot-qty[1]   = 0
              v-tot-cost[1]  = 0
              v-tot-sales[1] = 0.
        end.

      /*if last-of(tt-report.key-01) and
         (last(tt-report.key-01)) then put skip(1). */

      if last(tt-report.key-01) then do: 
        if line-counter + 4 gt page-size then page.

        if v-tot-cost[2] eq ? then v-tot-cost[2] = 0.
        v-tot-pct = if v-tot-sales[2] ne 0 then
                      (v-tot-sales[2] - v-tot-cost[2]) /
                       v-tot-sales[2] * 100 else 0.
         IF v-tot-pct = ?  THEN v-tot-pct = 0.

         IF v-tot-sales[2] = ? THEN v-tot-sales[2] = 0.
        /*put skip(1)
            "Order Qty"     to 42
            "Cost"          to 58
            "Sales"         to 74
            "GP%"           to 84 skip
            "GRAND TOTALS:" to 21
            v-tot-qty[2]    to 42.

        if v-profit then put v-tot-cost[2] to 58.
        IF security-flag  THEN PUT v-tot-sales[2] to 74.
        if v-profit AND security-flag then put v-tot-pct to 84.
        put skip(1).*/
            PUT str-line SKIP.
              ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
           
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "ord"        THEN cVarValue = "" .
                           WHEN "job"        THEN cVarValue = "" .
                           WHEN "due-dt"     THEN cVarValue = "" .
                           WHEN "cust"       THEN cVarValue = "" .
                           WHEN "name"       THEN cVarValue = "" .
                           WHEN "rep"        THEN cVarValue = "" .
                           WHEN "po"         THEN cVarValue = "" .
                           WHEN "dscr"       THEN cVarValue = "" .
                           WHEN "est"        THEN cVarValue = "" .
                           WHEN "qty-due"    THEN cVarValue = "" .
                           WHEN "status"     THEN cVarValue = "" .
                           WHEN "std-cst"    THEN cVarValue = IF v-profit and security-flag THEN STRING(v-tot-cost[2],"->>>>,>>9.99") ELSE "".
                           WHEN "uom"        THEN cVarValue =   "".
                           WHEN "gp-dolr"    THEN cVarValue = "".
                           WHEN "gp"         THEN cVarValue = IF v-profit and security-flag THEN STRING(v-tot-pct,"->>>>>>") ELSE "".
                           WHEN "lst-shp"    THEN cVarValue =   "".
                           WHEN "qty-oh"     THEN cVarValue =   "" .
                           WHEN "ord-dt"     THEN cVarValue =   "" .
                           WHEN "itm-no"     THEN cVarValue =   "" .
                           WHEN "rel-dt"     THEN cVarValue =   "" .
                           WHEN "prc-sal"    THEN cVarValue =   "" .
                           WHEN "stat"       THEN cVarValue =   "" .
                           WHEN "po-qty-rec" THEN cVarValue =   "" .
                           WHEN "t-pric"     THEN cVarValue =   IF v-profit and security-flag THEN STRING(v-tot-sales[2],">>>,>>9.99") ELSE "" .
                            WHEN "ord-qty"     THEN cVarValue =  STRING(v-tot-qty[2],">>>,>>>,>>9").
                                                              
                      END CASE.
                        
                      cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                                 FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
              END.
           
              PUT UNFORMATTED "     GRAND TOTALS: " +  substring(cDisplay,20,300) SKIP(1).
         
      end.
      delete w-ord.
    end.
