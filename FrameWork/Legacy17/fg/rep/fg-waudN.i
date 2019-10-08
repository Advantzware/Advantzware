/* ---------------------------------------------- fg/rep/fg-waud.i 06/2011    */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG                            */
/* -------------------------------------------------------------------------- */


   first itemfg where rowid(itemfg) eq tt-itemfg.row-id no-lock,
   each tt-fg-bin
        where tt-fg-bin.company eq itemfg.company
          and tt-fg-bin.i-no    eq itemfg.i-no
          and tt-fg-bin.job-no  eq tt-itemfg.job-no
          and tt-fg-bin.job-no2 eq tt-itemfg.job-no2
          and tt-fg-bin.loc     eq tt-itemfg.loc
          and tt-fg-bin.loc-bin eq tt-itemfg.loc-bin
          and tt-fg-bin.tag     eq tt-itemfg.tag
          and tt-fg-bin.cust-no eq tt-itemfg.bin-cust-no
        use-index co-ino NO-LOCK
      break by {1}
            BY tt-itemfg.loc
            BY tt-itemfg.loc-bin
            BY tt-itemfg.cases
            BY tt-itemfg.case-count
               {3}
            :

    IF FIRST-OF({1}) THEN
      STATUS DEFAULT "Processing:  " + TRIM(STRING({1})) + "/" + TRIM(STRING({2})). 

    if first-of({1}) then v-first[2] = yes.

    if first-of({2}) then do:
      assign
       v-first[1] = yes
       v-tot-sum  = 0
       v-ext-sum  = 0
       v-qoh      = 0.
      
    end.

    assign
     v-procat = itemfg.procat
     v-bin    = no.

    
    lv-rct-date = tt-fg-bin.first-date.

    assign
       v-costl = tt-fg-bin.std-lab-cost * tt-fg-bin.qty
       v-costm = tt-fg-bin.std-mat-cost * tt-fg-bin.qty 
       v-cost1 = if v-dl-mat then (tt-fg-bin.std-lab-cost + tt-fg-bin.std-mat-cost)
                             else tt-fg-bin.std-tot-cost
       v-cost  = v-cost1             * tt-fg-bin.qty.

                                                  /* Calculate Cost */
    if tt-fg-bin.pur-uom eq "CS" and tt-fg-bin.case-count ne 0 then
        assign
         v-costl = v-costl / tt-fg-bin.case-count
         v-costm = v-costm / tt-fg-bin.case-count
         v-cost  = v-cost  / tt-fg-bin.case-count.
    else
      if tt-fg-bin.pur-uom eq "L" then
        assign
         v-costl = v-costl / tt-fg-bin.qty
         v-costm = v-costm / tt-fg-bin.qty
         v-cost  = v-costm / tt-fg-bin.qty.
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

    IF tt-fg-bin.po-no NE "" AND NOT v-fgprice THEN
    DO:
         FIND FIRST po-ordl WHERE
              po-ordl.company EQ tt-fg-bin.company AND
              po-ordl.po-no EQ INT(tt-fg-bin.po-no) AND
              po-ordl.i-no EQ tt-fg-bin.i-no
              NO-LOCK NO-ERROR.

         IF AVAIL po-ordl THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ tt-fg-bin.company AND
                 oe-ordl.ord-no EQ po-ordl.ord-no AND
                 oe-ordl.i-no EQ tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
    END.

    ELSE IF TRIM(tt-fg-bin.job-no) NE "" AND NOT v-fgprice THEN
    DO:
         v-found-job = NO.

         FOR EACH job-hdr FIELDS(ord-no company i-no)
             WHERE job-hdr.company EQ tt-fg-bin.company
               AND job-hdr.job-no  EQ tt-fg-bin.job-no
               AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
               AND job-hdr.i-no    EQ tt-fg-bin.i-no
               AND job-hdr.ord-no  NE 0
             USE-INDEX job-no NO-LOCK,
             FIRST oe-ordl FIELDS(price pr-uom cas-cnt disc)
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no
               AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
             USE-INDEX item-ord NO-LOCK
             BY job-hdr.ord-no DESC:

           ASSIGN
            lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
            lv-sell-uom   = oe-ordl.pr-uom
            lv-case-count = oe-ordl.cas-cnt
            v-found-job = YES.
           LEAVE.
         END.

         IF v-found-job = NO THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ tt-fg-bin.company AND
                 oe-ordl.job-no EQ tt-fg-bin.job-no AND
                 oe-ordl.job-no2 EQ tt-fg-bin.job-no2 AND
                 oe-ordl.i-no EQ tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
    END.
                                         /* Calculate Selling Price */
    if lv-sell-uom eq "CS" and lv-case-count ne 0 then
        v-ext = (tt-fg-bin.qty * lv-sell-price) / lv-case-count.
    else do:
        find first uom
            where uom.uom  eq lv-sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-ext = tt-fg-bin.qty * lv-sell-price /
                (if avail uom then uom.mult else 1000).
    end.

    if itemfg.sell-uom eq "L" then
        if tt-fg-bin.qty le 0 then v-ext = 0.
        else v-ext = lv-sell-price.
        
    v-ext = round(v-ext,2).  

      if v-costl eq ? then v-costl = 0.
      if v-costm eq ? then v-costm = 0.
      if v-cost  eq ? then v-cost  = 0.
      if v-ext   eq ? then v-ext   = 0.

      assign
       v-qoh     = tt-fg-bin.qty
       v-tot-sum = if v-dl-mat then v-costl else v-cost
       v-ext-sum = if v-dl-mat then v-costm else v-ext.

      IF v-prt-msf THEN v-qoh = v-qoh * itemfg.t-sqft / 1000.

   

    /*if zbal or v-qoh ne 0 then*/ do:
        if tt-fg-bin.job-no ne "" then
          v-job-no = trim(tt-fg-bin.job-no) + "-" + string(tt-fg-bin.job-no2,"99").
        else
          v-job-no = "".

        ASSIGN
          v-arq = 0.

        /*IF v-prt-arqty THEN*/
        FOR EACH oe-relh FIELDS(company r-no)
            WHERE oe-relh.company EQ tt-fg-bin.company
              AND oe-relh.posted  EQ NO
              AND oe-relh.rel-date = TODAY
            USE-INDEX post NO-LOCK,
            EACH oe-rell FIELDS(qty)
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
              AND oe-rell.i-no    EQ tt-fg-bin.i-no
              AND oe-rell.loc     EQ tt-fg-bin.loc
              AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin
              AND oe-rell.tag     EQ tt-fg-bin.tag
              AND oe-rell.cust-no EQ tt-fg-bin.cust-no
            USE-INDEX r-no NO-LOCK:
          v-arq = v-arq + oe-rell.qty.
        END.
        FOR EACH oe-rel NO-LOCK WHERE oe-rel.company = cocode
                               AND oe-rel.i-no = tt-fg-bin.i-no
                               AND oe-rel.rel-date = TODAY
                               AND INDEX("A,B,P",oe-rel.stat) > 0 :
            v-arq = v-arq + oe-rel.tot-qty.
        END.
        ASSIGN v-qoh = v-qoh - v-arq
               v-bin-arq = v-bin-arq + v-arq
               v-qoh-s = STRING(v-qoh,v-qoh-f).

        ASSIGN v-bin-qoh = v-bin-qoh + v-qoh
               v-tot-bin-sum = v-tot-bin-sum + v-tot-sum
               v-ext-bin-sum = v-ext-bin-sum + v-ext-sum.

        assign v-tot-qty[1] = v-tot-qty[1] + v-qoh
               v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
               v-tot-ext[1] = v-tot-ext[1] + v-ext-sum.

        do:
           IF v-excel = TRUE THEN DO:
              /* EXPORT STREAM excel DELIMITER ","
                     itemfg.i-no
                     itemfg.i-name 
                      tt-fg-bin.loc 
                      tt-fg-bin.loc-bin
                      tt-itemfg.cases
                      tt-itemfg.case-count
                      v-qoh-s v-arq
                      .*/
           END.
           DO:
            /*display itemfg.i-no    when v-first[1]
                    itemfg.i-name  when v-first[1]
                    tt-fg-bin.loc
                    tt-fg-bin.loc-bin
                    tt-itemfg.cases @ tt-fg-bin.cases
                    tt-itemfg.case-count @ tt-fg-bin.case-count
                    v-qoh-s                    
                  with frame itemx1.
            down with frame itemx1.
            */
          END.            
        end. /* else do */
        
       assign
             v-prnt  = yes
             v-first = no
             v-bin   = yes.
    end. /* zbal or NE 0 */

    if last-of({2}) then do:
      /* put "-----------" to 94.
       put "ITEM COUNT TOTALS" to 70.

          IF v-prt-msf THEN
            PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 94.
          ELSE
            PUT v-tot-qty[1] TO 94.      
      
        put skip(1).
      */

     /* display itemfg.i-no    
              itemfg.i-name  
              tt-fg-bin.loc
              tt-fg-bin.loc-bin
              tt-itemfg.cases @ tt-fg-bin.cases 
              tt-itemfg.case-count @ tt-fg-bin.case-count FORM "->>>>9" 
              v-tot-qty[1] @ v-qoh-s    FORM "->>,>>>,>>9"             
                with frame itemx1. 
            down with frame itemx1. */

         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item"    THEN cVarValue = string(itemfg.i-no,"x(15)") .
                         WHEN "name"   THEN cVarValue = string(itemfg.i-name,"x(30)").
                         WHEN "whse"   THEN cVarValue = STRING(tt-fg-bin.loc,"x(5)").
                         WHEN "bin"  THEN cVarValue = STRING(tt-fg-bin.loc-bin,"x(9)") .
                         WHEN "unit"   THEN cVarValue = STRING(tt-itemfg.cases,"->>>>>>>9") .
                         WHEN "count"  THEN cVarValue = STRING(tt-itemfg.case-count,"->>>>>>9") .
                         WHEN "tot"   THEN cVarValue = STRING(v-tot-qty[1],"->>,>>>,>>>,>>>,>>9") .
                         WHEN "tag"   THEN cVarValue = STRING(tt-itemfg.tag) .
                       
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF v-excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

      assign
       v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
       v-tot-cst[2] = v-tot-cst[2] + v-tot-cst[1]
       v-tot-ext[2] = v-tot-ext[2] + v-tot-ext[1]
       v-tot-qty[1] = 0
       v-tot-cst[1] = 0
       v-tot-ext[1] = 0
       v-prnt[1]    = no.
    end.

    if last-of({1}) then do:
      /* put "ITEM TOTALS" to 70.

       IF v-prt-msf THEN
            PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 96.
          ELSE
            PUT v-tot-qty[2] TO 96.*/
         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item"    THEN cVarValue = "".
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "whse"   THEN cVarValue = "".
                         WHEN "bin"  THEN cVarValue = "" .
                         WHEN "unit"   THEN cVarValue = "" .
                         WHEN "count"  THEN cVarValue = ""  .
                         WHEN "tot"   THEN cVarValue = STRING(v-tot-qty[2] ,"->>,>>>,>>>,>>>,>>9") .
                         WHEN "tag"   THEN cVarValue = "" .
                       
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED  "            Item Totals " substring(cDisplay,25,350) SKIP.
            IF v-excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' Item Totals ,' 
                       substring(cExcelDisplay,4,350) SKIP(1).
             END.

               put skip(1).
     

      assign
       v-tot-qty[3] = v-tot-qty[3] + v-tot-qty[2]
       v-tot-cst[3] = v-tot-cst[3] + v-tot-cst[2]
       v-tot-ext[3] = v-tot-ext[3] + v-tot-ext[2]
       
       v-tot-qty[2] = 0
       v-tot-cst[2] = 0
       v-tot-ext[2] = 0
       v-prnt[2]    = no.
    end. /* last of {1} */

/* end ---------------------------------- copr. 1992  advanced software, inc. */

