/* ---------------------------------------------- fg/rep/fg-ibtag.i 08/98 JLF */
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
          and ((fslm EQ "" AND tslm begins "zzz") or 
                can-find(FIRST cust 
                   where cust.company eq itemfg.company
                     and cust.sman ge fslm  
                     and cust.sman le tslm 
                     and cust.cust-no eq itemfg.cust-no)) /*Task# 01221409*/
        use-index co-ino NO-LOCK

      break by {1}
            by {2}
            BY tt-itemfg.loc
            BY tt-itemfg.loc-bin
            BY tt-itemfg.job-no
            BY tt-itemfg.job-no2
               {3}
            :

      {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"}

    FIND FIRST cust
        WHERE cust.company EQ itemfg.company
          AND cust.cust-no EQ itemfg.cust-no
        NO-LOCK NO-ERROR.
            
/*     FIND FIRST fg-rctd                                                */
/*         WHERE fg-rctd.company EQ cocode                               */
/*           and (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E")  */
/*           AND fg-rctd.i-no    EQ tt-fg-bin.i-no                       */
/*           /*AND fg-rctd.job-no  EQ tt-fg-bin.job-no                   */
/*           AND fg-rctd.job-no2 EQ INT(tt-fg-bin.job-no2)*/             */
/*           AND fg-rctd.loc     EQ tt-fg-bin.loc                        */
/*           AND fg-rctd.loc-bin EQ tt-fg-bin.loc-bin                    */
/*           AND fg-rctd.tag     EQ tt-fg-bin.tag                        */
/*           AND fg-rctd.stack-code <> "" NO-LOCK NO-ERROR.              */
/*                                                                       */
/*     IF NOT AVAIL fg-rctd THEN                                         */
/*         FIND FIRST fg-rctd                                            */
/*         WHERE fg-rctd.company EQ cocode                               */
/*           and (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E")  */
/*           AND fg-rctd.i-no    EQ tt-fg-bin.i-no                       */
/*           AND fg-rctd.loc     EQ tt-fg-bin.loc                        */
/*           AND fg-rctd.loc-bin EQ tt-fg-bin.loc-bin                    */
/*           AND fg-rctd.stack-code <> "" NO-LOCK NO-ERROR.              */

     
             v-sales-rep = "" .

            IF AVAIL CUST THEN ASSIGN v-cust-name = cust.NAME .
            ELSE ASSIGN v-cust-name = "" .

            IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:

                 FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                     AND cust-part.i-no = itemfg.i-no
                     AND cust-part.cust-no EQ cust.cust-no
                     NO-LOCK:
         
                     IF cust-part.spare-char-1 NE "" THEN do:
                         FIND FIRST sman WHERE sman.company = itemfg.company
                             AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                         IF AVAIL sman THEN v-sales-rep = sman.sman.
                         LEAVE .
                     END.
                  END. /* end of cust-part */
         
                  IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                      FIND FIRST sman WHERE sman.company = cust.company
                          AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                      IF AVAIL sman THEN v-sales-rep = sman.sman.
                  END.
            END.
            ELSE DO:
                FIND FIRST sman WHERE sman.company = cust.company
                    AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                IF AVAIL sman THEN v-sales-rep = sman.sman.
            END.

      FIND FIRST fg-rdtlh
          WHERE fg-rdtlh.company EQ cocode
            AND fg-rdtlh.i-no    EQ tt-fg-bin.i-no
            AND fg-rdtlh.job-no  EQ tt-fg-bin.job-no
            AND fg-rdtlh.job-no2 EQ INT(tt-fg-bin.job-no2)
            AND fg-rdtlh.loc     EQ tt-fg-bin.loc
            AND fg-rdtlh.loc-bin EQ tt-fg-bin.loc-bin
            AND fg-rdtlh.tag     EQ tt-fg-bin.tag 
            AND fg-rdtlh.stack-code <> "" NO-LOCK NO-ERROR.


      IF NOT AVAIL fg-rdtlh THEN
          FIND FIRST fg-rdtlh
          WHERE fg-rdtlh.company EQ cocode
            AND fg-rdtlh.i-no    EQ tt-fg-bin.i-no
            AND fg-rdtlh.job-no  EQ tt-fg-bin.job-no
            AND fg-rdtlh.job-no2 EQ INT(tt-fg-bin.job-no2)
            AND fg-rdtlh.loc     EQ tt-fg-bin.loc
            AND fg-rdtlh.loc-bin EQ tt-fg-bin.loc-bin
            AND fg-rdtlh.stack-code <> "" NO-LOCK NO-ERROR.

       IF AVAIL fg-rdtlh THEN
           ASSIGN
           fg-lot-val =  fg-rdtlh.stack-code.
           ELSE
               fg-lot-val = "".

  /*  IF FIRST-OF({2}) THEN
      STATUS DEFAULT TRIM(STRING({1})) + "/" + TRIM(STRING({2})). */

    if first-of({1}) then v-first[2] = yes.
     
    if first-of({2}) then do:
      assign
       v-first[1] = yes
       v-tot-sum  = 0
       v-ext-sum  = 0
       v-qoh      = 0.

      if v-sort-by-cust eq "Wh" then v-first[2] = yes.
    end.

    assign
     v-procat = itemfg.procat
     v-bin    = no.

    IF v-summ-bin AND FIRST-OF(tt-itemfg.job-no2) THEN DO:
        ASSIGN v-tot-bin-sum  = 0
               v-ext-bin-sum  = 0
               v-bin-qoh      = 0
               v-bin-msf      = 0
               v-bin-arq      = 0
               v-ord-price    = 0.
    END.  

    /*for each tt-fg-bin
        where tt-fg-bin.company eq itemfg.company
          and tt-fg-bin.i-no    eq itemfg.i-no
          and tt-fg-bin.job-no  eq tt-itemfg.job-no
          and tt-fg-bin.job-no2 eq tt-itemfg.job-no2
          and tt-fg-bin.loc     eq tt-itemfg.loc
          and tt-fg-bin.loc-bin eq tt-itemfg.loc-bin
          and tt-fg-bin.tag     eq tt-itemfg.tag
          and tt-fg-bin.cust-no eq tt-itemfg.bin-cust-no
        use-index co-ino NO-LOCK
        
        BREAK BY tt-fg-bin.job-no
              BY tt-fg-bin.job-no2
              BY tt-fg-bin.loc
              BY tt-fg-bin.loc-bin
              BY tt-fg-bin.tag
              BY tt-fg-bin.cust-no
              BY tt-fg-bin.first-date:
      */

     /* if v-sort-by-cust eq "Pr" and v-first[2] then page.*/

      lv-rct-date = tt-fg-bin.first-date.

      assign
       v-costl = tt-fg-bin.std-lab-cost * tt-fg-bin.qty
       v-costm = tt-fg-bin.std-mat-cost * tt-fg-bin.qty 
       v-cost1 = tt-fg-bin.std-tot-cost
       v-cost  = v-cost1 * tt-fg-bin.qty.

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
       lv-sell-price-fg = itemfg.sell-price
       lv-sell-price = itemfg.sell-price
       lv-sell-uom = itemfg.sell-uom
       lv-sell-uom-fg   = itemfg.sell-uom
       lv-case-count = itemfg.case-count
       lv-sell-price-ord = 0
       lv-sell-value-ord = 0
       lv-sell-value-fg = 0
       .

      IF tt-fg-bin.po-no NE "" /*AND NOT v-fgprice*/ THEN
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
                  lv-sell-price-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
/*                   lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) */
                  lv-sell-uom-ord   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
      END.

      ELSE IF TRIM(tt-fg-bin.job-no) NE "" /*AND NOT v-fgprice*/ THEN
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
            lv-sell-price-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
/*             lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) */
            lv-sell-uom-ord   = oe-ordl.pr-uom
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
                  lv-sell-price-ord = oe-ordl.price * (1 - (oe-ordl.disc / 100))
/*                   lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) */
                  lv-sell-uom-ord   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
      END.
      
      /* Calculate Selling Price - FG */
      if lv-sell-uom-fg eq "CS" and lv-case-count ne 0 then
        ASSIGN 
            v-ext = (tt-fg-bin.qty * lv-sell-price) / lv-case-count
            lv-sell-value-fg = (tt-fg-bin.qty * lv-sell-price-fg) / lv-case-count
            .
        else do:
        find first uom
            where uom.uom  eq lv-sell-uom-fg
              and uom.mult ne 0
            no-lock no-error.
        v-ext = tt-fg-bin.qty * lv-sell-price /
                (if avail uom then uom.mult else 1000).
        lv-sell-value-fg = tt-fg-bin.qty * lv-sell-price-fg /
                (if avail uom then uom.mult else 1000).
      end.

      if lv-sell-uom-fg eq "L" then
        if tt-fg-bin.qty le 0 then 
            ASSIGN 
                v-ext = 0
                lv-sell-value-fg = 0
                .
        else 
            ASSIGN 
                v-ext = lv-sell-price
                lv-sell-value-fg = lv-sell-price-fg   
               .
            
      /* Calculate Selling Price - Order */
      if lv-sell-uom-ord eq "CS" and lv-case-count ne 0 then
        ASSIGN 
            lv-sell-value-ord = (tt-fg-bin.qty * lv-sell-price-ord) / lv-case-count
            .
      else do:
        find first uom
            where uom.uom  eq lv-sell-uom-ord
              and uom.mult ne 0
            no-lock no-error.
        lv-sell-value-ord = tt-fg-bin.qty * lv-sell-price-ord /
                (if avail uom then uom.mult else 1000).
       end.
        
      if lv-sell-uom-ord eq "L" then
        if tt-fg-bin.qty le 0 then 
            lv-sell-value-ord = 0.
        else 
            lv-sell-value-ord = lv-sell-price-ord.
        
/*                                                /* Calculate Selling Price */       */
/*       if lv-sell-uom eq "CS" and lv-case-count ne 0 then                        */
/*         ASSIGN                                                                     */
/*           v-ext = (tt-fg-bin.qty * lv-sell-price) / lv-case-count                  */
/*           lv-sell-value-fg = (tt-fg-bin.qty * lv-sell-price-fg) / lv-case-count    */
/*           lv-sell-value-ord = (tt-fg-bin.qty * lv-sell-price-ord) / lv-case-count. */
/*       else do:                                                                     */
/*         find first uom                                                             */
/*             where uom.uom  eq lv-sell-uom                                          */
/*               and uom.mult ne 0                                                    */
/*             no-lock no-error.                                                      */
/*         v-ext = tt-fg-bin.qty * lv-sell-price /                                    */
/*                 (if avail uom then uom.mult else 1000).                            */
/*         lv-sell-value-ord = tt-fg-bin.qty * lv-sell-price-ord /                    */
/*                 (if avail uom then uom.mult else 1000).                            */
/*         lv-sell-value-fg = tt-fg-bin.qty * lv-sell-price-fg /                      */
/*                 (if avail uom then uom.mult else 1000).                            */
/*       end.                                                                         */
/*                                                                                    */
/*       if itemfg.sell-uom eq "L" then                                               */
/*         if tt-fg-bin.qty le 0 then v-ext = 0.                                      */
/*         else                                                                       */
/*             ASSIGN                                                                 */
/*             v-ext = lv-sell-price                                                  */
/*             lv-sell-value-ord = lv-sell-price-ord                                  */
/*             lv-sell-value-fg = lv-sell-price-fg   .                                */

        ASSIGN
            v-ext = round(v-ext,2)  
            lv-sell-value-ord = ROUND(lv-sell-value-ord,2)
            lv-sell-value-fg = ROUND(lv-sell-value-fg,2)
            lv-sell-value-fg-s = lv-sell-value-fg-s + lv-sell-value-fg
            lv-sell-value-ord-s = lv-sell-value-ord-s + lv-sell-value-ord
            .
      if v-costl eq ? then v-costl = 0.
      if v-costm eq ? then v-costm = 0.
      if v-cost  eq ? then v-cost  = 0.
      if v-ext   eq ? then v-ext   = 0.

      assign
       v-qoh     = tt-fg-bin.qty
       v-tot-sum =  v-cost
       v-ext-sum =   v-ext.
       assign
         v-msf-oh   = 0 
         /*IF v-prt-msf THEN*/ v-msf-oh = v-qoh * itemfg.t-sqft / 1000.

      ASSIGN
       v-bin-qoh = v-bin-qoh + v-qoh
       v-bin-msf = v-bin-msf + v-msf-oh 
       v-tot-bin-sum = v-tot-bin-sum + v-cost1
       v-ext-bin-sum = v-ext-bin-sum + v-cost  .

      assign
       v-tot-qty[1] = v-tot-qty[1] + v-qoh
       v-tot-msf[1] = v-tot-msf[1] + v-msf-oh
       v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
       v-tot-ext[1] = v-tot-ext[1] + v-ext-sum  
       v-tot-mat[1] = v-tot-mat[1] + v-costm
       v-tot-lab[1] = v-tot-lab[1] + v-costl 
       v-tot-fgsell[1] = v-tot-fgsell[1] + lv-sell-value-fg
       v-tot-ordsell[1] = v-tot-ordsell[1] + lv-sell-value-ord
          .

      if zbal or v-qoh ne 0 then do:
        if tt-fg-bin.job-no ne "" then
          v-job-no = trim(tt-fg-bin.job-no) + "-" + string(tt-fg-bin.job-no2,"99").
        else
          v-job-no = "".

        
          find first job-hdr
              where job-hdr.company eq cocode
                and job-hdr.job-no  eq tt-fg-bin.job-no
                and job-hdr.job-no2 eq tt-fg-bin.job-no2
                and job-hdr.i-no    eq tt-fg-bin.i-no
              USE-INDEX job-no no-lock no-error.
          if avail job-hdr then do:
            
              find first oe-ordl
                  where oe-ordl.company eq cocode
                    and oe-ordl.ord-no  eq job-hdr.ord-no
                    and oe-ordl.i-no    eq job-hdr.i-no
                  no-lock no-error.
              if avail oe-ordl then 
                  ASSIGN  
                  v-po-no = oe-ordl.po-no
                  v-ord-price = oe-ordl.price .
            
              find first oe-ord
                  where oe-ord.company eq cocode
                    and oe-ord.ord-no  eq job-hdr.ord-no
                  no-lock no-error.
              if avail oe-ord then v-po-ord = oe-ord.po-no.
            
          end.
         
          ELSE ASSIGN
              v-po-no = ""
              v-po-ord = "".
      

        ASSIGN
          v-qoh-s = STRING(v-qoh,v-qoh-f)
          v-arq = 0.

        IF v-summ-bin THEN
           ASSIGN v-qoh-s =  STRING(v-bin-qoh,v-qoh-f).

      ASSIGN v-last-inv = "".
       IF lProcessLastSale THEN
       FOR EACH ar-invl WHERE ar-invl.i-no EQ itemfg.i-no NO-LOCK, 
           EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK BY ar-inv.inv-date DESC:

           ASSIGN v-last-inv = string(ar-inv.inv-date) .
           LEAVE.
       END.

      ASSIGN v-po-rel = "" . 
        IF lProcessRel THEN
        FOR EACH oe-relh FIELDS(company r-no)
            WHERE oe-relh.company EQ tt-fg-bin.company
              AND oe-relh.posted  EQ NO
            USE-INDEX post NO-LOCK,
            EACH oe-rell FIELDS(qty po-no)
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
              AND oe-rell.i-no    EQ tt-fg-bin.i-no
              AND oe-rell.loc     EQ tt-fg-bin.loc
              AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin
              AND oe-rell.tag     EQ tt-fg-bin.tag
              AND oe-rell.cust-no EQ tt-fg-bin.cust-no
            USE-INDEX r-no NO-LOCK:
          v-arq = v-arq + oe-rell.qty.
          v-po-rel = oe-rell.po-no .
        END.

       FIND FIRST fg-set
           WHERE fg-set.company EQ itemfg.company
           AND fg-set.part-no EQ itemfg.i-no NO-LOCK NO-ERROR .
/*                                                        */
/*         FOR EACH oe-relh FIELDS(company r-no)          */
/*             WHERE oe-relh.company EQ tt-fg-bin.company */
/*               AND oe-relh.posted  EQ NO                */
/*             USE-INDEX post NO-LOCK,                    */
/*             EACH oe-rell FIELDS(qty)                   */
/*             WHERE oe-rell.company EQ oe-relh.company   */
/*               AND oe-rell.r-no    EQ oe-relh.r-no      */
/*               AND oe-rell.i-no    EQ tt-fg-bin.i-no    */
/*               AND oe-rell.loc     EQ tt-fg-bin.loc     */
/*               AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin */
/*               AND oe-rell.tag     EQ tt-fg-bin.tag     */
/*               AND oe-rell.cust-no EQ tt-fg-bin.cust-no */
/*               AND oe-rell.po-no NE ""                  */
/*             USE-INDEX r-no NO-LOCK:                    */
/*           v-po-rel = oe-rell.po-no .                   */
/*           LEAVE .                                      */
/*         END.                                           */

        ASSIGN v-bin-arq = v-bin-arq + v-arq.
        
  IF  v-summ-bin AND  LAST-OF(tt-itemfg.job-no2) THEN do:
        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
              
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
              cDisplay = cDisplay + cTmpField + 
                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".      
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "tag" THEN cVarValue = tt-fg-bin.tag /*(SUBSTR(tt-fg-bin.tag,16,8))*/ .
                WHEN "tag#" THEN cVarValue = (SUBSTR(tt-fg-bin.tag,16,6)) .
                WHEN "fg-lot-val" THEN cvarValue = STRING(fg-lot-val,"x(20)") .
                WHEN "v-job-no" THEN cVarValue = string(v-job-no) .
                WHEN "recdate" THEN cVarValue = IF lv-rct-date NE ? THEN STRING(lv-rct-date) ELSE "" .
                WHEN "days-old" THEN cVarValue = STRING(INT(vdat - lv-rct-date),"->>>>>>9") .
                WHEN "loc" THEN cVarValue = STRING(tt-fg-bin.loc).
                WHEN "bin" THEN cVarValue =     STRING(tt-fg-bin.loc-bin).
                WHEN "msf-on-hand" THEN cVarValue = STRING(v-bin-msf,"->>9.999").
                WHEN "cost-uom" THEN cVarValue = STRING(tt-fg-bin.pur-uom,"x(5)").
                WHEN "rel-qty" THEN cVarValue = STRING(v-bin-arq,"->>,>>>,>>9") .
                WHEN "qty-on-hand" THEN cVarValue = STRING(v-bin-qoh,"->>,>>>,>>9") .
                WHEN "last-sale" THEN cVarValue = STRING(v-last-inv,"x(10)") .
                WHEN "view-po" THEN cVarValue = STRING(v-po-ord,"x(10)") .
                WHEN "line-po" THEN cVarValue = STRING(v-po-no,"x(10)") .
                WHEN "rel-po" THEN cVarValue = STRING(v-po-rel,"x(11)") .
                WHEN "ord-pr" THEN cVarValue = STRING(lv-sell-price-ord,"->>>,>>9.99").
                WHEN "sell-price" THEN cVarValue = STRING(itemfg.sell-price,"->>>,>>9.99").
                WHEN "uom-cost" THEN cVarValue = /*(IF ll-secure THEN STRING(v-tot-bin-sum,"->>>>>9.999") ELSE*/ "" . /*Task# 01271402 */
                WHEN "v-tot-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-ext-bin-sum,"->>>,>>9.99") ELSE "").
                WHEN "lab-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-costl,"->>>,>>9.99") ELSE "") .
                WHEN "mat-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-costm,"->>>,>>9.99") ELSE "") .
                WHEN "sale-rep" THEN cVarValue = string(v-sales-rep,"x(3)").
                WHEN "sell-value-ord" THEN cVarValue = STRING(lv-sell-value-ord-s,"->>,>>>,>>9.99") .    
                WHEN "sell-value-fg" THEN cVarValue = STRING(lv-sell-value-fg-s,"->>,>>>,>>9.99") .           /*Task# 01101401*/
                WHEN "custno" THEN cVarValue = STRING(tt-fg-bin.cust-no,"x(8)") .   
                WHEN "set-header" THEN cVarValue = IF AVAIL fg-set AND v-job-no <> "" THEN STRING(fg-set.set-no,"X(15)") ELSE "" .
                WHEN "qty-per-set" THEN cVarValue = IF AVAIL fg-set AND v-job-no <> "" THEN STRING(fg-set.qtyPerSet) ELSE "" .
                WHEN "cust-name" THEN cVarValue = STRING(v-cust-name,"X(30)") .
                WHEN "units" THEN cVarValue = STRING((tt-fg-bin.qty - tt-fg-bin.partial-count) / tt-fg-bin.case-count).
		WHEN "unit-count" THEN cVarValue = STRING(tt-fg-bin.case-count,">>>,>>9").	
		WHEN "partial" THEN cVarValue = STRING(tt-fg-bin.partial-count,"->>,>>9").	              
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.
     ASSIGN 
         lv-sell-value-fg-s = 0
         lv-sell-value-ord-s = 0.
 END.

 ELSE IF NOT v-summ-bin THEN DO:
     ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
              
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
              cDisplay = cDisplay + cTmpField + 
                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".      
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "tag" THEN cVarValue = tt-fg-bin.tag.
                WHEN "tag#" THEN cVarValue = IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN (SUBSTR(tt-fg-bin.tag,16,5)) ELSE  "" .
                WHEN "fg-lot-val" THEN cvarValue = STRING(fg-lot-val,"x(20)") .
                WHEN "v-job-no" THEN cVarValue = string(v-job-no) .
                WHEN "recdate" THEN cVarValue = IF lv-rct-date NE ? THEN STRING(lv-rct-date) ELSE "" .
                WHEN "days-old" THEN cVarValue = STRING(INT(vdat - lv-rct-date)) .
                WHEN "loc" THEN cVarValue = STRING(tt-fg-bin.loc).
                WHEN "bin" THEN cVarValue =     STRING(tt-fg-bin.loc-bin).
                WHEN "msf-on-hand" THEN cVarValue = STRING(v-msf-oh,"->>9.999").
                WHEN "cost-uom" THEN cVarValue = STRING(tt-fg-bin.pur-uom,"x(5)").
                WHEN "rel-qty" THEN cVarValue = STRING(v-bin-arq,"->>,>>>,>>9") .
                WHEN "qty-on-hand" THEN cVarValue = STRING(tt-fg-bin.qty,"->>,>>>,>>9") .
                WHEN "last-sale" THEN cVarValue = STRING(v-last-inv,"x(10)") .
                WHEN "view-po" THEN cVarValue = STRING(v-po-ord,"x(10)") .
                WHEN "line-po" THEN cVarValue = STRING(v-po-no,"x(10)") .
                WHEN "rel-po" THEN cVarValue = STRING(v-po-rel,"x(11)") .
                WHEN "ord-pr" THEN cVarValue = STRING(lv-sell-price-ord,"->>>,>>9.99").
                WHEN "sell-price" THEN cVarValue = STRING(itemfg.sell-price,"->>>,>>9.99").
                WHEN "uom-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-cost1,"->>>>>9.999") ELSE "") . /*Task# 01271402 */
                WHEN "v-tot-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-cost,"->>>,>>9.99") ELSE "").
                WHEN "lab-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-costl,"->>>,>>9.99") ELSE "") .
                WHEN "mat-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-costm,"->>>,>>9.99") ELSE "") .
                WHEN "sale-rep" THEN cVarValue = string(v-sales-rep,"x(3)").
                WHEN "sell-value-ord" THEN cVarValue = STRING(lv-sell-value-ord,"->>,>>>,>>9.99") .    
                WHEN "sell-value-fg" THEN cVarValue = STRING(lv-sell-value-fg,"->>,>>>,>>9.99") .
                WHEN "custno" THEN cVarValue = STRING(tt-fg-bin.cust-no,"x(8)") .
                WHEN "set-header" THEN cVarValue = IF AVAIL fg-set AND v-job-no <> "" THEN STRING(fg-set.set-no,"X(15)") ELSE "" .
                WHEN "qty-per-set" THEN cVarValue = IF AVAIL fg-set AND v-job-no <> "" THEN STRING(fg-set.qtyPerSet) ELSE "" .
                WHEN "cust-name" THEN cVarValue = STRING(v-cust-name,"X(30)") .
                   
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.
     ASSIGN 
         lv-sell-value-fg-s = 0
         lv-sell-value-ord-s = 0.

 END.  /*  not v-summ-bin */


        IF (v-summ-bin AND LAST-OF(tt-itemfg.job-no2)) OR NOT v-summ-bin THEN
          assign
             v-prnt  = yes
             v-first = no
             v-bin   = yes.

      end. /* zbal or NE 0 */
      
     /*end. each tt-fg-bin */

    if zbal and not v-bin then do:
      v-qoh-s = STRING(0,v-qoh-f).

      v-arq = 0.
      
      FOR EACH oe-relh
          WHERE oe-relh.company EQ itemfg.company
            AND oe-relh.posted  EQ NO
          USE-INDEX post NO-LOCK,
          EACH oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND oe-rell.i-no    EQ itemfg.i-no
          USE-INDEX r-no NO-LOCK:
        v-arq = v-arq + oe-rell.qty.
      END.
       
     /* if v-sort-by-cust eq "Pr" and v-first[2] then page.*/
      
    /*  ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     BUFFER b-itemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

       IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          IF cTmpField = "Cust-no" or
              cTmpField = "Sman" THEN hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
          ELSE hField = BUFFER b-itemfg:BUFFER-FIELD(cTmpField).
              
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
              cDisplay = cDisplay + cTmpField + 
                  FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".      
       END.
       ELSE DO:            
            CASE cTmpField:               
                WHEN "tag" THEN cVarValue = SUBSTRING(tt-fg-bin.tag,1,15) .
                WHEN "fg-lot-val" THEN cvarValue = STRING(fg-lot-val) .
                WHEN "v-job-no" THEN cVarValue = "" .
                WHEN "recdate" THEN cVarValue = "" .
                WHEN "loc" THEN cVarValue = STRING(tt-fg-bin.loc).
                WHEN "bin" THEN cVarValue =     STRING(tt-fg-bin.loc-bin).
                WHEN "msf-on-hand" THEN cVarValue = "".
                WHEN "cost-uom" THEN cVarValue = "".
                WHEN "rel-qty" THEN cVarValue = "" .
                WHEN "qty-on-hand" THEN cVarValue = STRING(v-qoh-s /*,"->>,>>>,>>9"*/ ) .
                WHEN "last-sale" THEN cVarValue = /*STRING(v-rfq,"x(10)")*/ "".
                WHEN "view-po" THEN cVarValue = "" .
                WHEN "line-po" THEN cVarValue = "" .
                WHEN "rel-po" THEN cVarValue = /*STRING(li-ship-qty,"->>>>>>,>>9")*/ "" .
                WHEN "ord-pr" THEN cVarValue = "" .
                
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.*/
      
      v-prnt = yes.
    end. /* if zbal and not v-bin */  

    
    if last-of({2}) then do:
        
      if "{2}" ne "1" and v-sort-by-cust ne "PA" and
         v-prnt[1] and v-subt                    then do:
        
       
         /* put "----------------" to 43.
           put "---------------" to 70.
          if v-prt-c then put "----------------" to 100 .
          if v-prt-p then put "----------------" to 130 .
          if v-prt-p then put "----------------" to 160 skip.

          put "ITEM TOTALS " to 20.

          PUT " QTY  " v-tot-qty[1] FORMAT "->>>,>>>,>>9.999" TO 42.
          PUT "MSF  " TO 55 .
          PUT v-tot-msf[1] TO 70.

          if v-prt-c THEN do:
           PUT "COST " TO 85 .
           put v-tot-cst[1] to 100.
          if v-prt-p then PUT "     MAT COST" v-tot-mat[1] to 130 "     LAB COST   " v-tot-lab[1] .
          END. */
        PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:    
                WHEN "itemfg.cust-no" THEN cVarValue = "" .
                WHEN "itemfg.i-no" THEN cvarValue = "" .
                WHEN "itemfg.part-no" THEN cVarValue = "" .
                WHEN "itemfg.i-name" THEN cVarValue = "" .
                WHEN "itemfg.procat" THEN cVarValue = "" .
                WHEN "tag" THEN cVarValue = "" .
                WHEN "tag#" THEN cVarValue = "" .
                WHEN "fg-lot-val" THEN cvarValue = "" .
                WHEN "v-job-no" THEN cVarValue = "" .
                WHEN "recdate" THEN cVarValue = "" .
                WHEN "days-old" THEN cVarValue = "" .
                WHEN "loc" THEN cVarValue = "".
                WHEN "bin" THEN cVarValue =    "".
                WHEN "msf-on-hand" THEN cVarValue = STRING(v-tot-msf[1],"->>9.999").
                WHEN "cost-uom" THEN cVarValue = "".
                WHEN "rel-qty" THEN cVarValue = "" .
                WHEN "qty-on-hand" THEN cVarValue = STRING(v-tot-qty[1],"->>,>>>,>>9") .
                WHEN "last-sale" THEN cVarValue = "" .
                WHEN "view-po" THEN cVarValue = "" .
                WHEN "line-po" THEN cVarValue = "" .
                WHEN "rel-po" THEN cVarValue = "" .
                WHEN "ord-pr" THEN cVarValue = "".
                WHEN "sell-price" THEN cVarValue = "".
                WHEN "uom-cost" THEN cVarValue = "" .
                WHEN "v-tot-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-cst[1],"->>>,>>9.99") ELSE "").
                WHEN "lab-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-lab[1],"->>>,>>9.99") ELSE "") .
                WHEN "mat-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-mat[1],"->>>,>>9.99") ELSE "") .
                WHEN "sale-rep" THEN cVarValue = "" .           /*Task# 01101401*/
                WHEN "sell-value-ord" THEN cVarValue = STRING(lv-sell-value-ord,"->>,>>>,>>9.99") .    
                WHEN "sell-value-fg" THEN cVarValue = STRING(lv-sell-value-fg,"->>,>>>,>>9.99") .
                WHEN "custno" THEN cVarValue = "" .
                WHEN "set-header" THEN cVarValue = "" .
                WHEN "qty-per-set" THEN cVarValue = "" .
                WHEN "cust-name" THEN cVarValue = "" .
                
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
    END.
    PUT UNFORMATTED  "        ITEM TOTALS" substring(cDisplay,20,300) SKIP.
    
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "ITEM TOTALS " + substring(cExcelDisplay,3,300) SKIP.
     END.
        
        end.  /*  v-prnt[1] and v-subt*/
        put skip(1).
      /*end.*/

      /*IF v-excel = TRUE /*AND v-subt*/ THEN DO:
       
         EXPORT STREAM excel DELIMITER ","
         ""
         "ITEM TOTALS"
         ""
         "QTY"
         v-tot-qty[1] FORMAT "->>>,>>>,>>9.99"
         ""
         "MSF "
         v-tot-msf[1] FORMAT "->>>,>>9.99"
         ""
         (IF v-prt-c THEN "COST " ELSE "")
         (if v-prt-c then v-tot-cst[1] ELSE 0)
         ""
         (IF v-prt-p THEN "MAT Cost " ELSE "")
         (if v-prt-p then v-tot-mat[1] ELSE 0)
         (IF v-prt-p THEN "LAB Cost " ELSE "")
         (if v-prt-p then v-tot-lab[1] ELSE 0)
         "" .
        end.*/

      assign
       v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
       v-tot-cst[2] = v-tot-cst[2] + v-tot-cst[1]
       v-tot-ext[2] = v-tot-ext[2] + v-tot-ext[1]
       v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
       v-tot-mat[2] = v-tot-mat[2] + v-tot-mat[1]
       v-tot-lab[2] = v-tot-lab[2] + v-tot-lab[1]
       v-tot-fgsell[2] = v-tot-fgsell[2] + v-tot-fgsell[1]
       v-tot-ordsell[2] = v-tot-ordsell[2] + v-tot-ordsell[1]

       v-tot-qty[1] = 0
       v-tot-cst[1] = 0
       v-tot-ext[1] = 0
       v-tot-msf[1] = 0
       v-tot-mat[1] = 0
       v-tot-lab[1] = 0
       v-tot-ordsell[1] = 0
       v-tot-fgsell[1] = 0
       v-prnt[1]    = no.
    
    end.

    if last-of({1}) then do: 
        
      if v-prnt[2] and v-subt then do:
        
          PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       
            CASE cTmpField:    
                WHEN "itemfg.cust-no" THEN cVarValue = "" .
                WHEN "itemfg.i-no" THEN cvarValue = "" .
                WHEN "itemfg.part-no" THEN cVarValue = "" .
                WHEN "itemfg.i-name" THEN cVarValue = "" .
                WHEN "itemfg.procat" THEN cVarValue = "" .
                WHEN "tag" THEN cVarValue = "" .
                WHEN "tag#" THEN cVarValue = "" .
                WHEN "fg-lot-val" THEN cvarValue = "" .
                WHEN "v-job-no" THEN cVarValue = "" .
                WHEN "recdate" THEN cVarValue = "" .
                WHEN "days-old" THEN cVarValue = "" .
                WHEN "loc" THEN cVarValue = "".
                WHEN "bin" THEN cVarValue =    "".
                WHEN "msf-on-hand" THEN cVarValue = STRING(v-tot-msf[2],"->>9.999").
                WHEN "cost-uom" THEN cVarValue = "".
                WHEN "rel-qty" THEN cVarValue = "" .
                WHEN "qty-on-hand" THEN cVarValue = STRING(v-tot-qty[2],"->>,>>>,>>9") .
                WHEN "last-sale" THEN cVarValue = "" .
                WHEN "view-po" THEN cVarValue = "" .
                WHEN "line-po" THEN cVarValue = "" .
                WHEN "rel-po" THEN cVarValue = "" .
                WHEN "ord-pr" THEN cVarValue = "".
                WHEN "sell-price" THEN cVarValue = "".
                WHEN "uom-cost" THEN cVarValue = "" .
                WHEN "v-tot-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-cst[2],"->>>,>>9.99") ELSE "").
                WHEN "lab-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-lab[2],"->>>,>>9.99") ELSE "") .
                WHEN "mat-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-mat[2],"->>>,>>9.99") ELSE "") .
                WHEN "sale-rep" THEN cVarValue = "" .
                WHEN "sell-value-ord" THEN cVarValue = STRING(v-tot-ordsell[2],"->>,>>>,>>9.99") .    
                WHEN "sell-value-fg" THEN cVarValue = STRING(v-tot-fgsell[2],"->>,>>>,>>9.99") .
                WHEN "custno" THEN cVarValue = "" .
                WHEN "set-header" THEN cVarValue = "" .
                WHEN "qty-per-set" THEN cVarValue = "" .
                WHEN "cust-name" THEN cVarValue = "" .
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
    END.
    
     IF tb_excel THEN DO:
         if v-sort-by-cust eq "Cu" then
         PUT STREAM excel UNFORMATTED  
               "CUST TOTALS " + substring(cExcelDisplay,3,300) SKIP.
         ELSE if v-sort-by-cust eq "FG" then
             PUT STREAM excel UNFORMATTED  
             "ITEM TOTALS " + substring(cExcelDisplay,3,300) SKIP.
         ELSE if v-sort-by-cust eq "Pa" then
             PUT STREAM excel UNFORMATTED  
             "PART TOTALS " + substring(cExcelDisplay,3,300) SKIP .
         ELSE if v-sort-by-cust eq "Pr" then
             PUT STREAM excel UNFORMATTED  
             "PROD CAT TOTALS " + substring(cExcelDisplay,3,300) SKIP .
         ELSE
             PUT STREAM excel UNFORMATTED  
             "WHS/BIN TOTALS"  + substring(cExcelDisplay,3,300) SKIP.

     END.
   
          if v-sort-by-cust eq "Cu" then
            PUT UNFORMATTED  "        CUST TOTALS" substring(cDisplay,20,300) SKIP.
           
          else
          if v-sort-by-cust eq "FG" then
            PUT UNFORMATTED  "        ITEM TOTALS" substring(cDisplay,20,300) SKIP.
            
          else
          if v-sort-by-cust eq "Pa" then
            PUT UNFORMATTED  "        PART TOTALS" substring(cDisplay,20,300) SKIP.
          
          else
          if v-sort-by-cust eq "Pr" then
              PUT UNFORMATTED  "  "  v-procat FORMAT "x(5)"  " PROD CAT TOTALS" substring(cDisplay,24,300) SKIP.
           
          ELSE
            PUT UNFORMATTED  "     WHS/BIN TOTALS" substring(cDisplay,20,300) SKIP.
           

          
         /* END.*/
                
          put skip(1).
      
       /*END.*/
      END.

      assign
       v-tot-qty[3] = v-tot-qty[3] + v-tot-qty[2]
       v-tot-cst[3] = v-tot-cst[3] + v-tot-cst[2]
       v-tot-ext[3] = v-tot-ext[3] + v-tot-ext[2]
       v-tot-msf[3] = v-tot-msf[3] + v-tot-msf[2]
       v-tot-mat[3] = v-tot-mat[3] + v-tot-mat[2]
       v-tot-lab[3] = v-tot-lab[3] + v-tot-lab[2]
       v-tot-ordsell[3] = v-tot-ordsell[3] + v-tot-ordsell[2]
       v-tot-fgsell[3] = v-tot-fgsell[3] + v-tot-fgsell[2]
       
       v-tot-qty[2] = 0
       v-tot-cst[2] = 0
       v-tot-ext[2] = 0
       v-tot-msf[2] = 0
       v-tot-mat[2] = 0
       v-tot-lab[2] = 0
       v-tot-fgsell[2] = 0
       v-tot-ordsell[2] = 0
       v-prnt[2]    = no.
    
      IF v-page THEN    /* Task 01021405  */
      IF LOOKUP(v-sort-by-cust,"Pr,Wh") <> 0 THEN PAGE.
      

    end. /* last of {1} */  
    

/* end ---------------------------------- copr. 1992  advanced software, inc. */
