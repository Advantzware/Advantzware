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
        use-index co-ino NO-LOCK
      break by {1}
            by {2}
            BY tt-itemfg.loc
            BY tt-itemfg.loc-bin
            BY tt-itemfg.job-no
            BY tt-itemfg.job-no2
               {3}
            :


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
      STATUS DEFAULT TRIM(STRING({1})) + "/" + TRIM(STRING({2})).  */

{custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"}

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
               v-bin-arq      = 0.
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

      if v-sort-by-cust eq "Pr" and v-first[2] then page.

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

      ASSIGN
       v-bin-qoh = v-bin-qoh + v-qoh
       v-tot-bin-sum = v-tot-bin-sum + v-tot-sum
       v-ext-bin-sum = v-ext-bin-sum + v-ext-sum.

      assign
       v-tot-qty[1] = v-tot-qty[1] + v-qoh
       v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
       v-tot-ext[1] = v-tot-ext[1] + v-ext-sum.

      if zbal or v-qoh ne 0 then do:
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

        ASSIGN
          v-qoh-s = STRING(v-qoh,v-qoh-f)
          v-arq = 0.

        IF v-summ-bin THEN
           ASSIGN v-qoh-s =  STRING(v-bin-qoh,v-qoh-f).

        IF v-prt-arqty THEN
        FOR EACH oe-relh FIELDS(company r-no)
            WHERE oe-relh.company EQ tt-fg-bin.company
              AND oe-relh.posted  EQ NO
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

        ASSIGN v-bin-arq = v-bin-arq + v-arq.

        IF v-rct-date THEN DO:
          if v-prt-cpn then do:
           IF v-excel = TRUE THEN DO:
              IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN 
                EXPORT STREAM excel DELIMITER ","
                  itemfg.cust-no 
                  itemfg.i-no 
                  /*(IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)*/ " "
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  lv-rct-date
                  (vdat - lv-rct-date)
                  itemfg.part-no
                  itemfg.i-name
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no 
                  v-bin-qoh
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-bin-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-bin-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-bin-arq) ELSE " ").
              ELSE IF NOT v-summ-bin THEN
                EXPORT STREAM excel DELIMITER ","
                 itemfg.cust-no  
                 itemfg.i-no  
                  (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  lv-rct-date
                  (vdat - lv-rct-date)
                  itemfg.part-no           
                  itemfg.i-name  
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").
           END. /* excel */

           IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN DO:
               
             display itemfg.cust-no when v-first[2]
                     itemfg.part-no when V-first[1]
                     itemfg.i-no    when v-first[1]
                     /*tt-fg-bin.tag
                       SUBSTR(tt-fg-bin.tag,16,8)
                                    WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                    @ tt-fg-bin.tag*/ " "
                     fg-lot-val WHEN v-fg-lot
                     lv-rct-date
/*                      (vdat - lv-rct-date) */
                     itemfg.i-name  
                     tt-fg-bin.loc
                     tt-fg-bin.loc-bin
                     v-job-no
                     v-qoh-s
                     tt-fg-bin.pur-uom
                     v-cost1   when v-prt-c
                     v-tot-bin-sum  when v-prt-c
                     v-ext-bin-sum when v-prt-p
                     v-po-no when v-prt-po
                     v-bin-arq when v-prt-arqty
                     with frame itemx5.
             down with frame itemx5.
           END.
           ELSE IF NOT v-summ-bin THEN DO:        
               
             display itemfg.cust-no when v-first[2]
                     itemfg.part-no when v-first[1]
                     itemfg.i-no    when v-first[1]
                     tt-fg-bin.tag
                       SUBSTR(tt-fg-bin.tag,16,8)
                                    WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                    @ tt-fg-bin.tag
                     fg-lot-val WHEN v-fg-lot
                     lv-rct-date
/*                      (vdat - lv-rct-date) */
                     itemfg.i-name  when v-first[1]
                     tt-fg-bin.loc
                     tt-fg-bin.loc-bin
                     v-job-no
                     v-qoh-s
                     tt-fg-bin.pur-uom
                     v-cost1   when v-prt-c
                     v-tot-sum when v-prt-c
                     v-ext-sum when v-prt-p
                     v-po-no when v-prt-po
                     v-arq when v-prt-arqty
                    with frame itemx4.
             down with frame itemx4.
           END. /* else */
          end. /*v-prt-cpn*/
          else do:
           IF v-excel = TRUE THEN DO:
              
            IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN 
               EXPORT STREAM excel DELIMITER ","
                  itemfg.cust-no  
                  itemfg.i-no  
                  /*(IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)*/ " "
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  lv-rct-date
                  (vdat - lv-rct-date)
                  itemfg.i-name 
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no
                  v-bin-qoh
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-bin-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-bin-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-bin-arq) ELSE " ").
            ELSE IF NOT v-summ-bin THEN
              EXPORT STREAM excel DELIMITER ","
                  itemfg.cust-no 
                  itemfg.i-no 
                  (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  lv-rct-date
                  (vdat - lv-rct-date)
                  itemfg.i-name 
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").
           END. /* excel */

           IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN DO:
               
              display itemfg.cust-no when v-first[2]
                    itemfg.i-no    when v-first[1]
                    /*tt-fg-bin.tag
                      SUBSTR(tt-fg-bin.tag,16,8)
                                   WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                   @ tt-fg-bin.tag*/ " "
                    fg-lot-val WHEN v-fg-lot
                    lv-rct-date
                    itemfg.i-name  when v-first[1]
                    tt-fg-bin.loc
                    tt-fg-bin.loc-bin
                    v-job-no
                    v-qoh-s
                    tt-fg-bin.pur-uom
                    v-cost1   when v-prt-c
                    v-tot-bin-sum when v-prt-c
                    v-ext-bin-sum when v-prt-p
                    v-po-no when v-prt-po
                    v-bin-arq when v-prt-arqty
                  with frame itemx33.

              down with frame itemx33.
           END.
           ELSE IF NOT v-summ-bin THEN DO:
               
            display itemfg.cust-no when v-first[2]
                    itemfg.i-no    when v-first[1]
                    tt-fg-bin.tag
                      SUBSTR(tt-fg-bin.tag,16,8)
                                   WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                   @ tt-fg-bin.tag
                    fg-lot-val WHEN v-fg-lot
                    lv-rct-date
                    itemfg.i-name  when v-first[1]
                    tt-fg-bin.loc
                    tt-fg-bin.loc-bin
                    v-job-no
                    v-qoh-s
                    tt-fg-bin.pur-uom
                    v-cost1   when v-prt-c
                    v-tot-sum when v-prt-c
                    v-ext-sum when v-prt-p
                    v-po-no when v-prt-po
                    v-arq when v-prt-arqty
                  with frame itemx3.

              down with frame itemx3.
           END.                    
          end. /* else v-prt-cpn */
        END. /* v-rct-date */
        ELSE
        if v-prt-cpn then do:
          IF v-excel = TRUE THEN DO:                  
              IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN 
                  EXPORT STREAM excel DELIMITER ","
                  itemfg.cust-no  
                  itemfg.i-no  
                  /*(IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)*/ " "
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  itemfg.part-no  
                  itemfg.i-name  
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-bin-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-bin-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-bin-arq) ELSE " ").
              ELSE IF NOT v-summ-bin THEN
                EXPORT STREAM excel DELIMITER ","
                  itemfg.cust-no 
                  itemfg.i-no  
                  (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                     THEN SUBSTR(tt-fg-bin.tag,16,8)
                     ELSE tt-fg-bin.tag)
                  (IF v-fg-lot THEN fg-lot-val ELSE " ")
                  itemfg.part-no  
                  itemfg.i-name  
                  tt-fg-bin.loc 
                  tt-fg-bin.loc-bin
                  v-job-no
                  v-qoh-s
                  tt-fg-bin.pur-uom              
                  (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                  (IF v-prt-c	THEN STRING(v-tot-sum) ELSE " ") 
                  (IF v-prt-p	THEN STRING(v-ext-sum) ELSE " ")                 
                  (IF v-prt-po	THEN v-po-no  ELSE " ")
                  (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").
          END.
          IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN DO:
              
              display itemfg.cust-no when v-first[2]
                      itemfg.i-no    when v-first[1]
                      tt-fg-bin.tag
                      fg-lot-val WHEN v-fg-lot
                      /*SUBSTR(tt-fg-bin.tag,16,8)
                                   WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                   @ tt-fg-bin.tag*/ " "
                      itemfg.part-no when v-first[1]
                      itemfg.i-name  when v-first[1]
                      tt-fg-bin.loc
                      tt-fg-bin.loc-bin
                      v-job-no
                      v-qoh-s
                      tt-fg-bin.pur-uom
                      v-cost1   when v-prt-c
                      v-tot-bin-sum when v-prt-c
                      v-ext-bin-sum when v-prt-p
                      v-po-no when v-prt-po
                      v-bin-arq when v-prt-arqty
                    with frame itemx22.
              down with frame itemx22. 
          END.
          ELSE IF NOT v-summ-bin THEN DO:  
              
            display itemfg.cust-no when v-first[2]
                    itemfg.i-no    when v-first[1]
                    tt-fg-bin.tag
                    SUBSTR(tt-fg-bin.tag,16,8)
                                 WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                 @ tt-fg-bin.tag
                    fg-lot-val WHEN v-fg-lot
                    itemfg.part-no when v-first[1]
                    itemfg.i-name  when v-first[1]
                    tt-fg-bin.loc
                    tt-fg-bin.loc-bin
                    v-job-no
                    v-qoh-s
                    tt-fg-bin.pur-uom
                    v-cost1   when v-prt-c
                    v-tot-sum when v-prt-c
                    v-ext-sum when v-prt-p
                    v-po-no when v-prt-po
                    v-arq when v-prt-arqty
                  with frame itemx2.
            down with frame itemx2.
          END.
        end. /* else */
        else do:
           IF v-excel = TRUE THEN DO:
               IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN     
                  EXPORT STREAM excel DELIMITER ","
                      itemfg.cust-no  
                      itemfg.i-no  
                      /*(IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                         THEN SUBSTR(tt-fg-bin.tag,16,8)
                         ELSE tt-fg-bin.tag)*/ " "
                      (IF v-fg-lot THEN fg-lot-val ELSE " ")
                      itemfg.i-name  
                      tt-fg-bin.loc 
                      tt-fg-bin.loc-bin
                      v-job-no
                      v-qoh-s
                      tt-fg-bin.pur-uom              
                      (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                      (IF v-prt-c	THEN STRING(v-tot-bin-sum) ELSE " ") 
                      (IF v-prt-p	THEN STRING(v-ext-bin-sum) ELSE " ")                 
                      (IF v-prt-po	THEN v-po-no  ELSE " ")
                      (IF v-prt-arqty 	THEN STRING(v-bin-arq) ELSE " "). 

               
               ELSE IF NOT v-summ-bin THEN
                  EXPORT STREAM excel DELIMITER ","
                      itemfg.cust-no 
                      itemfg.i-no 
                      (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                         THEN SUBSTR(tt-fg-bin.tag,16,8)
                         ELSE tt-fg-bin.tag)
                      (IF v-fg-lot THEN fg-lot-val ELSE " ")
                      itemfg.i-name  
                      tt-fg-bin.loc 
                      tt-fg-bin.loc-bin
                      v-job-no
                      v-qoh-s
                      tt-fg-bin.pur-uom              
                      (IF v-prt-c	THEN STRING(v-cost1)  ELSE " ") 
                      (IF v-prt-c	THEN STRING(v-tot-sum) ELSE " ") 
                      (IF v-prt-p	THEN STRING(v-ext-sum) ELSE " ")                 
                      (IF v-prt-po	THEN v-po-no  ELSE " ")
                      (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").
           END.
           

           IF v-summ-bin AND LAST-OF(tt-itemfg.job-no2) THEN DO:
               
               display itemfg.cust-no when v-first[2]
                       itemfg.i-no    when v-first[1]
                       /*tt-fg-bin.tag
                       SUBSTR(tt-fg-bin.tag,16,8)
                                    WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                    @ tt-fg-bin.tag*/ " "
                       fg-lot-val WHEN v-fg-lot
                       itemfg.i-name  when v-first[1]
                       tt-fg-bin.loc
                       tt-fg-bin.loc-bin
                       v-job-no
                       v-qoh-s
                       tt-fg-bin.pur-uom
                       v-cost1   when v-prt-c
                       v-tot-bin-sum when v-prt-c
                       v-ext-bin-sum when v-prt-p
                       v-po-no when v-prt-po
                       v-bin-arq when v-prt-arqty
                     with frame itemx11.
               down with frame itemx11.
           END.
           ELSE IF NOT v-summ-bin THEN DO:
               
            display itemfg.cust-no when v-first[2]
                    itemfg.i-no    when v-first[1]
                    tt-fg-bin.tag
                    SUBSTR(tt-fg-bin.tag,16,8)
                                 WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                                 @ tt-fg-bin.tag
                    fg-lot-val WHEN v-fg-lot
                    itemfg.i-name  when v-first[1]
                    tt-fg-bin.loc
                    tt-fg-bin.loc-bin
                    v-job-no
                    v-qoh-s
                    tt-fg-bin.pur-uom
                    v-cost1   when v-prt-c
                    v-tot-sum when v-prt-c
                    v-ext-sum when v-prt-p
                    v-po-no when v-prt-po
                    v-arq when v-prt-arqty
                  with frame itemx1.
            down with frame itemx1.
          END.            
        end. /* else do */
        
        
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
      IF v-prt-arqty THEN
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
       
      if v-sort-by-cust eq "Pr" and v-first[2] then page.
      
      IF v-rct-date THEN
      if v-prt-cpn then do:
          
           EXPORT STREAM excel DELIMITER ","
               itemfg.cust-no 
               itemfg.part-no            
               itemfg.i-no 
               " "  
               " "
               " "
                itemfg.i-name  
                itemfg.def-loc 
                itemfg.def-loc-bin  
                " " 
                v-qoh-s
                " "                
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-p	THEN "0" ELSE " ")                 
                " "
                (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").

        display itemfg.cust-no      when v-first[2]
                itemfg.part-no      when v-first[1]
                itemfg.i-no         when v-first[1]
                ""                  @ tt-fg-bin.tag
                ""                  @ lv-rct-date
                itemfg.i-name       when v-first[1]
                itemfg.def-loc      @ tt-fg-bin.loc
                itemfg.def-loc-bin  @ tt-fg-bin.loc-bin
                ""                  @ v-job-no
                v-qoh-s
                ""                  @ tt-fg-bin.pur-uom
                0                   when v-prt-c
                                    @ v-cost1
                0                   when v-prt-c
                                    @ v-tot-sum
                0                   when v-prt-p
                                    @ v-ext-sum
                ""                  when v-prt-po
                                    @ v-po-no
                v-arq               when v-prt-arqty
               

            with frame itemx4.

        down with frame itemx4.
                      
      end.
      

      else do:
        IF v-excel = TRUE THEN
           EXPORT STREAM excel DELIMITER ","
               itemfg.cust-no  
               itemfg.i-no  
               " "  
               " "
               " "
               itemfg.i-name  
                itemfg.def-loc 
                itemfg.def-loc-bin  
                " " 
                v-qoh-s
                " "                
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-p	THEN "0" ELSE " ")                 
                 " "
                (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").       
        display itemfg.cust-no      when v-first[2]
                itemfg.i-no         when v-first[1]
                ""                  @ tt-fg-bin.tag
                ""                  @ lv-rct-date
                itemfg.i-name       when v-first[1]
                itemfg.def-loc      @ tt-fg-bin.loc
                itemfg.def-loc-bin  @ tt-fg-bin.loc-bin
                ""                  @ v-job-no
                v-qoh-s
                ""                  @ tt-fg-bin.pur-uom
                0                   when v-prt-c
                                    @ v-cost1
                0                   when v-prt-c
                                    @ v-tot-sum
                0                   when v-prt-p
                                    @ v-ext-sum
                ""                  when v-prt-po
                                    @ v-po-no
                v-arq               when v-prt-arqty
               

            with frame itemx3.

        down with frame itemx3.
                
      end.

      ELSE
      if v-prt-cpn then do:
        IF v-excel = TRUE THEN
           EXPORT STREAM excel DELIMITER ","
               itemfg.cust-no  
               itemfg.part-no  
               itemfg.i-no  
               itemfg.i-name  
                itemfg.def-loc 
                itemfg.def-loc-bin  
                " " 
                " "                 
                v-qoh-s
                " "                
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-p	THEN "0" ELSE " ")                 
                " "
                (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").        
        display itemfg.cust-no      when v-first[2]
                itemfg.part-no      when v-first[1]
                itemfg.i-no         when v-first[1]
                itemfg.i-name       when v-first[1]
                itemfg.def-loc      @ tt-fg-bin.loc
                itemfg.def-loc-bin  @ tt-fg-bin.loc-bin
                ""                  @ tt-fg-bin.tag
                ""                  @ v-job-no
                v-qoh-s
                ""                  @ tt-fg-bin.pur-uom
                0                   when v-prt-c
                                    @ v-cost1
                0                   when v-prt-c
                                    @ v-tot-sum
                0                   when v-prt-p
                                    @ v-ext-sum
                ""                  when v-prt-po
                                    @ v-po-no
                v-arq               when v-prt-arqty
               

            with frame itemx2.

        down with frame itemx2.
        
        
      end.

      else do:
        IF v-excel = TRUE THEN
           EXPORT STREAM excel DELIMITER ","
               itemfg.cust-no  
               itemfg.i-no  
               itemfg.i-name  
                itemfg.def-loc 
                itemfg.def-loc-bin  
                " " 
                " "                 
                 v-qoh-s
                " "                
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-c	THEN "0" ELSE " ") 
                (IF v-prt-p	THEN "0" ELSE " ")                 
                " "
                (IF v-prt-arqty 	THEN STRING(v-arq) ELSE " ").        
        display itemfg.cust-no      when v-first[2]
                itemfg.i-no         when v-first[1]
                itemfg.i-name       when v-first[1]
                itemfg.def-loc      @ tt-fg-bin.loc
                itemfg.def-loc-bin  @ tt-fg-bin.loc-bin
                ""                  @ tt-fg-bin.tag
                ""                  @ v-job-no
                v-qoh-s
                ""                  @ tt-fg-bin.pur-uom
                0                   when v-prt-c
                                    @ v-cost1
                0                   when v-prt-c
                                    @ v-tot-sum
                0                   when v-prt-p
                                    @ v-ext-sum
                ""                  when v-prt-po
                                    @ v-po-no
                v-arq               when v-prt-arqty
               
            with frame itemx1.
        down with frame itemx1.
        
      end.
      
      v-prnt = yes.
    end. /* if zbal and not v-bin */

    
    if last-of({2}) then do:
        
      if "{2}" ne "1" and v-sort-by-cust ne "PA" and
         v-prnt[1] and v-subt                    then do:
        
        IF v-rct-date THEN
        if v-prt-cpn then do:
            IF v-summ-bin THEN DO:
                put "-----------" to 128.
                if v-prt-c then put "----------" to 153.
                if v-prt-p then put "-----------" to 166 skip.
                
                put "ITEM TOTALS" to 113.
                
                IF v-prt-msf THEN
                    PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 127.
                ELSE
                    PUT v-tot-qty[1] TO 127.
                    if v-prt-c then put v-tot-cst[1] to 153.
                    if v-prt-p then put v-tot-ext[1] to 166.
            END.
            ELSE do:
                put "-----------" to 134.
                if v-prt-c then put "----------" to 160.
                if v-prt-p then put "-----------" to 173 skip.
                
                put "ITEM TOTALS" to 120.
                
                IF v-prt-msf THEN
                    PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 134.
                ELSE
                    PUT v-tot-qty[1] TO 134.
                    
                    if v-prt-c then put v-tot-cst[1] to 160.
                    if v-prt-p then put v-tot-ext[1] to 173.
            END.
        end.

        else do:
          put "-----------" to 118.
          if v-prt-c then put "----------" to 144.
          if v-prt-p then put "-----------" to 157 skip.

          put "ITEM TOTALS" to 103.

          IF v-prt-msf THEN
            PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 118.
          ELSE
            PUT v-tot-qty[1] TO 118.

          if v-prt-c then put v-tot-cst[1] to 144.
          if v-prt-p then put v-tot-ext[1] to 157.
        end.
    
        ELSE
        if v-prt-cpn then do:
          put "-----------" to 125.
          if v-prt-c then put "----------" to 151.
          if v-prt-p then put "-----------" to 164 skip.

          put "ITEM TOTALS" to 110.

          IF v-prt-msf THEN
            PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 125.
          ELSE
            PUT v-tot-qty[1] TO 125.

          if v-prt-c then put v-tot-cst[1] to 151.
          if v-prt-p then put v-tot-ext[1] to 164.
        end.

        else do:
          put "-----------" to 109.
          if v-prt-c then put "----------" to 135.
          if v-prt-p then put "-----------" to 148 skip.

          put "ITEM TOTALS" to 90.

          IF v-prt-msf THEN
            PUT v-tot-qty[1] FORMAT "->>>,>>9.999" TO 109.
          ELSE
            PUT v-tot-qty[1] TO 109.

          if v-prt-c then put v-tot-cst[1] to 135.
          if v-prt-p then put v-tot-ext[1] to 148.
        end.
        put skip(1).
      /*end.*/

      IF v-excel = TRUE /*AND v-subt*/ THEN DO:
        IF v-rct-date THEN
        if v-prt-cpn then do:
            
         EXPORT STREAM excel DELIMITER ","
         " "
         " "
         " "
             " "
         " "
         " "
         " "
         " "
         " "
         " "
         "ITEM TOTALS" 
         v-tot-qty[1] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[1] ELSE 0)
         (if v-prt-p THEN v-tot-ext[1] ELSE 0).
        end.

        else do:
         EXPORT STREAM excel DELIMITER ","
         " "
         " "
             " "
         " "
         " "
         " "
         " "
         " "
         " "
         "ITEM TOTALS"
         v-tot-qty[1] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[1] ELSE 0)
         (if v-prt-p THEN v-tot-ext[1] ELSE 0).
        end.

        ELSE
        if v-prt-cpn then do:
          EXPORT STREAM excel DELIMITER ","
         " "
         " "
              " "
         " "
         " "
         " "
         " "
         " "
         "ITEM TOTALS" 
         v-tot-qty[1] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[1] ELSE 0)
         (if v-prt-p THEN v-tot-ext[1] ELSE 0).
        end.

        else do:
         EXPORT STREAM excel DELIMITER ","
         " "
         " "
             " "
         " "
         " "
         " "
         " "
         "ITEM TOTALS" 
         v-tot-qty[1] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[1] ELSE 0)
         (if v-prt-p THEN v-tot-ext[1] ELSE 0).
        end.
      END.
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
        
      if v-prnt[2] and v-subt then do:
        IF v-rct-date THEN
        if v-prt-cpn then do:
            IF v-summ-bin THEN DO:
                put "-----------" to 127.
                if v-prt-c then put "----------" to 153.
                if v-prt-p then put "-----------" to 166 skip.

                if v-sort-by-cust eq "Cu" then
                    put "CUST TOTALS" to 113.
                else
                if v-sort-by-cust eq "FG" then
                    put "ITEM TOTALS" to 113.
                else
                if v-sort-by-cust eq "Pa" then
                    put "PART TOTALS" to 113.
                else
                if v-sort-by-cust eq "Pr" then
                    put fill(" ",5 - length(trim(v-procat))) +
                    v-procat + " PROD CAT TOTALS" to 113 format "x(21)".
                else
                    put "WHS/BIN TOTALS" to 113.
                IF v-prt-msf THEN
                    PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 127.
                ELSE
                    PUT v-tot-qty[2] TO 127.
                if v-prt-c then put v-tot-cst[2] to 153.
                if v-prt-p then put v-tot-ext[2] to 166.
            END.
            ELSE do:
                put "-----------" to 134.
                if v-prt-c then put "----------" to 160.
                if v-prt-p then put "-----------" to 173 skip.
                
                if v-sort-by-cust eq "Cu" then
                  put "CUST TOTALS" to 120.
                
                else
                if v-sort-by-cust eq "FG" then
                  put "ITEM TOTALS" to 120.
                
                else
                if v-sort-by-cust eq "Pa" then
                  put "PART TOTALS" to 120.
                
                else
                if v-sort-by-cust eq "Pr" then
                  put fill(" ",5 - length(trim(v-procat))) +
                      v-procat + " PROD CAT TOTALS" to 120 format "x(21)".
                
                else
                  put "WHS/BIN TOTALS" to 120.
                
                IF v-prt-msf THEN
                  PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 134.
                ELSE
                  PUT v-tot-qty[2] TO 134.
                
                if v-prt-c then put v-tot-cst[2] to 160.
                if v-prt-p then put v-tot-ext[2] to 173.
            END.
        end.

        else do:
          put "-----------" to 118.
          if v-prt-c then put "----------" to 144.
          if v-prt-p then put "-----------" to 157 skip.

          if v-sort-by-cust eq "Cu" then
            put "CUST TOTALS" to 103.

          else
          if v-sort-by-cust eq "FG" then
            put "ITEM TOTALS" to 103.

          else
          if v-sort-by-cust eq "Pa" then
            put "PART TOTALS" to 103.

          else
          if v-sort-by-cust eq "Pr" then
            put fill(" ",5 - length(trim(v-procat))) +
                v-procat + " PROD CAT TOTALS" to 103 format "x(21)".

          else
            put "WHS/BIN TOTALS" to 103.

          IF v-prt-msf THEN
            PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 118.
          ELSE
            PUT v-tot-qty[2] TO 118.

          if v-prt-c then put v-tot-cst[2] to 144.
          if v-prt-p then put v-tot-ext[2] to 157.
        end.

        ELSE
        if v-prt-cpn then do:
          put "-----------" to 125.
          if v-prt-c then put "----------" to 151.
          if v-prt-p then put "-----------" to 164 skip.
   
          if v-sort-by-cust eq "Cu" then
            put "CUST TOTALS" to 110.

          else
          if v-sort-by-cust eq "FG" then
            put "ITEM TOTALS" to 110.
          
          else
          if v-sort-by-cust eq "Pa" then
            put "PART TOTALS" to 110.

          else
          if v-sort-by-cust eq "Pr" then
            put fill(" ",5 - length(trim(v-procat))) +
                v-procat + " PROD CAT TOTALS" to 110 format "x(21)".

          else
            put "WHS/BIN TOTALS" to 110.

          IF v-prt-msf THEN
            PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 125.
          ELSE
            PUT v-tot-qty[2] TO 125.

          if v-prt-c then put v-tot-cst[2] to 151.
          if v-prt-p then put v-tot-ext[2] to 164.
        end.

        else do:
          put "-----------" to 109.
          if v-prt-c then put "----------" to 135.
          if v-prt-p then put "-----------" to 148 skip.

          if v-sort-by-cust eq "Cu" then
            put "CUST TOTALS" to 90.

          else
          if v-sort-by-cust eq "FG" then
            put "ITEM TOTALS" to 90.

          else
          if v-sort-by-cust eq "Pa" then
            put "PART TOTALS" to 90.

          else
          if v-sort-by-cust eq "Pr" then
            put fill(" ",5 - length(trim(v-procat))) +
                v-procat + " PROD CAT TOTALS" to 90 format "x(21)".

          else
            put "WHS/BIN TOTALS" to 90.

          IF v-prt-msf THEN
            PUT v-tot-qty[2] FORMAT "->>>,>>9.999" TO 109.
          ELSE
            PUT v-tot-qty[2] TO 109.

          if v-prt-c then put v-tot-cst[2] to 135.
          if v-prt-p then put v-tot-ext[2] to 148.
        end.
        put skip(1).
      /*end.*/

       IF v-excel = TRUE /*AND v-subt*/ THEN DO:
        IF v-rct-date THEN
        if v-prt-cpn then do:
          if v-sort-by-cust eq "Cu" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             "CUST TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          ELSE if v-sort-by-cust eq "FG" then
            EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).
             
          ELSE if v-sort-by-cust eq "Pa" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             " "
             " "
             "PART TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).
             
          ELSE if v-sort-by-cust eq "Pr" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             " "
             v-procat + " PROD CAT TOTALS"  
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).
             
             
          else
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
                 " "
             " "
             " "
             " " 
             " "
             " "
             "WHS/BIN TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          /*IF v-prt-msf THEN
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             " "
             v-tot-qty[2] FORMAT "->>>,>>9.99".*/
          
          end.

        else do:
          if v-sort-by-cust eq "Cu" then
             EXPORT STREAM excel DELIMITER ","
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             " "
             "CUST TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          else
          if v-sort-by-cust eq "FG" then
            EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          else
          if v-sort-by-cust eq "Pa" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             "PART TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0) .
             
          else
          if v-sort-by-cust eq "Pr" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             v-procat + " PROD CAT TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).
             
          else
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
                 " "
             " "
             " "
             " "
             " " 
             " "
             "WHS/BIN TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          /*IF v-prt-msf THEN
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             " "
             v-tot-qty[2] FORMAT "->>>,>>9.99".*/
          
        end.

        ELSE
        if v-prt-cpn then do:
          if v-sort-by-cust eq "Cu" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             "CUST TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          ELSE if v-sort-by-cust eq "FG" then
            EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          ELSE if v-sort-by-cust eq "Pa" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             "PART TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0) .
             
          ELSE if v-sort-by-cust eq "Pr" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             v-procat + " PROD CAT TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).
             
          else
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
                 " "
             " "
             " "
             " "
             " "
             "WHS/BIN TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          /*IF v-prt-msf THEN
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             " "
             v-tot-qty[2] FORMAT "->>>,>>9.99".*/
                       
        END.

        else do:
          if v-sort-by-cust eq "Cu" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             "CUST TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          ELSE if v-sort-by-cust eq "FG" then
            EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          ELSE if v-sort-by-cust eq "Pa" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             "PART TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0) .
             
          ELSE if v-sort-by-cust eq "Pr" then
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
              " "
             " "
             " "
             " "
             " "
             v-procat + " PROD CAT TOTALS" 
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0).

          else
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
                 " "
             " "
             " "
             " "
             "WHS/BIN TOTALS"
             v-tot-qty[2] FORMAT "->>>,>>9.99"
             " "
             " "
             (if v-prt-c then v-tot-cst[2] ELSE 0)
             (if v-prt-p THEN v-tot-ext[2] ELSE 0). 
             
          /*IF v-prt-msf THEN
             EXPORT STREAM excel DELIMITER ","
             " "
             " "
             " "
             " "
             " "
             "ITEM TOTALS" 
             " "
             v-tot-qty[2] FORMAT "->>>,>>9.99".*/
          
        end.
       END.
      END.
         

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

