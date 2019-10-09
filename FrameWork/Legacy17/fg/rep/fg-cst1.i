
      FIRST itemfg WHERE ROWID(itemfg) EQ tt-itemfg.row-id NO-LOCK

      BREAK BY {1}
            BY {2}:

   /* IF FIRST-OF({2}) THEN
      STATUS DEFAULT TRIM(STRING({1})) + "/" + TRIM(STRING({2})). */

      {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"} 

    FOR EACH tt-fg-bin
        WHERE tt-fg-bin.company EQ cocode
          AND tt-fg-bin.i-no    EQ tt-itemfg.i-no
          AND tt-fg-bin.job-no  EQ tt-itemfg.job-no
          AND tt-fg-bin.job-no2 EQ tt-itemfg.job-no2
          AND tt-fg-bin.loc     EQ tt-itemfg.loc
          AND tt-fg-bin.loc-bin EQ tt-itemfg.loc-bin
          AND tt-fg-bin.tag     EQ tt-itemfg.tag
          AND tt-fg-bin.cust-no EQ tt-itemfg.bin-cust-no
        USE-INDEX co-ino NO-LOCK:

        ASSIGN
         v-qty = tt-fg-bin.qty
         v-cst = tt-fg-bin.std-tot-cost * v-qty.

        /* Calculate Cost */
        IF tt-fg-bin.pur-uom EQ "CS" AND tt-fg-bin.case-count NE 0 THEN
          v-cst = v-cst / tt-fg-bin.case-count.
        ELSE
        IF tt-fg-bin.pur-uom EQ "L" THEN v-cst = v-cst / v-qty.
        ELSE DO:
          FIND FIRST uom NO-LOCK
              WHERE uom.uom  EQ itemfg.prod-uom
                AND uom.mult NE 0
              NO-ERROR.
          IF AVAIL uom THEN v-cst  = v-cst / uom.mult.
                       ELSE v-cst = v-cst  / 1000.
        END.

        ASSIGN
         lv-sell-price = itemfg.sell-price
         lv-sell-uom   = itemfg.sell-uom
         lv-case-count = itemfg.case-count.

        IF TRIM(tt-fg-bin.job-no) NE "" THEN
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
        IF lv-sell-uom EQ "CS" AND lv-case-count NE 0 THEN
          v-ext = (v-qty * lv-sell-price) / lv-case-count.
        ELSE DO:
          FIND FIRST uom NO-LOCK
              WHERE uom.uom  EQ lv-sell-uom
                AND uom.mult NE 0
              NO-ERROR.
          v-ext = v-qty * lv-sell-price /
                  (IF AVAIL uom THEN uom.mult ELSE 1000).
        END.

        IF itemfg.sell-uom EQ "L" THEN
          IF v-qty LE 0 THEN v-ext = 0.
          ELSE v-ext = lv-sell-price.
          
        ASSIGN
         v-msf = v-qty * itemfg.t-sqft / 1000
         v-ext = ROUND(v-ext,2).

        IF v-qty EQ ? THEN v-qty = 0.
        IF v-msf EQ ? THEN v-msf = 0.
        IF v-cst EQ ? THEN v-cst = 0.
        IF v-ext EQ ? THEN v-ext = 0.

        ASSIGN
         v-tot-qty[1] = v-tot-qty[1] + v-qty
         v-tot-msf[1] = v-tot-msf[1] + v-msf
         v-tot-cst[1] = v-tot-cst[1] + v-cst
         v-tot-ext[1] = v-tot-ext[1] + v-ext
            
         v-qty = 0.
    END.

    IF LAST-OF({2}) THEN DO: 
      v-tot-per = v-tot-ext[1] / v-tot-msf[1].
      IF v-tot-per EQ ? THEN v-tot-per = 0.

      DISPLAY itemfg.cust-no
              itemfg.i-no
              itemfg.part-no
              tt-fg-bin.loc     WHEN AVAIL tt-fg-bin
              v-tot-qty[1]
              v-tot-msf[1]
              v-tot-cst[1]
              v-tot-ext[1]
              v-tot-per
          WITH FRAME itemx.

      DOWN WITH FRAME itemx.

      IF v-excel THEN  
        PUT STREAM excel UNFORMATTED
            '"' itemfg.cust-no '",' 
            '"' itemfg.i-no '",' 
            '"' itemfg.part-no '",'
            '"' (IF AVAIL tt-fg-bin THEN tt-fg-bin.loc ELSE "") '",'
            '"' v-tot-qty[1] '",'
            '"' v-tot-msf[1] '",'
            '"' v-tot-cst[1] '",'
            '"' v-tot-ext[1] '",'
            '"' v-tot-per '",'
            SKIP.

      ASSIGN
       v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
       v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
       v-tot-cst[2] = v-tot-cst[2] + v-tot-cst[1]
       v-tot-ext[2] = v-tot-ext[2] + v-tot-ext[1]

       v-tot-qty[1] = 0
       v-tot-msf[1] = 0
       v-tot-cst[1] = 0
       v-tot-ext[1] = 0.
    END.

    IF LAST-OF({1}) THEN DO:
      /*v-tot-per = v-tot-ext[2] / v-tot-msf[2].
      IF v-tot-per EQ ? THEN v-tot-per = 0.
      
      PUT SKIP(1).

      DISPLAY "     Sub Totals" @ itemfg.part-no
              v-tot-qty[2]      @ v-tot-qty[1]
              v-tot-msf[2]      @ v-tot-msf[1]
              v-tot-cst[2]      @ v-tot-cst[1]
              v-tot-ext[2]      @ v-tot-ext[1]
              v-tot-per
          WITH FRAME itemx.

      DOWN WITH FRAME itemx.*/

      ASSIGN
       v-tot-qty[3] = v-tot-qty[3] + v-tot-qty[2]
       v-tot-msf[3] = v-tot-msf[3] + v-tot-msf[2]
       v-tot-cst[3] = v-tot-cst[3] + v-tot-cst[2]
       v-tot-ext[3] = v-tot-ext[3] + v-tot-ext[2]
       
       v-tot-qty[2] = 0
       v-tot-msf[2] = 0
       v-tot-cst[2] = 0
       v-tot-ext[2] = 0.
    END.

    IF LAST({1}) THEN DO:
      v-tot-per = v-tot-ext[3] / v-tot-msf[3].
      IF v-tot-per EQ ? THEN v-tot-per = 0.

      PUT SKIP(2).

      DISPLAY "   Grand Totals" @ itemfg.part-no
              v-tot-qty[3]      @ v-tot-qty[1]
              v-tot-msf[3]      @ v-tot-msf[1]
              v-tot-cst[3]      @ v-tot-cst[1]
              v-tot-ext[3]      @ v-tot-ext[1]
              v-tot-per
          WITH FRAME itemx.

      DOWN WITH FRAME itemx.
    END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
