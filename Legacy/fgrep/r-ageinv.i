/* fgrep/r-ageinv.i

   Notes:
    - tt-file allows for break by  salesrep, customer, item 
    - tt-fg-bin allows for break by tag, job, loc, bin, i-no
    
*/    

    EMPTY TEMP-TABLE tt-file.

    ld-last = 01/01/0001.

    DEF VAR v-qohj-2 LIKE v-qohj NO-UNDO.
    DEF VAR v-qohi-2 LIKE v-qohi NO-UNDO.
    DEF VAR v-qty-2 LIKE v-qty NO-UNDO.
    DEF VAR v-shipdt AS CHAR EXTENT 2 NO-UNDO.
    DEF VAR v-custpart AS CHAR EXTENT 2 NO-UNDO.    
    DEF VAR v-dates  AS DATE EXTENT 5 NO-UNDO.
    DEF VAR v-sell-price AS DEC EXTENT 6 NO-UNDO.
    DEF VAR v-price AS DEC NO-UNDO.
    DEF VAR v-dr AS INT NO-UNDO.
    DEF VAR v-last-dt AS DATE NO-UNDO.
    DEF VAR lv-num-matches AS INT NO-UNDO.
    DEF VAR v-match-date AS DATE NO-UNDO.

    FOR EACH cust NO-LOCK
        WHERE cust.company          EQ cocode
          AND cust.cust-no          GE fcus
          AND cust.cust-no          LE tcus
          AND cust.sman             GE fslm
          AND cust.sman             LE tslm,

        EACH itemfg NO-LOCK
        WHERE itemfg.company        EQ cust.company
          AND itemfg.cust-no        EQ cust.cust-no
          AND itemfg.i-no           GE fitm
          AND itemfg.i-no           LE titm
        USE-INDEX customer
        
        BREAK BY cust.sman
              BY cust.cust-no :

      IF v-class NE "" AND
         LOOKUP(itemfg.class,v-class) EQ 0 THEN NEXT.

      EMPTY TEMP-TABLE tt-fg-bin.

      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no:
         
        CREATE tt-fg-bin.
        BUFFER-COPY fg-bin TO tt-fg-bin.
      END.

      IF vdat LT TODAY THEN
      FOR EACH fg-rcpth NO-LOCK
          WHERE fg-rcpth.company    EQ itemfg.company
            AND fg-rcpth.i-no       EQ itemfg.i-no
            AND fg-rcpth.trans-date LE vdat
          USE-INDEX tran,
          EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          BY fg-rcpth.trans-date
          BY fg-rdtlh.trans-time:

        IF NOT CAN-FIND(FIRST tt-fg-bin
                        WHERE tt-fg-bin.company EQ fg-rcpth.company
                          AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
                          AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                          AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                          AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                          AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                          AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                          AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no) THEN DO:
          CREATE tt-fg-bin.

          ASSIGN
           tt-fg-bin.company      = fg-rcpth.company
           tt-fg-bin.job-no       = fg-rcpth.job-no
           tt-fg-bin.job-no2      = fg-rcpth.job-no2
           tt-fg-bin.loc          = fg-rdtlh.loc
           tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
           tt-fg-bin.tag          = fg-rdtlh.tag
           tt-fg-bin.cust-no      = fg-rdtlh.cust-no
           tt-fg-bin.i-no         = fg-rcpth.i-no
           tt-fg-bin.aging-date   = fg-rcpth.trans-date
           tt-fg-bin.pur-uom      = itemfg.prod-uom
           tt-fg-bin.std-tot-cost = itemfg.total-std-cost
           tt-fg-bin.std-mat-cost = itemfg.std-mat-cost
           tt-fg-bin.std-lab-cost = itemfg.std-lab-cost
           tt-fg-bin.std-var-cost = itemfg.std-var-cost
           tt-fg-bin.std-fix-cost = itemfg.std-fix-cost.
        END.

        IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
          tt-fg-bin.case-count   = fg-rdtlh.qty-case.
        IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
          tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
        IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
          tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
      END.

      
      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company   EQ cocode
            AND tt-fg-bin.i-no      EQ itemfg.i-no
            AND tt-fg-bin.loc       GE begin_whse
            AND tt-fg-bin.loc       LE end_whse 
            AND  tt-fg-bin.loc-bin  GE begin_loc-bin 
            AND  tt-fg-bin.loc-bin  LE end_loc-bin 
            AND ((tt-fg-bin.cust-no EQ "" AND
                  tt-fg-bin.loc     NE "CUST") OR tb_cust-whse)
            AND STRING(FILL(" ",6 - LENGTH(TRIM(tt-fg-bin.job-no))) +
                       TRIM(tt-fg-bin.job-no) + STRING(tt-fg-bin.job-no2,"99"))
                                    GE fjob
            AND STRING(FILL(" ",6 - LENGTH(TRIM(tt-fg-bin.job-no))) +
                       TRIM(tt-fg-bin.job-no) + STRING(tt-fg-bin.job-no2,"99"))
                                    LE tjob
        USE-INDEX co-ino

        BREAK BY tt-fg-bin.i-no
              BY tt-fg-bin.job-no
              BY tt-fg-bin.job-no2
              BY tt-fg-bin.loc
              BY tt-fg-bin.loc-bin
              BY tt-fg-bin.tag:

        IF TRIM(tt-fg-bin.tag) EQ "" THEN
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company      EQ itemfg.company
              AND fg-rcpth.i-no         EQ itemfg.i-no
              AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
              AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
              AND fg-rcpth.trans-date   LE vdat,

            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
              AND fg-rdtlh.loc          EQ tt-fg-bin.loc
              AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
              AND fg-rdtlh.tag          EQ tt-fg-bin.tag
              AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
              AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
            USE-INDEX rm-rdtl

            BREAK BY fg-rcpth.trans-date
                  BY fg-rdtlh.trans-time
                  BY fg-rcpth.r-no:

          {fg/rep/fg-aging2.i 9999999999}
        END.

        ELSE
        FOR EACH fg-rdtlh
            WHERE fg-rdtlh.company      EQ tt-fg-bin.company
              AND fg-rdtlh.tag          EQ tt-fg-bin.tag
              AND fg-rdtlh.loc          EQ tt-fg-bin.loc
              AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
              AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
            USE-INDEX tag NO-LOCK,

            FIRST fg-rcpth NO-LOCK
            WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
              AND fg-rcpth.i-no         EQ tt-fg-bin.i-no
              AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
              AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
              AND fg-rcpth.rita-code    EQ fg-rdtlh.rita-code
              AND fg-rcpth.trans-date   LE vdat 
            USE-INDEX r-no

            BREAK BY fg-rcpth.trans-date
                  BY fg-rdtlh.trans-time
                  BY fg-rcpth.r-no:

          {fg/rep/fg-aging2.i 9999999999}
        END.

        if last-of(tt-fg-bin.tag) then do:
          ASSIGN
             v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                      v-qohj[4] + v-qohj[5] + v-qohj[6]
             v-qohj-2[1] = v-qohj[1]
             v-qohj-2[2] = v-qohj[2]
             v-qohj-2[3] = v-qohj[3]
             v-qohj-2[4] = v-qohj[4]
             v-qohj-2[5] = v-qohj[5]
             v-qohj-2[6] = v-qohj[6].
          
          if v-qohj[6] lt 0 then do:
            ASSIGN
               v-qty = v-qohj[6] * -1
               v-qohj-2[6] = 0.
          
            do v = 5 to 1 by -1:
              if v-qohj[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.
                
              if v-qty le 0 then leave.
            end.
            
            if v-qty gt 0 then
               ASSIGN v-qohi[6] = v-qohi[6] - v-qty
                      v-qohj-2[6] = v-qohj-2[6] - v-qty.
          end.
                      
          release oe-ordl.
          
          FIND FIRST job-hdr WHERE
               job-hdr.company EQ tt-fg-bin.company AND
               job-hdr.job-no  EQ tt-fg-bin.job-no AND
               job-hdr.job-no2 EQ tt-fg-bin.job-no2 AND
               job-hdr.i-no    EQ tt-fg-bin.i-no AND
               job-hdr.ord-no  NE 0
               NO-LOCK NO-ERROR.

          IF AVAIL job-hdr THEN
          DO:
             FIND FIRST oe-ordl WHERE
                  oe-ordl.company EQ job-hdr.company AND
                  oe-ordl.ord-no  EQ job-hdr.ord-no AND
                  oe-ordl.i-no    EQ job-hdr.i-no
                  NO-LOCK NO-ERROR.
             RELEASE job-hdr.
          END.

          IF NOT AVAIL oe-ordl THEN
             FIND LAST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ cocode
                   AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                   AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                   AND oe-ordl.i-no    EQ fg-rcpth.i-no
                 USE-INDEX item NO-ERROR.
              
          if not v-curr then
            assign
             v-qohj[1] = 0
             v-qohj[2] = 0
             v-qohj[3] = 0
             v-qohj-2[1] = 0
             v-qohj-2[2] = 0
             v-qohj-2[3] = 0.

          assign
           v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
           v-qty-2   = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5] + v-qohj[6]
           v-qohi[1] = v-qohi[1] + v-qohj[1]
           v-qohi[2] = v-qohi[2] + v-qohj[2]
           v-qohi[3] = v-qohi[3] + v-qohj[3]
           v-qohi[4] = v-qohi[4] + v-qohj[4]
           v-qohi[5] = v-qohi[5] + v-qohj[5]
           v-qohi-2[1] = v-qohi[1]
           v-qohi-2[2] = v-qohi[2]
           v-qohi-2[3] = v-qohi[3]
           v-qohi-2[4] = v-qohi[4]
           v-qohi-2[5] = v-qohi[5]
           v-qohi-2[6] = v-qohi[6]
           v-qohi-2[6] = v-qohi-2[6] + v-qohj[6]
           v-qohj    = 0
           v-qohj-2  = 0.

          if itemfg.prod-uom eq "EA" then
            v-u-cst = tt-fg-bin.std-tot-cost.
          else
            run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                   tt-fg-bin.std-tot-cost, output v-u-cst).

          if avail oe-ordl then
            assign
              v-u-val  = oe-ordl.t-price / oe-ordl.qty
              lv-case-count = oe-ordl.cas-cnt.
          else do:
              lv-case-count = itemfg.case-count.

            if itemfg.sell-uom eq "EA" THEN
                v-u-val = itemfg.sell-price.
              ELSE
              IF itemfg.sell-uom = "CS" AND lv-case-count <> 0 THEN
                v-u-val = itemfg.sell-price / lv-case-count.
              else
                run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                       itemfg.sell-price, output v-u-val).
          end.
        
          if v-u-cst eq ? then v-u-cst = 0.
          if v-u-val eq ? then v-u-val = 0.

          IF NOT tb_neg-sale OR (v-qty-2 * v-u-val) GT 0 THEN
            assign
             v-cst[1] = v-cst[1] + (v-qty-2 * v-u-cst)
             v-val[1] = v-val[1] + (v-qty-2 * v-u-val).
        end.

        if last-of(tt-fg-bin.i-no) then do:
          if v-qohi[6] lt 0 then do:

            v-qty = v-qohi[6] * -1.
            
            do v = 5 to 1 by -1:
              if v-qohi[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohi[v])
                 v-qohi[v] = v-qohi[v] - v-red
                 v-qty     = v-qty     - v-red
                 v-sell-price[v] = v-sell-price[v] - (v-red * v-price) .
                
              if v-qty le 0 then leave.
            end.
            
            if v-qty gt 0 then
              assign
               v-qohi   = 0
               v-cst[1] = 0
               v-val[1] = 0
               v-sell-price = 0.
          end.

          IF NOT v-q-or-v THEN DO:
            v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
            
            DO v = 1 TO 5:

               IF rd_price = "Avg" THEN
                  v-qohi[v] = v-val[1] / v-qty * v-qohi[v].
               ELSE
                  v-qohi[v] = v-sell-price[v].

               IF v-qohi[v] EQ ? THEN v-qohi[v] = 0.
            END.
          END.
        END.
   
        IF LAST-OF(tt-fg-bin.i-no) THEN DO:
          IF (v-curr AND (v-qohi[1] NE 0 OR v-qohi[2] NE 0 OR v-qohi[3] NE 0))  OR
             v-qohi[4] NE 0 OR v-qohi[5] NE 0 OR v-val[1] NE 0 OR v-cst[1] NE 0 THEN DO:
            create tt-file.
            ASSIGN
             tt-sman    = cust.sman
             tt-cust-no = cust.cust-no
             tt-i-no    = itemfg.i-no
             tt-cst[1]  = v-cst[1]
             tt-val[1]  = (IF v-qohi[1] NE 0 OR v-qohi[2] NE 0 OR v-qohi[3] NE 0 OR v-qohi[4] NE 0 OR v-qohi[5] NE 0 THEN v-val[1] ELSE 0)
             tt-qohi[1] = v-qohi[1]
             tt-qohi[2] = v-qohi[2]
             tt-qohi[3] = v-qohi[3]
             tt-qohi[4] = v-qohi[4]
             tt-qohi[5] = v-qohi[5]
             tt-days    = vdat - ld-last.
             v-last-dt = ld-last.
             DO v-dr = 1 TO 5:
               IF v-qohi[v-dr] GT 0 AND v-dates[v-dr] <> ? THEN
                 v-last-dt = v-dates[v-dr].
             END.
             IF v-last-dt NE ? THEN
               tt-days = vdat - v-last-dt.
          END.
        
          ASSIGN
           v-cst[1]   = 0
           v-val[1]   = 0
           v-qohi     = 0
           v-sell-price     = 0
           ld-last    = 01/01/0001.
        END.
      END.
    END.  /* each cust */    
    
    for each tt-file,
        
        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-cust-no
        no-lock,
        
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq tt-i-no
        no-lock
        
        break by tt-sman
              by tt-cust-no
              /*by tt-i-no*/
              by (IF sort-opt EQ "I" THEN itemfg.i-no ELSE  itemfg.part-no ):

      FIND FIRST sman NO-LOCK
          WHERE sman.company EQ cocode
            AND sman.sman    EQ tt-sman
          NO-ERROR.

      lv-sname = IF AVAIL sman AND sman.sname NE "" THEN sman.sname ELSE
                 IF cust.sman EQ "" THEN "No Sales Rep Name" ELSE "Not on File".

      if first-of(tt-sman)                  or
         (first-of(tt-cust-no) and v-break) then do:
        IF FIRST(tt-sman) THEN DISPLAY WITH FRAME r-top.
        ELSE page.
      END.

      if first-of(tt-cust-no) then v-cus = cust.name.
   
      FIND LAST fg-rcpth NO-LOCK 
          WHERE fg-rcpth.company EQ cocode 
            AND fg-rcpth.i-no EQ itemfg.i-no 
            AND fg-rcpth.rita-code EQ "S" NO-ERROR.

      ASSIGN
       v-cst[1]  = tt-cst[1]
       v-val[1]  = tt-val[1]
       v-qohi[1] = tt-qohi[1]
       v-qohi[2] = tt-qohi[2]
       v-qohi[3] = tt-qohi[3]
       v-qohi[4] = tt-qohi[4]
       v-qohi[5] = tt-qohi[5]
          
         
       lv-last-fld = IF rd_show2 BEGINS "Day" THEN STRING(tt-days,"->>>>>>9")
                                                   ELSE ""
       v-shipdt = IF AVAIL fg-rcpth AND v-sdate THEN STRING(fg-rcpth.trans-date) 
                                                   ELSE "" .  
        IF NOT v-cost THEN DO:
            IF v-cpart AND v-sdate  THEN do: 
                ASSIGN 
                    lv-last-fld[2]  = itemfg.part-no 
                    v-custpart[1]   = v-shipdt[1] .
             END.
             ELSE IF v-cpart AND NOT v-sdate  THEN do:
                 ASSIGN
                     lv-last-fld[2]  = itemfg.part-no
                     v-custpart[1] = "" .
             END.
             ELSE IF v-sdate THEN DO:
                 ASSIGN 
                     lv-last-fld[2]  = v-shipdt[1]  
                     v-shipdt[1]     = ""
                     v-shipdt[2]     = ""  .
             END.
             ELSE IF NOT v-sdate and NOT v-cpart then DO:
                 ASSIGN
                     lv-last-fld[2] = "".
             END.
        END.

        IF v-cost THEN DO:
            IF v-cpart AND v-sdate THEN DO:
                ASSIGN
                    v-custpart = itemfg.part-no .
            END.
            ELSE IF v-cpart AND NOT v-sdate THEN DO:
                ASSIGN
                    v-custpart = itemfg.part-no .
            END.
            ELSE IF NOT v-cpart AND v-sdate THEN do:
                ASSIGN
                    v-custpart  = v-shipdt[1]  
                    v-shipdt[2] = "".
            END.
        END.
    
         
      display v-cus                   format "x(12)"
              itemfg.i-no             format "x(15)"
              itemfg.i-name           format "x(30)"
              v-qohi[1] when v-curr   format "->>>>>>9"
              v-qohi[2] when v-curr   format "->>>>>>9"
              v-qohi[3] when v-curr   format "->>>>>>9"
              v-qohi[4]               format "->>>>>>9"
              v-qohi[5]               format "->>>>>>9"
              v-cst[1]  when v-cost   format "->,>>>,>>9.99"
                  v-val[1] when not v-cost @ v-cst[1]
              lv-last-fld[1]
                  v-val[1] when v-cost     @ lv-last-fld[1]
              lv-last-fld[2] 
              v-custpart[1] WHEN  v-cpart format "x(13)" 
              v-shipdt[1]  WHEN NOT v-cpart @ v-custpart[1]
              v-shipdt[2] WHEN v-cost
              skip(1)

          with frame detail no-box no-labels no-attr-space stream-io width 200 down.
      down with frame detail.


      IF tb_excel THEN 
         PUT STREAM excel UNFORMATTED
             '"' cust.sman + " " + lv-sname                            '",'
             '"' v-cus                                                 '",'
             '"' itemfg.i-no                                           '",'
             '"' itemfg.i-name                                         '",'
             '"' (IF v-curr THEN STRING(v-qohi[1],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohi[2],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohi[3],"->>>>>>9") ELSE "") '",'
             '"' STRING(v-qohi[4],"->>>>>>9")                          '",'
             '"' STRING(v-qohi[5],"->>>>>>9")                          '",'
             '"' (IF v-cost THEN STRING(v-cst[1],"->,>>>,>>9.99")
                  ELSE STRING(v-val[1],"->,>>>,>>9.99"))               '",'
             '"' (IF v-cost THEN STRING(v-val[1],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
             '"'  lv-last-fld[2]        '",'
             '"' (IF v-cpart  THEN  v-custpart[1] ELSE v-shipdt[1] )                             '",'
             '"' ( IF  v-sdate AND  v-cost  THEN  v-shipdt[2] ELSE "" )            '",'
             SKIP.

      ASSIGN
       v-cst[2]  = v-cst[2]  + v-cst[1]
       v-val[2]  = v-val[2]  + v-val[1]
       v-qohc[1] = v-qohc[1] + v-qohi[1]
       v-qohc[2] = v-qohc[2] + v-qohi[2]
       v-qohc[3] = v-qohc[3] + v-qohi[3]
       v-qohc[4] = v-qohc[4] + v-qohi[4]
       v-qohc[5] = v-qohc[5] + v-qohi[5]

       v-cus     = ""
       v-cst[1]  = 0
       v-val[1]  = 0
       v-sell-price    = 0
       v-qohi    = 0
          
       lv-last-fld = "".
      
      if last-of(tt-cust-no) then do:
        if v-sub-t then do:
          display "            Customer Subtotal:"  @ itemfg.i-name
                  v-qohc[1] when v-curr             @ v-qohi[1]
                  v-qohc[2] when v-curr             @ v-qohi[2]
                  v-qohc[3] when v-curr             @ v-qohi[3]
                  v-qohc[4]                         @ v-qohi[4]
                  v-qohc[5]                         @ v-qohi[5]
                  v-cst[2]  when v-cost             @ v-cst[1]
                      v-val[2] when not v-cost      @ v-cst[1]
                  lv-last-fld[1]
                      v-val[2] when v-cost          @ lv-last-fld[1]

              with frame detail.
          down with frame detail.

          IF tb_excel THEN 
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""                          '",'
                 '"' ""                          '",'
                 '"' ""                          '",'
                 '"' "            Customer Subtotal:"                      '",'
                 '"' (IF v-curr THEN STRING(v-qohc[1],"->>>>>>9") ELSE "") '",'
                 '"' (IF v-curr THEN STRING(v-qohc[2],"->>>>>>9") ELSE "") '",'
                 '"' (IF v-curr THEN STRING(v-qohc[3],"->>>>>>9") ELSE "") '",'
                 '"' STRING(v-qohc[4],"->>>>>>9")                          '",'
                 '"' STRING(v-qohc[5],"->>>>>>9")                          '",'
                 '"' (IF v-cost THEN STRING(v-cst[2],"->,>>>,>>9.99")
                      ELSE STRING(v-val[2],"->,>>>,>>9.99"))               '",'
                 '"' (IF v-cost THEN STRING(v-val[2],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
                 SKIP(1).
        end.
        
        assign
         v-cst[3]  = v-cst[3]  + v-cst[2]
         v-val[3]  = v-val[3]  + v-val[2]
         v-qohs[1] = v-qohs[1] + v-qohc[1]
         v-qohs[2] = v-qohs[2] + v-qohc[2]
         v-qohs[3] = v-qohs[3] + v-qohc[3]
         v-qohs[4] = v-qohs[4] + v-qohc[4]
         v-qohs[5] = v-qohs[5] + v-qohc[5]

         v-cst[2] = 0
         v-val[2] = 0
         v-qohc   = 0.
      end.

      if last-of(tt-sman) then do:
        display "         Salesperson Subtotal:"    @ itemfg.i-name
                v-qohs[1] when v-sub-t and v-curr   @ v-qohi[1]
                v-qohs[2] when v-sub-t and v-curr   @ v-qohi[2]
                v-qohs[3] when v-sub-t and v-curr   @ v-qohi[3]
                v-qohs[4] when v-sub-t              @ v-qohi[4]
                v-qohs[5] when v-sub-t              @ v-qohi[5]
                v-cst[3]  when v-cost               @ v-cst[1]
                    v-val[3] when not v-cost        @ v-cst[1]
                lv-last-fld[1]
                    v-val[3] when v-cost            @ lv-last-fld[1]

            with frame detail.
        down with frame detail.

        IF tb_excel THEN 
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' ""                          '",'
               '"' ""                          '",'
               '"' ""                          '",'
               '"' "         Salesperson Subtotal:"                      '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[1],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[2],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[3],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t THEN STRING(v-qohs[4],"->>>>>>9") ELSE "")                         '",'
               '"' (IF v-sub-t THEN STRING(v-qohs[5],"->>>>>>9") ELSE "")                         '",'
               '"' (IF v-cost THEN STRING(v-cst[3],"->,>>>,>>9.99")
                    ELSE STRING(v-val[3],"->,>>>,>>9.99"))               '",'
               '"' (IF v-cost THEN STRING(v-val[3],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
               SKIP(1).

        assign
         v-cst[4]  = v-cst[4]  + v-cst[3]
         v-val[4]  = v-val[4]  + v-val[3]
         v-qohg[1] = v-qohg[1] + v-qohs[1]
         v-qohg[2] = v-qohg[2] + v-qohs[2]
         v-qohg[3] = v-qohg[3] + v-qohs[3]
         v-qohg[4] = v-qohg[4] + v-qohs[4]
         v-qohg[5] = v-qohg[5] + v-qohs[5]

         v-cst[3] = 0
         v-val[3] = 0
         v-qohs   = 0.
      end.

      if last(tt-sman) then do:
        display "                  Grand Total:"    @ itemfg.i-name
                v-qohg[1] when v-sub-t and v-curr   @ v-qohi[1]
                v-qohg[2] when v-sub-t and v-curr   @ v-qohi[2]
                v-qohg[3] when v-sub-t and v-curr   @ v-qohi[3]
                v-qohg[4] when v-sub-t              @ v-qohi[4]
                v-qohg[5] when v-sub-t              @ v-qohi[5]
                v-cst[4]  when v-cost               @ v-cst[1]
                    v-val[4] when not v-cost        @ v-cst[1]
                lv-last-fld[1]
                    v-val[4] when v-cost            @ lv-last-fld[1]

            with frame detail.
        down with frame detail.

        IF tb_excel THEN 
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' ""                          '",'
               '"' ""                          '",'
               '"' ""                          '",'
               '"' "                  Grand Total:"                      '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohg[1],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohg[2],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t AND v-curr THEN STRING(v-qohg[3],"->>>>>>9") ELSE "") '",'
               '"' (IF v-sub-t THEN STRING(v-qohg[4],"->>>>>>9") ELSE "")                         '",'
               '"' (IF v-sub-t THEN STRING(v-qohg[5],"->>>>>>9") ELSE "")                         '",'
               '"' (IF v-cost THEN STRING(v-cst[4],"->,>>>,>>9.99")
                    ELSE STRING(v-val[4],"->,>>>,>>9.99"))               '",'
               '"' (IF v-cost THEN STRING(v-val[4],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
               SKIP(1).
      end.
      
      delete tt-file.
    end. /* each tt-file */

