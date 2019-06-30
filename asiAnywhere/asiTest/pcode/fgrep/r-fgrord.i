/*****************************************************************************
 Program: fgrep/r-fgrord.i
 
*****************************************************************************/

for each itemfg
   where itemfg.company    eq cocode
     and itemfg.cust-no    ge v-cust[1]
     and itemfg.cust-no    le v-cust[2]
     and itemfg.i-no       ge v-item[1]
     and itemfg.i-no       le v-item[2]
     and itemfg.procat     ge v-cat[1]
     and itemfg.procat     le v-cat[2]

     and ((itemfg.ord-policy     and v-lot-reo eq "R") or
          (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")

     and ((itemfg.pur-man        and v-pur-man eq "P") or
          (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")

     and ((itemfg.stocked        and v-stocked eq "S") or
          (not itemfg.stocked    and v-stocked eq "N") or v-stocked eq "A")

     use-index i-no no-lock:
   
    IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
    

    v-qty-onh = 0.

    IF v-custown = NO THEN
       FOR EACH fg-bin FIELDS(qty)
           WHERE fg-bin.company EQ itemfg.company
             AND fg-bin.i-no    EQ itemfg.i-no
             AND fg-bin.loc     GE begin_whse
             AND fg-bin.loc     LE end_whse
             AND fg-bin.cust-no EQ ""
             NO-LOCK:
         v-qty-onh = v-qty-onh + fg-bin.qty.
       END.
    ELSE
       FOR EACH fg-bin FIELDS(qty)
           WHERE fg-bin.company EQ itemfg.company
             AND fg-bin.i-no    EQ itemfg.i-no
             AND fg-bin.loc     GE begin_whse
             AND fg-bin.loc     LE end_whse
             NO-LOCK:
         v-qty-onh = v-qty-onh + fg-bin.qty.
       END.

    ASSIGN
     v-alloc-qty = 0
     v-qty-avail = v-qty-onh + (if v-inconh then itemfg.q-ono else 0).

    IF rd_qoh BEGINS "Total" THEN v-alloc-qty = itemfg.q-alloc.

    ELSE
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ itemfg.company
          AND oe-ordl.i-no    EQ itemfg.i-no
          AND oe-ordl.opened  EQ YES
          AND oe-ordl.stat    NE "C"
        USE-INDEX item,

        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-ordl.company
          AND oe-ord.ord-no  EQ oe-ordl.ord-no
          AND oe-ord.type    NE "T":

      IF rd_qoh BEGINS "Sched" OR rd_qoh BEGINS "All" THEN
      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            AND oe-rel.rel-date LE v-date
            AND NOT CAN-FIND(FIRST reftable
                             WHERE reftable.reftable EQ "oe-rel.s-code"
                               AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
                               AND reftable.code     EQ "T")
          USE-INDEX ord-item:
          
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).
        
        IF CAN-DO("S,I,L",v-stat) THEN v-alloc-qty = v-alloc-qty + oe-rel.qty.
        
      END.

      IF rd_qoh BEGINS "Actual" OR rd_qoh BEGINS "All" THEN DO:
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-ordl.company
              AND oe-rell.ord-no  EQ oe-ordl.ord-no
              AND oe-rell.i-no    EQ oe-ordl.i-no
              AND oe-rell.line    EQ oe-ordl.line
              AND oe-rell.s-code  NE "T"
            USE-INDEX ord-no,

            FIRST oe-relh NO-LOCK
            WHERE oe-relh.r-no     EQ oe-rell.r-no
              AND oe-relh.posted   EQ NO
              AND oe-relh.deleted  EQ NO
              AND oe-relh.rel-date LE v-date
            USE-INDEX r-no:
           
          v-alloc-qty = v-alloc-qty + oe-rell.qty.
          
        END.

        FOR EACH oe-boll FIELDS(b-no qty) NO-LOCK
            WHERE oe-boll.company EQ oe-ordl.company
              AND oe-boll.ord-no  EQ oe-ordl.ord-no
              AND oe-boll.i-no    EQ oe-ordl.i-no
              AND oe-boll.line    EQ oe-ordl.line
              AND oe-boll.s-code  NE "T"
            USE-INDEX ord-no,

            FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.b-no     EQ oe-boll.b-no
              AND oe-bolh.posted   EQ NO
              AND oe-bolh.deleted  EQ NO
              AND oe-bolh.rel-date LE v-date
            USE-INDEX b-no:

          v-alloc-qty = v-alloc-qty + oe-boll.qty.
        END.
      END.
    END. /* each oe-ordl */

    v-qty-avail = v-qty-avail - v-alloc-qty.
    
    if itemfg.ord-level gt v-qty-avail then do:
       v-reord-qty = itemfg.ord-level - v-qty-avail.

       if v-reord-qty lt itemfg.ord-min and
          itemfg.ord-min ne 0 then 
         v-reord-qty = itemfg.ord-min.

       if v-reord-qty gt itemfg.ord-max and
          itemfg.ord-max ne 0 then 
         v-reord-qty = itemfg.ord-max.
    end.
    else v-reord-qty = 0.

    if v-reord-qty gt 0 or v-prt-all then
       IF tb_history THEN DO:
           li-hist = 0.

           FOR EACH fg-rcpth
              WHERE fg-rcpth.company    EQ itemfg.company
                AND fg-rcpth.i-no       EQ itemfg.i-no
                AND fg-rcpth.trans-date GE ld-fr
                AND fg-rcpth.trans-date LE ld-to
                AND INDEX("SE",fg-rcpth.rita-code) GT 0
              USE-INDEX tran NO-LOCK,
               EACH fg-rdtlh
              WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              NO-LOCK:

               li = MONTH(TODAY) + 1 - MONTH(fg-rcpth.trans-date).
               IF li LT 1 THEN li = li + 12.
               IF li GT 6 THEN li = 6.

               IF fg-rcpth.rita-code EQ "S" THEN
                 li-hist[li] = li-hist[li] + fg-rdtlh.qty.
               ELSE
                 li-hist[li] = li-hist[li] - fg-rdtlh.qty.
           END.

           DISPLAY itemfg.i-no
                   itemfg.i-name         FORMAT "x(18)"
                   itemfg.procat         FORMAT "x(8)"
                   itemfg.sell-uom
                   itemfg.ord-level      FORMAT "->>>>>9"   
                   v-qty-onh             FORMAT "->>>>>,>>>9"   
                   v-alloc-qty           FORMAT "->>>,>>>9"   
                   itemfg.q-ono          FORMAT "->>>>>>>9"   
                   itemfg.ord-min        FORMAT ">>,>>>,>>>9"
                   v-qty-avail           FORMAT "->>>>>,>>>>9"   
                   itemfg.vend-item      FORMAT "x(20)"
                   v-reord-qty           FORMAT ">>>,>>>,>>>>,>>>9"
                   li-hist               FORMAT "->>>,>>>>9"
                 WITH FRAME itemhist NO-BOX DOWN STREAM-IO NO-LABELS WIDTH 320.

           DOWN WITH FRAME itemhist.

           IF tb_dash THEN PUT FILL("-",217) FORMAT "x(217)" SKIP.
           
           IF tb_excel THEN 
             EXPORT STREAM excel DELIMITER ","
               itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
               itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono 
               itemfg.ord-min v-qty-avail itemfg.vend-item v-reord-qty li-hist.
       END.
       ELSE DO:
           if v-prt-cpn then
               if v-prt-qty then
                   if v-prt-prc eq "P" then do:                       
                       display itemfg.i-no           column-label "ITEM#"
                               itemfg.part-no        format "x(14)"
                                                     column-label "CUST PART#"
                               itemfg.i-name         format "x(18)"
                                                     column-label "DESCRIPTION"
                               itemfg.procat         column-label "CAT"
                               itemfg.sell-uom       column-label "UOM"
                               itemfg.ord-level      format "->>>,>>>,>>9"
                                                     column-label "REORDER LEVEL"
                               v-qty-onh             format "->>>,>>>,>>9"
                                                     column-label "QT ON HAND"
                               v-alloc-qty           format "->>>,>>>,>>9"
                                                     column-label "QT ALLOC"
                               itemfg.q-ono          format "->>>,>>>,>>9"    
                                                     column-label "QT ON ORD"
                               itemfg.ord-min        format ">>,>>>,>>9"
                                                     column-label "MIN ORDER"
                               v-qty-avail           format "->>>,>>>,>>9"
                                                     column-label "QT AVAIL"
                               itemfg.sell-price     column-label "SELLING PRICE"
                                                     format ">>>>,>>>,>>>9 "
                               v-reord-qty           column-label "SUGGE REORDER "
                                                     format ">>>,>>>,>>>9 "
                          with frame itemx100 no-box down STREAM-IO NO-LABEL width 280.
                       down with frame itemx100.

                       IF tb_dash THEN PUT FILL("-",164) FORMAT "x(164)" SKIP.

                       IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no  itemfg.i-name 
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           v-qty-avail itemfg.sell-price v-reord-qty.
                   end.
                   else
                     if v-prt-prc eq "V" then do:
                        display itemfg.i-no          column-label "ITEM#"
                               itemfg.part-no        column-label "CUST PART#"
                                                     format "x(14)"
                               itemfg.i-name         format "x(19)"
                                                     column-label "DESCRIPTION"
                               itemfg.procat         column-label "CAT"
                               itemfg.sell-uom       column-label "UOM"
                               itemfg.ord-level      format "->>>,>>>,>>9"
                                                     column-label "REORDER"
                               v-qty-onh             format "->>>,>>>,>>9"
                                                     column-label "QT ON HAND"
                               v-alloc-qty           format "->>>,>>>,>>9"
                                                     column-label "QT ALLOC"
                               itemfg.q-ono          format "->>>,>>>,>9"    
                                                     column-label "QT ON ORD"
                               itemfg.ord-min        format ">>,>>>,>>9" 
                                                     column-label "MIN ORDER"
                               v-qty-avail           format "->>>,>>>,>>9"
                                                     column-label "QT AVAIL"
                               itemfg.vend-item      column-label "VENDOR"
                                                     format "x(15)"
                               v-reord-qty           column-label "SUGG REORDER "
                                                     format ">>>,>>>,>>9 " 
                               
                           with frame itemx200 no-box down STREAM-IO NO-LABEL width 320.
                           down with frame itemx200.
                       
                       IF tb_dash THEN PUT FILL("-",164) FORMAT "x(164)" SKIP.
                        
                       IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no  itemfg.i-name 
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           v-qty-avail itemfg.vend-item v-reord-qty. 

                     end.
                   else do:
                       display itemfg.i-no           column-label "ITEM #"
                               itemfg.part-no        format "x(14)"
                                                     column-label "CUST PART #"
                               itemfg.i-name         format "x(18)"
                                                     column-label "DESCRIPTION"
                               itemfg.procat         column-label "CAT"
                               itemfg.sell-uom       column-label "UOM"
                               itemfg.ord-level      format "->>>,>>>,>>9"
                                                     column-label "REORDER LEVEL"
                               v-qty-onh             format "->>>,>>>,>>9"
                                                     column-label "QTY ON HAND"
                               v-alloc-qty           format "->>>,>>>,>>9"
                                                     column-label "QTY ALLOC"
                               itemfg.q-ono          format "->>>,>>>,>>9"    
                                                     column-label "QTY ON ORD"
                               itemfg.ord-min        format ">>,>>>,>>9" 
                                                     column-label "MIN ORDER"
                               v-qty-avail           format "->>>,>>>,>>9"
                                                     column-label "QTY AVAIL"
                               itemfg.ord-max        column-label "MAX ORDER"
                               v-reord-qty           column-label "SUGG REORDER "
                                                     format ">>>,>>>,>>>9"
                           with frame itemx250 no-box down STREAM-IO NO-LABEL width 280.
                       down with frame itemx250.

                       IF tb_dash THEN PUT FILL("-",164) FORMAT "x(164)" SKIP.

                       IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           v-qty-avail itemfg.ord-max v-reord-qty.
                   end.
               else
                 if v-prt-prc eq "P" then do:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(14)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(18)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label " CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER LEVEL"
                             v-qty-onh             format "->>>,>>>,>>9"
                                                   column-label "QTY ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QTY ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "QTY ON ORD"
                             itemfg.ord-min        format ">>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label " VENDOR"
                             itemfg.sell-price     column-label "SELLING PRICE"
                             v-reord-qty           column-label "SUGG REORDER "
                                                   format ">>>,>>>,>>>9 "
                         with frame itemx300 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx300.

                     IF tb_dash THEN PUT FILL("-",164) FORMAT "x(164)" SKIP.

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name 
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           itemfg.vend-no itemfg.sell-price v-reord-qty.
                 end.
               else
                 if v-prt-prc eq "V" then do:                     
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(14)"
                                                   column-label "CUST PART#"
                             itemfg.i-name         format "x(18)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER "
                             v-qty-onh             format "->>>,>>>,>>9"
                                                   column-label "QT ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QT ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "QT ON ORD"
                             itemfg.ord-min        format ">>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label " VENDOR"
                             itemfg.vend-item      column-label "VEND NUMBER"
                             v-reord-qty           column-label "SUGG REORDER "
                                                   format ">>>,>>>,>>>9 "
                         with frame itemx400 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx400.

                     IF tb_dash THEN PUT FILL("-",166) FORMAT "x(166)" SKIP.                                        

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           itemfg.vend-no itemfg.vend-item v-reord-qty.
                 end.
                 else do:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(14)"
                                                   column-label "CUST PART#"
                             itemfg.i-name         format "x(18)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER LEVEL"
                             v-qty-onh             format "->>>,>>>,>>9"
                                                   column-label "QT ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QT ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "QT ON ORD"
                             itemfg.ord-min        format ">>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label " VEND"
                             itemfg.ord-max        column-label "MAX ORDER"
                             v-reord-qty           column-label "SUGG REORDER "
                                                   format ">>>,>>>,>>>9"
                         with frame itemx450 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx450.

                     IF tb_dash THEN PUT FILL("-",165) FORMAT "x(165)" SKIP.

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name
                           itemfg.procat itemfg.sell-uom itemfg.ord-level
                           v-qty-onh v-alloc-qty itemfg.q-ono itemfg.ord-min
                           itemfg.vend-no itemfg.ord-max v-reord-qty.
                 end.
           else
             if v-prt-qty then
               if v-prt-prc eq "P" then do:
                   display itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label "CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg.ord-level      format "->>>,>>>,>>9"
                                                 column-label "REORDER LEVEL"
                           v-qty-onh             format "->>>,>>>,>>9"
                                                 column-label "QTY ON HAND"
                           v-alloc-qty           format "->>>,>>>,>>9"
                                                 column-label "QTY ALLOC"
                           itemfg.q-ono          format "->>>,>>>,>>9"    
                                                 column-label "QTY ON ORD"
                           itemfg.ord-min        format ">>,>>>,>>9" 
                                                 column-label "MIN ORDER"
                           v-qty-avail           format "->>>,>>>,>>>9"
                                                 column-label "QTY AVAIL"
                           itemfg.sell-price     column-label "SELLING PRICE"
                                                 format ">>>>,>>>>,>>>9"
                           v-reord-qty           column-label "SUGGESTED REORDER "
                                                 format ">>>>,>>>>,>>>9"
                       with frame itemx500 no-box down STREAM-IO NO-LABEL width 280.
                   down with frame itemx500.

                   IF tb_dash THEN PUT FILL("-",154) FORMAT "x(154)" SKIP.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat
                       itemfg.sell-uom itemfg.ord-level v-qty-onh
                       v-alloc-qty itemfg.q-ono itemfg.ord-min v-qty-avail
                       itemfg.sell-price v-reord-qty.
               end.
               else
                 if v-prt-prc eq "V" then do:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER LEVEL"
                             v-qty-onh             format "->>>,>>>,>>9"
                                                   column-label "QTY ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QTY ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "QTY ON ORD"
                             itemfg.ord-min        format ">>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             v-qty-avail           format "->>>,>>>,>>9"
                                                   column-label "QTY AVAIL"
                             itemfg.vend-item      column-label "VENDOR NUMBER"
                             v-reord-qty           column-label "SUGGESTED REORDER "
                                                   format ">>>>,>>>,>>>9 "
                         with frame itemx600 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx600.
                     IF tb_dash THEN PUT FILL("-",158) FORMAT "x(158)" SKIP.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono
                         itemfg.ord-min v-qty-avail itemfg.vend-item v-reord-qty.
                 end.
                 else do:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER LEVEL"
                             v-qty-onh             format "->>>,>>>,>>>9"
                                                   column-label "QTY ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QTY ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "QTY ON ORD"
                             itemfg.ord-min        format ">>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             v-qty-avail           format "->>>,>>>,>>>9"
                                                   column-label "QTY AVAIL"
                             itemfg.ord-max        column-label "MAX REORDER"
                             v-reord-qty           column-label "SUGGESTED REORDER "
                                                   format ">>>>,>>>,>>>9"
                         with frame itemx650 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx650.

                     IF tb_dash THEN PUT FILL("-",155) FORMAT "x(155)" SKIP.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono
                         itemfg.ord-min v-qty-avail itemfg.ord-max v-reord-qty.
                 end.
             else
               if v-prt-prc eq "P" then DO:
                   display itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label " CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg.ord-level      format "->>>,>>>,>>9"
                                                 column-label "REORDER LEVEL"
                           v-qty-onh             format "->>>,>>>,>>>9"
                                                 column-label "QTY ON HAND"
                           v-alloc-qty           format "->>>,>>>,>>9"
                                                 column-label "QTY ALLOC"
                           itemfg.q-ono          format "->>>,>>>,>>9"    
                                                 column-label "QT ON ORD"
                           itemfg.ord-min        format ">>>,>>>,>>9" 
                                                 column-label "MIN ORDER"
                           itemfg.vend-no        format "x(13)"
                                                 column-label " VENDOR"
                           itemfg.sell-price     column-label "SELLING PRICE"
                           v-reord-qty           column-label "SUGGESTED REORDER"
                                                 format ">>>>,>>>,>>>9"
                       with frame itemx700 no-box down STREAM-IO NO-LABEL width 280.
                   down with frame itemx700.

                   IF tb_dash THEN PUT FILL("-",154) FORMAT "x(154)" SKIP.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                       itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono
                       itemfg.ord-min itemfg.vend-no 
                       itemfg.sell-price v-reord-qty.
               end.
               else
                 if v-prt-prc eq "V" then do:
                     display itemfg.i-no           column-label "   ITEM #  "
                             itemfg.i-name         format "x(20)"
                                                   column-label "    DESCRIPTION "
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER"
                             v-qty-onh             format "->>>>,>>>,>>9"
                                                   column-label "ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "ALLOC QT"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "ON ORD"
                             itemfg.ord-min        format ">>>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "VENDOR"
                             itemfg.vend-item      column-label "VENDOR NUM"
                             v-reord-qty           column-label "SUGGESTED REORDER "
                                                   format ">>>>,>>>>,>>9 "
                         with frame itemx800 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx800.

                     IF tb_dash THEN PUT FILL("-",156) FORMAT "x(156)" SKIP.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                       itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono
                       itemfg.ord-min itemfg.vend-no 
                       itemfg.vend-item v-reord-qty.

                 end.
                 else do:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg.ord-level      format "->>>,>>>,>>9"
                                                   column-label "REORDER LEVEL"
                             v-qty-onh             format "->>>>,>>>,>>9"
                                                   column-label "QTY ON HAND"
                             v-alloc-qty           format "->>>,>>>,>>9"
                                                   column-label "QTY ALLOC"
                             itemfg.q-ono          format "->>>,>>>,>>9"    
                                                   column-label "ON ORD"
                             itemfg.ord-min        format ">>>,>>>,>>9" 
                                                   column-label "MIN ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "VENDOR"
                             itemfg.ord-max        column-label "MAX REORDER"
                             v-reord-qty           column-label "SUGGESTED REORDER "
                                                   format ">>>>,>>>,>>9"
                         with frame itemx850 no-box down STREAM-IO NO-LABEL width 280.
                     down with frame itemx850.

                     IF tb_dash THEN PUT FILL("-",156) FORMAT "x(156)" SKIP.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg.ord-level v-qty-onh v-alloc-qty itemfg.q-ono
                         itemfg.ord-min itemfg.vend-no 
                         itemfg.ord-max v-reord-qty.
                 end.
      
     /*  IF tb_dash THEN PUT FILL("-",165) FORMAT "x(165)" SKIP.*/
    END.
end. /* each itemfg */
