/*****************************************************************************
 Program: fgrep/r-fgrord.i
 
*****************************************************************************/
                          FORM itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9"
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg.sell-price     column-label "SELLING PRICE"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                              with frame itemx100 no-box down stream-io width 167.


                         form itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9" 
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg.vend-item      FORMAT "x(15)"
                                                         column-label "VENDOR!ITEM NUMBER"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                               with frame itemx200 no-box down stream-io width 167.

                           form itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9" 
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg-loc.ord-max        format ">>>>>>>9"
                                                         column-label "MAXIMUM!ORDER"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                               with frame itemx250 no-box down stream-io width 167.
                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg.sell-price     column-label "SELLING PRICE"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx300 no-box down stream-io width 167.
                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(10)"
                                                   column-label "!VENDOR"
                             itemfg.vend-item      format "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx400 no-box down stream-io width 167.

                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx450 no-box down stream-io width 167.

                   form itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label "PROD!CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg-loc.ord-level      format "->>>>>9"
                                                 column-label "REORDER!LEVEL"
                           v-qty-onh             format "->>>>>9"
                                                 column-label "QTY!ON HAND"
                           v-alloc-qty           format "->>>>>9"
                                                 column-label "QTY!ALLOC"
                           itemfg-loc.q-ono          format "->>>>>9"    
                                                 column-label "QTY!ON ORD"
                           itemfg-loc.ord-min        format ">>>>>>>9" 
                                                 column-label "MINIMUM!ORDER"
                           v-qty-avail           format "->>>>>>9"
                                                 column-label "QTY!AVAIL"
                           itemfg.sell-price     column-label "SELLING PRICE"
                           v-reord-qty           column-label "SUGGESTED!REORDER"
                       with frame itemx500 no-box down stream-io width 150.

                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             v-qty-avail           format "->>>>>>9"
                                                   column-label "QTY!AVAIL"
                             itemfg.vend-item      FORMAT "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx600 no-box down stream-io width 150.

                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             v-qty-avail           format "->>>>>>9"
                                                   column-label "QTY!AVAIL"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx650 no-box down stream-io width 150.

                   form itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label "PROD!CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg-loc.ord-level      format "->>>>>9"
                                                 column-label "REORDER!LEVEL"
                           v-qty-onh             format "->>>>>9"
                                                 column-label "QTY!ON HAND"
                           v-alloc-qty           format "->>>>>9"
                                                 column-label "QTY!ALLOC"
                           itemfg-loc.q-ono          format "->>>>>9"    
                                                 column-label "QTY!ON ORD"
                           itemfg-loc.ord-min        format ">>>>>>>9" 
                                                 column-label "MINIMUM!ORDER"
                           itemfg.vend-no        format "x(12)"
                                                 column-label "!VENDOR"
                           itemfg.sell-price     column-label "SELLING PRICE"
                           v-reord-qty           column-label "SUGGESTED!REORDER"
                       with frame itemx700 no-box down stream-io width 150.

                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(10)"
                                                   column-label "!VENDOR"
                             itemfg.vend-item      FORMAT "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx800 no-box down stream-io width 150.
                     form itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx850 no-box down stream-io width 150.
/*****************************************************************************
display statements
 
*****************************************************************************/

     if TRUE then
       IF tb_history THEN DO:
           li-hist = 0.

           IF true THEN DO:
    
               DISPLAY 
    
                     WITH FRAME itemhist NO-BOX DOWN STREAM-IO NO-LABELS WIDTH 180.
    
               DOWN WITH FRAME itemhist.
          

           END.
       END.
       ELSE DO:
           if v-prt-cpn then
               if v-prt-qty then
                   if v-prt-prc eq "P" then do:
                       IF true THEN DO:
                           
                           display

                              with frame itemx100 no-box down stream-io width 150.
                           down with frame itemx100.
    
                      END.
                   end.
                   else
                     if v-prt-prc eq "V" then do:
                       IF true THEN DO:
                       
                           display
                               with frame itemx200 no-box down stream-io width 150.
                           down with frame itemx200.

                     END.
                     end.
                   else do:
                       IF true THEN DO:
                       
                           display
                               with frame itemx250 no-box down stream-io width 150.
                           down with frame itemx250.
    

                     END.
                   end.
               else
                 if v-prt-prc eq "P" then do:
                   IF true THEN DO:
                   
                     display 
                         with frame itemx300 no-box down stream-io width 150.
                   END.
                 end.
               else
                 if v-prt-prc eq "V" then do:
                   IF true THEN DO:
                   
                     display 
                         with frame itemx400 no-box down stream-io width 150.
                     down with frame itemx400.

                   END.
                 end.
                 else do:
                     IF true THEN DO:
                     
                     display

                         with frame itemx450 no-box down stream-io width 150.
                     down with frame itemx450.
                   END.
                 end.
           else
             if v-prt-qty then
               if v-prt-prc eq "P" then do:
                 IF true THEN DO:
                 
                   display 
                       with frame itemx500 no-box down stream-io width 150.
                   down with frame itemx500.

                 END.
               end.
               else
                 if v-prt-prc eq "V" then do:
                   IF true THEN DO:

                     display 

                         with frame itemx600 no-box down stream-io width 150.
                     down with frame itemx600.

                   END.
                 end.
                 else do:
                   IF true THEN DO:
                     
                      DISP
                         with frame itemx650 no-box down stream-io width 150.
                     down with frame itemx650.

                   END.
                 end.
             else
               if v-prt-prc eq "P" then DO:
                 IF true THEN DO:
                
                   DISPLAY
                       with frame itemx700 no-box down stream-io width 150.
                   down with frame itemx700.


                 END.
               end.
               else
                 if v-prt-prc eq "V" then do:
                   IF true THEN DO:
                     display 

                         with frame itemx800 no-box down stream-io width 150.
                     down with frame itemx800.

                   END.
                 end.
                 else do:
                   IF true THEN DO:
                     display 

                         with frame itemx850 no-box down stream-io width 150.
                     down with frame itemx850.


                   END.
                 end.

    END.

for EACH loc WHERE loc.loc GE begin_whse
     AND loc.loc LE end_whse NO-LOCK,
  each itemfg
   where itemfg.company    eq cocode
     and itemfg.cust-no    ge v-cust[1]
     and itemfg.cust-no    le v-cust[2]
     AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
          AND ttCustList.log-fld no-lock) else true)
     and itemfg.i-no       ge v-item[1]
     and itemfg.i-no       le v-item[2]
     and itemfg.procat     ge v-cat[1]
     and itemfg.procat     le v-cat[2]
     AND itemfg.CLASS      GE v-class[1]
     AND itemfg.CLASS      LE v-class[2]
     AND itemfg.spare-char-1 GE v-group[1]
     AND itemfg.spare-char-1 LE v-group[2]

     and ((itemfg.ord-policy     and v-lot-reo eq "R") or
          (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")

     and ((itemfg.pur-man        and v-pur-man eq "P") or
          (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")

     and ((itemfg.stocked        and v-stocked eq "S") or
          (not itemfg.stocked    and v-stocked eq "N") or v-stocked eq "A")

     use-index i-no no-lock,
     FIRST itemfg-loc WHERE itemfg-loc.company EQ cocode
                        AND itemfg-loc.i-no    EQ itemfg.i-no
                        AND itemfg-loc.loc     EQ loc.loc
                      NO-LOCK
     BREAK  BY loc.loc BY itemfg.i-no:


        ASSIGN
          v-whse-bin-found = NO.
          v-qty-onh = 0.

    /*Added for premier mod 08291201*/
    FIND FIRST cust WHERE cust.company = itemfg.company
        AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
   
    IF itemfg.spare-char-3 NE "" THEN do:
        FIND FIRST sman WHERE sman.company = itemfg.company
            AND sman.sman = itemfg.spare-char-3 NO-LOCK NO-ERROR.
        IF AVAIL sman THEN v-sales-rep = sman.sname.
    END.
    ELSE IF AVAIL cust THEN DO:
        FIND FIRST sman WHERE sman.company = cust.company
            AND sman.sman = cust.sman NO-LOCK NO-ERROR.
        IF AVAIL sman THEN v-sales-rep = sman.sname.
    END.
    ELSE v-sales-rep = "" .

    IF v-custown = NO THEN
       FOR EACH fg-bin FIELDS(qty)
           WHERE fg-bin.company EQ itemfg.company
             AND fg-bin.i-no    EQ itemfg.i-no
             AND fg-bin.loc     EQ itemfg-loc.loc
             AND fg-bin.cust-no EQ ""
             NO-LOCK:
         ASSIGN
            v-qty-onh = v-qty-onh + fg-bin.qty
            v-whse-bin-found = YES.
       END.
    ELSE
       FOR EACH fg-bin FIELDS(qty)
           WHERE fg-bin.company EQ itemfg.company
             AND fg-bin.i-no    EQ itemfg-loc.i-no
             AND fg-bin.loc     EQ itemfg-loc.loc
             NO-LOCK:
         ASSIGN
            v-qty-onh = v-qty-onh + fg-bin.qty
            v-whse-bin-found = YES.
       END.
/* This code was put in place in task 09081001 to avoid items being picked */
/* up that were not in the selected warehouse. Since this include is */
/* related to reorder levels by warehouse, this extra check is not needed */
/* as the existence of the item in the warehouse is controlled by itemfg-loc */
/*     IF v-whse-bin-found = NO AND                             */
/*        NOT(begin_whse EQ "" AND                              */
/*           (end_whse EQ "zzzzz" OR END_whse EQ "ZZZZZ")) THEN */
/*        NEXT.                                                 */

    ASSIGN
     v-alloc-qty = 0
     v-qty-avail = v-qty-onh + (if v-inconh then itemfg-loc.q-ono else 0).
    
    /* q-alloc is a calculation in I-F-1, but reinstating in task 07181302 */
    /* because itemfg-loc.q-alloc should now be accurate                   */
    IF rd_qoh BEGINS "Total" THEN DO: v-alloc-qty = itemfg-loc.q-alloc.
    END.
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
            AND oe-rel.spare-char-1  EQ itemfg-loc.loc
            AND oe-rel.rel-date LE v-date
            AND oe-rel.s-code   NE "T"
          USE-INDEX ord-item:
          
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

        IF CAN-DO("S,I,L",v-stat) THEN v-alloc-qty = v-alloc-qty + oe-rel.tot-qty.
      END.

      IF rd_qoh BEGINS "Actual" OR rd_qoh BEGINS "All" THEN DO:
        FOR EACH oe-rell  NO-LOCK
            WHERE oe-rell.company EQ oe-ordl.company
              AND oe-rell.ord-no  EQ oe-ordl.ord-no
              AND oe-rell.i-no    EQ oe-ordl.i-no
              AND oe-rell.line    EQ oe-ordl.line
              AND oe-rell.loc     EQ itemfg-loc.loc
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

        FOR EACH oe-boll  NO-LOCK
            WHERE oe-boll.company EQ oe-ordl.company
              AND oe-boll.ord-no  EQ oe-ordl.ord-no
              AND oe-boll.i-no    EQ oe-ordl.i-no
              AND oe-boll.line    EQ oe-ordl.line
              AND oe-boll.loc     EQ itemfg-loc.loc
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
    
    if itemfg-loc.ord-level gt v-qty-avail then do:
       v-reord-qty = itemfg-loc.ord-level - v-qty-avail.

       if v-reord-qty lt itemfg-loc.ord-min and
          itemfg-loc.ord-min ne 0 then 
         v-reord-qty = itemfg-loc.ord-min.

       if v-reord-qty gt itemfg-loc.ord-max and
          itemfg-loc.ord-max ne 0 then 
         v-reord-qty = itemfg-loc.ord-max.
    end.
    else v-reord-qty = 0.


    IF (FIRST-OF(loc.loc) OR FIRST-OF(itemfg.i-no)) 
        AND (v-reord-qty gt 0 or v-prt-all)
        AND v-whse-printed = NO THEN DO:
        
        PUT UNFORMATTED skip(1) "For Warehouse: " loc.loc SKIP(1).
        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                skip(1) "For Warehouse: " itemfg-loc.loc SKIP(1).
       v-whse-printed = YES.
    END.

    IF LAST-OF(loc.loc) THEN
        v-whse-printed = NO.


    if v-reord-qty gt 0 or v-prt-all then
       IF tb_history THEN DO:
           li-hist = 0.

           FOR EACH fg-rcpth
              WHERE fg-rcpth.company    EQ itemfg.company
                AND fg-rcpth.i-no       EQ itemfg-loc.i-no
                AND fg-rcpth.loc        EQ itemfg-loc.loc
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
           IF LAST-OF(itemfg.i-no) THEN DO:
    
           DISPLAY itemfg.i-no
                   itemfg.i-name         FORMAT "x(20)"
                   itemfg.procat
                   itemfg.sell-uom
                   itemfg-loc.ord-level      FORMAT "->>>>>9"
                   v-qty-onh             FORMAT "->>>>>9"
                   v-alloc-qty           FORMAT "->>>>>9"
                   itemfg-loc.q-ono          FORMAT "->>>>>9"
                   itemfg-loc.ord-min        FORMAT ">>>>>>>9"
                   v-qty-avail           FORMAT "->>>>>>9"
                   itemfg.vend-item      FORMAT "x(15)"
                   v-reord-qty
                   li-hist
                 WITH FRAME itemhist NO-BOX DOWN STREAM-IO NO-LABELS WIDTH 180.

           DOWN WITH FRAME itemhist.
          
           IF tb_dash THEN PUT FILL("-",180) FORMAT "x(180)" SKIP.
           
           IF tb_excel THEN 
             EXPORT STREAM excel DELIMITER ","
               itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
               itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono 
               itemfg-loc.ord-min v-qty-avail itemfg.vend-item v-reord-qty li-hist
               itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
           END.
       END.
       ELSE DO:
           if v-prt-cpn then
               if v-prt-qty then
                   if v-prt-prc eq "P" then do:
                       IF LAST-OF(itemfg.i-no) THEN DO:
                           
                           display itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9"
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg.sell-price     column-label "SELLING PRICE"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                              with frame itemx100 no-box down stream-io width 167.
                           down with frame itemx100.
    
                           IF tb_excel THEN 
                             EXPORT STREAM excel DELIMITER ","
                               itemfg.i-no itemfg.part-no  itemfg.i-name 
                               itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                               v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                               v-qty-avail itemfg.sell-price v-reord-qty
                               itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                      END.
                   end.
                   else
                     if v-prt-prc eq "V" then do:
                       IF LAST-OF(itemfg.i-no) THEN DO:
                       
                           display itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9" 
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg.vend-item      FORMAT "x(15)"
                                                         column-label "VENDOR!ITEM NUMBER"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                               with frame itemx200 no-box down stream-io width 167.
                           down with frame itemx200.
    
                           IF tb_excel THEN 
                             EXPORT STREAM excel DELIMITER ","
                               itemfg.i-no itemfg.part-no  itemfg.i-name 
                               itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                               v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                               v-qty-avail itemfg.vend-item v-reord-qty
                               itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                     END.
                     end.
                   else do:
                       IF LAST-OF(itemfg.i-no) THEN DO:
                       
                           display itemfg.i-no           column-label "ITEM #"
                                   itemfg.part-no        format "x(32)"
                                                         column-label "CUST PART #"
                                   itemfg.i-name         format "x(20)"
                                                         column-label "DESCRIPTION"
                                   itemfg.procat         column-label "PROD!CAT"
                                   itemfg.sell-uom       column-label "UOM"
                                   itemfg-loc.ord-level      format "->>>>>9"
                                                         column-label "REORDER!LEVEL"
                                   v-qty-onh             format "->>>>>9"
                                                         column-label "QTY!ON HAND"
                                   v-alloc-qty           format "->>>>>9"
                                                         column-label "QTY!ALLOC"
                                   itemfg-loc.q-ono          format "->>>>>9"    
                                                         column-label "QTY!ON ORD"
                                   itemfg-loc.ord-min        format ">>>>>>>9" 
                                                         column-label "MINIMUM!ORDER"
                                   v-qty-avail           format "->>>>>>9"
                                                         column-label "QTY!AVAIL"
                                   itemfg-loc.ord-max        format ">>>>>>>9"
                                                         column-label "MAXIMUM!ORDER"
                                   v-reord-qty           column-label "SUGGESTED!REORDER"
                               with frame itemx250 no-box down stream-io width 167.
                           down with frame itemx250.
    
                           IF tb_excel THEN 
                             EXPORT STREAM excel DELIMITER ","
                               itemfg.i-no itemfg.part-no itemfg.i-name
                               itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                               v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                               v-qty-avail itemfg-loc.ord-max v-reord-qty
                               itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                     END.
                   end.
               else
                 if v-prt-prc eq "P" then do:
                   IF LAST-OF(itemfg.i-no) THEN DO:
                   
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg.sell-price     column-label "SELLING PRICE"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx300 no-box down stream-io width 167.
                     down with frame itemx300.

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name 
                           itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                           v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                           itemfg.vend-no itemfg.sell-price v-reord-qty
                           itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
               else
                 if v-prt-prc eq "V" then do:
                   IF LAST-OF(itemfg.i-no) THEN DO:
                   
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(10)"
                                                   column-label "!VENDOR"
                             itemfg.vend-item      format "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx400 no-box down stream-io width 167.
                     down with frame itemx400.

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name
                           itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                           v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                           itemfg.vend-no itemfg.vend-item v-reord-qty
                           itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
                 else do:
                     IF LAST-OF(itemfg.i-no) THEN DO:
                     
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.part-no        format "x(32)"
                                                   column-label "CUST PART #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx450 no-box down stream-io width 167.
                     down with frame itemx450.

                     IF tb_excel THEN 
                         EXPORT STREAM excel DELIMITER ","
                           itemfg.i-no itemfg.part-no itemfg.i-name
                           itemfg.procat itemfg.sell-uom itemfg-loc.ord-level
                           v-qty-onh v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min
                           itemfg.vend-no itemfg-loc.ord-max v-reord-qty
                           itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
           else
             if v-prt-qty then
               if v-prt-prc eq "P" then do:
                 IF LAST-OF(itemfg.i-no) THEN DO:
                 
                   display itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label "PROD!CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg-loc.ord-level      format "->>>>>9"
                                                 column-label "REORDER!LEVEL"
                           v-qty-onh             format "->>>>>9"
                                                 column-label "QTY!ON HAND"
                           v-alloc-qty           format "->>>>>9"
                                                 column-label "QTY!ALLOC"
                           itemfg-loc.q-ono          format "->>>>>9"    
                                                 column-label "QTY!ON ORD"
                           itemfg-loc.ord-min        format ">>>>>>>9" 
                                                 column-label "MINIMUM!ORDER"
                           v-qty-avail           format "->>>>>>9"
                                                 column-label "QTY!AVAIL"
                           itemfg.sell-price     column-label "SELLING PRICE"
                           v-reord-qty           column-label "SUGGESTED!REORDER"
                       with frame itemx500 no-box down stream-io width 150.
                   down with frame itemx500.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat
                       itemfg.sell-uom itemfg.ord-level v-qty-onh
                       v-alloc-qty itemfg-loc.q-ono itemfg-loc.ord-min v-qty-avail
                       itemfg.sell-price v-reord-qty
                       itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                 END.
               end.
               else
                 if v-prt-prc eq "V" then do:
                   IF LAST-OF(itemfg.i-no) THEN DO:

                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             v-qty-avail           format "->>>>>>9"
                                                   column-label "QTY!AVAIL"
                             itemfg.vend-item      FORMAT "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx600 no-box down stream-io width 150.
                     down with frame itemx600.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono
                         itemfg-loc.ord-min v-qty-avail itemfg.vend-item v-reord-qty
                         itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
                 else do:
                   IF LAST-OF(itemfg.i-no) THEN DO:
                     
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             v-qty-avail           format "->>>>>>9"
                                                   column-label "QTY!AVAIL"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx650 no-box down stream-io width 150.
                     down with frame itemx650.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono
                         itemfg-loc.ord-min v-qty-avail itemfg-loc.ord-max v-reord-qty
                         itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
             else
               if v-prt-prc eq "P" then DO:
                 IF LAST-OF(itemfg.i-no) THEN DO:
                
                   display itemfg.i-no           column-label "ITEM #"
                           itemfg.i-name         format "x(20)"
                                                 column-label "DESCRIPTION"
                           itemfg.procat         column-label "PROD!CAT"
                           itemfg.sell-uom       column-label "UOM"
                           itemfg-loc.ord-level      format "->>>>>9"
                                                 column-label "REORDER!LEVEL"
                           v-qty-onh             format "->>>>>9"
                                                 column-label "QTY!ON HAND"
                           v-alloc-qty           format "->>>>>9"
                                                 column-label "QTY!ALLOC"
                           itemfg-loc.q-ono          format "->>>>>9"    
                                                 column-label "QTY!ON ORD"
                           itemfg-loc.ord-min        format ">>>>>>>9" 
                                                 column-label "MINIMUM!ORDER"
                           itemfg.vend-no        format "x(12)"
                                                 column-label "!VENDOR"
                           itemfg.sell-price     column-label "SELLING PRICE"
                           v-reord-qty           column-label "SUGGESTED!REORDER"
                       with frame itemx700 no-box down stream-io width 150.
                   down with frame itemx700.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                       itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono
                       itemfg-loc.ord-min itemfg.vend-no 
                       itemfg.sell-price v-reord-qty
                       itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                 END.
               end.
               else
                 if v-prt-prc eq "V" then do:
                   IF LAST-OF(itemfg.i-no) THEN DO:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(10)"
                                                   column-label "!VENDOR"
                             itemfg.vend-item      FORMAT "x(15)"
                                                   column-label "VENDOR!ITEM NUMBER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx800 no-box down stream-io width 150.
                     down with frame itemx800.

                   IF tb_excel THEN 
                     EXPORT STREAM excel DELIMITER ","
                       itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                       itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono
                       itemfg-loc.ord-min itemfg.vend-no 
                       itemfg.vend-item v-reord-qty
                       itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
                 else do:
                   IF LAST-OF(itemfg.i-no) THEN DO:
                     display itemfg.i-no           column-label "ITEM #"
                             itemfg.i-name         format "x(20)"
                                                   column-label "DESCRIPTION"
                             itemfg.procat         column-label "PROD!CAT"
                             itemfg.sell-uom       column-label "UOM"
                             itemfg-loc.ord-level      format "->>>>>9"
                                                   column-label "REORDER!LEVEL"
                             v-qty-onh             format "->>>>>9"
                                                   column-label "QTY!ON HAND"
                             v-alloc-qty           format "->>>>>9"
                                                   column-label "QTY!ALLOC"
                             itemfg-loc.q-ono          format "->>>>>9"    
                                                   column-label "QTY!ON ORD"
                             itemfg-loc.ord-min        format ">>>>>>>9" 
                                                   column-label "MINIMUM!ORDER"
                             itemfg.vend-no        format "x(12)"
                                                   column-label "!VENDOR"
                             itemfg-loc.ord-max        format ">>>>>>>9"
                                                   column-label "MAXIMUM!ORDER"
                             v-reord-qty           column-label "SUGGESTED!REORDER"
                         with frame itemx850 no-box down stream-io width 150.
                     down with frame itemx850.

                     IF tb_excel THEN 
                       EXPORT STREAM excel DELIMITER ","
                         itemfg.i-no itemfg.i-name itemfg.procat itemfg.sell-uom
                         itemfg-loc.ord-level v-qty-onh v-alloc-qty itemfg-loc.q-ono
                         itemfg-loc.ord-min itemfg.vend-no 
                         itemfg-loc.ord-max v-reord-qty
                         itemfg.cust-no v-sales-rep itemfg.total-std-cost itemfg.prod-uom.
                   END.
                 end.
       IF LAST-OF(loc.loc) THEN
       IF tb_dash THEN PUT FILL("-",150) FORMAT "x(180)" SKIP.
    END.
end. /* each itemfg */
