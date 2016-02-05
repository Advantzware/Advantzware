FOR each oe-ordl WHERE
             oe-ordl.company EQ oe-ord.company AND
             oe-ordl.ord-no  EQ oe-ord.ord-no AND
             oe-ordl.i-no ge begin_i-no AND
             oe-ordl.i-no le end_i-no 
             no-lock,
        
        EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.link-no ne 0
        NO-LOCK,
        
        first oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-rel.link-no
          and oe-rell.i-no    eq oe-rel.i-no
          and oe-rell.line    eq oe-rel.line
          and can-find(first oe-relh where oe-relh.r-no eq oe-rell.r-no)
        USE-INDEX r-no no-lock,
      
        each oe-boll
        where oe-boll.company  eq cocode
          and oe-boll.r-no     eq oe-rell.r-no
          and oe-boll.ord-no   eq oe-rell.ord-no
          and oe-boll.rel-no   eq oe-rell.rel-no
          and oe-boll.b-ord-no eq oe-rell.b-ord-no
          and oe-boll.i-no     eq oe-rell.i-no
          and oe-boll.line     eq oe-rell.line
        no-lock,
        
        first oe-bolh
        where oe-bolh.b-no     eq oe-boll.b-no
          AND oe-bolh.bol-date GE begin_bol-date
          AND oe-bolh.bol-date LE end_bol-date
        no-lock

        break by oe-ord.cust-no
              by oe-bolh.bol-date
              by oe-ord.ord-no
              by oe-ordl.i-no:
              
      FIND FIRST itemfg WHERE
           itemfg.company = oe-boll.company AND 
           itemfg.i-no    = oe-boll.i-no
           NO-LOCK NO-ERROR.

      ASSIGN
         v-sqft = IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0.
         v-msf = (oe-boll.qty * v-sqft )/ 1000. 

      if first-of(oe-ord.cust-no) then do:
         
         assign
          v-cust-no = oe-ord.cust-no
          v-name    = cust.NAME.
        
         if first(oe-ord.cust-no) THEN display "" with frame r-top.
         ELSE page.
      end.
      
      v-del[1] = v-del[1] + 1.
      
      if oe-bolh.bol-date le oe-rel.rel-date then v-ont[1] = v-ont[1] + 1.

      display oe-ordl.part-no       column-label "Customer Part#"
              space(2)
              oe-ordl.i-no          column-label "FG Item#"
              space(2)
              oe-ord.ord-no         column-label "Order#"
              space(2)
              oe-ord.ord-date       column-label "Ord Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-rel.rel-date       column-label "Due Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date      column-label "BOL Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date le oe-rel.rel-date format "Y/N"
                                    column-label "On-Time"  SPACE(2)
             v-msf                  COLUMN-LABEL  "MSF" WHEN tb_pmsf SPACE(2)
             oe-boll.weight         COLUMN-LABEL  "WT"  WHEN tb_pw   SPACE(2)
             oe-bolh.trailer        COLUMN-LABEL  "Trailer#"  WHEN tb_ptr
                
            with down no-box stream-io width 200 no-attr-space.
            
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' v-cust-no                                              '",'
            '"' v-name                                                 '",'
            '"' oe-ordl.part-no                                        '",'
            '"' oe-ordl.i-no                                           '",'
            '"' oe-ord.ord-no                                          '",'
            '"' (IF oe-ord.ord-date <> ? THEN STRING(oe-ord.ord-date)
                 ELSE "")                                              '",'
            '"' (IF oe-rel.rel-date <> ? THEN STRING(oe-rel.rel-date)
                 ELSE "")                                              '",'
            '"' (IF oe-bolh.bol-date <> ? THEN STRING(oe-bolh.bol-date)
                 ELSE "")                                              '",'
            '"'  STRING(oe-bolh.bol-date le oe-rel.rel-date,"Y/N")     '",'
            '"' ( IF tb_pmsf THEN v-msf ELSE 0 )                        '",'
            '"' (IF tb_pw THEN  oe-boll.weight ELSE 0 )                 '",'
            '"' (IF tb_ptr THEN oe-bolh.trailer ELSE  "" )              '",'
            SKIP.

      if last-of(oe-ord.cust-no) then do:
        put skip(1)
            "Customer Totals:"          at 5
            space(5)
            "Deliveries: " + trim(string(v-del[1],">,>>>,>>9"))
                                        format "x(21)"
            space(3)
            "On-Time: "    + trim(string(v-ont[1],">,>>>,>>9"))
                                        format "x(18)"
            v-ont[1] / v-del[1] * 100   format ">>9.99%"
            skip(1).
            
        assign
         v-del[2] = v-del[2] + v-del[1]
         v-ont[2] = v-ont[2] + v-ont[1]
         v-del[1] = 0
         v-ont[1] = 0.
      end.
      
      if last(oe-ord.cust-no) then do:
         assign
          v-cust-no = ""
          v-name    = "".
          
         page.
        
         put skip(3)
             "   Grand Totals:"          at 5
             space(5)
             "Deliveries: " + trim(string(v-del[2],">,>>>,>>9"))
                                         format "x(21)"
             space(3)
             "On-Time: "    + trim(string(v-ont[2],">,>>>,>>9"))
                                         format "x(18)"
             v-ont[2] / v-del[2] * 100   format ">>9.99%"
             skip(1).
      end.
   END.
