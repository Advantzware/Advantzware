    /* wfk - test */    
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    
    for each oe-ord
        where oe-ord.company  eq cocode
          and oe-ord.cust-no  ge v-cust[1]
          and oe-ord.cust-no  le v-cust[2]
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-ord.cust-no
          AND ttCustList.log-fld no-lock) else true)
          and oe-ord.ord-date ge v-date[1]
          and oe-ord.ord-date le v-date[2]
          AND ((oe-ord.po-no  GE v-po[1] AND
                oe-ord.po-no  LE v-po[2]))
          AND (v-ostat EQ "A"                           OR
               (oe-ord.opened AND v-ostat EQ "O")       OR
               (NOT oe-ord.opened AND v-ostat EQ "C"))
        use-index ordate no-lock,

        each oe-ordl of oe-ord
        where oe-ordl.i-no     ge v-item[1]
          and oe-ordl.i-no     le v-item[2]
          and fill(" ",6 - length(trim(oe-ordl.job-no))) +
              trim(oe-ordl.job-no) + string(oe-ordl.job-no2,"99") ge v-job[1]
          and fill(" ",6 - length(trim(oe-ordl.job-no))) +
              trim(oe-ordl.job-no) + string(oe-ordl.job-no2,"99") le v-job[2]
          AND oe-ordl.s-man[1] GE begin_slmn
          AND oe-ordl.s-man[1] LE end_slmn
          AND ((oe-ordl.po-no  GE v-po[1] AND
                oe-ordl.po-no  LE v-po[2]))
        no-lock:
    /*STATUS DEFAULT STRING(oe-ordl.i-no).*/
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 
      {oerep/r-ordop1.i}
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "":
      tt-report.q-onh = 0.
      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company   EQ cocode
            AND tt-fg-bin.i-no      EQ tt-report.key-06
            AND ((tt-fg-bin.job-no  EQ SUBSTR(tt-report.key-04,1,6) AND
                  tt-fg-bin.job-no2 EQ INT(SUBSTR(tt-report.key-04,8,2))) OR
                 SUBSTR(tt-report.key-04,1,6) EQ ""):
        tt-report.q-onh = tt-report.q-onh + tt-fg-bin.qty.
      END.
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND (tb_0-qoh OR tt-report.q-onh GT 0),

        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq tt-report.key-06
        NO-LOCK,

        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq tt-report.key-02
        NO-LOCK,

        FIRST oe-ordl WHERE ROWID(oe-ordl) EQ tt-report.row-id NO-LOCK,

        FIRST oe-ord OF oe-ordl NO-LOCK
            
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
              BY tt-report.key-05
              BY tt-report.key-06
              BY tt-report.row-id
              BY tt-report.key-07:

      release oe-rell.
      release ar-invl.
      RELEASE oe-rel.
        
      /*STATUS DEFAULT STRING(oe-ordl.i-no).*/
      {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

      if first-of(tt-report.key-01) AND tb_break then do:
        page.
      end.
      find first sman
            where sman.company eq cocode
              and sman.sman    eq oe-ordl.s-man[1]
            no-lock no-error.
        assign
         v-sman  = oe-ordl.s-man[1]
         v-sname = if avail sman then sman.sname else "Not on File".

      if first-of(tt-report.key-02) AND NOT tb_break then do:
        page.
      end.
      assign
         v-cust-no = tt-report.key-02
         v-name    = cust.name.

      find oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock no-error.

      if avail oe-rell then 
        find first oe-relh
            where oe-relh.r-no eq oe-rell.r-no
            no-lock.

      else do:
        find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.
        if avail ar-invl then do:
           find first oe-bolh
               where oe-bolh.company eq cocode
                 and oe-bolh.bol-no  eq ar-invl.bol-no
               no-lock no-error.
           if avail oe-bolh then do:
             find first oe-boll
                 where oe-boll.company eq cocode
                   and oe-boll.b-no    eq oe-bolh.b-no
                   and oe-boll.i-no    eq ar-invl.i-no
                 no-lock no-error.
             if avail oe-boll then do:
               find first oe-rell
                   where oe-rell.company eq cocode
                     and oe-rell.r-no    eq oe-boll.r-no
                     AND oe-rell.ord-no  EQ oe-boll.ord-no
                     and oe-rell.i-no    eq oe-boll.i-no
                     and oe-rell.line    eq oe-boll.line
                     no-lock no-error.
               if avail oe-rell then find first oe-relh of oe-rell no-lock.
             end.
           end.
        end.

        ELSE DO:
           find inv-line where recid(inv-line) eq tt-report.rec-id no-lock no-error.
           IF AVAIL inv-line THEN DO:
              find first oe-bolh
                  where oe-bolh.company eq cocode
                    and oe-bolh.b-no    eq inv-line.b-no
                  no-lock no-error.
              if avail oe-bolh then do:
                find first oe-boll
                    where oe-boll.company eq cocode
                      and oe-boll.b-no    eq oe-bolh.b-no
                      and oe-boll.i-no    eq inv-line.i-no
                    no-lock no-error.
                if avail oe-boll then do:
                  find first oe-rell
                      where oe-rell.company eq cocode
                        and oe-rell.r-no    eq oe-boll.r-no
                        and oe-rell.i-no    eq oe-boll.i-no
                        and oe-rell.line    eq oe-boll.line
                      USE-INDEX r-no no-lock no-error.
                  if avail oe-rell then find first oe-relh of oe-rell no-lock.
                end.
              end.
           END.
           ELSE
              find oe-rel where recid(oe-rel) eq tt-report.rec-id no-lock no-error.
           
        END.
      end.

      if first-of(tt-report.row-id) /*OR
         rd_prt-po EQ "Release"*/ then do:
        assign
         v-field1 = string(tt-report.key-06,"x(15)")   + " " +
                    string(oe-ord.ord-date,"99/99/99") + " " +
                    string(oe-ordl.qty,"->>,>>>,>>9")
         v-ord-no = string(oe-ord.ord-no).

       /* display tt-report.po-no   when first-of(tt-report.key-03) /*OR rd_prt-po EQ "Release"*/
                                  format "x(15)"                      
                v-ord-no          format "x(11)"
             
         with frame detail no-box stream-io width 180
              no-attr-space no-underline no-labels.*/
          
        if first-of(tt-report.row-id) THEN
           v-bal-qty = oe-ordl.qty.
      end.

      assign
       v-bal-qty = v-bal-qty -
                   (IF tt-report.inv THEN tt-report.q-shp
                    ELSE IF AVAIL oe-rell AND NOT tt-report.inv THEN oe-rell.qty
                    ELSE IF AVAIL oe-rel AND NOT tt-report.inv THEN oe-rel.tot-qty
                    else 0)
       v-dat     = if avail oe-rell or AVAIL oe-rell OR tt-report.inv then
                     date(int(substr(tt-report.key-07,5,2)),
                          int(substr(tt-report.key-07,7,2)),
                          int(substr(tt-report.key-07,1,4))) else ?.

      if v-bal-qty lt 0 then v-bal-qty = 0.

      assign
       v-field2 = if avail ar-invl then string(ar-invl.amt,">>>>,>>9.99")
                  else
                  if avail inv-line then string(inv-line.t-price,">>>>,>>9.99")
                  else ""
       v-field2 = fill(" ",11 - length(trim(v-field2))) + trim(v-field2)
       v-q-onh  = tt-report.q-onh.

      ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
           
              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField: 
                           WHEN "sman"     THEN cVarValue = STRING(v-sman) .
                           WHEN "sname"    THEN cVarValue = STRING(v-sname,"x(25)") .
                           WHEN "cust"     THEN cVarValue = STRING(v-cust-no,"x(8)") .
                           WHEN "cname"    THEN cVarValue = STRING(v-name,"x(30)") .
                           WHEN "ord-po"   THEN cVarValue = STRING(oe-ord.po-no,"x(15)") .
                           WHEN "item-po"  THEN cVarValue = STRING(oe-ordl.po-no,"x(15)") .
                           WHEN "rel-po"   THEN cVarValue = STRING(tt-report.po-no,"x(15)") .
                           WHEN "ordjob"   THEN cVarValue = STRING(v-ord-no) .
                           WHEN "jobno"    THEN cVarValue = IF trim(tt-report.key-04) NE "-00" THEN STRING(tt-report.key-04) ELSE "" .
                           WHEN "fgitem"    THEN cVarValue = STRING(tt-report.key-06) . 
                           WHEN "i-name"    THEN cVarValue = STRING(oe-ordl.i-name,"x(25)") .
                           WHEN "cust-part" THEN cVarValue = STRING(oe-ordl.part-no) .
                           WHEN "ord-date"  THEN cVarValue = IF oe-ord.ord-date NE ? THEN string(oe-ord.ord-date,"99/99/99") ELSE "" .
                           WHEN "ord-qty"   THEN cVarValue = STRING(oe-ordl.qty,"->>,>>>,>>9") .
                           WHEN "invrel"    THEN cVarValue = IF tt-report.inv THEN STRING(tt-report.inv-no) ELSE "" .
                           WHEN "date-inv"  THEN cVarValue = IF v-dat NE ? THEN STRING(v-dat,"99/99/99") ELSE "" .
                           WHEN "qty-ship"  THEN cVarValue = STRING(tt-report.q-shp,"->>,>>>,>>>") .
                           WHEN "rel-qty"   THEN cVarValue = IF AVAIL oe-rel AND NOT tt-report.inv THEN STRING(oe-rel.tot-qty,"->>,>>>,>>>")  ELSE iF AVAIL oe-rell AND NOT tt-report.inv THEN STRING(oe-rell.qty,"->>,>>>,>>>") ELSE "" .
                           WHEN "bal-due"   THEN cVarValue =  STRING(v-bal-qty,"->>,>>>,>>>").
                           WHEN "inv-amt"   THEN cVarValue =  STRING(v-field2).
                           WHEN "qty-on-hand"  THEN cVarValue = IF last-of(tt-report.row-id) THEN STRING(v-q-onh,"->>,>>>,>>>") ELSE  "" .
                           
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

      /*display v-field1          
              v-dat               when v-dat ne ?
              space(5)
              tt-report.inv-no    WHEN tt-report.inv 
                                  FORMAT ">>>>>>"
              tt-report.q-shp     FORMAT "->>,>>>,>>>"
              oe-rell.qty         WHEN AVAIL oe-rell AND NOT tt-report.inv
              oe-rel.tot-qty      WHEN AVAIL oe-rel AND NOT tt-report.inv @ oe-rell.qty
              v-field2
              v-q-onh             when last-of(tt-report.row-id) 
                                  format "->>,>>>,>>9"

          with frame detail.

      IF tb_excel THEN DO:
        IF tb_break THEN do:
          PUT STREAM excel UNFORMATTED
              '"' v-sman                                            '",'
              '"' v-sname                                           '",'
              '"' v-cust-no                                         '",'
              '"' v-name                                            '",'
              '"' tt-report.po-no                                   '",'.
           IF v-part THEN
              PUT STREAM excel UNFORMATTED
              '"' oe-ordl.part-no                                   '",'.
              PUT STREAM excel UNFORMATTED 
              '"' v-ord-no                                          '",'
              '"' tt-report.key-06                                  '",'
              '"' oe-ordl.i-name                                    '",'
              '"' (IF oe-ord.ord-date NE ? THEN
                    string(oe-ord.ord-date,"99/99/99") ELSE "")     '",'
              '"' string(oe-ordl.qty,"->>,>>>,>>9")                 '",'
              '"' (IF v-dat NE ? THEN STRING(v-dat)
                   ELSE "")                                         '",'
              '"' (IF tt-report.inv THEN STRING(tt-report.inv-no)
                   ELSE "")                                         '",'
              '"' tt-report.q-shp                                   '",'
              '"' (IF AVAIL oe-rell AND NOT tt-report.inv THEN
                     STRING(oe-rell.qty)
                  ELSE IF AVAIL oe-rel AND NOT tt-report.inv THEN
                     STRING(oe-rel.tot-qty) ELSE "")                '",'
              '"' v-field2                                          '",'
              '"' (IF last-of(tt-report.row-id) THEN
                     STRING(v-q-onh,"->>,>>>,>>9") ELSE "")         '",'
             SKIP. 
        END.
        ELSE do:
          PUT STREAM excel UNFORMATTED
              '"' v-cust-no                                         '",'
              '"' v-name                                            '",'
              '"' tt-report.po-no                                   '",'.
          IF v-part THEN
              PUT STREAM excel UNFORMATTED
              '"' oe-ordl.part-no                                   '",'.
              PUT STREAM excel UNFORMATTED       
              '"' v-ord-no                                          '",'
              '"' tt-report.key-06                                  '",'
              '"' oe-ordl.i-name                                    '",'
              '"' (IF oe-ord.ord-date NE ? THEN
                   string(oe-ord.ord-date,"99/99/99") ELSE "")      '",'
              '"' string(oe-ordl.qty,"->>,>>>,>>9")                 '",' 
              '"' (IF v-dat NE ? THEN STRING(v-dat)
                   ELSE "")                                         '",'
              '"' (IF tt-report.inv THEN STRING(tt-report.inv-no)
                   ELSE "")                                         '",'
              '"' tt-report.q-shp                                   '",'
              '"' (IF AVAIL oe-rell AND NOT tt-report.inv THEN
                      STRING(oe-rell.qty)
                   ELSE IF AVAIL oe-rel AND NOT tt-report.inv THEN
                      STRING(oe-rel.tot-qty)
                   ELSE "")                   '",'
              '"' v-field2                                          '",'
              '"' (IF last-of(tt-report.row-id) THEN
                     STRING(v-q-onh,"->>,>>>,>>9") ELSE "")         '",'
             SKIP.
        END.
      END.*/

      IF NOT tt-report.inv THEN
      DO:
         IF AVAIL oe-rell THEN v-q-rel = v-q-rel + oe-rell.qty.
         ELSE IF AVAIL oe-rel THEN v-q-rel = v-q-rel + oe-rel.tot-qty.
      END.
              
      v-field1 = /*if first-of(tt-report.row-id) then*/ oe-ordl.i-name /*else ""*/ .

     if last-of(tt-report.row-id) then do:
        /*DO WITH FRAME detail:
          DOWN.

          IF v-part THEN
              DISPLAY oe-ordl.part-no @ tt-report.po-no .

          if trim(tt-report.key-04) ne "-00" then
            display trim(tt-report.key-04) @ v-ord-no.

          if (/*first-of(tt-report.row-id) and*/ v-field1 ne "") then
            display v-field1.
        END.*/

        if v-jobq and tt-report.q-onh gt 0 then
        for each tt-fg-bin
            where tt-fg-bin.company           eq cocode
              and tt-fg-bin.i-no              eq tt-report.key-06
              and tt-fg-bin.qty               gt 0
              and ((tt-fg-bin.job-no          eq substr(tt-report.key-04,1,6) and
                    tt-fg-bin.job-no2         eq int(substr(tt-report.key-04,8,2))) or
                   substr(tt-report.key-04,1,6) eq "")
            no-lock
            break by tt-fg-bin.job-no
                  by tt-fg-bin.job-no2
                  by tt-fg-bin.loc
                  by tt-fg-bin.loc-bin
                  by tt-fg-bin.tag:
        
          if first(tt-fg-bin.job-no2) THEN DO:
            put skip(1)
                space(34)
                "Bins: Job "
                "Whs   "
                "Bin      "
                "Tag      "
                "        Qty"
                skip
                space(34)
                "--------- "
                "----- "
                "-------- "
                "-------- "
                "-----------"
                skip.

            IF tb_excel THEN DO:
              IF tb_break THEN
                PUT STREAM excel UNFORMATTED
                    '"' ""                                                '",'
                    '"' ""                                                '",'
                    '"' ""                                                '",'
                    '"' ""                                                '",'
                    '"' ""                                                '",'
                    '"' ""                                                '",'
                    '"' "Bins: Job "                                      '",'
                    '"' "Whs"                                             '",'
                    '"' "Bin"                                             '",'
                    '"' "Tag"                                             '",'
                    '"' "Qty"                                             '",'
                   SKIP.
              ELSE
                PUT STREAM excel UNFORMATTED
                    '"' ""                                               '",'
                    '"' ""                                               '",'
                    '"' ""                                               '",'
                    '"' ""                                               '",'
                    '"' "Bins: Job "                                     '",'
                   '"' "Whs"                                             '",'
                   '"' "Bin"                                             '",'
                   '"' "Tag"                                             '",'
                   '"' "Qty"                                             '",'
                   SKIP.
            END.
          END.
        
        
          put space(34)
              fill(" ",6 - length(trim(tt-fg-bin.job-no))) +
              trim(tt-fg-bin.job-no) + 
              (if tt-fg-bin.job-no ne "" then ("-" + string(tt-fg-bin.job-no2,"99"))
               else "")                   format "x(9)"
              space(1)
              tt-fg-bin.loc
              space(1)
              tt-fg-bin.loc-bin
              space(1).

          IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN
            PUT SUBSTR(tt-fg-bin.tag,16,8) FORMAT "x(8)".
          ELSE
            PUT tt-fg-bin.tag.

          PUT SPACE(1)
              tt-fg-bin.qty               FORMAT ">>>,>>>,>>9"
              SKIP.

          IF tb_excel THEN DO:
            IF tb_break THEN
              PUT STREAM excel UNFORMATTED
                  '"' ""                                                '",'
                  '"' ""                                                '",'
                  '"' ""                                                '",'
                  '"' ""                                                '",'
                  '"' ""                                                '",'
                  '"' ""                                                '",'
                  '"' trim(tt-fg-bin.job-no) + 
                      (if tt-fg-bin.job-no ne "" then "-"
                      + string(tt-fg-bin.job-no2,"99") ELSE "")         '",'
                  '"' tt-fg-bin.loc                                     '",'
                  '"' tt-fg-bin.loc-bin                                 '",'
                  '"' (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                       THEN SUBSTR(tt-fg-bin.tag,16,8) ELSE tt-fg-bin.tag) '",'
                  '"' STRING(tt-fg-bin.qty,">>>,>>>,>>9")               '",'
                 SKIP.
            ELSE
              PUT STREAM excel UNFORMATTED
                  '"' ""                                               '",'
                  '"' ""                                               '",'
                  '"' ""                                               '",'
                  '"' ""                                                '",'
                  '"' trim(tt-fg-bin.job-no) + 
                      (if tt-fg-bin.job-no ne "" then "-"
                      + string(tt-fg-bin.job-no2,"99") ELSE "")         '",'
                  '"' tt-fg-bin.loc                                     '",'
                  '"' tt-fg-bin.loc-bin                                 '",'
                  '"' (IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no
                       THEN SUBSTR(tt-fg-bin.tag,16,8) ELSE tt-fg-bin.tag) '",'
                  '"' STRING(tt-fg-bin.qty,">>>,>>>,>>9")               '",'
                  SKIP.
          END.

          if last(tt-fg-bin.job-no2) then put skip(1).
        end.            

        ELSE PUT SKIP(1).

        v-q-rel = 0.
      end.  

      delete tt-report.
    end. /* each tt-report */
