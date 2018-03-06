/*****************************************************************************
Program: fgrep/r-fgrordN.i

*****************************************************************************/
for each itemfg
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
  AND (itemfg.stat EQ "A" OR tb_inactive)
  
  and ((itemfg.ord-policy     and v-lot-reo eq "R") or
  (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")
  
  and ((itemfg.pur-man        and v-pur-man eq "P") or
  (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")
  
  and ((itemfg.stocked        and v-stocked eq "S") or
  (not itemfg.stocked    and v-stocked eq "N") or v-stocked eq "A")
  
  use-index i-no no-lock,
  EACH itemfg-loc WHERE itemfg-loc.company EQ cocode
  AND itemfg-loc.i-no    EQ itemfg.i-no
  AND itemfg-loc.loc GE begin_whse
  AND itemfg-loc.loc LE END_whse
  NO-LOCK
  BREAK  BY itemfg-loc.loc BY itemfg.i-no  :
    
    IF lExcludeComponents 
          AND NOT itemfg.isaset THEN DO:
          FIND FIRST fg-set 
              WHERE fg-set.company EQ itemfg.company
                AND fg-set.part-no EQ itemfg.i-no
          NO-LOCK NO-ERROR.
          IF AVAIL fg-set THEN NEXT.
      END.

  PROCESS EVENTS.
  STATUS DEFAULT "Processing Item #: " + TRIM(itemfg.i-no). 
  
  BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg)) .
  ASSIGN
  v-whse-bin-found = NO
  v-qty-onh = 0
  v-sales-rep = "".
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
/*   IF v-whse-bin-found = NO AND                       */
/*   NOT(begin_whse EQ "" AND                           */
/*   (end_whse EQ "zzzzz" OR END_whse EQ "ZZZZZ")) THEN */
/*   NEXT.                                              */

  ASSIGN
  v-alloc-qty = 0
  v-qty-avail = v-qty-onh + (if v-inconh then itemfg-loc.q-ono else 0).
  
  /* q-alloc is a calculation in I-F-1, but reinstating in 07181302 */
  /* since q-alloc should now be accurate                           */
  IF rd_qoh BEGINS "Total" THEN DO: 
      v-alloc-qty = itemfg-loc.q-alloc.
  END.
  ELSE DO:
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
          AND oe-rel.s-code   EQ "T"
          USE-INDEX ord-item:
          
          RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).
          
          IF CAN-DO("S,I,L",v-stat) THEN v-alloc-qty = v-alloc-qty + oe-rel.tot-qty.
        END.
        
        IF rd_qoh BEGINS "Actual" OR rd_qoh BEGINS "All" THEN DO:
          FOR EACH oe-rell FIELDS(r-no qty) NO-LOCK
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
          
          FOR EACH oe-boll FIELDS(b-no qty) NO-LOCK
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
          END. /* each oe-boll */
        END.  /* if begins actual or all */
    
    END. /* each oe-ordl */
  end. /* else do: */
  
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

  ASSIGN v-reord-msf = ((v-reord-qty / 1000) * itemfg.t-sqft) .

  IF (FIRST-OF(itemfg-loc.loc) OR first-of(itemfg.i-no)) AND (v-reord-qty gt 0 or v-prt-all) 
      AND v-this-loc-printed = NO THEN DO:
      
      PUT UNFORMATTED skip(1) "For Warehouse: " itemfg-loc.loc SKIP(1).
      /*IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
        skip(1) "For Warehouse: " itemfg-loc.loc SKIP(1).*/

    v-this-loc-printed = YES.
    
  END.
  IF LAST-OF(itemfg-loc.loc) THEN
      v-this-loc-printed = NO.

  ASSIGN v-last-ship = "".
         v-po-no = "" .
         IF LOOKUP("PO DUE DATE", cSelectedList) GT 0 THEN DO:
          
          FOR EACH oe-ordl NO-LOCK
               WHERE oe-ordl.company EQ itemfg.company
               AND oe-ordl.i-no    EQ itemfg.i-no
               AND oe-ordl.po-no-po <> 0
               AND oe-ordl.opened  EQ YES
               AND oe-ordl.stat    NE "C"
               USE-INDEX item,
               
               FIRST oe-ord NO-LOCK
               WHERE oe-ord.company EQ oe-ordl.company
               AND oe-ord.ord-no  EQ oe-ordl.ord-no
               AND oe-ord.type    NE "T" BY oe-ord.ord-date DESC :

                 v-po-no = string(oe-ordl.po-no-po) .
                 LEAVE.
           END.
         END.
         IF LOOKUP("LAST SHIP", cSelectedList) GT 0 THEN DO:
           
           FOR EACH fg-rcpth 
               WHERE fg-rcpth.company      EQ itemfg.company
               AND fg-rcpth.i-no         EQ itemfg.i-no
               AND fg-rcpth.rita-code    EQ "S" 
               NO-LOCK BY fg-rcpth.trans-date DESC:

               v-last-ship = string(fg-rcpth.trans-date).
               LEAVE.
           END.
         END.
  ASSIGN v-po-due-dt = "" .
        IF LOOKUP("PO DUE DATE", cSelectedList) GT 0 THEN DO:
           FOR EACH po-ordl WHERE po-ordl.company = itemfg.company
               AND lookup(po-ordl.stat, "o,p,u,a") > 0
               AND po-ordl.opened EQ YES
               /*AND po-ordl.item-type  = NO*/  /* task 07301405 */
                AND po-ordl.po-no = int(v-po-no)
               NO-LOCK BY po-ordl.due-date DESC :

               IF AVAIL po-ordl THEN ASSIGN v-po-due-dt = string(po-ordl.due-date) .
               LEAVE.
           END.
        END.
  ASSIGN v-job-due-dt = "" .
           FOR EACH job-hdr WHERE job-hdr.company = itemfg.company
               AND job-hdr.i-no EQ itemfg.i-no
               AND job-hdr.opened EQ YES NO-LOCK BY job-hdr.due-date DESC :

               IF AVAIL job-hdr THEN ASSIGN v-job-due-dt = string(job-hdr.due-date) .
               LEAVE.
           END.
           IF v-job-due-dt = ? THEN
             v-job-due-dt = "".
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

          ASSIGN li-avg-hist = 0.
           li-avg-hist = (li-hist[2] + li-hist[3] + li-hist[4] + li-hist[5] + li-hist[6]) / 5 .
          
  if v-reord-qty gt 0 or v-prt-all then
  IF tb_history THEN DO:
    
   IF LAST-OF(itemfg.i-no) THEN DO:
      v-ord-level = itemfg-loc.ord-level.
      v-ord-min   = itemfg-loc.ord-min.
      v-ord-max   = itemfg-loc.ord-max.
      v-q-ono     = itemfg-loc.q-ono.

      
      cDisplay = "".
      cTmpField = "".
      cVarValue = "".
      cExcelDisplay = "".
      cExcelVarValue = "".
      DO i = 1 TO NUM-ENTRIES(cSelectedlist):
        /*FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cSelectedList) NO-LOCK NO-ERROR.
        hField = BUFFER bitemfg:BUFFER-FIELD(substring(ttRptList.FieldList,INDEX(ttRptList.FieldList,".") + 1)).
        */
        cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        IF INDEX(cTmpField,".") > 0 THEN DO:
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
          cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
          cDisplay = cDisplay + cTmpField +
          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
          .
          cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
        END.
        ELSE DO:
          CASE cTmpField:
            WHEN "v-qty-onh" THEN ASSIGN cVarValue = string(v-qty-onh)
                cExcelVarValue = "".
            WHEN "v-alloc-qty" THEN ASSIGN cVarValue = string(v-alloc-qty)
                cExcelVarValue = "".
            WHEN "v-qty-avail" THEN ASSIGN cVarValue = string(v-qty-avail)
                cExcelVarValue = "".
            WHEN "v-reord-qty" THEN ASSIGN cVarValue = string(v-reord-qty)
                cExcelVarValue = "".
            WHEN "msf-reord" THEN ASSIGN cVarValue = string(v-reord-msf,">>>>>>>>9.99999")
                 cExcelVarValue = "".
            WHEN "v-ord-level" THEN ASSIGN cVarValue = STRING(v-ord-level)
                cExcelVarValue = "".
            WHEN "v-ord-min"   THEN ASSIGN cVarValue = STRING(v-ord-min)
                cExcelVarValue = "".
            WHEN "v-ord-max"   THEN ASSIGN cVarValue = STRING(v-ord-max)
                cExcelVarValue = "".
            WHEN "v-q-ono"     THEN ASSIGN cVarValue = STRING(v-q-ono)
                cExcelVarValue = "".
            WHEN "whs-day" THEN ASSIGN cVarValue = IF AVAIL cust THEN string(cust.ship-days,"->>>>>>>") ELSE ""
                cExcelVarValue = "".
            WHEN "last-ship" THEN ASSIGN
                 cVarValue = string(v-last-ship)
                 cExcelVarValue = "".
            WHEN "v-rep" THEN ASSIGN cVarValue = string(v-sales-rep)
                              cExcelVarValue = "".
            WHEN "po-due-dt" THEN ASSIGN cVarValue = string(v-po-due-dt)
                              cExcelVarValue = "".
            WHEN "job-due-dt" THEN ASSIGN cVarValue = string(v-job-due-dt)
                              cExcelVarValue = "".
            WHEN "mo-avg" THEN ASSIGN cVarValue = STRING(li-avg-hist,"->>>>>>9") 
                              cExcelVarValue = "".
            WHEN "sug-avg" THEN DO:
                ASSIGN cVarValue = STRING(v-reord-qty - li-avg-hist,"->,>>>,>>9")  
                   cExcelVarValue = "".
            END.
            WHEN "whse" THEN ASSIGN cVarValue = STRING(itemfg-loc.loc,"x(5)") 
                              cExcelVarValue = "".
            WHEN "li-hist" THEN
              ASSIGN cVarValue = STRING(li-hist[1],"->>>>>9") + " "  + STRING(li-hist[2],"->>>>>9") + " "  + STRING(li-hist[3],"->>>>>9") + " "  +
              STRING(li-hist[4],"->>>>>9") + " "  + STRING(li-hist[5],"->>>>>9") + " "  + STRING(li-hist[6],"->>>>>9")
              cExcelVarValue = quoter(STRING(li-hist[1],"->>>>>9")) + ","  + quoter(STRING(li-hist[2],"->>>>>9")) + ","  + quoter(STRING(li-hist[3],"->>>>>9")) + ","  +
              quoter(STRING(li-hist[4],"->>>>>9")) + ","  + quoter(STRING(li-hist[5],"->>>>>9")) + ","  + quoter(STRING(li-hist[6],"->>>>>9")).
          END CASE.
          IF cExcelVarValue = "" THEN cExcelVarValue = cVarValue.
          cDisplay = cDisplay + cVarValue +
          FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).
          cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".
        END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      
      IF tb_dash THEN PUT FILL("-",300) FORMAT "x(300)" SKIP.
      
      IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
      cExcelDisplay SKIP.
    END. /* last of item */
  END. /* if tb_hist */
  ELSE DO:
    IF LAST-OF(itemfg.i-no) THEN DO:
       v-ord-level = itemfg-loc.ord-level.
       v-ord-min   = itemfg-loc.ord-min.
       v-ord-max   = itemfg-loc.ord-max.
       v-q-ono     = itemfg-loc.q-ono.
    END.
    
    cDisplay = "".
    cTmpField = "".
    cExcelDisplay = "".
    cVarValue = "".
    cExcelVarValue = "".
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):
      cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
      IF INDEX(cTmpField,".") > 0 THEN DO:
        cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
        hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
        cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
        cDisplay = cDisplay + cTmpField +
        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
        .
        cExcelDisplay = cExcelDisplay + quoter(cTmpField) + ",".
        
      END.
      ELSE DO:
        /*{sys/inc/rptDisp2.i cTmpField "2"} */
        /*RUN getVarValue (cTmpField, OUTPUT cVarValue).*/
        CASE cTmpField:
          WHEN "v-qty-onh" THEN ASSIGN cVarValue = string(v-qty-onh)
              cExcelVarValue = "".
          WHEN "v-alloc-qty" THEN ASSIGN cVarValue = string(v-alloc-qty)
              cExcelVarValue = "".
          WHEN "v-qty-avail" THEN ASSIGN cVarValue = string(v-qty-avail)
              cExcelVarValue = "".
          WHEN "v-ord-level" THEN ASSIGN cVarValue = STRING(v-ord-level)
                cExcelVarValue = "".
          WHEN "v-reord-qty" THEN ASSIGN cVarValue = string(v-reord-qty)
              cExcelVarValue = "".
          WHEN "msf-reord" THEN ASSIGN cVarValue = string(v-reord-msf,">>>>>>>>9.99999")
                              cExcelVarValue = "".
          WHEN "v-ord-min"   THEN ASSIGN cVarValue = STRING(v-ord-min)
              cExcelVarValue = "".
          WHEN "v-ord-max"   THEN ASSIGN cVarValue = STRING(v-ord-max)
              cExcelVarValue = "".
          WHEN "v-q-ono"     THEN ASSIGN cVarValue = STRING(v-q-ono)
              cExcelVarValue = "".
          WHEN "whs-day" THEN ASSIGN cVarValue = IF AVAIL cust THEN string(cust.ship-days,"->>>>>>>") ELSE ""
                 cExcelVarValue = "".
          WHEN "last-ship" THEN ASSIGN cVarValue = string(v-last-ship)
                cExcelVarValue = "".
          WHEN "v-rep" THEN ASSIGN cVarValue = string(v-sales-rep)
                              cExcelVarValue = "".
          WHEN "po-due-dt" THEN ASSIGN cVarValue = string(v-po-due-dt)
                              cExcelVarValue = "".
          WHEN "job-due-dt" THEN ASSIGN cVarValue = string(v-job-due-dt)
                              cExcelVarValue = "".
          WHEN "mo-avg" THEN ASSIGN cVarValue = STRING(li-avg-hist,"->>>>>>9") 
                              cExcelVarValue = "".
          WHEN "sug-avg" THEN DO:
              ASSIGN cVarValue = STRING(v-reord-qty - li-avg-hist,"->,>>>,>>9")  
                 cExcelVarValue = "".
          END.
          WHEN "whse" THEN ASSIGN cVarValue = STRING(itemfg-loc.loc,"x(5)") 
                              cExcelVarValue = "".
          WHEN "li-hist" THEN
            ASSIGN cVarValue = STRING(li-hist[1],"->>>>>9") + " "  + STRING(li-hist[2],"->>>>>9") + " "  + STRING(li-hist[3],"->>>>>9") + " "  +
            STRING(li-hist[4],"->>>>>9") + " "  + STRING(li-hist[5],"->>>>>9") + " "  + STRING(li-hist[6],"->>>>>9")
            cExcelVarValue = quoter(STRING(li-hist[1],"->>>>>9")) + ","  + quoter(STRING(li-hist[2],"->>>>>9")) + ","  + quoter(STRING(li-hist[3],"->>>>>9")) + ","  +
            quoter(STRING(li-hist[4],"->>>>>9")) + ","  + quoter(STRING(li-hist[5],"->>>>>9")) + ","  + quoter(STRING(li-hist[6],"->>>>>9")).
        END CASE.
        IF cExcelVarValue = "" THEN cExcelVarValue = cVarValue.

        cDisplay = cDisplay + cVarValue +
        FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).
        cExcelDisplay = cExcelDisplay + quoter(cVarValue) + ",".
      END.
    END.
    PUT UNFORMATTED     cDisplay SKIP.
    IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
    cExcelDisplay SKIP.
    IF LAST-OF(itemfg-loc.loc) THEN
    IF tb_dash THEN PUT FILL("-",300) FORMAT "x(300)" SKIP.
    
    
  END. /* not tb_hist */
end. /* each itemfg */


