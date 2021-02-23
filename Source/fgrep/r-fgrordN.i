/*****************************************************************************
 Program: fgrep/r-fgrordN.i
 
*****************************************************************************/
DEFINE VARIABLE hftJobPros AS HANDLE NO-UNDO.
DEFINE VARIABLE cCsrName   AS CHARACTER NO-UNDO.

RUN jc/JobProcs.p PERSISTENT SET hftJobPros.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftJobPros).
MAIN-CUST:
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
     AND itemfg.part-no GE fcpart
     AND itemfg.part-no LE tcpart

     and ((itemfg.ord-policy     and v-lot-reo eq "R") or
          (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")

     and ((itemfg.pur-man        and v-pur-man eq "P") or
          (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")

     and ((itemfg.stocked        and v-stocked eq "S") or
          (not itemfg.stocked    and v-stocked eq "N") or v-stocked eq "A")

     use-index i-no no-lock:

     if ou-cust-int EQ 2 and tb_cust-list
     and can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no no-lock) then NEXT MAIN-CUST.
    
    IF lExcludeComponents 
        AND NOT itemfg.isaset THEN DO:
        FIND FIRST fg-set 
            WHERE fg-set.company EQ itemfg.company
              AND fg-set.part-no EQ itemfg.i-no
        NO-LOCK NO-ERROR.
        IF AVAIL fg-set THEN NEXT.
    END.

    {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"}
    BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
    ASSIGN
       v-whse-bin-found = NO
       v-qty-onh        = 0
       v-sales-rep      = ""
       cMachine         = "" 
       cCsrName         = ""
       cItemLoc         = "" 
       cGroup           = ""
       cItemLocation    = "".

       /*Added for premier mod 08291201*/
    FIND FIRST cust WHERE cust.company = itemfg.company
        AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
    if AVAIL cust then
    find first users no-lock
         WHERE users.user_id eq cust.csrUser_id no-error . 
     cCsrName = if AVAIL cust and AVAIL users then users.user_name else "" .

    IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:
    FOR EACH cust-part WHERE cust-part.company = itemfg.company   
        AND cust-part.i-no = itemfg.i-no
        AND cust-part.cust-no EQ itemfg.cust-no NO-LOCK:
        
       IF cust-part.spare-char-1 NE "" THEN do:
           FIND FIRST sman WHERE sman.company = itemfg.company
               AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
           IF AVAIL sman THEN v-sales-rep = sman.sname.
          LEAVE .
       END.
    END.

    IF v-sales-rep EQ "" AND itemfg.spare-char-3 NE "" THEN do:
        FIND FIRST sman WHERE sman.company = itemfg.company
            AND sman.sman = itemfg.spare-char-3 NO-LOCK NO-ERROR.
        IF AVAIL sman THEN v-sales-rep = sman.sname.
    END.
    ELSE IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
        FIND FIRST sman WHERE sman.company = cust.company
            AND sman.sman = cust.sman NO-LOCK NO-ERROR.
        IF AVAIL sman THEN v-sales-rep = sman.sname.
    END.
    /*ELSE v-sales-rep = "" .*/
   END.
   ELSE DO:
    FIND FIRST sman WHERE sman.company = cust.company
            AND sman.sman = cust.sman NO-LOCK NO-ERROR.
        IF AVAIL sman THEN v-sales-rep = sman.sname.
   END.
    ASSIGN 
       cGroup        = itemfg.spare-char-1
       cItemLocation = itemfg.def-loc.
  IF v-custown = NO THEN
    FOR EACH fg-bin FIELDS(qty loc)
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.loc     GE begin_whse
        AND fg-bin.loc     LE end_whse
        AND fg-bin.cust-no EQ ""        
        NO-LOCK:
      ASSIGN
        v-qty-onh = v-qty-onh + fg-bin.qty
        v-whse-bin-found = YES
        cItemLoc         = fg-bin.loc.
    END.    
  ELSE
    FOR EACH fg-bin FIELDS(qty loc)
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.loc     GE begin_whse
        AND fg-bin.loc     LE end_whse
        NO-LOCK:
      ASSIGN
        v-qty-onh = v-qty-onh + fg-bin.qty
        v-whse-bin-found = YES
        cItemLoc         = fg-bin.loc.
    END.

    IF v-whse-bin-found = NO AND
       NOT(begin_whse EQ "" AND
          (end_whse EQ "zzzzz" OR END_whse EQ "ZZZZZ")) THEN
       NEXT.

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
            AND oe-rel.s-code   NE "T"
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

    ASSIGN v-reord-msf = ROUND(((v-reord-qty / 1000) * itemfg.t-sqft),0) .


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
         FOR EACH po-ordl WHERE 
                     po-ordl.company EQ itemfg.company
                 AND po-ordl.i-no EQ itemfg.i-no
                 AND lookup(po-ordl.stat, "o,p,u,a") > 0
                 AND po-ordl.opened EQ YES
                 AND po-ordl.due-date NE ?
                 /*AND po-ordl.item-type  = NO*/  /* task 07301405 */
                 /* AND po-ordl.po-no = int(v-po-no) */ /* task 09021409 */
                 NO-LOCK BY po-ordl.due-date DESC :
  
                 IF AVAIL po-ordl THEN ASSIGN v-po-due-dt = string(po-ordl.due-date) .
                 LEAVE.
           END.
       END.
           
       ASSIGN v-job-due-dt = "" .
           FOR EACH job-hdr 
             WHERE job-hdr.company = itemfg.company
               AND job-hdr.i-no EQ itemfg.i-no
               AND job-hdr.opened EQ YES 
               AND job-hdr.due-date NE ?
             NO-LOCK BY job-hdr.due-date DESC :

               IF AVAIL job-hdr THEN ASSIGN v-job-due-dt = string(job-hdr.due-date) .
               LEAVE.
           END.
       
        IF v-job-due-dt EQ ?  THEN ASSIGN v-job-due-dt = "" .
       IF v-po-due-dt EQ ?  THEN ASSIGN v-po-due-dt = "" .
       IF v-last-ship EQ ?  THEN ASSIGN v-last-ship = "" .

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
               IF li GT 12 THEN li = 12.

               IF fg-rcpth.rita-code EQ "S" THEN
                 li-hist[li] = li-hist[li] + fg-rdtlh.qty.
               ELSE
                 li-hist[li] = li-hist[li] - fg-rdtlh.qty.
           END.
           
           ASSIGN li-avg-hist = 0.
            DO j = 1 TO display_hist:
                li-avg-hist = li-avg-hist + li-hist[j] .
            END.
            li-avg-hist = li-avg-hist / display_hist .

            RUN GetOperationsForEst(INPUT itemfg.company, INPUT itemfg.est-no, OUTPUT cMachine).
       
    if v-reord-qty gt 0 or v-prt-all then
       IF tb_history THEN DO:
            IF lookup("EST STYLE",cSelectedList) GT 0 OR lookup("EST BOARD",cSelectedList) GT 0 OR lookup("EST LENGTH",cSelectedList) GT 0 OR
             lookup("EST WIDTH",cSelectedList) GT 0 OR lookup("EST DEPTH",cSelectedList) GT 0 OR lookup("EST PLATE#",cSelectedList) GT 0  THEN
            DO:
              RUN pGetEstValues(INPUT itemfg.company, INPUT itemfg.est-no, INPUT itemfg.i-no,
                                OUTPUT cEstStyle, OUTPUT cEstBoard, OUTPUT dEstLength,
                                OUTPUT dEstWidth, OUTPUT dEstDepth, OUTPUT cEstPlate).
            END.                  
            iCount = (itemfg.case-count * itemfg.case-pall )  + itemfg.quantityPartial . 
            IF iCount EQ ? THEN iCount = 0.
          
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
	                  WHEN "csr-name" THEN ASSIGN cVarValue = string(cCsrName,"x(30)")
                              cExcelVarValue = "".
                          WHEN "v-alloc-qty" THEN ASSIGN cVarValue = string(v-alloc-qty)
                              cExcelVarValue = "".
                          WHEN "v-qty-avail" THEN ASSIGN cVarValue = string(v-qty-avail)
                              cExcelVarValue = "".
                          WHEN "v-reord-qty" THEN ASSIGN cVarValue = string(v-reord-qty)
                              cExcelVarValue = "".
                          WHEN "msf-reord" THEN ASSIGN cVarValue = string(v-reord-msf)
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
			              WHEN "opt-qty" THEN ASSIGN cVarValue = STRING(v-alloc-qty - li-avg-hist,"->>,>>>,>>>,>>9") 
				              cExcelVarValue = "". 
                           WHEN "whse" THEN ASSIGN cVarValue = STRING(cItemLoc,"x(5)") 
                              cExcelVarValue = "".
                          WHEN "est-rout" THEN ASSIGN cVarValue = STRING(cMachine,"X(200)")
                              cExcelVarValue = "".
                          WHEN "est-style" THEN ASSIGN cVarValue = STRING(cEstStyle,"X(9)")
                              cExcelVarValue = "".
                          WHEN "est-board" THEN ASSIGN cVarValue = STRING(cEstBoard,"X(10)")
                                              cExcelVarValue = "". 
                          WHEN "est-length" THEN ASSIGN cVarValue = STRING(dEstLength,"->>,>>9.99")
                                              cExcelVarValue = "".                              
                          WHEN "est-wid" THEN ASSIGN cVarValue = STRING(dEstWidth,"->>,>>9.99")
                                              cExcelVarValue = "". 
                          WHEN "est-depth" THEN ASSIGN cVarValue = STRING(dEstDepth,"->>,>>9.99")
                                              cExcelVarValue = "". 
                          WHEN "est-plate" THEN ASSIGN cVarValue = STRING(cEstPlate,"X(15)")
                                              cExcelVarValue = "".  
                          WHEN "item-count" THEN ASSIGN cVarValue = STRING(iCount,"->,>>>,>>9")
                                              cExcelVarValue = "".     
                          WHEN "li-hist" THEN do: 
                              cVarValue = "" .
                              cExcelVarValue = "" .
                              DO j = 1 TO display_hist:
                                  cVarValue = cVarValue + STRING(li-hist[j],"->>>>>9") + " " .
                                 IF j EQ display_hist THEN
                                     cExcelVarValue = cExcelVarValue + quoter(STRING(li-hist[j],"->>>>>9"))  .
                                 ELSE cExcelVarValue = cExcelVarValue + quoter(STRING(li-hist[j],"->>>>>9")) + "," .
                              END.
                          END.
                          WHEN "item-location" THEN ASSIGN cVarValue = STRING(cItemLocation,"X(15)")
                                              cExcelVarValue = "".
                          WHEN "group" THEN ASSIGN cVarValue = STRING(cGroup,"X(15)")
                                              cExcelVarValue = "".
                          
                      END CASE.
                      IF cExcelVarValue = "" THEN cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      IF cTmpField EQ "li-hist" THEN
                          cExcelDisplay = cExcelDisplay + (cExcelVarValue) + ",".
                      ELSE
                          cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".
/*
                  IF cTmpfield = "li-hist" THEN DO:
                     ASSIGN cDisplay = cDisplay + STRING(li-hist[1]) + " "  + STRING(li-hist[2]) + " "  + STRING(li-hist[3]) + " "  +
                                       STRING(li-hist[41]) + " "  + STRING(li-hist[5]) + " "  + STRING(li-hist[6])
                            cExcelDisplay = cExcelDisplay +
                                           QUOTER(STRING(li-hist[1]) + " "  + STRING(li-hist[2]) + " "  + STRING(li-hist[3]) + " "  +
                                       STRING(li-hist[41]) + " "  + STRING(li-hist[5]) + " "  + STRING(li-hist[6]))
                            .
                  END.
                  ELSE DO:
                     {sys/inc/rptDisp2.i cTmpField}
                     cDisplay = cDisplay + cVarValue. 
                     cExcelDisplay = cExcelDisplay + quoter(cVarValue) + ",".
                  END.
*/                  
              END.
           END.
           PUT UNFORMATTED cDisplay SKIP.

           IF tb_dash THEN PUT FILL("-",300) FORMAT "x(300)" SKIP.
          
           IF tb_excel THEN 
             PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
       END.
       ELSE DO:   
       
            IF lookup("EST STYLE",cSelectedList) GT 0 OR lookup("EST BOARD",cSelectedList) GT 0 OR lookup("EST LENGTH",cSelectedList) GT 0 OR
             lookup("EST WIDTH",cSelectedList) GT 0 OR lookup("EST DEPTH",cSelectedList) GT 0 OR lookup("EST PLATE#",cSelectedList) GT 0  THEN
            DO:
            
                RUN pGetEstValues(INPUT itemfg.company, INPUT itemfg.est-no, INPUT itemfg.i-no,
                                  OUTPUT cEstStyle, OUTPUT cEstBoard, OUTPUT dEstLength,
                                  OUTPUT dEstWidth, OUTPUT dEstDepth, OUTPUT cEstPlate).
            END.                  
            iCount = (itemfg.case-count * itemfg.case-pall )  + itemfg.quantityPartial . 
            IF iCount EQ ? THEN iCount = 0.
          
            cDisplay = "".
            cTmpField = "".
            cExcelDisplay = "".
            cVarValue = "".
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
                     cExcelDisplay = cExcelDisplay + quoter(cTmpField) + ",".
    
                END.
                ELSE DO: 
                      /*{sys/inc/rptDisp2.i cTmpField "2"} */
                      /*RUN getVarValue (cTmpField, OUTPUT cVarValue).*/
                      CASE cTmpField:  
                          WHEN "csr-name" THEN ASSIGN cVarValue = string(cCsrName,"x(30)")
                              cExcelVarValue = "".                    
                          WHEN "v-qty-onh" THEN ASSIGN cVarValue = string(v-qty-onh)
                              cExcelVarValue = "".
                          WHEN "v-alloc-qty" THEN ASSIGN cVarValue = string(v-alloc-qty)
                              cExcelVarValue = "".
                          WHEN "v-qty-avail" THEN ASSIGN cVarValue = string(v-qty-avail)
                              cExcelVarValue = "".
                          WHEN "v-reord-qty" THEN ASSIGN cVarValue = string(v-reord-qty)
                              cExcelVarValue = "".
                          WHEN "msf-reord" THEN ASSIGN cVarValue = string(v-reord-msf)
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
			              WHEN "opt-qty" THEN ASSIGN cVarValue = STRING(v-alloc-qty - li-avg-hist,"->>,>>>,>>>,>>9")   
                              cExcelVarValue = "".
                           WHEN "whse" THEN ASSIGN cVarValue = STRING(cItemLoc,"x(5)") 
                              cExcelVarValue = "".
                          WHEN "est-rout" THEN ASSIGN cVarValue = STRING(cMachine,"X(200)")
                              cExcelVarValue = "".
                          WHEN "est-style" THEN ASSIGN cVarValue = STRING(cEstStyle,"X(9)")
                              cExcelVarValue = "".
                          WHEN "est-board" THEN ASSIGN cVarValue = STRING(cEstBoard,"X(10)")
                                              cExcelVarValue = "". 
                          WHEN "est-length" THEN ASSIGN cVarValue = STRING(dEstLength,"->>,>>9.99")
                                              cExcelVarValue = "".                              
                          WHEN "est-wid" THEN ASSIGN cVarValue = STRING(dEstWidth,"->>,>>9.99")
                                              cExcelVarValue = "". 
                          WHEN "est-depth" THEN ASSIGN cVarValue = STRING(dEstDepth,"->>,>>9.99")
                                              cExcelVarValue = "". 
                          WHEN "est-plate" THEN ASSIGN cVarValue = STRING(cEstPlate,"X(15)")
                                              cExcelVarValue = "".  
                          WHEN "item-count" THEN ASSIGN cVarValue = STRING(iCount,"->,>>>,>>9")
                                              cExcelVarValue = "".     
                          WHEN "li-hist" THEN do: 
                              cVarValue = "" .
                              cExcelVarValue = "" .
                              DO j = 1 TO display_hist:
                                  cVarValue = cVarValue + STRING(li-hist[j],"->>>>>9") + " " .
                                  
                                  cExcelVarValue = cExcelVarValue + quoter(STRING(li-hist[j],"->>>>>9")) + "," .
                              END.
                          END.
                          WHEN "item-location" THEN ASSIGN cVarValue = STRING(cItemLocation,"X(15)")
                                              cExcelVarValue = "".
                          WHEN "group" THEN ASSIGN cVarValue = STRING(cGroup,"X(15)")
                                              cExcelVarValue = "".
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

       IF tb_dash THEN PUT FILL("-",300) FORMAT "x(300)" SKIP.
    END.
end. /* each itemfg */
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftJobPros).
