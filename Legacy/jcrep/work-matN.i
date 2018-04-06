   find first item
       where item.company eq cocode
         and item.i-no    eq work-mat.i-no
       use-index i-no no-lock no-error.

   IF work-mat.prd-qty EQ ? THEN work-mat.prd-qty = 0.
   IF work-mat.act-qty EQ ? THEN work-mat.act-qty = 0.
   IF work-mat.est-qty EQ ? THEN work-mat.est-qty = 0.
       
   if work-mat.prd-qty ne 0 and
      work-mat.act-qty ne 0 and
      work-mat.est-qty ne 0 then do:
      
     if work-mat.act-qty-uom ne "EA" then
       run sys/ref/convquom.p("EA", work-mat.act-qty-uom, work-mat.basis-w,
                              work-mat.len, work-mat.wid, item.s-dep,
                              work-mat.prd-qty, output work-mat.prd-qty).

     IF NOT tb_jobsSheets THEN
         work-mat.est-cost = work-mat.est-cost / work-mat.est-qty *
                         work-mat.prd-qty.
   end.
   IF work-mat.est-cost EQ ? THEN work-mat.est-cost = 0.

   assign
    v-cst-var = work-mat.est-cost - work-mat.act-cost
    v-prod-p  = v-cst-var / work-mat.est-cost * 100.

   if v-prod-p eq ? then v-prod-p = 0.
   if v-prd-var eq ? then v-prd-var = 0.
   v-supress-est = NO.
   IF AVAIL ITEM AND ITEM.mat-TYPE = "D" THEN DO:
       IF work-mat.act-qty GT 0 THEN DO:
           FOR EACH bf-work-mat WHERE rowid(bf-work-mat) NE ROWID(work-mat)
               AND bf-work-mat.est-cost GT 0,
               FIRST bf-item WHERE bf-item.company eq cocode
                     AND bf-item.i-no    eq bf-work-mat.i-no
                     AND bf-item.mat-type = "D"
                   use-index i-no no-lock .
               LEAVE.
               
           END.
           IF AVAIL bf-work-mat THEN
               v-supress-est = TRUE.
       END.
   END.
   /*display work-mat.i-no FORMAT "x(15)"
           item.i-dscr when avail item format "x(18)"
           work-mat.est-qty            format ">>,>>>,>>9"
           work-mat.prd-qty when work-mat.prd-qty ne 0 AND NOT tb_jobsSheets
                                       @ work-mat.est-qty
           work-mat.est-qty-uom 
           (IF v-supress-est THEN 0 ELSE work-mat.est-cost) @ work-mat.est-cost format ">>,>>>,>>9"
           work-mat.act-qty            format ">,>>>,>>9"
           work-mat.act-qty-uom
           work-mat.act-cost           format ">>,>>>,>>9"
           v-cst-var                   format ">>,>>>,>>9-"
           v-prod-p                    format ">>>>9.9-"

       WITH FRAME {1} STREAM-IO WIDTH 200 NO-LABELS NO-BOX down.*/

   ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
         
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):   

                IF LOOKUP(ENTRY(i,cSelectedList), v-header-3) = 0    THEN NEXT .
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item-code"    THEN cVarValue = string(work-mat.i-no,"x(15)") .
                         WHEN "item-desc"    THEN cVarValue = string(item.i-dscr,"x(20)").
                         WHEN "item-est-qty" THEN cVarValue = STRING(work-mat.est-qty,">>,>>>,>>9").
                         WHEN "item-est-um"  THEN cVarValue = STRING(work-mat.est-qty-uom,"x(3)") .
                         WHEN "item-est-cost"  THEN cVarValue = IF v-supress-est THEN "0" ELSE  STRING(work-mat.est-cost,">>,>>>,>>9") .
                         WHEN "item-act-qty" THEN cVarValue = STRING(work-mat.act-qty,">,>>>,>>9") .
                         WHEN "item-act-um"  THEN cVarValue = STRING(work-mat.act-qty-uom,"x(3)") .
                         WHEN "item-act-cost"  THEN cVarValue = STRING(work-mat.act-cost,">>,>>>,>>9") .
                         WHEN "item-cost-var"  THEN cVarValue = string(v-cst-var,">>,>>>,>>9-") .
                         WHEN "item-cost-var-per"   THEN cVarValue = string(v-prod-p,">>>>>>9.9-").
                         
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel2 THEN DO:
                 PUT STREAM excel2 UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

IF work-mat.mat-type = "B" THEN
    ASSIGN 
    acl-brd = acl-brd + round(work-mat.act-cost,0) .

   IF NOT v-supress-est THEN
     assign
       v-t-est-cost = v-t-est-cost + work-mat.est-cost.
   ASSIGN
     v-t-act-cost = v-t-act-cost + work-mat.act-cost.

   if last(work-mat.i-no) then do:


       /* Farmout Finished Goods Section */
       IF v-incl-farmout THEN DO:

           FOR EACH job-farm WHERE job-farm.job EQ job.job
               NO-LOCK:
             IF NOT AVAIL itemfg THEN
                 FIND itemfg WHERE itemfg.company EQ job-farm.company
                   AND itemfg.i-no EQ job-farm.i-no 
                   NO-LOCK NO-ERROR.
            /* DOWN WITH FRAME {1}.
             display job-farm.i-no @ work-mat.i-no
                     itemfg.i-dscr when avail itemfg @ item.i-dscr 
                     job-farm.qty                    @ work-mat.est-qty
                     "EA"            @ work-mat.est-qty-uom
                     (IF v-supress-est THEN 0 ELSE job-farm.std-tot-cost) 
                                                @ work-mat.est-cost 


                     job-farm.qty-iss           @ work-mat.act-qty
                     "EA"                       @ work-mat.act-qty-uom
                     job-farm.act-tot-cost      @ work-mat.act-cost


                     round(job-farm.std-tot-cost - job-farm.act-tot-cost, 0) @ v-cst-var
                     ROUND(ROUND(job-farm.std-tot-cost - job-farm.act-tot-cost, 0) / job-farm.std-tot-cost * 100, 1) @ v-prod-p
                     /* v-prod-p                    format ">>>9.9-" */               
               WITH FRAME {1} STREAM-IO WIDTH 200 NO-LABELS NO-BOX down. 
             DOWN WITH FRAME {1}.*/

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
         
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):   

                IF LOOKUP(ENTRY(i,cSelectedList), v-header-3) = 0    THEN NEXT .
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item-code"    THEN cVarValue = string(job-farm.i-no,"x(15)") .
                         WHEN "item-desc"    THEN cVarValue = string(itemfg.i-dscr,"x(20)").
                         WHEN "item-est-qty" THEN cVarValue = STRING(job-farm.qty,">>,>>>,>>9").
                         WHEN "item-est-um"  THEN cVarValue = STRING("EA","x(3)") .
                         WHEN "item-est-cost"  THEN cVarValue = IF v-supress-est THEN "0" ELSE  STRING(job-farm.std-tot-cost,">>,>>>,>>9") .
                         WHEN "item-act-qty" THEN cVarValue = STRING(job-farm.qty-iss,">,>>>,>>9") .
                         WHEN "item-act-um"  THEN cVarValue = STRING("EA","x(3)") .
                         WHEN "item-act-cost"  THEN cVarValue = STRING(job-farm.act-tot-cost,">>,>>>,>>9") .
                         WHEN "item-cost-var"  THEN cVarValue = string(round(job-farm.std-tot-cost - job-farm.act-tot-cost, 0),">>,>>>,>>9-") .
                         WHEN "item-cost-var-per"   THEN cVarValue = string(ROUND(job-farm.std-tot-cost - job-farm.act-tot-cost, 0),">>>>>>9.9-").
                         
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel2 THEN DO:
                 PUT STREAM excel2 UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

             /* Material grand total should include farmout */
             v-t-act-cost = v-t-act-cost + job-farm.act-tot-cost.
             IF NOT v-supress-est THEN
                 ASSIGN v-t-est-cost = v-t-est-cost + job-farm.std-tot-cost.
           END. /* for each job-farm */

       END. /* if v-incl-farmout */



     assign
      v-cst-var = v-t-est-cost - v-t-act-cost.
      v-prod-p = (v-cst-var / v-t-est-cost) * 100.00.

     if v-prod-p eq ? then v-prod-p = 0.

    /* put "----------" at 51
         "---------  ---------  -------" at 77 skip
         "  TOTAL MATERIAL   (DIRECT) :"
         v-t-est-cost at 51 FORM ">>,>>>,>>9"
         v-t-act-cost at 76 FORM ">>,>>>,>>9"
         " " v-cst-var FORM ">>,>>>,>>9-" 
         v-prod-p skip.*/
      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
         
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):   

                IF LOOKUP(ENTRY(i,cSelectedList), v-header-3) = 0    THEN NEXT .
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "item-code"    THEN cVarValue = "" .
                         WHEN "item-desc"    THEN cVarValue = "".
                         WHEN "item-est-qty" THEN cVarValue = "".
                         WHEN "item-est-um"  THEN cVarValue = "" .
                         WHEN "item-est-cost"  THEN cVarValue = STRING(v-t-est-cost,">>,>>>,>>9") .
                         WHEN "item-act-qty" THEN cVarValue = "" .
                         WHEN "item-act-um"  THEN cVarValue = "" .
                         WHEN "item-act-cost"  THEN cVarValue = STRING(v-t-act-cost,">>,>>>,>>9") .
                         WHEN "item-cost-var"  THEN cVarValue = string(v-cst-var,">>,>>>,>>9-") .
                         WHEN "item-cost-var-per"   THEN cVarValue = string(v-prod-p,">>>>>>9.9-").
                         
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
            PUT item-str-line SKIP .
            PUT UNFORMATTED  "   TOTAL MATERIAL   (DIRECT) :"  + substring(cDisplay,31,150)  SKIP(1).

   end.
