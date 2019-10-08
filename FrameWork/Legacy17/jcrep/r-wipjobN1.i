/* jcrep/r-wipjobN.i*/



              
       /* formular  
       MSF           (Formula =  ((Sheets x (Sht W x Sht L / 144))   / 1,000 
       Tons          (Formula=  (MSF x Basic Wt)  /  2000 Lbs
       MLF           (1000's of LF =  (Sheets x (Sht L / 12")   / 1000)
       Cost/Ton     =   Total Value / Tons
       Total Value  =  Board Cost from Bin Tab times Sheets 
       */
       
       IF ITEM.cons-uom NE "LF" THEN
             RUN sys/ref/convquom.p(item.cons-uom, "LF", item.basis-w,
                                   (if item.r-wid eq 0 THEN item.s-len
                                                       ELSE 12),
                                   (if item.r-wid eq 0 THEN item.s-wid
                                                       ELSE item.r-wid),
                                   item.s-dep,                    
                                   tt-job-mch.total-qty, OUTPUT v-lf-qty).
          ELSE
             v-lf-qty = tt-job-mch.total-qty /*job-mat.qty*/.
       /*
       IF job-mat.sc-uom EQ "EA" THEN vCost = job-mat.std-cost.
       ELSE
        RUN sys/ref/convcuom.p(job-mat.sc-uom,
                               "EA",
                               job-mat.basis-w,
                               job-mat.len,
                               job-mat.wid,
                               item.s-dep,
                               job-mat.std-cost,
                               OUTPUT vCost).
       */
       
        FIND FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
            AND tt-mat.job-no2 = job-mat.job-no2
            AND tt-mat.form-no = job-mat.frm
            AND tt-mat.blank-no = job-mat.blank-no
            /*AND tt-mat.rm-i-no = job-mat.rm-i-no*/
            AND tt-mat.cst-act > 0
            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-mat THEN
           FIND FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
            AND tt-mat.job-no2 = job-mat.job-no2
            AND tt-mat.form-no = job-mat.frm
            AND tt-mat.blank-no = job-mat.blank-no
            AND tt-mat.rm-i-no = job-mat.rm-i-no
            /*AND tt-mat.cst-act > 0*/
            NO-LOCK NO-ERROR.
        ASSIGN v-sheets = /*v-sheets +*/ v-lf-qty /*job-mat.qty*/  /* mch-act.qty ?*/
              vCost = (IF tt-mat.cst-act <> 0 THEN tt-mat.cst-act else tt-mat.cst-std) / v-sheets.

       IF vCost = ? THEN vCost = 0.

       /*vCost = IF  tt-job-mch.cst-act <> 0 THEN tt-job-mch.cst-act ELSE  tt-job-mch.cst-std. */
       ASSIGN v-wgt = job-mat.basis-w
              v-shtWid = /*job-mat.wid*/ tt-mat.wid
              v-shtLen = /*job-mat.len*/ tt-mat.len
              v-total = /*v-total +*/ v-lf-qty * vCost  /*job-mat.std-cost */ /* rm-bin.cost ? */
              v-sheetTotal = v-sheetTotal + v-sheets
              v-totalTot = v-totalTot + v-total
              .
    /*
   RUN sys/ref/convquom.p("LF", "TON", item.basis-w,
                                   (if item.r-wid eq 0 THEN item.s-len
                                                       ELSE 12),
                                   (if item.r-wid eq 0 THEN item.s-wid
                                                       ELSE item.r-wid),
                                   item.s-dep,                    
                                   v-total, OUTPUT v-totalTon).
   */
   v-totalTon = v-total.
   ASSIGN v-msf = v-sheets * (v-shtwid * v-shtlen / 144) / 1000
          v-ton = v-msf * v-wgt / 2000 /* Lb */
          v-mlf = v-sheets * (v-shtLen / 12 ) / 1000
          v-costTon = v-totalTon / v-ton
          .
   IF v-costTon = ? THEN v-costTon = 0.
   BUFFER bf-item:FIND-BY-ROWID(ROWID(item), NO-LOCK) .
   ASSIGN cDisplay = ""
          cExcelDisplay = "".
   
   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField =  BUFFER bf-item:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     cDisplay = cDisplay + 
                               IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                 END.
            END.
            ELSE DO: 
              cVarValue = "".
              CASE cTmpField:               
                 WHEN "v-job-no" THEN cVarValue = string(tt-job-mch.job-no + "-" + string(tt-job-mch.job-no2,"99")).
                 WHEN "v-vendor" THEN cVarValue = STRING(v-vendor,"X(20)").
                 WHEN "v-machine" THEN cVarValue = STRING(tt-job-mch.m-code).
                 WHEN "v-cust" THEN cVarValue = STRING(tt-job-mch.cust-no).
                 WHEN "v-die" THEN cVarValue = STRING(tt-job-mch.die).
                 WHEN "v-style" THEN cVarValue = STRING(tt-job-mch.style).
                 WHEN "v-wgt" THEN cVarValue = STRING(v-wgt,"->>,>>9.99").
                 WHEN "v-shtWid" THEN cVarValue = STRING(v-shtWid,"->>,>>9.99<<"). /* STRING(v-totalTon,"->>,>>9.99<<").*/
                 WHEN "v-shtLen" THEN cVarValue = STRING(v-shtLen,"->>,>>9.99<<"). /* STRING(vCost,"->>,>>9.99<<").  */
                 WHEN "v-sheets" THEN cVarValue =  STRING(v-sheets,"->>>,>>>,>>9").
                 WHEN "v-msf" THEN cVarValue =     STRING(v-msf,"->>>,>>>,>>9.99").
                 WHEN "v-ton" THEN cVarValue =     STRING(v-ton,"->>>,>>>,>>9.99").
                 WHEN "v-mlf" THEN cVarValue =     STRING(v-mlf,"->>>,>>>,>>9.99").
                 WHEN "v-costTon" THEN cVarValue = STRING(v-costTon,"->>>,>>>,>>9.99").
                 WHEN "v-total" THEN cVarValue = STRING(v-total,"->>>,>>>,>>9.99").
                 WHEN "v-total-mach-hrs" THEN cVarValue = STRING(v-total-mach-hrs,"->>>,>>>,>>9.99").
                 WHEN "v-rm-item" THEN cVarValue = STRING(tt-mat.rm-i-no).  /* item.i-no*/
              END CASE.

              cExcelVarValue = cVarValue.  
              cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
   END.
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.


