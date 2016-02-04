/* jcrep/r-jcostN1.i*/

 BUFFER bwork-item:FIND-BY-ROWID(ROWID(work-item), NO-LOCK) .
 ASSIGN cDisplay = ""
        cExcelDisplay = "".

 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField =  BUFFER bwork-item:BUFFER-FIELD(cTmpField).
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
                 WHEN "v-job-no" THEN cVarValue = string(job.job-no + "-" + string(job.job-no2,"99")).
                 WHEN "v-i-name" THEN cVarValue = IF AVAIL itemfg THEN substring(itemfg.i-name,1,25) ELSE "".
                 WHEN "v-fgcat" THEN cVarValue = IF AVAIL itemfg THEN itemfg.procat ELSE "".
                 WHEN "v-custname" THEN cVarValue = STRING(cust.name).
                 WHEN "v-act-mAT-cost" THEN cVarValue = STRING(v-act-mAT-cost,"->>>>>,>>9.99").
                 WHEN "v-est-mAT-cost" THEN cVarValue = STRING(v-est-mAT-cost,"->>>>>,>>9.99").
                 WHEN "v-var-mat-cost" THEN cVarValue = STRING(v-est-mAT-cost - v-act-mAT-cost,"->>>>>,>>9.99").
                 WHEN "v-var%-mat-cost" THEN cVarValue = 
                     STRING( IF v-act-mat-cost <> 0 THEN (((v-est-mAT-cost - v-act-mAT-cost) / v-act-mAT-cost)  * 100) ELSE 0 ,"->>>>>,>>9.99").
                 WHEN "v-act-lab-cost" THEN cVarValue = STRING(v-act-lab-cost,"->>>>>,>>9.99").
                 WHEN "v-est-lab-cost" THEN cVarValue = STRING(v-est-lab-cost,"->>>>>,>>9.99").
                 WHEN "v-var-lab-cost" THEN cVarValue = STRING( v-est-lab-cost - v-act-lab-cost,"->>>>>,>>9.99").
                 WHEN "v-var%-lab-cost" THEN cVarValue = 
                     STRING( IF v-act-lab-cost <> 0 THEN (((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost) * 100) ELSE 0,"->>>>>,>>9.99"). 
                 WHEN "v-act-foh-cost" THEN cVarValue = STRING(v-act-foh-cost,"->>>>>,>>9.99").
                 WHEN "v-est-foh-cost" THEN cVarValue = STRING(v-est-foh-cost,"->>>>>,>>9.99").
                 WHEN "v-var-foh-cost" THEN cVarValue = STRING( v-est-foh-cost - v-act-foh-cost,"->>>>>,>>9.99").
                 WHEN "v-var%-foh-cost" THEN cVarValue = 
                     STRING( IF v-act-foh-cost <> 0 THEN (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost) * 100) ELSE 0,"->>>>>,>>9.99"). 
                 WHEN "v-act-voh-cost" THEN cVarValue = STRING(v-act-voh-cost,"->>>>>,>>9.99").
                 WHEN "v-est-voh-cost" THEN cVarValue = STRING(v-est-voh-cost,"->>>>>,>>9.99").
                 WHEN "v-var-voh-cost" THEN cVarValue = STRING( v-est-voh-cost - v-act-foh-cost,"->>>>>,>>9.99").
                 WHEN "v-var%-voh-cost" THEN cVarValue = 
                     STRING( IF v-act-voh-cost <> 0 THEN (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost) * 100) ELSE 0,"->>>>>,>>9.99"). 
                 WHEN "v-act-tot-cost" THEN cVarValue = STRING(v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost,"->>>>>,>>9.99").
                 WHEN "v-est-tot-cost" THEN cVarValue = STRING(v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost,"->>>>>,>>9.99").
                 WHEN "v-var-tot-cost" THEN cVarValue = STRING( (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost),"->>>>>,>>9.99").
                 WHEN "v-var%-tot-cost" THEN cVarValue = 
                   STRING( IF (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost) <> 0 THEN (((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) -
                                                              (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost) / (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) * 100) ELSE 0,"->>>>>,>>9.99"). 
                 WHEN "v-mat-usage" THEN cVarValue = STRING((v-est-mAT-cost - v-act-mAT-cost),"->>>>>,>>9.99").
                 WHEN "v-lab-eff" THEN cVarValue = STRING((v-est-lab-cost - v-act-lab-cost),"->>>>>,>>9.99").
                 WHEN "v-foh-eff" THEN cVarValue = STRING((v-est-foh-cost - v-act-foh-cost),"->>>>>,>>9.99").
                 WHEN "v-voh-eff" THEN cVarValue = STRING((v-est-voh-cost - v-act-voh-cost),"->>>>>,>>9.99").
                 WHEN "v-std-price" THEN cVarValue = STRING(v-std-price,"->>>>>,>>9.99").
                 WHEN "v-act-price" THEN cVarValue = STRING(v-act-price,"->>>>>,>>9.99").
                 WHEN "v-std-cost" THEN cVarValue = STRING((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost),"->>>>>,>>9.99").
                 WHEN "v-act-cost" THEN cVarValue = STRING((v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost),"->>>>>,>>9.99").

                 WHEN "v-std-cont" THEN cVarValue = STRING((v-std-price - (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost)),"->>>>>,>>9.99").
                 WHEN "v-act-cont" THEN cVarValue = STRING((v-act-price - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),"->>>>>,>>9.99").
                 WHEN "v-std-cont%" THEN cVarValue = 
                     STRING((IF v-std-price NE 0 THEN ((v-std-price - (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost)) /
            v-std-price) * 100.00 ELSE 0),"->>>>>,>>9.99").
                 WHEN "v-act-cont%" THEN cVarValue = STRING((IF v-act-price NE 0 THEN ((v-act-price - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) /
            v-act-price) * 100.00 ELSE 0),"->>>>>,>>9.99").
                 WHEN "v-tot-cont" THEN cVarValue = STRING(((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),"->>>>>,>>9.99").
                 WHEN "v-qty-ord" THEN cVarValue = STRING(work-item.qty-ord,"->>>>,>>>,>>9").
                  WHEN "v-qty-prod" THEN cVarValue = STRING(work-item.qty-prod,"->>>>,>>>,>>9").
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


