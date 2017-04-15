/* jcrep/r-jcostN1.i*/

ASSIGN  v-prep-cost = 0 
        v-labor     = 0
        v-com       = 0
        v-comm      = 0
        v-t-inv-qty = 0 
        v-frate     = 0
        v-mater = v-act-mat-cost  .

/* inv Qty */
 v-t-inv-qty = getInvoiceTotal(job-hdr.ord-no, job-hdr.job-no, job-hdr.job-no2, "Qty").

FOR EACH misc-act
    WHERE misc-act.company eq cocode
      AND misc-act.job     eq job.job
    NO-LOCK:
    ASSIGN
  v-prep-cost = v-prep-cost + misc-act.cost.
END.

FOR EACH job-farm WHERE job-farm.job EQ job.job
               NO-LOCK:
     v-mater = v-mater + job-farm.act-tot-cost.
END.

/*Laber cost */
  v-labor = v-act-lab-cost + v-act-foh-cost + v-act-voh-cost .
 
/* comm */
  v-sale = /*(work-item.qty-ord / 1000) **/  v-act-price .
   
  FIND FIRST eb NO-LOCK
      WHERE eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        AND eb.form-no  NE 0
      NO-ERROR.
  IF AVAIL eb THEN v-com = eb.comm.
  v-comm  = v-sale * (v-com / 100).
  

/* Frate */
   iOrder = 0.
   iOrder = INTEGER(job.job-no) NO-ERROR.
   FIND FIRST oe-boll NO-LOCK WHERE  oe-boll.company EQ job.company
        AND oe-boll.ord-no EQ iOrder
        AND oe-boll.i-no   EQ job-hdr.i-no
        NO-ERROR.
   IF AVAIL oe-boll OR iOrder EQ 0 THEN DO:
       /* get freight from boll's */

       FOR EACH oe-boll WHERE oe-boll.company EQ job.company
                           AND oe-boll.ord-no EQ iOrder
                           AND oe-boll.i-no   EQ job-hdr.i-no
            NO-LOCK,
            FIRST oe-bolh 
                    WHERE oe-bolh.company = oe-boll.company 
                      AND oe-bolh.b-no = oe-boll.b-no                      
                    NO-LOCK.
    
                v-frate = v-frate + oe-boll.freight.
       END.
   END.
   ELSE DO:
       /* Original Code */
       v-wt = itemfg.weight-100 * work-item.qty-prod / 100.
    
       IF eb.fr-out-c ne 0 THEN
         v-frate = v-frate + (eb.fr-out-c * (v-wt / 100)).
    
       ELSE
       IF eb.fr-out-m ne 0 THEN
         v-frate = v-frate + (eb.fr-out-m * (work-item.qty-prod / 1000)).
    
       ELSE DO:
         v-rate = 0.
    
       RELEASE carr-mtx.
    
       FIND FIRST carrier
           WHERE carrier.company EQ cocode
           AND carrier.loc     EQ locode
           AND carrier.carrier EQ eb.carrier
           NO-LOCK NO-ERROR.
            
       IF AVAIL carrier THEN
       FIND FIRST carr-mtx NO-LOCK
           WHERE carr-mtx.company  EQ cocode
           AND carr-mtx.loc      EQ locode
           AND carr-mtx.carrier  EQ carrier.carrier
           AND carr-mtx.del-zone EQ eb.dest-code
           NO-ERROR.

       IF AVAIL carr-mtx THEN DO:
           IF carrier.by-pallet NE ? THEN DO:
               RUN util/ucarrier.p (RECID(carrier)).
               FIND CURRENT carrier NO-LOCK NO-ERROR.
           END.
    
           IF carrier.chg-method eq "P" THEN DO:
               v-wt = work-item.qty-prod / (eb.cas-cnt * eb.cas-pal).
               {sys/inc/roundup.i v-wt}
           END.
            
           ELSE
           IF carrier.chg-method EQ "M" THEN
             v-wt = (IF v-corr THEN (work-item.qty-prod * itemfg.t-sqin * .007)
                               ELSE (work-item.qty-prod * itemfg.t-sqin / 144)) /
                    1000.
            
           DO i = 1 to 10:
               IF carr-mtx.weight[i] GE v-wt THEN DO:
                   v-rate = carr-mtx.rate[i].
                   LEAVE.
               END.
           END.
         
           v-rate = v-rate * v-wt / (IF carrier.chg-method EQ "W" THEN 100 ELSE 1).
            
           IF v-rate LT carr-mtx.min-rate THEN v-rate = carr-mtx.min-rate.
         END. /* avail carr-mtx */
         IF v-rate NE ? THEN v-frate = v-frate + v-rate.
      END. /* eb.fr-out is zero */
    END. /* not avail an oe-boll */

  

 v-total = v-mater + v-prep + v-labor + v-lab-m + v-comm + v-frate .

 BUFFER bwork-item:FIND-BY-ROWID(ROWID(work-item), NO-LOCK) .
 ASSIGN cDisplay = ""
        cExcelDisplay = "".

 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField =  BUFFER bwork-item:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     cDisplay = cDisplay + 
                               IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  INTEGER(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                 END.
                 ELSE DO:
                    cTmpField = SUBSTRING(cFieldName,1,INTEGER( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + QUOTER(" ") + ",".
                 END.
            END.
            ELSE DO: 
              cVarValue = "".
              CASE cTmpField:               
                 WHEN "v-job-no" THEN cVarValue = STRING(job.job-no + "-" + STRING(job.job-no2,"99")).
                 WHEN "v-i-name" THEN cVarValue = IF AVAIL itemfg THEN SUBSTRING(itemfg.i-name,1,25) ELSE "".
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
                     
                 WHEN "qty-inv" THEN cVarValue = STRING(v-t-inv-qty,"->>>>,>>>,>>9").
                 WHEN "box-sales" THEN cVarValue = STRING( /*((work-item.qty-ord / 1000) **/ v-act-price /*work-item.price*/ /*)*/ ,"->>>>,>>>,>>9").
                 WHEN "prep-cost" THEN cVarValue = STRING(v-prep-cost ,"->,>>>,>>9.99").
                 WHEN "total-cost" THEN cVarValue = STRING(v-total,"->,>>>,>>9.99").
                 WHEN "lab-act-cost" THEN cVarValue = STRING(v-labor,"->,>>>,>>>,>>>,>>9.99").
                 WHEN "mat-fram-act-cost" THEN cVarValue = STRING(v-mater,"->>>,>>>,>>>,>>9.99").

                     
              END CASE.

             cExcelVarValue = cVarValue.  
             cDisplay = cDisplay + cVarValue +
                       FILL(" ",INTEGER(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
             cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
            END.
 END.
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
