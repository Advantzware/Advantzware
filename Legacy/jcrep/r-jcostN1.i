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

for each misc-act
    where misc-act.company eq cocode
      and misc-act.job     eq job.job
    no-lock:
    assign
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
   
  find first eb
      where eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        and eb.form-no  ne 0
      no-lock no-error.
  if avail eb then v-com = eb.comm.
  v-comm  = v-sale * (v-com / 100).
  

/* Frate */
   iOrder = 0.
   iOrder = INTEGER(job.job-no) NO-ERROR.
   FIND FIRST oe-boll WHERE oe-boll.company EQ job.company
        AND oe-boll.ord-no EQ iOrder
        AND oe-boll.i-no   EQ job-hdr.i-no
        NO-LOCK NO-ERROR.
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
    
       if eb.fr-out-c ne 0 then
         v-frate = v-frate + (eb.fr-out-c * (v-wt / 100)).
    
       else
       if eb.fr-out-m ne 0 then
         v-frate = v-frate + (eb.fr-out-m * (work-item.qty-prod / 1000)).
    
       else do:
         v-rate = 0.
    
       release carr-mtx.
    
       find first carrier
           where carrier.company eq cocode
           and carrier.loc     eq locode
           and carrier.carrier eq eb.carrier
           no-lock no-error.
            
       if avail carrier then
       find first carr-mtx
           where carr-mtx.company  eq cocode
           and carr-mtx.loc      eq locode
           and carr-mtx.carrier  eq carrier.carrier
           and carr-mtx.del-zone eq eb.dest-code
           no-lock no-error.

       if avail carr-mtx then do:
           if carrier.by-pallet ne ? THEN DO:
               run util/ucarrier.p (recid(carrier)).
               FIND CURRENT carrier NO-LOCK NO-ERROR.
           END.
    
           if carrier.chg-method eq "P" then do:
               v-wt = work-item.qty-prod / (eb.cas-cnt * eb.cas-pal).
               {sys/inc/roundup.i v-wt}
           end.
            
           else
           if carrier.chg-method eq "M" then
             v-wt = (if v-corr then (work-item.qty-prod * itemfg.t-sqin * .007)
                               else (work-item.qty-prod * itemfg.t-sqin / 144)) /
                    1000.
            
           do i = 1 to 10:
               if carr-mtx.weight[i] ge v-wt then do:
                   v-rate = carr-mtx.rate[i].
                   leave.
               end.
           end.
         
           v-rate = v-rate * v-wt / (if carrier.chg-method eq "W" then 100 else 1).
            
           if v-rate lt carr-mtx.min-rate then v-rate = carr-mtx.min-rate.
         end. /* avail carr-mtx */
         IF v-rate NE ? THEN v-frate = v-frate + v-rate.
      end. /* eb.fr-out is zero */
    END. /* not avail an oe-boll */

  

 v-total = v-mater + v-prep + v-labor + v-lab-m + v-comm + v-frate .

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
                     
                 WHEN "qty-inv" THEN cVarValue = STRING(v-t-inv-qty,"->>>>,>>>,>>9").
                 WHEN "box-sales" THEN cVarValue = STRING( /*((work-item.qty-ord / 1000) **/ v-act-price /*work-item.price*/ /*)*/ ,"->>>>,>>>,>>9").
                 WHEN "prep-cost" THEN cVarValue = STRING(v-prep-cost ,"->,>>>,>>9.99").
                 WHEN "total-cost" THEN cVarValue = STRING(v-total,"->,>>>,>>9.99").
                 WHEN "lab-act-cost" THEN cVarValue = STRING(v-labor,"->,>>>,>>>,>>>,>>9.99").
                 WHEN "mat-fram-act-cost" THEN cVarValue = STRING(v-mater,"->>>,>>>,>>>,>>9.99").

                     
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


