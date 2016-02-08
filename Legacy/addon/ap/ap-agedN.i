/* --------------------------------------------------- ap/ap-aged.i 08/98 JLF */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

for each ap-inv
    where ap-inv.company   eq vend.company
      and ap-inv.vend-no   eq vend.vend-no
      and ap-inv.posted    eq yes
      and (ap-inv.inv-date le as_of_date or not v-idate)
    use-index ap-inv no-lock,
    
    first ap-ledger
    where ap-ledger.company  eq vend.company
      and ap-ledger.vend-no  eq ap-inv.vend-no
      and ap-ledger.ref-date eq ap-inv.inv-date
      and ap-ledger.refnum   eq ("INV# " + ap-inv.inv-no)
      and (ap-ledger.tr-date le as_of_date or v-idate)
    use-index ap-ledger no-lock
    
    break by ap-inv.vend-no
          by (if v-idate then ap-inv.inv-date else ap-ledger.tr-date)
          by ap-inv.inv-no:
  
  if FIRST(ap-inv.vend-no) then do:
    assign
     first-time = yes
     cust-t[1]  = 0
     cust-t[2]  = 0
     cust-t[3]  = 0
     cust-t[4]  = 0
     cust-t[5]  = 0
     m3 = vend.area-code + vend.phone
     m2 = vend.cont
     ni = 0.
  end.

  assign
   v-amt  = 0
   v-date = if v-idate then ap-inv.inv-date else ap-ledger.tr-date.
   
  for each ap-payl no-lock
      where ap-payl.inv-no   eq ap-inv.inv-no
        and ap-payl.vend-no  eq ap-inv.vend-no
        and ap-payl.posted   eq yes
        and ap-payl.due-date eq ap-inv.due-date
      use-index inv-no:

    find first ap-pay no-lock
        where ap-pay.company eq vend.company
          and ap-pay.c-no eq ap-payl.c-no
        use-index c-no no-error.

    if avail ap-pay THEN
    DO:
       v-check-date = ap-pay.check-date.

       /*check for voided check transaction date*/
      if ap-payl.amt-paid lt 0  and
         ap-payl.memo                eq no and
         ap-inv.net + ap-inv.freight gt 0 THEN
         DO:
            v-refnum = "VOIDED CHECK"
                        + string(ap-pay.check-no, "zzzzzzz9").

            FIND FIRST xap-ledger WHERE
                 xap-ledger.company EQ vend.company AND
                 xap-ledger.vend-no EQ ap-pay.vend-no AND
                 xap-ledger.refnum = v-refnum
                 NO-LOCK NO-ERROR.

            IF AVAIL xap-ledger THEN
            DO:
               v-check-date = xap-ledger.tr-date.
               RELEASE xap-ledger.
            END.
         END.
         
/*          IF v-idate THEN DO:                                                */
/*              FIND FIRST xap-ledger                                          */
/*                 WHERE xap-ledger.company  eq vend.company                   */
/*                   AND xap-ledger.vend-no  eq ap-pay.vend-no                 */
/*                   AND xap-ledger.ref-date EQ ap-pay.check-date              */
/*                   AND ((xap-ledger.refnum BEGINS "MEMO" AND ap-pay.memo) OR */
/*                     (NOT xap-ledger.refnum BEGINS "MEMO" AND                */
/*                      NOT xap-ledger.refnum BEGINS "INV#"                    */
/*                      AND NOT ap-pay.memo))                                  */
/*              NO-LOCK NO-ERROR.                                              */
/*              IF AVAIL xap-ledger THEN DO:                                   */
/*                  v-check-date = xap-ledger.tr-date.                         */
/*                  RELEASE xap-ledger.                                        */
/*              END.                                                           */
/*          END.                                                               */

       IF v-check-date le as_of_date then do:

       if ap-payl.amt-paid ne 0 then v-amt = v-amt - ap-payl.amt-paid.
       if ap-payl.amt-disc ne 0 then do:
          if not ap-payl.memo then v-amt = v-amt - ap-payl.amt-disc.
          if ap-payl.memo then v-amt = v-amt + ap-payl.amt-disc.
       end.
    end.
    END.

    release ap-pay.
    
  end. /* for each ap-payl */

  assign
   d     = as_of_date - v-date
   v-amt = v-amt + ap-inv.net + ap-inv.freight
   t1    = t1 + v-amt
   ni    = ni + 1.

  /* if days old less then 0 make equal to 0 */
  if d lt 0 then assign d = 0.
  
  if v-amt ne 0 then do:
   
      FIND FIRST terms WHERE terms.company = vend.company AND
          terms.t-code = vend.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN v-terms = terms.dscr.
      
    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "curr"    THEN cVarValue = string(tt-vend.curr-code,"x(8)") .
                         WHEN "vend"   THEN cVarValue = string(vend.vend-no,"x(10)").
                         WHEN "vend-name"   THEN cVarValue = STRING(vend.name,"x(30)").
                         WHEN "phone"  THEN cVarValue = STRING(m3,"(999) 999-9999")  .
                         WHEN "type"   THEN cVarValue = STRING(vend.TYPE) .
                         WHEN "term"  THEN cVarValue = STRING(v-terms) .
                         WHEN "inv"   THEN cVarValue = STRING(ap-inv.inv-no) .
                         WHEN "date"  THEN cVarValue = STRING(v-date,"99/99/99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amt,"->,>>>,>>>,>>9.99") .
                         WHEN "day"  THEN cVarValue = STRING(d,">>>>>>") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
   
    assign
     ag        = 0
     i         = (IF d LE v-days[1] THEN 1 
                  ELSE IF d LE v-days[2] THEN 2
                  ELSE IF d LE v-days[3] THEN 3
                  ELSE IF d LE v-days[4] THEN 4
                  ELSE 5)
     ag[i]     = v-amt
     cust-t[i] = cust-t[i] + ag[i].
   
    if v-dtl then
    DO:
      /*display ap-inv.inv-no     at 3        format "x(12)"
              space(2)
              v-date
              space(3)
              v-amt
              d                             format "-999"
              ag[1]             to 67       when ag[1] ne 0
              ag[2]             to 83       when ag[2] ne 0
              ag[3]             to 99       when ag[3] ne 0
              ag[4]             to 115      when ag[4] ne 0 
              ag[5]             to 131      when ag[5] ne 0
              
          with frame detail{1} no-labels no-box stream-io width 132.*/

          cDisplay =  cDisplay + string(ag[1],"->,>>>,>>>,>>9.99") + " " + string(ag[2],"->,>>>,>>>,>>9.99") + 
                     " " + string(ag[3],"->,>>>,>>>,>>9.99") + " " + string(ag[4],"->,>>>,>>>,>>9.99") + " " +
                     string(ag[5],"->,>>>,>>>,>>9.99") .

           cExcelDisplay = cExcelDisplay +  string(ag[1]) + "," + string(ag[2]) + "," +
                            string(ag[3]) + "," + string(ag[4]) + "," +
                            string(ag[5]) .

          PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
    END.  /* end of detail */

    ELSE DO:
         if first-time then do:
      FIND FIRST terms WHERE terms.company = vend.company AND
          terms.t-code = vend.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN v-terms = terms.dscr.
     
      PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

      first-time = no.

     
    end.  /* end of first-time */

    END.

  end.  /* if v-amt ne 0  */

  if LAST(ap-inv.vend-no) and t1 ne 0 then do:
    if ni gt 1 then m3 = "".
    if ni eq 1 then m3 = m2.
    PUT str-line SKIP .
    
    /*display space (10) "VENDOR TOTALS" t1 to 44
            cust-t[1] to 67
            cust-t[2] to 83
            cust-t[3] to 99
            cust-t[4] to 115
            cust-t[5] to 131
            skip(1)
            
        with frame vendor3{1} no-labels no-box no-attr-space stream-io width 132.*/

     ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "curr"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "vend-name"   THEN cVarValue = "".
                         WHEN "phone"  THEN cVarValue = "" .
                         WHEN "type"   THEN cVarValue = "" .
                         WHEN "term"  THEN cVarValue = "" .
                         WHEN "inv"   THEN cVarValue = "" .
                         WHEN "date"  THEN cVarValue = "" .
                         WHEN "amt"   THEN cVarValue = STRING(t1,"->,>>>,>>>,>>9.99") .
                         WHEN "day"  THEN cVarValue = "" .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            cDisplay =  cDisplay + string(cust-t[1],"->,>>>,>>>,>>9.99") + " " + string(cust-t[2],"->,>>>,>>>,>>9.99") + 
                     " " + string(cust-t[3],"->,>>>,>>>,>>9.99") + " " + string(cust-t[4],"->,>>>,>>>,>>9.99") + " " +
                     string(cust-t[5],"->,>>>,>>>,>>9.99") .

            cExcelDisplay = cExcelDisplay +  string(cust-t[1]) + "," + string(cust-t[2]) + "," +
                            string(cust-t[3]) + "," + string(cust-t[4]) + "," +
                            string(cust-t[5]) .


          PUT UNFORMATTED "    Total Dollars" substring(cDisplay,18,300) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'Total Dollars ,' + SUBSTRING(cExcelDisplay,4,300) SKIP.
             END.
        
    
    do i = 1 to 5:
       curr-t[i] = curr-t[i] + cust-t[i].
    end.
    assign
     cust-t = 0
     t2     = t2 + t1
     t1     = 0.
  end.  /* last-of loop */
end.  /* for each ap-inv */

/* end ---------------------------------- copr. 1992  advanced software, inc. */
