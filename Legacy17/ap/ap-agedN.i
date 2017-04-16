/* --------------------------------------------------- ap/ap-aged.i 08/98 JLF */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

FOR EACH ap-inv NO-LOCK
    WHERE ap-inv.company   EQ vend.company
      AND ap-inv.vend-no   EQ vend.vend-no
      AND ap-inv.posted    EQ YES 
      AND (ap-inv.inv-date LE as_of_date OR NOT v-idate)
    USE-INDEX ap-inv ,
    
    FIRST ap-ledger NO-LOCK 
    WHERE ap-ledger.company  EQ vend.company
      AND ap-ledger.vend-no  EQ ap-inv.vend-no
      AND ap-ledger.ref-date EQ ap-inv.inv-date
      AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
      AND (ap-ledger.tr-date LE as_of_date OR v-idate)
    USE-INDEX ap-ledger
    
    BREAK BY ap-inv.vend-no
          BY (IF v-idate THEN ap-inv.inv-date ELSE ap-ledger.tr-date)
          BY ap-inv.inv-no:
  
  IF FIRST(ap-inv.vend-no) THEN DO:
    ASSIGN 
     first-time = YES 
     cust-t[1]  = 0
     cust-t[2]  = 0
     cust-t[3]  = 0
     cust-t[4]  = 0
     cust-t[5]  = 0
     m3 = vend.area-code + vend.phone
     m2 = vend.cont
     ni = 0.
  END.

  ASSIGN 
   v-amt  = 0
   v-date = IF v-idate THEN ap-inv.inv-date ELSE ap-ledger.tr-date.
   
  FOR EACH ap-payl NO-LOCK
      WHERE ap-payl.inv-no   EQ ap-inv.inv-no
        AND ap-payl.vend-no  EQ ap-inv.vend-no
        AND ap-payl.posted   EQ YES 
        AND ap-payl.due-date EQ ap-inv.due-date
      USE-INDEX inv-no:

    FIND FIRST ap-pay NO-LOCK
        WHERE ap-pay.company EQ vend.company
          AND ap-pay.c-no EQ ap-payl.c-no
        USE-INDEX c-no NO-ERROR.

    IF AVAILABLE ap-pay THEN
    DO:
       v-check-date = ap-pay.check-date.

       /*check for voided check transaction date*/
      IF ap-payl.amt-paid LT 0  AND
         ap-payl.memo                EQ NO AND
         ap-inv.net + ap-inv.freight GT 0 THEN
         DO:
            v-refnum = "VOIDED CHECK"
                        + STRING(ap-pay.check-no, "zzzzzzz9").

            FIND FIRST xap-ledger NO-LOCK WHERE
                 xap-ledger.company EQ vend.company AND
                 xap-ledger.vend-no EQ ap-pay.vend-no AND
                 xap-ledger.refnum  EQ v-refnum
                 NO-ERROR.

            IF AVAILABLE xap-ledger THEN
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

       IF v-check-date LE as_of_date THEN DO:

       IF ap-payl.amt-paid NE 0 THEN v-amt = v-amt - ap-payl.amt-paid.
       IF ap-payl.amt-disc NE 0 THEN DO:
          IF NOT ap-payl.memo THEN v-amt = v-amt - ap-payl.amt-disc.
          IF ap-payl.memo THEN v-amt = v-amt + ap-payl.amt-disc.
       END.
    END.
    END.

    RELEASE ap-pay.
    
  END. /* for each ap-payl */

  ASSIGN
   d     = as_of_date - v-date
   v-amt = v-amt + ap-inv.net + ap-inv.freight
   t1    = t1 + v-amt
   ni    = ni + 1.

  /* IF days old less then 0 make equal to 0 */
  IF d LT 0 THEN ASSIGN d = 0.
  
  IF v-amt NE 0 THEN DO:
   
      FIND FIRST terms NO-LOCK WHERE terms.company EQ vend.company AND
          terms.t-code EQ vend.terms NO-ERROR.
      IF AVAILABLE terms THEN v-terms = terms.dscr.
      
    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "curr"      THEN cVarValue = STRING(tt-vend.curr-code,"x(8)") .
                         WHEN "vend"      THEN cVarValue = STRING(vend.vend-no,"x(10)").
                         WHEN "vend-name" THEN cVarValue = STRING(vend.NAME,"x(30)").
                         WHEN "phone"     THEN cVarValue = STRING(m3,"(999) 999-9999")  .
                         WHEN "type"      THEN cVarValue = STRING(vend.TYPE) .
                         WHEN "term"      THEN cVarValue = STRING(v-terms,"x(17)") .
                         WHEN "inv"       THEN cVarValue = STRING(ap-inv.inv-no) .
                         WHEN "date"      THEN cVarValue = STRING(v-date,"99/99/99") .
                         WHEN "due-date"  THEN cVarValue = STRING(ap-inv.due-date,"99/99/99") .
                         WHEN "amt"       THEN cVarValue = STRING(v-amt,"->,>>>,>>>,>>9.99") .
                         WHEN "day"       THEN cVarValue = STRING(d,">>>>>>") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
            END.
   
    ASSIGN 
     ag        = 0
     i         = (IF d LE v-days[1] THEN 1 
                  ELSE IF d LE v-days[2] THEN 2
                  ELSE IF d LE v-days[3] THEN 3
                  ELSE IF d LE v-days[4] THEN 4
                  ELSE 5)
     ag[i]     = v-amt
     cust-t[i] = cust-t[i] + ag[i].
   
    IF v-dtl THEN 
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

          cDisplay =  cDisplay + STRING(ag[1],"->,>>>,>>>,>>9.99") + " " + STRING(ag[2],"->,>>>,>>>,>>9.99") + 
                     " " + STRING(ag[3],"->,>>>,>>>,>>9.99") + " " + STRING(ag[4],"->,>>>,>>>,>>9.99") + " " +
                     STRING(ag[5],"->,>>>,>>>,>>9.99") .

           cExcelDisplay = cExcelDisplay +  STRING(ag[1]) + "," + STRING(ag[2]) + "," +
                            STRING(ag[3]) + "," + STRING(ag[4]) + "," +
                            STRING(ag[5]) .

          PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
    END.  /* end of detail */

    ELSE DO:
         IF first-time THEN DO:
      FIND FIRST terms NO-LOCK WHERE terms.company EQ vend.company AND
          terms.t-code EQ vend.terms NO-ERROR.
      IF AVAILABLE terms THEN v-terms = terms.dscr.
     
      PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

      first-time = NO.

     
    END.  /* end of first-time */

    END.

  END.  /* if v-amt ne 0  */

  IF LAST(ap-inv.vend-no) AND t1 NE 0 THEN DO:
    IF ni GT 1 THEN m3 = "".
    IF ni EQ 1 THEN m3 = m2.
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
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "curr"      THEN cVarValue = "" .
                         WHEN "vend"      THEN cVarValue = "".
                         WHEN "vend-name" THEN cVarValue = "".
                         WHEN "phone"     THEN cVarValue = "" .
                         WHEN "type"      THEN cVarValue = "" .
                         WHEN "term"      THEN cVarValue = "" .
                         WHEN "inv"       THEN cVarValue = "" .
                         WHEN "date"      THEN cVarValue = "" .
                         WHEN "amt"       THEN cVarValue = STRING(t1,"->,>>>,>>>,>>9.99") .
                         WHEN "day"       THEN cVarValue = "" .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
            END.

            cDisplay =  cDisplay + STRING(cust-t[1],"->,>>>,>>>,>>9.99") + " " + STRING(cust-t[2],"->,>>>,>>>,>>9.99") + 
                     " " + STRING(cust-t[3],"->,>>>,>>>,>>9.99") + " " + STRING(cust-t[4],"->,>>>,>>>,>>9.99") + " " +
                     STRING(cust-t[5],"->,>>>,>>>,>>9.99") .

            cExcelDisplay = cExcelDisplay +  STRING(cust-t[1]) + "," + STRING(cust-t[2]) + "," +
                            STRING(cust-t[3]) + "," + STRING(cust-t[4]) + "," +
                            STRING(cust-t[5]) .


          PUT UNFORMATTED "    Total Dollars" SUBSTRING(cDisplay,18,300) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'Total Dollars ,' + SUBSTRING(cExcelDisplay,4,300) SKIP.
             END.
        
    
    DO i = 1 TO 5:
       curr-t[i] = curr-t[i] + cust-t[i].
    END.
    ASSIGN 
     cust-t = 0
     t2     = t2 + t1
     t1     = 0.
  END.  /* last-of loop */
END.  /* for each ap-inv */

/* end ---------------------------------- copr. 1992  advanced software, inc. */
