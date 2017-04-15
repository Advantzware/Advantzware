
 for each cust
        where cust.company eq cocode
          and cust.cust-no ge begin_cust
          and cust.cust-no le end_cust
        no-lock:

     {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

     FOR EACH ar-invl WHERE ar-invl.cust-no EQ cust.cust-no
                   AND ar-invl.posted   EQ yes               
                   AND ar-invl.company EQ cocode NO-LOCK,
         FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no 
                   AND ar-inv.inv-date GE begin_date
                   AND ar-inv.inv-date LE end_date NO-LOCK
/*          ,                                                                                  */
/*            EACH oe-boll WHERE oe-boll.b-no EQ ar-invl.b-no                                  */
/*                         AND oe-boll.company EQ ar-invl.company                              */
/*                         AND oe-boll.i-no EQ ar-invl.i-no,                                   */
/*                 FIRST job-mch WHERE job-mch.i-no EQ oe-boll.i-no                            */
/*                             AND job-mch.job-no EQ oe-boll.job-no USE-INDEX line-idx NO-LOCK */
         :

         {custom/statusMsg.i " 'Processing Customer#  '  + ar-invl.cust-no "}

           vdWeight = 0.
           vcMCode = "".
           FOR EACH oe-boll WHERE oe-boll.b-no EQ ar-invl.b-no
                        AND oe-boll.company EQ ar-invl.company
                        AND oe-boll.i-no EQ ar-invl.i-no NO-LOCK:
               vdWeight = vdWeight + oe-boll.weight.

               FIND FIRST job-mch WHERE job-mch.i-no EQ oe-boll.i-no
                   AND job-mch.job-no EQ oe-boll.job-no USE-INDEX line-idx NO-LOCK NO-ERROR.
               IF AVAIL job-mch THEN
                   ASSIGN vcMCode = job-mch.m-code.
           END.
           IF vcMCode = "" THEN DO:
               FIND FIRST job-mch WHERE /*job-mch.i-no EQ oe-boll.i-no*/
                    job-mch.job-no EQ string(ar-inv.ord-no) USE-INDEX line-idx NO-LOCK NO-ERROR.
               IF AVAIL job-mch THEN
                   ASSIGN vcMCode = job-mch.m-code.

           END.
/*           IF tb_detail THEN DO: */
             FIND FIRST tt-report-dtl 
                 WHERE tt-report-dtl.cust-no EQ ar-inv.cust-no 
                 AND tt-report-dtl.m-code  = vcMCode
                 AND tt-report-dtl.i-code = ar-invl.i-no
                 AND tt-report-dtl.inv-no = ar-invl.inv-no
                 NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-report-dtl THEN CREATE tt-report-dtl.
             ASSIGN 
                 tt-report-dtl.m-code     = vcMCode
                 tt-report-dtl.sales-rep  = cust.sman
                 tt-report-dtl.cust-no    = ar-invl.cust-no
                 tt-report-dtl.inv-no     = ar-invl.inv-no                                           
                 tt-report-dtl.bol-no     = ar-invl.bol-no
                 tt-report-dtl.i-code     = ar-invl.i-no
                 tt-report-dtl.total-sales = tt-report-dtl.total-sales + ar-invl.amt
                 tt-report-dtl.weight     = tt-report-dtl.weight + vdWeight. 

/*          END.                                                                */
/*          ELSE DO:                                                            */
/*              FIND FIRST tt-report WHERE tt-report.cust-no EQ ar-inv.cust-no  */
/*                  AND tt-report.m-code  = job-mch.m-code NO-LOCK NO-ERROR.    */
/*              IF NOT AVAIL tt-report THEN CREATE tt-report.                   */
/*              ASSIGN                                                          */
/*                  tt-report.sales-rep   = cust.sman                           */
/*                  tt-report.m-code      = job-mch.m-code                      */
/*                  tt-report.cust-no     = ar-invl.cust-no                     */
/*                  tt-report.total-sales = tt-report.total-sales + ar-invl.amt */
/*                  tt-report.weight      = tt-report.weight + oe-boll.weight.  */
/*          END.                                                                */
      END.
 END.

 /*IF NOT tb_detail THEN DO:
    FOR EACH tt-report-dtl:

        {custom/statusMsg.i " 'Processing Customer#  '  + tt-report-dtl.cust-no "}

        FIND FIRST tt-report WHERE tt-report.cust-no EQ tt-report-dtl.cust-no 
                 AND tt-report.m-code  = tt-report-dtl.m-code NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-report THEN CREATE tt-report.
         ASSIGN 
             tt-report.sales-rep   = tt-report-dtl.sales-rep
             tt-report.m-code      = tt-report-dtl.m-code
             tt-report.cust-no     = tt-report-dtl.cust-no
             tt-report.total-sales = tt-report.total-sales + tt-report-dtl.total-sales
             tt-report.weight      = tt-report.weight + tt-report-dtl.weight. 
    END.
 END.*/
/* IF tb_detail THEN DO:
     display with frame r-top-2.*/

      for each tt-report-dtl NO-LOCK
          BREAK BY tt-report-dtl.m-code
                BY tt-report-dtl.cust-no :    

          {custom/statusMsg.i " 'Processing Customer#  '  + tt-report-dtl.cust-no "}

/*             FIND FIRST itemfg WHERE itemfg.company EQ cocode                         */
/*                            AND itemfg.i-no EQ tt-report-dtl.i-code NO-LOCK NO-ERROR. */

            PUT SKIP(0).

       /*     IF tb_excel THEN
            EXPORT STREAM excel DELIMITER ","  
                tt-report-dtl.m-code
                tt-report-dtl.cust-no
                tt-report-dtl.sales-rep 
                tt-report-dtl.inv-no
                tt-report-dtl.bol-no
                tt-report-dtl.i-code 
                tt-report-dtl.total-sales
                tt-report-dtl.weight
                SKIP.
          DISPLAY SPACE(5)                           
              tt-report-dtl.m-code 
              tt-report-dtl.cust-no FORMAT "X(8)"
              tt-report-dtl.sales-rep FORMAT "X(3)"  
              tt-report-dtl.inv-no FORMAT ">>>>>9"
              tt-report-dtl.bol-no FORMAT ">>>>>>>9"
              tt-report-dtl.i-code 
              tt-report-dtl.total-sales FORMAT "ZZZ,ZZZ,ZZ9.99"
              tt-report-dtl.weight FORMAT "ZZZ,ZZZ,ZZ9.99"
           WITH FRAME mch-dtl STREAM-IO width 132 NO-BOX NO-LABELS DOWN . */


          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch"         THEN cVarValue =  STRING(tt-report-dtl.m-code) .
                         WHEN "cust"    THEN cVarValue =  tt-report-dtl.cust-no .
                         WHEN "rep"         THEN cVarValue =  STRING(tt-report-dtl.sales-rep) .
                         WHEN "inv"         THEN cVarValue =  STRING(tt-report-dtl.inv-no,">>>>>9") .
                         WHEN "bol"         THEN cVarValue =  STRING(tt-report-dtl.bol-no,">>>>>>>9") .
                         WHEN "itm"         THEN cVarValue =  STRING(tt-report-dtl.i-code).
                         WHEN "ttl-sal"     THEN cVarValue =  STRING(tt-report-dtl.total-sales,"->>>,>>>,>>9.99")  .
                         WHEN "wgt"         THEN cVarValue =  STRING(tt-report-dtl.weight,"->>>,>>>,>>9.99")      .
                         
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

          ASSIGN  
              v-total-sales = v-total-sales + tt-report-dtl.total-sales
              v-total-weight = v-total-weight + tt-report-dtl.weight.

          IF LAST-OF(tt-report-dtl.m-code) THEN
          DO:
            /* DISPLAY SKIP(1)
             SPACE(70)
               "Machine Totals : " FORMAT "X(20)" SPACE(2)
               v-total-sales FORMAT "ZZZ,ZZZ,ZZ9.99" SPACE(8)
               v-total-weight FORMAT "ZZZ,ZZZ,ZZ9.99"
               WITH FRAME tot-mch1 STREAM-IO width 180 NO-LABELS no-box down. 

            IF tb_excel THEN
            DO:
                EXPORT STREAM excel DELIMITER ","
                        ""
                        ""                
                        ""
                        ""
                        ""
                        "Machine Totals :"   
                        v-total-sales
                        v-total-weight
                        SKIP.

                PUT STREAM excel SKIP(1).
           END.        */

              PUT SKIP str-line SKIP .
              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch"         THEN cVarValue =  "" .
                         WHEN "cust"    THEN cVarValue =   "" .
                         WHEN "rep"         THEN cVarValue =   "" .
                         WHEN "inv"         THEN cVarValue =   "" .
                         WHEN "bol"         THEN cVarValue =   "" .
                         WHEN "itm"         THEN cVarValue =   "" .
                         WHEN "ttl-sal"     THEN cVarValue =  STRING(v-total-sales,">>>,>>>,>>9.99")  .
                         WHEN "wgt"         THEN cVarValue =  STRING(v-total-weight,">>>,>>>,>>9.99")      .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "Machine Totals : " substring(cDisplay,17,300) SKIP.
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   "Machine Totals : " + substring(cExcelDisplay,3,300) SKIP.
        END.

           ASSIGN
               v-total-sales = 0
               v-total-weight = 0.
           PUT SKIP(1).
        END.
   END. 
 /*END.
 ELSE DO:
     for each tt-report
         break BY tt-report.m-code
              BY tt-report.cust-no:

          {custom/statusMsg.i " 'Processing Customer#  '  + tt-report.cust-no "}

        PUT SKIP(0).
        if FIRST-OF(tt-report.m-code) then do:        
          DISPLAY SPACE(3) "Machine: " + tt-report.m-code FORMAT "X(17)"                
             WITH FRAME mach-head STREAM-IO width 180 no-labels no-box down.
        
          display with frame r-top-2.
        END.


        IF tb_excel THEN
            EXPORT STREAM excel DELIMITER ","  
                   tt-report.m-code
                   tt-report.cust-no
                   tt-report.sales-rep 
                   tt-report.total-sales
                   tt-report.weight
                   SKIP.     
      
        DISPLAY SPACE(5)
             tt-report.cust-no FORMAT "X(8)" 
             tt-report.sales-rep FORMAT "X(3)"
             tt-report.total-sales FORMAT "ZZZ,ZZZ,ZZ9.99"
             tt-report.weight FORMAT "ZZZ,ZZZ,ZZ9.99"
             WITH FRAME mch-head STREAM-IO width 132 NO-BOX NO-LABELS DOWN .   

        ASSIGN
            v-total-sales  = v-total-sales + tt-report.total-sales
            v-total-weight = v-total-weight + tt-report.weight.

        IF LAST-OF(tt-report.m-code) THEN
        DO:
           DISPLAY SKIP(1)
              SPACE(12)
              "Machine Totals : " FORMAT "X(25)" SPACE(1)
              v-total-sales FORMAT "ZZZ,ZZZ,ZZ9.99" SPACE(13)
              v-total-weight FORMAT "ZZZ,ZZZ,ZZ9.99"
              WITH FRAME tot-mch STREAM-IO width 180 NO-LABELS no-box down.

           IF tb_excel THEN
           DO:
              EXPORT STREAM excel DELIMITER ","
                  ""
                  ""                
                  "Machine Totals :"
                  v-total-sales
                  v-total-weight
                  SKIP.

              PUT STREAM excel SKIP(1).
           END.

           ASSIGN
                v-total-weight = 0
                v-total-sales  = 0.

           PUT SKIP(1).
        END.
    END. 
END.*/

