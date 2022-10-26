/* ----------------------------------------------- oe/rep/schdrel.i 02/99 JLF */
/* Schedule Release Report                                                    */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE cReasonCode AS CHARACTER NO-UNDO .
DEFINE VARIABLE cReasonDesc AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdJobProcs  AS HANDLE    NO-UNDO.
DEFINE VARIABLE iFormNo     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlankNo    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalPallet AS INTEGER  NO-UNDO.
RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

if chosen eq 2 then DO:
    
    IF rd_printOnhand = "z" AND w-ord.tot-qty <> 0 THEN NEXT.
    IF rd_printOnHand = "n" AND (w-ord.onh-qty GE w-ord.tot-qty  ) THEN NEXT.   /* task 04171301 */
    IF rd_printOnHand = "P" AND (w-ord.onh-qty LE w-ord.tot-qty  ) THEN NEXT.   /* task 04171301 */

 
     
    assign
       v-tot-qty[1] = v-tot-qty[1] + 1
       v-tot-msf[1] = v-tot-msf[1] + w-ord.msf
       v-tot-val[1] = v-tot-val[1] + w-ord.t-price.
       .
       
       ASSIGN
       iCount       = INT((INT(itemfg.case-count) * INT(itemfg.case-pall))
                        + INT(itemfg.quantityPartial))
       iTotalPallet = INT(w-ord.rel-qty / iCount)           
       .
                   
    /*{oe/rep/schdrel3N.i}*/

/* oe/rep/schdrel3N.i */ 
       
       ASSIGN cReasonCode  = ""
             cReasonDesc  = ""
             iFormNo      = 0
             iBlankNo     = 0.

       FIND FIRST job-hdr NO-LOCK
           WHERE job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ w-ord.job-no
             AND job-hdr.job-no2 EQ w-ord.job-no2
             AND job-hdr.ord-no  EQ w-ord.ord-no
             AND job-hdr.i-no    EQ w-ord.i-no
           NO-ERROR.
       IF NOT AVAIL job-hdr THEN
           FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ cocode
                  AND job-hdr.job-no  EQ w-ord.job-no
                  AND job-hdr.job-no2 EQ w-ord.job-no2
                  AND job-hdr.ord-no  EQ w-ord.ord-no
                NO-ERROR.
       
       IF AVAIL job-hdr THEN
           FIND FIRST job NO-LOCK
                WHERE job.company EQ job-hdr.company
                  AND job.job     EQ job-hdr.job
                  AND job.job-no  EQ job-hdr.job-no
                  AND job.job-no2 EQ job-hdr.job-no2
                NO-ERROR.

       IF AVAILABLE job AND job.stat = "H" THEN DO: 
           FIND FIRST rejct-cd NO-LOCK
                WHERE rejct-cd.type = "JH" 
                  AND rejct-cd.code = job.reason NO-ERROR.
       
           IF AVAILABLE job-hdr AND AVAILABLE rejct-cd AND AVAIL job AND job.job-no NE "" THEN
               ASSIGN cReasonCode = job.reason
                     cReasonDesc  =  rejct-cd.dscr.      
       END.
       
       IF AVAILABLE job AND w-ord.is-a-component AND
        CAN-FIND(FIRST reftable
                 WHERE reftable.reftable EQ "jc/jc-calc.p"
                 AND reftable.company  EQ job.company
                 AND reftable.loc      EQ ""
                 AND reftable.code     EQ STRING(job.job,"999999999")) THEN
        FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-hdr.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-hdr.job,"999999999")
        AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
             (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component))
        NO-LOCK:
         ASSIGN
           iFormNo   = reftable.val[12]
           iBlankNo  = reftable.val[13].        
       END .
       ELSE IF AVAIL job-hdr THEN DO:                 
            assign
              iFormNo = job-hdr.frm
              iBlankNo = job-hdr.blank-no .
       END.
        
          
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField = BUFFER bw-ord:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).

                     IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) <> "C" THEN DO:
                        IF LENGTH(trim(cTmpField)) < int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))
                           THEN cTmpField = FILL(" ",INT(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(TRIM(cTmpField)) )
                                          + TRIM(cTmpField).
                        /*MESSAGE "3n.i:" w-ord.ord-no w-ord.i-no w-ord.onh-qty w-ord.ord-qty SKIP
                            cfieldname ":" cTmpfield LENGTH(trim(cTmpField)) int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                        cTmpField = cTmpField + " ".
                     END.
                     ELSE cTmpField =  cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                     cDisplay = cDisplay + cTmpField.
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField:                               
                WHEN "ttl-alc" THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.q-alloc,"->,>>>,>>9") ELSE "".
                WHEN "ttl-avl" THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.q-avail,"->,>>>,>>9") ELSE "".
                WHEN "fg-cat"  THEN cVarValue = IF AVAIL itemfg THEN  STRING(itemfg.procat,"x(5)") ELSE "".
                WHEN "over-run"  THEN cVarValue = IF AVAIL cust THEN  STRING(cust.over-pct,">>9.99%") ELSE "".
                WHEN "style" THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.style) ELSE "".
                WHEN "v-del-zone" THEN cVarValue = string(v-del-zone).
                WHEN "v-terr" THEN cVarValue = string(v-terr).                
                WHEN "v-crRate" THEN cVarValue = string(v-crRate). 
                WHEN "oh-relqty" THEN cVarValue = string(v-oh-relqty,"->>,>>>,>>9").
                 WHEN "ship-add1" THEN cVarValue = string(v-ship-add1).
                WHEN "ship-add2" THEN cVarValue = string(v-ship-add2).
                WHEN "ship-cty" THEN cVarValue = string(v-ship-city).
                WHEN "ship-stat" THEN cVarValue = string(v-ship-stat).
                WHEN "ship-zip" THEN cVarValue = string(v-ship-zip).
                WHEN "ship-name" THEN cVarValue = string(v-ship-name).
                WHEN "trans-day"  THEN DO:
                    IF AVAIL shipto THEN
                        ASSIGN cVarValue = STRING(INT(shipto.del-time),">>>>>>>>>>9").
                    ELSE 
                        cVarValue = "" .
                END.
                WHEN "notes"  THEN DO:
                    IF AVAIL shipto THEN
                        ASSIGN cVarValue = STRING((shipto.dock-hour),"x(20)").
                    ELSE 
                        cVarValue = "" .
                END.
                WHEN "sa-ship-date" THEN DO:
                  IF AVAIL shipto AND w-ord.xls-rel-date NE ? THEN
                      cVarValue = STRING(w-ord.xls-rel-date - INT(shipto.spare-int-1),"99/99/9999") .
                  ELSE cVarValue = "" .
                END.
                WHEN "dock-ship-date" THEN DO:
                  IF AVAIL shipto AND w-ord.xls-rel-date NE ? THEN
                      cVarValue = STRING(w-ord.xls-rel-date - INT(shipto.spare-int-2),"99/99/9999") .
                  ELSE cVarValue = "" .
                END.
                WHEN "ear-ship-date" THEN DO:
                  IF AVAIL shipto AND w-ord.xls-rel-date NE ?  THEN
                      cVarValue = STRING(w-ord.xls-rel-date - INT(shipto.spare-int-3),"99/99/9999") .
                  ELSE cVarValue = "" .
                END.
                WHEN "lat-ship-date" THEN DO:
                  IF AVAIL shipto AND w-ord.xls-rel-date NE ?  THEN
                      cVarValue = STRING(w-ord.xls-rel-date + INT(shipto.spare-int-4),"99/99/9999") .
                  ELSE cVarValue = "" .
                END.
                WHEN "due-dt" THEN DO:
                  IF w-ord.due-date NE ? THEN
                      cVarValue = STRING(w-ord.due-date) .
                  ELSE cVarValue = "" .
                END.
                WHEN "po-due-date" THEN DO:
                  IF w-ord.po-due-date NE ? THEN
                      cVarValue = STRING(w-ord.po-due-date,"99/99/9999") .
                  ELSE cVarValue = "" .
                END.
                WHEN "ord-date" THEN DO:
                  IF w-ord.ord-date NE ? THEN
                      cVarValue = STRING(w-ord.ord-date) .
                  ELSE cVarValue = "" .
                END.
                WHEN "stat" THEN DO:
                  IF AVAIL shipto THEN
                      cVarValue = STRING(shipto.ship-stat,"x(5)") .
                  ELSE cVarValue = "" .
                END.
                WHEN "job-h-code" THEN DO:
                    cVarValue = cReasonCode .
                END.
                 WHEN "job-h-desc" THEN DO:
                    cVarValue = string(cReasonDesc,"x(15)") .
                END.
                WHEN "run-comp" OR WHEN "comp-date" THEN do: 
                        ASSIGN v-lst-m-code = "" .

                        FOR EACH tt-fg-set:
                            DELETE tt-fg-set.
                        END.
                        RELEASE job-hdr.
                        RELEASE job.
                        RELEASE reftable.
                    
                    IF TRIM(w-ord.job-no) EQ "" THEN do:
                        find first itemfg
                            where itemfg.company eq cocode
                            and itemfg.i-no    eq w-ord.i-no
                            no-lock.
                       
                        IF AVAIL itemfg AND itemfg.est-no NE "" THEN
                          FOR EACH job-hdr
                            WHERE job-hdr.company EQ cocode
                            AND job-hdr.est-no    EQ itemfg.est-no
                            AND job-hdr.opened  EQ YES
                            NO-LOCK
                            BY ROWID(job-hdr) DESC:
                            LEAVE.
                        END.
                        ELSE DO:
                            FOR EACH job-hdr
                                WHERE job-hdr.company EQ cocode
                                AND job-hdr.ord-no  EQ w-ord.ord-no
                                AND job-hdr.cust-no EQ w-ord.cust-no
                                AND job-hdr.i-no    EQ w-ord.i-no
                                AND job-hdr.opened  EQ YES
                                NO-LOCK
                                BY ROWID(job-hdr) DESC:
                                LEAVE.
                            END.
                        END.
                    END.

                    ELSE DO:
                        FIND FIRST job-hdr
                            WHERE job-hdr.company EQ cocode
                            AND job-hdr.job-no  EQ w-ord.job-no
                            AND job-hdr.job-no2 EQ w-ord.job-no2
                            AND job-hdr.ord-no  EQ w-ord.ord-no
                            AND job-hdr.i-no    EQ w-ord.i-no
                            NO-LOCK NO-ERROR.
                        IF NOT AVAIL job-hdr THEN
                            FIND FIRST job-hdr
                            WHERE job-hdr.company EQ cocode
                            AND job-hdr.job-no  EQ w-ord.job-no
                            AND job-hdr.job-no2 EQ w-ord.job-no2
                            AND job-hdr.ord-no  EQ w-ord.ord-no
                            NO-LOCK NO-ERROR.
                    END.
                   
                        IF AVAIL job-hdr THEN
                            FIND FIRST job
                            WHERE job.company EQ job-hdr.company
                            AND job.job     EQ job-hdr.job
                            AND job.job-no  EQ job-hdr.job-no
                            AND job.job-no2 EQ job-hdr.job-no2
                            NO-LOCK NO-ERROR.
                        
                        IF AVAIL job THEN DO:
                            IF (itemfg.isaset OR w-ord.is-a-component)                          AND
                                CAN-FIND(FIRST reftable
                                         WHERE reftable.reftable EQ "jc/jc-calc.p"
                                         AND reftable.company  EQ job.company
                                         AND reftable.loc      EQ ""
                                         AND reftable.code     EQ STRING(job.job,"999999999")) THEN
                                FOR EACH reftable
                                WHERE reftable.reftable EQ "jc/jc-calc.p"
                                AND reftable.company  EQ job-hdr.company
                                AND reftable.loc      EQ ""
                                AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                                AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
                                     (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component))
                                NO-LOCK:
                                CREATE tt-fg-set.
                                ASSIGN
                                    tt-fg-set.part-no      = reftable.code2
                                    tt-fg-set.QtyPerSet     = reftable.val[12]
                                    tt-fg-set.part-qty-dec = reftable.val[13].
                                END.
                                ELSE DO:
                                    CREATE tt-fg-set.
                                    ASSIGN
                                        tt-fg-set.part-no      = job-hdr.i-no
                                        tt-fg-set.QtyPerSet     = job-hdr.frm
                                        tt-fg-set.part-qty-dec = job-hdr.blank-no.
                                END.

                                FOR EACH tt-fg-set
                                    BREAK BY tt-fg-set.QtyPerSet
                                    BY tt-fg-set.part-qty-dec:
                                     
                                    FOR EACH job-mch
                                        WHERE job-mch.company EQ job.company
                                        AND job-mch.job     EQ job.job
                                        AND job-mch.job-no  EQ job.job-no
                                        AND job-mch.job-no2 EQ job.job-no2
                                        AND job-mch.frm     EQ INTEGER(tt-fg-set.QtyPerSet)
                                        NO-LOCK
                                        BREAK BY job-mch.line:
                                      /*  IF FIRST(job-mch.line) AND v-m-code <> "" THEN 
                                            v-m-code = v-m-code + "," .
                                        ASSIGN v-m-code = v-m-code + job-mch.m-code.*/
                                       /* PUT UNFORMATTED job-mch.m-code. */
                                          IF LAST(job-mch.line) THEN do: /*PUT ", ".*/
                                          IF cTmpField = "run-comp"  THEN ASSIGN  cVarValue = STRING(job-mch.run-complete) .
                                          ELSE IF cTmpField = "comp-date" THEN ASSIGN  cVarValue = IF job-mch.end-date NE ? THEN STRING(job-mch.end-date) ELSE "" .
                                          END.
                                    END.
                                END.
                        END.
                        ELSE DO:
                            ASSIGN  cVarValue = "".
                        END.
                END.

                WHEN "routing" THEN do:  /* task 04091206*/
                        ASSIGN v-m-code = "" .
                        FOR EACH tt-fg-set:
                            DELETE tt-fg-set.
                        END.
                        RELEASE job-hdr.
                        RELEASE job.
                        RELEASE reftable.
                    
                    IF TRIM(w-ord.job-no) EQ "" THEN do:
                        find first itemfg
                            where itemfg.company eq cocode
                            and itemfg.i-no    eq w-ord.i-no
                            no-lock.
                       
                        IF AVAIL itemfg AND itemfg.est-no NE "" THEN
                        RUN GetOperationsForEst IN hdJobProcs (
                                 INPUT itemfg.company,
                                 INPUT itemfg.est-no,
                                 OUTPUT v-m-code
                                 ).
                    END.

                    ELSE DO:                          
                        RUN GetOperationsForJob IN hdJobProcs (
                                INPUT cocode,
                                INPUT w-ord.job-no,
                                INPUT w-ord.job-no2,
                                INPUT iFormNo,
                                INPUT iBlankNo,
                                INPUT-OUTPUT v-m-code
                                ). 
                    END.

                    cVarValue = IF v-m-code NE "" THEN string(v-m-code,"x(35)") ELSE "" .
                END.
                
                WHEN "remaining-routing" THEN do:  
                        ASSIGN v-m-code = "" .
                        RUN GetOperationsForJobNotCompleted IN hdJobProcs (
                                INPUT cocode,
                                INPUT w-ord.job-no,
                                INPUT w-ord.job-no2,
                                INPUT iFormNo,
                                INPUT iBlankNo,
                                INPUT-OUTPUT v-m-code
                                ).
                        
                    cVarValue = IF v-m-code NE "" THEN string(v-m-code,"x(35)") ELSE "" .  
                END.
                
                WHEN "mfg-date" THEN DO:
                  IF w-ord.prom-date NE ? THEN
                      cVarValue = STRING(w-ord.prom-date) .
                  ELSE cVarValue = "" .
                END.
                WHEN "dock-appointment" THEN cVarValue = IF v-dockTime NE ? THEN STRING(v-dockTime,"99/99/9999 HH:MM") ELSE "" .
            
                WHEN "pallet-count-quantity" THEN cVarValue = STRING(iCount).
                WHEN "iTotalPallet" THEN cVarValue = STRING(iTotalPallet, "->>,>>>,>>9").
                WHEN "xls-rel-date" THEN cVarValue = IF w-ord.xls-rel-date NE ? THEN STRING(w-ord.xls-rel-date,"99/99/9999") ELSE "" .
                WHEN "last-date" THEN cVarValue = IF w-ord.last-date NE ? THEN STRING(w-ord.last-date) ELSE "" .
                WHEN "promiseDate" THEN cVarValue = IF w-ord.promiseDate NE ? THEN STRING(w-ord.promiseDate) ELSE "" .
                WHEN "rel-due-date" THEN cVarValue = IF w-ord.rel-due-date NE ? THEN STRING(w-ord.rel-due-date) ELSE "" .
                WHEN "ord-due-date" THEN cVarValue = IF w-ord.ord-due-date NE ? THEN STRING(w-ord.ord-due-date) ELSE "" .
                
            END CASE.
            
            IF cTmpField = "sa-ship-date" THEN cExcelVarValue = IF AVAIL shipto THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.xls-rel-date - INT(shipto.spare-int-1)) ELSE "".
            ELSE IF cTmpField = "dock-ship-date" THEN cExcelVarValue = IF AVAIL shipto THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.xls-rel-date - INT(shipto.spare-int-2)) ELSE "" .
            ELSE IF cTmpField = "ear-ship-date" THEN cExcelVarValue = IF AVAIL shipto THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.xls-rel-date - INT(shipto.spare-int-3)) ELSE "" .
            ELSE IF cTmpField = "lat-ship-date" THEN cExcelVarValue = IF AVAIL shipto THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.xls-rel-date + INT(shipto.spare-int-4)) ELSE "" .
            ELSE IF cTmpField = "due-dt" THEN cExcelVarValue = IF w-ord.due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.due-date) ELSE "" .
            ELSE IF cTmpField = "po-due-date" THEN cExcelVarValue = IF w-ord.po-due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.po-due-date) ELSE "" .
            ELSE IF cTmpField = "ord-date" THEN cExcelVarValue = IF w-ord.ord-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.ord-date) ELSE "" .
            ELSE IF cTmpField = "mfg-date" THEN cExcelVarValue = IF w-ord.prom-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.prom-date) ELSE "" .
            ELSE IF cTmpField = "dock-appointment" THEN cExcelVarValue = IF v-dockTime NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-dockTime) ELSE "" .
            ELSE IF cTmpField = "last-date" THEN cExcelVarValue = IF w-ord.last-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.last-date) ELSE "" .
            ELSE IF cTmpField = "promiseDate" THEN cExcelVarValue = IF w-ord.promiseDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.promiseDate) ELSE "" .
            ELSE IF cTmpField = "xls-rel-date" THEN cExcelVarValue = IF  w-ord.xls-rel-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date", w-ord.xls-rel-date) ELSE "" .
            ELSE IF cTmpField = "comp-date" THEN ASSIGN  cExcelVarValue = IF job-mch.end-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",job-mch.end-date) ELSE "" .
            ELSE IF cTmpField = "rel-due-date" THEN ASSIGN  cExcelVarValue = IF w-ord.rel-due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.rel-due-date) ELSE "" .
            ELSE IF cTmpField = "ord-due-date" THEN ASSIGN  cExcelVarValue = IF w-ord.ord-due-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.ord-due-date) ELSE "" .
            
            ELSE cExcelVarValue = cVarValue.
            
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
         END.
         
      END.
        /*MESSAGE "3n.i: " cSelectedList SKIP
             cDisplay SKIP
             "------" SKIP
             w-ord.i-no ":" w-ord.onh-qty  w-ord.rel-qty w-ord.ord-qty
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
          */
      PUT UNFORMATTED cDisplay SKIP.
      IF rd-dest EQ 3 THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /* ========== end of new selectable culumns =======*/

END.



FOR EACH tt-formtext:
  DELETE tt-formtext.
END.

IF tb_notes AND AVAIL itemfg THEN DO:
  lv-text = "".

  FOR EACH notes
      WHERE notes.rec_key   EQ itemfg.rec_key
        AND notes.note_type EQ "S"
        AND notes.note_code GE begin_spec
        AND notes.note_code LE end_spec
      NO-LOCK
      BREAK BY notes.note_code:
    IF FIRST-OF(notes.note_code) THEN
      lv-text = lv-text + " " + TRIM(notes.note_code) + ":".
              
    lv-text = lv-text + " " + TRIM(notes.note_text).
     
    IF LAST-OF(notes.note_code) THEN lv-text = lv-text + CHR(10).
  END.

  DO li = 1 TO 50:
    CREATE tt-formtext.
    ASSIGN
     tt-line-no = li
     tt-length  = 115.
  END.

  RUN custom/formtext.p (lv-text).

  FOR EACH tt-formtext WHERE tt-text EQ "":
    DELETE tt-formtext.
  END.
END.

IF tb_stats THEN DO:
  FOR EACH tt-fg-set:
    DELETE tt-fg-set.
  END.

  RELEASE job-hdr.
  RELEASE job.
  RELEASE reftable.

  IF TRIM(w-ord.job-no) EQ "" THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ cocode
        AND job-hdr.ord-no  EQ w-ord.ord-no
        AND job-hdr.cust-no EQ w-ord.cust-no
        AND job-hdr.i-no    EQ w-ord.i-no
        AND job-hdr.opened  EQ YES
      NO-LOCK
      BY ROWID(job-hdr) DESC:
    LEAVE.
  END.

  ELSE DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no  EQ w-ord.job-no
          AND job-hdr.job-no2 EQ w-ord.job-no2
          AND job-hdr.ord-no  EQ w-ord.ord-no
          AND job-hdr.i-no    EQ w-ord.i-no
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no  EQ w-ord.job-no
          AND job-hdr.job-no2 EQ w-ord.job-no2
          AND job-hdr.ord-no  EQ w-ord.ord-no
        NO-LOCK NO-ERROR.
  END.

  IF AVAIL job-hdr THEN
  FIND FIRST job
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
      NO-LOCK NO-ERROR.

  IF AVAIL job THEN DO:
    
    IF (itemfg.isaset OR w-ord.is-a-component)                          AND
       CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                  AND reftable.company  EQ job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(job.job,"999999999")) THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-hdr.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
          AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
               (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component))
        NO-LOCK:
      CREATE tt-fg-set.
      ASSIGN
       tt-fg-set.part-no      = reftable.code2
       tt-fg-set.QtyPerSet     = reftable.val[12]
       tt-fg-set.part-qty-dec = reftable.val[13].
    END.

    ELSE DO:
      CREATE tt-fg-set.
      ASSIGN
       tt-fg-set.part-no      = job-hdr.i-no
       tt-fg-set.QtyPerSet     = job-hdr.frm
       tt-fg-set.part-qty-dec = job-hdr.blank-no.
    END.

    FOR EACH tt-fg-set
        BREAK BY tt-fg-set.QtyPerSet
              BY tt-fg-set.part-qty-dec:

      PUT SPACE(5)
          "S/B: "
          TRIM(STRING(tt-fg-set.QtyPerSet,"->>>,>99.99<<<<")) + "/" +
              TRIM(STRING(tt-fg-set.part-qty-dec,">>"))   FORMAT "x(5)"
          SPACE(1).

      ll-po = NO.
   
      IF LAST-OF(tt-fg-set.QtyPerSet) THEN
      FOR EACH po-ordl
          WHERE po-ordl.company   EQ job.company
            AND po-ordl.job-no    EQ job.job-no
            AND po-ordl.job-no2   EQ job.job-no2
            AND po-ordl.s-num     EQ INTEGER(tt-fg-set.QtyPerSet)
            AND po-ordl.item-type EQ YES
          USE-INDEX job-no NO-LOCK,
          FIRST po-ord
          WHERE po-ord.company EQ po-ordl.company
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-LOCK,
          FIRST item
          WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
            AND INDEX("1234BPR",item.mat-type) GT 0
          NO-LOCK
          BREAK BY po-ordl.po-no
                BY po-ordl.i-no
                BY po-ordl.rec_key:

        ll-po = YES.

        IF po-ordl.pr-qty-uom EQ "EA" THEN
          ld-qty-ord = po-ordl.ord-qty.
        ELSE
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                                 item.basis-w,
                                 po-ordl.s-len,
                                 po-ordl.s-wid,
                                 item.s-dep,
                                 po-ordl.ord-qty, output ld-qty-ord).
        {sys/inc/roundup.i ld-qty-ord}

        IF po-ordl.cons-uom EQ "EA" THEN
          ld-qty-rec = po-ordl.t-rec-qty.
        ELSE
          RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",
                                 item.basis-w,
                                 po-ordl.s-len,
                                 po-ordl.s-wid,
                                 item.s-dep,
                                 po-ordl.t-rec-qty, output ld-qty-rec).
        {sys/inc/roundup.i ld-qty-rec}

        IF NOT LAST(po-ordl.po-no) THEN PUT SPACE(16).

        PUT "Brd PO#: " 
            TRIM(STRING(po-ordl.po-no,">>>>>>>>")) FORMAT "x(8)"
            SPACE(1)
            "Vendor: "
            po-ord.vend-no                         FORMAT "x(8)"
            SPACE(1)
            "Qty Rec'd: "
            TRIM(STRING(ld-qty-rec,">>>,>>>,>>9")) FORMAT "x(11)"
            SPACE(1).

        IF NOT LAST(po-ordl.po-no) THEN PUT SKIP.
      END.

      IF NOT ll-po THEN PUT SPACE(58).

      FOR EACH job-mch
          WHERE job-mch.company EQ job.company
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2
            AND job-mch.frm     EQ INTEGER(tt-fg-set.QtyPerSet)
          NO-LOCK
          BREAK BY job-mch.line:
        IF FIRST(job-mch.line) THEN PUT "Routing: ".
        PUT UNFORMATTED job-mch.m-code.
        IF NOT LAST(job-mch.line) THEN PUT ", ".
      END.

      PUT SKIP.

      IF LAST(tt-fg-set.QtyPerSet)        AND
         NOT CAN-FIND(FIRST tt-formtext) THEN PUT SKIP(1).
    END.
  END.
END.

FOR EACH tt-formtext BREAK BY tt-line-no:
  IF FIRST(tt-line-no) THEN
    PUT SPACE(5)
        "Spec Notes: ".

  PUT tt-text AT 18 FORMAT "x(115)" SKIP.
         
  IF LAST(tt-line-no) THEN PUT SKIP(1).

END.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
