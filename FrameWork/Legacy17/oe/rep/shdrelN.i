/* ----------------------------------------------- oe/rep/schdrel.i 02/99 JLF */
/* Schedule Release Report                                                    */
/* -------------------------------------------------------------------------- */
/*MESSAGE "chosen  " + STRING(chosen) VIEW-AS ALERT-BOX ERROR.*/
/*if chosen eq 2 then DO:*/
    /* oe/rep/schdrel3N.i */     
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
                    /* IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(w-ord.job-no2,"99") ELSE "".                  
                    */

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
                WHEN "v-del-zone" THEN cVarValue = string(v-del-zone).
                WHEN "style" THEN cVarValue = string(style).
                WHEN "v-terr" THEN cVarValue = string(v-terr).                
                WHEN "v-crRate" THEN cVarValue = string(v-crRate).
                WHEN "w-ord-last-date" THEN cVarValue = IF w-ord.last-date NE ? THEN STRING(w-ord.last-date) ELSE "".
                    
                WHEN "routing" THEN do:  /* task 04091206*/
                        ASSIGN v-m-code = "" .
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
                                    tt-fg-set.part-qty     = reftable.val[12]
                                    tt-fg-set.part-qty-dec = reftable.val[13].
                                END.
                                ELSE DO:
                                    CREATE tt-fg-set.
                                    ASSIGN
                                        tt-fg-set.part-no      = job-hdr.i-no
                                        tt-fg-set.part-qty     = job-hdr.frm
                                        tt-fg-set.part-qty-dec = job-hdr.blank-no.
                                END.

                                FOR EACH tt-fg-set
                                    BREAK BY tt-fg-set.part-qty
                                    BY tt-fg-set.part-qty-dec:

                                    FOR EACH job-mch
                                        WHERE job-mch.company EQ job.company
                                        AND job-mch.job     EQ job.job
                                        AND job-mch.job-no  EQ job.job-no
                                        AND job-mch.job-no2 EQ job.job-no2
                                        AND job-mch.frm     EQ tt-fg-set.part-qty
                                        NO-LOCK
                                        BREAK BY job-mch.line:
                                        IF FIRST(job-mch.line) AND v-m-code <> "" THEN 
                                            v-m-code = v-m-code + "," .
                                        ASSIGN v-m-code = v-m-code + job-mch.m-code.
                                       /* PUT UNFORMATTED job-mch.m-code. */
                                          IF NOT LAST(job-mch.line) THEN do: /*PUT ", ".*/
                                          ASSIGN  v-m-code = v-m-code + "," .
                                          END.
                                        END.
                             END.
                        END.

                                    cVarValue = IF v-m-code NE "" THEN v-m-code ELSE "" .
                END.
            END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.
         
      END.
        /*MESSAGE "3n.i: " cSelectedList SKIP
             cDisplay SKIP
             "------" SKIP
             w-ord.i-no ":" w-ord.onh-qty  w-ord.rel-qty w-ord.ord-qty
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
          */
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /* ========== end of new selectable culumns =======*/


   /* {oe/rep/schdrel3N.i}*/
/*END.*/

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
       tt-fg-set.part-qty     = reftable.val[12]
       tt-fg-set.part-qty-dec = reftable.val[13].
    END.

    ELSE DO:
      CREATE tt-fg-set.
      ASSIGN
       tt-fg-set.part-no      = job-hdr.i-no
       tt-fg-set.part-qty     = job-hdr.frm
       tt-fg-set.part-qty-dec = job-hdr.blank-no.
    END.

    FOR EACH tt-fg-set
        BREAK BY tt-fg-set.part-qty
              BY tt-fg-set.part-qty-dec:

      PUT SPACE(5)
          "S/B: "
          TRIM(STRING(tt-fg-set.part-qty,">>")) + "/" +
              TRIM(STRING(tt-fg-set.part-qty-dec,">>"))   FORMAT "x(5)"
          SPACE(1).

      ll-po = NO.
   
      IF LAST-OF(tt-fg-set.part-qty) THEN
      FOR EACH po-ordl
          WHERE po-ordl.company   EQ job.company
            AND po-ordl.job-no    EQ job.job-no
            AND po-ordl.job-no2   EQ job.job-no2
            AND po-ordl.s-num     EQ tt-fg-set.part-qty
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
            AND job-mch.frm     EQ tt-fg-set.part-qty
          NO-LOCK
          BREAK BY job-mch.line:
        IF FIRST(job-mch.line) THEN PUT "Routing: ".
        PUT UNFORMATTED job-mch.m-code.
        IF NOT LAST(job-mch.line) THEN PUT ", ".
      END.

      PUT SKIP.

      IF LAST(tt-fg-set.part-qty)        AND
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
