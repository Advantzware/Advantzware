/* ------------------------------------------------- ar/rep/custcontact.i */
/* -----------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-cust.
   EMPTY TEMP-TABLE tt-contact.
   
   ASSIGN
      v-seq = 0
      v-page-break = NO.

   IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).

    /*  IF tb_contact-sort EQ NO THEN
      DO:
         excelheader = "Cust #,Name,Contact". */
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
   /*   END. */
   END.

 FOR EACH ttCustList
       WHERE ttCustList.log-fld
       NO-LOCK,
     EACH cust
       WHERE cust.company       EQ cocode
         AND cust.cust-no       EQ ttCustList.cust-no /*begin_cust-no*/
        /* AND cust.cust-no       LE end_cust-no*/
         AND cust.type          GE begin_cust-type
         AND cust.type          LE end_cust-type
         AND cust.sman          GE begin_slsmn
         AND cust.sman          LE end_slsmn
         AND cust.date-field[1] GE begin_date
         AND cust.date-field[1] LE end_date
         AND (cust.cust-level   EQ price-level OR price-level EQ 99)
         AND (cust.disc         NE 0 OR NOT tb_disc-only)
         AND (cust.active       NE "I" AND tb_active OR
              cust.active       EQ "I" AND tb_inactive)
       NO-LOCK
       {2},
       EACH phone FIELDS(attention) WHERE 
            phone.table_rec_key = cust.rec_key AND
            phone.titlcode = scr-title
           NO-LOCK
       BREAK BY cust.{1} BY cust.cust-no:

       {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

       IF NOT CAN-FIND(FIRST tt-cust WHERE
          tt-cust.cust-no EQ cust.cust-no) THEN
          DO:
             CREATE tt-cust.
             ASSIGN
                tt-cust.cust-no = cust.cust-no
                tt-cust.cust-name = cust.NAME
                tt-cust.seq = v-seq
                v-seq = v-seq + 1.
             RELEASE tt-cust.
          END.

       CREATE tt-contact.
       ASSIGN
          tt-contact.cust-no = cust.cust-no
          tt-contact.attention = phone.attention.
       RELEASE tt-contact.
   end.

   IF tb_contact-sort EQ NO THEN
      FOR EACH tt-cust,
          EACH tt-contact WHERE
               tt-contact.cust-no EQ tt-cust.cust-no
          BREAK BY tt-cust.seq:
          
       FIND FIRST cust WHERE cust.cust-no EQ tt-cust.cust-no NO-LOCK NO-ERROR .

       {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

        /*  DISPLAY tt-cust.cust-no WHEN FIRST-OF(tt-cust.seq) LABEL "Cust #"
                  tt-cust.cust-name WHEN FIRST-OF(tt-cust.seq) LABEL "Name"
                  tt-contact.attention LABEL "Contact"
                  SKIP.
          DOWN.
     
          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"' (IF FIRST-OF(tt-cust.seq) THEN tt-cust.cust-no ELSE "") '",'
               '"' (IF first-of(tt-cust.seq) THEN tt-cust.cust-name ELSE "") '",'
               '"' tt-contact.attention '",'
               SKIP. */
       find first sman
            where sman.company eq cust.company
            and sman.sman    eq cust.sman
            no-lock no-error.

        find first terms where terms.t-code = cust.terms 
            and terms.company = cocode
            no-lock no-error.
        
        assign
            v-fax-area-code = substr(cust.fax,1,3)
            v-fax-phone     = substr(cust.fax,4,7).

        ar-inv-loop:
        FOR EACH ar-inv FIELDS(inv-date) WHERE
           ar-inv.company EQ cust.company AND
           ar-inv.cust-no EQ cust.cust-no AND
           ar-inv.inv-date NE ?
           NO-LOCK
           USE-INDEX ar-inv
           BY ar-inv.inv-date:

           IF ar-inv.inv-date EQ ? THEN NEXT ar-inv-loop.

           IF ar-inv.inv-date GE begin_date AND
              ar-inv.inv-date LE END_date THEN
           DO:
           
            v-fst-invdt = STRING(ar-inv.inv-date) .
            LEAVE ar-inv-loop.    
           END.

           
        END.
        dtLastOrdered = ?.
        FOR EACH oe-ord
            WHERE oe-ord.company EQ cust.company
            AND oe-ord.cust-no eq cust.cust-no
            USE-INDEX cust NO-LOCK
            BY oe-ord.ord-date DESC:
            dtLastOrdered = oe-ord.ord-date.
            LEAVE.                                
        END.

        ASSIGN cDisplay = ""
                cTmpField = ""
                cVarValue = ""
                cExcelDisplay = ""
                cExcelVarValue = "".
         
         DO j = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(j,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField: 
                      WHEN "cust"          THEN cVarValue = STRING(cust.cust-no) .
                      WHEN "name"          THEN cVarValue = string(cust.NAME) .
                      WHEN "add1"          THEN cVarValue = string(cust.addr[1]) .
                      WHEN "add2"          THEN cVarValue = string(cust.addr[2]) .
                      WHEN "city"          THEN cVarValue = string(cust.city) .
                      WHEN "stat"          THEN cVarValue = STRING(cust.state) .
                      WHEN "zip"           THEN cVarValue = string(cust.zip) .
                      WHEN "phn"           THEN cVarValue = if cust.phone ne "" then string("(" + cust.area-code + ") ") + STRING(cust.phone,"999-9999") else "" .
                      WHEN "fax"           THEN cVarValue = if v-fax-phone ne "" AND v-fax-phone NE ? THEN string("(" + v-fax-area-code + ") ") + STRING(v-fax-phone,"999-9999") else "" .
                      WHEN "typ"           THEN cVarValue = STRING(cust.TYPE) .                         
                      WHEN "actv"          THEN cVarValue = string(cust.active) .                       
                      WHEN "cont"          THEN cVarValue = string(cust.contact) .                      
                      WHEN "rep"           THEN cVarValue = string(cust.sman) .                         
                      WHEN "rep-nam"       THEN cVarValue = if available sman then sman.sname else "" . 
                      WHEN "ato-rep"       THEN cVarValue = string(cust.auto-reprice) .                
                      WHEN "cust-prc"      THEN cVarValue = string(cust.cust-level) .                  
                      WHEN "edi"           THEN cVarValue = string(cust.an-edi-cust) .                 
                      WHEN "bal-mth"       THEN cVarValue = STRING(cust.stat-type,"O/F")  .            
                      WHEN "mfg-dys"       THEN cVarValue = STRING(cust.ship-days) .                   
                      WHEN "palt"          THEN cVarValue = string(cust.pallet) .                      
                      WHEN "case"          THEN cVarValue = string(cust.case-bundle) .                 
                      WHEN "stmnt"         THEN cVarValue = string(cust.stat-grp) .                    
                      WHEN "terr"          THEN cVarValue = string(cust.terr) .                        
                      WHEN "cr-act"        THEN cVarValue = STRING(cust.cr-use) .                      
                      WHEN "ord-loc"       THEN cVarValue = string(cust.loc) .                         
                      WHEN "carr"          THEN cVarValue = string(cust.carrier) .                     
                      WHEN "delvry"        THEN cVarValue = string(cust.del-zone) .                    
                      WHEN "cr-rat"        THEN cVarValue = string(cust.cr-rating) .                   
                      WHEN "holdy"         THEN cVarValue = STRING(cust.cr-hold-invdays) .             
                      WHEN "crd-lim"       THEN cVarValue = string(cust.cr-lim,">>>,>>>,>>9.99") .                      
                      WHEN "partl"         THEN cVarValue = IF cust.ship-part NE ? THEN STRING(cust.ship-part,"YES/NO") ELSE "" .         
                      WHEN "fr-pay"        THEN cVarValue = string(cust.frt-pay) .                     
                      WHEN "ord-lim"       THEN cVarValue = string(cust.ord-lim,">>,>>>,>>9.99") .                     
                      WHEN "taxbl"         THEN cVarValue = STRING(cust.sort,"X")   .                  
                      WHEN "fob"           THEN cVarValue = string(cust.fob-code) .                    
                      WHEN "crd-hld"       THEN cVarValue = string(cust.cr-hold) .                     
                      WHEN "tax-cd"        THEN cVarValue = string(cust.tax-gr) .                      
                      WHEN "fin-chrg"      THEN cVarValue = string(cust.fin-chg) .                     
                      WHEN "inv-po"        THEN cVarValue = IF cust.inv-meth NE ? THEN STRING(cust.inv-meth,"YES/NO") ELSE "" .           
                      WHEN "tax-rsl"       THEN cVarValue = string(cust.tax-id) .                      
                      WHEN "exp"           THEN cVarValue = IF cust.date-field[2] NE ? THEN string(cust.date-field[2]) ELSE "" .               
                      WHEN "trm-cd"        THEN cVarValue = string(cust.terms) .                       
                      WHEN "crd-hld-trm"   THEN cVarValue = if avail terms then terms.dscr else ""   . 
                      WHEN "date-ad"       THEN cVarValue = IF cust.date-field[1] NE ? THEN string(cust.date-field[1]) ELSE "" .
                      WHEN "disc"          THEN cVarValue = string(cust.disc,">>9.99%") .         
                      WHEN "mark"          THEN cVarValue = string(cust.markup) .       
                      WHEN "undr"          THEN cVarValue = string(cust.under-pct,">>9.99%") .    
                      WHEN "ovr"           THEN cVarValue = string(cust.over-pct,">>9.99%") .     
                      WHEN "emal"          THEN cVarValue = string(cust.email) .       
                      WHEN "lod-tag"       THEN cVarValue = string(cust.int-field[1]) . 
                      WHEN "fst-invdt"       THEN cVarValue = IF v-fst-invdt NE ? THEN string(v-fst-invdt) ELSE "" . 
                      WHEN "group"       THEN cVarValue = string(cust.spare-char-2)  .  
                      WHEN "last-ordered"  THEN cVarValue = IF dtLastOrdered = ? THEN "" ELSE string(dtLastOrdered)  . 
                 END CASE.                                                           
  
                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(j,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.                                                                       
         PUT UNFORMATTED cDisplay SKIP.
         IF tb_excel THEN DO:
              PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
         END.
      END.
   ELSE
      FOR EACH tt-contact,
          EACH tt-cust WHERE
               tt-cust.cust-no EQ tt-contact.cust-no
          BREAK BY tt-contact.attention
                BY tt-cust.seq:
          
       {custom/statusMsg.i " 'Processing Customer#  '  + string(tt-cust.cust-no) "}

          IF FIRST-OF(tt-contact.attention) THEN
          DO:
             IF v-page-break THEN
                PAGE.
             
          /*   DISPLAY tt-contact.attention LABEL "Contact" WITH FRAME contact-top.

             IF tb_excel THEN
             DO:
                IF v-page-break THEN
                   PUT STREAM excel UNFORMATTED SKIP(1).

                PUT STREAM excel UNFORMATTED
                    '"' REPLACE("Contact",',','","') '"' SKIP
                    '"' tt-contact.attention '",' SKIP(1)
                    '"' REPLACE("Cust #,Name",',','","') '"' SKIP.
             END. */
             find first sman
                 where sman.company eq cust.company
                 and sman.sman    eq cust.sman
                 no-lock no-error.
             
             find first terms where terms.t-code = cust.terms 
                 and terms.company = cocode
                 no-lock no-error.
             
             assign
                 v-fax-area-code = substr(cust.fax,1,3)
                 v-fax-phone     = substr(cust.fax,4,7).

       FOR EACH ar-inv FIELDS(inv-date) WHERE
           ar-inv.company EQ cust.company AND
           ar-inv.cust-no EQ cust.cust-no AND
           ar-inv.inv-date NE ?
           NO-LOCK
           USE-INDEX inv-date:
       
           IF ar-inv.inv-date GE begin_date AND
              ar-inv.inv-date LE END_date THEN
           DO:
           
            v-fst-invdt = STRING(ar-inv.inv-date) .
                     
           END.

           LEAVE.
       END.
        dtLastOrdered = ?.
        FOR EACH oe-ord
            WHERE oe-ord.company EQ cust.company
            AND oe-ord.cust-no eq cust.cust-no
            USE-INDEX cust NO-LOCK
            BY oe-ord.ord-date DESC:
            dtLastOrdered = oe-ord.ord-date.
            LEAVE.                                
        END.
        ASSIGN cDisplay = ""
                cTmpField = ""
                cVarValue = ""
                cExcelDisplay = ""
                cExcelVarValue = "".
         
         DO j = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(j,cSelectedList)), cFieldListToSelect).
                 CASE cTmpField: 
                      WHEN "cust"          THEN cVarValue = STRING(cust.cust-no) .
                      WHEN "name"          THEN cVarValue = string(cust.NAME) .
                      WHEN "add1"          THEN cVarValue = string(cust.addr[1]) .
                      WHEN "add2"          THEN cVarValue = string(cust.addr[2]) .
                      WHEN "city"          THEN cVarValue = string(cust.city) .
                      WHEN "stat"          THEN cVarValue = STRING(cust.state) .
                      WHEN "zip"           THEN cVarValue = string(cust.zip) .
                      WHEN "phn"           THEN cVarValue = if cust.phone ne "" then string("(" + cust.area-code + ") ") + STRING(cust.phone,"999-9999") else "" .
                      WHEN "fax"           THEN cVarValue = if v-fax-phone ne "" AND v-fax-phone NE ? THEN string("(" + v-fax-area-code + ") ") + STRING(v-fax-phone,"999-9999") else "" .
                      WHEN "typ"           THEN cVarValue = STRING(cust.TYPE) .                         
                      WHEN "actv"          THEN cVarValue = string(cust.active) .                       
                      WHEN "cont"          THEN cVarValue = string(cust.contact) .                      
                      WHEN "rep"           THEN cVarValue = string(cust.sman) .                         
                      WHEN "rep-nam"       THEN cVarValue = if available sman then sman.sname else "" . 
                      WHEN "ato-rep"       THEN cVarValue = string(cust.auto-reprice) .                
                      WHEN "cust-prc"      THEN cVarValue = string(cust.cust-level) .                  
                      WHEN "edi"           THEN cVarValue = string(cust.an-edi-cust) .                 
                      WHEN "bal-mth"       THEN cVarValue = STRING(cust.stat-type,"O/F")  .            
                      WHEN "mfg-dys"       THEN cVarValue = STRING(cust.ship-days) .                   
                      WHEN "palt"          THEN cVarValue = string(cust.pallet) .                      
                      WHEN "case"          THEN cVarValue = string(cust.case-bundle) .                 
                      WHEN "stmnt"         THEN cVarValue = string(cust.stat-grp) .                    
                      WHEN "terr"          THEN cVarValue = string(cust.terr) .                        
                      WHEN "cr-act"        THEN cVarValue = STRING(cust.cr-use) .                      
                      WHEN "ord-loc"       THEN cVarValue = string(cust.loc) .                         
                      WHEN "carr"          THEN cVarValue = string(cust.carrier) .                     
                      WHEN "delvry"        THEN cVarValue = string(cust.del-zone) .                    
                      WHEN "cr-rat"        THEN cVarValue = string(cust.cr-rating) .                   
                      WHEN "holdy"         THEN cVarValue = STRING(cust.cr-hold-invdays) .             
                      WHEN "crd-lim"       THEN cVarValue = string(cust.cr-lim,">>>,>>>,>>9.99") .                      
                      WHEN "partl"         THEN cVarValue = IF cust.ship-part NE ? THEN STRING(cust.ship-part,"YES/NO") ELSE "" .         
                      WHEN "fr-pay"        THEN cVarValue = string(cust.frt-pay) .                     
                      WHEN "ord-lim"       THEN cVarValue = string(cust.ord-lim,">>,>>>,>>9.99") .                     
                      WHEN "taxbl"         THEN cVarValue = STRING(cust.sort,"X")   .                  
                      WHEN "fob"           THEN cVarValue = string(cust.fob-code) .                    
                      WHEN "crd-hld"       THEN cVarValue = string(cust.cr-hold) .                     
                      WHEN "tax-cd"        THEN cVarValue = string(cust.tax-gr) .                      
                      WHEN "fin-chrg"      THEN cVarValue = string(cust.fin-chg) .                     
                      WHEN "inv-po"        THEN cVarValue = IF cust.inv-meth NE ? THEN STRING(cust.inv-meth,"YES/NO") ELSE "" .           
                      WHEN "tax-rsl"       THEN cVarValue = string(cust.tax-id) .                      
                      WHEN "exp"           THEN cVarValue = IF cust.date-field[2] NE ? THEN string(cust.date-field[2]) ELSE "" .               
                      WHEN "trm-cd"        THEN cVarValue = string(cust.terms) .                       
                      WHEN "crd-hld-trm"   THEN cVarValue = if avail terms then terms.dscr else ""   . 
                      WHEN "date-ad"       THEN cVarValue = IF cust.date-field[1] NE ? THEN string(cust.date-field[1]) ELSE "" .
                      WHEN "disc"          THEN cVarValue = string(cust.disc,">>9.99%") .         
                      WHEN "mark"          THEN cVarValue = string(cust.markup) .       
                      WHEN "undr"          THEN cVarValue = string(cust.under-pct,">>9.99%") .    
                      WHEN "ovr"           THEN cVarValue = string(cust.over-pct,">>9.99%") .     
                      WHEN "emal"          THEN cVarValue = string(cust.email) .       
                      WHEN "lod-tag"       THEN cVarValue = string(cust.int-field[1]) . 
                      WHEN "fst-invdt"       THEN cVarValue = IF v-fst-invdt NE ? THEN string(v-fst-invdt) ELSE "" . 
                      WHEN "group"       THEN cVarValue = string(cust.spare-char-2)  .   
                      WHEN "last-ordered"  THEN cVarValue = IF dtLastOrdered = ? THEN "" ELSE string(dtLastOrdered)  . 
                 END CASE.                                                            
  
                 cExcelVarValue = cVarValue.
                 cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(j,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                 cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
         END.                                                                       
         PUT UNFORMATTED cDisplay SKIP.
         IF tb_excel THEN DO:
              PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
         END.  
             

             v-page-break = YES.
          END.

       /*   DISPLAY tt-cust.cust-no LABEL "Cust #"
                  tt-cust.cust-name LABEL "Name"
                  SKIP.
          DOWN.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' tt-cust.cust-no '",' 
                 '"' tt-cust.cust-name '",' SKIP. */
      END.
