/*po/po-centx2.i*/

         ASSIGN
         v-ord-qty = po-ordl.ord-qty
         lv-format = IF CAN-DO("MSF,TON", po-ordl.pr-qty-uom) 
                      THEN "->>,>>>,>>9.99"
                      ELSE
                      IF CAN-DO("LF,EA", po-ordl.pr-qty-uom)
                      THEN "->>>,>>>,>>9"
                      ELSE "->>,>>>,>>9.9<<<<<".

        IF po-ordl.pr-qty-uom EQ "LF" THEN DO:
          {sys/inc/roundup.i v-ord-qty}
        END. 

          PUT  v-LINE FORM ">>>"
               STRING(v-ord-qty, lv-format) FORMAT "x(13)" SPACE(3)
               po-ordl.pr-qty-uom SPACE(1)
               po-ordl.i-no FORM "x(30)" SPACE(1)
               v-job-no FORM "x(9)" AT 80 SPACE(1)
               po-ordl.cost FORM "->>>9.99<<"
               po-ordl.pr-uom
               po-ordl.t-cost FORM "$>>>,>>9.99" SKIP. 

           PUT po-ordl.i-name AT 25 SPACE(1)
               v-change-dscr AT 107  SKIP.
           v-printline = v-printline + 2.
           
           IF v-itemDescription THEN
           DO:
              IF po-ordl.dscr[1] NE '' THEN
              DO:
                 RUN next-page-proc.
                 v-printline = v-printline + 1.
                 PUT po-ordl.dscr[1] format "x(50)" at 25 skip.
              END.

              IF po-ordl.dscr[2] NE '' THEN
              DO:
                 RUN next-page-proc.
                 v-printline = v-printline + 1.
                 PUT po-ordl.dscr[2] format "x(50)" at 25 skip.
              END.
           END.

           FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
     
           IF v-vend-item <> "" THEN DO:
              PUT trim(po-ord.vend) AT 25 "Item#: " v-vend-item SKIP.
              v-printline = v-printline + 1.
           END.
           /* check whether i-no is roll */
           FIND FIRST job-hdr WHERE job-hdr.company = po-ordl.company
                                AND job-hdr.job-no = po-ordl.job-no
                                AND job-hdr.job-no2 = po-ordl.job-no2 NO-LOCK NO-ERROR.
           IF AVAIL job-hdr THEN 
              FIND FIRST ef WHERE ef.company = po-ordl.company
                              AND ef.est-no = job-hdr.est-no
                              AND ef.form-no = po-ordl.s-num NO-LOCK NO-ERROR.
    
           RUN calc-cost (recid(po-ordl),OUTPUT v-cost,OUTPUT v-setup).            
           IF AVAIL ITEM AND ITEM.mat-type = "C" THEN
              put "W: " at 25 v-wid space(2) "L: " v-len SPACE(2) "D: "  ITEM.case-d FORM ">>>9.99<<" .
           ELSE IF (AVAIL ITEM AND ITEM.r-wid > 0) or
                   (AVAIL ef AND ef.roll) THEN  /* no length for Roll*/
                   put "W: " at 25 v-wid .
           ELSE put "W: " at 25 v-wid space(2) "L: " v-len  
                 "                          ".
           assign v-printline = v-printline + 1.
           put skip(1).
           assign v-printline = v-printline + 1.
            /* calc total sq feet */
           
           IF v-printline > 47 THEN DO:         
               PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
               PUT "<FBook Antiqua>"
                  "<R59><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                  " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                  " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                  SKIP.    
               PAGE.
               v-printline = 0.
               {po/po-centx.i}
            END.
            FOR EACH tt-text WHERE tt-text.TYPE = "Linenote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
                IF v-printline > 47 THEN DO:         
                   PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                   PUT "<FBook Antiqua>"
                       "<R59><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                       " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                       " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                       SKIP.    
                    PAGE.
                    v-printline = 0.
                    {po/po-centx.i}
                END.
                PUT tt-text.tt-text FORM "x(80)"   SKIP.
                  v-printline = v-printline + 1.
            END.
            /* === spec note print */
            IF v-print-sn THEN DO:
               ASSIGN v-tmp-lines = 0
                       v-inst-lines = 0
                       lv-item-rec = "".
        
               FOR EACH tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) BY tt-text.tt-line:
                  IF v-printline > 47 THEN DO:         
                     PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                     PUT "<FBook Antiqua>"
                        "<R59><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
                        " Acknowledge this order verifying price, freight terms, quantity, and delivery date." SKIP
                        " INVOICES WILL BE PAID ACCORDING TO THIS PURCHASE ORDER ONLY!" SKIP
                        SKIP.    
                      PAGE.
                      v-printline = 0.
                      {po/po-centx.i}
                  END.
                  PUT tt-text.tt-text FORM "x(80)"  SKIP.
                  v-printline = v-printline + 1.
               END.
             END.
