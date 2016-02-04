/*po/po-valy2.i*/
          /*IF po-ordl.setup EQ 0 THEN*/ v-po-cost = po-ordl.cost.

          /*ELSE DO:
            ASSIGN v-basis-w = 0
                   v-dep     = 0.

            IF po-ordl.item-type THEN
            FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                              AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
              ASSIGN v-basis-w = item.basis-w
                     v-dep     = item.s-dep.
          
            v-qty = po-ordl.ord-qty.

            IF po-ordl.pr-qty-uom NE po-ordl.pr-uom THEN
              RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.pr-uom,
                                      v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                                      v-qty, OUTPUT v-qty).

            v-po-cost = po-ordl.cost * v-qty.

            v-po-cost = (v-po-cost - po-ordl.setup) /
                        (v-po-cost / po-ordl.cost).
          END.*/

          vs-ord-qty = IF INDEX(STRING(po-ordl.ord-qty),".") > 0 THEN
                           SUBSTRING(STRING(po-ordl.ord-qty),1,INDEX(STRING(po-ordl.ord-qty),".") - 1) + "." +
                           SUBSTRING(STRING(po-ordl.ord-qty),INDEX(STRING(po-ordl.ord-qty),".") + 1)
                        ELSE STRING(po-ordl.ord-qty).
          vs-ord-qty = IF length(trim(vs-ord-qty)) < 13 THEN 
                        FILL(" ",13 - length(TRIM(vs-ord-qty))) + TRIM(vs-ord-qty)
               ELSE vs-ord-qty.

          PUT  v-LINE /*po-ordl.line*/ FORM ">>>"
               /*po-ordl.ord-qty FORM "->>>>,>>>,>>9"  SPACE(3)*/ /* no qty display space(16) */
               vs-ord-qty FORM "x(13)" SPACE(3)
               po-ordl.pr-qty-uom SPACE(1)
               po-ordl.i-no FORM "x(30)" SPACE(1)
               v-adder[1]
               v-job-no FORM "x(9)" AT 79 SPACE(1)
               v-po-cost FORM "->>>9.99<<" SPACE(1)
               po-ordl.pr-uom
               po-ordl.t-cost FORM "$>>>,>>9.99" SKIP. 

           PUT po-ordl.i-name AT 25 SPACE(1) /*v-vend-item FORM "x(15)" space(1)*/
               v-adder[2] SPACE(1)
               "Setup:" AT 82 po-ordl.setup
               v-change-dscr AT 107  SKIP.
           v-printline = v-printline + 2.
           
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
           ELSE DO:
              IF (AVAIL ITEM AND ITEM.r-wid > 0) or
                   (AVAIL ef AND ef.roll) THEN  /* no length for Roll*/
                   put "W: " AT 25 v-wid SPACE(22).
              ELSE put "W: " AT 25 v-wid SPACE(2) "L: " v-len SPACE(11).

              PUT v-adder[3].
           END.

           PUT " MSF: " + TRIM(STRING(v-msf,">>9.999")) FORMAT "x(13)".
            
           {po/poprints.i}

               v-printline = v-printline + 1.

               if not v-test-scr then
                 put skip
                     space(24)
                     "Score: "
                     len-score format "x(80)".
          
               else
               if dec(trim(len-score)) ne v-wid then
                 put skip
                     space(24)
                     "Score: "
                     len-score format "x(80)".

               ELSE v-printline = v-printline - 1.
             end.
             END.
           end.

           assign v-printline = v-printline + 1.
           put skip(1).
           assign v-printline = v-printline + 1.
            /* calc total sq feet */
           
           IF v-printline > 47 THEN DO:         
               PUT "<R63><C70>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
               PUT "<FBook Antiqua>"
               /*    "<R56><C1><From><R56><C35><LINE>"
                  "<R55><C1>X"
                  "<R56><C5>Authorized Signature" */
                  "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
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
                   /*    "<R56><C1><From><R56><C35><LINE>"
                       "<R55><C1>X"
                       "<R56><C5>Authorized Signature" */
                       "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
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
                     /*    "<R56><C1><From><R56><C35><LINE>"
                       "<R55><C1>X"
                       "<R56><C5>Authorized Signature" */
                        "<R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
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
