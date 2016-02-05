/* --------------------------------------------- oe/rep/bollprnt12.i 10/09 GDM */
/* N-K BOLFMT = Loylang - FORM for Loylang                                    */
/* -------------------------------------------------------------------------- */ 

IF NOT v-broker
  THEN PUT "<FArial>"  SKIP
           "<P14><R37><C+50><B>Bill Of Lading</B> "
           "<C1><R36><#1><C+3><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP(1)
           "<FCourier New><P10>"
           "Sold To:" SPACE(30) "Ship To:"  SKIP
           SPACE(5) v-comp-name    v-ship-name    AT 45 SKIP
           SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
           SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
           SPACE(5) v-comp-addr3   v-ship-addr3   AT 45 SKIP
           .

  ELSE PUT "<FArial><P14>"  SKIP
           "<R37><C+50><B>Bill Of Lading</B>"
           "<C1><R39><#1><C+4>"             
           "<=#1><R+1><C+6>" v-comp-name    
           "<=#1><R+2><C+6>" v-comp-addr[1] 
           "<=#1><R+3><C+6>" v-comp-addr[2] 
           "<=#1><R+4><C+6>" v-comp-addr3   
           "<FCourier New><P10><C1><R+5>"                            
           SPACE(38) "Ship To:"  SKIP            
           v-ship-name    AT 45 SKIP
           v-ship-addr[1] AT 45 SKIP
           v-ship-addr[2] AT 45 SKIP
           v-ship-addr3   AT 45 SKIP
           .

PUT  "<R40><C55><#3>" SKIP
     "<FArial><P14><=#3><P10>" SKIP
     "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
     "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
     "<=#3><R+3>Contact: " v-shipto-contact SKIP
     "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP.

PUT  
     "<|10><R51><C1><#5><FROM><R53><C81><RECT>" SKIP    
     "<R51><C9.5><FROM><R53><C9.5><LINE>" SKIP
     "<R51><C22.5><FROM><R53><C22.5><LINE>" SKIP
     "<R51><C34.5><FROM><R53><C34.5><LINE>" SKIP
     "<R51><C61><FROM><R53><C61><LINE>" SKIP  
     "<R51><C73><FROM><R53><C73><LINE>" SKIP
     "<R51><C76><FROM><R53><C76><LINE>" SKIP.
 
 PUT "<FArial><=5><R+1> Order#/Job#     PO# / Lot#              FG # / Cust Part            Item Name                                                Unit-Quantity       P/C  Weight  <FCourier New>" SKIP(1).
                                                                       
 v-printline = v-printline + 12.
 

 IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
   IF LAST(report.key-01) THEN
   if v-print-components THEN do:
      for each fg-set
          where fg-set.company eq cocode
            and fg-set.set-no  eq oe-boll.i-no
          no-lock,
          
          first xitemfg
          where xitemfg.company eq cocode
            and xitemfg.i-no    eq fg-set.part-no
          no-lock
          
          break by fg-set.set-no:
          
        {sys/inc/part-qty.i v-part-qty fg-set}
        
        put {1}
            xitemfg.part-no
            fg-set.part-no                  AT 33
            xitemfg.i-name                        FORMAT "x(22)"
            oe-boll.qty * v-part-qty        TO 80 FORMAT "->>>,>>9"
            skip.
      
     /*   v-printline = v-printline + 2.*/
      end. 
    END. 

    IF LAST-OF(report.key-02) THEN DO:
       
       i = 0.
       v-recnt = 0 .
 
       FOR EACH tt-w2 BREAK BY tt-w2.cases * tt-w2.cas-cnt DESC:
           v-recnt = v-recnt + 1.
       END.

       FOR EACH tt-w2 BREAK BY tt-w2.cases * tt-w2.cas-cnt DESC:
         i = i + 1.
         IF i eq 1 THEN ASSIGN v-job-po    = oe-boll.po-no                                                              
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-fgitem    = oe-boll.i-no
                               v-part-dscr = oe-ordl.i-name. 
         ELSE
         if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-job-po    = IF AVAIL reftable THEN reftable.code 
                                                               ELSE ""
                               v-fgitem    = oe-ordl.part-no.
         ELSE
         if i eq 3 then ASSIGN v-part-dscr = oe-ordl.part-dscr2
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".
         ELSE
         if i eq 4 then ASSIGN v-part-dscr = ""
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".  
   

         IF FIRST(tt-w2.cases * tt-w2.cas-cnt) THEN 
           PUT {1} oe-ordl.ord-no FORM ">>>>>9"
                   v-job-po FORM "x(15)" AT 11
                   v-fgitem  AT 27 
                   v-part-dscr FORMAT "x(30)" AT 42
                   tt-w2.cases    AT 72 FORMAT "->>>9" " @"
                   tt-w2.cas-cnt    FORM "->>>>9"
                   SKIP.
         ELSE PUT {1} 
                  v-job-var FORM "x(9)"
                  v-job-po  FORM "x(15)" AT 11
                  v-fgitem   AT 27
                  v-part-dscr FORMAT "x(30)" AT 42
                  tt-w2.cases  AT 72 FORMAT "->>>9" " @"
                  tt-w2.cas-cnt FORMAT "->>>>9" SKIP.

         v-printline = v-printline + 1.
         
         IF LAST(tt-w2.cases * tt-w2.cas-cnt) THEN DO:
           IF FIRST(tt-w2.cases * tt-w2.cas-cnt) THEN DO:
             PUT {1} 
                 v-job-var  FORMAT "x(9)"
                 IF AVAIL reftable 
                   THEN reftable.code ELSE "" FORMAT "x(15)" AT 11
                 oe-ordl.part-no AT 27
                 oe-ordl.part-dscr1 FORMAT "x(30)" AT 42
                 SKIP.
             v-printline = v-printline + 1.
           END.
      
           PUT {1}
               "================" AT 72 SKIP
               v-tot-pkgs AT 72 FORM "->>>9"  " ="
               v-ship-qty FORM "->>>>9" SPACE(4)
               oe-boll.p-c SPACE(1)
               v-weight  FORMAT ">>>>>9" SKIP.
      
           ASSIGN
              v-printline = v-printline + 2
              v-tot-pkgs  = 0.
      
           IF v-print-dept THEN
           DO:
              FIND FIRST job-hdr WHERE
                   job-hdr.company eq cocode AND
                   job-hdr.job-no  EQ oe-ordl.job-no AND
                   job-hdr.job-no2 EQ oe-ordl.job-no2
                   NO-LOCK NO-ERROR.

              IF AVAIL job-hdr THEN
              DO:
                 FIND FIRST job WHERE
                      job.company eq cocode AND
                      job.job     eq job-hdr.job AND
                      job.job-no  eq job-hdr.job-no AND
                      job.job-no2 eq job-hdr.job-no2
                      NO-LOCK NO-ERROR.

                 IF AVAIL job THEN
                 DO:
                     ASSIGN j = 0 .
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                       
                       
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                       
                        IF notes.note_text <> "" THEN
                           DO i = 1 TO v-tmp-lines:
                       
                              /*IF v-printline >= 38 THEN DO:
                                 v-printline = 0.
                                 PAGE {1}.
                                 {oe/rep/bollprnt12.i}
                              END.*/
                             
                             
                              PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                              v-printline = v-printline + 1.
                               j = j + 1 .
                                IF j >= 2 THEN LEAVE.
                           END.
                            IF j >= 2 THEN LEAVE.
                    END.
                    RELEASE job.
                 END.
                 RELEASE job-hdr.
              END.
           END.
         END.
         v-tot-cases = v-tot-cases + tt-w2.cases.
         DELETE tt-w2.
       END.
       v-printline = v-printline + 1.
       PUT "<FArial><P9> " 
             "<C1>"                          
             "__________________________________________________________________________________________________________________" SKIP(1) 
             "<C1>" "<B>  Signature of Receipt </B>"  SKIP
             "<C7>" "Customer ________________________________________                       Date _______________________________________"  SKIP(2)
            "<FCourier New><P10>"
    .   
   /*   PUT {1} SKIP(1).*/
      v-printline = v-printline + 5.
    END.
 END.
 ELSE DO:
     IF LAST(report.key-01) THEN
   if v-print-components THEN do:
      for each fg-set
          where fg-set.company eq cocode
            and fg-set.set-no  eq oe-boll.i-no
          no-lock,
          
          first xitemfg
          where xitemfg.company eq cocode
            and xitemfg.i-no    eq fg-set.part-no
          no-lock
          
          break by fg-set.set-no:
          
        {sys/inc/part-qty.i v-part-qty fg-set}
        
        put {1}
            xitemfg.part-no
            fg-set.part-no                  AT 33
            xitemfg.i-name                        FORMAT "x(22)"
            oe-boll.qty * v-part-qty        TO 80 FORMAT "->>>,>>9"
            skip.
      
     /*   v-printline = v-printline + 2.*/
      end. 
    END. 

    IF LAST-OF(report.key-02) THEN DO:
       
       i = 0.
       v-recnt = 0 .
 
       FOR EACH tt-w2 BREAK BY tt-w2.cases * tt-w2.cas-cnt DESC:
           v-recnt = v-recnt + 1.
       END.

       FOR EACH tt-w2 BREAK BY tt-w2.cases * tt-w2.cas-cnt DESC:
         i = i + 1.
         IF i eq 1 THEN ASSIGN v-job-po    = oe-boll.po-no                                                              
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-fgitem    = oe-boll.i-no
                               v-part-dscr = oe-ordl.i-name. 
         ELSE
         if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1
                               v-job-var   = if oe-boll.job-no eq "" then "" else
                                             (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))
                               v-job-po    = IF AVAIL reftable THEN reftable.code 
                                                               ELSE ""
                               v-fgitem    = oe-ordl.part-no.
         ELSE
         if i eq 3 then ASSIGN v-part-dscr = oe-ordl.part-dscr2
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".
         ELSE
         if i eq 4 then ASSIGN v-part-dscr = ""
                               v-job-var = ""
                               v-job-po  = ""
                               v-fgitem  = "".  
   

         IF FIRST(tt-w2.cases * tt-w2.cas-cnt) THEN 
           PUT {1} oe-ordl.ord-no FORM ">>>>>9"
                   v-job-po FORM "x(15)" AT 11
                   v-fgitem  AT 27 
                   v-part-dscr FORMAT "x(30)" AT 42
                   tt-w2.cases    AT 72 FORMAT "->>>9" " @"
                   tt-w2.cas-cnt    FORM "->>>>9"
                   SKIP.
         ELSE PUT {1} 
                  v-job-var FORM "x(9)"
                  v-job-po  FORM "x(15)" AT 11
                  v-fgitem   AT 27
                  v-part-dscr FORMAT "x(30)" AT 42
                  tt-w2.cases  AT 72 FORMAT "->>>9" " @"
                  tt-w2.cas-cnt FORMAT "->>>>9" SKIP.

         v-printline = v-printline + 1.
         
         IF LAST(tt-w2.cases * tt-w2.cas-cnt) THEN DO:
           IF FIRST(tt-w2.cases * tt-w2.cas-cnt) THEN DO:
             PUT {1} 
                 v-job-var  FORMAT "x(9)"
                 IF AVAIL reftable 
                   THEN reftable.code ELSE "" FORMAT "x(15)" AT 11
                 oe-ordl.part-no AT 27
                 oe-ordl.part-dscr1 FORMAT "x(30)" AT 42
                 SKIP.
             v-printline = v-printline + 1.
           END.
      
           PUT {1}
               "================" AT 72 SKIP
               v-tot-pkgs AT 72 FORM "->>>9"  " ="
               v-ship-qty FORM "->>>>9" SPACE(4)
               oe-boll.p-c SPACE(1)
               v-weight  FORMAT ">>>>>9" SKIP.
      
           ASSIGN
              v-printline = v-printline + 2
              v-tot-pkgs  = 0.
      
           IF v-print-dept THEN
           DO:
              FIND FIRST job-hdr WHERE
                   job-hdr.company eq cocode AND
                   job-hdr.job-no  EQ oe-ordl.job-no AND
                   job-hdr.job-no2 EQ oe-ordl.job-no2
                   NO-LOCK NO-ERROR.

              IF AVAIL job-hdr THEN
              DO:
                 FIND FIRST job WHERE
                      job.company eq cocode AND
                      job.job     eq job-hdr.job AND
                      job.job-no  eq job-hdr.job-no AND
                      job.job-no2 eq job-hdr.job-no2
                      NO-LOCK NO-ERROR.

                 IF AVAIL job THEN
                 DO:
                     ASSIGN j = 0 .
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                       
                       
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                       
                        IF notes.note_text <> "" THEN
                           DO i = 1 TO v-tmp-lines:
                       
                              /*IF v-printline >= 38 THEN DO:
                                 v-printline = 0.
                                 PAGE {1}.
                                 {oe/rep/bollprnt12.i}
                              END.*/
                             
                             
                              PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                              v-printline = v-printline + 1.
                               j = j + 1 .
                                IF j >= 2 THEN LEAVE.
                           END.
                            IF j >= 2 THEN LEAVE.
                    END.
                    RELEASE job.
                 END.
                 RELEASE job-hdr.
              END.
           END.
         END.
         v-tot-cases = v-tot-cases + tt-w2.cases.
         DELETE tt-w2.
       END.
       v-printline = v-printline + 1.
       PUT "<FArial><P9> " 
             "<C1>"                          
             "__________________________________________________________________________________________________________________" SKIP(1) 
             "<C1>" "<B>  Signature of Receipt </B>"  SKIP
             "<C7>" "Customer ________________________________________                       Date _______________________________________"  SKIP(2)
            "<FCourier New><P10>"
    .   
   /*   PUT {1} SKIP(1).*/
      v-printline = v-printline + 5.
    END.
 END.

