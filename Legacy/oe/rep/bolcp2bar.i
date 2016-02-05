/* ---------------------------------------------- oe/rep/bolcpbc2.i YSK     */
/* PRINT Consolidated Box                                                  */
/* -------------------------------------------------------------------------- */
assign
 v-tot-cases = 0
 v-tot-palls = 0.

FOR EACH tt-boll,
    first itemfg where itemfg.company eq cocode
                 and itemfg.i-no    eq tt-boll.i-no no-lock
    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq tt-boll.ord-no
        and oe-ordl.i-no    eq tt-boll.i-no
        and oe-ordl.line    eq tt-boll.line
      no-lock no-error.
 
  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq tt-boll.ord-no
      no-lock no-error.

    v-tot-pkgs = v-tot-pkgs + tt-boll.cases +
                     if tt-boll.partial gt 0 then 1 else 0. 
    
  if tt-boll.qty-case ne 0 and tt-boll.cases ne 0 then do:
     find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
     if not avail w2 then create w2.
     ASSIGN w2.cas-cnt = tt-boll.qty-case
            w2.cases   = w2.cases + tt-boll.cases.
  end.

  if tt-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  v-lines = 0.
  for each w2 break by w2.cases:
    v-lines = v-lines + 1.
  end.
  i = 0.

  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-LOCK NO-ERROR.
  ASSIGN lv-cases-tot = lv-cases-tot + tt-boll.cases
         lv-qty-tot = lv-qty-tot + tt-boll.qty
         lv-qcase-tot = lv-qcase-tot + tt-boll.qty-case
         lv-partial-tot = lv-partial-tot + tt-boll.partial
         lv-pal-tot = lv-pal-tot + tt-boll2.pallets
         v-job-no = fill(" ",6 - length(trim(tt-boll.job-no))) +
                    trim(tt-boll.job-no) + "-" +
                    trim(string(tt-boll.job-no2,"99"))
         tt-boll.printed = yes.
         lv-cases = lv-cases-tot.

  IF trim(v-job-no) = "-00" THEN v-job-no = "".

  
      IF v-printline >= 39 THEN DO:
          PAGE {1}.
          v-printline = 0.
          {oe/rep/bolcp1bar.i}
       END. 

/*ELSE DO:*/
   lv-qty-sum2 = lv-qty-sum2 + lv-qty-tot.
   IF AVAIL oe-ordl THEN do:
   PUT  
      oe-ordl.LINE  SPACE(9)
       "<AT=,0.9><FROM><AT=+.5,+1.7><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        oe-ordl.part-no ">" 
          "<AT=,0.9>" oe-ordl.part-no 

      "<R-3><C26>" oe-ordl.i-name  FORM "x(25)" 
      
       "<AT=,5.2><FROM><AT=+.5,+1><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        tt-boll.qty ">" 
          "<AT=,4.6>" tt-boll.qty 

      "<R-3><C61>" oe-ordl.qty  FORM ">>>>>>>>"
      /*oe-ordl.pr-uom FORM "x(5)"          */
     "<AT=,7.2><FROM><AT=+.5,+1><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                       oe-ordl.pr-uom ">" 
          "<AT=,7.3>" oe-ordl.pr-uom FORM  "x(5)" SKIP  .

   PUT /*"<FBook Antiqua>*/"<C1>_______________________________________________________________________________" SKIP(1) .
   
   
   v-printline = v-printline + 2.
   v-printline = v-printline + 2.
   END.

   
/*END. /* else - not summary */*/

  ASSIGN v-tot-palls = v-tot-palls + lv-pal-tot 
           v-tot-cases = v-tot-cases + v-tot-pkgs
           v-tot-pkgs = 0
           lv-pal-tot = 0.

    IF lv-bolfmt-int <> 1 THEN
        ASSIGN lv-cases-tot = 0
               lv-qty-tot = 0
               lv-qcase-tot = 0
               lv-partial-tot = 0
               lv-pal-tot = 0.

END. /* for each tt-boll */

/*
FOR EACH tt-boll,
    first itemfg where itemfg.company eq cocode
                 and itemfg.i-no    eq tt-boll.i-no no-lock
    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq tt-boll.ord-no
        and oe-ordl.i-no    eq tt-boll.i-no
        and oe-ordl.line    eq tt-boll.line
      no-lock no-error.
 
  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq tt-boll.ord-no
      no-lock no-error.

    v-tot-pkgs = v-tot-pkgs + tt-boll.cases +
                     if tt-boll.partial gt 0 then 1 else 0. 
    
  if tt-boll.qty-case ne 0 and tt-boll.cases ne 0 then do:
     find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
     if not avail w2 then create w2.
     ASSIGN w2.cas-cnt = tt-boll.qty-case
            w2.cases   = w2.cases + tt-boll.cases.
  end.

  if tt-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  v-lines = 0.
  for each w2 break by w2.cases:
    v-lines = v-lines + 1.
  end.
  i = 0.

  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-LOCK NO-ERROR.
  ASSIGN lv-cases-tot = lv-cases-tot + tt-boll.cases
         lv-qty-tot = lv-qty-tot + tt-boll.qty
         lv-qcase-tot = lv-qcase-tot + tt-boll.qty-case
         lv-partial-tot = lv-partial-tot + tt-boll.partial
         lv-pal-tot = lv-pal-tot + tt-boll2.pallets
         v-job-no = fill(" ",6 - length(trim(tt-boll.job-no))) +
                    trim(tt-boll.job-no) + "-" +
                    trim(string(tt-boll.job-no2,"99"))
         tt-boll.printed = yes.
         lv-cases = lv-cases-tot.

  IF trim(v-job-no) = "-00" THEN v-job-no = "".

  

/*ELSE DO:*/
   lv-qty-sum2 = lv-qty-sum2 + lv-qty-tot.
   PAGE {1} .
  
   put 
       "<FCourier New>"
       "<R7><C24><P16><b>"
        "Form:"   
        v-comp-name            SKIP     
       "<C31>" v-comp-addr[1]          SKIP
       "<C31>" v-comp-addr[2]          SKIP                                                           
       "<C31>" v-comp-addr3            SKIP .

     put 
         "<FCourier New>"
         "<||5><R12><C5><P16><#5><FROM><R52><C80><RECT>" SKIP    
                "<R22><C5><FROM><R22><C80><LINE>" SKIP
                "<R32><C5><FROM><R32><C80><LINE>" SKIP
                "<R42><C5><FROM><R42><C80><LINE>" SKIP 
                 .
   PUT  
       "<R15><C20> Part Number:"
       "<AT=,4><FROM><AT=+.7,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        oe-ordl.part-no ">" 
          "<AT=,4>" oe-ordl.part-no 

      "<R24> <C24> Item Description: " SKIP(1)
       "<R26><C18> <P18>" oe-ordl.i-name  FORM "x(25)" 
      
       "<R34><C20> Qty <AT=,4><FROM><AT=+.6,+1.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        tt-boll.qty ">" 
          "<AT=,3.5>" tt-boll.qty 

     
      /*oe-ordl.pr-uom FORM "x(5)"          */
     "<R44><C20> UOM <AT=,4><FROM><AT=+.6,+1.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                       oe-ordl.pr-uom ">" 
          "<AT=,4.5>" oe-ordl.pr-uom FORM  "x(5)"  "</b><P12>" SKIP  .

   

   v-printline = v-printline + 2.
   v-printline = v-printline + 1.

   IF v-printline >= 39 THEN DO:
      PAGE {1}.
      v-printline = 0.
  END.
/*END. /* else - not summary */*/

  

END. /* for each tt-boll */                        */




/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */



