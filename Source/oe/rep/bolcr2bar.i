/* ---------------------------------------------- oe/rep/bolcrbc2.i YSK       */
/* PRINT Consolidated Box                                                     */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.              */
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
         v-job-no = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', tt-boll.job-no, tt-boll.job-no2)))
         tt-boll.printed = yes.
         lv-cases = lv-cases-tot.

  IF trim(v-job-no) = "-000" THEN v-job-no = "".

  
      IF v-printline >= 39 THEN DO:
          PAGE {1}.
          v-printline = 0.
          {oe/rep/bolcr1bar.i}
       END. 
   
/*ELSE DO:*/
   lv-qty-sum2 = lv-qty-sum2 + lv-qty-tot.
  /* IF AVAIL oe-ordl THEN do:*/
   IF LAST-OF(tt-boll.i-no) THEN DO:
   
   PUT  
      "<C1>" (IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0)  FORM ">>>>>>>>"  
      "<C15>" tt-boll.po-no
      "<C35>" (IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE "") FORM "x(25)"
      "<C65>" tt-boll.ord-no SKIP(.4) .

   IF tt-boll.po-no NE "" THEN
     PUT
      "<AT=,1.5><FROM><AT=+1,+1.4><BARCODE=128,TYPE=39,CHECKSUM=NONE,VALUE="  
                        tt-boll.po-no ">" 
          "<AT=,0.4>PO Number:  " tt-boll.po-no "<R-6>"  .
   
   IF AVAIL itemfg AND itemfg.part-no NE "" THEN
     PUT
      "<AT=,4.2><FROM><AT=+1,+1.6><BARCODE=128,TYPE=39,CHECKSUM=NONE,VALUE="  
                        itemfg.part-no ">" 
          "<AT=,3>Material No:  " itemfg.part-no  "<R-6>".

   IF tt-boll.ord-no NE 0 THEN
    PUT
     "<AT=,6.6><FROM><AT=+1,+1.3><BARCODE=128,TYPE=39,CHECKSUM=NONE,VALUE="  
                       tt-boll.ord-no ">" 
          "<AT=,5.8>Lot No:  " tt-boll.ord-no   .

   PUT SKIP(1).
   /*PUT /*"<FBook Antiqua>*/"<C1>_______________________________________________________________________________" SKIP(1) .*/
   PUT "<||5><C1><#5><FROM><C81><LINE>" SKIP(1) .
   
   
   v-printline = v-printline + 3.
   v-printline = v-printline + 3.
  /* END.*/

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



