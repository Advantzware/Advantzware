 /* cec/quote/quohawl2.i */
 
     PUT "<C1><#1><R+6><C+35><IMAGE#1=" ls-full-img1 SKIP. 
     /*PUT "<C+25><#1>".
     PUT "<=1>" SKIP. 
     PUT "<C1><#1><R+6><C+35><IMAGE#2=" ls-full-img1 SKIP.  /* company image */ 
     */            
     PUT  "<C1><#2><Farial>"
         /*"<=2><R+4>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
         "<P10><=2><R-2>"
          v-comp-add1 AT 8 SKIP 
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP
          v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
          lv-email AT 8 SKIP(2)
         "<FCourier New>"
         "Client/Customer:" AT 4 space(32) "Livr" + CHR(233) + " " + CHR(224) + "/Ship To:" FORM "x(16)" shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
     /*
     IF lv-display-comp THEN
        PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            . 
     */       
     v-printline = v-printline + 15.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>"        SKIP
         "<R4><C62><FROM><R6><C62><LINE>"        SKIP
         "<R6><C65><FROM><R8><C65><LINE>" 
         .
    
   PUT "<FArial><P12><=#3>" /*<R-3>" "    Page#: " + string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" */ .
   IF NOT s-sep-page THEN
   PUT    "<=#3><R-2><C-8> <B>Soumission / Quotation#: " v-first-q-no "</B>" "     Page#: " + string(PAGE-NUM,">>9") + " of " +  "<#PAGES>" FORM "x(30)" .
   ELSE
       PUT    "<=#3><R-2><C-8> <B>Soumission / Quotation#: " v-first-q-no "</B>" "     Page#: " + string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg)  FORM "x(30)" .

     PUT  "<P10>" SKIP
   "<=#3>  Client/Customer#                    ATTN:"
   "<=#3><R+2> Tele.                                           Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

   /* same as request - no space between hd & line
   PUT "<R11><#4><R17><C80><RECT>" SKIP
   "<R13><C1><FROM><R13><C80><LINE>" SKIP
   "<R15><C1><FROM><R15><C80><LINE>" SKIP

   "<R11><C20><FROM><R15><C20><LINE>" SKIP
   "<R11><C30><FROM><R15><C30><LINE>" SKIP
   "<R11><C40><FROM><R15><C40><LINE>" SKIP
   "<R11><C50><FROM><R15><C55><LINE>" SKIP
   "<R11><C60><FROM><R15><C60><LINE>" SKIP
   "<R11><C70><FROM><R15><C70><LINE>" SKIP
   .
   */  

   PUT "<|10><R23><C1><#4><FROM><R27><C81><RECT>" SKIP
   "<R25><C1><FROM><R25><C81><LINE>"     SKIP
   "<R23><C11><FROM><R27><C11><LINE>"    SKIP
   "<R23><C22><FROM><R27><C22><LINE>"    
   "<R23><C38><FROM><R27><C38><LINE>"    
   "<R23><C53><FROM><R27><C53><LINE>" 
   "<R23><C71><FROM><R27><C71><LINE>" 
   .
   PUT "<FArial><=4><C72> Plus-Moins /"
       "<R+1><C1>         Date                 FOB                  Expediteur / Ship Via              Termes / Terms     Representant/Sales Person   Over-Under%" SKIP
   "<FCourier New><=4><R+3> " v-quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R28><C1><#5><FROM><R30><C81><RECT>" SKIP    
             "<R28><C7><FROM><R30><C7><LINE>"  SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP
             "<R28><C45><FROM><R30><C45><LINE>" SKIP
             "<R28><C56><FROM><R30><C56><LINE>" SKIP
             /*"<R28><C63><FROM><R30><C63><LINE>" SKIP*/
             "<R28><C72><FROM><R30><C72><LINE>" 
             .
   PUT "<FArial><=5><R+1> Est#/Qt#        Description                           Specifications                                  Qte / Qty                  Prix  /  Price           UM / UOM" SKIP(1).
   PUT "<FCourier New>".
