/* po/po-metro.i */  
  PUT
         "<FCourier New>"   SKIP
           "<P14><C+47><B>Purchase Order</B> <P10>" v-change-ord SKIP
      /* "<C4><R2><#1><R+8><C+45>" small */
         "<C4><R1><#1><R+10><C+45>"    /* larger */
         "<IMAGE#1=" ls-full-img1  SKIP     
    
         "Purchase Order To:" AT 7  "Ship To:" AT 55 SKIP(1)
         SPACE(6) vend.name v-sname AT 55 skip
         SPACE(6) vend.add1 v-saddr[1] AT 55 SKIP
         SPACE(6) vend.add2 v-saddr[2] AT 55 SKIP
         SPACE(6) vend.city + " " + vend.state + " " + 
                  (IF length(vend.zip) > 5 THEN string(vend.zip,"xxxxx-xxxx")
                  ELSE vend.zip)     FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 55 SKIP(1)
         SPACE(6) "Phone:" vend.area-code vend.phone SKIP
         SPACE(6) "Fax:  " vend.fax-area vend.fax SKIP
         
       "<R4><C50><#3>" SKIP
       "<P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
          "<=#3><R+1><C-2><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+2><C-2>Date: " po-ord.po-date        SKIP
          "<=#3><R+3><C-2>Changed Date: " po-ord.po-change-date SKIP
          "<=3><R+4><C-2>Date Required: " po-ord.due-date SKIP.
            /*
          WITH  FRAME po-top NO-BOX NO-LABEL STREAM-IO.

      VIEW FRAME po-top.
              */
      v-printline = v-printline + 15.    
      
      PUT "<||3><R22><C1><#4><FROM><R26><C80><RECT>" SKIP
      "<R24><C1><FROM><R24><C80><LINE>" SKIP    
      "<R22><C8><FROM><R26><C8><LINE>" SKIP
      "<R22><C25><FROM><R26><C25><LINE>" SKIP
      /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
      "<R22><C46><FROM><R26><C46><LINE>" SKIP
      "<R22><C51><FROM><R26><C51><LINE>" SKIP
      "<R22><C72><FROM><R26><C72><LINE>" SKIP
      .
      PUT "<=4><R+1>  Buyer        Contact                 Terms           FOB          Ship Via           Freight" SKIP
          "<=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(20)" terms.dscr FORM "x(23)" SPACE(1) po-ord.fob-code space(1) carrier.dscr FORM "x(24)" space(1) v-freight-dscr
          .
      
      PUT "<||3><R27><C1><#5><FROM><R29><C80><RECT>" SKIP    
             "<R27><C6><FROM><R29><C6><LINE>" SKIP
             "<R27><C15.5><FROM><R29><C15.5><LINE>" SKIP
             "<R27><C19><FROM><R29><C19><LINE>" SKIP       
             "<R27><C48><FROM><R29><C48><LINE>" SKIP
             "<R27><C56><FROM><R29><C56><LINE>" SKIP
             "<R27><C68><FROM><R29><C68><LINE>" 
             "<R27><C72><FROM><R29><C72><LINE>" SKIP
             .

      PUT "<=5><R+1> Line    Quantity UOM    Our Item Code/Name/Description      Job#          Cost  UOM  Ext Cost" SKIP(1).
     /* PUT "<FMS Mincho>"          .*/
      v-printline = v-printline + 8.
