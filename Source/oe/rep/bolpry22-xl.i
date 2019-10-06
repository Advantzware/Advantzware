 /* oe/rep/bolsth22.i  */  
 
   
  /************ fields for the header *****************
  /* Sold to information */
  
  v-comp-name
  v-comp-addr[1]
  v-comp-addr[2]
  v-comp-addr3 /* city, state zip */
  
  /* Ship To Information */
  v-ship-name  
  v-ship-addr[1] 
  v-ship-addr[2] 
  v-ship-addr3  /* City State Zip */
  v-phone-num    = cust.area-code + cust.phone.
  
  /* other */
  v-salesman
  v-fob
  v-terms
  v-frt-terms
  v-po-no
  oe-bolh.bol-no
  oe-bolh.bol-date
  oe-bolh.ship-date
  carrier.dscr
  *******************************************************/
 
  
  /* EXCEL AUTOMATION */
  
  /*ASSIGN v-cell = "R2C25".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-shpfrm . */
  
  /* Sold To */
  
 /* ASSIGN v-cell = "R7C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-id . */

  ASSIGN v-cell = "R7C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-name .
  
  /*
  ASSIGN v-cell = "R9C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-custcontact .
  */
  
  ASSIGN v-cell = "R8C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-addr[1] .
  
  ASSIGN v-cell = "R9C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-addr[2] .
  
  ASSIGN v-cell = "R10C1".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-city .
  
  ASSIGN v-cell = "R10C11".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-state .

  ASSIGN v-cell = "R10C13".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-comp-zip .
  
  /* Ship To */
  
  /* Ship To Phone */

 /* ASSIGN v-cell = "R7C18".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-id .*/

  ASSIGN v-cell = "R7C28".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-phone-num .  

  ASSIGN v-cell = "R7C18".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-name .
  
  ASSIGN v-cell = "R8C18".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-addr[1] .
  
  ASSIGN v-cell = "R9C18".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-addr[2] .
  

  ASSIGN v-cell = "R10C18".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-city .
  
  ASSIGN v-cell = "R10C28".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-state .
    
  ASSIGN v-cell = "R10C30".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-ship-zip .
  
  /* Ship Contact */
  /*
  ASSIGN v-cell = "R3C8".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-shipcontact .  
  */
  
  
  /*
  v-salesman
  v-fob
  v-terms
  v-frt-terms
  v-po-no
  oe-bolh.bol-no
  oe-bolh.bol-date
  oe-bolh.ship-date
  carrier.dscr
  */
  
  /* Receipt# */
  ASSIGN v-cell = "R5C34".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = oe-bolh.bol-no .    
  
  /* Contact */
  ASSIGN v-cell = "R8C34".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-shipcontact .    

  /* Shia Via */
    
  ASSIGN v-cell = "R11C34".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = carrier.dscr .  
  
  /* Order Date */ 
  ASSIGN v-cell = "R5C40".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = string(v-ord-date ).  

  /* Cust PO# */
  
  ASSIGN v-cell = "R8C40".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-po-no .  
  
  /* FRT Terms */
  ASSIGN v-cell = "R11C40".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-frt-terms .   

  /* Delivery Date */
  ASSIGN v-cell = "R5C46".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-rel-date /*string(oe-bolh.bol-date) */ . 
  
  /* PMT Terms */
  ASSIGN v-cell = "R8C46".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-terms . 
  
  
  /* Sales Person */
  ASSIGN v-cell = "R11C46".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-salesman . 
  
  /* FRT AC */
  /*
  ASSIGN v-cell = "R12C17".
  chExcelApplication:Goto(v-cell) NO-ERROR.
  ASSIGN chExcelApplication:ActiveCell:Value = v-frt-acct-num. 
  */  
  
   /******************************************* Commented Out **********************************************************
   
   put 
         "<FArial>"  SKIP
          "<C1><#1><P14><C+45><B>DELIVERY RECEIPT</B>     Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" SKIP
          /*"<C1><#1><R+5><C+25>" /*<IMAGE#1=" ls-full-img1 */ SKIP /* pacific package */             
          */
          "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"

          "<=1><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+2>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1)
               "<FCourier New>"
               space(5) "Sold To:"  "Ship To:" AT 59  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP(1)
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
                "<=#3><B>    #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
                 SKIP     
                "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Date                    FOB                                                                                   Carrier                                            Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(30) carrier.dscr v-frt-terms SKIP
                "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C57><FROM><R26><C57><LINE>" SKIP  
                "<R24><C61><FROM><R26><C61><LINE>" SKIP
                "<R24><C67><FROM><R26><C67><LINE>" SKIP            
                "<R24><C73><FROM><R26><C73><LINE>"  
                "<R24><C76><FROM><R26><C76><LINE>" SKIP
            "<FArial><=5><C74>P" 
            "<=5><R+1> Order Qty / Item#            PO# / Job#                        Description                                              Units    Count        Total     C    Weight <FCourier New>" SKIP(1)            
            .

            v-printline = v-printline + 16.
*************************************************************************************************************************/
