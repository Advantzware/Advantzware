 /* oe/rep/bolxprn2.i  */  
 
 RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

 PUT "<C1><R2><#1><R+7><C+65><IMAGE#1=" ls-full-img1 SKIP .
        
    PUT "<R3><C45> 2620 Centennial Road Suite S <C45><R3>" SKIP
        "<R4><C47>         Toledo, Ohio 43617  <C47><R4>" SKIP
        "<R5><C47>        Phone: 419-842-0309 <C47><R5>" SKIP
        "<R6><C47>          Fax: 419-842-0708  <C47><R6>" SKIP(3) .

/*      PUT    "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
          "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
          "<P10></B>"

          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
         "<P10><=1><R+3>"  
         v-comp-add1 AT 8 SKIP
         v-comp-add2 AT 8  SKIP
         v-comp-add3 AT 8 SKIP
         v-comp-add4 AT 8 SKIP
         v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
         lv-email AT 8 SKIP(1) */
    PUT "<P18><B> BOL #   <C7>" v-bol-no "<C7></B><P12>" SKIP(1)
        "<FCourier New>"
         "Order Date:" SPACE(2) v-orddate "<C50>Ship Date:<C50>" SPACE(12) v-ship-date SKIP(1)
        "Sales Order"  "<C50>Customer<C50>"  SKIP
        "Number:"  space(2) v-sale-num "<C50>Contact:<C50>" SPACE(10) v-cust-contact SKIP(1)
         "Purchase"  "<C50>Customer<C50>"  SKIP
        "Order:"  SPACE(2) v-po-num "<C50>Account:<C50>" SPACE(10) v-cust-account SKIP(1).
       
    PUT
               "<FCourier New>"
               "Ship To:" SPACE(35) "Bill To:"  SKIP
               SPACE(5) v-ship-name v-comp-name  AT 50 skip
               SPACE(5) v-ship-addr[1] v-comp-addr[1]  AT 50 SKIP
             SPACE(5) v-ship-addr[2] v-comp-addr[2]  AT 50 SKIP
               SPACE(5) v-ship-addr3 v-comp-addr3  AT 50 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
           /*     "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>Contact: " v-shipto-contact SKIP
                "<=#3><R+4>Phone: " v-phone FORM "x(15)"  SKIP     */
                "<|10><R26><C1><#4><FROM><R30><C81><RECT>" SKIP
             /*   "<R21><C1><FROM><R21><C81><LINE>" SKIP    */
                "<R26><C12><FROM><R30><C12><LINE>" SKIP
              /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                "<R19><C34><FROM><R23><C34><LINE>" SKIP */
                "<R26><C39><FROM><R30><C39><LINE>" SKIP
                "<R26><C46><FROM><R30><C46><LINE>" SKIP
                "<R26><C55><FROM><R30><C55><LINE>" SKIP
                "<R26><C62><FROM><R30><C62><LINE>" SKIP
                "<R26><C72><FROM><R30><C72><LINE>" SKIP
                /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
                "<FArial><=4><R+1>    Part #                    Description                                                  Unit Type        Order            Unit             Ship               Back Order" SKIP .
              PUT   space(125) "Quantity"  space(8) "Count" SPACE(8) "Quantity" SPACE(11) "Quantity" SKIP(2)
                
            /*     "<|10><R23><C1><#4><FROM><R29><C81><RECT>" SKIP 
                  "<|10><R25><C1><#4><FROM><R27><C81><RECT>" SKIP 
                  "<R23><C12><FROM><R29><C12><LINE>" SKIP
                 "<R23><C39><FROM><R29><C39><LINE>" SKIP
                "<R23><C48><FROM><R29><C48><LINE>" SKIP
                "<R23><C57><FROM><R29><C57><LINE>" SKIP
                "<R23><C66><FROM><R29><C66><LINE>" SKIP */.
   
  /* IF lv-bolfmt-int = 1 THEN PUT "<FArial><=5><R+1> Part#/Order#                       PO#                  Finished Good#              Description                         Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
                       ELSE  PUT "<FArial><=5><R+1> Part#                        PO#                            Finished Good#        Description                                Units       Qty/Unit   P/C    Weight  <FCourier New>" SKIP(1).
            . */

            v-printline = v-printline + 26.
