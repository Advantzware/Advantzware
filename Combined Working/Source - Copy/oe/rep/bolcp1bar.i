/* oe/rep/bolcpbc1.i */      
      put 
         "<FCourier New>"
            "<C1><R2><P12>"
               space(3) "Sold To:"  "<b>Ship Date: " AT 80 oe-bolh.bol-date "</b>" SKIP
               SPACE(7) v-comp-name              SKIP     
                SPACE(7) v-comp-addr[1]           SKIP
               SPACE(7) v-comp-addr[2]      "Packing List:" AT 38      SKIP                                                           
               SPACE(7) v-comp-addr3             SKIP(2)
          "<R9><C1>"
          space(3) "Ship To:"  SKIP      
          SPACE(7) v-ship-name  skip   
          SPACE(7) v-ship-addr[1] FORMAT "x(25)"    "PO#:"  AT 36  "Release#:" AT 59 SKIP
          SPACE(7) v-ship-addr[2]      SKIP
          SPACE(7) v-ship-addr3  SKIP(2)

          "<R16><C1>"
          SPACE(3) "Carrier Info: " SKIP
          SPACE(7) vcarrdesc FORMAT "x(30)"    "ASN#:" AT 42    SKIP
          SPACE(7) vzonedesc FORMAT "x(30)"     skip   
          SPACE(7) vterrdesc FORMAT "x(30)" SKIP(2)

          "<R4><C40>    <AT=,5.3><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        OE-BOLH.BOL-NO ">" 
          "<AT=,6>" OE-BOLH.BOL-NO

          "<R10><C40>   <AT=,4.2><FROM><AT=+.6,+1.6><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        v-po-no ">" 
          "<AT=,4.3>" v-po-no

          "<R10><C40> <AT=,7><FROM><AT=+.6,+1><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        v-rel-no ">" 
          "<AT=,7.2>" v-rel-no 

          "<R16><C40>    <AT=,5><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="  
                        oe-bolh.spare-int-5 ">" 
          "<AT=,5.7>" /*oe-ctrl.n-rec*/ oe-bolh.spare-int-5

          "<R9><C46><#3>" SKIP
              
                "<||5><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C7><FROM><R26><C7><LINE>" SKIP
                "<R24><C25><FROM><R26><C25><LINE>" SKIP
                "<R24><C50><FROM><R26><C50><LINE>" SKIP 
                /*"<R24><C58><FROM><R26><C58><LINE>" */
                "<R24><C60><FROM><R26><C60><LINE>" SKIP            
                "<R24><C70><FROM><R26><C70><LINE>"
            "<=5> Line     Part Number#     Item Description       Quantity  Quantity   Uom  "
            "<=5><R+1> No#                                              Shipped   Ordered  " SKIP(1)
            .
            v-printline = v-printline + 16.

