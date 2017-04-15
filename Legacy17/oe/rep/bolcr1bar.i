/* oe/rep/bolcpbc1.i */      
      put 
         "<FCourier New>"
          "<C34><R3><P13><B>PACKING LIST</B>"
          
          "<R+2><C3><P11><B>Carrier:</B> " vcarrdesc FORMAT "x(30)"

          "<R+2><C3>"
          "<C6>" "<B>Ship From:" "<C55>" "Deliver To:</B>"  SKIP      
          "<C6>" v-ship-name  "<C55>" v-comp-name skip   
          "<C6>" v-ship-addr[1] FORMAT "x(25)" "<C55>" v-comp-addr[1]  SKIP
          "<C6>" v-ship-addr[2] "<C55>"  v-comp-addr[2]   SKIP
          "<C6>" v-ship-addr3  "<C55>" v-comp-addr3 SKIP

          "<R+2><C3>Reference: " oe-ord.cc-auth FORMAT "x(30)"
          "<C60>Ref/PO: "  oe-ord.po-no         SKIP(.3)
          "<C60>Seal: "  oe-bolh.airway-bill         SKIP(.3)
          "<C60>Trailer: " oe-bolh.trailer          SKIP(.3)
          "<C3>Ship Date: " oe-ord.last-date
          "<C32>Order Date: "  oe-ord.ord-date         
          "<C60>Appt.: "  appt-time        SKIP


         /*"<R16><C1>"
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
          "<AT=,5.7>" /*oe-ctrl.n-rec*/ oe-bolh.spare-int-5 */

         /* "<R+2><C46><#3>" SKIP*/
              
                "<||5><R21><C1><#5><FROM><R22><C81><RECT>" SKIP    
                "<R21><C15><FROM><R22><C15><LINE>" SKIP
                "<R21><C33><FROM><R22><C33><LINE>" SKIP 
                "<R21><C61><FROM><R22><C61><LINE>" SKIP            
                
            "<=5><C3>Quantity<C21>PO#<C40>Description<C68>Lot#  " SKIP(1)
            .
            v-printline = v-printline + 16.

