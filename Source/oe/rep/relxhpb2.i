/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.              */
/* oe/rep/relxhpb2.i */   
   
   PUT "<FArial>" SKIP
           "<P14><C+35><B>Pick Ticket</B> " SKIP
            "<C1><#1><R+5><C+25>" /*<IMAGE#1=" ls-full-img1*/ SKIP /* pacific package */ 
            "<=1>" SKIP
           /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
           */
           "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
           "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
           "<P10></B>"
           "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"        
           "<P10><=1><R+3>"
            v-comp-add1 AT 8 SKIP
            v-comp-add2 AT 8  SKIP
            v-comp-add3 AT 8 SKIP
            v-comp-add4 AT 8 SKIP
            v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
            lv-email AT 8 SKIP(1)
                  "<FCourier New>"
                  "Sold To:" SPACE(30) "Ship To:"  SKIP
                  SPACE(5) cust.name shipto.ship-name AT 45 skip
                  SPACE(5) cust.addr[1] shipto.ship-addr[1] AT 45 SKIP.
      IF cust.addr[2] <> "" OR shipto.ship-addr[2] <> "" THEN
                  PUT SPACE(5) cust.addr[2] shipto.ship-addr[2] AT 45 SKIP
                  SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                           shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP.
      ELSE PUT SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                        shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP(1).

      PUT
           "<R4><C50><#3>" SKIP
           "<FArial><P14><=#3><P12>" SKIP
                   "<=#3><B>Ticket #: "
                   "<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                   STRING(oe-relh.release#) + ">" FORM "X(100)" "</B><P10>"
                   "<AT=,6.5>" oe-relh.release#        SKIP(1)
                   "<=#3><R+5>Release Date:" v-ticket-date   FORM "99/99/9999" " " STRING(oe-relh.upd-time,"hh:mm am")   SKIP
                   "<=#3><R+6>Ship Date:" oe-relh.rel-date        SKIP
                   "<=#3><R+7>CSR: " v-csr SKIP
                    SKIP     
                   "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                   /*"<R19><C35><FROM><R23><C35><LINE>" SKIP */
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>        Pallets                    Weight                    FOB                            Ship Via                                           Freight Terms" SKIP
                   "<FCourier New><=4><R+3> " v-pallets SPACE(10) v-weight FORM "->>>>>9" space(9) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C8.3><FROM><R26><C8.3><LINE>" SKIP
                   "<R24><C20><FROM><R26><C20><LINE>" SKIP
                   "<R24><C26><FROM><R26><C26><LINE>" SKIP
                   "<R24><C37><FROM><R26><C37><LINE>" SKIP                   
                   /*"<R24><C57><FROM><R26><C57><LINE>" SKIP*/
                   "<R24><C64><FROM><R26><C64><LINE>" SKIP 
                   "<R24><C72><FROM><R26><C72><LINE>" SKIP                                                                                                     
               "<FArial><=5><R+1>  Order#             PO#                   Bin#              FG#                                         Description                           Order Qty   Release Qty" SKIP(1)
               "<FCourier New>"          
           .
               v-printline = v-printline + 16.

