/* oe/rep/relxppi2.i */   
   
   PUT "<FArial>" SKIP
           "<P14><C+40><B>Pick Ticket</B> " SKIP
           "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP
           SKIP(1)
           "<P10><FCourier New>"
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
                   /*"<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                   STRING(oe-relh.release#) + ">" FORM "X(100)" "</B><P10>"*/
                   oe-relh.release# SKIP(1) /*<AT=,6.5>**/
                   "<P10><=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999"     SKIP
                   "<=#3><R+6>Ship Date:" oe-relh.rel-date        SKIP
                    SKIP     
                   "</B><|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>         Cases                   Weight                    FOB              Ship Via                                                    Freight Terms" SKIP
                   "<FCourier New><=4><R+3>" v-cases SPACE(8) v-weight FORM "->>>>>9" space(9) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C7><FROM><R26><C7><LINE>" SKIP
                   "<R24><C18><FROM><R26><C18><LINE>" SKIP
                   "<R24><C24><FROM><R26><C24><LINE>" SKIP
                   "<R24><C35><FROM><R26><C35><LINE>" SKIP                   
                   "<R24><C64><FROM><R26><C64><LINE>" SKIP 
                   "<R24><C72><FROM><R26><C72><LINE>" SKIP                                                                                                     /*job#*/
               "<FArial><=5><R+1> Order#            PO#                Bin#         FG#/Part#               Description                                                      Order Qty     Released" SKIP(1)
               "<FCourier New>"          
           .
               v-printline = v-printline + 16.

