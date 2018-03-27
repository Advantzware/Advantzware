/* oe/rep/relindc2.i */   
   
   IF NOT s-print-pricing THEN
      PUT "<FArial><P14><C+35><B>Pick Ticket".
   ELSE
      PUT "<FArial><P14><C+30><B>Release Acknowledgement".
   PUT "</B> " SKIP(1)
            "<C1><#1><R+8><C+45><IMAGE#1=" ls-full-img1 SKIP 
            "<=1><R+9>" SKIP
                  "<FCourier New>"
                  "<P12>Sold To:" SPACE(30) "Ship To:"  SKIP
                  SPACE(5) cust.name shipto.ship-name AT 45 SKIP
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
                   "<UNITS=INCHES><AT=.48,6><FROM><AT=+.55,+2><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" +
                   STRING(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
                   "<AT=,6.9>" oe-relh.release# FORM "->>>>>>9" SKIP(1)
                   "<=#3><R+3>Print Date:" v-ticket-date   FORM "99/99/9999"     SKIP
                   "<=#3><R+4>Due Date: " oe-relh.rel-date        SKIP
                   "<=#3><R+6>PO#: " IF AVAIL tt-rell THEN tt-rell.po-no  ELSE "" FORMAT "x(15)"  SKIP
                    SKIP     
                   "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                   /*"<R19><C35><FROM><R23><C35><LINE>" SKIP */
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>        CASES                    WEIGHT                    FOB                         SHIP VIA                                             FREIGHT TERMS" SKIP
                   "<FCourier New><=4><R+3>     " lv-tot-cases /*v-pallets*/ SPACE(6) v-weight SPACE(10) oe-ord.fob-code SPACE(5) v-carrier SPACE(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C14.5><FROM><R26><C14.5><LINE>" SKIP
                   /*"<R24><C18><FROM><R26><C18><LINE>" SKIP*/
                   /*"<R24><C24><FROM><R26><C24><LINE>" SKIP*/
                   "<R24><C35><FROM><R26><C35><LINE>" SKIP                   
                   /*"<R24><C57><FROM><R26><C57><LINE>" SKIP*/
                   "<R24><C64><FROM><R26><C64><LINE>" SKIP 
                   "<R24><C72><FROM><R26><C72><LINE>" SKIP                                                                                                     /*job#*/
               "<FArial><=5><C64.5>"
               "<=5><R+1>       Part#/Job#                     FG ITEM #                                            DESCRIPTION                                     PACK          CASES" SKIP
               "<FCourier New>"          
           .
               v-printline = v-printline + 15.

