/* oe/rep/relpaci2.i */

PUT "<FArial>" SKIP
           "<P14><C+35><B>Pick Ticket</B> " SKIP
            "<C1><#1><R+5><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
            "<=1>" SKIP
           /* "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
           */ "<P10><=1><R+4>"
            v-comp-add1 AT 8 SKIP
            v-comp-add2 AT 8  SKIP
            v-comp-add3 AT 8 SKIP
            v-comp-add4 AT 8 SKIP
            "www.actionbox.ca" AT 8 SKIP(1)
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
                   "<=#3><B>Ticket #: " oe-relh.release# FORM ">>>>>>9" "</B><P10>" SKIP(1)
                   "<=#3><R+2>Print Date:" v-ticket-date        SKIP
                   "<=#3><R+3>Ship Date:" oe-relh.rel-date        SKIP
                    SKIP     
                   "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                   /*"<R19><C35><FROM><R23><C35><LINE>" SKIP */
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>    Pallets/Bags              Weight                    FOB               Ship Via                                                    Freight Terms" SKIP
                   "<FCourier New><=4><R+3> " v-pallets SPACE(10) v-weight space(10) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C13><FROM><R26><C13><LINE>" SKIP
                   "<R24><C16><FROM><R26><C16><LINE>" SKIP
                   "<R24><C21><FROM><R26><C21><LINE>" SKIP
                   "<R24><C25><FROM><R26><C25><LINE>" SKIP
                   "<R24><C33><FROM><R26><C33><LINE>" SKIP                   
                   "<R24><C45><FROM><R26><C45><LINE>" SKIP
                   "<R24><C64><FROM><R26><C64><LINE>" SKIP 
                   "<R24><C72><FROM><R26><C72><LINE>" SKIP                                                                                                     /*job#*/
               "<FArial><=5><R+1> Order#/PO#           UNTCOUNT  BIN#   JOB#           FG#                        Description                                Order Qty.  Release Qty" SKIP(1)
               "<FCourier New>"          
           .
               v-printline = v-printline + 16.
