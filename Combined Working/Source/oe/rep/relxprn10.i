/* oe/rep/relxprn10.i */   
   PUT "<FArial>" SKIP
           "<P14><C+48><B>Pick Ticket</B> " SKIP
           /* "<C1><#1><R+5><C+25>" /*<IMAGE#1=" ls-full-img1*/ SKIP /* pacific package */ */
            "<=1>" SKIP.
       IF lChkImage THEN DO:
         PUT  "<P10><C3><#1><R+11><C+43>" "<IMAGE#1=" + ls-full-img1 FORM "x(160)" SKIP .
       END.
       ELSE DO:
         PUT
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
            lv-email AT 8 SKIP(1).
       END.
         PUT
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
                   "<=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999"     SKIP
                   "<=#3><R+6>Ship Date:" oe-relh.rel-date        SKIP
                    SKIP     
                   "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP      
                   /*"<R19><C35><FROM><R23><C35><LINE>" SKIP */
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>    Pallets/Bags              Weight                    FOB               Ship Via                                                    Freight Terms" SKIP
                   "<FCourier New><=4><R+3> " space(2) v-pallets FORM ">>>>>9" SPACE(7) v-weight FORM "->>>>>>9" space(8) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C7><FROM><R26><C7><LINE>" SKIP
                   "<R24><C18><FROM><R26><C18><LINE>" SKIP
                   "<R24><C27><FROM><R26><C27><LINE>" SKIP
                   "<R24><C39><FROM><R26><C39><LINE>" SKIP                   
                   /*"<R24><C57><FROM><R26><C57><LINE>" SKIP*/
                   "<R24><C64><FROM><R26><C64><LINE>" SKIP 
                   "<R24><C72><FROM><R26><C72><LINE>" SKIP                                                                                                     /*job#*/
               "<FArial><=5><R+1> Order#            PO#                Bin#                FG#                       Description                                               Order Qty.   Release Qty" SKIP(1)
               "<FCourier New>"          
           .
               v-printline = v-printline + 16.

