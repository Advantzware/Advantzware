/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.             */
/* oe/rep/relacpi2.i */   
   
   PUT "<FArial>" SKIP
           "<P14><C+45><B>Pick Ticket</B> " 
            /*"<C1><#1><R+5><C+25>" SKIP*/
           "<P10><C3><#1><R+11><C+40>" "<IMAGE#1=" + ls-full-img1 FORM "x(160)" SKIP 
          /* "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
           "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
           "<P10></B>"
           "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"        
           "<P10><=1><R+3>"
            v-comp-add1 AT 8 SKIP
            v-comp-add2 AT 8  SKIP
            v-comp-add3 AT 8 SKIP
            v-comp-add4 AT 8 SKIP
            v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
            lv-email AT 8 SKIP(1)*/
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
                    string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
                     "<AT=,6.5>" oe-relh.release#        SKIP(1)
                   "<=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999" " "   STRING(TIME,"hh:mm am") /*STRING(oe-relh.upd-time,"hh:mm am")*/ SKIP
                   "<=#3><R+6>Ship Date:" oe-relh.rel-date        SKIP
                   "<=#3><R+7>CSR: " v-csr SKIP
                    SKIP     
                   "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
                   "<R21><C1><FROM><R21><C80><LINE>" SKIP    
                   "<R19><C12><FROM><R23><C12><LINE>" SKIP
                   "<R19><C25><FROM><R23><C25><LINE>" SKIP
                   "<R19><C34><FROM><R23><C34><LINE>" SKIP
                   "<R19><C57><FROM><R23><C57><LINE>" SKIP
                   "<FArial><=4><R+1>    Delivery Zone             Weight                    FOB                           Ship Via                                        Freight Terms" SKIP
                   "<FCourier New><=4><R+3> " v-zone space(10) v-weight space(10) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
                   "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24><C9><FROM><R26><C9><LINE>" SKIP
                   "<R24><C31><FROM><R26><C31><LINE>" SKIP
                   "<R24><C51><FROM><R26><C51><LINE>" SKIP                   
                   "<R24><C53.5><FROM><R26><C53.5><LINE>" SKIP
                   "<R24><C58><FROM><R26><C58><LINE>" SKIP 
                   "<R24><C63><FROM><R26><C63><LINE>" SKIP   
                   "<R24><C69><FROM><R26><C69><LINE>" SKIP 
                   "<R24><C74><FROM><R26><C74><LINE>" SKIP  
               "<FArial><=5><R+1> Order# <C14>Item / Desc / Lot #<C35>TAG/Whs/Bin <C52>X     #Pal     #Cas<C64.2>Count<C69.5>Partial <C75>Bin Qty" SKIP(1)
               "<FCourier New>"          
               .
               v-printline = v-printline + 16.

