/* oe/rep/relpchtr2.i */   
   
   PUT "<FArial>" SKIP
           "<P14><C+35><B>Pick Ticket</B>                                     <P10>Page: " string(PAGE-NUM - v-page-num,">>9")  SKIP
            "<P14><C1><#1><R+5><C+25>" SKIP
            "<=1>" SKIP
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
                    string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
                     "<AT=,6.5>" oe-relh.release#        SKIP(1)
                   "<=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999" " "   STRING(oe-relh.upd-time,"hh:mm am") SKIP
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
                   "<R24><C10><FROM><R26><C10><LINE>" SKIP
                   "<R24><C26><FROM><R26><C26><LINE>" SKIP
                   "<R24><C49><FROM><R26><C49><LINE>" SKIP                   
                   "<R24><C51.5><FROM><R26><C51.5><LINE>" SKIP
                   "<R24><C56><FROM><R26><C56><LINE>" SKIP 
                   "<R24><C62><FROM><R26><C62><LINE>" SKIP   
                   "<R24><C70><FROM><R26><C70><LINE>" SKIP  
               "<FArial><=5><R24><C2>Order #<R+1><C3>Job# <C11>Cust Part#/FG#/PO# <C28>TAG/Whs/Bin <C50>X   #Pal     #Cas<C64.2>Count<C72>Bin Qty" SKIP(1)
               "<FCourier New>"          
               .
               v-printline = v-printline + 16.

