/* oe/rep/relcardx2.i */   
   
   PUT "<FArial>" SKIP
           "<P14><C+35><B>Pick Ticket</B> " SKIP
            "<C1><#1><R+5><C+25>" SKIP
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
                  SPACE(5) cust.addr[1] shipto.ship-addr[1] AT 45 ("Contact: " + shipto.contact) FORMAT "x(35)" AT 75 SKIP.
      IF cust.addr[2] <> "" OR shipto.ship-addr[2] <> "" THEN
                  PUT SPACE(5) cust.addr[2] shipto.ship-addr[2] AT 45 SKIP
                  SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                           shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)"
                           "Phone:  " + string(shipto.area-code,"(999)") + string(shipto.phone,"999-9999") AT 75 SKIP.
      ELSE PUT SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                        shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)"
                        "Phone:  " + string(shipto.area-code,"(999)") + string(shipto.phone,"999-9999") AT 75 SKIP(1).

      PUT
           "<R4><C50><#3>" SKIP
           "<FArial><P14><=#3><P12>" SKIP
                   "<=#3><B>Ticket #: " 
                   "<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                    string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
                     "<AT=,6.5>" oe-relh.release#   FORMAT ">>>>>>>>"     SKIP(1)
                   "<=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999" " "   STRING(TIME,"hh:mm am") SKIP
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
                    "<FArial><R+0.5> <C45>  ON HAND INVENTORY        RELEASE QUANTITY" SKIP
                   "<FCourier New><|10><R24.5><C1><#5><FROM><R26><C80><RECT>" SKIP    
                   "<R24.5><C7><FROM><R26><C7><LINE>" SKIP
                   "<R24.5><C23.5><FROM><R26><C23.5><LINE>" SKIP
                   "<R24.5><C36><FROM><R26><C36><LINE>" SKIP                   
                   "<R24.5><C41><FROM><R26><C41><LINE>" SKIP
                   "<R24.5><C46><FROM><R26><C46><LINE>" SKIP 
                   "<R24.5><C54><FROM><R26><C54><LINE>" SKIP   
                   "<R24.5><C62><FROM><R26><C62><LINE>" SKIP 
                   "<R24.5><C67><FROM><R26><C67><LINE>" SKIP 
                   "<R24.5><C73><FROM><R26><C73><LINE>" SKIP
               "<FArial><=5><R+0.5> Order# <C10>Item / Desc / Lot #<C28>Whs/Bin <C37> #Pal     #Cas     #Count         Bin Qty         #Pal      #Case        Rel Qty" SKIP
               "<FCourier New>"          
               .
               v-printline = v-printline + 16.

