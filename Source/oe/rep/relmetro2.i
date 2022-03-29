/* oe/rep/relmetro2.i */   
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.    */   
   
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
                  "Sold To:" SPACE(30) "Ship To:"  v-transfer AT 90 SKIP
                  SPACE(5) cust.name shipto.ship-name AT 45 skip
                  SPACE(5) cust.addr[1] shipto.ship-addr[1] AT 45 SKIP.
      IF cust.addr[2] <> "" OR shipto.ship-addr[2] <> "" THEN
                  PUT SPACE(5) cust.addr[2] shipto.ship-addr[2] AT 45 SKIP
                  SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                           shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP.
      ELSE PUT SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                        shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP(1).
      PUT 
          shipto.contact AT 45  SKIP
          shipto.area-code AT 45 shipto.phone SKIP.

      PUT
           "<R4><C50><#3>" SKIP
           "<FArial><P14><=#3><P12>" SKIP
                   "<=#3><B>Ticket #: " 
                   "<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
                    string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
                     "<AT=,6.5>" oe-relh.release# FORM "->>>>>>9" SKIP(1)
                   "<=#3><R+5>Release Date: " v-ticket-date   FORM "99/99/9999" " "   STRING(oe-relh.upd-time,"hh:mm am")  SKIP
                   "<=#3><R+6>Ship Date: " oe-relh.rel-date        SKIP
                   "<=#3><R+7>CSR: " v-csr SKIP     
                   "<|10><R21><C1><#4><FROM><R25><C80><RECT>" SKIP
                   "<R23><C1><FROM><R23><C80><LINE>" SKIP    
                   "<R21><C12><FROM><R25><C12><LINE>" SKIP
                   "<R21><C25><FROM><R25><C25><LINE>" SKIP      
                   "<R21><C35><FROM><R25><C35><LINE>" SKIP
                   "<R21><C57><FROM><R25><C57><LINE>" SKIP
                   "<FArial><=4><R+1>    Delivery Zone             Weight                    FOB                              Ship Via                                            Freight Terms" SKIP
                   "<FCourier New><=4><R+3> " v-zone space(9) v-weight FORM ">>>>>>9" space(8) v-fob-code SPACE(1) v-carrier space(8) v-frt-terms   SKIP
                   "<|10><R26><C1><#5><FROM><R28><C80><RECT>" SKIP    
                   "<R26><C9><FROM><R28><C9><LINE>" SKIP
/*                    "<R26><C33><FROM><R28><C33><LINE>" SKIP */
                   "<R26><C53><FROM><R28><C53><LINE>" SKIP
/*                    "<R26><C57><FROM><R28><C57><LINE>" SKIP     */
/*                    "<R26><C62.5><FROM><R28><C62.5><LINE>" SKIP */
                   "<R26><C72><FROM><R28><C72><LINE>" SKIP  
               "<FArial><=5><R+1>   Order# <C18>Item / Description<C60>Whse/Bin <C75.5>#Pal" SKIP
               "<FCourier New><P11>".

ASSIGN v-printline = v-printline + 18
       v-ship-i[1] = oe-relh.ship-i[1]
       v-ship-i[2] = oe-relh.ship-i[2]
       v-ship-i[3] = oe-relh.ship-i[3]
       v-ship-i[4] = oe-relh.ship-i[4].

PUT "<FArial><R56><C1><P12><B>     Shipping Instructions: </B> <P9> "
    "<R57><C1>" v-ship-i[1] AT 7 
    "<R58><C1>" v-ship-i[2] AT 7 
    "<R59><C1>" v-ship-i[3] AT 7 
    "<R60><C1>" v-ship-i[4] AT 7 "<C70>Page " + string(PAGE-NUM - lv-pg-num,">>9") FORM "x(20)"     
    "<R62><C1>"
    "__________________________________________________________________________________________________________________"  SKIP .

PUT "<FCourier New><P11><=5><R+2>".
