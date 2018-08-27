/* oe/rep/relrfc2.i */   
   
PUT "<FMS Sans Serif>" SKIP
    "<P14><C+45><B>Pick Ticket</B> " SKIP
    /*"<C1><#1><R+5><C+25>" SKIP*/
    "<=1>" SKIP
    "<P10><C3><#1><R+11><C+40>" "<IMAGE#1=" + ls-full-img1 FORM "x(160)" SKIP 
          
    "<FMS Sans Serif>"
    "Sold To:" SPACE(30) "<C39>Ship To:"  SKIP
    "<C5>" cust.name "<C43>" shipto.ship-name  skip
    "<C5>" cust.addr[1] "<C43>" shipto.ship-addr[1] SKIP.
IF cust.addr[2] <> "" OR shipto.ship-addr[2] <> "" THEN
    PUT "<C5>" cust.addr[2] "<C43>" shipto.ship-addr[2]  SKIP
        "<C5>"  cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
        "<C43>"  shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip FORM "x(30)" SKIP.
ELSE PUT "<C5>" cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
        shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP(1).

PUT
    "<R4><C50><#3>" SKIP
    "<FMS Sans Serif><P14><=#3><P12>" SKIP
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
    "<FMS Sans Serif><=4><R+1>    Delivery Zone             Weight                    FOB                           Ship Via                                        Freight Terms" SKIP
    "<FMS Sans Serif><=4><R+3> " v-zone "<C14>" v-weight "<C27>" oe-ord.fob-code "<C35>" v-carrier "<C60>" v-frt-terms   SKIP
    "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
    "<R24><C7><FROM><R26><C7><LINE>" SKIP
    "<R24><C23><FROM><R26><C23><LINE>" SKIP
    "<R24><C49><FROM><R26><C49><LINE>" SKIP                   
    "<R24><C51.5><FROM><R26><C51.5><LINE>" SKIP
    "<R24><C56><FROM><R26><C56><LINE>" SKIP 
    "<R24><C62><FROM><R26><C62><LINE>" SKIP   
    "<R24><C70><FROM><R26><C70><LINE>" SKIP  
    "<FMS Sans Serif><=5><R+1> Order# <C10>Item / Desc / Lot #<C28>TAG/Whs/Bin <C50>X   #Pal     #Cas<C64.2>Count<C72>Bin Qty" SKIP(1)
    "<FMS Sans Serif>"          
    .
v-printline = v-printline + 16.

