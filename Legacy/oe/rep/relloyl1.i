/* ------------------------------------------ oe/rep/relloyl1.i 09220907 GDM */
/* RELEASE PRINT  Program for N-K-1 RELPRINT = LoyLang                       */
/* ------------------------------------------------------------------------- */

PUT "<FArial>"                       SKIP
    "<P14><C+35><B>Pick Ticket</B> " SKIP
    "<C1><#1><R+5><C+25>"            SKIP 
    "<=1>"                           SKIP
    "<=1><C3><FGCOLOR=" TRIM (lv-comp-color) + ">"
    "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" TRIM(lv-other-color) + ">" FORM "x(6)" 
    "<P10></B>"
    "<=1><R+2>" "<FGCOLOR=" + TRIM(lv-comp-color) + ">" FORM "x(15)"        
    "<P10><=1><R+3>"
    v-comp-add1 AT 8 SKIP
    v-comp-add2 AT 8 SKIP
    v-comp-add3 AT 8 SKIP
    v-comp-add4 AT 8 SKIP
    v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
    lv-email AT 8 SKIP(1).

PUT "<R4><C50><#3>" SKIP
    "<FArial><P14><=#3><P12>" SKIP
    "<=#3><B>Ticket #: " 
    "<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
    STRING(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
    "<AT=,6.5>" oe-relh.release# FORM "->>>>>>9" SKIP(1)
    "<=#3><R+3><C31>Ship To:" v-transfer AT 45
    "<=#3><R+5><C54>Release Date: " v-ticket-date   FORM "99/99/9999" " "   
    STRING(oe-relh.upd-time,"hh:mm am")  SKIP
    "<=#3><R+4><C32>" shipto.ship-name
    "<=#3><R+6><C54>Ship Date: " oe-relh.rel-date        SKIP
    "<=#3><R+5><C32>" shipto.ship-addr[1]
    "<=#3><R+7><C54>CSR: " v-csr SKIP
    "<=#3><R+6><C32>" shipto.ship-addr[2] SKIP
    "<=#3><R+7><C32>" shipto.ship-city + " " + shipto.ship-state + " " +  shipto.ship-zip FORM "x(30)"
    SKIP.

PUT "<|10><R15><C1><#4><FROM><R19><C80><RECT>" SKIP
    "<R17><C1><FROM><R17><C80><LINE>" SKIP    
    "<R15><C12><FROM><R19><C12><LINE>" SKIP
    "<R15><C25><FROM><R19><C25><LINE>" SKIP      
    "<R15><C35><FROM><R19><C35><LINE>" SKIP
    "<R15><C57><FROM><R19><C57><LINE>" SKIP
    "<FArial><=4><R+1>    Delivery Zone             Weight                    FOB                              Ship Via                                            Freight Terms" SKIP
    "<FCourier New><=4><R+3> " v-zone space(9) v-weight FORM ">>>>>>9" space(7) v-fob SPACE(5) v-carrier space(10) v-frt-terms   SKIP
    "<|10><R20><C1><#5><FROM><R23><C80><RECT>" SKIP    
    "<R20><C7><FROM><R23><C7><LINE>" SKIP
    "<R20><C33><FROM><R23><C33><LINE>" SKIP
    "<R20><C51><FROM><R23><C51><LINE>" SKIP
    "<R20><C62.5><FROM><R23><C62.5><LINE>" SKIP   
    "<R20><C71><FROM><R23><C71><LINE>" SKIP  
    "<FArial><=5> <C53.7>   Total <C65>Count " 
    "<=5><R+1>   Order# <C8>Cust Part# / Item Name / Description <C36>Whs / Bin / TAG <C53.7>   Units     <C64>Per Units  <C73.5>Bin Qty" SKIP                                
    "<FCourier New><P11>".
             

ASSIGN v-printline = v-printline + 16
       v-ship-i[1] = oe-relh.ship-i[1]
       v-ship-i[2] = oe-relh.ship-i[2]
       v-ship-i[3] = oe-relh.ship-i[3]
       v-ship-i[4] = oe-relh.ship-i[4].

PUT "<FArial><R56><C1><P12><B>     Shipping Instructions: </B> <P9> "
    "<R58><C1>" v-ship-i[1] AT 7 
    "<R59><C1>" v-ship-i[2] AT 7 
    "<R60><C1>" v-ship-i[3] AT 7 
    "<R61><C1>" v-ship-i[4] AT 7 "<C70>Page " + string(PAGE-NUM - lv-pg-num,">>9") FORM "x(20)"     
    "<R62><C1>" " Dock #: "  AT 7 v-dock-loc " Dock Hours: " AT 30 v-dock-hrs
    "<R63><C1>"
    "__________________________________________________________________________________________________________________"  SKIP .

PUT "<FCourier New><P11><=5><R+2>".
