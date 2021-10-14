/* oe/rep/invlovepac-CAN.i */

PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2>"
    "<C2><P10><R2><#1><R+10><C+37><IMAGE#1=" ls-full-img1  SKIP
    
    "<FCourier New>"
    space(7) "Factur� �,Bill To:" SPACE(30) "Exp�di� �,Ship To:" SKIP
    SPACE(7) inv-head.cust-name v-shipto-name AT 56 skip
    SPACE(7) cust.addr[1] FORMAT "x(42)"    v-shipto-addr[1] AT 56 FORMAT "x(42)" SKIP
    SPACE(7) cust.addr[2] FORMAT "x(42)"  v-shipto-addr[2] AT 56 FORMAT "x(42)" SKIP
    SPACE(7) v-addr3 FORMAT "x(42)"   v-sold-addr3 AT 56 FORMAT "x(42)"  SKIP 
    SPACE(7) cAddr4 FORMAT "x(42)"   cShipAddr4 AT 56 FORMAT "x(42)" SKIP .
v-printline = v-printline + 18.
/*IF lv-display-comp THEN
   PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C2><R+3><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . */

PUT "</B><P10><R4><C50><#3><FROM><R10><C80><RECT><||3>" SKIP.
PUT "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
    "<R8><C50><FROM><R8><C80><LINE><||3>" SKIP
    "<R4><C62><FROM><R6><C62><LINE><||3>" SKIP
    "<R6><C65><FROM><R8><C65><LINE><||3>" SKIP
    "<R8><C65><FROM><R10><C65><LINE><||3>" SKIP.
        
PUT "<FArial><P12><=#3><R-2><B><C50>Factur�/Invoice#: " inv-head.inv-no "</B><P10><C74>Page: " string(PAGE-NUM - v-page-num,">>9") SKIP
    "<=#3> Client                      Contact"
    "<=#3><R+2> T�l�phone                        Fax" 
    "<=#3><R+4> Bdc PO                           Date <FCourier New>"    
    "<=3><R+1> " inv-head.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-po-no space(3) v-inv-date .

    
PUT "<R21><C1><#4><FROM><R25><C80><RECT><||3>" SKIP
    "<R23><C1><FROM><R23><C80><LINE><||3>" SKIP    
    "<R21><C11><FROM><R25><C11><LINE><||3>" SKIP
    "<R21><C22><FROM><R25><C22><LINE><||3>" SKIP
    "<R21><C38><FROM><R25><C38><LINE><||3>" SKIP
    "<R21><C55><FROM><R25><C55><LINE><||3>" SKIP
    "<R21><C65><FROM><R25><C65><LINE><||3>" SKIP
    "<R21><C72><FROM><R25><C72><LINE><||3>" SKIP
    .
v-printline = v-printline + 5.


PUT "<FArial><=4>     Ship Date               FAB                          Ship Via                             Terms                          S.Person       Palettes          BL" SKIP
    "<FArial><=4><R+1>     Exp�dition              FOB                        Exp�diteur                         Termes                      Repr�sentant     Pallets         BOL#" SKIP
     "<FCourier New><=4><R+3> " v-date-ship FORM "99/99/9999" space(2)
     v-fob FORM "x(12)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     xinv-head.terms-d FORM "x(15)" space(4) v-salesman FORM "x(8)"
     v-tot-pallets FORM "->>>>,>>9" 
     "<C72>"xinv-head.bol-no
    SKIP.


PUT "<R26><C1><#5><FROM><R28><C80><RECT><||3>" SKIP    
                "<R26><C8.5><FROM><R28><C8.5><LINE><||3>" SKIP
                "<R26><C14.5><FROM><R28><C14.5><LINE><||3>" SKIP
                "<R26><C22><FROM><R28><C22><LINE><||3>" SKIP
                "<R26><C34.5><FROM><R28><C34.5><LINE><||3>" SKIP
                "<R26><C56><FROM><R28><C56><LINE><||3>" SKIP
                "<R26><C65><FROM><R28><C65><LINE><||3>" SKIP
                "<R26><C69><FROM><R28><C69><LINE><||3>" SKIP
                .   
PUT "<FArial><=5>  Ordered     Shipped    Order #                                                                                                      Price         UoM          Amount" SKIP(1).
PUT "<FArial><=5><R+1>Command�  Exp�di�  Commande   Item#/CustPart#                       Description                            Prix          UM            Montant" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".
