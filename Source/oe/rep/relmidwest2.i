/* oe/rep/relmidwest2.i */   
   
   PUT "<FArial>" SKIP
       "<P14><C+42><B>Pick     Ticket</B> " SKIP
        "<C1><LEFT=9mm><#1><R+5><C+25>" SKIP
        "<=1>" SKIP.
   IF lvFirstPage THEN DO:
      PUT UNFORMATTED   
       "<C2><R3.5><#1><R+10><C+45><IMAGE#1=" ls-full-img1
       "<P10></B>"
       "<P10><=1><R+10>"
        /*v-comp-add1 AT 8 SKIP
        v-comp-add2 AT 8  SKIP
        v-comp-add3 AT 8 SKIP
        v-comp-add4 AT 8 SKIP
        v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)"  SKIP
        lv-email AT 8  SKIP(1)*/
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
         "<R4><C63><#3>" SKIP
         "<FArial><P14><=#3><P12>" SKIP
         "<=#3><B>Ticket #: " 
         "<UNITS=INCHES><AT=.54,7.5><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
          string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
           "<AT=,8>" oe-relh.release#        SKIP(1)
         "<=#3><R+5>Print Date:" v-ticket-date   FORM "99/99/9999" " "   STRING(oe-relh.upd-time,"hh:mm am") SKIP
         "<=#3><R+6>Ship Date:" oe-relh.rel-date        SKIP
         "<=#3><R+7>CSR: " v-csr "     "  "Sales Rep: " cSalRepName FORMAT "X(20)"  SKIP
          SKIP     
         "<|10><R19><C1><#4><FROM><R21><C63><RECT>" SKIP
         "<R20><C1><FROM><R20><C63><LINE>" SKIP    
         "<R19><C11><FROM><R21><C11><LINE>" SKIP
         "<R19><C22><FROM><R21><C22><LINE>" SKIP
         "<R19><C30><FROM><R21><C30><LINE>" SKIP
         "<R19><C50><FROM><R21><C50><LINE>" SKIP
         "<FArial><=4>  Delivery Zone         Weight                FOB                        Ship Via                            Freight Terms" SKIP
         "<FCourier New><=4><R+1>    " v-zone space(6) v-weight space(7) oe-ord.fob-code SPACE(4) v-carrier space(5) v-frt-terms   SKIP.

       PUT "<FArial><=#3><R+8><B>Shipping Instructions: </B>"SKIP(1)
       "<=#3><C+.5><R+9>" v-ship-i[1]  SKIP
       "<=#3><C+.5><R+10>" v-ship-i[2] SKIP
       "<=#3><C+.5><R+11>" v-ship-i[3] SKIP
       "<=#3><C+.5><R+12>" v-ship-i[4] SKIP
       "<=#3><R+8><FROM><R+9><C+41><RECT>"  SKIP    .

      lvFirstPage = NO.
   END.
   PUT
         "<|10><R22><C1><#5><FROM><R24><C104><RECT>" SKIP    
         "<R22><C7><FROM><R24><C7><LINE>" SKIP
         "<R22><C30.5><FROM><R24><C30.5><LINE>" SKIP
         "<R22><C47><FROM><R24><C47><LINE>" SKIP 
         "<R22><C59><FROM><R24><C59><LINE>" SKIP   
         "<R22><C64><FROM><R24><C64><LINE>" SKIP   
         "<R22><C73><FROM><R24><C73><LINE>" SKIP
         "<R22><C80><FROM><R24><C80><LINE>" SKIP
         "<R22><C93><FROM><R24><C93><LINE>" SKIP

         "<FArial><=5><C31>TAG/Whs/Bin<C48>Receipt Date<C81>QOH: RelQty: Ship<C94>Pallets:   Total" SKIP
         "<=5><R+1> Order# <C8>Item/Desc/Lot#<C31>Vendor Name/PO#(Job#)<C60>Units<C65>Unit Count<C74>Partial<C81>Qty<C94>Pallets" SKIP
         "<FCourier New>"
         "<|10><R24><C1><#7><FROM><R45><C104><RECT><=5><R+2>" SKIP    
         "<R24><C93><FROM><R45><C93><LINE>" "<R24><C80><FROM><R45><C80><LINE>" /*qty vert line*/
         "<R25><C80><FROM><R25><C93><LINE>" "<R27><C80><FROM><R27><C93><LINE>" SKIP   
         "<R31><C80><FROM><R31><C93><LINE>" "<R32><C80><FROM><R32><C93><LINE>" 
         "<R34><C80><FROM><R34><C93><LINE>"
         "<R38><C80><FROM><R38><C93><LINE>" "<R39><C80><FROM><R39><C93><LINE>"
         "<R31><C1><FROM><R31><C104><LINE>"  /* seperate line per item */
         "<R38><C1><FROM><R38><C104><LINE>"  /* seperate line per item */
         "<R41><C80><FROM><R41><C93><LINE>"
    /*     "<R43><C80><FROM><R43><C93><LINE>" "<R44><C80><FROM><R44><C93><LINE>" */
         "<=#7>".

      v-printline = v-printline + 16.

