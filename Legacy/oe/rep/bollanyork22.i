 /* oe/rep/bollanyork22.i  */  
 
   put 
         "<FArial>"  SKIP
          "<P14><C+48.5><B>Bill Of Lading</B>    " SKIP .
          if NOT lBroker THEN DO:
              Put "<C2><R2><#1><R+10><C+37><IMAGE#1=" ls-full-img1  SKIP .
          END.
          ELSE DO:
              Put "<C1><#1><R+5><C+25>"
                  "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
                  "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
                  "<P10></B>"

                  "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
                  "<P10><=1><R+3>"  
                  v-comp-add1 AT 8 SKIP
                  v-comp-add2 AT 8  SKIP
                  v-comp-add3 AT 8 SKIP
                  v-comp-add4 AT 8 SKIP
                  v-comp-add5 AT 8 SKIP
                  lv-email AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1) .
           END.

           Put   "<FCourier New>"
               SPACE(30) "Ship To:" AT 59  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C50><#3>" SKIP
        "<FArial><P14><=#3>" "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" "<c66>Page: " +  string(PAGE-NUM - lv-pg-num,">>9") + " of " +  string(lv-tot-pg,">>9")  FORM "x(40)"  SKIP(1)
                "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+3>" SKIP
                 SKIP     
                "<R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                "<FArial><=4><R+1>    Date                    FOB                                                                                   Carrier                                            Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(30) carrier.dscr v-frt-terms SKIP
                "<R24><C1><#5><FROM><R27><C81><RECT>" SKIP    
                "<R24><C13><FROM><R27><C13><LINE>" SKIP
                "<R24><C23><FROM><R27><C23><LINE>" SKIP
                "<R24><C35><FROM><R27><C35><LINE>" SKIP  
                "<R24><C47><FROM><R27><C47><LINE>" SKIP
                "<R24><C60><FROM><R27><C60><LINE>" SKIP 
            "<FArial><=5><R+1><C2>Qty<C14>Units / Unit<C24>Qty Shipped<C39>PO#<C48>Order# / Lot#<C61>Item# / Part# / Description" SKIP(1)
            "<FArial><=5><R+2><C2>Ordered<C14>Count<C24>[P/C]" SKIP(1)
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 25.
