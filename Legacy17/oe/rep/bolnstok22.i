 /* oe/rep/bolnstok22.i  */  
 
   put 
         "<FArial>"  SKIP
          "<P14><C+34><B>Bill Of Lading</B>                                     Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" SKIP
          "<C1><#1><R+5><C+25>"
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
         lv-email AT 8 SKIP
               "<FCourier New>"
               "Sold To:" AT 6   "Ship To:" AT 45  SKIP
               SPACE(5) v-comp-name v-ship-name AT 45 skip
               SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
             SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
               SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP
        "<R5><C65><#3>" SKIP
        "<FArial><P14><=#3>" "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                "<=#3><R+1>Date: " oe-bolh.bol-date        SKIP
                "<=#3><R+2>Contact: " cust.contact        SKIP
                "<=#3><R+3>Phone: "  cust.area-code + cust.phone format "(999)999-9999"        SKIP
                "<=#3><R+4>CSR: " vuser-id FORMAT "x(10)" SKIP
                 SKIP     
                "<R19><C1><#4><FROM><R23><C81><RECT>" SKIP
                "<R21><C1><FROM><R21><C81><LINE>" SKIP    
                "<R19><C12><FROM><R23><C12><LINE>" SKIP
                "<R19><C25><FROM><R23><C25><LINE>" SKIP
                "<R19><C46><FROM><R23><C46><LINE>" SKIP
                "<R19><C66><FROM><R23><C66><LINE>" SKIP
                "<FArial><=4><R+1>    Date                    Phone                            FOB                                          Ship Via                                            Freight Terms" SKIP 
                "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) cust.area-code + cust.phone format "(999)999-9999" space(3) v-fob space(13) carrier.dscr v-frt-terms SKIP
                "<R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C20><FROM><R26><C20><LINE>" SKIP
                "<R24><C32><FROM><R26><C32><LINE>" SKIP
                "<R24><C53><FROM><R26><C53><LINE>" SKIP 
                "<R24><C60><FROM><R26><C60><LINE>" SKIP
                "<R24><C65><FROM><R26><C65><LINE>" SKIP
                "<R24><C72><FROM><R26><C72><LINE>" SKIP            
                "<R24><C78><FROM><R26><C78><LINE>" SKIP 
            "<FArial><=5><R+1>    PO# / Order#        Bin#            FG#                       Part#/Description                           Rel Qty       Units    Count           Total     P/C" SKIP(1)
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 16.
