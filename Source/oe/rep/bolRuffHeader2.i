 /* oe/rep/bolRuffHeader2.i  */  
 
   put 
         "<FArial>"  SKIP
          "<P14><C+48.5><B>Bill Of Lading</B>    " SKIP
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
         v-comp-add5 AT 8 SKIP
         lv-email AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1)
               "<FCourier New>"
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
                "<R19><C14><FROM><R23><C14><LINE>" SKIP
                "<R19><C52><FROM><R23><C52><LINE>" SKIP
                "<R19><C60><FROM><R23><C60><LINE>" SKIP
                "<R19><C68><FROM><R23><C68><LINE>" SKIP
                "<FArial><=4><R+1><C5>Date
                                 <C30>Carrier
                                 <C54>NMFC#
                                 <C62>Class
                                 <C70>Freight Terms" SKIP
                "<FCourier New><=4><R+3><C2>" oe-bolh.bol-date
                                      "<C20>" carrier.dscr
                                      "<C53> 151320"
                                     "<C62.5>" "55"
                                      "<C71>" v-frt-terms SKIP
                "<R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
                "<R24><C13><FROM><R26><C13><LINE>" SKIP
                "<R24><C26><FROM><R26><C26><LINE>" SKIP
                "<R24><C56><FROM><R26><C56><LINE>" SKIP  
                "<R24><C61><FROM><R26><C61><LINE>" SKIP
                "<R24><C67><FROM><R26><C67><LINE>" SKIP            
                "<R24><C77><FROM><R26><C77><LINE>" SKIP 
            "<FArial><=5><R+1><C5> FG#                      PO# / Job#                        Part#/Description                                    Units    Count                 Total       P/C" SKIP(1)
            "<FCourier New>"                                  
            .

            v-printline = v-printline + 25.
