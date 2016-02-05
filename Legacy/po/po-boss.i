/* po/po-xprnt.i */

PUT 
     "<FArial>"   SKIP
       "<P14><C+35> <B>Purchase Order</B> <P10>" v-change-ord SKIP       
   "<C1><#1><R+5><C+25>"  "<IMAGE#1=" ls-full-img1  SKIP  
      "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
      "<P10><=1><R+3>"
      v-comp-add1  AT 8 SKIP
      /*v-comp-add2  AT 8  SKIP*/
      v-comp-add3  AT 8 SKIP
      v-comp-add4  AT 8 SKIP
      v-comp-add5  AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" skip(1)
      /*lv-email AT 8 SKIP(1)*/
     "<FCourier New>"
     "Purchase Order To:" AT 4 SPACE(43) "Ship To:"  SKIP.

IF AVAIL vend THEN
         PUT SPACE(3) vend.name "Phone#:" AT 38 "(" + vend.area-code + ")" + string(vend.phone,"xxx-xxxx") FORM "x(14)" v-sname AT 65 skip
         SPACE(3) vend.add1     "Fax#  :" AT 38 "(" + vend.fax-area + ")" + string(vend.fax,"xxx-xxxx") FORM "x(14)" v-saddr[1] AT 65 SKIP
         SPACE(3) vend.add2 v-saddr[2] AT 65 SKIP
         SPACE(3) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 65 SKIP.
ELSE PUT v-sname AT 65 skip
         v-saddr[1] AT 65 SKIP
         v-saddr[2] AT 65 SKIP
         v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 65 SKIP.
PUT
       "<R4><C50><#3>" SKIP
       "<FArial><P14><=#3><P10>" SKIP
          "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+2>Date: " po-ord.po-date        SKIP
          /*"<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP*/
          "<=3><R+3>Date Required: " po-ord.due-date SKIP
       .
    /*IF lv-display-comp THEN 
        PUT "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=1><C3><R+1><P20><B>" lv-comp-name "</B><P10><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . */
      v-printline = v-printline + 10.    

      PUT "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
      "<R21><C1><FROM><R21><C80><LINE>" SKIP    
      "<R19><C18><FROM><R23><C18><LINE>" SKIP
      "<R19><C48><FROM><R23><C48><LINE>" SKIP
      /*"<R20><C49><FROM><R24><C49><LINE>" SKIP
      "<R20><C55><FROM><R24><C55><LINE>" SKIP*/
      /*"<R20><C72><FROM><R24><C72><LINE>" SKIP*/
      .
      PUT "<FArial><=4><R+1>                Buyer                                                 Contact                                                                      Terms                          " SKIP /*FOB              Ship Via                        Freight*/
          "<FCourier New><=4><R+3>    " po-ord.buyer FORM "x(20)"   po-ord.contact FORM "x(35)" terms.dscr FORM "x(33)" /*po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr*/
          .
      
      PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
             "<R24><C5><FROM><R26><C5><LINE>" SKIP
             "<R24><C15><FROM><R26><C15><LINE>" SKIP
             /*"<R25><C20><FROM><R27><C20><LINE>" SKIP */
             "<R24><C43.5><FROM><R26><C43.5><LINE>" SKIP
             "<R24><C54><FROM><R26><C54><LINE>" SKIP
             "<R24><C63><FROM><R26><C63><LINE>" SKIP
             "<R24><C68><FROM><R26><C68><LINE>" 
             /*"<R25><C72><FROM><R27><C72><LINE>" SKIP*/
             .
      PUT "<FArial><=5><C7>"
          "<=5><R+1> Line         Quantity                          Our Item/Description                           Job#/Fm#              Cost          UOM          Ext Cost" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
