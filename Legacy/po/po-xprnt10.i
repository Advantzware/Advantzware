/* po/po-xprnt.i */

PUT 
     "<FArial>"   SKIP
       "<P14><C+48> <B>Purchase Order</B> <P10>" v-change-ord SKIP  
      "<C2><R2><#1><R+10><C+40><IMAGE#1=" ls-full-img1  SKIP     
   
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
          "<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP
          "<=3><R+4>Date Required: " po-ord.due-date SKIP
       .   
        v-printline = v-printline + 10.    

      PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
      "<R22><C1><FROM><R22><C80><LINE>" SKIP    
      "<R20><C10><FROM><R24><C10><LINE>" SKIP
      "<R20><C29><FROM><R24><C29><LINE>" SKIP
      "<R20><C49><FROM><R24><C49><LINE>" SKIP
      "<R20><C55><FROM><R24><C55><LINE>" SKIP
      "<R20><C72><FROM><R24><C72><LINE>" SKIP
      .
      PUT "<FArial><=4><R+1>       Buyer                         Contact                                         Terms                        FOB              Ship Via                        Freight" SKIP
          "<FCourier New><=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(25)" terms.dscr FORM "x(23)" po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr
          .
      
      PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
             "<R25><C5><FROM><R27><C5><LINE>" SKIP
             "<R25><C15><FROM><R27><C15><LINE>" SKIP
             "<R25><C20><FROM><R27><C20><LINE>" SKIP 
             "<R25><C47><FROM><R27><C47><LINE>" SKIP
             "<R25><C53><FROM><R27><C53><LINE>" SKIP
             "<R25><C62><FROM><R27><C62><LINE>" SKIP
             "<R25><C68><FROM><R27><C68><LINE>" 
             "<R25><C72><FROM><R27><C72><LINE>" SKIP
             .
      PUT "<FArial><=5><C7>Quantity"
          "<=5><R+1> Line     Over/Under%     UOM          Our Item/Description/Vendor ID               Adder      Job#/Fm#        Cost     UOM      Ext Cost" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
