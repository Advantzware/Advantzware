/* po/po-consb.i */
/*IF AVAIL vend THEN
         PUT SPACE(5) vend.name v-sname AT 42 skip
         SPACE(5) vend.add1 v-saddr[1] AT 42 SKIP
         SPACE(5) vend.add2 v-saddr[2] AT 42 SKIP
         SPACE(5) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 42 SKIP.
ELSE PUT v-sname AT 42 skip
         v-saddr[1] AT 42 SKIP
         v-saddr[2] AT 42 SKIP
         v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 42 SKIP.
*/

PUT "<FArial><C1><#1>"  
           "<P16><C+25><B>PURCHASE ORDER - BOARD</B> <P10>"  SKIP       
    /*   "<C1><#1><R+5><C+25>" /*"<IMAGE#1=" + ls-full-img1 FORM "x(60)" */ SKIP 
          "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
          "<P10><=1><R+3>"
          v-comp-add1  AT 8 SKIP
          v-comp-add2  AT 8  SKIP
          v-comp-add3  AT 8 SKIP
          v-comp-add4  AT 8 SKIP
          v-comp-add5  AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" skip
          lv-email AT 8 */
          SKIP(2)
         "<FCourier New>"
         "BILL TO: CONSOLIDATED BOX"  FORM "x(30)" "         SHIP TO: CONSOLIDATED BOX"  /*v-sname*/ space(16) "Date: " po-ord.po-date SKIP
         "P.O. BOX 26323" AT 10 FORM "x(30)" /*v-saddr[1]*/ "1314 EASTPORT ROAD" AT 49 " Page: " AT 80 PAGE-NUM - lv-page-no SKIP
         /*v-vend-addr2 AT 10 FORM "x(30)" v-saddr[2] AT 49 SKIP*/
         /*v-vend-addr3 AT 10 FORM "x(30)"*v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 49 SKIP(1)*/
         "JACKSONVILLE, FL 32226" AT 10 "JACKSONVILLE, FL 32218" AT 49 SKIP(1)
         "<=1><R+7.5><C1><FROM><C80><LINE><||5>" 
         "<=1><R+8><C2>PO<C55>         Due        Special"
         "<=1><R+9><C1>Number Quantity Sheet Size            Test/Flute  Paper                   Date     Instructions" 
         "<=1><R+10><C1><FROM><C80><LINE><||3>" SKIP
         .
/*
PUT "<R4><C50><#3>" SKIP
       "<FArial><P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
          "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+2>Date: " po-ord.po-date        SKIP
          "<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP
          "<=3><R+4>Date Required: " po-ord.due-date SKIP
       .
    IF lv-display-comp THEN 
        PUT "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=1><C3><R+1><P20><B>" lv-comp-name "</B><P10><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 
      v-printline = v-printline + 10.    
      /* same as request - no space between hd & line
      PUT "<R11><#4><R17><C80><RECT>" SKIP
      "<R13><C1><FROM><R13><C80><LINE>" SKIP
      "<R15><C1><FROM><R15><C80><LINE>" SKIP

      "<R11><C20><FROM><R15><C20><LINE>" SKIP
      "<R11><C30><FROM><R15><C30><LINE>" SKIP
      "<R11><C40><FROM><R15><C40><LINE>" SKIP
      "<R11><C50><FROM><R15><C55><LINE>" SKIP
      "<R11><C60><FROM><R15><C60><LINE>" SKIP
      "<R11><C70><FROM><R15><C70><LINE>" SKIP
      .
      */  

      PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
      "<R22><C1><FROM><R22><C80><LINE>" SKIP    
      "<R20><C10><FROM><R24><C10><LINE>" SKIP
      "<R20><C29><FROM><R24><C29><LINE>" SKIP
      /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
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
          "<=5><R+1> Line     Over/Under%     UOM          Our Item/Description/Vendor ID               Adder      Job#/Fm#        Cost    UOM      Ext Cost" SKIP(1).
*/          
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
