/* po/po-xprem.i */

PUT 
         "<C1><#1><FArial>"   SKIP
         "<P14><C+45> <B>Purchase Order</B> <P10>" v-change-ord SKIP       
       "<C3><R2><#2><R+8><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(60)"  SKIP 
       /*"<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
          "<P10><=1><R+8>"   
      /*    v-comp-add1  AT 8 SKIP
          v-comp-add2  AT 8  SKIP
          v-comp-add3  AT 8 SKIP
          v-comp-add4  AT 8 SKIP
          v-comp-add5  AT 8 /*"<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)"*/ skip
          lv-email AT 8 */
          "<FCourier New>" SKIP(1)
         space(3) "Purchase Order To:"   "Ship To:" AT 50 SKIP
         SPACE(3) vend.name v-sname AT 50 skip
         SPACE(3) vend.add1 v-saddr[1] AT 50 SKIP
         SPACE(3) vend.add2 v-saddr[2] AT 50 SKIP
         SPACE(3) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 50 SKIP
         
       "<R4><C50><#3>" SKIP
       "<FArial><P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
          "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+2>Date: " po-ord.po-date        SKIP
          "<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP
          "<=3><R+4>Date Required: " po-ord.due-date SKIP
       .
    /*IF lv-display-comp THEN 
        PUT "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=1><C3><R+1><P20><B>" lv-comp-name "</B><P10><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 
   */

      v-printline = v-printline + 10.    
     
      PUT "<|10><R18><C1><#4><FROM><R22><C80><RECT>" SKIP
      "<R20><C1><FROM><R20><C80><LINE>" SKIP    
      "<R18><C10><FROM><R22><C10><LINE>" SKIP
      "<R18><C29><FROM><R22><C29><LINE>" SKIP
      /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
      "<R18><C49><FROM><R22><C49><LINE>" SKIP
      "<R18><C55><FROM><R22><C55><LINE>" SKIP
      "<R18><C72><FROM><R22><C72><LINE>" SKIP
      .

      PUT "<FArial><=4><R+1>      Buyer                         Contact                                     Terms                             FOB                Ship Via                        Freight" SKIP
          "<FCourier New><=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(25)" terms.dscr FORM "x(23)" po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr
          .
      
      PUT "<|10><R23><C1><#5><FROM><R25><C80><RECT>" SKIP    
             "<R23><C5><FROM><R25><C5><LINE>" SKIP
             "<R23><C15><FROM><R25><C15><LINE>" SKIP
             "<R23><C20><FROM><R25><C20><LINE>" SKIP 
             "<R23><C47><FROM><R25><C47><LINE>" SKIP
             "<R23><C53><FROM><R25><C53><LINE>" SKIP
             "<R23><C62><FROM><R25><C62><LINE>" SKIP
             "<R23><C68><FROM><R25><C68><LINE>" 
             "<R23><C72><FROM><R25><C72><LINE>" SKIP
             .

      PUT "<FArial><=5><C55>Job#/Fm#                                Ext Cost" 
          "<=5><R+1> Line         Quantity        UOM    Our Item/CP#/Description/Vendor ID               Adder      Due Date        Cost     UOM     Ext MSF" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
