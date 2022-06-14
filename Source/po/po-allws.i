/* ----------------------------------------------- po/po-allws.i GDM 04200905 */
/* Purchase Order Print Program for N-K-1-POPRINT = Allwest                   */
/* -------------------------------------------------------------------------- */

PUT 
   "<FArial>"   "<P14><C+35> <B>Purchase Order</B> <P10>" v-change-ord SKIP(1)       
   "<C1><#1><R+9><C+48>" "<IMAGE#1=" + ls-full-img1 FORM "x(200)"  SKIP 
   "<P10><=1><R+4>"    
   SKIP(6)
   "<FCourier New>"
   "Purchase Order To:" SPACE(30) "Ship To:"  SKIP.

IF AVAIL vend THEN
         PUT SPACE(5) vend.name v-sname AT 55 skip
         SPACE(5) vend.add1 v-saddr[1] AT 55 SKIP
         SPACE(5) vend.add2 v-saddr[2] AT 55 SKIP
         SPACE(5) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 55 SKIP.
ELSE PUT v-sname AT 55 skip
         v-saddr[1] AT 55 SKIP
         v-saddr[2] AT 55 SKIP
         v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 55 SKIP.
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
             "<R25><C42><FROM><R27><C42><LINE>" SKIP
             "<R25><C49><FROM><R27><C49><LINE>" SKIP
             "<R25><C60><FROM><R27><C60><LINE>" SKIP
             "<R25><C68><FROM><R27><C68><LINE>" 
             "<R25><C72><FROM><R27><C72><LINE>" SKIP
             .
      PUT "<FArial><=5><C7>Quantity"
          "<=5><R+1> Line
                <C5> Over/Under%
               <C16> UOM
               <C21> Our Item/Description/Vendor ID
               <C43> Adder
               <C53> Job#
               <C62> Cost
               <C68> UOM
               <C73> Ext Cost" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
