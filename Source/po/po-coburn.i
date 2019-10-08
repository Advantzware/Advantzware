/* po/po-protg2.i - Last Change 3/28/16 */

PUT "<C1><#1><FArial>"   SKIP
    "<P14><C+45> <B>Purchase Order</B> <P10>" "     Page " string(PAGE-NUM,">>9") + " of <#PAGES>" FORM "x(20)" SKIP       
    "<C3><R2><#2><R+7><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)"  SKIP 
    "<P12><=1><R+7>" 
    "<P10><=1><C3><R7> " SKIP  "<C3> P.O.Box 447" SKIP

    "<C3> Ashland OH 44805 "  SKIP
    "<C3> Phone 419-368-4051" SKIP 

    "<FCourier New>" SKIP(1)
    space(3) "Purchase Order To:"   "Ship To:" AT 50 SKIP
    SPACE(3) vend.name v-sname AT 50 skip
    SPACE(3) vend.add1 /*v-saddr[1] AT 50*/ SKIP
    SPACE(3) vend.add2 /*v-saddr[2] AT 50*/ SKIP
    SPACE(3) vend.city + " " + vend.state + " " + vend.postal FORM "x(35)"
             /*v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 50*/ SKIP
    "<R4><C50><#3>" SKIP
    "<FArial><P14><=#3>" "<P10>" SKIP
       "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
       "<=#3><R+2>Date: " po-ord.po-date        SKIP
       /*"<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP*/
       "<=3><R+3>Date Required: " po-ord.due-date SKIP(1)
    "<R4><C49.5><#3>" SKIP
       "<=3><R+5>Purchase Order and Job Number must appear on all"  SKIP
       "<=3><R+6>correspondence,invoices,receiving slips and packages."  SKIP(1)
       .
    
      v-printline = v-printline + 10.    
     
      PUT "<|10><R18><C1><#4><FROM><R22><C81><RECT>" SKIP
      "<R20><C1><FROM><R20><C81><LINE>" SKIP    
      "<R18><C10><FROM><R22><C10><LINE>" SKIP
      "<R18><C29><FROM><R22><C29><LINE>" SKIP
      "<R18><C49><FROM><R22><C49><LINE>" SKIP
      "<R18><C55><FROM><R22><C55><LINE>" SKIP
      "<R18><C72><FROM><R22><C72><LINE>" SKIP
      .

      PUT "<FArial><=4><R+1>    Buyer                         Contact                                     Terms                             FOB                Ship Via                       Freight" SKIP
          "<FCourier New><=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(25)" terms.dscr FORM "x(23)" po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr
          .
      
      PUT "<|10><R23><C1><#5><FROM><R25><C81><RECT>" SKIP    
             "<R23><C5><FROM><R25><C5><LINE>" SKIP
             "<R23><C15><FROM><R25><C15><LINE>" SKIP
             "<R23><C20><FROM><R25><C20><LINE>" SKIP 
             "<R23><C50><FROM><R25><C50><LINE>" SKIP 
             /*"<R23><C53><FROM><R25><C53><LINE>" SKIP*/
             "<R23><C60><FROM><R25><C60><LINE>" SKIP
             "<R23><C68><FROM><R25><C68><LINE>" 
             "<R23><C72><FROM><R25><C72><LINE>" SKIP
             .

      PUT "<FArial><=5><C52>Job#/Order#                                " 
          "<=5><R+1> Line         Quantity        UOM      Item Description/Vendor Item#                                Due Date             Cost       UOM    Ext Cost" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
