/* ----------------------------------------------- po/po-soule.i GDM 06030906  */
/* Purchase Order Print Program for N-K-1-POPRINT = Soule                      */
/* -------------------------------------------------------------------------- */

PUT
    "<FArial>"   SKIP
    "<P14><C+35> <B>Purchase Order</B> <P10>" v-change-ord SKIP
    "<C3><R4><#1><R+30><C+30>" "<IMAGE#1=" ls-full-img1    SKIP     
    "<P10><=1><C3><R8>"
            SKIP.

PUT SKIP(3).

PUT
    "<FCourier New>"
    "Purchase Order To:"  SPACE(30) "Ship To:"  SKIP
    SPACE(5) vend.name v-sname    AT 55 SKIP
    SPACE(5) vend.add1 v-saddr[1] AT 55 SKIP
    SPACE(5) vend.add2 v-saddr[2] AT 55 SKIP
    SPACE(5) vend.city + " " + vend.state + " " + vend.zip FORMAT "x(35)"
             v-scity + " " + v-sstate + " " + v-szip FORMAT "x(35)" AT 55 SKIP
    SPACE(5) "Phone: " vend.area-code SPACE vend.phone SKIP
    SPACE(5) "Fax: " vend.fax-area SPACE vend.fax SKIP

    "<R4><C50><#3>" SKIP
    "<FArial><P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
    "<=#3><R+1><B><P12>PO #: " po-ord.po-no "</B><P10>" SKIP(1)
    "<=#3><R+2>Date: " po-ord.po-date        SKIP
    "<=#3><R+3>Changed Date: " po-ord.po-change-date SKIP
    "<=3><R+4>Date Required: " po-ord.due-date SKIP.

v-printline = v-printline + 15.    

PUT 
    "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
    "<R20><C10><FROM><R24><C10><LINE>" SKIP
    "<R20><C29><FROM><R24><C29><LINE>" SKIP
  /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
    "<R20><C49><FROM><R24><C49><LINE>" SKIP
    "<R20><C55><FROM><R24><C55><LINE>" SKIP
    "<R20><C72><FROM><R24><C72><LINE>" SKIP.

PUT 
    "<FArial><=4><R+1>  Buyer                 Contact                                     Terms                                        FOB        Ship Via                                Freight" SKIP
    "<FCourier New><=4><R+3> "
    po-ord.buyer    
    po-ord.contact FORMAT "x(25)" 
    terms.dscr     FORMAT "x(23)" 
    po-ord.fob-code SPACE(2) 
    carrier.dscr FORMAT "x(20)" " "
    v-freight-dscr.

PUT 
    "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
    "<R25><C5><FROM><R27><C5><LINE>" SKIP
    "<R25><C35><FROM><R27><C35><LINE>" SKIP
    "<R25><C48><FROM><R27><C48><LINE>" SKIP 
    "<R25><C55><FROM><R27><C55><LINE>" SKIP
    "<R25><C67><FROM><R27><C67><LINE>" SKIP .

PUT "<FArial><=5><C7>"
    "<=5><R+1> Line            Our Item/Description/Vendor Item                   Quantity                   UOM              Cost                   Extended Cost" SKIP(1).

PUT "<FCourier New>"          .

v-printline = v-printline + 8.
