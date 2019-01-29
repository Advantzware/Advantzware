/* oe/rep/bolxpr22.i  */  
 
PUT 
    "<FArial>"  SKIP
    "<P20><C+38.5><B>Bill Of Lading</B>    " SKIP
    "<C1><#1><R+5><C+25>"
    "<=1><C3><FGCOLOR=" TRIM(lv-comp-color) + ">"
    /*"<=1><C3><R+1><P20><B>Sold To:</B>"*/
    "<=1><C3><R+1><P20><B>" v-comp-name "</B><FGCOLOR=" TRIM(lv-other-color) + ">" FORM "x(6)" 
    "<P10></B>"

    "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
    "<P14><=1><R+3>"  
    v-comp-addr[1] AT 8 SKIP
    v-comp-addr[2] AT 8  SKIP
    v-comp-addr3 AT 8 SKIP
    v-comp-add4 AT 8 SKIP
         /*v-comp-add5 AT 8 SKIP*/
         /*lv-email AT 8*/ "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1)
    "<FCourier New>"
    SPACE(30) "<P10>Ship To:" AT 45  SKIP
    SPACE(5) /*v-comp-name*/ v-ship-name AT 45 SKIP
    SPACE(5) /*v-comp-addr[1]*/ v-ship-addr[1] AT 45 SKIP
    SPACE(5) /*v-comp-addr[2]*/ v-ship-addr[2] AT 45 SKIP
    SPACE(5) /*v-comp-addr3*/ v-ship-addr3 AT 45 SKIP
    "<R5><C60><#3>" SKIP
    "<FArial><P14><=#3>" "<P14>" SKIP
    "<=#3><B>BOL #: <P16>" oe-bolh.bol-no "<P10></B>" /*"<c66>Page: " +  string(PAGE-NUM - lv-pg-num,">>9") + " of " +  string(lv-tot-pg,">>9")  FORM "x(40)"*/  SKIP(1)
    "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
    "<=#3><R+3>" SKIP
    SKIP     
    "<R19><C1><#4><FROM><R23><C81><RECT>" SKIP
    "<R21><C1><FROM><R21><C81><LINE>" SKIP    
    "<R19><C12><FROM><R23><C12><LINE>" SKIP
    "<R19><C25><FROM><R23><C25><LINE>" SKIP
    "<R19><C43><FROM><R23><C43><LINE>" SKIP
    "<R19><C66><FROM><R23><C66><LINE>" SKIP
    "<FArial><=4><R+1>    Date                    FOB                                 Trailer #                                     Carrier                                            Freight Terms" SKIP 
    "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob SPACE(4) oe-bolh.trailer FORMAT "x(20)" SPACE(3) carrier.dscr v-frt-terms SKIP
    "<R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
    "<R24><C13><FROM><R26><C13><LINE>" SKIP
    "<R24><C26><FROM><R26><C26><LINE>" SKIP
    "<R24><C52><FROM><R26><C52><LINE>" SKIP
    "<R24><C59><FROM><R26><C59><LINE>" SKIP  
    "<R24><C66><FROM><R26><C66><LINE>" SKIP
    "<R24><C72><FROM><R26><C72><LINE>" SKIP            
    "<R24><C76.5><FROM><R26><C76.5><LINE>" SKIP 
    "<FArial><=5>                                                             <c28>Item Name /                             <c52.5> Boxes/          <c59.5>Bundles/      <c66.5>Qty Per        <c72.5>#            <c77.5>Total" SKIP 
    "<FArial><=5><R+1> Order Qty / FG #              PO# / Order#             <c28>Description                             <c52.5> Code            <c59.5>Pallet        <c66.5>Pallet         <c72.5>Pallet       <c77.5>Qty " SKIP(1)
    "<FCourier New>"                                  
    .

v-printline = v-printline + 25.
