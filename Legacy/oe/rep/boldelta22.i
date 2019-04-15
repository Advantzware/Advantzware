/* oe/rep/bolxpr22.i  */  
 
PUT 
    "<FArial>"  SKIP
    "<P22><C+38.5><B>Bill Of Lading</B>    " SKIP
    "<C1><#1><R+5><C+25>"
    "<=1><C3><FGCOLOR=" TRIM(lv-comp-color) + ">"
    /*"<=1><C3><R+1><P20><B>Sold To:</B>"*/
    "<=1><C3><R+1><P15><B>" v-comp-name "</B><FGCOLOR=" TRIM(lv-other-color) + ">" FORM "x(6)" 
    "<P10></B>"

    "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
    "<P14><=1><R+1.5>"  
    v-comp-addr[1] AT 5 SKIP(1)
    v-comp-addr[2] AT 5  SKIP(1)
    v-comp-addr3 AT 5 SKIP 
         /*v-comp-add5 AT 8 SKIP*/
         /*lv-email AT 8*/ "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP(1)
    "<FCourier New>"
     "<P10><b>Ship To:</b>" AT 3.5  SKIP
    v-ship-name AT 5 SKIP
    v-ship-addr[1] AT 5 SKIP
    v-ship-addr[2] AT 5 SKIP
    v-ship-addr3 AT 5 SKIP
    "<R5><C60><#3>" SKIP
    "<FArial><P14><=#3>" "<P14>" SKIP
    "<=#3><B><P18> BOL #:" trim(string(oe-bolh.bol-no)) "<P14></B>" /*"<c66>Page: " +  string(PAGE-NUM - lv-pg-num,">>9") + " of " +  string(lv-tot-pg,">>9")  FORM "x(40)"*/  SKIP(1)
    "<=#3><R+2>Date: " oe-bolh.bol-date  "<P10>"      SKIP
    "<=#3><R+3>" SKIP
    SKIP     
    "<R19><C1><#4><FROM><R23><C81><RECT>" SKIP
    "<R21><C1><FROM><R21><C81><LINE>" SKIP    
    "<R19><C12><FROM><R23><C12><LINE>" SKIP
    "<R19><C25><FROM><R23><C25><LINE>" SKIP
    "<R19><C43><FROM><R23><C43><LINE>" SKIP
    "<R19><C66><FROM><R23><C66><LINE>" SKIP
    "<FArial><=4><R+1>    <b><c5>Date  <c17>FOB   <c30> Trailer #       <C50>Carrier        <c68> Freight Terms</b>" SKIP 
    "<FCourier New><=4><R+3>  " oe-bolh.bol-date SPACE(5) v-fob SPACE(4) oe-bolh.trailer FORMAT "x(20)" SPACE(1) carrier.dscr v-frt-terms SKIP
    "<R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
    "<R24><C13><FROM><R26><C13><LINE>" SKIP
    "<R24><C24><FROM><R26><C24><LINE>" SKIP
    "<R24><C50><FROM><R26><C50><LINE>" SKIP
    "<R24><C56.5><FROM><R26><C56.5><LINE>" SKIP  
    "<R24><C63.5><FROM><R26><C63.5><LINE>" SKIP
    "<R24><C70><FROM><R26><C70><LINE>" SKIP            
    "<R24><C74.5><FROM><R26><C74.5><LINE>" SKIP 
    "<FArial><=5> <b><C1.5>Order#               <c50.5> Amt Per          <c57.5>Bundles/      <c64.5>Qty Per        <c70.5># of           <c75>Total" SKIP 
    "<FArial><=5><R+1><C1.5> PO#   <C17>Order Qty    <c25.5>Item Name / Description     <c50.5> Bundle            <c57.5>Pallet        <c64.5>Pallet         <c70.5>Pallet       <c75>Amount </b>" SKIP
    "<FCourier New>"                                  
    .

v-printline = v-printline + 25.
