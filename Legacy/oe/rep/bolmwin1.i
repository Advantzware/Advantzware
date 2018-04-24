/* oe/rep/bolmwin.i */    
     
            
        PUT  "<FCourier New>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT UNFORMATTED   
               "<C25><R1.5><P10> " "<B> BILL OF LADING - SHORT FORM - NOT NEGOTIABLE " .
        PUT "<FCourier New><=4><R+3> " 
           "<R3><C5><P8> Date    " dBoldate  " <R3><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>" FORM "x(20)"  
         "<|10><R4><C4><#5><FROM><R21.5><C81><RECT>" SKIP  .
        PUT "<R4><C42><FROM><R21.5><C42><LINE>" SKIP.
        PUT "<R5.5><C4><FROM><R5.5><C42><LINE>" SKIP.
        PUT "<R9.5><C4><FROM><R9.5><C81><LINE>" SKIP.
        PUT "<R11><C4><FROM><R11><C42><LINE>" SKIP.
        PUT "<R15><C4><FROM><R15><C81><LINE>" SKIP.
        PUT "<R16.5><C4><FROM><R16.5><C42><LINE>" SKIP.
        PUT "<R18><C42><FROM><R18><C81><LINE>" SKIP.

        PUT "<=5><R4.3><C18><B><p7> SHIP FROM </B>" SKIP.
        PUT "<=5><R4.3><C45><B><p7> Bill Of Lading Number:<C65>"  iBolno FORMAT ">>>>>>>>>9"  "</B>" SKIP.
        
        PUT "<=5><R5.5><C5><B><p7>"  v-comp-name FORMAT "x(30)"     "</B>" 
            "<=5><R6><C5><B><p7>"  v-comp-addr[1] FORMAT "x(30)"    "</B>" 
            "<=5><R6.5><C5><B><p7>"  v-comp-addr[2] FORMAT "x(30)"  "</B>" 
            "<=5><R7><C5><B><p7>"  v-comp-addr3 FORMAT "x(30)"      "</B>" 
            .
        
        PUT "<=5><R8><C7><B><p7> SID No.: </B>" SKIP.
        PUT "<R8.8><C14><FROM><R8.8><C36><LINE>" SKIP.

        PUT "<=5><R9.8><C18><B><p7> SHIP TO </B>" SKIP.
        PUT "<=5><R9.8><C42.5><B><p7> Carrier name: " carrier.dscr FORMAT "x(30)" "</B>" SKIP.
        
        PUT "<=5><R11><C5><B><p7>"  v-ship-name FORMAT "x(30)"  "</B>" 
            "<=5><R11.5><C5><B><p7>"  v-ship-addr[1] FORMAT "x(30)"  "</B>" 
            "<=5><R12><C5><B><p7>"  v-ship-addr[2] FORMAT "x(30)"  "</B>" 
            "<=5><R12.5><C5><B><p7>"  v-ship-addr3 FORMAT "x(30)"  "</B>"  .

        PUT "<=5><R13.5><C7><B><p7> SID No.: </B>" SKIP.
        PUT "<R14.3><C13><FROM><R14.3><C30><LINE>" SKIP.

        PUT "<=5><R11><C47><B><p7> Trailer Number: </B>" SKIP.
        PUT "<R11.8><C57><FROM><R11.8><C73><LINE>" SKIP.

        PUT "<=5><R13><C47><B><p7> Seal Number(S): </B>" SKIP.
        PUT "<R13.8><C57><FROM><R13.8><C73><LINE>" SKIP.

        PUT "<=5><R15.3><C12><B><p7> THIRD PART FREIGHT CHARGES BILL TO </B>" SKIP.
        PUT "<R18.8><C4.5><B><p7> Special Instructions:  </B> <C19>" v-ship-i[1]   SKIP.
        PUT "<R19.5><C4.5><B><p7> Do Not Stack </B><C19>"  v-ship-i[2]  .
        PUT  "<R20.3><p7><C19>"  v-ship-i[3] .

        PUT "<=5><R15.3><C44><B><p7> SCAC: </B>" SKIP.
        PUT "<R16.1><C50><FROM><R16.1><C55><LINE>" SKIP.

        PUT "<=5><R16.3><C43><B><p7> Pro Number: </B>" SKIP.
        PUT "<R17.2><C50><FROM><R17.2><C63><LINE>" SKIP.

        PUT "<=5><R18.3><C42.5><B><p6> Freight Charge Terms(Freight charges are prepaid unless marked otherwise): </B>" SKIP
            "<=5><R19><C43><B><p7>" v-frt-terms FORMAT "x(30)"  .

      
       PUT "<R22><C36><P8><b> CUSTOMER ORDER INFORMATION </b>"
           "<|10><R23><C4><#6><FROM><R42><C81><RECT>" 
           "<R24><C4><FROM><R24><C81><LINE>"  
           "<R25><C4><FROM><R25><C81><LINE>"
           "<R26><C4><FROM><R26><C81><LINE>"
           "<R27><C4><FROM><R27><C81><LINE>"
           "<R28><C4><FROM><R28><C81><LINE>"
           "<R29><C4><FROM><R29><C81><LINE>"
           "<R30><C4><FROM><R30><C81><LINE>"
           "<R31><C4><FROM><R31><C81><LINE>"
           "<R32><C4><FROM><R32><C81><LINE>"
           "<R33><C4><FROM><R33><C81><LINE>"
           "<R34><C4><FROM><R34><C81><LINE>"
           "<R35><C4><FROM><R35><C81><LINE>"
           "<R36><C4><FROM><R36><C81><LINE>"
           "<R37><C4><FROM><R37><C81><LINE>"
           "<R38><C4><FROM><R38><C81><LINE>"
           "<R39><C4><FROM><R39><C81><LINE>"
           "<R40><C4><FROM><R40><C81><LINE>"
           "<R41><C4><FROM><R41><C81><LINE>"
            
            "<R23><C13><FROM><R41><C13><LINE>" 
            "<R23><C40><FROM><R42><C40><LINE>"
            "<R23><C47><FROM><R42><C47><LINE>"
            "<R23><C53><FROM><R42><C53><LINE>" 
            "<R23><C59><FROM><R42><C59><LINE>"
            "<R23><C68><FROM><R42><C68><LINE>"
            
           "<=6><R23.2><C15><B><p4>Kind of Package, Description of Articles, Special Marks & Exceptions "
             "<C40><P6> # of Pallets <C47> Weight-lbs <C53> Total Qty <C59> Purchage Order# <C68> Additionel Shipper Info"  SKIP
           .
