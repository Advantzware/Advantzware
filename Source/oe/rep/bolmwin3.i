PUT "<#9><C4><R43><from><R48><C81><RECT><|3>" SKIP. 
        PUT "<R44><C4><FROM><R44><C81><LINE>" SKIP.
        PUT "<R46><C4><FROM><R46><C81><LINE>" SKIP.
        PUT "<R47><C4><FROM><R47><C81><LINE>" SKIP.

        PUT "<R44><C10><FROM><R48><C10><LINE>" SKIP.
        PUT "<R44><C16><FROM><R48><C16><LINE>" SKIP.
        PUT "<R44><C22><FROM><R48><C22><LINE>" SKIP.
        PUT "<R43><C40><FROM><R48><C40><LINE>" SKIP.
        PUT "<R44><C45><FROM><R48><C45><LINE>" SKIP.
        PUT "<R44><C49><FROM><R48><C49><LINE>" SKIP.
        PUT "<R43><C70><FROM><R48><C70><LINE>" SKIP.
        PUT "<R44><C76><FROM><R48><C76><LINE>" SKIP.

        PUT "<=9><R43.2><C10><B><p7> Handling Unit              Package" "<C73> LTL Only </B>" SKIP.
        PUT "<=9><R44.2><C6><B><p6>Qty " "<C11> Type" "<C16> Pallet Qty" "<C29> Type" "<C40> Weight" "<C45> HM(X)"
            "<C71> NMFC" "<C77> Class</B>"
            "<=9><R45><C72><B><p6>No. </B>"
            "<=9><R44><C50><p4>Commodity Description:Commodities requiring special or  " 
            "<=9><R44.5><C50><p4>additional care or attention in handling or stowing must "
            "<=9><R45><C50><p4>be so marked and packaged as to ensure safe transportation  "
            "<=9><R45.5><C50><p4>with ordinary care. See Section 2(e) of NMFC item 360  "

            "<=9><R46.2><p7>" "<C3>" iTotBolQty "<C11>" cProCat FORMAT "x(10)" "<C16>" v-tot-palls "<C23>Pallets"  "<C38>" v-tot-wt "<C50>Printed Matter "
            .
 
        PUT "<#10><R48.5><C4><P4>Where the rate is dependent on value, shippers are required to state specifically in writing the agreed or declared value of the property as follows: The agreed OR declared value " SKIP
            "<R49><C4><P4>of the property is specifically stated by the shipper to be not exceeding__________________per______________________.<P6>" SKIP
            .
        
        PUT "<R50><C4><FROM><R50><C81><LINE>" SKIP.
        PUT "<C5><R-1><P7><b>Notes: Liability limitation for loss or damage in this Shipment may be applicable. See 49 USC $ 14706(C)(1)(A) and (B).</B><P6>" SKIP .
   
        PUT "<#11><C4><R51><from><R59.7><C81><RECT><|3>" SKIP. 
            PUT "<R54><C4><FROM><R54><C81><LINE>" SKIP.
            PUT "<R57><C4><FROM><R57><C30><LINE>" SKIP.
            PUT "<R57><C53.5><FROM><R57><C81><LINE>" SKIP.

            PUT "<R51><C55><FROM><R54><C55><LINE>" SKIP. 
            PUT "<R54><C30><FROM><R59.7><C30><LINE>" SKIP.
            PUT "<R54><C42><FROM><R59.7><C42><LINE>" SKIP.
            PUT "<R54><C53.5><FROM><R59.7><C53.5><LINE>" SKIP.

            PUT "<=11><R51.3><C4><p5> Received. Subject to individually determined rates or contracts that have been agreed upon in writing between the carrier"
                "<=11><R51.9><C4><p5> and shipper, if applicable, otherwise to the rates, classifications, and rules that have been established by the carrier " SKIP 
                "<=11><R52.5><C4><p5> and are available to the shipper, on request, and to all applicable state and federal regulations."
                "<=11><R51.5><C58><B><p5>The carrier shall not make delivery of this shipment "
                "<=11><R52.2><C58><B><p5>without payment of charges and all other lawful fees. "
                "<=11><R53.2><C55><B><p7> Shipper Signature__________________________</B> " 

                "<=11><R54.5><C32><B><p7>Trailer Loaded <C43> Freight Counted "      
                "<=11><R54.6><C6><B><p7>Shipper Signature <C60> Carrier Signature/Date </B>" 
                
                "<=11><R55.6><C30><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By Shipper   "
                "<=11><R56.8><C30><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver/pieces   "
                "<=11><R55.6><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By Shipper   "
                "<=11><R56.6><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver/pallets" SKIP "<R57.1><C44>  said to contain   "
                "<=11><R57.8><C42><B><p6> <FROM><R+0.5><C+1><RECT><||2><R-0.5> By driver    "

                "<=11><R57.5><C4><p4> This is to certify that the above named materials are properly classified, " 
                "<=11><R58><C4><p4>     packaged, marked, and labeled are in proper condition for "
                "<=11><R58.5><C4><p4>     transportion according to the applicable regulations of the DOT. "

                "<=11><R57.5><C54><p4>Carrier acknowledges receipt of packages and required placards. Carrier certifies" 
                "<=11><R58><C54><p4>emergency response information was made available and/or carrier has the DOT "
                "<=11><R58.5><C54><p4>emergency response guidebook or equivalent documentation in the vehicle, property "
                 "<=11><R59><C54><p4>            described above is received in good order,except as noted. "

               .
