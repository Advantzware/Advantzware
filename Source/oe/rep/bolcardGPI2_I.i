/* oe/rep/bolcardGPI2_I.i */     

    PUT    "<FArial><P10>" SKIP
                "<|10><R2><C1><#1><FROM><R+4><C81><RECT>" SKIP
                "<=1><R+2><C1><FROM><=1><R+2><C81><LINE>" SKIP    
                "<=1><C15><FROM><R+4><C15><LINE>" SKIP
                "<=1><C29><FROM><R+4><C29><LINE>" SKIP
                "<=1><C44><FROM><R+4><C44><LINE>" SKIP
                "<=1><C63><FROM><R+4><C63><LINE>" SKIP
                "<FArial><=1><R+1>
                <C4>Weight (Lbs)
                <C18>Weight (Kg)
                <C35>Bol#
                <C49>Lot Number
                <C68.5>Bol Date" SKIP
                "<FCourier New>" tt-boll.weight FORMAT ">>,>>9" AT 4 (tt-boll.weight * 0.453592) FORMAT ">>,>>9" AT 18 oe-bolh.bol-no AT 39 oe-ordl.lot-no AT 61  tt-boll.bol-date AT 81 SKIP
                .
     
     PUT        "<|10><=1><R+5><C1><#2><FROM><=2><R+5><#3><C81><RECT>" SKIP
                "<=2><R+2><C1><FROM><=2><R+2><C81><LINE>" SKIP
                "<=2><C24><FROM><=3><C24><LINE>" SKIP
                "<=2><C46><FROM><=3><C46><LINE>" SKIP
                "<FArial><=2><R+0.4><P12><B>" 
                "<C6>Part Number" 
                "<C30>PO Number" 
                "<C56>FG Item Number</B>" SKIP
                "<FCourier New>" STRING(itemfg.part-no) FORMAT "x(15)" AT 3 IF AVAIL po-ordl THEN STRING(po-ordl.po-no) ELSE "" AT 32  tt-boll.i-no AT 56 SKIP
                .
    RUN pDisplayTagsHeadings.           
     
      ASSIGN
         v-printline  = 0
         v-qty        = 0
         iRelCase     = 0
         iCartonsCase = 0
         iRelPallet   = 0
         dTotalWeight = 0
         .
      
    FOR EACH xoe-boll NO-LOCK 
        WHERE xoe-boll.company EQ tt-boll.company 
        AND xoe-boll.b-no EQ tt-boll.b-no 
        AND xoe-boll.i-no EQ tt-boll.i-no
        BY xoe-boll.tag
        :
            PUT "<FArial><=4><R+" + STRING(v-printline) + ">" "<C3>" xoe-boll.tag FORMAT "X(20)" .
            PUT "<C20>" (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1) .
            PUT "<C28>" xoe-boll.cases "<C36>" xoe-boll.qty-case "<C44.5>" xoe-boll.qty "<C57>" xoe-boll.weight FORMAT ">>,>>9" "<C70>" (xoe-boll.weight * 0.453592) FORMAT ">>,>>9" SKIP
            "<C1><FROM><C81><LINE>".
            v-qty        = v-qty + xoe-boll.qty.
            iRelCase     = iRelCase +  xoe-boll.cases.
            iCartonsCase = iCartonsCase +  xoe-boll.qty-case.
            dTotalWeight = dTotalWeight + xoe-boll.weight.
            iRelPallet   = iRelPallet + (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1).

            v-printline  = v-printline + 1 .   
    
            IF v-printline GT 30 THEN
            DO:
                RUN pDisplayTagsBorder(v-printline).
                PAGE.
                v-printline = 0.
                PUT "<FArial><P10><R1><#3>" .
                RUN pDisplayTagsHeadings.
            END.
    
    END.
    
    RUN pDisplayTagsBorder(v-printline).


     PUT         "<=8><C1><FROM><R+3><C81><RECT><||3>" FORM "x(150)" SKIP
                 "<=8><R+0.4><C3><P12><B>Total #: "
                 "<=8><R+0.4><C19>" iRelPallet 
                 "<=8><R+0.4><C26>" iRelCase 
                 "<=8><R+0.4><C34.5>" iCartonsCase 
                 "<=8><R+0.4><C43.5>" v-qty 
                 "<=8><R+0.4><C54>" dTotalWeight FORMAT ">>,>>>,>>9"
                 "<=8><R+0.4><C68>" (dTotalWeight * 0.453592) FORMAT ">>,>>>,>>9"
                 "</B><P10>" 
                 .
    
     
                 
PROCEDURE pDisplayTagsHeadings:

    PUT        "<P10><||3><=3><R+1><C1><FROM><R+1><#4><C81><RECT><||3>" FORM "x(150)" SKIP
                "<=3><R+1><C20><FROM><=4><C20><LINE>" SKIP
                "<=3><R+1><C28><FROM><=4><C28><LINE>" SKIP
                "<=3><R+1><C34.5><FROM><=4><C34.5><LINE>" SKIP
                "<=3><R+1><C44.5><FROM><=4><C44.5><LINE>" SKIP
                "<=3><R+1><C53><FROM><=4><C53><LINE>" SKIP
                "<=3><R+1><C67><FROM><=4><C67><LINE>" SKIP
                "<FArial><=3><R+1>"
                "<C10>Tag#"
                "<C22>Pallets"
                "<C29.5>Cases"
                "<C35>Cartons/Case"
                "<C46>Quantity"
                "<C53.5>Weight Of Pallet(Lbs)"
                "<C68>Weight Of Pallet(Kg)" SKIP
                .
      
END PROCEDURE.

PROCEDURE pDisplayTagsBorder:
DEFINE INPUT PARAMETER ipiCount AS INTEGER NO-UNDO.

    PUT    
    "<=4><C1><FROM><R+" + STRING(ipiCount) + "><#8><C81><RECT><||3>" FORM "x(150)" SKIP
    "<=4><C20><FROM><R+" + STRING(ipiCount) + "><C20><LINE>" FORM "x(150)" SKIP
    "<=4><C28><FROM><R+" + STRING(ipiCount) + "><C28><LINE>" FORM "x(150)" SKIP
    "<=4><C34.5><FROM><R+" + STRING(ipiCount) + "><C34.5><LINE>" FORM "x(150)" SKIP
    "<=4><C44.5><FROM><R+" + STRING(ipiCount) + "><C44.5><LINE>" FORM "x(150)" SKIP
    "<=4><C53><FROM><R+" + STRING(ipiCount) + "><C53><LINE>" FORM "x(150)" SKIP
    "<=4><C67><FROM><R+" + STRING(ipiCount) + "><C67><LINE>" FORM "x(150)" SKIP
    .
    
    PUT "<R65><C37><P10>Page: " + STRING(PAGE-NUM - iPageNum,">>9") + " of [=@endPage" + TRIM(STRING(iPageNum)) + "-@startPage" + TRIM(STRING(iPageNum)) + "] " FORM "x(120)" .            

END PROCEDURE.                 
