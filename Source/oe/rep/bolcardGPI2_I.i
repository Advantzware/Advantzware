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
                <C49>Order Number
                <C68.5>Ship Date" SKIP
                "<FCourier New>" tt-boll.weight AT 4 (tt-boll.weight * 0.453592) AT 18 oe-bolh.bol-no AT 39 tt-boll.ord-no AT 61  oe-bolh.rel-date AT 81 SKIP
                .
     
     PUT        "<|10><=1><R+5><C1><#2><FROM><=2><R+5><#3><C81><RECT>" SKIP
                "<=2><R+2><C1><FROM><=2><R+2><C81><LINE>" SKIP
                "<=2><C24><FROM><=3><C24><LINE>" SKIP
                "<=2><C46><FROM><=3><C46><LINE>" SKIP
                "<FArial><=2><R+0.4><P12><B>" 
                "<C6>Part Number" 
                "<C30>PO On Release" 
                "<C56>FG Item Number</B>" SKIP
                "<FCourier New>" STRING(oe-ordl.part-no) FORMAT "x(15)" AT 3 STRING(oe-bolh.release#) AT 32  tt-boll.i-no AT 56 SKIP
                .
                
     
     PUT        "<P10><=3><R+1><C1><FROM><R+1><#4><C81><RECT><||3>" FORM "x(150)" SKIP
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
      ASSIGN
         v-printline  = 0
         v-qty        = 0
         iRelCase     = 0
         iCartonsCase = 0
         iRelPallet   = 0
         dTotalWeight = 0
         .
      
    FOR EACH xoe-boll 
        WHERE xoe-boll.company EQ oe-bolh.company 
        AND xoe-boll.b-no EQ oe-bolh.b-no 
        AND xoe-boll.i-no EQ tt-boll.i-no 
        NO-LOCK:
            PUT "<FArial><=4><R+" + STRING(v-printline) + ">" "<C3>" xoe-boll.tag FORMAT "X(20)" .
            PUT "<C20>" (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1) .
            PUT "<C28>" xoe-boll.cases "<C36>" xoe-boll.qty-case "<C44.5>" xoe-boll.qty "<C57>" xoe-boll.weight "<C70>" (xoe-boll.weight * 0.453592) SKIP
            "<C1><FROM><C81><LINE>".
            v-qty        = v-qty + xoe-boll.qty.
            iRelCase     = iRelCase +  xoe-boll.cases.
            iCartonsCase = iCartonsCase +  xoe-boll.qty-case.
            dTotalWeight = dTotalWeight + xoe-boll.weight.
            iRelPallet   = iRelPallet + (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1).

            v-printline  = v-printline + 1 .   
    END.
    
     PUT        "<=4><C1><FROM><R+" + STRING(v-printline) + "><#8><C81><RECT><||3>" FORM "x(150)" SKIP
                "<=4><C20><FROM><R+" + STRING(v-printline + 3) + "><C20><LINE>" FORM "x(150)" SKIP
                "<=4><C28><FROM><R+" + STRING(v-printline + 3) + "><C28><LINE>" FORM "x(150)" SKIP
                "<=4><C34.5><FROM><R+" + STRING(v-printline + 3) + "><C34.5><LINE>" FORM "x(150)" SKIP
                "<=4><C44.5><FROM><R+" + STRING(v-printline + 3) + "><C44.5><LINE>" FORM "x(150)" SKIP
                "<=4><C53><FROM><R+" + STRING(v-printline + 3) + "><C53><LINE>" FORM "x(150)" SKIP
                "<=4><C67><FROM><R+" + STRING(v-printline + 3) + "><C67><LINE>" FORM "x(150)" SKIP
                .

     PUT         "<=8><C1><FROM><R+3><C81><RECT><||3>" FORM "x(150)" SKIP
                 "<=8><R+0.4><C3><P12><B>Total #: "
                 "<=8><R+0.4><C19>" iRelPallet 
                 "<=8><R+0.4><C26>" iRelCase 
                 "<=8><R+0.4><C34.5>" iCartonsCase 
                 "<=8><R+0.4><C43.5>" v-qty 
                 "<=8><R+0.4><C55>" dTotalWeight
                 "<=8><R+0.4><C69>" (dTotalWeight * 0.453592)
                 "</B><P10>" 
                 .
