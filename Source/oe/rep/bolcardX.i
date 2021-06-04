/* oe/rep/relcardedx2.i */     
   
    PUT "<FArial>"  SKIP
            "<C2><R2><#1><R+10><#2><C+38><IMAGE#1=" ls-full-img3  SKIP.
          
    PUT        "<FCourier New>"
               v-comp-add1 AT 5 "Ship To:" AT 58 SKIP
               v-comp-add2 AT 5 v-ship-name AT 58 SKIP
               v-comp-add3 AT 5 v-ship-addr[1] AT 57 SKIP
               v-comp-add4 AT 5 v-ship-addr[2] AT 57 SKIP
               v-comp-add5 AT 5 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
               lv-email AT 5 SKIP
               "<=2><R+1><C1><FROM><R+7><C81><RECT><||3>" FORM "x(150)" SKIP
               "<=2><R+1><C42><FROM><R+7><C42><LINE>" SKIP
               .
    
    PUT    "<R5><C50><#3>" SKIP
           "<FArial><P14><=#3>"  "<P10>" SKIP
                "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP(1)
                     
                "<|10><R21><C1><#4><FROM><R25><C81><RECT>" SKIP
                "<R23><C1><FROM><R23><C81><LINE>" SKIP    
                "<R21><C12><FROM><R25><C12><LINE>" SKIP
                "<R21><C24><FROM><R25><C24><LINE>" SKIP
                "<R21><C36><FROM><R25><C36><LINE>" SKIP
                "<R21><C52><FROM><R25><C52><LINE>" SKIP
                "<R21><C67><FROM><R25><C67><LINE>" SKIP
                "<FArial><=4><R+1>    Weight (Lbs)           Weight (Kg)         Company Name              PO Number                 Order Number                  Ship Date" SKIP
                "<FCourier New>" tt-boll.weight AT 2 (tt-boll.weight * 0.453592) AT 14 CoCode AT 35 tt-boll.po-no AT 48 tt-boll.ord-no AT 69  oe-bolh.rel-date AT 84 SKIP
                .
     
     PUT        "<|10><R26><C1><#5><FROM><R30><#6><C81><RECT>" SKIP
                "<R28><C1><FROM><R28><C81><LINE>" SKIP
                "<R26><C24><FROM><R30><C24><LINE>" SKIP
                "<R26><C46><FROM><R30><C46><LINE>" SKIP
                "<FArial><=5><R+0.4><P12><B>" 
                "<C6>Part Number" 
                "<C30>PO On Release" 
                "<C56>FG Item Number</B>" SKIP
                "<FCourier New>" STRING(oe-ordl.part-no) FORMAT "x(15)" AT 3 STRING(oe-bolh.release#) AT 32  tt-boll.i-no AT 56 SKIP
                .
                 
     
     PUT        "<P10><=6><R+1><C1><FROM><R+1><#7><C81><RECT><||3>" FORM "x(150)" SKIP
                "<=6><R+1><C20><FROM><=7><C20><LINE>" SKIP
                "<=6><R+1><C33><FROM><=7><C33><LINE>" SKIP
                "<=6><R+1><C44><FROM><=7><C44><LINE>" SKIP
                "<=6><R+1><C60><FROM><=7><C60><LINE>" SKIP
                "<FArial><=6><R+1>"
                "<C10>Tag#"
                "<C22.5>Cases/Pallet"
                "<C36>Cases"
                "<C49>Quantity"
                "<C66>Weight Of Pallet" SKIP
                .
      ASSIGN
         v-printline  = 0
         v-qty        = 0
         iRelCase     = 0
         iRelPallet   = 0
         dTotalWeight = 0
         .
      FOR EACH xoe-boll 
         WHERE xoe-boll.company EQ oe-bolh.company 
           AND xoe-boll.b-no EQ oe-bolh.b-no 
          NO-LOCK:
          PUT "<FArial><=7><R+" + STRING(v-printline) + ">" "<C3>" xoe-boll.tag FORMAT "X(20)" .
          PUT "<C24.5>" (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1).
          PUT "<C35>" xoe-boll.cases "<C48>" xoe-boll.qty "<C66.5>" xoe-boll.weight SKIP
          "<C1><FROM><C81><LINE>".
          
          v-qty        = v-qty + xoe-boll.qty.
          iRelCase     = iRelCase +  xoe-boll.cases.
          dTotalWeight = dTotalWeight + xoe-boll.weight.
          iRelPallet   = iRelPallet + (IF xoe-boll.tot-pallets NE 0 THEN xoe-boll.tot-pallets ELSE 1).
          
          v-printline  = v-printline + 1 .
      END.
     
         
     PUT         "<=7><C1><FROM><R+" + STRING(v-printline) + "><#8><C81><RECT><||3>" FORM "x(150)" SKIP
                "<=7><C20><FROM><R+" + STRING(v-printline + 3) + "><C20><LINE>" FORM "x(150)" SKIP
                "<=7><C33><FROM><R+" + STRING(v-printline + 3) + "><C33><LINE>" FORM "x(150)" SKIP
                "<=7><C44><FROM><R+" + STRING(v-printline + 3) + "><C44><LINE>" FORM "x(150)" SKIP
                "<=7><C60><FROM><R+" + STRING(v-printline + 3) + "><C60><LINE>" FORM "x(150)" SKIP
                .
     
     PUT         "<=8><C1><FROM><R+3><C81><RECT><||3>" FORM "x(150)" SKIP
                 "<=8><R+0.4><C3><P12><B>Total #: "
                 "<=8><R+0.4><C23>" iRelPallet 
                 "<=8><R+0.4><C34>" iRelCase 
                 "<=8><R+0.4><C48>" v-qty 
                 "<=8><R+0.4><C65>" dTotalWeight
                 "</B><P10>" 
                 .
            v-printline = v-printline + 16.
