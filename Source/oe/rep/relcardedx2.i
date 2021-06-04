/* oe/rep/relcardedx2.i */     
   
    PUT "<FArial>"  SKIP
            "<C2><R2><#1><R+10><#2><C+38><IMAGE#1=" ls-full-img1  SKIP.
          
    PUT        "<FCourier New>"
               v-comp-add1 AT 5 "Ship To:" AT 58 SKIP
               v-comp-add2 AT 5 shipto.ship-name AT 58 SKIP
               v-comp-add3 AT 5 shipto.ship-addr[1] AT 57 SKIP
               v-comp-add4 AT 5 SKIP
               v-comp-add5 AT 5 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
               lv-email AT 5 SKIP
               "<=2><R+1><C1><FROM><R+7><C81><RECT><||3>" FORM "x(150)" SKIP
               "<=2><R+1><C42><FROM><R+7><C42><LINE>" SKIP
               .
    
    PUT    "<R5><C50><#3>" SKIP
           "<FArial><P14><=#3>"  "<P10>" SKIP
                "<=#3><B>Ticket #: " STRING(oe-relh.release#) "</B>" SKIP(1)
                     
                "<|10><R21><C1><#4><FROM><R25><C81><RECT>" SKIP
                "<R23><C1><FROM><R23><C81><LINE>" SKIP    
                "<R21><C12><FROM><R25><C12><LINE>" SKIP
                "<R21><C24><FROM><R25><C24><LINE>" SKIP
                "<R21><C36><FROM><R25><C36><LINE>" SKIP
                "<R21><C52><FROM><R25><C52><LINE>" SKIP
                "<R21><C67><FROM><R25><C67><LINE>" SKIP
                "<FArial><=4><R+1>    Weight Lbs             Weight Kg           Company Name              PO Number                 Order Number                  Ship Date" SKIP
                "<FCourier New>" v-weight AT 2 (v-weight * 0.453592) AT 14 CoCode AT 35 w-oe-rell.po-no AT 48 w-oe-rell.ord-no AT 69  oe-relh.rel-date AT 84 SKIP
                .
     
     PUT        "<|10><R26><C1><#5><FROM><R30><#6><C81><RECT>" SKIP
                "<R28><C1><FROM><R28><C81><LINE>" SKIP
                "<R26><C24><FROM><R30><C24><LINE>" SKIP
                "<R26><C46><FROM><R30><C46><LINE>" SKIP
                "<FArial><=5><R+0.4><P12><B>" 
                "<C6>Part Number" 
                "<C30>PO On Release" 
                "<C56>FG Item Number</B>" SKIP
                "<FCourier New>" STRING(oe-ordl.part-no) FORMAT "x(15)" AT 3 STRING(oe-relh.release#) AT 32  w-oe-rell.i-no AT 56 SKIP
                .
               
     
     PUT        "<P10><=6><R+1><C1><FROM><R+1><#7><C81><RECT><||3>" FORM "x(150)" SKIP
                "<=6><R+1><C20><FROM><=7><C20><LINE>" SKIP
                "<=6><R+1><C33><FROM><=7><C33><LINE>" SKIP
                "<=6><R+1><C44><FROM><=7><C44><LINE>" SKIP
                "<=6><R+1><C60><FROM><=7><C60><LINE>" SKIP
                "<FArial><=6><R+1>"
                "<C10>Tag#"
                "<C22>Cases/Pallet"
                "<C36>Cases"
                "<C49>Quantity"
                "<C66>Weight Of Pallet" SKIP
                .
      ASSIGN
         v-printline  = 0
         v-rel-qty    = 0
         iRelCase     = 0
         iRelPallet   = 0
         dTotalWeight = 0
         .
      FOR EACH xoe-rell
          WHERE xoe-rell.company EQ cocode
            AND xoe-rell.r-no    EQ oe-relh.r-no
            NO-LOCK,
            EACH oe-ordl
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ xoe-rell.ord-no
                AND oe-ordl.i-no    EQ xoe-rell.i-no
                AND oe-ordl.line    EQ xoe-rell.line
              NO-LOCK:
          PUT "<FArial><=7><R+" + STRING(v-printline) + ">" "<C3>" xoe-rell.tag FORMAT "X(20)" .
          PUT "<C24.5>" (IF xoe-rell.units-pallet NE 0 THEN xoe-rell.units-pallet ELSE 1).
          PUT "<C35>" xoe-rell.cases "<C48>" xoe-rell.qty "<C65>" oe-ordl.t-weight SKIP
          "<C1><FROM><C81><LINE>".
          
          v-rel-qty    = v-rel-qty + xoe-rell.qty.
          iRelCase     = iRelCase +  xoe-rell.cases.
          dTotalWeight = dTotalWeight + oe-ordl.t-weight.
          iRelPallet   = iRelPallet + (IF xoe-rell.units-pallet NE 0 THEN xoe-rell.units-pallet ELSE 1).
          
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
                 "<=8><R+0.4><C48>" v-rel-qty 
                 "<=8><R+0.4><C65>" dTotalWeight
                 "</B><P10>" 
                 .
            v-printline = v-printline + 16.
