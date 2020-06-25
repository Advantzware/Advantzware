
PUT "<FArial>" .
PUT "<C1><#2><R1>" 
    "<P10><=2><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"
            "<P10><=2><R+3>"
            space(3) v-comp-add1  SKIP
            space(3) v-comp-add2 SKIP
            space(3) v-comp-add3 SKIP
            space(3) v-comp-add4 SPACE(2) v-comp-add5 "<C61>Date:" oe-bolh.bol-date SKIP
            
            space(3) "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)"  SKIP
            "<FCourier New>" 
           
            space(3) "To:"  SKIP
            SPACE(3) v-ship-name  skip
            SPACE(3) v-ship-addr[1]    SKIP
            SPACE(3) v-ship-addr[2]    SKIP
            SPACE(3) v-ship-addr3      SKIP.   
            
 PUT "<=2><C3><R+2><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=2><C2><R+1><P20><B>" lv-comp-name "<FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)"
       "<C61>Packing Slip". 
  
 PUT    "<P10><ADJUST=LPI>"
                "<|10><R17><C1><#4><FROM><R21><C81><RECT>" 
                "<R19><C1><FROM><R19><C81><LINE>"     
                "<R17><C25><FROM><R21><C25><LINE>" 
                "<R17><C50><FROM><R21><C50><LINE>"  
                
                "<FArial><=4><R+0.8><P10><ADJUST=LPI>          VENDOR ORDER                                   P.O. Number                                     DATE SHIPPED " SKIP 
                "<FCourier New><=4><R+2.5> " space(3) oe-boll.ord-no SPACE(24)  string(iPurchaseOrder,">>>>>>>>9") FORMAT "x(15)" SPACE(17) string(oe-bolh.bol-date) FORMAT "x(15)"  SKIP
                "<|10><R22><C1><#5><FROM><R24><C81><RECT>"     
                "<R22><C6><FROM><R24><C6><LINE>" 
                "<R22><C23><FROM><R24><C23><LINE>" 
                "<R22><C37><FROM><R24><C37><LINE>" 
                "<R22><C58><FROM><R24><C58><LINE>" 
                "<R22><C64><FROM><R24><C64><LINE>" 
                "<R22><C72.5><FROM><R24><C72.5><LINE>" 
                "<FArial><=5><C5>           <c7>Item#        <C24> VENDOR                                 <C64.5>TOT UNITS <C73.5>TOT UNITS"
                "<FArial><=5><R+1><C2>Line  <c7>(SPLS SKU#)  <C24> MODEL#  <C38> DESCRIPTION  <C59> U.O.M <C64.5> ORDERED <C73.5>SHIPPED    " SKIP(1)
            "<FCourier New><R25>"                                  
            .
   PUT  "<|10><R22><C1><#5><FROM><R58><C81><RECT>"  .
   PUT  "<|10><R58><C1><#6><FROM><R59><C81><RECT>" 
        "<R58><C72.5><FROM><R59><C72.5><LINE>" 
        "<R58><C63><FROM><R59><C63><LINE>" .
   PUT  "<|10><R59><C1><#7><FROM><R65><C81><RECT>"  .
   PUT  "<R58><C2><b>TOTALS</b>"
        "<R60><C2>COMMENTS:"  
        "<R64><C63>Page #:" string(PAGE-NUM,">9") + " of <#PAGES>" FORM "x(20)" "<R25>".  
