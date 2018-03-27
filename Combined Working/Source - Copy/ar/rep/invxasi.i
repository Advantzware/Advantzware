/* oe/rep/invxasi.i */
PUT "<FArial>".
       PUT "<C3><R2><#1><R+13><C+40><IMAGE#1=" ls-full-img1 SKIP  /* company image */ 
          /*  "<P10><=2><R+9>"
            space(3) v-comp-add1 SKIP
            space(3) v-comp-add2 space(74) "Attn: Account Payable Dept."  SKIP
            space(3) v-comp-add3 space(74) "Remit To: 5821 Production Way Langley, BC V3A 4N5"  SKIP
            space(3) v-comp-add4 SKIP
            */
            "<FCourier New><P10><=1><R+10>" SKIP
            space(10) "Bill To:"  "Ship To:" AT 60 SKIP
            SPACE(10) ar-inv.cust-name v-shipto-name AT 60 skip
            SPACE(10) ar-inv.addr[1]   v-shipto-addr[1] AT 60 SKIP
            SPACE(10) ar-inv.addr[2]  v-shipto-addr[2] AT 60 SKIP
            SPACE(10) v-addr3   v-sold-addr3 AT 60 SKIP.
        v-printline = v-printline + 15.
        PUT "<|10><R4><C50><#3><FROM><R10><C80><RECT>" SKIP.
        PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP
            "<R8><C50><FROM><R8><C80><LINE>" SKIP
            "<R4><C62><FROM><R6><C62><LINE>" SKIP
            "<R6><C65><FROM><R8><C65><LINE>" SKIP
            "<R8><C65><FROM><R10><C65><LINE>" SKIP.
        
PUT "<FArial><P12><=#3><R-2> <B>Invoice#: " ar-inv.inv-no "</B><P10>" SKIP
    "<=#3> Customer ID             Contact"
    "<=#3><R+2> Telephone                        Fax" 
    "<=#3><R+4> Customer PO                  Invoice Date <FCourier New>"    
    "<=3><R+1> " ar-inv.cust-no  space(7) cust.contact
    "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999" space(5) cust.fax
    "<=3><R+5> " v-po-no space(3) v-inv-date .

    
PUT "<|10><R20><C1><#4><FROM><R24><C80><RECT>" SKIP                     /*Task# 10301309*/
    "<R22><C1><FROM><R22><C80><LINE>" SKIP    
  /*  "<R20><C11><FROM><R24><C11><LINE>" SKIP*/                 
    "<R20><C22><FROM><R24><C22><LINE>" SKIP
   /* "<R20><C38><FROM><R24><C38><LINE>" SKIP*/
    "<R20><C51><FROM><R24><C51><LINE>" SKIP
  /*  "<R20><C60><FROM><R24><C60><LINE>" SKIP
    "<R20><C72><FROM><R24><C72><LINE>" SKIP */
    .
v-printline = v-printline + 5.


v-termsdscr = "" .      
FIND FIRST terms                                                                /*Task# 10301309*/
        WHERE terms.company EQ cocode
          AND terms.t-code    EQ ar-inv.terms
        NO-LOCK NO-ERROR.
IF AVAIL terms THEN 
    ASSIGN v-termsdscr = terms.dscr  .

PUT "<FArial><=4><R+1><C8>Salesman<C34>Ship Via<C63>Terms                " SKIP         /*Task# 10301309*/
     "<FCourier New><=4><R+3><C3> " v-salesman FORM "x(20)" space(2)
     "<C24>" v-shipvia FORM "x(30)" SPACE(1)
     "<C53>" v-termsdscr FORM "x(30)" space(1) 
    SKIP.



PUT "<|10><R25><C1><#5><FROM><R27><C80><RECT>" SKIP    
                "<R25><C10><FROM><R27><C10><LINE>" SKIP
                "<R25><C58><FROM><R27><C58><LINE>" SKIP
                "<R25><C65><FROM><R27><C65><LINE>" SKIP
                /*"<R28><C68><FROM><R30><C68><LINE>" SKIP  */
                .   
PUT "<FArial><=5><R+1>     Ordered                                                     Description                                        <C61>Price                     Amount" SKIP(1).
v-printline = v-printline + 4.
           

PUT "<FCourier New>".

