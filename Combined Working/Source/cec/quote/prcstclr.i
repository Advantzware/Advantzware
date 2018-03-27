/* cec/quote/prcstclr.i  Price Sheet form */  
/*DEF VAR li-cline AS INT NO-UNDO.*/
 
PUT "<C3><R4.5><#1><R+6><C+55><IMAGE#1=" ls-full-img1 SKIP 
    "<R4><C2><#2><FROM><R62><C80><RECT><|10><FArial>" SKIP
    "<=2><R+16><From><R+2><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+52><From><R+1><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><C55><FROM><R+1.2><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+14><FROM><R+1><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+8><FROM><R+1><C80><RECT><BGcolor=gray><Fillrect>"

    "<=2><R+7><C2><From><C80><LINE>" skip
    "<=2><R+8><C2><From><C80><LINE><|3>" skip
    "<=2><R+9><C2><From><C80><LINE><|3>" skip
    /*"<=2><R+13><C2><From><C80><LINE>" skip*/
    "<=2><R+14><C2><From><C80><LINE>" skip
    "<=2><R+15><C2><From><C80><LINE>" skip
    "<=2><R+16><C2><From><C28><LINE>" skip
    "<=2><R+17><C2><From><C80><LINE>" skip
    "<=2><R+18><C2><From><C80><LINE>" skip
    "<=2><R+19><C2><From><C80><LINE>" skip
    "<=2><R+20><C2><From><C80><LINE>" skip
    "<=2><R+21><C2><From><C80><LINE>" skip
    "<=2><R+22><C2><From><C80><LINE>" skip
    "<=2><R+23><C2><From><C80><LINE>" skip
    "<=2><R+24><C2><From><C80><LINE>" skip
    "<=2><R+25><C2><From><C80><LINE>" skip
    "<=2><R+26><C2><From><C80><LINE>" skip
    "<=2><R+27><C2><From><C80><LINE>" skip
    "<=2><R+28><C2><From><C80><LINE>" skip
    "<=2><R+29><C2><From><C80><LINE>" skip
    "<=2><R+30><C2><From><C80><LINE>" skip
    "<=2><R+31><C2><From><C80><LINE>" skip
    "<=2><R+32><C2><From><C80><LINE>" skip
    "<=2><R+33><C2><From><C80><LINE>" skip
    "<=2><R+34><C2><From><C80><LINE>" skip
    "<=2><R+35><C2><From><C80><LINE>" skip
    "<=2><R+36><C2><From><C80><LINE>" skip
    "<=2><R+37><C2><From><C80><LINE>" skip
    "<=2><R+38><C2><From><C80><LINE>" skip
    "<=2><R+39><C2><From><C80><LINE>" skip
    "<=2><R+40><C2><From><C80><LINE>" skip
    "<=2><R+41><C2><From><C80><LINE>" skip
    "<=2><R+42><C2><From><C80><LINE>" skip
    "<=2><R+43><C2><From><C80><LINE>" skip
    "<=2><R+44><C2><From><C80><LINE>" skip
    "<=2><R+45><C2><From><C80><LINE>" skip
    "<=2><R+46><C2><From><C80><LINE>" skip
    "<=2><R+47><C2><From><C80><LINE>" skip
    "<=2><R+48><C2><From><C80><LINE>" skip
    "<=2><R+49><C2><From><C80><LINE>" skip
    "<=2><R+50><C2><From><C80><LINE>" skip
    "<=2><R+51><C2><From><C80><LINE>" skip
    "<=2><R+52><C2><From><C80><LINE>" skip
    "<=2><R+53><C2><From><C80><LINE>" skip
    /*"<=2><C25><From><R+7><C25><LINE>" SKIP */
    "<=2><R+16><C28><From><R+37><C28><LINE>" skip
    "<=2><C55><From><R+7><C55><LINE>" skip
    "<=2><R+8><C45><From><R+6><C45><LINE>" skip
    "<=2><R+16><C55><From><R+37><C55><LINE>" skip

    "<=2><C3><P20>" lv-comp-name
    "<=2><R+2><C3><P9>" v-comp-add1
    "<=2><R+3><C3>" v-comp-add2
    "<=2><R+4><C3>" v-comp-add3
    "<=2><R+5><C3>" v-comp-add4
    "<=2><R+6><C3>" lv-comp-email

    "<=2><C56><P13><B><P13>CUSTOMER ORDER FORM"
    "<=2><R+1.2><C55><From><C80><LINE>"
    "<=2><R+2.5><C55><From><C80><LINE>"
    "<=2><R+4><C55><From><C80><LINE>"
    "<=2><R+5><C55><From><C80><LINE>"
    "<=2><R+6><C55><From><C80><LINE>"
    "<=2><R+7><C55><From><C80><LINE>"
    "<=2><R+1.5><C56><B><P9>CUST ID       CONTACT          PAGE</B>"
    "<=2><R+2.5><C56>"  lv-cust-id  "<C62>" lv-contact   "<C72>" lv-pg-num " of " lv-tot-pg
    "<=2><R+4><C57.5><B>TELEPHONE                        FAX</B>"
    "<=2><R+5><C57>" lv-tel      "<C70>"  lv-fax
    "<=2><R+6><C8><B><P10><C56>E-Mail:</B>" lv-email "</B>"
    "<=2><R+7><C8><B><P10>A Certified <I> ""COMPLETE SOURCE"" </I>Master Distributor" "</B>"
    
    "<=2><R+8><C20><P12><B>INVOICE TO:  <C65>SHIP TO:"
    
    "<=2><R+14><C3><B><P10>DATE          FILE NO.            Sales Rep / Phone                                VIA                Terms            CUSTOMER PO#</B>"
    "<=2><R+16><C10><B><P10>ITEM NUMBER       <C38>PRODUCT    <C56>MIN       ON        U           UNIT"
    "<=2><R+17><C5>Cust. Item             Our Item <C37>DESCRIPTION  <C56>QTY     HAND     M          PRICE"
    "<=2><R+17><C14><FROM><R+36><C14><LINE><|3>"
    "<=2><R+16><C60><FROM><R+37><C60><LINE><|3>"
    "<=2><R+16><C64.8><FROM><R+37><C64.8><LINE><|3>"
    "<=2><R+16><C69><FROM><R+37><C69><LINE><|3>"
   /* "<=2><R+16><C74><FROM><R+37><C74><LINE><|3>"*/
    "<=2><R+1><C62><FROM><R+3><C62><LINE><|3>"
    "<=2><R+1><C72><FROM><R+3><C72><LINE><|3>"
    "<=2><R+5><C68><FROM><R+1><C68><LINE><|3>"
    "<=2><R+14><C10><FROM><R+2><C10><LINE><|3>"
    "<=2><R+14><C17><FROM><R+2><C17><LINE><|3>"
    "<=2><R+14><C45><FROM><R+2><C45><LINE><|3>"
    "<=2><R+14><C53><FROM><R+2><C53><LINE><|3>"
    "<=2><R+14><C62><FROM><R+2><C62><LINE><|3>"  
    "<=2><C30><R+52><P10><B>COMMENTS AND SUGGESTIONS</B>"
    "<=2><C18><R+57><P10><FArial></B></I>" 
    
    .

PUT "<=2><R+9><FCourier New><P10>   " v-soldto[1] SPACE(21) v-shipto[1]  SKIP
    v-soldto[2] AT 5 v-shipto[2] AT 56 SKIP
    v-soldto[3] AT 5 v-shipto[3] AT 56 SKIP
    v-soldto[4] AT 5 v-shipto[4] AT 56 SKIP
           "Phone:" AT 56  vshiphone  SKIP
    "<=2><R+15><P9>" quotehd.quo-date  space(3) quotehd.q-no space(5) v-sman FORMAT "x(15)" SPACE(1)  vphone  SPACE(7)
       quotehd.carrier space(6) quotehd.terms
    .
  li-cline = 1.
  do i = 1 to 5:
      if quotehd.comment[i] ne "" THEN DO: 
          put "<C1><R" string(56 + li-cline,">9") + "><C6>" quotehd.comment[i]. 
          li-cline = li-cline + 1.
      END.
  end.

              
