/* cec/quote/prcsheet.i  Price Sheet form for Southpak*/  
 
PUT "<C3><R4.5><#1><R+6><C+45><IMAGE#1=" ls-full-img1 SKIP 
    "<R4><C2><#2><FROM><R62><C80><RECT><|10><FArial>" SKIP
    "<=2><R+15><From><R+2><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+52><From><R+1><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><C55><FROM><R+1.2><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+13><FROM><R+1><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+8><FROM><R+1><C80><RECT><BGcolor=gray><Fillrect>"
    "<=2><R+7><C2><From><C80><LINE>" skip
    "<=2><R+8><C2><From><C80><LINE><|3>" skip
    "<=2><R+9><C2><From><C80><LINE><|3>" skip
    /*"<=2><R+13><C2><From><C80><LINE>" skip*/
    "<=2><R+14><C2><From><C80><LINE>" 
    "<=2><R+15><C2><From><C80><LINE>" 
    "<=2><R+16><C2><From><C28><LINE>" 
    "<=2><R+17><C2><From><C80><LINE>" 
    "<=2><R+18><C2><From><C80><LINE>" 
    "<=2><R+19><C2><From><C80><LINE>" 
    "<=2><R+20><C2><From><C80><LINE>" 
    "<=2><R+21><C2><From><C80><LINE>" 
    "<=2><R+22><C2><From><C80><LINE>" 
    "<=2><R+23><C2><From><C80><LINE>" 
    "<=2><R+24><C2><From><C80><LINE>" 
    "<=2><R+25><C2><From><C80><LINE>" 
    "<=2><R+26><C2><From><C80><LINE>" 
    "<=2><R+27><C2><From><C80><LINE>" 
    "<=2><R+28><C2><From><C80><LINE>" 
    "<=2><R+29><C2><From><C80><LINE>" 
    "<=2><R+30><C2><From><C80><LINE>" 
    "<=2><R+31><C2><From><C80><LINE>" 
    "<=2><R+32><C2><From><C80><LINE>" 
    "<=2><R+33><C2><From><C80><LINE>" 
    "<=2><R+34><C2><From><C80><LINE>" 
    "<=2><R+35><C2><From><C80><LINE>" 
    "<=2><R+36><C2><From><C80><LINE>" 
    "<=2><R+37><C2><From><C80><LINE>" 
    "<=2><R+38><C2><From><C80><LINE>" 
    "<=2><R+39><C2><From><C80><LINE>" 
    "<=2><R+40><C2><From><C80><LINE>" 
    "<=2><R+41><C2><From><C80><LINE>" 
    "<=2><R+42><C2><From><C80><LINE>" 
    "<=2><R+43><C2><From><C80><LINE>" 
    "<=2><R+44><C2><From><C80><LINE>" 
    "<=2><R+45><C2><From><C80><LINE>" 
    "<=2><R+46><C2><From><C80><LINE>" 
    "<=2><R+47><C2><From><C80><LINE>" 
    "<=2><R+48><C2><From><C80><LINE>" 
    "<=2><R+49><C2><From><C80><LINE>" 
    "<=2><R+50><C2><From><C80><LINE>" 
    "<=2><R+51><C2><From><C80><LINE>" 
    "<=2><R+52><C2><From><C80><LINE>" 
    "<=2><R+53><C2><From><C80><LINE>" 
    "<=2><R+54><C2><From><C80><LINE>" 
    "<=2><R+55><C2><From><C80><LINE>" 
    "<=2><R+56><C2><From><C80><LINE>" skip
    /*"<=2><R+57><C2><From><C80><LINE>" skip*/
    "<=2><C25><From><R+7><C25><LINE>" skip
    "<=2><R+15><C28><From><R+37><C28><LINE>" skip
    "<=2><C55><From><R+8><C55><LINE>" skip
    "<=2><R+8><C45><From><R+5><C45><LINE>" skip
    "<=2><R+15><C55><From><R+37><C55><LINE>" skip
    "<=2><R+1><C28><P8><FArial Narrow>MAIN OFFICE: P.O. Box 28090, Birmingham, AL 35228"
    "<=2><R+2><C30>DISTRIBUTION CENTER: 660 Bessemer Super Hwy"
    "<=2><R+3><C43>Birmingham, AL 35228"
    "<=2><R+4><C30><P7><FArial>LOCAL (205)428-7700  TOLL FREE (800)927-2440"
    "<=2><R+5><C30>FAX: (205)428-6983  E-MAIL sales@southpak.net".

PUT    
    "<=2><C56><P13><B><P13>CUSTOMER ORDER FORM"
    "<=2><R+1.2><C55><From><C80><LINE>"
    "<=2><R+2.5><C55><From><C80><LINE>"
    "<=2><R+4><C55><From><C80><LINE>"
    "<=2><R+5><C55><From><C80><LINE>"
    "<=2><R+6><C55><From><C80><LINE>"
    "<=2><R+7><C55><From><C80><LINE>"
    "<=2><R+1.5><C55.5><B><P9>CUST ID         CONTACT            PAGE</B>"
    "<=2><R+2.5><C55.5>"  lv-cust-id  "  " lv-contact   "    " lv-pg-num " of " lv-tot-pg
    "<=2><R+4><C57.5><B>TELEPHONE                        FAX</B>"
    "<=2><R+5><C57>" lv-tel      "<C70>"  lv-fax
    "<=2><R+7><C8><B><P10>A Certified <I> ""COMPLETE SOURCE"" </I>Master Distributor <C56>E-Mail:</B>" lv-email "</B>"
    
    "<=2><R+8><C20><P12><B>INVOICE TO:  <C65>SHIP TO:"
    
    "<=2><R+13><C3><B><P10>DATE          FILE NO.            SALESPERSON & E-MAIL                          VIA                TERM                CUSTOMER PO#</B>"
    "<=2><R+15><C10><B><P10>ITEM NUMBER       <C38>PRODUCT    <C56>MIN       ON     ORDER     U      UNIT"
    "<=2><R+16><C5>Cust. Item             Our Item <C37>DESCRIPTION  <C56>QTY     HAND     NOW     M     PRICE"
    "<=2><R+16><C14><FROM><R+36><C14><LINE><|3>"
    "<=2><R+15><C60><FROM><R+37><C60><LINE><|3>"
    "<=2><R+15><C64.8><FROM><R+37><C64.8><LINE><|3>"
    "<=2><R+15><C71><FROM><R+37><C71><LINE><|3>"
    "<=2><R+15><C74><FROM><R+37><C74><LINE><|3>"
    "<=2><R+1><C62><FROM><R+3><C62><LINE><|3>"
    "<=2><R+1><C72><FROM><R+3><C72><LINE><|3>"
    "<=2><R+4><C68><FROM><R+3><C68><LINE><|3>"
    "<=2><R+13><C10><FROM><R+2><C10><LINE><|3>"
    "<=2><R+13><C17><FROM><R+2><C17><LINE><|3>"
    "<=2><R+13><C45><FROM><R+2><C45><LINE><|3>"
    "<=2><R+13><C53><FROM><R+2><C53><LINE><|3>"
    "<=2><R+13><C62><FROM><R+2><C62><LINE><|3>"    
    "<=2><C30><R+52><P10><B>COMMENTS AND SUGGESTIONS</B>"
    "<=2><C19><R+56><P8><FArial Narrow><B><I>NOTE: Pricing effective for 30 days from date indicated above."
          " Subject to changes without notice." 
    "<=2><C18><R+57>ALL PRICING IS SUBJECT TO FINAL APPROVAL BY SOUTH-PAK, Inc. UPON RECEIPT OF ORDER.<P10><FArial></B></I>"

    .

PUT "<=2><R+9><FCourier New><P10>   " /*v-soldto[1]*/ SPACE(21) /*v-shipto[1]*/ SKIP
    v-soldto[2] AT 5 v-shipto[2] AT 56 SKIP
    v-soldto[3] AT 5 v-shipto[3] AT 56 SKIP
    v-soldto[4] AT 5 v-shipto[4] AT 56 SKIP
    "<=2><R+14><P9>" quotehd.quo-date  space(3) quotehd.q-no space(5) v-sman SPACE(7)
       quotehd.carrier space(5) quotehd.terms
    .
     

              
