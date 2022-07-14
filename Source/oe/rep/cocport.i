 /* oe/rep/cocport.i  */  
PUT UNFORMATTED "<FArial>"  SKIP
    "<C65><P12>Data: " STRING(TODAY,"99/99/9999")
    "<C1><#1><R+5><C+30><IMAGE#1=" ls-full-img1  SKIP           
    "<=1><R+6><P10>" SKIP
     "      Cliente: " cust.name        SKIP
     "        Endereço: " cust.addr[1] SKIP   
     SPACE(24) cust.addr[2] SKIP
     space(24) v-cust-addr3 skip
     "<||5><C1><FROM><C80><LINE>" SKIP
     "<B><C26><P14>CERTIFICADO DE CONFORMIDADE</B><P10>" SKIP(1)
     "O objetivo deste documento é certificar que os produtos abaixo estão em conformidade com as especificaçães do cliente." SKIP(2)  .
     
