 /* oe/rep/cocprempkg.i  */  
PUT UNFORMATTED "<FArial>"  SKIP
    "<C65><P12>Date: " STRING(TODAY,"99/99/9999")
    "<C1><#1><R+5><C+30><IMAGE#1=" ls-full-img1  SKIP           
    "<=1><R+6><P10>" SKIP
     "      Customer: " cust.name        SKIP
     "        Address: " cust.addr[1] SKIP   
     SPACE(24) cust.addr[2] SKIP
     space(24) v-cust-addr3 skip
     "<||5><C1><FROM><C80><LINE>" SKIP
     "<B><C26><P14>CERTIFICATE OF CONFORMANCE</B><P10>" SKIP(1)
     "This is to certify that the products below conform to customer specifications." SKIP(2)  .
     
