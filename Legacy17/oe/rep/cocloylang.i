 /* oe/rep/cocprempkg.i  */  
PUT UNFORMATTED "<FArial>"  SKIP
    /*"<C65><P12>Date: " STRING(TODAY,"99/99/9999")*/
    "<C1><#1><R+6><C+40><IMAGE#1=" ls-full-img1  SKIP           
    "<=1><R+6><P10>" SKIP
     "      Customer: " cust.name  FORMAT "x(30)"       SKIP
     "        Address: " cust.addr[1] FORMAT "x(30)" SKIP   
     SPACE(24) cust.addr[2] FORMAT "x(30)" SKIP
     space(24) v-cust-addr3 FORMAT "x(40)" skip
     "<||5><C1><FROM><C80><LINE>" SKIP
     "<B><C26><P14>CERTIFICATE OF COMPLIANCE</B><P10>" SKIP(1)
     "In accordance with the required quality assurance procedures and internal specifications, Loy Lange Box Company certifies " SKIP
     "that it has manufactured the products listed below, and that they are in compliance." 
      skip(1) 
    
     "1. All material is in compliance with current specifications." SKIP
     "2. All process checks have been performed and documented." SKIP
     "3. Finished pallets comply with customer stacking specifications."  SKIP(2)

  .
     
