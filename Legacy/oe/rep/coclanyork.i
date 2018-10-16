/* oe/rep/coclanyork.i  */  
PUT "<FCourier New>"
    "<C3><R2><#1><R+8><C+45><IMAGE#1=" ls-full-img1 
    "<B><R4><C45><P14>CERTIFICATE OF COMPLIANCE</B><P10>" SKIP
    "<R12><C1>"
    SPACE(7) "Customer:" SPACE(39) "Ship To:"  SKIP
    SPACE(15) cust.name  v-ship-name AT 64 SKIP
    SPACE(15) cust.addr[1]  v-ship-addr[1] AT 64 SKIP
    SPACE(15) cust.addr[2]  v-ship-addr[2] AT 64 SKIP
    SPACE(15) v-cust-addr3  v-ship-addr3 AT 64 SKIP.
PUT SKIP(1) "<||5><C3><FROM><C80><LINE>" SKIP
    "<P10>"
    "<C4>It is hereby certified that all articles in the shipment below and in the quantity given " SKIP
    "<C4>are in conformance with the requirements, specifications and drawings applicable to the " SKIP
    "<C4>above Purchase Order. Substantiated evidence is in our files that the accompanying " SKIP
    "<C4>shipment complies with material and process specifications." 
    SKIP(2) 
    .
     
