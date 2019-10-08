 /* oe/rep/cocprempkg.i  */  
PUT UNFORMATTED "<FArial>"  SKIP
    "<C65><P12>Fecha: " STRING(TODAY,"99/99/9999")
    "<C1><#1><R+5><C+30><IMAGE#1=" ls-full-img1  SKIP           
    "<=1><R+6><P10>" SKIP
     "      Cliente: " cust.name        SKIP
     "  Dirección: " cust.addr[1] SKIP   
     SPACE(24) cust.addr[2] SKIP
     space(24) v-cust-addr3 skip
     "<||5><C1><FROM><C80><LINE>" SKIP
     "<B><C26><P14>Certificado de conformidad</B><P10>" SKIP(1)
     "Esto es para certificar que los productos a continuación cumplen con las especificaciones del cliente." SKIP(2)  .
     
