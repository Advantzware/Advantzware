/* oe/rep/bolHarwell2.i */  

PUT 
    "<FArial><C+25><#1><=1>" SKIP.

/* Business Form Logo */
PUT 
    "<C1><#2>" 
    "<R2><C2><P10><#1><R+10><C+37><IMAGE#1=" ls-full-img1  SKIP.

/* Top Right BOL Section - preprint*/
PUT 
    "<FArial><P12><=#3><B>"
    "<R2><C55>REÇU DE LIVRAISON"
    "<R3><C55>DELIVERY RECEIPT" 
    "<FArial></B>" /* turn off bold */
    "<R5><C50><P7>REÇU DE LIVRAISON"
    "<R5.25><C65><P12>:"
    "<R5.5><C50><P7>DELIVERY RECEIPT NO"
    "<R7><C50><P7>DATE D'EXPÉDITION"
    "<R7.25><C65><P12>:"
    "<R7.5><C50><P7>SHIPPING DATE"
    "<R9.25><C50><P12>Page"
    "<R9.25><C65>:"
    .

/* Top right BOL Section - data */     
PUT "<FCourier New><P10>"
    "<R5.25><C67>" TRIM(STRING(oe-bolh.bol-no)) 
    "<R7.25><C67>" oe-bolh.bol-date 
    "<R9.25><C67>" TRIM(STRING(PAGE-NUM,">>9"))
    .

/* Address Headers */
PUT
  "<FArial><P7><B>"
  "<R11.7><C3>EXPÉDIÉ À"
  "<R12.5><C3>SHIP TO" 
  "<R11.7><C41>VENDU À"
  "<R12.5><C41>SOLD TO"
  "</B>"
  .
  
/* Address Data */
PUT 
  "<FCourier New><P10>"
  "<R12><C10>" v-ship-name
  "<R12><C48>" v-comp-name 
  "<R13><C10>" v-ship-addr[1] 
  "<R13><C48>" v-comp-addr[1] 
  "<R14><C10>" v-ship-addr[2] 
  "<R14><C48>" v-comp-addr[2] 
  "<R15><C10>" v-ship-addr3
  "<R15><C48>" v-comp-addr3 
  . 
  
/* First header rectangle */
PUT
  "<R17><C2><#4><FROM><R19.5><C84><RECT>" SKIP  /* First Rectangle */
  "<R18><C2><FROM><R18><C84><LINE>" SKIP        /* First Horizontal Line */
  "<R17><C27><FROM><R19.5><C27><LINE>" SKIP     /* First set of vertical lines (3) */
  "<R17><C46><FROM><R19.5><C46><LINE>" SKIP 
  "<R17><C68><FROM><R19.5><C68><LINE>" SKIP
  .

/* Labels for first rectangle */
PUT
  "<FArial><P7><B>"
  "<R17.2><C8>VENDEUR / SALESMAN"
  "<R17.2><C31>EXPÉDIÉ VIA / SHIP VIA"
  "<R17.2><C52>CAMION N° / TRUCK NO."
  "<R17.2><C73>F.A.B./F.O.B."
  . 

/* Second header rectangle */
PUT   
  "<R20><C2><#4><FROM><R22><C84><RECT>" SKIP  /* Second Rectangle */
  "<R20><C05><FROM><R22><C05><LINE>" SKIP     /* Second set of vertical lines (9) */
  "<R20><C10.2><FROM><R22><C10.2><LINE>" SKIP 
  "<R20><C38><FROM><R22><C38><LINE>" SKIP
  "<R20><C53><FROM><R22><C53><LINE>" SKIP     /* Second set of vertical lines (9) */
  "<R20><C58><FROM><R22><C58><LINE>" SKIP 
  "<R20><C66><FROM><R22><C66><LINE>" SKIP
  "<R20><C72><FROM><R22><C72><LINE>" SKIP     /* Second set of vertical lines (9) */
  "<R20><C74><FROM><R22><C74><LINE>" SKIP 
  "<R20><C81><FROM><R22><C81><LINE>" SKIP
  .
  
/* Labels for second rectangle */
PUT
  "<FArial><P5><B>"
  "<R20.5><C2.2>COL:IS"
  "<R20.5><C5.2>QTÉ COMM."
  "<R20.5><C54.2># UNITÉS"
  "<R20.5><C60>QTÉ PAR UN."
  "<R20.5><C67>QTÉ LIVRÉE"
  "<R21.2><C2.2>PKGS"
  "<R21.2><C5.2>QTY ORD'D"
  "<R21.2><C54.2># UNITS"
  "<R21.2><C60>QTY PER UNIT"
  "<R21.2><C67>QTY SHIPPED"

  "<FArial><P7><B>"
  "<R20.3><C16.8>N° COMMANDE /DESCRIPTION"
  "<R20.3><C39>N° COMMANDE DU CLIENT"
  "<R20.3><C72.5>P"
  "<R20.7><C72.9>/"
  "<R20.3><C76>POIDS"
  "<R21><C17.8>ORDER NO./DESCRIPTION"
  "<R21><C39>CUSTOMER P.O. NUMBER"
  "<R21><C73>C"
  "<R21><C75.5>WEIGHT"
  /* Stupid checkmark */
  "<FWingdings><P14><R20.5><C81.5>ü"
  "<FArial><P7><B>"
  . 
 
v-printline = v-printline + 24.
