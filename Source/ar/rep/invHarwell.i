/* ar/rep/invHarwell.i */

PUT 
    "<FArial><C+25><#1><=1>" SKIP.

/* Business Form Logo */
PUT 
    "<C1><#2>" 
    "<C2><P10><R2><#1><R+10><C+37><IMAGE#1=" ls-full-img1  SKIP.

/* Top Right Invoice Section - preprint*/
PUT 
    "<FArial><P12><=#3><R2><B><C55>"
    "FACTURE / INVOICE" 
    "<FArial><P7></B>" /*Shrink font, turn off bold */
    "<R4><C55>Facture No"
    "<R4.25><C65>:"
    "<R4.5><C55>Invoice No"
    "<R6><C55>Date de la Facture"
    "<R6.25><C65>:"
    "<R6.5><C55>Invoice Date"
    "<R8.25><C55>Page"
    "<R8.25><C65>:"
    "<R10><C55>Bon de Livraison No"
    "<R10.25><C65>:"
    "<R10.5><C55>Delivery Receipt No"
    .
    
    
/* Top right Invoice Section - data */     
PUT "<FCourier New><P10>"
    "<R4.25><C67>" TRIM(STRING(ar-inv.inv-no)) 
    "<R6.25><C67>" v-inv-date 
    "<R8.25><C67>" TRIM(STRING(PAGE-NUM - v-page-num,">>9"))
    "<R10.25><C67>" TRIM(STRING(lv-bol-no)) 
    .

/* Address information */
PUT 
    "<FArial><P8>"
    "<R13><C2>Vendu à"
    "<R13><C41>Expédié à"
    "<R13.8>"
    "<R13.8><C2>Sold To"
    "<R13.8><C41>Ship To"
    "<FCourier New><P10>"
    ar-inv.cust-name AT 8 
    v-shipto-name AT 56 SKIP 
    cust.addr[1] FORMAT "x(42)" AT 8
    v-shipto-addr[1] FORMAT "x(42)" AT 56 SKIP
    cust.addr[2] FORMAT "x(42)" AT 8 
    v-shipto-addr[2] FORMAT "x(42)" AT 56 SKIP
    v-addr3 FORMAT "x(42)" AT 8 
    v-sold-addr3 FORMAT "x(42)" AT 56 SKIP
/*    cAddr4 FORMAT "x(42)" AT 8          */
/*    cShipAddr4 FORMAT "x(42)" AT 56 SKIP*/
    .

PUT "</B><P10>".
    
/* First header block - box */
PUT "<R20.5><C1><#4><FROM><R23.5><C80><RECT><||3>"  /* Rectangle */
    "<R22><C1><FROM><R22><C80><LINE><||3>"          /* Horiz. Divider */
    "<R20.5><C16><FROM><R23.5><C16><LINE><||3>"     /* Vert Line */
    "<R20.5><C37><FROM><R23.5><C37><LINE><||3>"     /* Vert Line */
    "<R20.5><C53><FROM><R23.5><C53><LINE><||3>"     /* Vert Line */
    "<R20.5><C69><FROM><R23.5><C69><LINE><||3>"     /* Vert Line */
    .
/* First header block - labels and data */
PUT 
    "<FArial><P8>"
    "<R21><C2>CONDITIONS / TERMS"
    "<R21><C17>VENDEUR / SALESMAN"
    "<R21><C38>EXPEDIE VIA / SHIP VIA"
    "<R21><C54>CAMION NO/TRUCK NO"
    "<R21><C70>F.A.B./F.O.B."
    "<FCourier New><P10>"
    "<R22.3><C2>" ar-inv.terms-d    FORMAT "x(15)" 
    "<R22.3><C17>" v-salesman       FORMAT "x(30)"
    "<R22.3><C38>" v-shipvia        FORMAT "x(20)"
    "<R22.3><C70>" v-fob            FORMAT "x(12)" 
    SKIP.

/* Line list title box */
PUT 
    "<R24><C1><#5><FROM><R25.5><C80><RECT><||3>"      /* Rectangle */    
    "<R24><C8><FROM><R25.5><C8><LINE><||3>"           /* Vert Line */ 
    "<R24><C34><FROM><R25.5><C34><LINE><||3>"         /* Vert Line */
    "<R24><C49><FROM><R25.5><C49><LINE><||3>"         /* Vert Line */
    "<R24><C58><FROM><R25.5><C58><LINE><||3>"         /* Vert Line */
    "<R24><C60><FROM><R25.5><C60><LINE><||3>"         /* Vert Line */
    "<R24><C70><FROM><R25.5><C70><LINE><||3>"         /* Vert Line */
    .   
/* Line list labels */
PUT 
    "<FArial><P7>"
    "<R24.1><C1.5>QTE COMM"
    "<R24.1><C14>COMMANDE NO/DESCRIPTION"
    "<R24.1><C36>NO COMMANDE CLIENT"
    "<R24.1><C50>QTE EXPEDIEE"
    "<R24.1><C58.5>P"
    "<R24.5><C62>PRIX/PRICE"
    "<R24><C73>MONTANT"
    "<R24.8><C1.7>QTY ORD"
    "<R24.8><C15>ORDER NO/DESCRIPTION"
    "<R24.8><C38>CUSTOMER PO#"
    "<R24.8><C50>QTY SHIPPED"
    "<R24.8><C58.59>C"
    "<R24.8><C73.5>AMOUNT"
    .

ASSIGN 
v-printline = v-printline + 25.
PUT "<FCourier New><P9>".
