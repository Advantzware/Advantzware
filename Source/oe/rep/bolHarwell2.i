/* oe/rep/bolHarwell2.i */  


PUT
  "<FArial>"  
    "<P10><R1><C70><B>Page #:</B>" string(PAGE-NUM,">9")  SKIP 
  "<#1><P10><ADJUST=LPI><C50><B> "  SKIP
  "<=1><R+1><C25></B>"            SKIP
  "<=1><R+2><C25><B><C60>BON DE LIVRAISON/<R+1><C60>DELIVERY RECEIPT:" oe-bolh.bol-no   "</B>"
  "<C3><R-4><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
  "<FCourier New>"
  "   Expédié A/Ship To:" SPACE(21) "Vendu A/Sold To:"  SKIP
  SPACE(5) v-comp-name v-ship-name AT 45 skip
  SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
  SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
  SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP 
  "<R5><C47><#3>" SKIP
  "<FArial><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
  
  SKIP              
  /*"<=#3><R+3>Page #:             " PAGE-NUM SKIP*/
  "<=#3><R+4><C60>DATE   :                     " oe-bolh.bol-date        SKIP
  "<=#3><R+5> " SKIP
  /*SKIP*/
  SKIP(1).

PUT
  "<P10><ADJUST=LPI>"
  "<|{&incl2}><R17><C2><#4><FROM><R21><C83><RECT>" SKIP
  "<R19><C2><FROM><R19><C83><LINE>" SKIP    
  "<R17><C12><FROM><R21><C12><LINE>" SKIP
  "<R17><C27><FROM><R21><C27><LINE>" SKIP 
  "<R17><C46><FROM><R21><C46><LINE>" SKIP
  "<R17><C68><FROM><R21><C68><LINE>" SKIP
  "<FArial><=4><R+1><P10><ADJUST=LPI>        DATE                   F.A.B./F.O.B.                   CAMION / TRUCK #           EXPEDIE VIA/SHIPPED VIA          TERMES/TERMS" SKIP 
  "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(7) v-trailer SPACE(3) carrier.dscr v-frt-terms SKIP
  
  "<|{&incl2}><R22><C2><#5><FROM><R24><C83><RECT>" SKIP    
  "<R22><C11><FROM><R24><C11><LINE>" SKIP
  "<R22><C33><FROM><R24><C33><LINE>" SKIP
  "<R22><C53><FROM><R24><C53><LINE>" SKIP
  "<R22><C61><FROM><R24><C61><LINE>" SKIP
  "<R22><C70><FROM><R24><C70><LINE>" SKIP
  "<R22><C81><FROM><R24><C81><LINE>" SKIP
  "<FArial><=5><C3>QTE COMM<C12>No.COMMANDE/DESCRIPTION<C34>No.COMMANDE DU CLIENT<C54># UNITES<C62>QTE / UN.<C71>QTE LIVREE<C81.2>P/"
  "<FArial><=5><R+1><C3>QTY ORD<C12>ORDER NO./DESCRIPTION<C34>CUSTOMER PO NUMBER<C54># UNITS<C62>QTY/UNIT<C71>QTY SHIPPED<C81.6>C"
  "<FCourier New>"                                  
  .

v-printline = v-printline + 24.
