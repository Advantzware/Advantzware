/* oe/rep/bolprem2.i */  

RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

PUT
  "<FArial>"  
    "<P10><R1><C70><B>Page #:</B>" string(PAGE-NUM,">9") + " of <#PAGES>" FORM "x(20)"  SKIP 
  "<#1><P10><ADJUST=LPI><C50><B>Connaissement / Bill of Lading "  SKIP
  "<=1><R+1><C25></B>"            SKIP
  "<=1><R+2><C25><B><C50>Connaissement # / Bill of Lading #: " oe-bolh.bol-no   "</B>"
  "<=1><R+3.5><UNITS=INCHES><C50><FROM><C+20><R+3><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE= " + string(oe-bolh.bol-no) + ">" FORMAT "x(250)"
  "<C3><R-7><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
  "<FCourier New>"
  "Vendu à / Bill To:" SPACE(20) "Livré a / Ship To:"  SKIP
  SPACE(5) v-comp-name v-ship-name AT 45 skip
  SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
  SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
  SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP 
  "<R5><C50><#3>" SKIP
  "<FArial><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
  
  SKIP              
  /*"<=#3><R+3>Page #:             " PAGE-NUM SKIP*/
  "<=#3><R+4>Date  :                     " oe-bolh.bol-date        SKIP
  "<R10><C40>Heure du quai client/Customer Dock time_____________________" SKIP
  /*SKIP*/
  SKIP(1).

PUT
  "<P10><ADJUST=LPI>"
  "<|{&incl2}><R17><C1><#4><FROM><R21><C81><RECT>" SKIP
  "<R19><C1><FROM><R19><C81><LINE>" SKIP    
  "<R17><C12><FROM><R21><C12><LINE>" SKIP
  "<R17><C27><FROM><R21><C27><LINE>" SKIP 
  "<R17><C46><FROM><R21><C46><LINE>" SKIP
  "<R17><C66><FROM><R21><C66><LINE>" SKIP
  "<FArial><=4><P10><ADJUST=LPI><C68>Frais de Transport /" SKIP
  "<FArial><=4><R+1><P10><ADJUST=LPI>          Date                           FOB                   Bandeannonce / Trailer#          Transporteur / Carrier                <C68>Freight Terms" SKIP 
  "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(7) v-trailer SPACE(3) carrier.dscr v-frt-terms SKIP
  "<|{&incl2}><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
  "<R22><C13.5><FROM><R24><C13.5><LINE>" SKIP
  "<R22><C28><FROM><R24><C28><LINE>" SKIP
  "<R22><C58><FROM><R24><C58><LINE>" SKIP
  "<R22><C64><FROM><R24><C64><LINE>" SKIP
  "<R22><C71><FROM><R24><C71><LINE>" SKIP
  "<FArial><=5><C1.5>Article/Item#                   PO#                        Description des marchandises et particularités       Unite/       Par/Per"
  "<FArial><=5><R+1><C1.5>Commande/Order#        JOB#                        / Product Description                                              Unit           Unit             Total         " SKIP(1)
  "<FCourier New>"                                  
  .

v-printline = v-printline + 24.
