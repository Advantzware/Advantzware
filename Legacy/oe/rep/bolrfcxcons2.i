/* oe/rep/bolrfcxcons2.i */  

RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

PUT
  "<FMS Sans Serif>"  
    "<P10><R1><C70><B>Page #:</B>" string(PAGE-NUM,">9")  SKIP 
  "<#1><P10><ADJUST=LPI><C50><B>STRAIGHT BILL OF LADING"  SKIP
  "<=1><R+1><C25></B>"            SKIP
  "<=1><R+3><C25><B><C50>Bill of Lading #: " oe-bolh.bol-no   "</B>"
  "<R-4><C1><#2><R+9><C+60><IMAGE#1=" ls-full-img1 SKIP
  "<FMS Sans Serif>"
  "Bill To:" SPACE(30) "<C35>Ship To:"  SKIP
  SPACE(5) v-comp-name "<C39>" v-ship-name skip
  SPACE(5) v-comp-addr[1] "<C39>" v-ship-addr[1]  SKIP
  SPACE(5) v-comp-addr[2] "<C39>" v-ship-addr[2]  SKIP
  SPACE(5) v-comp-addr3 "<C39>" v-ship-addr3  SKIP 
  "<R5><C50><#3>" SKIP
  "<FMS Sans Serif><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
  
  SKIP              
  /*"<=#3><R+3>Page #:             " PAGE-NUM SKIP*/
  "<=#3><R+4>Date  :                     " oe-bolh.bol-date        SKIP
  "<=#3><R+5>Customer Dock time_____________________" SKIP
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
  "<FMS Sans Serif><=4><R+1><P10><ADJUST=LPI>          Date                           FOB                                Trailer#                                    Carrier                                   Freight Terms" SKIP 
  "<FMS Sans Serif><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(7) v-trailer SPACE(3) carrier.dscr v-frt-terms SKIP
  "<|{&incl2}><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
  "<R22><C13><FROM><R24><C13><LINE>" SKIP
  "<R22><C28><FROM><R24><C28><LINE>" SKIP
  "<R22><C58><FROM><R24><C58><LINE>" SKIP
  "<R22><C64><FROM><R24><C64><LINE>" SKIP
  "<R22><C71><FROM><R24><C71><LINE>" SKIP
  "<FMS Sans Serif><=5><C5>Item#                         PO#                                 Item Name/Customer Part#                                          Per"
  "<FMS Sans Serif><=5><R+1><C5>Order#                       JOB#                               Product Description                                     Unit           Unit             Total         " SKIP(1)
  "<FMS Sans Serif>"                                  
  .

v-printline = v-printline + 24.
