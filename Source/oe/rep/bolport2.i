/* oe/rep/bolport.i */  

RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

PUT
  "<FArial>"  
    "<P9><R1><C70><B>N° da página:</B>" string(PAGE-NUM,">9")  SKIP 
  "<#1><P9><ADJUST=LPI><C45><B>CONHECIMENTO DE EMBARQUE DIRETO"  SKIP
  "<=1><R+1><C25><C50>N° do conhecimento de </B>"            SKIP
  "<=1><R+2><C25><B><C50>embarque: " oe-bolh.bol-no   "</B>"
  "<=1><R+3.5><UNITS=INCHES><C50><FROM><C+20><R+3><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE= " + string(oe-bolh.bol-no) + ">" FORMAT "x(250)"
  "<C3><R-7><#1><R+5><C+45><IMAGE#1=" ls-full-img1 SKIP
  "<FCourier New><C3><=1><R+1><C30><P18><B>Cópia do<P10></B> "
  "<C3><=1><R+3><C30><P18><B>motorista<P10></B> "
  "<C3><=1><R+7> PO Box 39505, Louisville, KY 40233 "
  "<C3><=1><R+8> Telefone:800 518-6305 Fax: 502 935-3354 " skip(1)
  "<FCourier New>"
  "Faturar para:" SPACE(25) "Enviar para:"  SKIP
  SPACE(5) v-comp-name v-ship-name AT 45 skip
  SPACE(5) v-comp-addr[1] v-ship-addr[1] AT 45 SKIP
  SPACE(5) v-comp-addr[2] v-ship-addr[2] AT 45 SKIP
  SPACE(5) v-comp-addr3 v-ship-addr3 AT 45 SKIP 
  "<R5><C50><#3>" SKIP
  "<FArial><P14><=#3>" "<P10><ADJUST=LPI>" SKIP
  
  SKIP              
  /*"<=#3><R+3>Page #:             " PAGE-NUM SKIP*/
  "<=#3><R+4>Data  :                     " oe-bolh.bol-date        SKIP
  "<=#3><R+5>Horário de chegada do cliente________________" SKIP
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
  "<FArial><=4><R+1><P10><ADJUST=LPI>          Data                           FOB                             N° do reboque                          Transportadora                     Termos de frete" SKIP 
  "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) v-fob space(7) v-trailer SPACE(3) carrier.dscr v-frt-terms SKIP
  "<|{&incl2}><R22><C1><#5><FROM><R24><C81><RECT>" SKIP    
  "<R22><C13><FROM><R24><C13><LINE>" SKIP
  "<R22><C28><FROM><R24><C28><LINE>" SKIP
  "<R22><C58><FROM><R24><C58><LINE>" SKIP
  "<R22><C64><FROM><R24><C64><LINE>" SKIP
  "<R22><C71><FROM><R24><C71><LINE>" SKIP
  "<FArial><=5><C2>N° do item            N° do pedido de compra          Nome do item/N° de peça do cliente                                Por"
  "<FArial><=5><R+1><C2>N° do pedido              N° do trabalho                               Descrição do produto                        unidade    unidade           Total         " SKIP(1)
  "<FCourier New>"                                  
  .

v-printline = v-printline + 24.
