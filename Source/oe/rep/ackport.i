/* oe/rep/ackPort.i */

RUN Format_Date(oe-ord.ord-date,"DD/MM/YYYY", OUTPUT opcDateStringOrdDate).
RUN Format_Date(oe-ord.due-date,"DD/MM/YYYY", OUTPUT opcDateStringDueDate).

PUT "<FArial>".
     PUT "<C1><#1><R+10><C+47><IMAGE#1=" ls-full-img1 SKIP. /* pacific package */ 
           
PUT "<=1>" SKIP.
PUT /*"<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
    "<=2><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"     
   "<P10><=2><R+2>"
     v-comp-add1 AT 8 SKIP
     v-comp-add2  AT 8  SKIP
     v-comp-add3  AT 8 SKIP
     v-comp-add4  AT 8 skip
     v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
     lv-email AT 8 SKIP(1) */
   "<R+10>"
   "<FCourier New>"
   "Faturar para:" SPACE(30) "Enviar para:"  SKIP
   SPACE(5) oe-ord.cust-name 
    ( IF avail bf-shipto THEN bf-shipto.ship-name ELSE oe-ord.cust-name) AT 45 FORM "x(30)" skip
   SPACE(5) oe-ord.addr[1] 
    (IF avail bf-shipto THEN bf-shipto.ship-addr[1] ELSE oe-ord.addr[1]) AT 45 FORM "x(30)" SKIP
   SPACE(5) oe-ord.addr[2] 
    (IF avail bf-shipto THEN bf-shipto.ship-addr[2] ELSE  oe-ord.addr[2]) AT 45 FORM "x(30)" SKIP
   SPACE(5) v-addr3  cShipAdd3 AT 45 SKIP.
/*
 IF lv-display-comp THEN
        PUT "<=2><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".
*/
v-printline = v-printline + 14.
PUT "<||3><R4><C48><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C48><FROM><R6><C80><LINE>" SKIP      
    "<R8><C48><FROM><R8><C80><LINE>" SKIP      
   "<R4><C63><FROM><R6><C63><LINE>" SKIP
   "<R6><C65><FROM><R8><C65><LINE>" SKIP
   "<R8><C65><FROM><R10><C65><LINE>" SKIP
   .

PUT "<FArial><P12><=#3><R-2> <B>Reconhecimento</B> " "<P10>" SKIP
    "<=#3> Identificação do cliente             Contato"
    "<=#3><R+2> Telefone                               Fax" 
    "<=#3><R+4> PO do cliente                       Data do pedido <FCourier New>"
    "<=3><R+1> " oe-ord.cust-no  space(9) cust.contact
    "<=3><R+3> " v-cust-phone  space(7) cust.fax
    "<=3><R+5> " oe-ord.po-no space(5) opcDateStringOrdDate FORMAT "x(10)" .

PUT "<||3><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
"<R21><C1><FROM><R21><C80><LINE>" SKIP    
"<R19><C11><FROM><R23><C11><LINE>" SKIP
"<R19><C22><FROM><R23><C22><LINE>" SKIP
"<R19><C38><FROM><R23><C38><LINE>" SKIP
"<R19><C52><FROM><R23><C52><LINE>" SKIP
"<R19><C65><FROM><R23><C65><LINE>" SKIP
"<R19><C73><FROM><R23><C73><LINE>" SKIP
.
PUT "<FArial><=4><R+.1> <c66> N° do <c74> N° da " SKIP
"<=4><R+1> Data Req.             FOB                     Enviar Via                              Termos                        vendedor              pedido         cotação" SKIP
"<FCourier New><=4><R+3> " opcDateStringDueDate FORMAT "x(10)" space(2)
oe-ord.fob-code FORM "x(11)" SPACE(2)
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(1)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" space(5) v-salesman "<C65.5>" oe-ord.ord-no space(2) v-q-no SKIP.


PUT "<||3><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C6><FROM><R26><C6><LINE>" SKIP
       "<R24><C20><FROM><R26><C20><LINE>" SKIP
     /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
       "<R24><C49><FROM><R26><C49><LINE>" SKIP
       "<R24><C61><FROM><R26><C61><LINE>" SKIP
       "<R24><C72><FROM><R26><C72><LINE>" SKIP
       .

PUT "<FArial><=5><R+1> Linha    N° da peça do cliente                        Descrição                                               Pedido                       Preço             UOM" SKIP(1).
PUT "<FCourier New>"          .
v-printline = v-printline + 6.
