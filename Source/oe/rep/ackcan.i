/* oe/rep/ackxprnt.i */

PUT "<FArial>".
     PUT "" SKIP. /* pacific package */ 
           
PUT "<=1>" SKIP.
IF AVAIL bf-shipto AND bf-shipto.broker THEN DO:
PUT "<C1><#2>" 
    "<=2><R+1>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"     
   "<P12><=2><R+2>"
     v-comp-add1 AT 8 SKIP
     v-comp-add2  AT 8  SKIP
     v-comp-add3  AT 8 SKIP
     v-comp-add4  AT 8 SKIP
     v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
     lv-email AT 8 SKIP(3).
END.
ELSE DO: 
    PUT "<C2><R2><#1><R+11><C+45><IMAGE#1=" ls-full-img1  SKIP .
END.
   PUT
   "<FCourier New>"
   "<R-1>Factur� �:" SPACE(30) "Vendu �:"  SKIP
   SPACE(5) oe-ord.cust-name 
    ( IF oe-ord.sold-name = "" THEN oe-ord.cust-name ELSE oe-ord.sold-name) AT 45 FORM "x(30)" SKIP
   SPACE(5) oe-ord.addr[1] 
    (IF oe-ord.sold-addr[1] = "" THEN oe-ord.addr[1] ELSE oe-ord.sold-addr[1]) AT 45 FORM "x(30)" SKIP
   SPACE(5) (IF oe-ord.addr[2] EQ "" THEN v-addr3 ELSE oe-ord.addr[2]) FORMAT "x(30)"
    (IF oe-ord.sold-addr[2] = "" AND oe-ord.addr[2] EQ "" THEN v-sold-addr3 ELSE
        IF oe-ord.sold-addr[2] = "" AND oe-ord.addr[2] NE "" THEN oe-ord.addr[2] ELSE oe-ord.sold-addr[2]) AT 45 FORM "x(30)" SKIP
   SPACE(5) (IF oe-ord.addr[2] NE "" THEN v-addr3 ELSE "") FORMAT "x(30)"
       (IF oe-ord.sold-addr[2] NE "" OR oe-ord.addr[2] NE "" THEN v-sold-addr3 ELSE "") AT 45 FORMAT "x(30)" SKIP.

 IF AVAIL bf-shipto AND bf-shipto.broker THEN
        PUT "<=2><C3><FGCOLOR=" TRIM(lv-comp-color) + ">"
            "<=2><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" TRIM(lv-other-color) + ">" FORM "x(6)" 
            "<P10>".

v-printline = v-printline + 14.
PUT "<|10><R4><C48><#3><FROM><R10><C80><RECT>" SKIP.
PUT "<R6><C48><FROM><R6><C80><LINE>" SKIP      
    "<R8><C48><FROM><R8><C80><LINE>" SKIP      
   "<R4><C61><FROM><R6><C61><LINE>" SKIP
   "<R6><C65><FROM><R8><C65><LINE>" SKIP
   "<R8><C65><FROM><R10><C65><LINE>" SKIP
   .

PUT "<FArial><P12><=#3><R-2> <B>Reconnaissance/Acknowledgment</B> " "<P10>" SKIP
    "<=#3> Client                         Contact"
    "<=#3><R+2> T�l�phone                            T�l�copieur" 
    "<=#3><R+4> Bon De Commande/P.O.      Date <FCourier New>"
    "<=3><R+1> " oe-ord.cust-no  SPACE(7) oe-ord.contact
    "<=3><R+3> " v-cust-phone  SPACE(7) cust.fax
    "<=3><R+5> " oe-ord.po-no SPACE(7) oe-ord.ord-date .

/* same as request - no space between hd & line
PUT "<R11><#4><R17><C80><RECT>" SKIP
"<R13><C1><FROM><R13><C80><LINE>" SKIP
"<R15><C1><FROM><R15><C80><LINE>" SKIP

"<R11><C20><FROM><R15><C20><LINE>" SKIP
"<R11><C30><FROM><R15><C30><LINE>" SKIP
"<R11><C40><FROM><R15><C40><LINE>" SKIP
"<R11><C50><FROM><R15><C55><LINE>" SKIP
"<R11><C60><FROM><R15><C60><LINE>" SKIP
"<R11><C70><FROM><R15><C70><LINE>" SKIP
.
*/  

PUT "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
"<R21><C1><FROM><R21><C80><LINE>" SKIP    
"<R19><C11><FROM><R23><C11><LINE>" SKIP
"<R19><C22><FROM><R23><C22><LINE>" SKIP
"<R19><C38><FROM><R23><C38><LINE>" SKIP
"<R19><C52><FROM><R23><C52><LINE>" SKIP
"<R19><C66.8><FROM><R23><C66.8><LINE>" SKIP
"<R19><C74.5><FROM><R23><C74.5><LINE>" SKIP
.
PUT "<FArial><=4><C23>Exp�diteur <C39>Termes <C53>Repr�sentant <C67>Commande     " SKIP
    "<FArial><=4><R+1><C2>Date Req. <C12>Destination <C23>Ship Via <C39>Terms <C53>Sales Person <C67>Order# <C75>Quote#" SKIP
"<FCourier New><=4><R+3> " lv-due-date FORM "99/99/9999" SPACE(2)
oe-ord.fob-code FORM "x(11)" SPACE(2) /* gdm 01060906 */
v-shipvia /*carrier.carrier*/ FORM "x(20)" SPACE(1)
oe-ord.terms-d /*terms.dscr*/ FORM "x(15)" "<C52.5>" v-salesman SPACE(1) oe-ord.ord-no "<C74.8>" v-q-no FORMAT ">>>>>9" SKIP.


PUT "<|10><R24><C1><#5><FROM><R26><C80><RECT>" SKIP    
       "<R24><C6><FROM><R26><C6><LINE>" SKIP
       "<R24><C20><FROM><R26><C20><LINE>" SKIP
     /*"<R26><C30><FROM><R28><C30><LINE>" SKIP */
       "<R24><C49><FROM><R26><C49><LINE>" SKIP
       "<R24><C61><FROM><R26><C61><LINE>" SKIP
       "<R24><C72><FROM><R26><C72><LINE>" SKIP
       .
PUT "<FArial><=5><C2>Ligne <C50>Commande <C62>Prix <C73>UM" SKIP(1) 
    "<FArial><=5><R+1><C2>Line <C7>R�f�rence# <C21>Description <C50>Order <C62>Price <C73>UOM" SKIP(1).
PUT "<FCourier New>"          .
v-printline = v-printline + 6.

