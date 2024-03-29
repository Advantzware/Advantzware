/* ar/rep/invpremx.i  */
RUN Format_Date(v-inv-date,"DD/MM/YYYY", OUTPUT opcDateStringInvDate).
RUN Format_Date(v-date-ship,"DD/MM/YYYY", OUTPUT opcDateStringShipDate).

PUT "<FTimes New Roman>".
    IF lChkImage THEN
         PUT  "<C3><R2><#1><R+7><C+45>" "<IMAGE#1=" + ls-full-img1 FORM "x(200)" SKIP(1) .
/*         PUT "<C3><R2><#1>"                                         */
/*             "<R+8><C+45><IMAGE#1=" ls-full-img1 SKIP. /* image */ .*/
        PUT "<=1>" SKIP.


IF company.company = '004' THEN
        PUT "<C1><#2>" 
            "<P10><=2><R+6>"
            "<FCourier New>" 
            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>"  SKIP(1)
            SPACE(12) "ENVIAR PARA: Premier Packaging Canada, LLC" SKIP
            SPACE(12) "          Dept 400175" SKIP
            SPACE(12) "          PO Box 4375 STN A" SKIP 
            SPACE(12) "          Toronto ON M5W OJ3" SKIP 
            SPACE(12) "FATURAR PARA:" SPACE(38) "ENVIAR PARA:" SKIP
            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
            SPACE(12) v-email    "</B>" SKIP    .
ELSE IF company.company = '005' THEN
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+6>"
            "<FCourier New>"
            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
            SPACE(12) "ENVIAR PARA: MCI PACKAGING LLC" SKIP
            SPACE(12) "          PO BOX 39505" SKIP
            SPACE(12) "          Louisville, KY 40233" SKIP (1)
            SPACE(12) "FATURAR PARA:" SPACE(38) "ENVIAR PARA:" SKIP
            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
            SPACE(12) v-email    "</B>" SKIP    .
ELSE IF company.company EQ '006' THEN
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+6>"
            "<FCourier New>" SKIP(1)
            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" 
            SKIP(3)
            SPACE(12) "FATURAR PARA:" SPACE(38) "ENVIAR PARA:" SKIP
            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
            SKIP
            SKIP
            SKIP
            SKIP
            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
            SPACE(12) v-email    "</B>" SKIP    .
ELSE           
        PUT "<C1><#2>" /*<R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */
            "<P10><=2><R+5>"
            "<FCourier New>"
            SPACE(4) "<P11><B>" cCompanyID FORMAT "x(30)" "</B><P10>" SKIP(1)
            SPACE(12) "ENVIAR PARA: PREMIER PACKAGING" SKIP
            SPACE(12) "          3254 RELIABLE PARKWAY" SKIP
            SPACE(12) "          CHICAGO, IL  60686" SKIP (1)
            SPACE(12) "FATURAR PARA:" SPACE(38) "ENVIAR PARA:" SKIP
            SPACE(12) ar-inv.cust-name v-shipto-name AT 64 SKIP
            SPACE(12) ar-inv.addr[1]   v-shipto-addr[1] AT 64 SKIP
            SPACE(12) ar-inv.addr[2]  v-shipto-addr[2] AT 64 SKIP
            SPACE(12) v-addr3   v-sold-addr3 AT 64  SKIP
            SPACE(12) v-email    "</B>" SKIP    .

        v-printline = v-printline + 15.
        PUT "<|10><R5><C53><#3><FROM><R7><C78><RECT>" SKIP.
        PUT "<R6><C53><FROM><R6><C78><LINE>" SKIP
            "<R5><C65><FROM><R7><C65><LINE>" SKIP .
            /*"<R8><C65><FROM><R10><C65><LINE>" SKIP.*/
        
PUT "<FArial><P12><=#3><R-2> <P10>" ip-copy-title FORM "x(20)" SKIP
    "<=#3>  N� DA FATURA                    " ar-inv.inv-no FORMAT ">>>>>>>9"
    "<=#3><R+1>              DATE               " opcDateStringInvDate FORMAT "x(10)" "<FCourier New>"    
    SKIP(1)
    .

PUT "<|10><R21><C1><#4><FROM><R24><C81><RECT>" SKIP
    "<R23><C1><FROM><R23><C81><LINE>" SKIP    
    "<R21><C12.5><FROM><R24><C12.5><LINE>" SKIP
    "<R21><C22.5><FROM><R24><C22.5><LINE>" SKIP
    "<R21><C38><FROM><R24><C38><LINE>" SKIP
    "<R21><C52><FROM><R24><C52><LINE>" SKIP
    /* "<R21><C59><FROM><R24><C59><LINE>" */ SKIP 
    "<R21><C72><FROM><R24><C72><LINE>" SKIP
    .

v-printline = v-printline + 3.
FIND FIRST sman WHERE sman.company = ar-inv.company 
                  AND sman.sman = v-salesman
                  NO-LOCK NO-ERROR.
v-salesname = IF AVAIL sman THEN sman.sname ELSE "".

PUT "<FArial><=4> DATA DE ENVIO        FOB                  ENVIAR VIA                TERMOS                    NOME DO VENDEDOR              N� CONHEC. " SKIP
     "<=4><R+1><C72> EMBARQUE " SKIP
     "<FCourier New><=4><R+2><B> " opcDateStringShipDate FORMAT "x(10)" SPACE(4)
     v-fob FORM "x(11)" SPACE(1)
     v-shipvia FORM "x(20)" SPACE(1)
     ar-inv.terms-d FORM "x(15)" SPACE(1)
    /* v-salesman FORM "x(8)" */
     v-salesname FORM "x(23)"
     lv-bol-no FORM ">>>>>>>" "</B>"
    SKIP.


PUT 
   /* "<|10><R28><C1><#5><FROM><R28><C80><LINE>" SKIP     
    "<|10><R31><C1><#6><FROM><R31><C80><LINE>" SKIP 
   */
    "<|10><R25><C1><#5><FROM><R29><C81><RECT>" SKIP    
    "<R25><C13><FROM><R29><C13><LINE>" SKIP
    "<R25><C25><FROM><R29><C25><LINE>"     
    "<R25><C52><FROM><R29><C52><LINE>" SKIP
    "<R25><C60><FROM><R29><C60><LINE>" SKIP
    "<R25><C65><FROM><R29><C65><LINE>" 
    "<R25><C71><FROM><R29><C71><LINE>" SKIP 

    .       
/*puT "<FArial><=5><R+1>   Ordered     Shipped       B.O.      Item#                        Description                                              Price     UOM                 Amount" SKIP(1). */
PUT "<FArial><=5>  N� DO PO DO        N� DE PE�A DO                                                                            <C62>P       PRE�O                  " SKIP.
IF ip-print-s THEN do:
    PUT "<FArial>  CLIENTE                CLIENTE                                DESCRI��O                                  ENVIADO       <C62>C        (UOM)        VALOR" SKIP.
END.    
ELSE do: 
    PUT "<FArial>  CLIENTE                CLIENTE                                 DESCRI��O                                 ENVIADO        <C62>C        (UOM)        VALOR" SKIP.
END.
 
   PUT "<FArial>  N� DO NOSSO       N� DO ITEM" SKIP. 
   PUT "<FArial>  PEDIDO" SKIP(1). 
v-printline = v-printline + 3.
           
PUT "<FCourier New><B>".
