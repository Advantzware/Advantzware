/* oe/rep/relPort2.i */   

   PUT "<FArial>" SKIP
       "<P14><C+31><B>Lista de produtos a recolher</B> " SKIP
        "<C1><LEFT=9mm><#1><R+5><C+25>" SKIP
        "<=1>" SKIP
       "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
       "<=1><C3><R+1><P20><B>" lv-comp-name "</B><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" 
       "<P10></B>"
       "<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)"        
       "<P10><=1><R+3>"
        v-comp-add1 AT 8 SKIP
        v-comp-add2 AT 8  SKIP
        v-comp-add3 AT 8 SKIP
        v-comp-add4 AT 8 SKIP
        v-comp-add5 AT 8 "<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)" SKIP
        lv-email AT 8 SKIP(1)
              "<FCourier New>"
              "Vendido para:" SPACE(30) "Enviar para:"  SKIP
              SPACE(5) cust.name shipto.ship-name AT 45 skip
              SPACE(5) cust.addr[1] shipto.ship-addr[1] AT 45 SKIP.
      IF cust.addr[2] <> "" OR shipto.ship-addr[2] <> "" THEN
                  PUT SPACE(5) cust.addr[2] shipto.ship-addr[2] AT 45 SKIP
                  SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                           shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP.
      ELSE PUT SPACE(5) cust.city + " " + cust.state + " " + cust.zip FORM "x(30)"
                        shipto.ship-city + " " + shipto.ship-state + " " + shipto.ship-zip AT 45 FORM "x(30)" SKIP(1).

      PUT
         "<R4><C46><#3>" SKIP
         "<FArial><P14><=#3><P12>" SKIP
         "<=#3><B>N� do ticket: " 
         "<UNITS=INCHES><AT=.54,6><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
          string(oe-relh.release#) + ">" FORM "x(100)" "</B><P10>" 
           "<AT=,6.5>" oe-relh.release#        SKIP(1)
         "<=#3><R+5>Data de impress�o:" v-ticket-date   FORM "99/99/9999" " "   STRING(oe-relh.upd-time,"hh:mm am") SKIP
         "<=#3><R+6>Data de envio:" oe-relh.rel-date        SKIP
         "<=#3><R+7>CSR: " v-csr SKIP
          SKIP     
         "<|10><R19><C1><#4><FROM><R23><C80><RECT>" SKIP
         "<R21><C1><FROM><R21><C80><LINE>" SKIP    
         "<R19><C12><FROM><R23><C12><LINE>" SKIP
         "<R19><C25><FROM><R23><C25><LINE>" SKIP
         "<R19><C34><FROM><R23><C34><LINE>" SKIP
         "<R19><C57><FROM><R23><C57><LINE>" SKIP
         "<FArial><=4><R+1> Zona de entrega              Peso                    FOB                           Enviar via                                         Termos de frete" SKIP
         "<FCourier New><=4><R+3> " v-zone space(10) v-weight FORMAT "->>>>>9" space(10) oe-ord.fob-code SPACE(5) v-carrier space(10) v-frt-terms   SKIP
         "<|10><R24><C1><#5><FROM><R28><C80><RECT>" SKIP    
         "<R24><C7.5><FROM><R28><C7.5><LINE>" SKIP
         "<R24><C29><FROM><R28><C29><LINE>" SKIP
         "<R24><C57><FROM><R28><C57><LINE>" SKIP 
         "<R24><C65><FROM><R28><C65><LINE>" SKIP   
         "<R24><C71><FROM><R28><C71><LINE>" SKIP
         "<FArial><=5><C65.2>N�mero<C71.5>Quant./UOM" SKIP
         "<=5><R+1><C2>N� do<C9>N� do<C65.2>de<C71.5>do pedido" SKIP
         "<=5><R+2>  pedido 
               <C8>  item/descri��o/lote
              <C29>  N� da etiqueta/comporta/caixa/trabalho 
              <C58>  Unidades<C65.2>Unidades<C71.5>Quant./UOM" SKIP
         "<=5><R+3><C71.5> cada" SKIP(1)    
         "<FCourier New>".
      v-printline = v-printline + 16.

