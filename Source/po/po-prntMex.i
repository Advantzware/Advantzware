/* po/po-xprem.i */

PUT 
         "<C1><#1><FArial>" 
         "<P10><R1><C70><B>Página #:</B>" string(PAGE-NUM,">9")  SKIP 
         "<P14><C+45> <B>Orden de compra</B> <P10>" v-change-ord SKIP       
       "<C3><R2><#2><R+8><C+40>" "<IMAGE#2=" + ls-full-img1 FORM "x(200)"  SKIP 
       /*"<=1><R+2>" "<FGCOLOR=" + trim(lv-comp-color) + ">" FORM "x(15)" */
          "<P10><=1><R+9><C3><FCourier New>Orden de compra To:" 
          "<P10><=1><R+11><C3><FCourier New>" vend.name
          "<P10><=1><R+8>"   
      /*    v-comp-add1  AT 8 SKIP
          v-comp-add2  AT 8  SKIP
          v-comp-add3  AT 8 SKIP
          v-comp-add4  AT 8 SKIP
          v-comp-add5  AT 8 /*"<FGCOLOR=" + trim(lv-other-color) + ">" FORM "x(15)"*/ skip
          lv-email AT 8 */
          "<FCourier New>" SKIP(2)
         space(21)    "Envie a:" AT 50 SKIP
         SPACE(3)  v-sname AT 50 SKIP
         SPACE(3) vend.add1 v-saddr[1] AT 50 SKIP
         SPACE(3) vend.add2 v-saddr[2] AT 50 SKIP
         SPACE(3) vend.city + " " + vend.state + " " + vend.zip FORM "x(35)"
                  v-scity + " " + v-sstate + " " + v-szip FORM "x(35)" AT 50 SKIP
         
       "<R4><C50><#3>" SKIP
       "<FArial><P14><=#3>" /*<C-20><R-2> <B>Purchase Order</B>*/  "<P10>" SKIP
          "<=#3><R+1.5><B><P12>PO #:     " po-ord.po-no "</B><P10>" SKIP(1)
          "<=#3><R+4.5>Fecha: " po-ord.po-date        SKIP
          "<=#3><R+5.5>Fecha cambiada: " po-ord.po-change-date SKIP
          "<=3><R+6.5>Fecha requerida: " po-ord.due-date SKIP
       .
    /*IF lv-display-comp THEN 
        PUT "<=1><C3><FGCOLOR=" trim(lv-comp-color) + ">"
            "<=1><C3><R+1><P20><B>" lv-comp-name "</B><P10><FGCOLOR=" trim(lv-other-color) + ">" FORM "x(6)" . 
   */

      v-printline = v-printline + 10.    
     
      PUT "<|10><R18><C1><#4><FROM><R22><C80><RECT>" SKIP
      "<R20><C1><FROM><R20><C80><LINE>" SKIP    
      "<R18><C10><FROM><R22><C10><LINE>" SKIP
      "<R18><C29><FROM><R22><C29><LINE>" SKIP
      /*"<R19><C38><FROM><R23><C38><LINE>" SKIP*/
      "<R18><C49><FROM><R22><C49><LINE>" SKIP
      "<R18><C55><FROM><R22><C55><LINE>" SKIP
      "<R18><C72><FROM><R22><C72><LINE>" SKIP
      .
      PUT "<FArial><=4><R18><C50>Flete" SKIP.
      PUT "<FArial><=4><R+1>  Comprador                     Contacto                             Condiciones                       a bordo       Embarcar vía                   Carga" SKIP
          "<FCourier New><=4><R+3> " po-ord.buyer    po-ord.contact FORM "x(25)" terms.dscr FORM "x(23)" po-ord.fob-code space(2) carrier.dscr FORM "x(20)" v-freight-dscr
          .
      
      PUT "<|10><R23><C1><#5><FROM><R25><C80><RECT>" SKIP    
             "<R23><C5><FROM><R25><C5><LINE>" SKIP
             "<R23><C13.9><FROM><R25><C13.9><LINE>" SKIP
             "<R23><C20.6><FROM><R25><C20.6><LINE>" SKIP 
/*              "<R23><C47><FROM><R25><C47><LINE>" SKIP */
             "<R23><C51><FROM><R25><C51><LINE>" SKIP
             "<R23><C59><FROM><R25><C59><LINE>" SKIP
             "<R23><C66><FROM><R25><C66><LINE>" 
             "<R23><C72><FROM><R25><C72><LINE>" SKIP
             .

      PUT "<FArial><=5><C14.1>Unidad<C21.5>Nuestro articulo/CP #/Descripción/<C51.5>Fecha de<C66.1>Unidad<C72><P8>Costo extendido<P10>" 
          "<=5><R+1><C1.5>Línea"
	  "<C7>Cantidad"
          "<C14.1>de medida"
	  "<C21.5>ID de proveedor/Artículo del vendedor #"
          "<C51.5>vencimiento"
          "<C60>Costo"
          "<C66.1><P9>de medida"
          "<C72.1><P8>MSF extendido<P10>" SKIP(1).
      PUT "<FCourier New>"          .
      v-printline = v-printline + 8.
