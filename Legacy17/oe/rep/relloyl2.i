/* ------------------------------------------ oe/rep/relloyl2.i 09220907 GDM */
/* RELEASE PRINT  Program for N-K-1 RELPRINT = LoyLang                       */
/* ------------------------------------------------------------------------- */

PUT {1}
    "<P10><|10><C1><#8><FROM><C80><R+2><RECT> " 
    "<=8>Pulled By              Checked By              # of Units              Total Quantity" SKIP
    "<=8><C20><FROM><R+2><C20><Line>" 
    "<=8><C40><FROM><R+2><C40><Line>" 
    "<=8><C60><FROM><R+2><C60><Line>" SKIP

    "<P10><|10><C1><R-0.5><#8><FROM><C80><R+2><RECT> " 
    "<=8><FArial>  Pallet Code <C22> Name    <C46>  Description" SKIP
    "<=8><C20><FROM><R+2><C20><Line>" 
    "<=8><C45><FROM><R+2><C45><Line>" 
    "<=8><FCourier New><R+1><C2>" v-i-no  "<C22>" v-i-name "<C46>" v-i-desc
    "<FCourier New>" SKIP(1).
