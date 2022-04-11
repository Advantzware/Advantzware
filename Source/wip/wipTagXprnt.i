/* wip/wipTagXprnt.i */
       
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        PUT  SKIP "<R23><C3>_____________________________________<B>Fold Here</B>_____________________________________" SKIP .
        PUT  SKIP "<R25><C35><P18><B>WIP Tag</B><P14>" SKIP .
        PUT   "<#=100><C10><R28><FROM><C72><R+3><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" ttPrintInventoryStock.tag FORMAT "x(30)" ">"
            "<C30><R31>" ttPrintInventoryStock.tag FORMAT "x(30)" .
            
        PUT  SKIP(3) "<R32.5><C3><FROM><R32.5><C80><LINE>" SKIP .        
        PUT  "<R32.5><C70><FROM><R65><C70><LINE>" SKIP .            
        PUT  "<R34><C10><P16> <b>WIP #:  </b> " ttPrintInventoryStock.tag FORMAT "x(30)".
        PUT  "<R36><C10><P16> <b>Job #:  </b> " ttPrintInventoryStock.jobNumber + "-" + STRING(ttPrintInventoryStock.jobRunNumber,"999") FORMAT "x(13)".
        PUT  "<R38><C10><P16> <b>Form #:</b>" ttPrintInventoryStock.formNo FORMAT ">>9" .
         
        PUT   "<#=100><C40><R36><FROM><C68><R+3><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" ttPrintInventoryStock.jobNumber + "-" + STRING(ttPrintInventoryStock.jobRunNumber,"999") FORMAT "x(13)" ">"
              "<C52><R39>" ttPrintInventoryStock.jobNumber + "-" + STRING(ttPrintInventoryStock.jobRunNumber,"999") FORMAT "x(13)" .
         
        PUT  "<R41><C3><FROM><R41><C70><LINE>" SKIP .          
        PUT  "<R43><C10><P16><b>Date / Time Created:</b> " .            
        PUT  "<R46><C10><P16><b>From Machine:</b>  " ttPrintInventoryStock.machineName FORMAT "x(30)" .           
        PUT  "<R49><C10><P16><b>Next Machine:</b> " .            
        PUT  "<R53><C10><P26><b>Quantity:</b> " ttPrintInventoryStock.quantity FORMAT ">>>>>>>>9" "<P14>".
         
         
        PUT   "<#=100><C72><R33.5><FROM><C122><R+3.5><BARCODE,TYPE=128B,ANGLE=270,CHECKSUM=NONE,VALUE=" ttPrintInventoryStock.tag FORMAT "x(30)" ">"
               "<C78><R57.5><angle=90>" ttPrintInventoryStock.tag FORMAT "x(30)" .
       
       
        
        PUT "<FCourier New>".

  
