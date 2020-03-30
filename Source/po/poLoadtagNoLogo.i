 /*op/poLoadtagNoLogo.i */                

        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".

        PUT "<R19.5><P12><C8><B>- - - - - - - - - - - - - - - - - - - - - - - - - - - - -  FOLD HERE  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -</B>" SKIP . 

        PUT "<||><R22><C8><#4><FROM><R26><C78><RECT>" SKIP.            
        
        PUT "<FArial><=4><R22.6><C8><B><P28>  " (IF AVAIL cust THEN cust.NAME ELSE "") FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R28><C8><#5><FROM><R32><C78><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-1.6><C8><B><p15> DESCRIPTION </B>" SKIP.
        PUT "<FArial><=5><R28.6><C8><B><P28>  " po-ordl.i-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R34><C8><#7><FROM><R37><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-1.6><C8><B><p15> CUSTOMER PART# </B>" SKIP.
        PUT "<FArial><=7><R34.3><C8><B><P20>  " (IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE "") FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R34><C43><#8><FROM><R37><C78><RECT>" SKIP.            
        
        PUT "<FArial><=8><R-1.6><C43><B><p15> PRODUCTION DATE </B>" SKIP.
        PUT "<FArial><=8><R34.3><C43><B><P20>  " STRING(po-ordl.due-date) FORMAT "x(12)" "</B>"  SKIP.

        PUT "<||><R39><C8><#9><FROM><R42><C40><RECT>" SKIP.            
        
        PUT "<FArial><=9><R-1.6><C8><B><p15> PURCHASE ORDER # </B>" SKIP.
        PUT "<FArial><=9><R39.3><C8><B><P20>  " ( IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0) FORMAT ">>>>>>>>>9" "</B>" SKIP.

        PUT "<||><R39><C43><#10><FROM><R42><C78><RECT>" SKIP.            
       
        PUT "<FArial><=10><R-1.6><C43><B><p15> FINISHED GOODS ITEM  </B>" SKIP.
        PUT "<FArial><=10><R39.3><C43><B><P20>  " po-ordl.i-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R44><C8><#11><FROM><R47><C40><RECT>" SKIP.            
        
        PUT "<FArial><=11><R-1.6><C8><B><p15> ORDER# </B>" SKIP.
        PUT "<FArial><=11><R44.3><C8><B><P20>  " STRING(Po-ordl.ord-no) FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R44><C43><#12><FROM><R47><C78><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C43><B><p15> ORDER QUANTITY </B>" SKIP.
        PUT "<FArial><=12><R44.3><C43><B><P20>  " ( IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0) FORMAT ">,>>>,>>>,>>9" "</B>" SKIP.
        
        PUT "<||><R49><C8><#12><FROM><R52><C40><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C8><B><p15> PALLET QUANTITY </B>" SKIP.
        PUT "<FArial><=12><R49.3><C8><B><P20>  "  "</B>" SKIP.
        
        PUT "<||><R49><C43><#12><FROM><R52><C78><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C43><B><p15> TOTAL PALLETS </B>" SKIP.
        PUT "<FArial><=12><R49.3><C43><B><P20>  "  "</B>" SKIP.

        PUT "<||><R53><C8><#13><FROM><R59><C78><RECT>" SKIP. 
        PUT UNFORMATTED "<R53.7><#1><UNITS=INCHES><C10><FROM><c76><R57.2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              string(string(po-ordl.po-no) + "-" + STRING(po-ordl.LINE,"99")) FORMAT "x(20)" ">"
            "<C39>" string(string(po-ordl.po-no) + "-" + STRING(po-ordl.LINE,"99"),"x(20)")  "<=#1><R+5>" .
       
        
        PUT "<FCourier New>".
        
        PAGE.
        
   
