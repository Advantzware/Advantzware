 /*op/poLoadtagprint.i */      
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".

        PUT "<R16.5><P12><C8><B>- - - - - - - - - - - - - - - - - - - - - - - - - - - - -  FOLD HERE  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -</B>" SKIP . 

        PUT "<||><R19><C8><#4><FROM><R23><C78><RECT>" SKIP.            
        
        PUT "<FArial><=4><R19.6><C8><B><P28>  " (IF AVAIL cust THEN cust.NAME ELSE "") FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R25><C8><#5><FROM><R29><C78><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-1.6><C8><B><p15> DESCRIPTION </B>" SKIP.
        PUT "<FArial><=5><R25.6><C8><B><P28>  " po-ordl.i-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R31><C8><#7><FROM><R34><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-1.6><C8><B><p15> CUSTOMER PART# </B>" SKIP.
        PUT "<FArial><=7><R31.3><C8><B><P20>  " (IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE "") FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R31><C43><#8><FROM><R34><C78><RECT>" SKIP.            
        
        PUT "<FArial><=8><R-1.6><C43><B><p15> PRODUCTION DATE </B>" SKIP.
        PUT "<FArial><=8><R31.3><C43><B><P20>  " STRING(po-ordl.due-date) FORMAT "x(12)" "</B>"  SKIP.

        PUT "<||><R36><C8><#9><FROM><R39><C40><RECT>" SKIP.            
        
        PUT "<FArial><=9><R-1.6><C8><B><p15> PURCHASE ORDER # </B>" SKIP.
        PUT "<FArial><=9><R36.3><C8><B><P20>  " ( IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0) FORMAT ">>>>>>>>>9" "</B>" SKIP.

        PUT "<||><R36><C43><#10><FROM><R39><C78><RECT>" SKIP.            
       
        PUT "<FArial><=10><R-1.6><C43><B><p15> FINISHED GOODS ITEM  </B>" SKIP.
        PUT "<FArial><=10><R36.3><C43><B><P20>  " po-ordl.i-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R41><C8><#11><FROM><R44><C40><RECT>" SKIP.            
        
        PUT "<FArial><=11><R-1.6><C8><B><p15> ORDER# </B>" SKIP.
        PUT "<FArial><=11><R41.3><C8><B><P20>  " STRING(Po-ordl.ord-no) FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R41><C43><#12><FROM><R44><C78><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C43><B><p15> ORDER QUANTITY </B>" SKIP.
        PUT "<FArial><=12><R41.3><C43><B><P20>  " ( IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0) FORMAT ">,>>>,>>>,>>9" "</B>" SKIP.
        
        PUT "<||><R46><C8><#12><FROM><R49><C40><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C8><B><p15> PALLET QUANTITY </B>" SKIP.
        PUT "<FArial><=12><R46.3><C8><B><P20>  "  "</B>" SKIP.
        
        PUT "<||><R46><C43><#12><FROM><R49><C78><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C43><B><p15> TOTAL PALLETS </B>" SKIP.
        PUT "<FArial><=12><R46.3><C43><B><P20>  "  "</B>" SKIP.

        PUT "<||><R50><C8><#13><FROM><R56><C78><RECT>" SKIP. 
        PUT UNFORMATTED "<R50.7><#1><UNITS=INCHES><C10><FROM><c76><R54.2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              string(string(po-ordl.po-no) + "-" + STRING(po-ordl.LINE,"99")) FORMAT "x(20)" ">"
            "<C39>" string(string(po-ordl.po-no) + "-" + STRING(po-ordl.LINE,"99"),"x(20)")  "<=#1><R+5>" .
       
        IF ipcPrintFormat EQ "POLoadtag2" THEN 
        DO: 
            PUT "<||><R57.5><C19><#13><FROM><R64><C60><RECT>" SKIP. 

            PUT UNFORMATTED   
                   "<C20><R58><#1><R+5><C+40><IMAGE#1=" ls-full-img1.
        END.
        PUT "<FCourier New>".
        
        PAGE.
    
