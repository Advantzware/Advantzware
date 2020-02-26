/* oe/rep/lodxprntNoLogo.i */  

 FIND FIRST oe-ord NO-LOCK 
             WHERE oe-ord.company = cocode
             AND oe-ord.ord-no = tt-word-print.ord-no  NO-ERROR.

 find first  oe-ordl NO-LOCK
     where oe-ordl.company eq cocode
     and oe-ordl.ord-no  eq tt-word-print.ord-no no-error.
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".

        PUT "<R19.5><P12><C8><B>- - - - - - - - - - - - - - - - - - - - - - - - - - - - -  FOLD HERE  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -</B>" SKIP . 

        PUT "<||><R22><C8><#4><FROM><R26><C78><RECT>" SKIP.            
        
        PUT "<FArial><=4><R22.6><C8><B><P28>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R28><C8><#5><FROM><R32><C78><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-1.6><C8><B><p15> DESCRIPTION </B>" SKIP.
        PUT "<FArial><=5><R28.6><C8><B><P28>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R34><C8><#7><FROM><R37><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-1.6><C8><B><p15> CUSTOMER PART# </B>" SKIP.
        PUT "<FArial><=7><R34.3><C8><B><P20>  " tt-word-print.cust-part-no FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R34><C43><#8><FROM><R37><C78><RECT>" SKIP.            
        
        PUT "<FArial><=8><R-1.6><C43><B><p15> PRODUCTION DATE </B>" SKIP.
        PUT "<FArial><=8><R34.3><C43><B><P20>  " (IF AVAIL loadtag AND loadtag.tag-date NE ? THEN STRING(loadtag.tag-date,"99/99/9999") ELSE "") FORMAT "x(12)" "</B>"  SKIP.

        PUT "<||><R39><C8><#9><FROM><R42><C40><RECT>" SKIP.            
        
        PUT "<FArial><=9><R-1.6><C8><B><p15> JOB # </B>" SKIP.
        PUT "<FArial><=9><R39.3><C8><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.

        PUT "<||><R39><C43><#10><FROM><R42><C78><RECT>" SKIP.            
       
        PUT "<FArial><=10><R-1.6><C43><B><p15> FINISHED GOODS ITEM  </B>" SKIP.
        PUT "<FArial><=10><R39.3><C43><B><P20>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R44><C8><#11><FROM><R47><C40><RECT>" SKIP.            
        
        PUT "<FArial><=11><R-1.6><C8><B><p15> ORDER PO# </B>" SKIP.
        PUT "<FArial><=11><R44.3><C8><B><P20>  " (IF AVAIL oe-ord THEN string(oe-ord.po-no,"x(15)") ELSE "") FORMAT "x(15)"  "</B>" SKIP.

        PUT "<||><R44><C43><#12><FROM><R47><C78><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-1.6><C43><B><p15> PALLET QUANTITY </B>" SKIP.
        PUT "<FArial><=12><R44.3><C43><B><P20>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.

        PUT "<||><R48><C8><#13><FROM><R54><C78><RECT>" SKIP. 
        PUT UNFORMATTED "<R48.7><#1><UNITS=INCHES><C10><FROM><c76><R52.2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              tt-word-print.tag-no FORMAT "x(20)" ">"
            "<C39>" string(tt-word-print.tag-no,"x(20)")  "<=#1><R+5>" .
       
        
        PUT "<FCourier New>".
