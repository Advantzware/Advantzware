/* oe/rep/invxprnt.i */  
DEFINE VARIABLE v-comp-add1 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-name AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email AS CHARACTER FORMAT "x(56)" NO-UNDO.

FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ tt-word-print.cust-no NO-ERROR .
 IF AVAIL cust THEN DO:
     ASSIGN
       cEmail = cust.email .
       cPhone = string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx") .
       cFax   = STRING(SUBSTR(cust.fax,1,3),"(xxx)") + "-" + STRING(SUBSTR(cust.fax,4),"xxx-xxxx")   .
 END.

 FIND FIRST oe-ord NO-LOCK 
             WHERE oe-ord.company = cocode
             AND oe-ord.ord-no = tt-word-print.ord-no  NO-ERROR.

 find first  oe-ordl NO-LOCK
     where oe-ordl.company eq cocode
     and oe-ordl.ord-no  eq tt-word-print.ord-no no-error.

 IF AVAIL oe-ordl THEN
     find first oe-rel no-lock where oe-rel.company eq cocode
     and oe-rel.ord-no  eq oe-ordl.ord-no
     and oe-rel.i-no    eq oe-ordl.i-no
     and oe-rel.line    eq oe-ordl.line
     no-error.
 if avail oe-rel then do:
     find first shipto NO-LOCK where shipto.company eq cocode
         and shipto.cust-no eq oe-rel.cust-no
         and shipto.ship-id eq oe-rel.ship-id no-error.
     if shipto.broker then DO:
         assign
             v-comp-add1 = cust.addr[1]
             v-comp-add2 = cust.addr[2]
             v-comp-add3   = cust.city + ", " +
             cust.state + "  " +
             cust.zip
             v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
             v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
             lv-email    = "Email:  " + cust.email   
             lv-comp-name = cust.NAME .
         
         FIND FIRST oe-ord NO-LOCK 
             WHERE oe-ord.company = cocode
             AND oe-ord.ord-no = tt-word-print.ord-no  NO-ERROR.
         IF AVAIL oe-ord THEN
             ASSIGN lv-comp-name = oe-ord.sold-name
             v-comp-add1 = oe-ord.sold-addr[1]
             v-comp-add2 = oe-ord.sold-addr[2]
             v-comp-add3 = oe-ord.sold-city + ", " +
             oe-ord.sold-state + "  " +
             oe-ord.sold-zip.        
     END.
 END.

 FIND FIRST company NO-LOCK
     WHERE company.company  = cocode  NO-ERROR. 

IF v-comp-add2 EQ "" THEN
    ASSIGN
    v-comp-add2 = v-comp-add3
    v-comp-add3 = v-comp-add4
    v-comp-add4 = v-comp-add5
    v-comp-add5 = lv-email
    lv-email = "".

IF tb_print-view THEN DO:
        
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        IF AVAIL shipto AND shipto.broker THEN DO:
            PUT "<FArial><R2><C3><B><P25>  " lv-comp-name FORMAT "x(27)" "</B>" SKIP.
            PUT "<FArial><R4.5><C3><B><P19>  " v-comp-add1 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R6><C3><B><P19>  " v-comp-add2 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R7.5><C3><B><P19>  " v-comp-add3 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R9><C3><B><P19>  " v-comp-add4 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R10.5><C3><B><P19>  " v-comp-add5 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R12><C3><B><P19>  " lv-email FORMAT "x(56)" "</B>" SKIP.

        END.
        ELSE DO:
        PUT UNFORMATTED   
               "<C4><R2><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        END.

        PUT UNFORMATTED "<r13.7><#1><UNITS=INCHES><C39><FROM><c78><r16.2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              tt-word-print.tag-no FORMAT "x(20)" ">"
            "<C46>" string(tt-word-print.tag-no,"x(20)")  "<=#1><R+5>".
        
        PUT "<FArial><#1><R16.3><C39><C39><B><p15><b>Tag #  </b></B>" SKIP.

        PUT "<||><R13><C37><#2><FROM><R18><C80><RECT>" SKIP.

        PUT "<||><R19><C3><#4><FROM><R22><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R-2><C3><B><p15> Customer Name </B>" SKIP.
        
        PUT "<FArial><=4><R19.3><C3><B><P20>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R25><C3><#5><FROM><R28><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><C3><B><p15> Description </B>" SKIP.
        PUT "<FArial><=5><R25.3><C3><B><P17>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R25><C43><#6><FROM><R28><C80><RECT>" SKIP.            
        
        PUT "<FArial><=6><R-2><C43><B><p15> Customer Part# </B>" SKIP.
        PUT "<FArial><=6><R25.3><C43><B><P20>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R31><C3><#7><FROM><R34><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><C3><B><p15> Job# </B>" SKIP.
        PUT "<FArial><=7><R31.3><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.

        PUT "<||><R31><C43><#8><FROM><R34><C80><RECT>" SKIP.            
        
        PUT "<FArial><=8><R-2><C43><B><p15> Production Date </B>" SKIP.
        PUT "<FArial><=8><R31.3><C43><B><P20>  " (IF AVAIL loadtag AND loadtag.tag-date NE ? THEN STRING(loadtag.tag-date,"99/99/9999") ELSE "") FORMAT "x(12)" "</B>"  SKIP.

        PUT "<||><R37><C3><#9><FROM><R40><C40><RECT>" SKIP.            
        
        PUT "<FArial><=9><R-2><C3><B><p15> Order PO # </B>" SKIP.
        PUT "<FArial><=9><R37.3><C3><B><P20>  " (IF AVAIL oe-ord THEN string(oe-ord.po-no,"x(15)") ELSE "") FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R37><C43><#10><FROM><R40><C80><RECT>" SKIP.            
       /* MESSAGE "test" VIEW-AS ALERT-BOX ERROR.*/
        PUT "<FArial><=10><R-2><C43><B><p15> Line PO # </B>" SKIP.
        PUT "<FArial><=10><R37.3><C43><B><P20>  " (IF AVAIL oe-ordl THEN string(oe-ordl.po-no,"x(15)") ELSE "") FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R43><C3><#11><FROM><R46><C40><RECT>" SKIP.            
        
        PUT "<FArial><=11><R-2><C3><B><p15> Finished Goods Item </B>" SKIP.
        PUT "<FArial><=11><R43.3><C3><B><P20>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R43><C43><#12><FROM><R46><C80><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-2><C43><B><p15> Pallet Quantity </B>" SKIP.
        PUT "<FArial><=12><R43.3><C43><B><P20>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.

        PUT "<||><R47><C3><#13><FROM><R52><C20><RECT>" SKIP.            
        
        PUT "<FArial><=13><R+0.7><C5><B><p15> Qty / Case </B>" SKIP.
        PUT "<FArial><=13><R+2><C5><B><P20>  " tt-word-print.pcs "</B>" SKIP.

        PUT "<||><R47><C33><#14><FROM><R52><C50><RECT>" SKIP.            
        
        PUT "<FArial><=14><R+0.7><C35><B><p15> Cases / Pallet </B>" SKIP.
        PUT "<FArial><=14><R+2><C35><B><P20>  " tt-word-print.bundle "</B>" SKIP.

        PUT "<||><R47><C63><#15><FROM><R52><C80><RECT>" SKIP.            
        
        PUT "<FArial><=15><R+0.7><C65><B><p15> Partial Cases </B>" SKIP.
        PUT "<FArial><=15><R+2><C65><B><P20>  " tt-word-print.partial "</B>" SKIP.

        PUT "<||><R53><C3><#16><FROM><R65><C80><RECT>" SKIP.

        PUT "<||><R54><C29><#17><FROM><R58><C39><RECT>" SKIP.
        PUT "<||><R54><C48><#17><FROM><R58><C59><RECT>" SKIP.
        PUT "<||><R54><C66><#17><FROM><R58><C78><RECT>" SKIP.

        PUT "<FArial><#17><R56><C5><B><p15> Operator Checked By: </B>" SKIP.
        PUT "<FArial><#17><R56><C40><B><p15> Case#: </B>" SKIP.
        PUT "<FArial><#17><R56><C60><B><p15> Date: </B>" SKIP.


        PUT "<||><R60><C29><#17><FROM><R64><C39><RECT>" SKIP.
        PUT "<||><R60><C48><#17><FROM><R64><C59><RECT>" SKIP.
        PUT "<||><R60><C66><#17><FROM><R64><C78><RECT>" SKIP.

        PUT "<FArial><#17><R62><C15><B><p15> QC Checked: </B>" SKIP.
        PUT "<FArial><#17><R62><C40><B><p15> Case#: </B>" SKIP.
        PUT "<FArial><#17><R62><C60><B><p15> Date: </B>" SKIP.
         .
        
        PUT "<FCourier New>".
END.
ELSE DO: 
            
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".
        IF AVAIL shipto AND shipto.broker THEN DO:
            PUT "<FArial><R2><C3><B><P25>  " lv-comp-name FORMAT "x(27)" "</B>" SKIP.
            PUT "<FArial><R4.5><C3><B><P19>  " v-comp-add1 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R6><C3><B><P19>  " v-comp-add2 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R7.5><C3><B><P19>  " v-comp-add3 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R9><C3><B><P19>  " v-comp-add4 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R10.5><C3><B><P19>  " v-comp-add5 FORMAT "x(30)" "</B>" SKIP.
            PUT "<FArial><R12><C3><B><P19>  " lv-email FORMAT "x(56)" "</B>" SKIP.

        END.
        ELSE DO:
        PUT UNFORMATTED   
               "<C4><R2><#1><R+10><C+70><IMAGE#1=" ls-full-img1.
        END.

        PUT UNFORMATTED "<r13.7><#1><UNITS=INCHES><C39><FROM><c78><r16.2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              tt-word-print.tag-no FORMAT "x(20)" ">"
            "<C46>" string(tt-word-print.tag-no,"x(20)")  "<=#1><R+5>".
        
        PUT "<FArial><#1><R16.3><C39><C39><B><p15><b>Tag #  </b></B>" SKIP.
        
        PUT "<||><R13><C37><#2><FROM><R18><C80><RECT>" SKIP.

        PUT "<||><R19><C3><#4><FROM><R22><C80><RECT>" SKIP.            
        
        PUT "<FArial><=4><R-2><C3><B><p15> Customer Name </B>" SKIP.
        
        PUT "<FArial><=4><R19.3><C3><B><P20>  " tt-word-print.cust-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R25><C3><#5><FROM><R28><C40><RECT>" SKIP.            
        
        PUT "<FArial><=5><R-2><C3><B><p15> Description </B>" SKIP.
        PUT "<FArial><=5><R25.3><C3><B><P17>  " tt-word-print.i-name FORMAT "x(30)" "</B>" SKIP.

        PUT "<||><R25><C43><#6><FROM><R28><C80><RECT>" SKIP.            
        
        PUT "<FArial><=6><R-2><C43><B><p15> Customer Part# </B>" SKIP.
        PUT "<FArial><=6><R25.3><C43><B><P20>  " tt-word-print.cust-part-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R31><C3><#7><FROM><R34><C40><RECT>" SKIP.            
        
        PUT "<FArial><=7><R-2><C3><B><p15> Job# </B>" SKIP.
        PUT "<FArial><=7><R31.3><C3><B><P20>  " tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>" SKIP.

        PUT "<||><R31><C43><#8><FROM><R34><C80><RECT>" SKIP.            
        
        PUT "<FArial><=8><R-2><C43><B><p15> Production Date </B>" SKIP.
        PUT "<FArial><=8><R31.3><C43><B><P20>  " (IF AVAIL loadtag AND loadtag.tag-date NE ? THEN STRING(loadtag.tag-date,"99/99/9999") ELSE "") FORMAT "x(12)" "</B>"  SKIP.

        PUT "<||><R37><C3><#9><FROM><R40><C40><RECT>" SKIP.            
        
        PUT "<FArial><=9><R-2><C3><B><p15> Order PO # </B>" SKIP.
        PUT "<FArial><=9><R37.3><C3><B><P20>  " (IF AVAIL oe-ord THEN string(oe-ord.po-no,"x(15)") ELSE "") FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R37><C43><#10><FROM><R40><C80><RECT>" SKIP.            
        
        PUT "<FArial><=10><R-2><C43><B><p15> Line PO # </B>" SKIP.
        PUT "<FArial><=10><R37.3><C43><B><P20>  " (IF AVAIL oe-ordl THEN string(oe-ordl.po-no,"x(15)") ELSE "") FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R43><C3><#11><FROM><R46><C40><RECT>" SKIP.            
        
        PUT "<FArial><=11><R-2><C3><B><p15> Finished Goods Item </B>" SKIP.
        PUT "<FArial><=11><R43.3><C3><B><P20>  " tt-word-print.i-no FORMAT "x(15)" "</B>" SKIP.

        PUT "<||><R43><C43><#12><FROM><R46><C80><RECT>" SKIP.            
        
        PUT "<FArial><=12><R-2><C43><B><p15> Pallet Quantity </B>" SKIP.
        PUT "<FArial><=12><R43.3><C43><B><P20>  " tt-word-print.total-unit FORMAT ">,>>>,>>9" "</B>" SKIP.

        PUT "<||><R47><C3><#13><FROM><R52><C20><RECT>" SKIP.            
        
        PUT "<FArial><=13><R+0.7><C5><B><p15> Qty / Case </B>" SKIP.
        PUT "<FArial><=13><R+2><C5><B><P20>  " tt-word-print.pcs "</B>" SKIP.

        PUT "<||><R47><C33><#14><FROM><R52><C50><RECT>" SKIP.            
        
        PUT "<FArial><=14><R+0.7><C35><B><p15> Cases / Pallet </B>" SKIP.
        PUT "<FArial><=14><R+2><C35><B><P20>  " tt-word-print.bundle "</B>" SKIP.

        PUT "<||><R47><C63><#15><FROM><R52><C80><RECT>" SKIP.            
        
        PUT "<FArial><=15><R+0.7><C65><B><p15> Partial Cases </B>" SKIP.
        PUT "<FArial><=15><R+2><C65><B><P20>  " tt-word-print.partial "</B>" SKIP.

        PUT "<||><R53><C3><#16><FROM><R65><C80><RECT>" SKIP.

        PUT "<||><R54><C29><#17><FROM><R58><C39><RECT>" SKIP.
        PUT "<||><R54><C48><#17><FROM><R58><C59><RECT>" SKIP.
        PUT "<||><R54><C66><#17><FROM><R58><C78><RECT>" SKIP.

        PUT "<FArial><#17><R56><C5><B><p15> Operator Checked By: </B>" SKIP.
        PUT "<FArial><#17><R56><C40><B><p15> Case#: </B>" SKIP.
        PUT "<FArial><#17><R56><C60><B><p15> Date: </B>" SKIP.

        PUT "<||><R60><C29><#17><FROM><R64><C39><RECT>" SKIP.
        PUT "<||><R60><C48><#17><FROM><R64><C59><RECT>" SKIP.
        PUT "<||><R60><C66><#17><FROM><R64><C78><RECT>" SKIP.

        PUT "<FArial><#17><R62><C15><B><p15> QC Checked: </B>" SKIP.
        PUT "<FArial><#17><R62><C40><B><p15> Case#: </B>" SKIP.
        PUT "<FArial><#17><R62><C60><B><p15> Date: </B>" SKIP.
         .
        
        PUT "<FCourier New>".
END. /* else do */







