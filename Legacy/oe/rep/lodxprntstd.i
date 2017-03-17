/* oe/rep/invxprnt.i */

 FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ tt-word-print.cust-no NO-ERROR .
 IF AVAIL cust THEN
     ASSIGN
       cEmail = cust.email .
       cPhone = string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx") .
       cFax   = STRING(SUBSTR(cust.fax,1,3),"(xxx)") + "-" + STRING(SUBSTR(cust.fax,4),"xxx-xxxx")   .
     FIND FIRST company NO-LOCK
          WHERE company.company  = cocode  NO-ERROR.

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".
PUT "<FArial><=4><R11><C5><P12> " "......................................................................  Fold Here  ......................................................................" "" SKIP(2).

PUT   "<#=100><AT=2.10,1.5><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=180,CHECKSUM=NONE,VALUE= " tt-word-print.tag-no FORMAT "x(20)"  ">"
    "<AT=2.9,3.7><P16>" tt-word-print.tag-no FORMAT "x(20)"  .

PUT  SKIP(3) "<R21><C5><FROM><R21><C80><LINE>" SKIP.

PUT  "<R24><C39><FROM><R32><C39><LINE>" SKIP.

PUT "<R23><C5><#4><FROM><R23><C70><RECT><||3>" SKIP
    "<R33><C5><FROM><R33><C70><LINE><||3>" SKIP  
    "<R23><C5><FROM><R33><C5><LINE><||3>" SKIP
    "<R23><C70><FROM><R33><C70><LINE><||3>" SKIP.


PUT "<FArial><=4><R25><C6><P16><B>" company.NAME FORMAT "x(25)"  "</B>" SKIP.
PUT "<FArial><=4><R25><C40><P14> Phone: " cPhone FORMAT "x(15)"  "" SKIP(2).
PUT "<FArial><=4><R27><C6><P14>" tt-word-print.ship-add1 FORMAT "x(30)" "" .
PUT "<FArial><=4><R27><C40><P14> Fax: " cFax FORMAT "x(15)" "" SKIP(3).
PUT "<FArial><=4><R29><C6><P14>" tt-word-print.ship-city FORMAT "x(15)" tt-word-print.ship-state FORMAT "x(2)" " " tt-word-print.ship-zip "" .
PUT "<FArial><=4><R29><C40><P14> " cEmail "" SKIP(2).

PUT "<R34><C5><#4><FROM><R34><C70><RECT><||3>" SKIP
    "<R47><C5><FROM><R47><C70><LINE><||3>" SKIP  
    "<R34><C5><FROM><R47><C5><LINE><||3>" SKIP
    "<R34><C70><FROM><R47><C70><LINE><||3>" SKIP.

PUT "<FArial><=5><R35><C6><P16> Customer:    " "<B><C20>" tt-word-print.cust-name FORMAT "x(27)" "</B>"   "" SKIP.
PUT "<FArial><=5><R37><C6><P16> PO#:            " "<B><C20>" tt-word-print.cust-po-no FORMAT "x(15)" "</B>"   "" SKIP.
PUT "<FArial><=5><R39><C6><P16> Job#:            " "<B><C20>" tt-word-print.job-no FORMAT "x(6)" "-" STRING(tt-word-print.job-no2,"99") "</B>"   "" SKIP.
PUT "<FArial><=5><R41><C6><P16> Item Name:  " "<B><C20>" tt-word-print.i-name FORMAT "x(27)" "</B>"   "" SKIP.
PUT "<FArial><=5><R43><C6><P16> Part#:           " "<B><C20>" tt-word-print.cust-part-no FORMAT "x(15)" "</B>"   "" SKIP.
PUT "<FArial><=5><R45><C6><P16> FG#              " "<B><C20>" tt-word-print.i-no FORMAT "x(15)" "</B>"   "" SKIP.

PUT  "<R21><C71><FROM><R64><C71><LINE>" SKIP.

PUT   "<#=100><AT=4.08,7.50><FROM><AT=+.6,+6><BARCODE,TYPE=128B,ANGLE=90,CHECKSUM=NONE,VALUE= " tt-word-print.tag-no FORMAT "x(20)"  ">"
    "<AT=6.00,7.35><angle=270>" tt-word-print.tag-no FORMAT "x(20)" "</angle>"  .
 
PUT "<FArial><=6><R49><C5><P16> Qty/Case " SKIP.
PUT "<FArial><=6><R51><C6><B><P16>  " tt-word-print.pcs "</B>" SKIP.
PUT "<FArial><=6><R49><C22><P16> Case/Pallett " SKIP.
PUT "<FArial><=6><R51><C23><B><P16>  " tt-word-print.bundle "</B>" SKIP.
PUT "<FArial><=6><R49><C39><P16> Partial Case " SKIP.
PUT "<FArial><=6><R51><C40><B><P16>  " tt-word-print.partial "</B>" SKIP.
PUT "<FArial><=6><R49><C56><P16> Qty/Pallett " SKIP.
PUT "<FArial><=6><R51><C57><B><P16>  " tt-word-print.total-unit "</B>" SKIP.

PUT "<FArial><=7><R55><C5><P16> Weight Case " SKIP.
PUT "<FArial><=7><R57><C6><B><P16>  " tt-word-print.case-wt "</B>" SKIP.


PUT  "<R53><C22><FROM><R53><C70><LINE>" SKIP.
PUT  "<R53><C22><FROM><R64><C22><LINE>" SKIP.

PUT "<FArial><=8><R54><C24><P14> Counted By: " SKIP.

PUT "<R54><C36><#8><FROM><R54><C51><RECT><||3>" SKIP
    "<R56><C36><FROM><R56><C51><LINE><||3>" SKIP  
    "<R54><C36><FROM><R56><C36><LINE><||3>" SKIP
    "<R54><C51><FROM><R56><C51><LINE><||3>" SKIP.

PUT "<FArial><=9><R54><C53><P14> Date: " SKIP.

PUT "<R54><C60><#9><FROM><R54><C70><RECT><||3>" SKIP
    "<R56><C60><FROM><R56><C70><LINE><||3>" SKIP  
    "<R54><C60><FROM><R56><C60><LINE><||3>" SKIP
    "<R54><C70><FROM><R56><C70><LINE><||3>" SKIP.

PUT "<FArial><=10><R57><C24><P14> Checked By " SKIP.

PUT "<R57><C36><#10><FROM><R57><C51><RECT><||3>" SKIP
    "<R59><C36><FROM><R59><C51><LINE><||3>" SKIP  
    "<R57><C36><FROM><R59><C36><LINE><||3>" SKIP
    "<R57><C51><FROM><R59><C51><LINE><||3>" SKIP.

PUT "<FArial><=11><R57><C53><P14> Date: " SKIP.

PUT "<R57><C60><#11><FROM><R57><C70><RECT><||3>" SKIP
    "<R59><C60><FROM><R59><C70><LINE><||3>" SKIP  
    "<R57><C60><FROM><R59><C60><LINE><||3>" SKIP
    "<R57><C70><FROM><R59><C70><LINE><||3>" SKIP.


.

PUT "<FCourier New>".







