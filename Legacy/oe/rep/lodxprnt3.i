/* oe/rep/invxprnt.i */                     

PUT  "<FArial>".
PUT  "<C+25><#1>".
PUT  "<=1>" SKIP.
PUT  "<C1><#2>".
PUT UNFORMATTED   
       "<C4><R5.5><#1><R+90><C+80><IMAGE#1=" ls-full-img1.

 PUT "<R33><C15><P25><FROM><R33><C80><LINE>" SKIP.

 PUT "<FArial><=9><R30> <B><P25> ITEM#: "   "<C30><P30>" tt-word-print.i-no FORMAT "x(15)"  "</B>" SKIP.

 PUT "<R38><C29><P25><FROM><R38><C80><LINE>" SKIP.

 PUT "<FArial><=9><R35> <B><P22> DESCRIPTION: "   "<C29>" tt-word-print.part-dscr1 FORMAT "x(25)"  "</B>" SKIP.

 PUT "<R42><C3><P25><FROM><R42><C80><LINE>" SKIP.

 PUT "<R48><C17><P25><FROM><R48><C37><LINE>" SKIP.

 PUT "<FArial><=9><R45><C3> <B><P25> QTY.: "   "<C17>" tt-word-print.ord-qty  "</B>".

 PUT "<R48><C51><P25><FROM><R48><C80><LINE>".

 PUT "<FArial><=9><R45><C40> <B><P23> FG#.: "    "<C52>" tt-word-print.i-no FORMAT "x(15)"  "</B>" SKIP.


 PUT "<R54><C17><P25><FROM><R54><C37><LINE>" SKIP.

 PUT "<FArial><=9><R51><C3> <B><P25> DATE: "    "<C17>" tt-word-print.due-date format "99/99/99"  "</B>".

 PUT "<R54><C51><P25><FROM><R54><C73><LINE>".

 PUT "<FArial><=9><R51><C40> <B><P23> P.O #: "    "<C55>" tt-word-print.po-no FORMAT ">>>>>>>>>"  "</B>" SKIP.

 PUT "<FArial><=9><R56><C40> <B><P23> ORDER# "    "<C55>" tt-word-print.ord-no FORMAT ">>>>>>>>>"  "</B>" SKIP.
 .

PUT "<FCourier New>".







