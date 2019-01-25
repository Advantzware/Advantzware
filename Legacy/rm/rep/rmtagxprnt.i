/* oe/rep/invxprnt.i */
DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTagno AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVenderTagno AS CHARACTER NO-UNDO.
ASSIGN cTagno = tt-po-print.tag-no 
       cVenderTagno = tt-po-print.vend-tag  .

PUT "<FArial>".
PUT "<C+25><#1>".
PUT "<=1>" SKIP.
PUT "<C1><#2>".
PUT "<R4><C3><#4><FROM><R4><C83><RECT><||3>" SKIP
    "<R11><C3><FROM><R11><C83><LINE><||3>" SKIP  
    "<R4><C3><FROM><R11><C3><LINE><||3>" SKIP
    "<R4><C83><FROM><R11><C83><LINE><||3>" SKIP.

PUT "<FArial><R6><C10><P25><B>" tt-po-print.vend-name FORMAT "x(30)" "</B>" SKIP.
PUT "<FArial><R12><C25><P16><B>" "Purchased Materials"  "</B>" SKIP(2).
PUT "<FArial><R14><C23><P16><B>" "Material Item Description" "</B>" .

PUT "<R16><C3><#5><FROM><R16><C83><RECT><||3>" SKIP
    "<R23><C3><FROM><R23><C83><LINE><||3>" SKIP  
    "<R16><C3><FROM><R23><C3><LINE><||3>" SKIP
    "<R16><C83><FROM><R23><C83><LINE><||3>" SKIP.

PUT "<FArial><R19><C5><P18><B>" tt-po-print.i-no FORMAT "x(20)" "</B>".
PUT "<FArial><R19><C30><P18><B>" tt-po-print.i-name FORMAT "x(30)" "</B>" SKIP.

PUT "<FArial><R24><C5><P16><B>" "Purchase Order No"  "</B>" SKIP.
PUT "<FArial><R24><C40><P16><B>" "Quantity"  "</B>" SKIP.
PUT "<FArial><R24><C64><P16><B>" "Stock UOM"  "</B>" SKIP.

PUT "<R26><C3><#6><FROM><R26><C28><RECT><||3>" SKIP
    "<R30><C3><FROM><R30><C28><LINE><||3>" SKIP  
    "<R26><C3><FROM><R30><C3><LINE><||3>" SKIP
    "<R26><C28><FROM><R30><C28><LINE><||3>" SKIP.

PUT "<FArial><R27><C10><P16><B>" tt-po-print.po-no  "</B>".                
                                                                     
PUT "<R26><C31><#7><FROM><R26><C56><RECT><||3>" SKIP                 
    "<R30><C31><FROM><R30><C56><LINE><||3>" SKIP  
    "<R26><C31><FROM><R30><C31><LINE><||3>" SKIP
    "<R26><C56><FROM><R30><C56><LINE><||3>" SKIP.

PUT "<FArial><R27><C42><P16><B>" tt-po-print.rcpt-qty  "</B>".

PUT "<R26><C59><#8><FROM><R26><C83><RECT><||3>" SKIP
    "<R30><C59><FROM><R30><C83><LINE><||3>" SKIP  
    "<R26><C59><FROM><R30><C59><LINE><||3>" SKIP
    "<R26><C83><FROM><R30><C83><LINE><||3>" SKIP.

PUT "<FArial><R27><C66><P16><B>" tt-po-print.pr-qty-uom  "</B>" SKIP.

PUT "<FArial><R32><C8><P16><B>" "Bin Location"  "</B>" SKIP.
PUT "<FArial><R32><C40><P16><B>" "Weight"  "</B>" SKIP.
PUT "<FArial><R32><C64><P16><B>" "Date Received"  "</B>" SKIP.

PUT "<R34><C3><#9><FROM><R34><C28><RECT><||3>" SKIP
    "<R38><C3><FROM><R38><C28><LINE><||3>" SKIP  
    "<R34><C3><FROM><R38><C3><LINE><||3>" SKIP
    "<R34><C28><FROM><R38><C28><LINE><||3>" SKIP.

PUT "<FArial><R35><C10><P16><B>" tt-po-print.loc-bin FORMAT "x(8)"  "</B>".

PUT "<R34><C31><#10><FROM><R34><C56><RECT><||3>" SKIP
    "<R38><C31><FROM><R38><C56><LINE><||3>" SKIP  
    "<R34><C31><FROM><R38><C31><LINE><||3>" SKIP
    "<R34><C56><FROM><R38><C56><LINE><||3>" SKIP.

PUT "<FArial><R35><C42><P16><B>" " "  "</B>".

PUT "<R34><C59><#11><FROM><R34><C83><RECT><||3>" SKIP
    "<R38><C59><FROM><R38><C83><LINE><||3>" SKIP  
    "<R34><C59><FROM><R38><C59><LINE><||3>" SKIP
    "<R34><C83><FROM><R38><C83><LINE><||3>" SKIP.

PUT "<FArial><R35><C66><P16><B>" tt-po-print.tag-date  "</B>" SKIP.
PUT "<FArial><R39><C36><P16><B>" "Vendor"  "</B>" SKIP.

PUT "<R41><C3><#12><FROM><R41><C83><RECT><||3>" SKIP
    "<R45><C3><FROM><R45><C83><LINE><||3>" SKIP  
    "<R41><C3><FROM><R45><C3><LINE><||3>" SKIP
    "<R41><C83><FROM><R45><C83><LINE><||3>" SKIP.

PUT "<FArial><R42><C10><P16><B>" tt-po-print.vend-no FORMAT "x(12)"  "</B>" .

PUT "<R48><C3><#13><FROM><R48><C83><RECT><||3>" SKIP
    "<R53.5><C3><FROM><R53.5><C83><LINE><||3>" SKIP  
    "<R48><C3><FROM><R53.5><C3><LINE><||3>" SKIP
    "<R48><C83><FROM><R53.5><C83><LINE><||3>" SKIP.

/*PUT "<FArial><R49><C10><P16><B>" tt-po-print.vend-name FORMAT "x(30)"  "</B>".*/
PUT "<FArial><R47><C10><FROM><AT=+.8,+6><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" cVenderTagno FORMAT 'x(20)' ">"
    "<AT=,2.6>" cVenderTagno FORMAT "x(20)"  .

PUT "<R55><C3><#14><FROM><R55><C83><RECT><||3>" SKIP
    "<R66><C3><FROM><R66><C83><LINE><||3>" SKIP  
    "<R55><C3><FROM><R66><C3><LINE><||3>" SKIP
    "<R55><C83><FROM><R66><C83><LINE><||3>" SKIP.

PUT "<FArial><R56><C6><P16><B>" "PKG ID-Unit (S)"  "</B>" SKIP(2).

PUT "<#=100><AT=,1.0><FROM><AT=+.8,+6><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" cTagno FORMAT 'x(20)' ">"
    "<AT=,2.6>" cTagno FORMAT "x(20)"  .

PUT "<FCourier New>".
