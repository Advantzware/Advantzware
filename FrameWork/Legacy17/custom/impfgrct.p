/* import FGReceipts from xml files */


DEF TEMP-TABLE FGReceiptRow LIKE fg-rctd.


/*  write xml files ===
FOR EACH fg-rctd WHERE rita-code = "R":
    CREATE tt-rctd.
    BUFFER-COPY fg-rctd TO tt-rctd.
END.

TEMP-TABLE tt-rctd:WRITE-XML("FILE","c:\temp\fg-rctd.xml", TRUE).
==========*/

TEMP-TABLE FGReceiptRow:READ-XML ("File", "c:\tmp\FGReceipts.xml", "Empty",?,NO).

FOR EACH FGReceiptRow:
    DISP FGReceiptRow WITH 2 COL.
END.