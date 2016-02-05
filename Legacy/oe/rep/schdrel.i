/* ----------------------------------------------- oe/rep/schdrel.i 02/99 JLF */
/* Schedule Release Report                                                    */
/* -------------------------------------------------------------------------- */

if chosen eq 2 then DO:
  if v-sort eq "N" then
    if v-ponum then DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.i-name      COLUMN-LABEL "Item Name"
                                   FORMAT "x(15)"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier" SKIP      
        
             WITH DOWN FRAME ordhead4-po NO-BOX STREAM-IO WIDTH 200.
         
         DOWN WITH FRAME ordhead4-po.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.i-name      COLUMN-LABEL "Item Name"
                                   FORMAT "x(15)"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier" SKIP      
        
             WITH DOWN FRAME ordhead41-po NO-BOX STREAM-IO WIDTH 200.
         
         DOWN WITH FRAME ordhead41-po.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job                '",'
            '"' w-ord.i-name                  '",'
            '"' w-ord.po-num                  '",'
            '"' w-ord.ord-no                  '",'
            '"' STRING(w-ord.rel-no,">>>>>9") '",'
            '"' w-ord.i-no                    '",'
            '"' w-ord.ord-qty                 '",'
            '"' w-ord.shp-qty                 '",'
            '"' w-ord.rel-qty                 '",'
            '"' w-ord.xls-rel-date            '",'
            '"' w-ord.xls-status              '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier                 '",'
            SKIP.
    END.

    ELSE DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.i-name      COLUMN-LABEL "Item Name"
                                   FORMAT "x(27)"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead4 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead4.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.i-name      COLUMN-LABEL "Item Name"
                                   FORMAT "x(27)"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead41 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead41.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' (IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job)      '",'
            '"' w-ord.i-name        '",'
            '"' w-ord.ord-no        '",'
            '"' w-ord.rel-no        '",'
            '"' w-ord.i-no          '",'
            '"' w-ord.ord-qty       '",'
            '"' w-ord.shp-qty       '",'
            '"' w-ord.rel-qty       '",'
            '"' w-ord.xls-rel-date  '",'
            '"' w-ord.xls-status    '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier       '",'
            SKIP.
    END.

  ELSE
  if v-print eq "I" then
    if v-sort eq "T" then DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.job         COLUMN-LABEL "Job No"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      COLUMN-LABEL "Description"
                                   FORMAT "x(17)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead1-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead1-terr.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      COLUMN-LABEL "Description"
                                   FORMAT "x(17)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead12-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead12-terr.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ship-id    '",'
            '"' w-ord.job        '",'
            '"' cust.terr        '",'
            '"' cust.del-zone    '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' w-ord.i-no       '",'
            '"' w-ord.i-name     '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.
    
    ELSE
    if v-ponum then DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(18)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      COLUMN-LABEL "Description"
                                   FORMAT "x(17)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead1-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead1-po.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(18)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      COLUMN-LABEL "Description"
                                   FORMAT "x(17)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead12-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead12-po.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job    '",'
            '"' w-ord.cust-name   '",'
            '"' w-ord.ship-id     '",'
            '"' w-ord.po-num      '",'
            '"' w-ord.ord-no      '",'
            '"' w-ord.rel-no      '",'
            '"' w-ord.i-no        '",'
            '"' w-ord.i-name      '",'
            '"' w-ord.rel-qty     '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier     '",'
            SKIP.
    END.

    ELSE DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(30)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      label "Description"
                                   FORMAT "x(22)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead1 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead1.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(30)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 w-ord.i-no        COLUMN-LABEL "Item"
                 w-ord.i-name      label "Description"
                                   FORMAT "x(22)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead12 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead12.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ship-id    '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' w-ord.i-no       '",'
            '"' w-ord.i-name     '",'
            '"' w-ord.rel-qty    '",' 
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.

  ELSE
  if v-print eq "S" then DO:  /* print sales value */
    if v-sort eq "T" then DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(13)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.job         COLUMN-LABEL "Job No"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                                   COLUMN-LABEL "Item"
                                   FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead2-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead2-terr.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(13)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead21-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead21-terr.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ship-id    '",'
            '"' w-ord.job        '",'
            '"' cust.terr        '",'
            '"' cust.del-zone    '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' w-ord.t-price    '",'
            '"' w-ord.msf        '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.
    
    ELSE
    if v-ponum then DO:

      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead2-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead2-po.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead21-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead21-po.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ship-id    '",'
            '"' w-ord.po-num     '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' w-ord.t-price    '",'
            '"' w-ord.msf        '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.

    ELSE DO:
      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(27)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead2 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead2.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(27)"
                 w-ord.ship-id     COLUMN-LABEL "Ship To"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.t-price     COLUMN-LABEL "Sales!Value"
                 w-ord.msf         COLUMN-LABEL "!MSF"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead21 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead21.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ship-id    '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' w-ord.t-price    '",'
            '"' w-ord.msf        '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.
  END.

  ELSE
    if v-sort eq "T" then DO:
      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(13)"
                 w-ord.job         COLUMN-LABEL "Job No"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead3-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead3-terr.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(13)"
                 cust.terr         when avail cust
                                   COLUMN-LABEL "Ter"
                 cust.del-zone     when avail cust
                                   COLUMN-LABEL "Del!Zone"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead31-terr NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead31-terr.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.job        '",'
            '"' cust.terr        '",'
            '"' cust.del-zone    '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.ord-qty    '",'
            '"' w-ord.shp-qty    '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.
    
    ELSE
    if v-ponum then DO:
      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                 COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead3-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead3-po.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(15)"
                 w-ord.po-num      COLUMN-LABEL "PO!Number"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                  COLUMN-LABEL "Item" FORMAT "X(15)"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead31-po NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead31-po.
      END.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.po-num     '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.ord-qty    '",'
            '"' w-ord.shp-qty    '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.

    ELSE DO:
      IF v-qty-opt NE "Job#" THEN
      DO:
         DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                   FORMAT "->>,>>>,>>9"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(27)"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                  COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead3 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead3.
      END.
      ELSE
      DO:
         DISPLAY w-ord.job         COLUMN-LABEL "Job No"
                 w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                   FORMAT "x(27)"
                 w-ord.ord-no      COLUMN-LABEL "Order!Number"
                 w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                                   FORMAT ">>>>>9"
                 (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) COLUMN-LABEL "Item"
                 w-ord.ord-qty     COLUMN-LABEL "Order!Quantity"
                 w-ord.shp-qty     COLUMN-LABEL "Quantity!Shipped"
                 w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                 w-ord.rel-date    COLUMN-LABEL "Release!Date"
                 w-ord.prom-code   COLUMN-LABEL " Due!Alert" WHEN tg-print-due
                 w-ord.carrier     COLUMN-LABEL "Carrier"      skip
        
             WITH DOWN FRAME ordhead31 NO-BOX STREAM-IO WIDTH 200.
        
         DOWN WITH FRAME ordhead31.
      END.

      IF tb_excel THEN

        PUT STREAM excel UNFORMATTED
            '"' IF v-qty-opt NE "Job#" THEN
                   STRING(w-ord.onh-qty,"->>,>>>,>>9")
                ELSE w-ord.job   '",'
            '"' w-ord.cust-name  '",'
            '"' w-ord.ord-no     '",'
            '"' w-ord.rel-no     '",'
            '"' (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name) '",'
            '"' w-ord.ord-qty    '",'
            '"' w-ord.shp-qty    '",'
            '"' w-ord.rel-qty    '",'
            '"' w-ord.xls-rel-date '",'
            '"' w-ord.xls-status   '",'
            '"' (IF tg-print-due THEN w-ord.prom-code ELSE "") '",'
            '"' w-ord.carrier    '",'
            SKIP.
    END.
END.

ELSE
IF chosen EQ 3 THEN DO:
  DEF VAR lv-issue AS CHAR FORMAT "x(5)" NO-UNDO.


  lv-issue = IF w-ord.rel-qty GT w-ord.onh-qty THEN "ISSUE" ELSE "OK".

  DISPLAY w-ord.cust-no     COLUMN-LABEL "Customer#"
                            FORMAT "x(08)"
          w-ord.rel-date    COLUMN-LABEL "Release!Date"
                            FORMAT "x(08)"
          w-ord.rel-no      COLUMN-LABEL "Rel!Num"
                            FORMAT ">>>>>9"
          w-ord.ship-id     COLUMN-LABEL "Ship To"
          w-ord.carrier     COLUMN-LABEL "Carrier"
          w-ord.ord-no      COLUMN-LABEL "Order!Number"
          w-ord.part-no     COLUMN-LABEL "Customer!Part#"
          w-ord.i-name      COLUMN-LABEL "Description"
                            FORMAT "x(24)"
          w-ord.i-no        COLUMN-LABEL "FG Item#"
                            FORMAT "x(12)"
          w-ord.po-num      COLUMN-LABEL "PO!Number"
                            FORMAT "x(12)"
          w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                            FORMAT "->>,>>>,>>9"
          w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
          w-ord.t-price     COLUMN-LABEL "Sales!Value"
                            FORMAT "$->>,>>>,>>9.99"
                            WHEN tb_show-val
          w-ord.palls       COLUMN-LABEL "No. of!Pallets"
                            FORMAT ">>>,>>9"
          lv-issue          COLUMN-LABEL "Status"
          SKIP
        
      WITH DOWN FRAME sched-rel3 NO-BOX STREAM-IO WIDTH 200.
        
  DOWN WITH FRAME sched-rel3.

  IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
        '"' w-ord.cust-no              '",'
        '"' SUBSTR(w-ord.rel-date,1,8) '",'
        '"' w-ord.rel-no               '",'
        '"' w-ord.ship-id              '",'
        '"' w-ord.carrier              '",'
        '"' w-ord.ord-no               '",'
        '"' w-ord.part-no              '",'
        '"' w-ord.i-name               '",'
        '"' w-ord.i-no                 '",'
        '"' w-ord.po-num               '",'
        '"' w-ord.onh-qty              '",'
        '"' w-ord.rel-qty              '",'
        '"' (IF tb_show-val THEN STRING(w-ord.t-price) ELSE "") '",'
        '"' w-ord.palls                '",'
        '"' lv-issue                   '",'
        SKIP.
END.

ELSE DO:
  IF tb_prt-qoh THEN
    IF tb_prt-last THEN
      IF rd_print2 BEGINS "M" THEN DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead1-po-mf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead1-po-mf.
        END.

        ELSE DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                  COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead1-po-me NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead1-po-me.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.onh-qty    '",'
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.last-date  '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.msf        '",'
              '"' itemfg.style     '",'
              SKIP.
      END.

      ELSE DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead1-po-pf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead1-po-pf.
        END.

        ELSE DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                   COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead1-po-pe NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead1-po-pe.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.onh-qty    '",'
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.last-date  '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.palls      '",'
              SKIP.
      END.

    ELSE
      IF rd_print2 BEGINS "M" THEN DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead2-po-mf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead2-po-mf.
        END.

        ELSE DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                  COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead2-po-me NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead2-po-me.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.onh-qty     '",'
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.msf        '",'
              '"' itemfg.style     '",'
              SKIP.
      END.

      ELSE DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead2-po-pf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead2-po-pf.
        END.

        ELSE DO:
          DISPLAY w-ord.onh-qty     COLUMN-LABEL "Quantity!On Hand"
                                    FORMAT "->>,>>>,>>9"
                  w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                   COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead2-po-pe NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead2-po-pe.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.onh-qty    '",'
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.msf        '",'
              '"' w-ord.palls      '",'
              SKIP.
      END.

  ELSE
    IF tb_prt-last THEN
      IF rd_print2 BEGINS "M" THEN DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead3-po-mf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead3-po-mf.
        END.

        ELSE DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                  COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead3-po-me NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead3-po-me.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.last-date  '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.msf        '",'
              '"' itemfg.style     '",'
              SKIP.
      END.

      ELSE DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead3-po-pf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead3-po-pf.
        END.

        ELSE DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release!Date"
                  w-ord.last-date   COLUMN-LABEL "LastShip!  Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                   COLUMN-LABEL "Description" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead3-po-pe NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead3-po-pe.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.last-date  '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.palls      '",'
              SKIP.
      END.

    ELSE
      IF rd_print2 BEGINS "M" THEN DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead4-po-mf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead4-po-mf.
        END.

        ELSE DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                   COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.msf         COLUMN-LABEL "MSF"
                  itemfg.style      COLUMN-LABEL "Style"
                                    FORMAT "x(6)"
        
              WITH DOWN FRAME jobhead4-po-me NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead4-po-me.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' w-ord.cust-name  '",'
              '"' w-ord.xls-rel-date '",'
              '"' w-ord.xls-status   '",'
              '"' w-ord.po-num     '",'
              '"' w-ord.job        '",'
              '"' w-ord.part-no    '",'
              '"' w-ord.i-no       '",'
              '"' w-ord.i-name     '",'
              '"' w-ord.rel-qty    '",'
              '"' w-ord.msf        '",'
              '"' itemfg.style     '",'
              SKIP.
      END.

      ELSE DO:
        IF rd_print3 BEGINS "F" THEN DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  w-ord.i-no        COLUMN-LABEL "Item"
                  w-ord.i-name      COLUMN-LABEL "Description"
                                    FORMAT "x(20)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead4-po-pf NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead4-po-pf.
        END.

        ELSE DO:
          DISPLAY w-ord.cust-name   COLUMN-LABEL "Customer Name"
                                    FORMAT "x(16)"
                  w-ord.rel-date    COLUMN-LABEL "Release! Date"
                  w-ord.po-num      COLUMN-LABEL "PO!Number"
                  w-ord.job         COLUMN-LABEL "Job No"
                  w-ord.part-no     COLUMN-LABEL "Part!Number"
                  (IF rs-item-option EQ "#" THEN w-ord.i-no ELSE w-ord.i-name)
                   COLUMN-LABEL "Item" FORMAT "x(30)"
                  w-ord.rel-qty     COLUMN-LABEL "Release!Quantity"
                  w-ord.palls       COLUMN-LABEL "Pallet!  Qty"
        
              WITH DOWN FRAME jobhead4-po-pe NO-BOX STREAM-IO WIDTH 200.
        
          DOWN WITH FRAME jobhead4+-po-pe.
        END.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
               '"' w-ord.cust-name  '",'
               '"' w-ord.xls-rel-date '",'
               '"' w-ord.xls-status   '",'
               '"' w-ord.po-num     '",'
               '"' w-ord.job        '",'
               '"' w-ord.part-no    '",'
               '"' w-ord.i-no       '",'
               '"' w-ord.i-name     '",'
               '"' w-ord.rel-qty    '",'
               '"' w-ord.msf        '",'
               '"' w-ord.palls      '",'
               SKIP.
      END.
END.

FOR EACH tt-formtext:
  DELETE tt-formtext.
END.

IF tb_notes AND AVAIL itemfg THEN DO:
  lv-text = "".

  FOR EACH notes
      WHERE notes.rec_key   EQ itemfg.rec_key
        AND notes.note_type EQ "S"
        AND notes.note_code GE begin_spec
        AND notes.note_code LE end_spec
      NO-LOCK
      BREAK BY notes.note_code:
    IF FIRST-OF(notes.note_code) THEN
      lv-text = lv-text + " " + TRIM(notes.note_code) + ":".
              
    lv-text = lv-text + " " + TRIM(notes.note_text).
     
    IF LAST-OF(notes.note_code) THEN lv-text = lv-text + CHR(10).
  END.

  DO li = 1 TO 50:
    CREATE tt-formtext.
    ASSIGN
     tt-line-no = li
     tt-length  = 115.
  END.

  RUN custom/formtext.p (lv-text).

  FOR EACH tt-formtext WHERE tt-text EQ "":
    DELETE tt-formtext.
  END.
END.

IF tb_stats THEN DO:
  FOR EACH tt-fg-set:
    DELETE tt-fg-set.
  END.

  RELEASE job-hdr.
  RELEASE job.
  RELEASE reftable.

  IF TRIM(w-ord.job-no) EQ "" THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ cocode
        AND job-hdr.ord-no  EQ w-ord.ord-no
        AND job-hdr.cust-no EQ w-ord.cust-no
        AND job-hdr.i-no    EQ w-ord.i-no
        AND job-hdr.opened  EQ YES
      NO-LOCK
      BY ROWID(job-hdr) DESC:
    LEAVE.
  END.

  ELSE DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no  EQ w-ord.job-no
          AND job-hdr.job-no2 EQ w-ord.job-no2
          AND job-hdr.ord-no  EQ w-ord.ord-no
          AND job-hdr.i-no    EQ w-ord.i-no
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no  EQ w-ord.job-no
          AND job-hdr.job-no2 EQ w-ord.job-no2
          AND job-hdr.ord-no  EQ w-ord.ord-no
        NO-LOCK NO-ERROR.
  END.

  IF AVAIL job-hdr THEN
  FIND FIRST job
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
      NO-LOCK NO-ERROR.

  IF AVAIL job THEN DO:
    
    IF (itemfg.isaset OR w-ord.is-a-component)                          AND
       CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                  AND reftable.company  EQ job.company
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(job.job,"999999999")) THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-hdr.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
          AND ((reftable.code2  EQ w-ord.i-no AND w-ord.is-a-component) OR
               (job-hdr.i-no    EQ w-ord.i-no AND NOT w-ord.is-a-component))
        NO-LOCK:
      CREATE tt-fg-set.
      ASSIGN
       tt-fg-set.part-no      = reftable.code2
       tt-fg-set.part-qty     = reftable.val[12]
       tt-fg-set.part-qty-dec = reftable.val[13].
    END.

    ELSE DO:
      CREATE tt-fg-set.
      ASSIGN
       tt-fg-set.part-no      = job-hdr.i-no
       tt-fg-set.part-qty     = job-hdr.frm
       tt-fg-set.part-qty-dec = job-hdr.blank-no.
    END.

    FOR EACH tt-fg-set
        BREAK BY tt-fg-set.part-qty
              BY tt-fg-set.part-qty-dec:

      PUT SPACE(5)
          "S/B: "
          TRIM(STRING(tt-fg-set.part-qty,">>")) + "/" +
              TRIM(STRING(tt-fg-set.part-qty-dec,">>"))   FORMAT "x(5)"
          SPACE(1).

      ll-po = NO.
   
      IF LAST-OF(tt-fg-set.part-qty) THEN
      FOR EACH po-ordl
          WHERE po-ordl.company   EQ job.company
            AND po-ordl.job-no    EQ job.job-no
            AND po-ordl.job-no2   EQ job.job-no2
            AND po-ordl.s-num     EQ tt-fg-set.part-qty
            AND po-ordl.item-type EQ YES
          USE-INDEX job-no NO-LOCK,
          FIRST po-ord
          WHERE po-ord.company EQ po-ordl.company
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-LOCK,
          FIRST item
          WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
            AND INDEX("1234BPR",item.mat-type) GT 0
          NO-LOCK
          BREAK BY po-ordl.po-no
                BY po-ordl.i-no
                BY po-ordl.rec_key:

        ll-po = YES.

        IF po-ordl.pr-qty-uom EQ "EA" THEN
          ld-qty-ord = po-ordl.ord-qty.
        ELSE
          RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                                 item.basis-w,
                                 po-ordl.s-len,
                                 po-ordl.s-wid,
                                 item.s-dep,
                                 po-ordl.ord-qty, output ld-qty-ord).
        {sys/inc/roundup.i ld-qty-ord}

        IF po-ordl.cons-uom EQ "EA" THEN
          ld-qty-rec = po-ordl.t-rec-qty.
        ELSE
          RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA",
                                 item.basis-w,
                                 po-ordl.s-len,
                                 po-ordl.s-wid,
                                 item.s-dep,
                                 po-ordl.t-rec-qty, output ld-qty-rec).
        {sys/inc/roundup.i ld-qty-rec}

        IF NOT LAST(po-ordl.po-no) THEN PUT SPACE(16).

        PUT "Brd PO#: " 
            TRIM(STRING(po-ordl.po-no,">>>>>>>>")) FORMAT "x(8)"
            SPACE(1)
            "Vendor: "
            po-ord.vend-no                         FORMAT "x(8)"
            SPACE(1)
            "Qty Rec'd: "
            TRIM(STRING(ld-qty-rec,">>>,>>>,>>9")) FORMAT "x(11)"
            SPACE(1).

        IF NOT LAST(po-ordl.po-no) THEN PUT SKIP.
      END.

      IF NOT ll-po THEN PUT SPACE(58).

      FOR EACH job-mch
          WHERE job-mch.company EQ job.company
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2
            AND job-mch.frm     EQ tt-fg-set.part-qty
          NO-LOCK
          BREAK BY job-mch.line:
        IF FIRST(job-mch.line) THEN PUT "Routing: ".
        PUT UNFORMATTED job-mch.m-code.
        IF NOT LAST(job-mch.line) THEN PUT ", ".
      END.

      PUT SKIP.

      IF LAST(tt-fg-set.part-qty)        AND
         NOT CAN-FIND(FIRST tt-formtext) THEN PUT SKIP(1).
    END.
  END.
END.

FOR EACH tt-formtext BREAK BY tt-line-no:
  IF FIRST(tt-line-no) THEN
    PUT SPACE(5)
        "Spec Notes: ".

  PUT tt-text AT 18 FORMAT "x(115)" SKIP.
         
  IF LAST(tt-line-no) THEN PUT SKIP(1).

END.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
