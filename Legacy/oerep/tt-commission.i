DEF {1} TEMP-TABLE tt-commission
   FIELD company                 LIKE ar-inv.company
   FIELD sman                    LIKE sman.sman EXTENT 1
   FIELD inv-no                  LIKE ar-invl.inv-no
   FIELD i-no                    LIKE ar-invl.i-no    
   FIELD inv-date                AS DATE COLUMN-LABEL "Invoice Date"
   FIELD create-date             AS DATE COLUMN-LABEL "Entered on"
   FIELD char1                   AS CHAR
   FIELD char2                   AS CHAR
   FIELD char3                   AS CHAR
   FIELD int1                    AS INT
   FIELD int2                    AS INT
   FIELD int3                    AS INT
   FIELD deci1                   AS DECI
   FIELD deci2                   AS DECI
   FIELD deci3                   AS DECI
   FIELD set-sale-price          LIKE oe-ordl.price COLUMN-LABEL "Set Sales Price"
   FIELD fixed-gross-profit      AS DECIMAL FORMAT ">>9.99" COLUMN-LABEL "Fixed Gross Profit" /* as percent */
   FIELD commission-adder        AS DEC
   FIELD base-cost               AS DECIMAL FORMAT "->>,>>>,>>9.99"
   FIELD set-sales-price         AS DECIMAL FORMAT "->>,>>>,>>9.99"
   FIELD overhead-percent        AS DECIMAL
   FIELD warehouse-percent       AS DECIMAL
   FIELD misc-percent            AS DECIMAL 
   FIELD freight-percent         AS DECIMAL 
   FIELD industrial-percent      AS DECIMAL 
   FIELD commission-rate-percent AS DECIMAL.

