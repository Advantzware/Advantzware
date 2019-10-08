/* oerep/tt-comm-calc.i rtc 11/20/2008 */

DEF {1} TEMP-TABLE tt-comm-calc 
   FIELD slsm                    LIKE sman.sman EXTENT 1
   FIELD company                 LIKE ar-inv.company 
   FIELD inv-date                AS DATE COLUMN-LABEL "Invoice Date"
   FIELD cust-no                 LIKE cust.cust-no
   FIELD inv-no                  LIKE ar-inv.inv-no
   FIELD inv-type                AS CHAR
   FIELD rec-id                  AS RECID
   FIELD row-id                  AS ROWID
   FIELD job-no                  LIKE job.job-no
   FIELD job-no2                 LIKE job.job-no2
   FIELD ord-no                  LIKE ar-invl.ord-no
   FIELD i-no                    LIKE ar-invl.i-no    COLUMN-LABEL "FG Item!Number" 
   FIELD qty                     AS DECI              COLUMN-LABEL "QTY" 
   FIELD orig-inv-qty            AS DECI
   FIELD set-sell-price-qty      AS DECI
   FIELD base-cost-qty           AS DECI
   FIELD p-sman                  AS CHAR format "x(3)"
   FIELD camt                    LIKE ar-invl.amt
   FIELD prof                    LIKE ar-invl.amt
   FIELD comm                    AS DEC FORMAT ">>9.99"
   FIELD gp                      AS DEC FORMAT ">>9.99"
   FIELD slsc                    LIKE ar-invl.s-comm EXTENT 1
   FIELD slsp                    LIKE ar-invl.s-pct EXTENT 1
   FIELD procat                  LIKE itemfg.procat   COLUMN-LABEL "FG Cat."
   FIELD amt                     AS DECIMAL  COLUMN-LABEL "Invoice!Total"
   FIELD orig-inv-amt            AS DECIMAL     COLUMN-LABEL "Invoice!Total"
   FIELD cost                    LIKE ar-invl.t-cost  COLUMN-LABEL "Invoice!T-Cost"
   FIELD cust-part               LIKE ar-invl.part-no 
   FIELD basis                   LIKE sman.commbasis
   FIELD tot-samt                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD tot-camt                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD tot-cost                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD part-fg                 LIKE ar-invl.part-no
   FIELD base-cost               AS DECI COLUMN-LABEL "Base!Cost"       /* input field */
   FIELD overhead-percent        AS DECI COLUMN-LABEL "Add!Overhead"    /* input field */
   FIELD warehouse-percent       AS DECI COLUMN-LABEL "Add!Warehouse"   /* input field */
   FIELD misc-percent            AS DECI COLUMN-LABEL "Add!Misc"        /* input field */
   FIELD freight-percent         AS DECI COLUMN-LABEL "Add!Freight"     /* input field */
   FIELD industrial-percent      AS DECI COLUMN-LABEL "Add!Industrial"  /* input field */
   FIELD commission-rate-percent AS DECI COLUMN-LABEL "Commission!Rate"  /* input field */
   FIELD sell-price              AS DECI COLUMN-LABEL "Sell Price"
   FIELD ordl-sell-price         LIKE oe-ordl.price COLUMN-LABEL "Order Line!Sale Price"
   FIELD sale-price-matrix       AS DECI
   FIELD uom                     AS CHAR COLUMN-LABEL "UOM"
   FIELD commission-cost         AS DECI COLUMN-LABEL "Commission!Cost"
   FIELD profit-margin           AS DECI COLUMN-LABEL "Profit!Margin $"
   FIELD profit-margin-percent   AS DECI COLUMN-LABEL "Profit!Margin %"
   FIELD total-costs             AS DECI COLUMN-LABEL "Total!Costs"
   FIELD commission              AS DECI COLUMN-LABEL "Commission"
   FIELD tot-item-resale         AS DECI COLUMN-LABEL "Total Item!Resale"
   FIELD pr-uom                  LIKE oe-ordl.pr-uom
   FIELD cost-uom                LIKE ar-invl.cons-uom
   FIELD set-sell-price-uom      LIKE oe-ordl.pr-uom
   FIELD base-cost-uom           LIKE oe-ordl.pr-uom
   FIELD cas-cnt                 LIKE oe-ordl.cas-cnt
   FIELD ordl-qty                LIKE oe-ordl.qty
   FIELD sname                   LIKE sman.sname
   FIELD set-sales-price         LIKE oe-ordl.price COLUMN-LABEL "Set Sales Price"
   FIELD fixed-gross-profit      AS DEC COLUMN-LABEL "Fixed Gross Profit" /* as percent */
   FIELD commission-adder        AS DEC
   FIELD rebate-percent          AS DEC
   FIELD bol-no                  LIKE oe-boll.bol-no.
