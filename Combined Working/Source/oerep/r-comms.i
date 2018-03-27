
DEF {1} TEMP-TABLE tt-report LIKE report
   FIELD row-id                  AS ROWID
   FIELD job-no                  LIKE job.job-no
   FIELD job-no2                 LIKE job.job-no2
   FIELD ord-no                  LIKE ar-invl.ord-no
   FIELD i-no                    LIKE ar-invl.i-no    COLUMN-LABEL "FG Item!Number" 
   FIELD qty                     AS DECI              COLUMN-LABEL "QTY" 
   FIELD p-sman                  AS CHAR format "x(3)"
   FIELD camt                    LIKE ar-invl.amt
   FIELD prof                    LIKE ar-invl.amt
   FIELD comm                    AS DEC FORMAT ">>9.99"
   FIELD gp                      AS DEC FORMAT ">>9.99"
   FIELD slsm                    LIKE ar-invl.sman EXTENT 1
   FIELD slsc                    LIKE ar-invl.s-comm EXTENT 1
   FIELD slsp                    LIKE ar-invl.s-pct EXTENT 1
   FIELD inv-no                  LIKE ar-invl.inv-no
   FIELD procat                  LIKE itemfg.procat   COLUMN-LABEL "FG Cat."
   FIELD amt                     LIKE ar-invl.amt     COLUMN-LABEL "Invoice!Total"
   FIELD cost                    LIKE ar-invl.t-cost  COLUMN-LABEL "Invoice!T-Cost"
   FIELD cust-part               LIKE ar-invl.part-no 
   FIELD basis                   LIKE sman.commbasis
   FIELD tot-samt                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD tot-camt                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD tot-cost                AS DEC FORMAT "->>>>>>>9.99" EXTENT 3
   FIELD part-fg                 LIKE ar-invl.part-no
   FIELD base-cost               AS DECI COLUMN-LABEL "Base!Cost"       /* input field */
   FIELD overhead                AS DECI COLUMN-LABEL "Add!Overhead"    /* input field */
   FIELD warehouse               AS DECI COLUMN-LABEL "Add!Warehouse"   /* input field */
   FIELD misc                    AS DECI COLUMN-LABEL "Add!Misc"        /* input field */
   FIELD freight                 AS DECI COLUMN-LABEL "Add!Freight"     /* input field */
   FIELD industrial              AS DECI COLUMN-LABEL "Add!Industrial"  /* input field */
   FIELD commission-rate         AS DECI COLUMN-LABEL "Commission!Rate"  /* input field */
   FIELD sell-price              AS DECI COLUMN-LABEL "Sell Price"
   FIELD ordl-sell-price         LIKE oe-ordl.t-price COLUMN-LABEL "Order Line!Sale Price"
   FIELD sale-price-matrix       AS DECI
   FIELD uom                     AS CHAR COLUMN-LABEL "UOM"
   FIELD commission-cost         AS DECI COLUMN-LABEL "Commission!Cost"
   FIELD profit-margin           AS DECI COLUMN-LABEL "Profit!Margin $"
   FIELD profit-margin-percent   AS DECI COLUMN-LABEL "Profit!Margin %"
   FIELD total-costs             AS DECI COLUMN-LABEL "Total!Costs"
   FIELD commission              AS DECI COLUMN-LABEL "Commission"
   FIELD tot-item-resale         AS DECI COLUMN-LABEL "Total Item!Resale"
   FIELD pr-uom                  LIKE oe-ordl.pr-uom  
   FIELD cas-cnt                 LIKE oe-ordl.cas-cnt
   FIELD ordl-qty                LIKE oe-ordl.qty
   FIELD sname                   LIKE sman.sname.
