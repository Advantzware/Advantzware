
DEF {1} SHARED TEMP-TABLE w-ord
  FIELD ord-no LIKE oe-ord.ord-no
  FIELD job-no LIKE oe-ordl.job-no
  FIELD job-no2 LIKE oe-ordl.job-no2
  FIELD cust-no LIKE oe-ord.cust-no
  FIELD cust-name LIKE oe-ord.cust-name
  FIELD cust-add1 AS CHAR FORMAT "x(30)"
  FIELD cust-add2 AS CHAR FORMAT "x(30)"
  FIELD cust-city LIKE cust.city
  FIELD cust-state LIKE cust.state
  FIELD cust-zip LIKE cust.zip
  FIELD cust-ctry LIKE cust.country
  FIELD i-no LIKE oe-ordl.i-no
  FIELD cust-part-no LIKE oe-ordl.part-no
  FIELD cust-po-no LIKE oe-ordl.po-no
  FIELD ord-qty LIKE oe-ordl.qty
  FIELD pcs LIKE eb.cas-cnt
  FIELD bundle LIKE eb.cas-pal format ">>>9"    /* 9902 CAH per TRIAD */
  FIELD total-unit LIKE eb.tr-cnt
  FIELD total-tags AS int
  FIELD ship-ctry LIKE shipto.country
  FIELD sold-code LIKE soldto.sold-id
  FIELD sold-name LIKE soldto.sold-name
  FIELD sold-add1 AS char FORMAT "x(30)"
  FIELD sold-add2 AS char FORMAT "x(30)"
  FIELD sold-city LIKE soldto.sold-city
  FIELD sold-state LIKE soldto.sold-state
  FIELD sold-zip LIKE soldto.sold-zip
  FIELD sold-ctry LIKE soldto.country
  FIELD ship-code LIKE shipto.ship-id
  FIELD ship-name LIKE shipto.ship-name
  FIELD ship-add1 AS char FORMAT "x(30)"
  FIELD ship-add2 AS char FORMAT "x(30)"
  FIELD ship-city LIKE shipto.ship-city
  FIELD ship-state LIKE shipto.ship-state
  FIELD ship-zip LIKE shipto.ship-zip
  FIELD i-name LIKE oe-ordl.i-name
  FIELD part-dscr1 LIKE oe-ordl.part-dscr1
  FIELD part-dscr2 LIKE oe-ordl.part-dscr2
  FIELD part-dscr3 LIKE oe-ordl.part-dscr3
  FIELD due-date LIKE oe-ord.due-date
  FIELD rel-date LIKE oe-rel.rel-date
  FIELD upc-no LIKE eb.upc-no
  FIELD mult AS INT
  FIELD est-no LIKE oe-ordl.est-no
  FIELD form-no LIKE oe-ordl.form-no
  FIELD box-len AS DEC FORMAT ">>>9.99<<<"
  FIELD box-wid AS DEC FORMAT ">>>9.99<<<"
  FIELD box-dep AS DEC FORMAT ">>>9.99<<<"
  FIELD flute AS CHAR
  FIELD test AS CHAR
  FIELD vendor LIKE company.name
  FIELD gross-wt AS DEC FORMAT ">>>>9.99"
  FIELD tare-wt AS DEC FORMAT ">>>>9.99"
  FIELD net-wt AS DEC FORMAT ">>>>9.99"
  FIELD sheet-wt AS DEC FORMAT ">>>9.99"
  FIELD uom LIKE oe-ordl.pr-uom
  FIELD partial AS INT FORM ">>>,>>9"
  FIELD dont-run-set AS LOG INIT NO
  FIELD cas-no LIKE eb.cas-no
  FIELD prod-notes LIKE itemfg.prod-notes
  FIELD po-no LIKE po-ord.po-no
  FIELD l-code AS CHAR FORMAT "x"
  FIELD case-wt AS INT FORMAT ">>>>9"
  FIELD lot# AS CHAR FORMAT "x(15)"
  FIELD rel-lot# AS CHAR FORMAT "X(15)"
  FIELD draw# AS CHAR FORMAT "x(15)"
  FIELD over-pct LIKE oe-ordl.over-pct
  FIELD qty-before LIKE oe-ordl.qty
  FIELD style AS CHAR
  FIELD style-desc AS CHAR FORMAT "X(30)"
  FIELD ord-desc1 AS CHAR FORMAT "X(30)"
  FIELD ord-desc2 AS CHAR FORMAT "X(30)"
  FIELD due-date-job AS CHAR
  FIELD due-date-jobhdr AS CHAR
  FIELD is-component AS LOG
  /* gdm - 08130804 */
  FIELD linenum like oe-ordl.e-num
  /* gdm - 07170905 */
  FIELD unit-wt  LIKE loadtag.misc-dec[1]
  FIELD pallt-wt LIKE loadtag.misc-dec[2]
  FIELD pallt-no LIKE eb.tr-no
  FIELD ship-notes LIKE oe-rel.ship-i
  /* rstark - zoho13731 */
  FIELD SSCC AS CHARACTER FORMAT "x(20)"
  .
