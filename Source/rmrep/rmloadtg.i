DEFINE {1} SHARED TEMP-TABLE w-po NO-UNDO
  FIELD acknowledge LIKE po-ord.acknowledge
  FIELD addr LIKE po-ord.addr
  FIELD bill-to LIKE po-ord.bill-to
  FIELD buyer LIKE po-ord.buyer
  FIELD carrier LIKE po-ord.carrier
  FIELD city LIKE po-ord.city
  FIELD company LIKE po-ord.company
  FIELD contact LIKE po-ord.contact
  FIELD curr-code LIKE po-ord.curr-code
  FIELD cust-no LIKE po-ord.cust-no
  FIELD deleted LIKE po-ord.deleted
  FIELD due-date LIKE po-ord.due-date
  FIELD ex-rate LIKE po-ord.ex-rate
  FIELD fob-code LIKE po-ord.fob-code
  FIELD frt-pay LIKE po-ord.frt-pay
  FIELD last-ship-date LIKE po-ord.last-ship-date
  FIELD loc LIKE po-ord.loc
  FIELD opened LIKE po-ord.opened
  FIELD over-pct LIKE po-ord.over-pct
  FIELD po-change-date LIKE po-ord.po-change-date
  FIELD po-date LIKE po-ord.po-date
  FIELD po-no LIKE po-ord.po-no
  FIELD printed LIKE po-ord.printed
  FIELD received LIKE po-ord.received
  FIELD ship-addr LIKE po-ord.ship-addr
  FIELD ship-city LIKE po-ord.ship-city
  FIELD ship-id LIKE po-ord.ship-id
  FIELD ship-name LIKE po-ord.ship-name
  FIELD ship-no LIKE po-ord.ship-no
  FIELD ship-state LIKE po-ord.ship-state
  FIELD ship-zip LIKE po-ord.ship-zip
  FIELD spec-i LIKE po-ord.spec-i
  FIELD stat LIKE po-ord.stat
  FIELD state LIKE po-ord.state
  FIELD t-cost LIKE po-ord.t-cost
  FIELD t-freight LIKE po-ord.t-freight
  FIELD tax LIKE po-ord.tax
  FIELD tax-gr LIKE po-ord.tax-gr
  FIELD tax-id LIKE po-ord.tax-id
  FIELD terms LIKE po-ord.terms
  FIELD type LIKE po-ord.type
  FIELD under-pct LIKE po-ord.under-pct
  FIELD upd-date LIKE po-ord.upd-date
  FIELD upd-time LIKE po-ord.upd-time
  FIELD vend-no LIKE po-ord.vend-no
  FIELD zip LIKE po-ord.zip
  FIELD actnum LIKE po-ordl.actnum
  FIELD adders$ LIKE po-ordl.adders$
  FIELD b-num LIKE po-ordl.b-num
  FIELD cons-cost LIKE po-ordl.cons-cost
  FIELD cons-qty LIKE po-ordl.cons-qty
  FIELD cons-uom LIKE po-ordl.cons-uom
  FIELD cost LIKE rm-rctd.cost
  FIELD disc LIKE po-ordl.disc
  FIELD dscr LIKE po-ordl.dscr
  FIELD i-name LIKE po-ordl.i-name
  FIELD i-no LIKE po-ordl.i-no
  FIELD item-type LIKE po-ordl.item-type
  FIELD j-no LIKE po-ordl.j-no
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD line LIKE po-ordl.line
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD ord-qty LIKE po-ordl.ord-qty
  FIELD pr-qty-uom LIKE po-ordl.pr-qty-uom
  FIELD pr-uom LIKE po-ordl.pr-uom
  FIELD pur-cnt LIKE po-ordl.pur-cnt
  FIELD rel-qty LIKE po-ordl.rel-qty
  FIELD s-len LIKE po-ordl.s-len
  FIELD s-num LIKE po-ordl.s-num
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD setup LIKE po-ordl.setup
  FIELD ship-i LIKE po-ordl.ship-i
  FIELD t-inv-qty LIKE po-ordl.t-inv-qty
  FIELD t-rec-qty LIKE po-ordl.t-rec-qty
  FIELD t-rel-qty LIKE po-ordl.t-rel-qty
  FIELD vend-i-no LIKE po-ordl.vend-i-no
  FIELD company-name LIKE company.name
  FIELD loc-bin LIKE item.loc-bin
  FIELD partial AS INT FORMAT '>>>,>>9'
  FIELD rcpt-qty LIKE rm-rctd.qty
  FIELD tot-rec-qty LIKE rm-rctd.qty
  FIELD tag-date AS DATE
  FIELD total-tags AS INT
  FIELD vend-name LIKE vend.NAME 
  FIELD overrun-qty AS INT
  FIELD add-setup AS LOG  
  FIELD case-l as DECIMAL
  FIELD case-w as DECIMAL
  FIELD case-d AS DECIMAL  
  INDEX po IS PRIMARY po-no ASC line ASC.
