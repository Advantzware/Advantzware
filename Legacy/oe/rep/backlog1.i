
def workfile w-ord
  field ord-no as char format "x(9)"
  field est-no like oe-ord.est-no
  field ship-date like oe-ordl.req-date format "99/99/99"
  field due-date like oe-ordl.prom-date format "99/99/99"
  field ord-date like oe-ord.ord-date
  field cust-no like oe-ord.cust-no
  field cust-name like oe-ord.cust-name
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field part-no like oe-ordl.part-no
  field qty-due as int format "->>>,>>>,>>9"
  field qty-onh like itemfg.q-onh
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price format ">>,>>9.99"
  field uom like oe-ordl.pr-uom
  field disc like oe-ordl.disc
  field t-price like oe-ordl.t-price format ">>>,>>9.99"
  field sman as char format "x(11)"
  field po-num like oe-ordl.po-no
  field job-no like oe-ordl.job-no
  field job-no2 like oe-ordl.job-no2
  field stat like oe-ordl.prom-code format "x(4)"
  field pallets as dec format "->>>,>>>,>>9"
  field rel-date like oe-relh.rel-date
  field rel-stat as char
  FIELD po-received AS INT FORMAT "->>>,>>>,>>>"
  field inv-qty like oe-ordl.qty 
  field rel-type  as character   .                          
  
