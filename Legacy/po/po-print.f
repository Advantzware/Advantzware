/* --------------------------------------------------- po/po-print.f 10/94 rd */
/* Purchase Order Print Program Form Statement - P/O Module                   */
/* -------------------------------------------------------------------------- */

/* Form Statements For Pre-Printed PO Forms */

def var v-over-under-hdr as char format "x(14)" init "OVER/UNDER RUN" no-undo.
def var v-phone as char no-undo.

form
     skip (5)
     po-ord.po-no at 60
     v-page-counter at 70
     skip (3)
     po-ord.po-date at 53 format "99/99/99"
     v-po-type at 63
     po-ord.po-change-date at 73 format "99/99/99"
     skip (3)
     vend.name at 13
     v-sname at 50
     vend.add1 at 13
     v-saddr [1] at 50
     vend.add2 at 13
     v-saddr [2] at 50
     vend.city at 13
     vend.state at 31
     vend.zip at 35
     v-scity at 50
     v-sstate at 67
     v-szip at 71
     skip (4)
     po-ord.buyer format "x(8)" /* CTS */ at 2
     /* CTS */ po-ord.contact format "x(12)" /* CTS end */
     terms.dscr at 24 format "x(15)"
     po-ord.acknowledge at 40
     po-ord.fob-code at 50
     carrier.dscr at 64 format "x(15)"
     po-ord.frt-pay at 80
     skip (2)
     with frame po-head no-box no-labels no-underline stream-io.

form
     po-ordl.line at 2
     po-ordl.ord-qty at 4 format ">>>,>>9.9<<"
     po-ordl.pr-qty-uom at 14
     po-ordl.i-no at 18
/*
     po-ordl.vend-i-no at 33
*/
     v-ino-job at 33
     po-ordl.cost at 51 format ">>>>,>>9.999"
     po-ordl.pr-uom
     po-ordl.t-cost at 68
     po-ordl.i-name at 18
     po-ordl.due-date at 52 format "99/99/99"
     v-change-dscr to 80
     skip(1)
     with frame po-line no-box no-labels no-underline stream-io down.

form
     po-ordl.line at 2
     po-ordl.ord-qty at 4 format ">>,>>>,>>9"
     po-ordl.pr-qty-uom at 14
     po-ordl.i-no at 18
     po-ordl.vend-i-no at 33
     v-ino-job at 33
     po-ordl.cost at 51 format ">>>>,>>9.999"
     po-ordl.pr-uom
     po-ordl.t-cost at 68
     po-ordl.i-name at 18
     po-ordl.due-date at 52 format "99/99/99"
     v-change-dscr to 80
     skip(1)
     with frame po-line-sonoco no-box no-labels no-underline stream-io down.

form
     /* po-ord.t-freight at 50 - RLL Need a form to test this. */
     po-ord.t-cost at 62
     skip (2)
     po-ord.spec-i[1] format "x(80)" skip
     po-ord.spec-i[2] format "x(80)" skip
     po-ord.spec-i[3] format "x(80)" skip
     po-ord.spec-i[4] format "x(80)" skip
     with frame po-totals no-box no-labels no-underline stream-io.

/* Form Statements For Blank Paper PO Forms */

form
     /*skip (1)   7/01/02 YSK  commented out for fax number */
     "P  U  R  C  H  A  S  E      O  R  D  E  R" at 20
     skip
     space(22) v-change-ord
     skip(1)
/*
     skip (2)
*/
     "FROM" at 7
     company.name at 13
     "P/O NUMBER:" at 55
     po-ord.po-no at 67
     company.addr [1] at 13
     "PAGE:" at 61
     v-page-counter at 67
     company.addr [2] at 13
     "PO DATE:" at 58
     po-ord.po-date at 67 format "99/99/99"
     company.city at 13
     company.state at 31
     company.zip at 35
     "PO TYPE:" at 58
     v-po-type at 67
     "CHANGE DATE:" at 54
     po-ord.po-change-date at 67 format "99/99/99"
     skip (2)
     "ORDERED" at 4
     vend.name at 13
     "SHIP" at 44
     v-sname at 50
     "FROM" at 7
     vend.add1 at 13
     "TO" at 46
     v-saddr [1] at 50
     vend.add2 at 13
     v-saddr [2] at 50
     vend.city at 13
     vend.state at 31
     vend.zip at 35
     v-scity at 50
     v-sstate at 67
     v-szip at 71
     v-phone form "x(30)" at 13
     v-fax form "x(30)" at 13 
     skip (1)  
     /****
     /* CTS */
     "OVER-RUN / UNDER-RUN:"
     po-ord.over-pct po-ord.under-pct skip
     /* CTS end */ 
     ****/
     /* CTS */
     v-dash-line [1] at 1
     "BUYER   " at 2
     "CONTACT"
     "TERMS" at 24
     "ACK" at 40
     "FOB CODE" at 45
     "SHIP VIA" at 55
     "FREIGHT" at 74
     skip (1)
     po-ord.buyer format "x(8)" at 2
     /* CTS */ po-ord.contact format "x(12)" /* CTS end */
     terms.dscr at 24 format "x(15)"
     po-ord.acknowledge at 40
     po-ord.fob-code at 45
     carrier.dscr at 55 format "x(15)"
     v-freight-dscr at 74
     v-dash-line [2] at 1
     skip (1)
     "LINE" at 2
     "QUANTITY" at 7
     "UOM" at 16
     "OUR ITEM/" at 21
/*
     "VEND ITEM" at 38
     "JOB #" at 38
*/
     v-hdr at 38
     "COST UOM/" at 54
     "EXT COST" at 68 SKIP
     v-over-under-hdr
     "DESCRIPTION" at 21
     "REQ DATE" at 54
     "CHANGE MADE" at 68
     "CUSTOMER NUMBER" AT 1 
     v-dash-line [3] at 1
     with frame po-head-2 no-box no-labels no-underline stream-io.

form
     /* skip (1) 07/01/02 YSK  commented out for fax number */
     "P  U  R  C  H  A  S  E      O  R  D  E  R" at 20
     skip
     space(22) v-change-ord
     skip(1)
/*
     skip (2)
*/
     /* "FROM" at 7 */
     /* company.name at 13 */
     "P/O NUMBER:" at 55
     po-ord.po-no at 67
     /* company.addr [1] at 13 */
     "PAGE:" at 61
     v-page-counter at 67
     /* company.addr [2] at 13 */
     "PO DATE:" at 58
     po-ord.po-date at 67 format "99/99/99"
     /* company.city at 13
     company.state at 31
     company.zip at 35 */
     "PO TYPE:" at 58
     v-po-type at 67
     "CHANGE DATE:" at 54
     po-ord.po-change-date at 67 format "99/99/99"
     skip (2)
     "ORDERED" at 4
     vend.name at 13
     "SHIP" at 44
     v-sname at 50
     "FROM" at 7
     vend.add1 at 13
     "TO" at 46
     v-saddr [1] at 50
     vend.add2 at 13
     v-saddr [2] at 50
     vend.city at 13
     vend.state at 31
     vend.zip at 35
     v-scity at 50
     v-sstate at 67
     v-szip at 71
     skip (2)  /****
     /* CTS */
     "OVER-RUN / UNDER-RUN:"
     po-ord.over-pct po-ord.under-pct skip
     /* CTS end */ ****/ /* CTS */
     v-dash-line [1] at 1
     "BUYER   " at 2
     "CONTACT"
     "TERMS" at 24
     "ACK" at 40
     "FOB CODE" at 45
     "SHIP VIA" at 55
     "FREIGHT" at 74
     skip (1)
     po-ord.buyer format "x(8)" at 2
     /* CTS */ po-ord.contact format "x(12)" /* CTS end */
     terms.dscr at 24 format "x(15)"
     po-ord.acknowledge at 40
     po-ord.fob-code at 45
     carrier.dscr at 55 format "x(15)"
     v-freight-dscr at 74
     v-dash-line [2] at 1
     skip (1)
     "LINE" at 2
     "QUANTITY" at 7
     "UOM" at 16
     "OUR ITEM/" at 21
/*
     "VEND ITEM" at 38
     "JOB #" at 38
*/
     v-hdr at 38
     "COST UOM/" at 54
     "EXT COST" at 68 SKIP
     v-over-under-hdr
     "DESCRIPTION" at 21
     "REQ DATE" at 54
     "CHANGE MADE" at 68
     v-dash-line [3] at 1
     with frame po-head-3 no-box no-labels no-underline stream-io.

form
     po-ordl.line at 2
     po-ordl.ord-qty at 4 format ">>>,>>9.9<<"
     po-ordl.pr-qty-uom at 16
     po-ordl.i-no at 21
/*
     po-ordl.vend-i-no at 38
*/
     v-ino-job at 38
     po-ordl.cost at 54 format ">>>>,>>9.999"
     po-ordl.pr-uom
     po-ordl.t-cost at 71 SKIP
     /* CTS */
     po-ordl.over-pct
     po-ordl.under-pct
     /* CTS  end */
     po-ordl.i-name at 21
     po-ordl.due-date at 54 format "99/99/99"
     v-change-dscr to 80
     skip
     with frame po-line-2 no-box no-labels no-underline stream-io down.

form
     po-ordl.line at 2
     po-ordl.ord-qty at 4 format ">>,>>>,>>9"
     po-ordl.pr-qty-uom at 16
     po-ordl.i-no at 21
     v-ino-job at 38
     po-ordl.cost at 54 format ">>>>,>>9.999"
     po-ordl.pr-uom
     po-ordl.t-cost at 71 SKIP
     po-ordl.over-pct
     po-ordl.under-pct
     po-ordl.i-name at 21
     po-ordl.due-date at 54 format "99/99/99"
     v-change-dscr to 80
     skip
     with frame po-line-sonoco-2 no-box no-labels no-underline stream-io down.

form
     v-dash-line [1] at 1
     "FREIGHT:" at 20
     po-ord.t-freight
     "TOTAL EXTENDED PRICE:" at 44
     po-ord.t-cost at 66 skip
     "COMMENTS:"
     po-ord.spec-i[1] format "x(80)" skip
     po-ord.spec-i[2] format "x(80)" skip
     po-ord.spec-i[3] format "x(80)" skip
     po-ord.spec-i[4] format "x(80)" skip
     with frame po-totals-2 no-box no-labels no-underline stream-io.

/*
form
     skip (1)
     "P  U  R  C  H  A  S  E      O  R  D  E  R" at 20
     skip (2)
     "FROM" at 7
     company.name at 13
     "P/O NUMBER:" at 55
     po-ord.po-no at 67
     company.addr [1] at 13
     "PAGE:" at 61
     v-page-counter at 67
     company.addr [2] at 13
     "PO DATE:" at 58
     po-ord.po-date at 67 format "99/99/99"
     company.city at 13
     company.state at 31
     company.zip at 35
     "PO TYPE:" at 58
     v-po-type at 67
     "CHANGE DATE:" at 54
     po-ord.po-change-date at 67 format "99/99/99"
     skip (2)
     "ORDERED" at 4
     vend.name at 13
     "SHIP" at 44
     v-sname at 50
     "FROM" at 7
     vend.add1 at 13
     "TO" at 46
     v-saddr [1] at 50
     vend.add2 at 13
     v-saddr [2] at 50
     vend.city at 13
     vend.state at 31
     vend.zip at 35
     v-scity at 50
     v-sstate at 67
     v-szip at 71
     skip (2)  /****
     /* CTS */
     "OVER-RUN / UNDER-RUN:"
     po-ord.over-pct po-ord.under-pct skip
     /* CTS end */ ****/ /* CTS */
     v-dash-line [1] at 1
     "BUYER   " at 2
     "CONTACT"
     "TERMS" at 24
     "ACK" at 40
     "FOB CODE" at 45
     "SHIP VIA" at 55
     "FREIGHT" at 74
     skip (1)
     po-ord.buyer format "x(8)" at 2
     /* CTS */ po-ord.contact format "x(12)" /* CTS end */
     terms.dscr at 24 format "x(15)"
     po-ord.acknowledge at 40
     po-ord.fob-code at 45
     carrier.dscr at 55 format "x(15)"
     v-freight-dscr at 74
     v-dash-line [2] at 1
     skip (1)
     "LINE" at 2
     "QUANTITY" at 7
     "UOM" at 16
     "OUR ITEM/" at 21
     "VEND ITEM" at 38
     "COST UOM/" at 54
     "EXT COST" at 68 SKIP
     v-over-under-hdr
     "DESCRIPTION" at 21
     "REQ DATE" at 54
     "CHANGE MADE" at 68 skip
     "WIDTH" at 21
     "LENGTH" at 31
     "FLUTE" at 41
     "TEST" at 48 skip
     "SCORE" at 21
     v-dash-line [3] at 1
     with frame po-head-2cw no-box no-labels no-underline stream-io.

form
     skip (1)
     "P  U  R  C  H  A  S  E      O  R  D  E  R" at 20
     skip (2)
     /* "FROM" at 7 */
     /* company.name at 13 */
     "P/O NUMBER:" at 55
     po-ord.po-no at 67
     /* company.addr [1] at 13 */
     "PAGE:" at 61
     v-page-counter at 67
     /* company.addr [2] at 13 */
     "PO DATE:" at 58
     po-ord.po-date at 67 format "99/99/99"
     /* company.city at 13
     company.state at 31
     company.zip at 35 */
     "PO TYPE:" at 58
     v-po-type at 67
     "CHANGE DATE:" at 54
     po-ord.po-change-date at 67 format "99/99/99"
     skip (2)
     "ORDERED" at 4
     vend.name at 13
     "SHIP" at 44
     v-sname at 50
     "FROM" at 7
     vend.add1 at 13
     "TO" at 46
     v-saddr [1] at 50
     vend.add2 at 13
     v-saddr [2] at 50
     vend.city at 13
     vend.state at 31
     vend.zip at 35
     v-scity at 50
     v-sstate at 67
     v-szip at 71
     skip (2)  /****
     /* CTS */
     "OVER-RUN / UNDER-RUN:"
     po-ord.over-pct po-ord.under-pct skip
     /* CTS end */ ****/ /* CTS */
     v-dash-line [1] at 1
     "BUYER   " at 2
     "CONTACT"
     "TERMS" at 24
     "ACK" at 40
     "FOB CODE" at 45
     "SHIP VIA" at 55
     "FREIGHT" at 74
     skip (1)
     po-ord.buyer format "x(8)" at 2
     /* CTS */ po-ord.contact format "x(12)" /* CTS end */
     terms.dscr at 24 format "x(15)"
     po-ord.acknowledge at 40
     po-ord.fob-code at 45
     carrier.dscr at 55 format "x(15)"
     v-freight-dscr at 74
     v-dash-line [2] at 1
     skip (1)
     "LINE" at 2
     "QUANTITY" at 7
     "UOM" at 16
     "OUR ITEM/" at 21
     "VEND ITEM" at 38
     "COST UOM/" at 54
     "EXT COST" at 68 SKIP
     v-over-under-hdr
     "DESCRIPTION" at 21
     "REQ DATE" at 54
     "CHANGE MADE" at 68 skip
     "WIDTH" at 21
     "LENGTH" at 31
     "FLUTE" at 41
     "TEST" at 48 skip
     "SCORE" at 21
     v-dash-line [3] at 1
     with frame po-head-3cw no-box no-labels no-underline stream-io.
*/

/* End ---------------------------------- Copr. 1994  Advanced Software, Inc. */
