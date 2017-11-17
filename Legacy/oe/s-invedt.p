/* oe/s-invedt.p  batch procedure for r-invedt.w */


/* ===== input parameters =======*/
DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

DEF VAR v-prg-name AS CHAR INIT "r-invedt.r" NO-UNDO.
DEF VAR begin_date AS DATE NO-UNDO.
DEF VAR end_date AS DATE NO-UNDO.
DEF VAR begin_cust-no AS cha NO-UNDO.
DEF VAR end_cust-no AS cha NO-UNDO.
DEF VAR begin_slsmn AS cha NO-UNDO.
DEF VAR end_slsmn AS cha NO-UNDO.
DEF VAR begin_i-no AS cha NO-UNDO.
DEF VAR end_i-no AS cha NO-UNDO.
DEF VAR rd_print AS cha NO-UNDO.
DEF VAR rd_sort AS cha NO-UNDO.
DEF VAR tb_detailed AS LOG NO-UNDO.
DEF VAR tb_cost AS LOG NO-UNDO.
DEF VAR tb_printed AS LOG NO-UNDO.
DEF VAR tb_unprinted AS LOG NO-UNDO.
DEF var lv-ornt AS cha NO-UNDO.
DEF var lines-per-page AS INT NO-UNDO.
DEF var rd-dest AS INT NO-UNDO.
DEF var lv-font-no AS INT NO-UNDO.
DEF VAR lv-font-name AS cha NO-UNDO.
DEF var td-show-parm AS LOG NO-UNDO.
DEF VAR tb_excel AS LOG NO-UNDO.

DEF VAR v-prgmname AS cha NO-UNDO.
DEF VAR v-prt-name AS cha NO-UNDO.
DEF VAR v-copies AS INT INIT 1 NO-UNDO.
def var parm-fld-list as cha no-undo.
def var parm-lbl-list as cha no-undo.
DEF VAR parm-val-list AS cha NO-UNDO.
DEF VAR parm-var-list AS cha NO-UNDO.
DEF VAR v-sman-found AS LOG NO-UNDO.

DEF STREAM excel.

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared}

assign
   v-prgmname = "oe\s-invedt.r"
   parm-var-list = "tb_detailed,tb_printed,tb_unprinted,tb_cost,rd_sort,begin_date,end_date," +
                   "begin_slsmn,end_slsmn," +
                   "lv-ornt,lines-per-page,rd-dest,lv-font-no,lv-font-name,td-show-parm"
   cocode = g_company
   locode = g_loc.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report.

DEF BUFFER xtt-report FOR tt-report.

def TEMP-TABLE work-rel NO-UNDO
  field company like oe-relh.company
  field loc like oe-rell.loc
  field r-no like oe-relh.r-no
  field bol-no like oe-bolh.bol-no
  field carrier like oe-relh.carrier
  field cust-no like oe-relh.cust-no
  field ord-no like oe-relh.ord-no
  field po-no like oe-relh.po-no
  field rel-date AS CHAR FORMAT "99/99/99"
  field ship-id like oe-relh.ship-id
  field ship-i like oe-relh.ship-i
  field i-no like oe-rell.i-no
  field line like oe-rell.line
  field qty like oe-rell.qty
  field tot-qty like oe-rell.qty
  field posted like oe-rell.posted
  field printed like oe-relh.printed
  field ship-addr as char format "x(20)"
  field ship-city as char format "x(10)"
  field ship-state as char format "x(2)"
  field ship-zip as char format "x(10)"
  field completed as character format "x(1)".

DEF TEMP-TABLE work-rel-copy NO-UNDO LIKE work-rel.

/* =========== main ========*/
RUN get-values.
RUN run-report.
RUN RUN-print.
RETURN.
/* ======= end of main ======*/

PROCEDURE run-report:

{sys/form/r-topw2.f}

def var v-detail as log format "Detail/Summary" NO-UNDO.
def var v-sort   as log format "Customer/BOL"   init YES NO-UNDO.

def var v-ext-price like inv-line.t-price NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-cases as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as INT NO-UNDO.
def var v-tax-rate as dec format ">,>>9.99<<<" NO-UNDO.
def var v-fr-tax as log NO-UNDO.
def var v-postable as log NO-UNDO.
def var v-tot-cost like inv-head.t-inv-cost NO-UNDO.     /* total cost invoiced */
def var v-tot-weight like inv-head.t-inv-weight NO-UNDO.  /* total weight shipped */
def var v-line-price like inv-line.price NO-UNDO.
def var v-line-cost like inv-line.t-price NO-UNDO.
def var v-line-freight like inv-line.t-freight NO-UNDO.
def var v-set-qty as INT NO-UNDO.
def var v-part-qty as dec format "999.9999" NO-UNDO.
def var v-bol-cases like oe-boll.cases NO-UNDO.
def var v-line-tot  like inv-line.t-price NO-UNDO.
def var v-misc-tot like inv-misc.amt NO-UNDO.
def var v-tmp as DEC NO-UNDO.
DEF VAR ld-margin AS DEC NO-UNDO.
DEF VAR ld-total-p AS DEC NO-UNDO.
DEF VAR ld-total-c AS DEC NO-UNDO.
DEF VAR lv-dash AS CHAR FORMAT "x" INIT "-" NO-UNDO.

DEFINE VARIABLE dfreight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE cfreightCode AS CHARACTER NO-UNDO.

/* gdm - 10130810 */
DEF VAR v_misc-amt  AS CHAR NO-UNDO.

def buffer xinv-line for inv-line.

form header
  " Customer"
      "Weight" to 47 "Pallets" to 58 "Cases" to 65 "Freight Terms" TO 80 "Freight" to 96
      "Tax" to 106 "Misc" to 120 "Items" to 135 " Total" to 160 SKIP
  fill("=",160) format "x(160)"
  with FRAME r-top.

form
  inv-head.cust-no lv-dash
  inv-head.cust-name format "x(25)"
  inv-head.t-inv-weight v-tot-pallets v-tot-cas format "->>>>9"
  cfreightCode TO 80 FORMAT "x(10)"
  dfreight TO 96 format "->,>>9.99"
  inv-head.t-inv-tax TO 106 format "->>,>>9.99"
  v-misc-tot to 120 format "->>>,>>9.99"
  v-line-tot to 135
  inv-head.t-inv-rev format "->>,>>>,>>9.999999" to 160
with down STREAM-IO width 180 no-labels no-box no-underline frame ord.

form
  inv-head.cust-no lv-dash
  inv-head.cust-name format "x(25)"
  inv-head.t-inv-weight v-tot-pallets v-tot-cas format "->>>>9"
  cfreightCode TO 80 FORMAT "x(10)"
  dfreight TO 96 format "->,>>9.99"
  inv-head.t-inv-tax TO 106 format "->>,>>9.99"
  v-misc-tot to 120 format "->>>,>>9.99"
  v-line-tot to 135
  ld-total-c format "->>,>>>,>>9.99" to 160
  inv-head.t-inv-rev format "->>,>>>,>>9.999999"
with down STREAM-IO width 180 no-labels no-box no-underline frame ord-c.

form
  inv-line.ord-no at 5 label "Order#"
  oe-ordl.po-no label "Order PO Number"
  inv-line.i-no label "Item"
  inv-line.i-name format "x(20)" label "Description"
  inv-line.qty format "->>,>>>,>>9" label "Order"
  inv-line.inv-qty format "->>,>>>,>>9" column-label "Quantities!Invoiced "
  inv-line.ship-qty format "->>,>>>,>>9" label "Shipped"
  inv-line.price format "->>>,>>9.999999" label "Price"
  inv-line.pr-uom label "UOM"
  inv-line.t-price format "->>,>>>,>>9.999999" column-label "Extended! Price" TO 140 skip
  with down no-box STREAM-IO width 140 frame ordl.

form
  inv-line.ord-no at 5 label "Order#"
  oe-ordl.po-no label "Order PO Number"
  inv-line.i-no label "Item"
  inv-line.i-name format "x(20)" label "Description"
  inv-line.qty format "->>,>>>,>>9" label "Order"
  inv-line.inv-qty format "->>,>>>,>>9" column-label "Quantities!Invoiced "
  inv-line.ship-qty format "->>,>>>,>>9" label "Shipped"
  inv-line.price format "->>>,>>9.999999" label "Price"
  inv-line.pr-uom label "UOM"
  v-line-cost format "->>,>>>,>>9.99" column-label "Extended!  Cost" TO 132 
  inv-line.t-price format "->>,>>>,>>9.999999" column-label "Extended! Price"
  ld-margin format "->>,>>9.99" column-label "!Margin%"
  skip
  with down no-box STREAM-IO width 180 frame ordl-c.

form
  inv-misc.charge at 10 label "Charge"
  inv-misc.dscr label "Description"
  inv-misc.amt format "->>,>>>,>>9.999999" to 132 label "Price"
  skip
  with down STREAM-IO width 132 no-box frame ordm.

form
  inv-misc.charge at 10 label "Charge"
  inv-misc.dscr label "Description"
  inv-misc.cost format "->>,>>>,>>9.99" to 132 label "Cost"
  inv-misc.amt format "->>,>>>,>>9.999999" label "Price"
  ld-margin format "->>,>>9.99" column-label "!Margin%"
  skip
  with down STREAM-IO width 180 no-box frame ordm-c.

form
  work-rel.i-no at 10 column-label "RELEASE!Items"
  work-rel.po-no label "PO Number"
  work-rel.loc label "Location"
  work-rel.rel-date label "Date"
  work-rel.bol-no label "BOL#"
  work-rel.completed column-label "P/C"
  work-rel.r-no   label "REL#"
  work-rel.carrier label "Carrier"
  work-rel.ship-id label "Ship To"
  work-rel.qty   label "Quantity" skip
  with down no-box STREAM-IO width 132 frame rel.

    
find first oe-ctrl where oe-ctrl.company EQ cocode no-lock no-error.

assign
 str-tit2 = "Invoice Edit Listing"
 {sys/inc/ctrtext.i str-tit2 112}
    
 v-fr-tax = oe-ctrl.f-tax  /** if fREIGHT IS TAXABLE **/

 v-detail = tb_detailed
 v-sort   = rd_sort BEGINS "Cust".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

{oe/r-invedt.i}

end procedure.  /* run procedure */


PROCEDURE run-print:

   IF rd-dest NE 5 THEN
      RUN custom/prntbat.p (list-name,INT(lv-font-no),lv-ornt,v-copies,v-prt-name).
   ELSE
   DO:                                     
      {custom/asimailr3.i &TYPE = "SalesRep1" /*1 is silent mode*/
                          &group-title= v-prg-name
                          &begin_cust= begin_slsmn
                          &END_cust= end_slsmn
                          &mail-subject= "Invoice Edit Listing"
                          &mail-body= "Invoice Edit Listing"
                          &mail-file=list-name }
   END.


END PROCEDURE.

PROCEDURE get-values:
  DEF VAR li AS INT NO-UNDO. 

  FIND FIRST user-print WHERE
       user-print.company EQ g_company AND
       user-print.program-id EQ v-prgmname AND
       user-print.batch-seq = ip-batch-seq AND
       user-print.batch <> ""
       NO-LOCK NO-ERROR.
   
  ASSIGN v-prt-name = IF AVAIL user-print THEN user-print.PRINTER-NAME ELSE "".
         v-copies = IF AVAIL user-print THEN int(user-print.frequency) ELSE 1.

  IF AVAIL user-print THEN DO li = 1 TO 50:
    
     IF INDEX(parm-var-list,user-print.field-name[li]) > 0 THEN
     ASSIGN parm-val-list = parm-val-list + user-print.field-value[li] + ","
            parm-lbl-list = parm-lbl-list +
                            (IF user-print.field-label[li] = ? THEN user-print.field-name[li]
                                 ELSE user-print.field-label[li]) + ","
            parm-fld-list = parm-fld-list + user-print.field-name[li] + ",".

     CASE user-print.field-name[li]:
        
          {BATCH/rptvalue.i user-print.field-name[li] "rd_sort" rd_sort }
          {BATCH/rptvalue.i user-print.field-name[li] "begin_date" begin_date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "end_date" end_date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "begin_slsmn" begin_slsmn }
          {BATCH/rptvalue.i user-print.field-name[li] "end_slsmn" end_slsmn }
          
          {BATCH/rptvalue.i user-print.field-name[li] "rd-dest" rd-dest "INT" }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_print" rd_print }
          {BATCH/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt }
          {BATCH/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name }
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_detailed" tb_detailed "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_printed" tb_printed "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_unprinted" tb_unprinted "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_cost" tb_cost "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical" "yes"}
    
     END CASE.
  END.
END.

PROCEDURE show-param:
  DEF VAR lv-label AS cha NO-UNDO.

  put space(28)
     "< Selection Parameters >"
     skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
         
   if (entry(i,parm-fld-list) ne "" or
      entry(i,parm-lbl-list) ne ""  ) AND
       INDEX(parm-var-list,ENTRY(i,parm-fld-list)) > 0
        then do:

     lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                trim(entry(i,parm-lbl-list)) + ":".

     put lv-label format "x(35)" at 5
         space(1)
         trim(entry(i,parm-val-list)) format "x(40)"
         skip.              
   end.
 end.

 put fill("-",80) format "x(80)" skip.

END PROCEDURE.

