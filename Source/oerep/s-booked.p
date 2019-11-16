/* oerep/s-booked.p  batch procedure for r-booked.w */

/* ===== input parameters =======*/
DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

DEF var begin_cust-no AS cha NO-UNDO.
DEF var END_cust-no AS cha NO-UNDO.
DEF var begin_ord-date AS DATE NO-UNDO.
DEF var END_ord-date AS DATE NO-UNDO.
DEF var begin_slsmn AS cha NO-UNDO.
DEF var END_slsmn AS cha NO-UNDO.
DEF var begin_fg-cat AS cha NO-UNDO.
DEF var END_fg-cat AS cha NO-UNDO.
DEF var rd_sqft AS cha NO-UNDO.
DEF var tb_smn-no AS LOG NO-UNDO.
DEF var tb_sortby AS LOG NO-UNDO.
DEF var tb_desc AS LOG NO-UNDO.
DEF var tb_ton AS LOG NO-UNDO.
DEF var tb_prft AS LOG NO-UNDO.
DEF var tb_prepmisc AS LOG NO-UNDO.
DEF var tb_comm AS LOG NO-UNDO.
DEF VAR tb_margin AS LOG NO-UNDO.
DEF VAR tb_exclude-transfer AS LOG NO-UNDO.
DEF VAR tb_exclude-set-comps AS LOG NO-UNDO.
DEF VAR v-code AS CHAR NO-UNDO.
DEF var lv-ornt AS cha NO-UNDO.
DEF var lines-per-page AS INT NO-UNDO.
DEF var rd-dest AS INT NO-UNDO.
DEF var lv-font-no AS INT NO-UNDO.
DEF VAR lv-font-name AS cha NO-UNDO.
DEF var td-show-parm AS LOG NO-UNDO.

/*==============  definition =======================*/

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR v-prgmname AS cha NO-UNDO.
DEF VAR v-prt-name AS cha NO-UNDO.
DEF VAR v-copies AS INT INIT 1 NO-UNDO.
def var parm-fld-list as cha no-undo.
def var parm-lbl-list as cha no-undo.
DEF VAR parm-val-list AS cha NO-UNDO.
DEF VAR parm-var-list AS cha NO-UNDO.
DEF VAR v-margin AS DEC FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".

DEF VAR tb_excel AS LOG NO-UNDO. /*not used here*/
DEF STREAM excel. /*not used here*/
DEF BUFFER b-itemfg FOR itemfg.

ASSIGN
v-prgmname = PROGRAM-NAME(1)
parm-var-list = "begin_ord-date,end_ord-date,begin_slsmn,end_slsmn,,begin_prod-cat,end_prod-cat," +
                "rd_sqft,tb_smn-no,tb_sortby,tb_desc,tb_ton,tb_prft,tb_prepmisc,lv-ornt,lines-per-page," +
                "rd-dest,lv-font-no,lv-font-name,td-show-parm,tb_margin,tb_exclude-transfer,tb_exclude-set-comps".

{custom/globdefs.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def TEMP-TABLE w-data no-undo
  field ord-no like oe-ord.ord-no
  field line   like oe-ordl.line
  field sman   as char format "x(3)"
  field item-n like itemfg.i-name column-label "Item Description"
        format "x(27)"
  field procat like itemfg.procat column-label "Prod!Code"
  field qty like oe-ordl.qty column-label "Quantity!Ordered/EA"
        format ">,>>>,>>>"
  field sqft like itemfg.t-sqft column-label "Sq Ft" format ">>,>>>.999"
  field t-sqft like itemfg.t-sqft column-label "Total!Sq Ft/M" format "->,>>>.999"
  field t-tons as dec column-label "Total!  Tons" format "->,>>>.9"
  field price like oe-ordl.price format ">>>,>>9.99<<<<"
  field revenue like oe-ordl.t-price column-label "Order!Amount"
  field misc as log
  field cost as dec
  field comm as dec label "Comm %"
  FIELD margin AS DEC
  FIELD shp-qty LIKE oe-ordl.ship-qty
  FIELD cShip-from LIKE oe-rel.spare-char-1  .
  
def TEMP-TABLE wkrecap no-undo    /* recap by product category */
  field procat like itemfg.procat column-label "Cat"
  field t-sqft like itemfg.t-sqft  extent 2 column-label "Sq Ft" format ">>,>>>.999"
  field t-tons as dec column-label "Tons" extent 2 format "->,>>>.9"
  field revenue like oe-ordl.t-price   extent 2 column-label "Amount"
  field price-per-m  as dec column-label "$/MSF" extent 2
  field price-per-t  as dec column-label "$/TON" extent 2
  field num-of-ord as int column-label "#Orders".

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.

/*=====================================*/

/* =========== main ========*/
RUN get-values.
RUN run-report.
RUN RUN-print.
RETURN.
/* ======= end of main ======*/

PROCEDURE run-report:
    /***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: Salesman Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

def var fdate as date format "99/99/9999" init 01/01/0001 no-undo.
def var tdate like fdate init 12/31/9999 no-undo.
def var v-break as log init no no-undo.
def var prt-sqft as log init yes format "SqFt/PartNo" no-undo.
def var p-m-chg as log init no no-undo.
def var prt-profit as log init yes no-undo.
def var item-dscr as log init no no-undo.
def var mdate as date no-undo.
def var lo_trandate like fdate no-undo.
def var v-per-days as int extent 2 no-undo init 0.
def var v-n-lines  as int no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" init "zzz" no-undo.
def var v-sman like w-data.sman no-undo.
def var v-exclude as log no-undo.
def var v-misc as log.
def var v-amt  like oe-ord.t-revenue.
def var v-pct as dec format "99.99".
def var v-sqft like itemfg.t-sqft  format ">,>>9.999".
def var v-tons as DEC.
def var v-qty like oe-ordl.qty format "->>>,>>9.99".
def var v-price-per-m as dec column-label "$/MSF" no-undo.
def var v-price-per-t as dec column-label "$/TON" no-undo.
def var v-msf like v-price-per-m extent 2 no-undo.
def var v-ton like v-price-per-t extent 2 no-undo.

def var v-revenue like oe-ordl.t-price format "->,>>>,>>9.99" no-undo
  column-label "Order!Amount".
def var v-profit as dec format "->>,>>9.9" no-undo
  column-label "% Profit".
def var v-sname like sman.sname.

def var v as int.
def var qm as dec.
def var mat as dec.
def var lab as dec.

def var ii like i no-undo.

find first w-data no-error.

form header "Sales Rep:"
            w-data.sman
            "-"
            v-sname
    with frame r-top1 no-box no-attr-space page-top stream-io width 180.


assign
 str-tit2 = "Orders Booked   (O-R-5)"
 {sys/inc/ctrtext.i str-tit2 112}

 fdate      = begin_ord-date
 tdate      = end_ord-date
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-break    = tb_smn-no
 prt-sqft   = rd_sqft eq "Square Ft"
 item-dscr  = tb_desc
 prt-profit = tb_prft
 p-m-chg    = tb_prepmisc.

IF tb_margin THEN
   prt-profit = NO.

/*if prt-profit then do:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  prt-profit = security-flag.
end.*/

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE w-data.
EMPTY TEMP-TABLE wkrecap.

{oerep/r-booked.i} 

end procedure.  /* run procedure */


PROCEDURE run-print:
     RUN custom/prntbat.p (list-name,INT(lv-font-no),lv-ornt,v-copies,v-prt-name).
END PROCEDURE.

PROCEDURE get-values:
    DEF VAR li AS INT NO-UNDO. 

    FIND FIRST user-print WHERE user-print.company    EQ g_company
                            AND user-print.program-id EQ v-prgmname  
                            AND user-print.batch-seq = ip-batch-seq
                            AND user-print.batch      <> ""
                            NO-LOCK    NO-ERROR .
   
   
  ASSIGN parm-fld-list = ""
         parm-lbl-list = ""
         parm-val-list = ""
         v-prt-name = IF AVAIL user-print THEN user-print.PRINTER-NAME ELSE "".
         v-copies = IF AVAIL user-print THEN int(user-print.frequency) ELSE 1.
  
  IF AVAIL user-print THEN DO li = 1 TO 50:
    /* IF user-print.field-name[li] = "" THEN LEAVE. */
     /*IF user-print.field-name[li] EQ lv-field-hdl:NAME THEN DO: */
     IF INDEX(parm-var-list,user-print.field-name[li]) > 0 THEN
     ASSIGN parm-val-list = parm-val-list + user-print.field-value[li] + ","
            parm-lbl-list = parm-lbl-list +
                            (IF user-print.field-label[li] = ? THEN user-print.field-name[li]
                                 ELSE user-print.field-label[li]) + ","
            parm-fld-list = parm-fld-list + user-print.field-name[li] + "," .

     CASE user-print.field-name[li]:
        
          {BATCH/rptvalue.i user-print.field-name[li] "begin_ord-date" begin_ord-date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "begin_cust-no" begin_cust-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_cust-no" end_cust-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_ord-date" end_ord-date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "begin_slsmn" begin_slsmn }
          {BATCH/rptvalue.i user-print.field-name[li] "end_slsmn" end_slsmn }
          {BATCH/rptvalue.i user-print.field-name[li] "begin_fg-cat" begin_fg-cat }
          {BATCH/rptvalue.i user-print.field-name[li] "end_fg-cat" end_fg-cat }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_sqft" rd_sqft }
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_smn-no" tb_smn-no "Logical" "no"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_sortby" tb_sortby "Logical" "no"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_desc" tb_desc "Logical" "no"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_ton" tb_ton "Logical" "no"}  
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_prft" tb_prft "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_prepmisc" tb_prepmisc "Logical" "no"} 
          {BATCH/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt }
          {BATCH/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name }
          {BATCH/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_margin" tb_margin "Logical" "no"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_exclude-set-comps" tb_exclude-set-comps "Logical" "no"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_exclude-transfer" tb_exclude-transfer "Logical" "no"}
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
