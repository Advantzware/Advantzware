/* salrep/s-lastvs.p  batch procedure for r-lastvs.w */


/* ===== input parameters =======*/
DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

DEF VAR as-of-date AS DATE NO-UNDO.
DEF VAR begin_cust-no AS cha NO-UNDO.
DEF VAR end_cust-no AS cha NO-UNDO.
DEF VAR begin_slmn AS cha NO-UNDO.
DEF VAR end_slmn AS cha NO-UNDO.
DEF VAR begin_i-no AS cha NO-UNDO.
DEF VAR end_i-no AS cha NO-UNDO.
DEF VAR rd_print AS cha NO-UNDO.
DEF VAR rd_sort AS cha NO-UNDO.
DEF VAR tb_msf AS LOG NO-UNDO.
DEF VAR tb_round AS LOG NO-UNDO.
DEF VAR tb_exc-zero AS LOG NO-UNDO.
DEF VAR tb_fin-chg AS LOG NO-UNDO.
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

DEF STREAM excel.

v-prgmname = PROGRAM-NAME(1).
parm-var-list = "as-of-date,begin_cust-no,end_cust-no,begin_slmn,end_slmn,begin_i-no,end_i-no," +
                "rd_print,rb_sort,tb_msf,tb_round,tb_exc-zero,tb_fin-chg," +
                "lv-ornt,lines-per-page,rd-dest,lv-font-no,lv-font-name,td-show-parm".

/*==============  definition =======================*/


/*{methods/defines/hndldefs.i} 
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
*/
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

/*
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
*/

{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/ref/CustList.i NEW}

assign
 cocode = g_company
 locode = g_loc.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report.

DEF BUFFER xtt-report FOR tt-report.

/*=====================================*/

/* =========== main ========*/

RUN get-values.


RUN run-report.
RUN RUN-print.
RETURN.
/* ======= end of main ======*/

PROCEDURE run-report:
{sys/form/r-topw.f}

def var fdate     as   date extent 2.
def var tdate     as   date extent 2.
def var fcust     as   char format "x(8)" init "".
def var tcust     like fcust init "zzzzzzzz".
def var fsman     as   char format "x(3)" init "".
def var tsman     like fsman init "zzz".
def var v-ytd     as   log format "Yearly/Monthly" init yes.
def var v-sort    as   char no-undo.

def var v-date    as   date format "99/99/9999".
def var v-inc-fc  as   log init no.

def var v-sman-no as   char format "x(3)" no-undo.
def var v-amt     as   dec extent 4.
def var v-msf     as   dec extent 4.
def var v-one     as   log.
def var v-pct     as   dec column-label "%-of Last Year Sales".
def var v-diff    as   dec.

def var v-yr      like period.yr.
def var v-fac     as   int.
def var v         as   int.
DEF VAR lv-i-no   LIKE itemfg.i-no NO-UNDO.
DEF VAR ll-first  AS   LOG NO-UNDO.
DEF VAR lv-r-no   LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type   AS   CHAR NO-UNDO.
DEFINE VARIABLE lselected AS LOGICAL NO-UNDO .

ASSIGN
 str-tit2 = /*c-win:title*/ "Sales Analysis - This Year vs Last Year"
 {sys/inc/ctrtext.i str-tit2 112}

 v-date     = as-of-date
 fcust      = begin_cust-no
 tcust      = END_cust-no
 fsman      = begin_slmn
 tsman      = end_slmn
 v-ytd      = rd_print BEGINS "Y"
 v-sort     = if rd_sort begins "C" then "C" else
              if rd_sort eq "High Sales" then "S" else "M"
 v-inc-fc   = tb_fin-chg.

IF tb_round THEN DO:
  IF tb_msf THEN DO:
    form cust.cust-no   column-label "Customer"
     cust.name      column-label "Name/City/St"     format "x(26)"
     cust.sman      column-label "Sls!Rep"
     v-amt[1]       column-label "This Year Sales"  format "->>>,>>>,>>9.99"
     v-msf[1]       column-label "This Year MSF"    format "->>>,>>9.9999"
     v-amt[2]       column-label "Last Year Sales"  format "->>>,>>>,>>9.99"
     v-msf[2]       column-label "Last Year MSF"    format "->>>,>>9.9999"
     v-diff         column-label "Sale Difference"  format "->>>,>>>,>>9.99"
     v-pct          column-label "%-of LYR Sales"   format "->>>,>>>,>>9.9"

    with no-box frame detail-msf down STREAM-IO width 132.

    DO WITH FRAME detail-msf:
      ASSIGN
       v-amt[1]:FORMAT = "->>,>>>,>>>,>>9"
       v-msf[1]:FORMAT = "->>>>,>>>,>>9"
       v-amt[2]:FORMAT = "->>,>>>,>>>,>>9"
       v-msf[2]:FORMAT = "->>>>,>>>,>>9"
       v-diff:FORMAT   = "->>,>>>,>>>,>>9"
       v-pct:FORMAT    = "->,>>>,>>>,>>9".
    END.
  END.

  ELSE DO:
    form cust.cust-no   column-label "Customer"
     cust.name      column-label "Name/City/St"
     cust.sman      column-label "Sls!Rep"
     v-amt[1]       column-label "This Year Sales"  format "->,>>>,>>>,>>9.99"
     v-amt[2]       column-label "Last Year Sales"  format "->,>>>,>>>,>>9.99"
     v-diff         column-label "Difference"       format "->,>>>,>>>,>>9.99"
     v-pct          format "->>>,>>>,>>9.9"

    with no-box frame detail down STREAM-IO width 132.

    DO WITH FRAME detail:
      ASSIGN
       v-amt[1]:FORMAT = "->>>>,>>>,>>>,>>9"
       v-amt[2]:FORMAT = "->>>>,>>>,>>>,>>9"
       v-diff:FORMAT   = "->>>>,>>>,>>>,>>9"
       v-pct:FORMAT    = "->,>>>,>>>,>>9".
    END.
  END.
END.

find first period
    where period.company eq cocode
      and period.pst     le v-date
      and period.pend    ge v-date
    no-lock.

assign
 tdate[1] = v-date
 v-yr     = period.yr.

if v-ytd then
find first period
    where period.company eq cocode
      and period.yr      eq v-yr
    no-lock.
               
fdate[1] = period.pst.
  
{sys/inc/lastyear.i fdate[1] fdate[2]}
{sys/inc/lastyear.i tdate[1] tdate[2]}
     
if not v-ytd then tdate[1] = v-date.
     
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}
/* for xprint pringing logic
   PUT CONTROL "<PRINTER" v-prt-name "> " . */

if td-show-parm then run show-param.

display "" with frame r-top.

    FOR EACH tt-report:
      DELETE tt-report.
    END.

    {salrep/r-lastvs.i}

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
         parm-val-list = "".
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
         
        
          {BATCH/rptvalue.i user-print.field-name[li] "As-of-date" as-of-date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "begin_cust-no" begin_cust-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_cust-no" end_cust-no}
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_slmn" begin_slmn }
          {BATCH/rptvalue.i user-print.field-name[li] "end_slmn" end_slmn }
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_i-no" begin_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_i-no" end_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_print" rd_print }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_sort" rd_sort }
          {BATCH/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt }
          {BATCH/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name }
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_msf" tb_msf "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_round" tb_round "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_exc-zero" tb_exc-zero "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_fin-chg" tb_fin-chg "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical" "yes"} 
      

         
    /*
          begin_ord-date END_ord-date begin_slsmn END_slsmn tb_smn-no 
          rd_sqft tb_desc tb_prft tb_prepmisc lv-ornt lines-per-page rd-dest 
          lv-font-no lv-font-name td-show-parm
    */
     END CASE.
     
  END.
  /*
  MESSAGE "values: avail user?" AVAIL user-print SKIP
       begin_ord-date 
       END_ord-date
       begin_slsmn
       END_slsmn 
      tb_smn-no 
       rd_sqft 
       tb_desc 
       tb_prft 
       tb_prepmisc
       lv-ornt
       lines-per-page
       rd-dest 
       lv-font-no
       lv-font-name
       td-show-parm
  VIEW-AS ALERT-BOX. */
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

