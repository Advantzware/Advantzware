/* oerep/s-ordopn.p  batch procedure for r-ordopn.w */

/* ===== input parameters =======*/
DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

DEF VAR begin_cust-no AS cha NO-UNDO.
DEF VAR end_cust-no AS cha NO-UNDO.
DEF VAR begin_ord-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR END_ord-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR begin_po-no AS cha NO-UNDO.
DEF VAR end_po-no AS cha NO-UNDO.
DEF VAR begin_job-no AS cha NO-UNDO.
DEF VAR end_job-no AS cha NO-UNDO.
DEF VAR begin_job-no2 AS cha NO-UNDO.
DEF VAR end_job-no2 AS cha NO-UNDO.
DEF VAR begin_i-no AS cha NO-UNDO.
DEF VAR end_i-no AS cha NO-UNDO.
DEF VAR begin_cad-no AS cha NO-UNDO.
DEF VAR end_cad-no AS cha NO-UNDO.
DEF VAR begin_due-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR end_due-date AS DATE  FORM "99/99/9999" NO-UNDO.
DEF VAR begin_userid AS cha NO-UNDO.
DEF VAR end_userid AS cha NO-UNDO.
DEF VAR begin_slsmn AS cha NO-UNDO.
DEF VAR end_slsmn AS cha NO-UNDO.
DEF VAR rd_sort AS cha NO-UNDO.
DEF VAR rd_sort-1 AS cha NO-UNDO.
DEF VAR rd_jstat AS cha NO-UNDO.
DEF VAR rd_ostat AS cha NO-UNDO.
DEF VAR rd_due-date AS cha NO-UNDO.
DEF VAR rd_wip-qty AS cha NO-UNDO.
DEF VAR tb_0-bal AS LOG NO-UNDO.
DEF VAR tb_0-wip AS LOG NO-UNDO.
DEF VAR tb_0-avl AS LOG NO-UNDO.
DEF VAR tb_under AS LOG NO-UNDO.
DEF VAR tb_job-qoh AS LOG NO-UNDO.
DEF VAR tb_job-qty AS LOG NO-UNDO.
DEF VAR tb_due-date AS LOG NO-UNDO.
DEF VAR tb_excel AS LOG NO-UNDO.
DEF VAR tb_est-count AS LOG NO-UNDO.
DEF VAR tb_est-pallets AS LOG NO-UNDO.
DEF VAR tb_sch AS LOG NO-UNDO.
DEF VAR v-excel-file AS cha NO-UNDO.

DEF var lv-ornt AS cha NO-UNDO.
DEF var lines-per-page AS INT NO-UNDO.
DEF var rd-dest AS INT NO-UNDO.
DEF var lv-font-no AS INT NO-UNDO.
DEF VAR lv-font-name AS cha NO-UNDO.
DEF var td-show-parm AS LOG NO-UNDO.

DEF VAR v-prgmname AS cha NO-UNDO.
DEF VAR v-prt-name AS cha NO-UNDO.
DEF VAR v-copies AS INT INIT 1 NO-UNDO.
def var parm-fld-list as cha no-undo.
def var parm-lbl-list as cha no-undo.
DEF VAR parm-val-list AS cha NO-UNDO.
DEF VAR parm-var-list AS cha NO-UNDO.

ASSIGN
v-prgmname = PROGRAM-NAME(1)
parm-var-list = "begin_cust-no,end_cust-no,begin_ord-date,end_ord-date,begin_po-no,end_po-no," +
                "begin_job-no,end_job-no,begin_job-no2,end_job-no2,begin_i-no,end_i-no,begin_cad-no,end_cad-no," +
                "begin_due-date,end_due-date,begin_userid,end_userid,begin_slsmn,end_slsmn" +
                "rb_sort,rd_jstat,rd_ostat,rd_due-date,rd_wip-qty,rd_sort-1,tb_due-date,tb_0-bal,tb_0-wip,tb_0-avl,tb_under,tb_job-qty," +
                "tb_excel,tb_est-count,tb_est-pallets,v-excel-file,lv-ornt,lines-per-page,rd-dest,lv-font-no,lv-font-name,td-show-parm,tb_sch".

/*==============  definition =======================*/

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

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-report-title AS CHAR NO-UNDO.

DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.
DEF VAR v-q-onh LIKE itemfg.q-onh NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report
    FIELD q-onh    LIKE itemfg.q-onh
    FIELD q-shp    LIKE itemfg.q-onh
    FIELD q-rel    LIKE itemfg.q-onh
    FIELD q-wip    LIKE itemfg.q-onh
    FIELD q-avl    LIKE itemfg.q-onh
    FIELD po-no    LIKE oe-ord.po-no
    FIELD inv      AS   LOG
    FIELD inv-no   LIKE ar-invl.inv-no
    FIELD cad-no   LIKE itemfg.cad-no
    FIELD row-id   AS ROWID
    FIELD due-date LIKE oe-ordl.req-date
    FIELD unit-count LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    INDEX row-id row-id.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF VAR lv-pdf-file AS cha NO-UNDO.
{custom/xprint.i}

DEF STREAM st-excel.

/*=====================================*/

/* ************************  Function Implementations ***************** */


FUNCTION ReplaceCommas RETURNS CHARACTER
  ( ipcString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   

  RETURN REPLACE(ipcString,","," ").
  
END FUNCTION.

/* =========== main ========*/

RUN get-values.

RUN run-report.
RUN RUN-print.
RETURN.
/* ======= end of main ======*/

PROCEDURE run-report:
    /* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
    /* Order Backlog Summary / Detail Report                                      */
    /* -------------------------------------------------------------------------- */

    {sys/form/r-top3w.f}

    DEF BUFFER b-tt-report FOR tt-report.
    DEF BUFFER b-oe-rell FOR oe-rell.

    def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
    def var v-date  like ar-inv.inv-date format "99/99/9999"
                                         extent 2 init [today, 12/31/9999].
    def var v-po    like oe-relh.po-no   extent 2 init ["","zzzzzzzzzzzzzzz"].
    def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzz"].
    def var v-job2  like oe-ord.job-no2  format "99" extent 2 init [0,99].
    def var v-item  like oe-ordl.i-no    extent 2 init ["","zzzzzzzzzzzzzzz"].
    def var v-inc   as   log             format "Yes/No" init yes.
    def var v-stat  as   char format "!" init "A".
    def var v-ostat as   char format "!" init "A".
    def var v-jobq  as   log             format "Yes/No" init no.

    def var v-dat     as   date format "99/99/99".
    def var v-bal-qty as   int format ">>>,>>>,>>9".
    def var v-inv-amt as   int format ">>>>,>>9.99".
    def var v-cust-no like cust.cust-no.
    def var v-name    like cust.name.
    def var v-sman    like sman.sman.
    def var v-sname   like sman.sname.
    def var v-field1  as   char format "x(36)".
    def var v-ord-no  as   char.
    def var v-q-onh   like itemfg.q-onh NO-UNDO.
    def var v-q-shp   like v-q-onh NO-UNDO.
    def var v-q-rel   like v-q-onh NO-UNDO.
    def var v-q-wip   like v-q-onh NO-UNDO.
    def var v-q-avl   like v-q-onh NO-UNDO.
    DEF VAR lv-slsmn  AS CHAR NO-UNDO.

    def var v-time as int.
    v-time = time.
    DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
    DEF VAR lv-job-qty AS DEC NO-UNDO.
    DEF VAR lv-rec-qty AS DEC NO-UNDO.

    FORMAT HEADER
       SKIP(1)
       "Sales Rep:"
       lv-slsmn
    WITH FRAME r-top2 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.

    FORMAT HEADER
           SKIP(1)
           "Cust#   "
           "Due Date"
           "Cust Part#     "
           "Item Description              "
           "FG Item #      "
           "Order#"
           "CAD#         "
           "PO#       "
           " Order Qty"
           "Qty OnHand"
           "Qty Shippd"
           "Qty ActRel"
           "   Qty WIP"
           " Qty Avail"
           SKIP
           "--------"
           "--------"
           "---------------"
           "------------------------------"
           "---------------"
           "------"
           "-------------"
           "----------"
           "----------"
           "----------"
           "----------"
           "----------"
           "----------"
           "----------"

        WITH FRAME r-top3 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.
    
    ASSIGN 
     str-tit2 = "Open Order Report"
     str-tit3 = (IF rd_sort-1 EQ "Customer#" THEN ""
                 ELSE "By " + TRIM(rd_sort-1) + " ") +
                 "By Customer# " +
                 "By " + TRIM(rd_sort)
     {sys/inc/ctrtext.i str-tit2 112}
     {sys/inc/ctrtext.i str-tit3 132}

     v-cust[1]  = begin_cust-no
     v-cust[2]  = end_cust-no
     v-date[1]  = begin_ord-date
     v-date[2]  = end_ord-date
     v-po[1]    = begin_po-no
     v-po[2]    = end_po-no
     v-job[1]   = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"-99")
     v-job[2]   = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"-99")
     v-item[1]  = begin_i-no
     v-item[2]  = end_i-no
     v-sort     = substr(rd_sort,1,2)
     v-inc      = tb_0-bal
     v-stat     = substr(rd_jstat,1,1)
     v-ostat    = substr(rd_ostat,1,1)
     v-jobq     = tb_job-qty.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(0)}

    IF rd-dest = 5 THEN DO:
       IF lv-ornt = "L" THEN PUT "<OLANDSCAPE><PREVIEW>".                   /*<p7><CPI20>*/ 
       ELSE PUT "<PREVIEW>".
       PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7><ADJUST=LPI>" FORM "x(165)" SKIP.
    END. 
    OUTPUT CLOSE.


    IF tb_excel THEN DO:
       OUTPUT STREAM st-excel TO VALUE(v-excel-file).
       PUT STREAM st-excel
           "Cust#   ," 
           "Due Date,"
           "Cust Part#,"
           "Item Description,"
           "FG Item #," 
           "Order#,"
           "CAD#,"
           "PO#,"
           " Order Qty,"
           "Qty OnHand,"
           "Qty Shippd,"
           "Qty ActRel,"
           "   Qty WIP,"
           " Qty Avail".

       IF tb_est-count THEN
          PUT STREAM st-excel ",Est Unit (Case) Count".

       IF tb_est-pallets THEN
          PUT STREAM st-excel ",Est Units/Pallet".

       PUT STREAM st-excel SKIP.
    END.

    {sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.

    FOR EACH tt-report:
      DELETE tt-report.
    END.
    EMPTY TEMP-TABLE tt-fg-bin.

    {oerep/r-ordopn.i}

    OUTPUT STREAM st-excel CLOSE.
    

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

  IF AVAIL user-print THEN DO li = 1 TO 70:
    
     IF INDEX(parm-var-list,user-print.field-name[li]) > 0 THEN
        ASSIGN parm-val-list = parm-val-list + user-print.field-value[li] + ","
               parm-lbl-list = parm-lbl-list +
                               (IF user-print.field-label[li] = ? THEN user-print.field-name[li]
                                ELSE user-print.field-label[li]) + ","
               parm-fld-list = parm-fld-list + user-print.field-name[li] + "," .

     CASE user-print.field-name[li]:
         
          {BATCH/rptvalue.i user-print.field-name[li] "begin_cust-no" begin_cust-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_cust-no" end_cust-no}
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_ord-date" begin_ord-date "Date" }
          {BATCH/rptvalue.i user-print.field-name[li] "end_ord-date" end_ord-date "date"}
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_po-no" begin_po-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_po-no" end_po-no }
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_job-no" begin_job-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_job-no" end_job-no }
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_job-no2" begin_job-no2 }
          {BATCH/rptvalue.i user-print.field-name[li] "end_job-no2" end_job-no2 }
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_i-no" begin_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_i-no" end_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "begin_cad-no" begin_cad-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_cad-no" end_cad-no }
          {BATCH/rptvalue.i user-print.field-name[li] "begin_due-date" begin_due-date "date"}
          {BATCH/rptvalue.i user-print.field-name[li] "end_due-date" end_due-date "date"}
          {BATCH/rptvalue.i user-print.field-name[li] "begin_userid" begin_userid }
          {BATCH/rptvalue.i user-print.field-name[li] "end_userid" end_userid }
          {BATCH/rptvalue.i user-print.field-name[li] "begin_slsmn" begin_slsmn }
          {BATCH/rptvalue.i user-print.field-name[li] "end_slsmn" end_slsmn }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_sort" rd_sort }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_jstat" rd_jstat }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_ostat" rd_ostat }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_wip-qty" rd_wip-qty }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_sort-1" rd_sort-1 }
          {BATCH/rptvalue.i user-print.field-name[li] "rd_due-date" rd_due-date }
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_due-date" tb_due-date "Logical" "yes}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt }
          {BATCH/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name }
          {BATCH/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_job-qty" tb_job-qty "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_excel" tb_excel "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_0-bal" tb_0-bal "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_0-wip" tb_0-wip "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_0-avl" tb_0-avl "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_under" tb_under "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_est-count" tb_est-count "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_est-pallets" tb_est-pallets "Logical" "yes}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_sch" tb_sch "Logical" "yes}
          {BATCH/rptvalue.i user-print.field-name[li] "v-excel-file" v-excel-file }
    
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

PROCEDURE build-tt:
    /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DEF BUFFER b-ar-invl FOR ar-invl.
  DEF BUFFER b-inv-head FOR inv-head.
  DEF BUFFER b-inv-line FOR inv-line.
  DEF BUFFER b-oe-rell FOR oe-rell.

  DEF VAR v-po-no LIKE oe-ord.po-no NO-UNDO.


  v-po-no = oe-ordl.po-no.

  CREATE tt-report.

  FIND FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN tt-report.cad-no = itemfg.cad-no.

  IF tt-report.cad-no EQ "" THEN DO:
    RELEASE eb.
    IF TRIM(oe-ordl.est-no) NE "" THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.est-no   EQ oe-ordl.est-no
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX est-no NO-LOCK NO-ERROR.
    IF NOT AVAIL eb THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX stock NO-LOCK NO-ERROR.
    IF AVAIL eb THEN tt-report.cad-no = eb.cad-no.
  END.

  RELEASE eb.

  IF TRIM(oe-ordl.est-no) NE "" THEN
  DO:
     FIND FIRST eb WHERE
          eb.company  EQ oe-ordl.company AND
          eb.est-no   EQ oe-ordl.est-no AND
          eb.stock-no EQ oe-ordl.i-no AND
          eb.form-no  EQ oe-ordl.form-no AND
          eb.blank-no EQ oe-ordl.blank-no
          NO-LOCK NO-ERROR.

     IF AVAIL eb THEN
     DO:
        ASSIGN
          tt-report.unit-count = eb.cas-cnt
          tt-report.units-pallet = eb.cas-pal.
        RELEASE eb.
     END.
  END.

  ASSIGN
   tt-report.term-id  = ""
   tt-report.key-01   = IF rd_sort-1 EQ "Due Date" THEN
                          STRING(YEAR(lv-due-date),"9999") +
                          STRING(MONTH(lv-due-date),"99")  +
                          STRING(DAY(lv-due-date),"99")
                        ELSE
                        IF rd_sort-1 EQ "Salesman" THEN
                          oe-ordl.s-man[1]         ELSE ""
   tt-report.key-02   = oe-ord.cust-no
   tt-report.key-03   = IF v-sort EQ "PO" THEN v-po-no
                        ELSE 
                        IF v-sort EQ "It" THEN
                          (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                        ELSE
                        IF v-sort EQ "Cu" THEN
                          (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "FG" THEN
                          (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "Or" THEN
                          (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE
                        IF v-sort EQ "CA" THEN
                          (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                          (STRING(YEAR(lv-due-date),"9999") +
                           STRING(MONTH(lv-due-date),"99")  +
                           STRING(DAY(lv-due-date),"99")    +
                           STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
   tt-report.key-04   = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                        TRIM(oe-ordl.job-no) + "-" +
                        STRING(oe-ordl.job-no2,"99")
   tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
   tt-report.key-06   = oe-ordl.i-no
   tt-report.key-07   = STRING(YEAR(ip-date),"9999") +
                        STRING(MONTH(ip-date),"99")  +
                        STRING(DAY(ip-date),"99")
   tt-report.po-no    = v-po-no
   tt-report.rec-id   = ip-recid
   tt-report.row-id   = ROWID(oe-ordl)
   tt-report.due-date = lv-due-date
   v-ordl             = NO.

  FIND b-ar-invl WHERE RECID(b-ar-invl) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-ar-invl THEN
    ASSIGN
     tt-report.q-shp  = b-ar-invl.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-ar-invl.inv-no.

  FIND b-inv-line WHERE RECID(b-inv-line) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-inv-line THEN DO:
    FIND FIRST b-inv-head WHERE b-inv-head.r-no EQ b-inv-line.r-no NO-LOCK.
    ASSIGN
     tt-report.q-shp  = b-inv-line.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-inv-head.inv-no.
  END.

  FIND b-oe-rell WHERE RECID(b-oe-rell) EQ ip-recid NO-LOCK NO-ERROR.
  IF NOT tt-report.inv AND
     AVAIL b-oe-rell   THEN tt-report.q-rel = b-oe-rell.qty.

END PROCEDURE.


PROCEDURE calc-qoh:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC                EXTENT 6.
  DEF VAR v-qohi        LIKE v-qohj.
  DEF VAR v-qty         AS   INT.
  DEF VAR v-qty1        LIKE v-qty.
  DEF VAR v-qtyc        LIKE v-qty.
  DEF VAR v-red         LIKE v-qty.
  DEF VAR v             AS   INT.
  DEF VAR v-val         AS   DEC                EXTENT 4.
  DEF VAR v-cst         AS   DEC                EXTENT 4.
  DEF VAR v-u-val       AS   DEC.
  DEF VAR v-u-cst       AS   DEC.
  DEF VAR v-date        AS   DATE.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.


  ASSIGN
   vdat     = TODAY
   v-curr   = YES
   v-q-or-v = YES.
  
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
  END.

END PROCEDURE.

