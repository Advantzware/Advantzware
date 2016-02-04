/* salrep/s-slmprd.p  batch procedure for r-slmprd.w */


/* ===== input parameters =======*/
DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

DEF VAR inv-date AS DATE NO-UNDO.
DEF VAR begin_slmn AS cha NO-UNDO.
DEF VAR end_slmn AS cha NO-UNDO.
DEF VAR begin_i-no AS cha NO-UNDO.
DEF VAR end_i-no AS cha NO-UNDO.
DEF VAR tb_round AS LOG NO-UNDO.
DEF VAR tb_fin-chg AS LOG NO-UNDO.
DEF var lv-ornt AS cha NO-UNDO.
DEF var lines-per-page AS INT NO-UNDO.
DEF var rd-dest AS INT NO-UNDO.
DEF var lv-font-no AS INT NO-UNDO.
DEF VAR lv-font-name AS cha NO-UNDO.
DEF var td-show-parm AS LOG NO-UNDO.
DEF VAR tb_excel AS LOG NO-UNDO.
DEF VAR tb_runExcel AS LOG NO-UNDO.
DEF VAR fi_file AS CHAR NO-UNDO.

DEF VAR v-prgmname AS cha NO-UNDO.
DEF VAR v-prt-name AS cha NO-UNDO.
DEF VAR v-copies AS INT INIT 1 NO-UNDO.
def var parm-fld-list as cha no-undo.
def var parm-lbl-list as cha no-undo.
DEF VAR parm-val-list AS cha NO-UNDO.
DEF VAR parm-var-list AS cha NO-UNDO.
 
DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID.

DEF BUFFER b-tt-report FOR tt-report.
DEF STREAM excel.

ASSIGN
v-prgmname = PROGRAM-NAME(1)
parm-var-list = "inv-date,begin_slmn,end_slmn,begin_i-no,end_i-no," +
                "tb_round,tb_fin-chg," +
                "lv-ornt,lines-per-page,rd-dest,lv-font-no,lv-font-name,td-show-parm," +
                "tb_excel,tb_runExcel,fi_file".

/*==============  definition =======================*/
  /* from report's definition section */

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def TEMP-TABLE w-data no-undo
  field w-type      as   char
  field w-sman-no   as   char format "x(3)"
  field w-sqft      AS   DEC      extent 3   column-label "Sq Ft/M"
  field w-amt       AS   DEC      extent 3   column-label "Amount"
  field w-pmsf      AS   DEC      extent 3   column-label "$/MSF".
  
def TEMP-TABLE w-data1 NO-UNDO like w-data.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF BUFFER xreport FOR tt-report.
/*=====================================*/

/* =========== main ========*/

RUN get-values.
RUN run-report.
RUN run-print.
RETURN.
/* ======= end of main ======*/

PROCEDURE run-report:
    /***************************************************************************\
    *****************************************************************************
    **  Program: /u2/fold/all/test/asi/sa/sa
    **       by: Christopher A. Heins
    ** Descript: Invoicing summary by rep
    **
    *****************************************************************************
    \***************************************************************************/

    {sys/form/r-topw.f}

    def var called as log no-undo.

    called = NO.

    def var fdate as date extent 3 no-undo.
    /* [day] [period] [year] */
    def var edate as date extent 3 no-undo.
    def var tdate as date format "99/99/9999" no-undo.
    def var fsman as char format "x(3)" no-undo.
    def var tsman as char format "x(3)" no-undo.
    def var v-inc-fc as log init no no-undo.

    def var v-sman-no   as   char format "x(3)" no-undo.
    def var v-amt       AS   DEC no-undo.
    def var v-sqft      AS   DEC no-undo.

    def var v-tot-sqft  like v-sqft extent 6 NO-UNDO.
    def var v-tot-amt   like v-amt  extent 6 NO-UNDO.
    def var v-tot-pmsf  like v-amt  extent 6 NO-UNDO.

    def var v-period as int no-undo.
    def var v-year as int format "9999" no-undo.
    def var v-qty like ar-invl.ship-qty format "->>>,>>9.99" no-undo.
    def var v-exclude as log no-undo.
    def var v-pct as dec format "99.99" no-undo.
    def var v-misc as log no-undo.
    def var v-leave as log no-undo.
    def var v-fac as INT NO-UNDO.
    def var head as char no-undo.
    DEF VAR v-term LIKE tt-report.term-id NO-UNDO.
    DEF VAR lv-i-no LIKE itemfg.i-no NO-UNDO.
    DEF VAR lv-r-no LIKE oe-retl.r-no NO-UNDO.
    DEF VAR lv-type AS CHAR NO-UNDO.
    DEF VAR ld-inv-pct  AS   DEC NO-UNDO.
    DEF VAR excelheader AS CHAR NO-UNDO.
    DEF VAR lv-format-1 AS CHAR NO-UNDO.
    DEF VAR lv-format-2 AS CHAR NO-UNDO.
    DEF VAR lv-format-3 AS CHAR NO-UNDO.

    DEF BUFFER b-ar-invl FOR ar-invl.

    form w-data1.w-sman-no  column-label "No"
         sman.sname         column-label "Name" format "x(17)"
         w-data1.w-sqft[1]                      format "->>,>>9.999"
         w-data1.w-amt[1]                       format "->,>>>,>>9.99"
         w-data1.w-pmsf[1]                      format "->>,>>9.99"
         w-data1.w-sqft[2]                      format "->>,>>9.999"
         w-data1.w-amt[2]                       format "->,>>>,>>9.99"
         w-data1.w-pmsf[2]                      format "->>,>>9.99"
         w-data1.w-sqft[3]                      format "->>,>>9.999"
         w-data1.w-amt[3]                       format "->,>>>,>>9.99"
         w-data1.w-pmsf[3]                      format "->>,>>9.99"
         header skip(1)
         "    SalesRep"
         "---------------Daily----------------" at 23
         "-----------Period to Date-----------"
         "------------Year to Date------------"

        with no-box frame f-prod down STREAM-IO width 144.

    assign
     fdate = today
     tdate = today
     edate[3] = TODAY
     head = if called then "SALES ANALYSIS - SALESREP PERFORMANCE REPORT"
                      else "SALES ANALYSIS - SALESREP PRODUCTION REPORT".

    assign
     str-tit2 = /*c-win:title*/ "Sales Analysis - Sales Rep Production"
     {sys/inc/ctrtext.i str-tit2 112}

     tdate        = inv-date
     fsman        = begin_slmn
     tsman        = end_slmn
     v-inc-fc     = tb_fin-chg.

    IF tb_round THEN DO:
      DO WITH FRAME f-prod:
        ASSIGN
         w-data1.w-sqft[1]:FORMAT = "->>,>>>,>>9"
         w-data1.w-amt[1]:FORMAT  = "->>>>,>>>,>>9"
         w-data1.w-pmsf[1]:FORMAT = "->,>>>,>>9"
         w-data1.w-sqft[2]:FORMAT = "->>,>>>,>>9"
         w-data1.w-amt[2]:FORMAT  = "->>>>,>>>,>>9"
         w-data1.w-pmsf[2]:FORMAT = "->,>>>,>>9"
         w-data1.w-sqft[3]:FORMAT = "->>,>>>,>>9"
         w-data1.w-amt[3]:FORMAT  = "->>>>,>>>,>>9"
         w-data1.w-pmsf[3]:FORMAT = "->,>>>,>>9"
         lv-format-1 = "->>,>>>,>>9"
         lv-format-2 = "->>>>,>>>,>>9"
         lv-format-3 = "->,>>>,>>9".
      END.
    END.
    ELSE
       ASSIGN
         lv-format-1 = "->>,>>9.999"
         lv-format-2 = "->,>>>,>>9.99"
         lv-format-3 = "->>,>>9.99".

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN DO:
       OUTPUT STREAM excel TO VALUE(fi_file).
       excelheader = "No,SalesRep Name,Daily Sq Ft/M,Daily Amount,Daily $/MSF,"
                   + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,"
                   + "YTD $/MSF".
       PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    if td-show-parm then run show-param.

    SESSION:SET-WAIT-STATE ("general").

    display "" with frame r-top.

    FOR EACH tt-report:
      DELETE tt-report.
    END.

    FOR EACH w-data:
      DELETE w-data.
    END.

    FOR EACH w-data1:
      DELETE w-data1.
    END.

    {salrep/r-slmprd.i}

    IF tb_excel THEN DO:
       OUTPUT STREAM excel CLOSE.
       IF tb_runExcel THEN
          OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.


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
        
          {BATCH/rptvalue.i user-print.field-name[li] "inv-date" inv-date "Date"}
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_slmn" begin_slmn }
          {BATCH/rptvalue.i user-print.field-name[li] "end_slmn" end_slmn }
          {BATCH/rptvalue.i user-print.field-name[li] "Begin_i-no" begin_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "end_i-no" end_i-no }
          {BATCH/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt }
          {BATCH/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "INT"}
          {BATCH/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name }
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_round" tb_round "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_fin-chg" tb_fin-chg "Logical" "yes"} 
          {BATCH/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_excel" tb_excel "Logical" "yes"}
          {BATCH/rptvalu2.i user-print.field-name[li] "tb_runExcel" tb_runExcel "Logical" "yes"}
          {BATCH/rptvalue.i user-print.field-name[li] "fi_file" fi_file}
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

