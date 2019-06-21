DEFINE TEMP-TABLE ttbalanordpo NO-UNDO
    FIELD vBalFile AS CHAR
    FIELD vtextfile AS CHAR
    FIELD vFile AS CHAR.
    DEFINE DATASET dsbalanordpo FOR ttbalanordpo .
DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmActBal          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegcust         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndCust         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegOrdDate      AS DATE       NO-UNDO.
DEFINE INPUT PARAMETER prmEndOrdDate      AS DATE       NO-UNDO.
DEFINE INPUT PARAMETER prmBegCustPo       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndCustPo       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegJob          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndJob          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegJob2         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndJob2         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegItem         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndItem         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegSman         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEndSman         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSort            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJobStat         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrdStat         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPrint           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUnderrun        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJobqty          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmZerobal         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPofrom          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDays            AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER prmDate            AS DATE       NO-UNDO.
DEFINE INPUT PARAMETER prmZeroqoh         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPageBreak       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSchRel          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOut             AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbalanordpo.
DEFINE OUTPUT PARAMETER cError            AS CHARACTER       NO-UNDO.


IF prmUser      =   ?  THEN ASSIGN prmUser       =  "".
IF prmActBal    =   ?  THEN ASSIGN prmActBal     =  "".
IF prmBegcust   =   ?  THEN ASSIGN prmBegcust    =  "".
IF prmEndCust   =   ?  THEN ASSIGN prmEndCust    =  "".
IF prmBegCustPo =   ?  THEN ASSIGN prmBegCustPo  =  "".
IF prmEndCustPo =   ?  THEN ASSIGN prmEndCustPo  =  "".
IF prmBegJob    =   ?  THEN ASSIGN prmBegJob     =  "".
IF prmEndJob    =   ?  THEN ASSIGN prmEndJob     =  "".
IF prmBegItem   =   ?  THEN ASSIGN prmBegItem    =  "".
IF prmEndItem   =   ?  THEN ASSIGN prmEndItem    =  "".
IF prmBegSman   =   ?  THEN ASSIGN prmBegSman    =  "".
IF prmEndSman   =   ?  THEN ASSIGN prmEndSman    =  "".
IF prmSort      =   ?  THEN ASSIGN prmSort       =  "".
IF prmJobStat   =   ?  THEN ASSIGN prmJobStat    =  "".
IF prmOrdStat   =   ?  THEN ASSIGN prmOrdStat    =  "".
IF prmPrint     =   ?  THEN ASSIGN prmPrint      =  "".
IF prmUnderrun  =   ?  THEN ASSIGN prmUnderrun   =  "".
IF prmJobqty    =   ?  THEN ASSIGN prmJobqty     =  "".
IF prmZerobal   =   ?  THEN ASSIGN prmZerobal    =  "".
IF prmPofrom    =   ?  THEN ASSIGN prmPofrom     =  "".
IF prmDays      =   ?  THEN ASSIGN prmDays       =  0.
IF prmZeroqoh   =   ?  THEN ASSIGN prmZeroqoh    =  "".


DEF TEMP-TABLE tt-report LIKE report
    FIELD q-onh  LIKE itemfg.q-onh
    FIELD q-shp  LIKE itemfg.q-onh
    FIELD q-wip  LIKE itemfg.q-onh
    FIELD po-no  LIKE oe-ord.po-no
    FIELD inv    AS   LOG
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD part-no LIKE oe-ordl.part-no 
    FIELD i-name  LIKE oe-ordl.i-name 
    FIELD prod-qty    AS INT 
    FIELD qty-to-prod AS INT
    FIELD row-id AS ROWID
    INDEX row-id row-id.
DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.
DEF VAR lv-pdf-file AS cha NO-UNDO.  
DEF STREAM excel.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF BUFFER b-oe-bolh FOR oe-bolh.
DEF BUFFER b-oe-boll FOR oe-boll.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.

DEFINE NEW SHARED VARIABLE lv-stat AS CHARACTER NO-UNDO.
DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999      NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)"                       NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)"                         NO-UNDO.
DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)"                        NO-UNDO.
DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99" INITIAL "00"           NO-UNDO.
DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001   NO-UNDO.
DEFINE VARIABLE begin_po-no AS CHARACTER FORMAT "X(15)"                        NO-UNDO.
DEFINE VARIABLE begin_slmn AS CHARACTER FORMAT "XXX"                           NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"      NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz"         NO-UNDO.
DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99" INITIAL "99"             NO-UNDO.
DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999    NO-UNDO.
DEFINE VARIABLE end_po-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO. 
DEFINE VARIABLE end_slmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0           NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  /*INITIAL "C:\Inetpub\wwwroot\pdfs\orderBalance.csv"*/   NO-UNDO.
DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)"                         NO-UNDO.
DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)"                         NO-UNDO.
DEFINE VARIABLE lbl_prt-baldue-2 AS CHARACTER FORMAT "X(256)"                  NO-UNDO.
DEFINE VARIABLE lbl_prt-baldue-4 AS CHARACTER FORMAT "X(256)"                  NO-UNDO.
DEFINE VARIABLE lbl_prt-po AS CHARACTER FORMAT "X(256)"                        NO-UNDO.
DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)"                          NO-UNDO.
DEFINE VARIABLE rd_jstat AS CHARACTER                                          NO-UNDO.
DEFINE VARIABLE rd_ostat AS CHARACTER                                          NO-UNDO.
DEFINE VARIABLE rd_prt-baldue AS CHARACTER                                     NO-UNDO.
DEFINE VARIABLE tb_0-bal AS LOGICAL INITIAL yes                                NO-UNDO.
DEFINE VARIABLE tb_0-qoh AS LOGICAL INITIAL yes                                NO-UNDO.
DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no                               NO-UNDO.
DEFINE VARIABLE init-dir AS CHAR NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 48.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_sch AS LOGICAL INITIAL no NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER NO-UNDO.
DEFINE VARIABLE vPdfFile AS CHAR NO-UNDO.
DEFINE  VARIABLE rd_prt-po AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_under AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_break AS LOGICAL INITIAL no NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-report-title AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEF VAR v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.
    
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust OR prmEndCust = "zzzzzzzz" )  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.

  FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
  END.

IF prmActBal = "OrderBal" THEN DO:
 assign
 cocode = prmComp
 locode = usercomp.loc
 v-today = TODAY. 
 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    ASSIGN
     begin_cust-no      =     prmBegcust                                          
     end_cust-no        =     prmEndCust                                         
     begin_ord-date     =     prmBegOrdDate                                         
     end_ord-date       =     prmEndOrdDate                                         
     begin_po-no        =     prmBegCustPo                                         
     end_po-no          =     prmEndCustPo                                         
     begin_job-no       =     prmBegJob
     begin_job-no2      =     prmBegJob2
     end_job-no         =     prmEndJob  
     end_job-no2        =     prmEndJob2
     begin_i-no         =     prmBegItem                                         
     end_i-no           =     prmEndItem                                         
     rd_sort            =     prmSort
     rd_jstat           =     prmJobStat
    rd_ostat            =     prmOrdStat
     rd_prt-baldue      =     prmPrint
     begin_slmn         =     prmBegSman
    end_slmn            =     prmEndSman
    rd_prt-po           =     prmPofrom
    fi_days-old         =     prmDays
    as-of-date          =     prmDate
    tb_break            =  IF    prmPageBreak = "Yes" THEN TRUE ELSE FALSE
    tb_0-qoh            =  IF   prmZeroqoh = "Yes" THEN TRUE ELSE FALSE
    tb_0-bal            =  IF  prmZerobal = "Yes" THEN TRUE ELSE FALSE 
    tb_under            =  IF   prmUnderrun = "Yes" THEN TRUE ELSE FALSE 
    tb_job-qty          =  IF    prmJobqty  = "Yes" THEN TRUE ELSE FALSE
    tb_sch              =  IF    prmSchRel  = "Yes" THEN TRUE ELSE FALSE.
    
ASSIGN
    init-dir    = v-webrootpath
    fi_file = init-dir + "Ordbalpo" +
    STRING(YEAR(v-today),"9999")
    + STRING(MONTH(v-today),"99")
    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
vPdfFile   = "Ordbalpo" +
    STRING(YEAR(v-today),"9999")
    + STRING(MONTH(v-today),"99")
    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       

run run-report.

CREATE ttbalanordpo.
    ASSIGN ttbalanordpo.vFile = vPdfFile.
END. /*IF prmActBal = "OrderBal" THEN DO:*/

/*********************************************************************************************************/    
PROCEDURE run-report :
{sys/form/r-top3w.f "Hot Keys O-R-8"}

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
def var v-bal   as   log format "BalDue/InvAmt" init yes.
def var v-jobq  as   log             format "Yes/No" init no.

def var v-dat     as   date format "99/99/99".
def var v-bal-qty as   int format ">>>,>>>,>>9".
def var v-inv-amt as   int format ">>>>,>>9.99".
def var v-cust-no like cust.cust-no.
def var v-name    like cust.name.
def var v-sman    like sman.sman.
def var v-sname   like sman.sname.
def var v-field1  as   char format "x(36)".
def var v-label   as   char format "x(11)" init "Invoice Amt".
def var v-field2  like v-label.
def var v-ord-no  as   char.
def var v-q-onh   like itemfg.q-onh NO-UNDO.
def var v-q-shp   like v-q-onh NO-UNDO.
def var v-q-rel   like v-q-onh NO-UNDO.
def var v-q-wip   like v-q-onh NO-UNDO.
def var v-q-avl   like v-q-onh NO-UNDO.
DEF VAR begin_due-date AS DATE NO-UNDO.
DEF VAR end_due-date AS DATE NO-UNDO.
DEF VAR rd_due-date AS CHAR NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.
DEF VAR act-rel-qty AS INT NO-UNDO.

def var v-time as int.
v-time = time.

DEF VAR excelheader AS CHAR NO-UNDO.

FORMAT HEADER
       SKIP(1)
       "Sales Rep:"
       v-sman FORMAT "x(8)"
       v-sname
       
    WITH FRAME r-top1 STREAM-IO WIDTH 180 NO-BOX PAGE-TOP.

FORMAT HEADER
       SKIP(1)
       "Customer:"
       v-cust-no
       v-name
       SKIP(1)
       "PO Number      "
       "Order#/Job#"
       "FG Item #      "
       "Ord Date"
       "  Order Qty"
       "Inv/Rel Date"
       "Inv#  "
       "Qty Shipped"
       "Release Qty"
       v-label
       "Qty On-Hand"
       SKIP
       "---------------"
       "-----------"
       "---------------"
       "--------"
       "-----------"
       "------------"
       "------"
       "-----------"
       "-----------"
       "-----------"
       "-----------"
       
    WITH FRAME r-top2 STREAM-IO WIDTH 180 NO-BOX PAGE-TOP.

SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = "Order Balance by Po#"
 str-tit3 = "By Customer" 
 {sys/inc/ctrtext.i str-tit2 112}
 {sys/inc/ctrtext.i str-tit3 132}

 v-cust[1]  = begin_cust-no
 v-cust[2]  = end_cust-no
 v-date[1]  = begin_ord-date
 v-date[2]  = end_ord-date
 v-po[1]    = begin_po-no
 v-po[2]    = end_po-no
 v-job[1]   = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
 v-job[2]   = fill(" ",6 - length(trim(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99")
 v-item[1]  = begin_i-no
 v-item[2]  = end_i-no
 v-sort     = substr(rd_sort,1,1)
 v-inc      = tb_0-bal
 v-stat     = substr(rd_jstat,1,1)
 v-ostat    = substr(rd_ostat,1,1)
 v-jobq     = tb_job-qty
 v-bal      = rd_prt-baldue eq "Balance Due".

if v-bal then v-label = "Balance Due".

/*
if tmp-dir = "" then tmp-dir = "C:\Inetpub\wwwroot\pdfs".
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.*/

{sys/inc/print1.i}
{sys/inc/outprint.i value(0)}

   PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7>" FORM "x(150)" SKIP.

OUTPUT CLOSE.
{sys/inc/outprint.i "value(lines-per-page) append" }

VIEW FRAME r-top.

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  IF tb_break THEN
    excelheader = "Sales Rep ID,Sales Rep Name,".

  excelheader = excelheader + "Cust #,Cust Name,PO Number,Order#/Job#,"
               + "FG Item #,Item Name,Ord Date,Order Qty,Inv/Rel Date,Inv#,"
               + "Qty Shipped,Release Qty," + v-label + ",Qty On-Hand".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF tb_break THEN VIEW FRAME r-top1.

VIEW FRAME r-top2.
FOR EACH tt-report:
  DELETE tt-report.
END.
FOR EACH tt-fg-bin:
  DELETE tt-fg-bin.
END.

{oerep/r-ordbal.i}

end procedure.



/***************************************************************/

PROCEDURE build-tt :
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

  DEF VAR v-po-no LIKE oe-ord.po-no NO-UNDO.


  /*v-po-no = IF rd_prt-po EQ "Line" THEN oe-ordl.po-no ELSE oe-ord.po-no.*/

    IF rd_prt-po EQ "Line" THEN
     v-po-no = oe-ordl.po-no.
  ELSE IF rd_prt-po EQ "Header" THEN
     v-po-no = oe-ord.po-no.
  ELSE
  DO:
     IF AVAIL oe-rell THEN
        v-po-no = oe-rell.po-no.
     ELSE
     DO: 
        if avail ar-invl then do:
           find first b-oe-bolh
               where b-oe-bolh.company eq cocode
                 and b-oe-bolh.bol-no  eq ar-invl.bol-no
               no-lock no-error.
           if avail b-oe-bolh then do:
              find first b-oe-boll
                  where b-oe-boll.company eq cocode
                    and b-oe-boll.b-no    eq b-oe-bolh.b-no
                    and b-oe-boll.i-no    eq ar-invl.i-no
                  no-lock no-error.
              if avail b-oe-boll then do:
                find first b-oe-rell
                    where b-oe-rell.company eq cocode
                      and b-oe-rell.r-no    eq b-oe-boll.r-no
                      AND b-oe-rell.ord-no  EQ b-oe-boll.ord-no
                      and b-oe-rell.i-no    eq b-oe-boll.i-no
                      and b-oe-rell.line    eq b-oe-boll.line
                      no-lock no-error.
             
                IF AVAIL b-oe-rell THEN 
                   v-po-no = b-oe-rell.po-no. 
             END.
           END.
        END.
        ELSE IF AVAIL inv-line THEN DO:
           find first b-oe-bolh
               where b-oe-bolh.company eq cocode
                 and b-oe-bolh.b-no    eq inv-line.b-no
               no-lock no-error.
           if avail b-oe-bolh then do:
              find first b-oe-boll
                  where b-oe-boll.company eq cocode
                    and b-oe-boll.b-no    eq b-oe-bolh.b-no
                    and b-oe-boll.i-no    eq inv-line.i-no
                  no-lock no-error.
              if avail b-oe-boll then do:
                 find first b-oe-rell
                     where b-oe-rell.company eq cocode
                       and b-oe-rell.r-no    eq b-oe-boll.r-no
                       and b-oe-rell.i-no    eq b-oe-boll.i-no
                       and b-oe-rell.line    eq b-oe-boll.line
                     USE-INDEX r-no no-lock no-error.
                 if avail b-oe-rell THEN 
                    v-po-no = b-oe-rell.po-no.
              end.
           end.
        END.
        ELSE IF AVAIL oe-rel THEN
             v-po-no = oe-rel.po-no.

        ELSE IF AVAIL oe-ordl THEN
        DO:
           FIND FIRST b-oe-rel WHERE
                b-oe-rel.company EQ cocode AND
                b-oe-rel.ord-no EQ oe-ordl.ord-no
                NO-LOCK NO-ERROR.

           IF AVAIL b-oe-rel THEN
              v-po-no = b-oe-rel.po-no.
        END.
        else v-po-no = "".

       IF tb_sch THEN do:
        IF AVAIL oe-rel THEN
             v-po-no = oe-rel.po-no.
        ELSE v-po-no = "".
       END.

        IF NOT(v-po-no GE begin_po-no AND
               v-po-no LE end_po-no) THEN NEXT.
     END.
  END.



  CREATE tt-report.
  ASSIGN
   tt-report.term-id = ""
   tt-report.key-01  = IF tb_break THEN oe-ordl.s-man[1] ELSE ""
   tt-report.key-02  = oe-ord.cust-no
   tt-report.key-03  = IF v-sort EQ "P" THEN v-po-no
                       ELSE 
                       IF v-sort EQ "I" THEN
                         (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                       ELSE
                       IF v-sort EQ "C" THEN
                         (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                       ELSE
                       IF v-sort EQ "F" THEN
                         (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                       ELSE
                       IF v-sort EQ "O" THEN
                         (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                       ELSE  
                         (STRING(YEAR(oe-ordl.req-date),"9999") +
                          STRING(MONTH(oe-ordl.req-date),"99")  +
                          STRING(DAY(oe-ordl.req-date),"99")    +
                          STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
   tt-report.key-04  = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                       TRIM(oe-ordl.job-no) + "-" +
                       STRING(oe-ordl.job-no2,"99")
   tt-report.key-05  = STRING(oe-ord.ord-no,"99999999999")
   tt-report.key-06  = oe-ordl.i-no
   tt-report.key-07  = STRING(YEAR(ip-date),"9999") +
                       STRING(MONTH(ip-date),"99")  +
                       STRING(DAY(ip-date),"99")
   tt-report.po-no   = v-po-no
   tt-report.rec-id  = ip-recid
   tt-report.row-id  = ROWID(oe-ordl)
   v-ordl            = no
                             .

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
  
 IF NOT CAN-FIND(FIRST tt-fg-bin WHERE tt-fg-bin.i-no EQ tt-report.key-06) THEN RUN calc-qoh.

END PROCEDURE.
/***********************************************************************************/
PROCEDURE calc-qoh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC EXTENT 6.
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
  DEF VAR lv-tag        LIKE fg-rdtlh.tag NO-UNDO.
  DEF VAR ld-last       AS   DATE NO-UNDO.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.


  ASSIGN
   vdat     = as-of-date
   v-curr   = YES
   v-q-or-v = YES.
  
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-report.key-06
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.

    IF fi_days-old NE 0 THEN DO:
      tt-fg-bin.qty = 0.

      FOR EACH fg-rcpth
          WHERE fg-rcpth.company    EQ cocode
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.trans-date LE as-of-date
          NO-LOCK USE-INDEX tran,

          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no       EQ fg-rcpth.r-no
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.rita-code  EQ fg-rcpth.rita-code
          NO-LOCK

          BREAK BY fg-bin.i-no
                BY fg-bin.job-no
                BY fg-bin.job-no2
                BY fg-bin.loc
                BY fg-bin.loc-bin
                BY fg-bin.tag
                BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:

        IF FIRST(fg-bin.i-no) THEN
          ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.
                  
        {fg/rep/fg-aging.i fi_days-old}
            
        if last-of(fg-bin.tag) then do:
          v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                   v-qohj[4] + v-qohj[5] + v-qohj[6].

          if v-qohj[6] lt 0 then do:
            v-qty = v-qohj[6] * -1.
          
            do v = 5 to 1 by -1:
              if v-qohj[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.
               
              if v-qty le 0 then leave.
            end.
          
            if v-qty gt 0 then v-qohi[6] = v-qohi[6] - v-qty.
          end.

          release oe-ordl.
          if fg-bin.job-no ne "" then
          find last oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.job-no  eq fg-bin.job-no
                and oe-ordl.job-no2 eq fg-bin.job-no2
                and oe-ordl.i-no    eq fg-rcpth.i-no
              use-index job no-lock no-error.
              
          if not v-curr then
            assign
             v-qohj[1] = 0
             v-qohj[2] = 0
             v-qohj[3] = 0.

          assign
           v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
           v-qohi[1] = v-qohi[1] + v-qohj[1]
           v-qohi[2] = v-qohi[2] + v-qohj[2]
           v-qohi[3] = v-qohi[3] + v-qohj[3]
           v-qohi[4] = v-qohi[4] + v-qohj[4]
           v-qohi[5] = v-qohi[5] + v-qohj[5]
           v-qohj    = 0.
         
          if avail oe-ordl then
            assign
             v-u-cst  = oe-ordl.t-cost / oe-ordl.qty
             v-u-val  = oe-ordl.t-price / oe-ordl.qty.
           
          else do:
            if itemfg.prod-uom eq "EA" then
              v-u-cst = itemfg.total-std-cost.
            else
              run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                   itemfg.total-std-cost, output v-u-cst).
                                   
            if itemfg.sell-uom eq "EA" then
              v-u-val = itemfg.sell-price.
            else
              run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                     itemfg.sell-price, output v-u-val).
          end.
        
          if v-u-cst eq ? then v-u-cst = 0.
          if v-u-val eq ? then v-u-val = 0.
        
          assign
           v-cst[1] = v-cst[1] + (v-qty * v-u-cst)
           v-val[1] = v-val[1] + (v-qty * v-u-val).
        end.

        if last-of(fg-bin.i-no) then do:
          if v-qohi[6] lt 0 then do:
            v-qty = v-qohi[6] * -1.
          
            do v = 5 to 1 by -1:
              if v-qohi[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohi[v])
                 v-qohi[v] = v-qohi[v] - v-red
                 v-qty     = v-qty     - v-red.
               
              if v-qty le 0 then leave.
            end.
          
            if v-qty gt 0 then
              assign
               v-qohi   = 0
               v-cst[1] = 0
               v-val[1] = 0.
          end.

          if v-cst[1] lt 0 then v-cst[1] = 0.
          if v-val[1] lt 0 then v-val[1] = 0.
        
          if not v-q-or-v then do:
            v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
          
            do v = 1 to 5:
              v-qohi[v] = v-val[1] / v-qty * v-qohi[v].
             
              if v-qohi[v] eq ? then v-qohi[v] = 0.
            end.
          end.

          tt-fg-bin.qty = v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
        end.
      END.
    END.
  END.

END PROCEDURE.
