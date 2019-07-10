

/*------------------------------------------------------------------------
    File        : Ordbalancepo.p
    Purpose     :  Order Balance by PO# Report

    Syntax      :

    Description : Return a Dataset  For Order Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
    DEFINE TEMP-TABLE ttbalancepo NO-UNDO
    FIELD vExFile AS CHAR
    FIELD vFile AS CHAR.
    DEFINE DATASET dsbalancepo FOR ttbalancepo .

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
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbalancepo.

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

DEFINE VARIABLE list-name AS CHARACTER no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report
    FIELD q-onh  LIKE itemfg.q-onh
    FIELD q-shp  LIKE itemfg.q-onh
    FIELD q-wip  LIKE itemfg.q-onh
    FIELD po-no  LIKE oe-ord.po-no
    FIELD inv    AS   LOG
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD row-id AS ROWID
    INDEX row-id row-id.
DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.
DEF VAR lv-pdf-file AS cha NO-UNDO.  
DEF STREAM excel.

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
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ordbal.csv" NO-UNDO.
DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)"                         NO-UNDO.
DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)"                         NO-UNDO.
DEFINE VARIABLE lbl_prt-baldue-2 AS CHARACTER FORMAT "X(256)"                  NO-UNDO.
DEFINE VARIABLE lbl_prt-baldue-4 AS CHARACTER FORMAT "X(256)"                  NO-UNDO.
DEFINE VARIABLE lbl_prt-po AS CHARACTER FORMAT "X(256)"                        NO-UNDO.
DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)"                          NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 99               NO-UNDO.
DEFINE VARIABLE rd-dest AS INTEGER                                             NO-UNDO.
DEFINE VARIABLE rd_jstat AS CHARACTER                                          NO-UNDO.
DEFINE VARIABLE rd_ostat AS CHARACTER                                          NO-UNDO.
DEFINE VARIABLE rd_prt-baldue AS CHARACTER                                     NO-UNDO.
DEFINE VARIABLE tb_0-bal AS LOGICAL INITIAL yes                                NO-UNDO.
DEFINE VARIABLE tb_0-qoh AS LOGICAL INITIAL yes                                NO-UNDO.
DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no                               NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO. 
{custom/xprint.i}
/*{{sys/inc/var.i new shared}*/

def new shared var cocode     as   char  format "x(3)"  no-undo.
def new shared var locode     as   char  format "x(5)"  no-undo.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegcust
    AND usercust.cust-no = prmEndCust  NO-ERROR.
/*IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Cust No should be same for Begin Cust and End Cust".
    RETURN.
END.*/
    assign
 cocode = prmComp
 locode = usercomp.loc. 
    IF prmActBal = "ordbalance" THEN DO:
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
     tb_0-bal           =     prmZerobal                                             
     rd_jstat           =     prmJobStat
     rd_ostat           =     prmOrdStat
     tb_job-qty         =     prmJobqty   .
    
    /*rd_prt-baldue      =
    prmBegSman  prmEndSman   prmPrint     rmUnderrun  prmPofrom    prmDays      prmDate      prmZeroqoh            */

/*
ASSIGN
    init-dir    = "C:\Inetpub\wwwroot\pdfs\".
    lv-pdf-file = init-dir + 'OPEN ORDER' .
    lv-pdf-file = lv-pdf-file + prmBeginCust.
    vPdfFile   = 'ORDER' + prmBeginCust + '.pdf'.*/
    run run-report.
    
    CREATE ttbalancepo.
    ASSIGN ttbalancepo.vFile = vPdfFile.
     
    END.

    /********************************************************************************************************************/

    PROCEDURE run-report :
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

DEF NEW SHARED VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.

def var v-time as int.
v-time = time.

DEF VAR excelheader AS CHAR NO-UNDO.

FORMAT HEADER
       SKIP(1)
       "Salesman:"
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
    ASSIGN
 str-tit2 = "Order Balance BY PO#"
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

{sys/inc/print1.i}

{sys/inc/outprint.i value(0)}
PUT "<PDF-EXCLUDE=MS Mincho><OLANDSCAPE></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7>" FORM "x(150)" SKIP.
OUTPUT CLOSE.
/*{sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/*/

DISPLAY "" FRAME r-top.

/*VIEW FRAME r-top2.*/

FOR EACH tt-report:
  DELETE tt-report.
END.
FOR EACH tt-fg-bin:
  DELETE tt-fg-bin.
END.


{oerep/r-ordbal.i}

end procedure.

