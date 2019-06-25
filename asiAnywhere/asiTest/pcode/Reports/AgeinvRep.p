/*------------------------------------------------------------------------
    File        : AgeinvRep.p
    Purpose     : Inventory Report
    Main File   : fgrep/r-ageinv.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRageinvRep NO-UNDO
    FIELD rageifile AS CHAR.
DEFINE DATASET dsRageinvRep FOR ttRageinvRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAsof       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeSman     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSman    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCust     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeItem     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeJob      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJob     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeJob2      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndjob2    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeWare     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWare    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmInvClass   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAgedDay1   AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmAgedDay2   AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmAgedDay3   AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmAgedDay4   AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmRdPrice    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmQtyValue   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComment    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustWhse   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPgBreak    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTbCurr     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCost       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmNegSale    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmValCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLastShip   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRageinvRep.

    IF prmUser     = ? THEN ASSIGN prmUser      = "".
    IF prmAction   = ? THEN ASSIGN prmAction    = "".
    IF prmAsof     = ? THEN ASSIGN prmAsof      = "".
    IF prmBeSman   = ? THEN ASSIGN prmBeSman    = "".
    IF prmEndSman  = ? THEN ASSIGN prmEndSman   = "".
    IF prmBeCust   = ? THEN ASSIGN prmBeCust    = "".
    IF prmEndCust  = ? THEN ASSIGN prmEndCust   = "".
    IF prmBeItem   = ? THEN ASSIGN prmBeItem    = "".
    IF prmEndItem  = ? THEN ASSIGN prmEndItem   = "".
    IF prmBeJob    = ? THEN ASSIGN prmBeJob     = "".
    IF prmEndJob   = ? THEN ASSIGN prmEndJob    = "".
    IF prmBeJob2    = ? THEN ASSIGN prmBeJob2   = "".
    IF prmEndjob2  = ? THEN ASSIGN prmEndjob2   = "". 
    IF prmBeWare   = ? THEN ASSIGN prmBeWare    = "". 
    IF prmEndWare  = ? THEN ASSIGN prmEndWare   = "". 
    IF prmInvClass = ? THEN ASSIGN prmInvClass  = "".  
    IF prmAgedDay1 = ? THEN ASSIGN prmAgedDay1  = 0. 
    IF prmAgedDay2 = ? THEN ASSIGN prmAgedDay2  = 0. 
    IF prmAgedDay3 = ? THEN ASSIGN prmAgedDay3  = 0. 
    IF prmAgedDay4 = ? THEN ASSIGN prmAgedDay4  = 0. 
    IF prmRdPrice  = ? THEN ASSIGN prmRdPrice   = "". 
    IF prmQtyValue = ? THEN ASSIGN prmQtyValue  = "". 
    IF prmComment  = ? THEN ASSIGN prmComment   = "". 
    IF prmSort     = ? THEN ASSIGN prmSort      = "". 
    IF prmCustWhse = ? THEN ASSIGN prmCustWhse  = "". 
    IF prmPgBreak  = ? THEN ASSIGN prmPgBreak   = "". 
    IF prmTbCurr   = ? THEN ASSIGN prmTbCurr    = "". 
    IF prmCustPart = ? THEN ASSIGN prmCustPart  = "".  
    IF prmCost     = ? THEN ASSIGN prmCost      = "". 
    IF prmNegSale  = ? THEN ASSIGN prmNegSale   = "". 
    IF prmValCust  = ? THEN ASSIGN prmValCust   = "". 
    IF prmLastShip = ? THEN ASSIGN prmLastShip  = "". 
    
    

DEFINE VARIABLE aged-days-1 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 NO-UNDO.
DEFINE VARIABLE aged-days-2 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 NO-UNDO.
DEFINE VARIABLE aged-days-3 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 NO-UNDO.
DEFINE VARIABLE aged-days-4 AS INTEGER FORMAT ">,>>>":U INITIAL 9999  NO-UNDO.
DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01  NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U NO-UNDO.
DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" NO-UNDO.
DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U  NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)":U NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz"  NO-UNDO.
DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99"  NO-UNDO.
DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE list_class AS CHARACTER FORMAT "X(256)":U  NO-UNDO.
DEFINE VARIABLE rd_price AS CHARACTER  NO-UNDO.
DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Quantity"  NO-UNDO.
DEFINE VARIABLE rd_show2 AS CHARACTER INITIAL "Comments"  NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item#"  NO-UNDO.
DEFINE VARIABLE tb_break AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_cost AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_curr AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_custpart AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_last-ship-date AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_neg-sale AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_val-cust AS LOGICAL INITIAL no NO-UNDO.
    
DEFINE VARIABLE v-tot-for-item AS INT NO-UNDO.
DEF VAR v-sell-price AS DEC EXTENT 6 NO-UNDO.
DEF NEW SHARED VAR age-days AS INT EXTENT 4 NO-UNDO.
DEF VAR lv-rct-date AS DATE NO-UNDO.
DEF VAR v-qty-2 AS DEC NO-UNDO.
DEF VAR v-shipdt AS CHAR EXTENT 2 NO-UNDO.
DEF VAR v-custpart AS CHAR EXTENT 2 NO-UNDO.
DEF VAR v-print-sls-tots AS LOG INIT YES NO-UNDO.
DEF VAR v-print-grand-tots AS LOG INIT YES NO-UNDO.
DEF VAR v-q AS INT NO-UNDO. 
DEF VAR v-tot-positive AS INT NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "c:\Inetpub\wwwroot\pdfs\Commission.csv" */  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes     NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no   NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin
                   FIELD first-date AS DATE
                   FIELD aged-qty   AS DEC EXTENT 6.
DEF BUFFER bf-tt-fg-bin FOR tt-fg-bin.
DEF NEW SHARED temp-table tt-file no-undo
                       field tt-sman like sman.sman
                       field tt-cust-no like cust.cust-no
                       field tt-i-no like itemfg.i-no
                       field tt-qohi AS DEC EXTENT 5
                       field tt-cst AS DEC EXTENT 4
                       field tt-val AS DEC EXTENT 4
                       FIELD tt-sell-price AS DEC EXTENT 5
                       FIELD tt-days AS INT.
DEF TEMP-TABLE tt-items 
    FIELD i-no LIKE itemfg.i-no
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2
    FIELD first-date AS DATE
    FIELD first-qty AS INT
    INDEX i1 i-no.

DEF NEW SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS   ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS   CHAR
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS   CHAR
    INDEX i-no  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX cust-no cust-no  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX part-no part-cust  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX procat procat  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX loc-bin-tag loc-bin-tag  i-no job-no job-no2 loc loc-bin tag bin-cust-no.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

def var vdat        as   date                   format "99/99/9999" init today.
def var fslm        like sman.sman.
def var tslm        like fslm                   init "zzz".
def var fcus        like cust.cust-no.
def var tcus        like fcus                   init "zzzzzzzz".
def var fitm        like itemfg.i-no.
def var titm        like fitm                   init "zzzzzzzzzzzzzzz".
def var fjob        like fg-bin.job-no.
def var tjob        like fjob                   init "zzzzzz".
def var fjob2       like fg-bin.job-no2         format "99".
def var tjob2       like fjob2                  init 99.
def var v-q-or-v    as   log                    format "Qty/Value" init yes.
def var v-sub-t     as   log                    format "Yes/No"    init no.
def var v-break     as   log                    format "Yes/No"    init yes.
def var v-cost      as   log                    format "Yes/No"    init no.
def var v-curr      as   log                    format "Yes/No"    init yes.
def var v-cpart     as   log                    format "Yes/No"    init no.
def var v-sdate     as   log                    format "Yes/No"    init no.

def var v-label     as   char                   format "x(8)"  extent 15.
def var v-label1    as   char                   format "x(13)" extent 13.
def var v-label2    as   char                   format "x(13)" extent 4.
def var v-label3    as   char                   format "x(4)" extent 4 NO-UNDO.
def var v-qohj      LIKE tt-qohi                extent 6.
def var v-qohi      like v-qohj.
def var v-qohc      like v-qohj                 extent 5.
def var v-qohs      like v-qohj                 extent 5.
def var v-qohg      like v-qohj                 extent 5.
def var v-qty       as   int.
def var v-qty1      like v-qty.
def var v-qtyc      like v-qty.
def var v-red       like v-qty.
def var v           as   int.
def var v-cus       like cust.cust-no.
def var v-val       as   dec    FORMAT "->,>>>,>>9.99"                extent 4.
def var v-cst       as   dec                    extent 4.
def var v-u-val     as   dec.
def var v-u-cst     as   dec.
def var v-date      as   date.
def var v-class     as   char.
def var sort-opt as char no-undo init "I" FORMAT "!".
DEF VAR lv-tag      LIKE fg-rdtlh.tag NO-UNDO.
DEF VAR ld-last     AS   DATE NO-UNDO.
DEF VAR lv-last-fld AS   CHAR FORMAT "x(13)" EXTENT 2 NO-UNDO.
DEF VAR lv-sname    LIKE sman.sname NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

DEF VAR li AS INT NO-UNDO.

def buffer b-f-rc for fg-rcpth.
def buffer b-f-rd for fg-rdtlh.


DEF STREAM excel.

/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE lv-case-count AS INT NO-UNDO.
DEF VAR v-dates  AS DATE EXTENT 5 NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    cocode = prmComp .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

IF prmAction = "Aged" THEN DO:

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    assign
    v-today       =  TODAY
    cocode        =   prmComp
    aged-days-1   = INT(prmAgedDay1 )
    aged-days-2   = INT( prmAgedDay2 )
    aged-days-3   = INT( prmAgedDay3 )
    aged-days-4   = INT( prmAgedDay4 )
    as-of-date    = DATE(prmAsof )
    begin_cust-no = prmBeCust
    begin_i-no    = prmBeItem
    begin_job-no  = prmBeJob
    begin_job-no2 = prmBeJob2
    begin_slm     = prmBeSman
    begin_whse    = prmBeWare
    end_cust-no   = prmEndCust
    end_i-no      = prmEndItem
    end_job-no    = prmEndJob
    end_job-no2   = prmEndjob2
    end_slm       = prmEndSman
    end_whse      = prmEndWare
    list_class    = prmInvClass
    rd_price      = prmRdPrice 
    rd_show       = prmQtyValue
    rd_show2      = prmComment
    rd_sort       = prmSort  .
   
   tb_break  =  IF  prmPgBreak ="True" THEN TRUE ELSE FALSE.
   tb_cost  =  IF  prmCost  ="True" THEN TRUE ELSE FALSE.
   tb_curr  = IF prmTbCurr = "True" THEN TRUE ELSE FALSE.
   tb_cust-whse  = IF prmCustWhse = "True" THEN TRUE ELSE FALSE.
   tb_custpart   =  IF  prmCustPart ="True" THEN TRUE ELSE FALSE.
   tb_last-ship-date  =  IF  prmLastShip  ="True" THEN TRUE ELSE FALSE.
   tb_neg-sale   = IF prmNegSale = "True" THEN TRUE ELSE FALSE.
   tb_val-cust  = IF prmValCust = "True" THEN TRUE ELSE FALSE.
   tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

    assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "AgedRep" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "AgedRep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "AgedRep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".
 RUN produce-report.
run run-report.
   
    CREATE ttRageinvRep.
    IF tb_excel  THEN
        ASSIGN ttRageinvRep.rageifile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttRageinvRep.rageifile = vTextFile .
END.

PROCEDURE run-report :
/* ---------------------------------------------- fg/rep/fg-aging.p 12/96 JLF */
/* finished goods aged inventory report                                       */
/* -------------------------------------------------------------------------- */

DEF VAR lv-comm-tail AS CHAR NO-UNDO. /* end of comment */
{sys/form/r-topw.f}

    form  header
      skip(1)
      day_str
      str-tit  format "x(112)"
      "Page" at 123
      page-number format ">>9"
      skip
      tim_str
      str-tit2 format "x(112)"   "{1}" at 123
      skip(1)
      
     with frame r-top2 row 1 column 1 stream-io width 150
	   no-labels no-box no-underline page-top.

FIND FIRST tt-file NO-LOCK NO-ERROR.

assign
 str-tit2 = "Aged Inventory Report"
 {sys/inc/ctrtext.i str-tit2 112}

 vdat      = as-of-date
 fslm      = begin_slm
 tslm      = END_slm
 fcus      = begin_cust-no
 tcus      = end_cust-no
 fitm      = begin_i-no
 titm      = end_i-no
 fjob      = fill(" ",6 - length(trim(begin_job-no))) +
                trim(begin_job-no) + string(int(begin_job-no2),"99")
 tjob      = fill(" ",6 - length(trim(end_job-no)))   +
                trim(end_job-no)   + string(int(end_job-no2),"99") 
 v-q-or-v  = rd_show EQ "Quantity"
 v-sub-t   = tb_val-cust
 v-break   = tb_break
 v-cost    = tb_cost
 v-curr    = tb_curr
 v-cpart   = tb_custpart
 v-sdate   = tb_last-ship-date

 v-label    = string(v-q-or-v,"    Qty/  Value")
 v-class    = list_class
 list_class = ""
 sort-opt    = SUBSTR(rd_sort,1,1) .

DO li = 1 TO NUM-ENTRIES(v-class):
  list_class = list_class + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
                                   ELSE ENTRY(li,v-class)).
END.
list_class = TRIM(list_class).

/*DO WITH FRAME {&FRAME-NAME}:
  DISPLAY list_class.
  ASSIGN list_class.
END.*/

IF v-cpart AND v-sdate THEN
    ASSIGN
     v-label2[01] = "Cust Part # "
     v-label2[02] = "------------" 
     v-label2[03] = "Last Ship " 
     v-label2[04] = "----------".
ELSE IF v-cpart AND NOT(v-sdate) THEN
    ASSIGN
     v-label2[01] = "Cust Part # "      
     v-label2[02] = "------------" 
     v-label2[03] = ""      
     v-label2[04] = "" .
ELSE IF v-sdate AND NOT(v-cpart) THEN
    ASSIGN
     v-label2[01] = "Last Ship   "      
     v-label2[02] = "------------" 
     v-label2[03] = ""      
     v-label2[04] = "" .
ELSE
    ASSIGN
     v-label2[01] = ""      
     v-label2[02] = ""
     v-label2[03] = ""      
     v-label2[04] = ""    .
     
IF v-cost THEN
  ASSIGN
   v-label1[1] = "             "
   v-label1[2] = "      Selling"
   v-label1[3] = "         Cost"
   v-label1[4] = "      Value $"
   v-label1[5] = "-------------"
   v-label1[6] = "-------------"
   v-label1[7] = "             "
   v-label1[8] = IF rd_show2 BEGINS "Com" THEN "Comm"
                                          ELSE "Days"
   v-label1[9] = "--------     " 
   v-label1[10] = v-label2[01]
   v-label1[11] = v-label2[02] 
   v-label1[12] = v-label2[03]
   v-label1[13] = v-label2[04] .
   
ELSE
  ASSIGN
   v-label1[1]  = "      Selling"
   v-label1[2]  = "             "
   v-label1[3]  = "      Value $"
   v-label1[4]  = IF rd_show2 BEGINS "Com" THEN "Comm"
                                          ELSE "Days"
   v-label1[5]  = "-------------"
   v-label1[6]  = IF rd_show2 BEGINS "Com" THEN "--------     " ELSE "----"
   v-label1[7]  = ""
   v-label1[8]  = v-label2[01]
   v-label1[9]  = v-label2[02]
   v-label1[10] = v-label2[03]
   v-label1[11] = v-label2[04]
   v-label1[12] = ""
   v-label1[13] = "".

ASSIGN v-label3[1]  = IF rd_show2 BEGINS "Com" THEN "Comm" ELSE "Days"          /*Task# 02041406*/
   v-label3[2]  = "----" .

ASSIGN
 v-label[06] = "00-" + TRIM(STRING(aged-days-1 - 1,">,>99"))
 v-label[07] = TRIM(STRING(aged-days-1,">,>99")) + "-" +
               TRIM(STRING(aged-days-2 - 1,">,>99"))
 v-label[08] = TRIM(STRING(aged-days-2,">,>99")) + "-" +
               TRIM(STRING(aged-days-3 - 1,">,>99"))
 v-label[09] = TRIM(STRING(aged-days-3,">,>99")) + "-" +
               TRIM(STRING(aged-days-4 - 1,">,>99"))
 v-label[10] = TRIM(STRING(aged-days-4,">,>99")) + "+".

DO li = 6 TO 10:
  v-label[li] = FILL(" ",8 - LENGTH(TRIM(v-label[li]))) + TRIM(v-label[li]).
END.
            
if v-curr then
  assign
   v-label[01] = v-label[01]
   v-label[02] = v-label[02]
   v-label[03] = v-label[03]
   v-label[04] = v-label[04]
   v-label[05] = v-label[05]
   v-label[11] = "--------"
   v-label[12] = "--------"
   v-label[13] = "--------"
   v-label[14] = "--------"
   v-label[15] = "--------".
       
else
  assign
   v-label[01] = "        "
   v-label[02] = "        "
   v-label[03] = "        "
   v-label[04] = v-label[04]
   v-label[05] = v-label[05]
   v-label[11] = "        "
   v-label[12] = "        "
   v-label[13] = "        "
   v-label[14] = "--------"
   v-label[15] = "--------".

IF v-cost THEN do:
form header
     "As of:"
     vdat
     skip(1)

    /* "Salesperson:"
     tt-sman 
     lv-sname */                           skip

     "Sls"
     "            "
     "               "
     "                              "     
     v-label[01]
     v-label[02]
     v-label[03]
     v-label[04]
     v-label[05]
     v-label1[1]
     v-label1[2]
     v-label1[7]                        skip

     "Rep"
     "Customer    "
     "Item #         "
     "Description                   "     
     v-label[06]
     v-label[07]
     v-label[08]
     v-label[09]
     v-label[10]
     v-label1[3]
     v-label1[4]
     v-label3[1]  
     v-label1[10]
     v-label1[12]                       skip

     "---"
     "------------"
     "---------------"
     "------------------------------"     
     v-label[11]
     v-label[12]
     v-label[13]
     v-label[14]
     v-label[15]          
     v-label1[5]
     v-label1[6]
     v-label3[2]
     v-label1[11]
     v-label1[13]                       skip

     skip(1)

    with frame r-top WIDTH 200.
END.
ELSE DO:                                                /*Task# 02041406*/
    form header
     "As of:"
     vdat
     skip(1)

    /* "Salesperson:"
     tt-sman 
     lv-sname */                           skip

     "Sls"
     "            "
     "               "
     "                              "     
     v-label[01]
     v-label[02]
     v-label[03]
     v-label[04]
     v-label[05]
     v-label1[1]
     v-label1[2]
     v-label1[7]                        skip

     "Rep"
     "Customer    "
     "Item #         "
     "Description                   "     
     v-label[06]
     v-label[07]
     v-label[08]
     v-label[09]
     v-label[10]
     v-label1[3]
     v-label3[1] 
     v-label1[8]  
     v-label1[10]
     v-label1[12]                       skip

     "---"
     "------------"
     "---------------"
     "------------------------------"     
     v-label[11]
     v-label[12]
     v-label[13]
     v-label[14]
     v-label[15]          
     v-label1[5]
     v-label3[2]
     v-label1[9]
     v-label1[11]
     v-label1[13]                       skip

     skip(1)

    with frame r-top2 WIDTH 200.
END.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

/*
DO WITH FRAME {&FRAME-NAME}:
  list_class:SCREEN-VALUE = v-class.
  ASSIGN list_class.
END.*/

/*READKEY PAUSE 0.*/
     
/*SESSION:SET-WAIT-STATE ("general").*/

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Salesperson,Customer,Item #,Description," + TRIM(v-label[1]) + " "
               + TRIM(v-label[6]) + "," + TRIM(v-label[2]) + " " + TRIM(v-label[7])
               + "," + TRIM(v-label[3]) + " " + TRIM(v-label[8]) + ","
               + TRIM(v-label[4]) + " " + TRIM(v-label[9]) + "," + TRIM(v-label[5])
               + " " + TRIM(v-label[10]) + "," + TRIM(v-label1[1]) + " "
               + TRIM(v-label1[3]) + "," + TRIM(v-label1[2]) + " " + TRIM(v-label1[4])
               + "," + TRIM(v-label1[7]) + " " + TRIM(v-label1[8]) + "," + TRIM(v-label1[10]) 
               + "," + TRIM(v-label1[12]).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END. 

/*{fgrep/r-ageinv.i}*/

FOR EACH tt-file.
    IF tt-file.tt-cust = "" THEN DO:
        FIND FIRST cust WHERE cust.company = cocode NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
            ASSIGN tt-file.tt-cust = cust.cust-no .
        IF AVAIL cust AND tt-file.tt-sman EQ "" THEN
            ASSIGN tt-file.tt-sman = s-man[1].
    END.
END.

for each tt-file WHERE
    (tt-qohi[1] NE 0 OR
    tt-qohi[2] NE 0 OR
    tt-qohi[3] NE 0 OR
    tt-qohi[4] NE 0 OR
    tt-qohi[5] NE 0)
/*    AND (tt-qohi[1] + tt-qohi[2] + tt-qohi[3] + tt-qohi[4] + tt-qohi[5]) GT
         0 */
    AND NOT (NOT v-curr AND (tt-qohi[1] + tt-qohi[2] + tt-qohi[3]) GT 0)
    ,    
    first cust
    where cust.company eq cocode
      and cust.cust-no eq tt-cust-no
    no-lock,    
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq tt-i-no
    NO-LOCK    
    break by tt-sman
          by tt-cust-no
          /*by tt-i-no*/
          by (IF sort-opt EQ "I" THEN itemfg.i-no ELSE  itemfg.part-no ):

  FIND FIRST sman NO-LOCK
      WHERE sman.company EQ cocode
        AND sman.sman    EQ tt-sman
      NO-ERROR.

  lv-sname = IF AVAIL sman AND sman.sname NE "" THEN sman.sname ELSE
             IF cust.sman EQ "" THEN "No Sales Rep Name" ELSE "Not on File".
  
  if first-of(tt-sman)                  or
     (first-of(tt-cust-no) and v-break) then do:
    IF FIRST(tt-sman) THEN do: 
        IF v-cost THEN DISPLAY WITH FRAME r-top.
        ELSE DISPLAY WITH FRAME r-top2.
    END.
    /* ELSE page. 01031332 no longer breaking */
  END.
/*if first-of(tt-cust-no) then*/ v-cus = cust.name.

  FIND LAST fg-rcpth NO-LOCK 
      WHERE fg-rcpth.company EQ cocode 
        AND fg-rcpth.i-no EQ itemfg.i-no 
        AND fg-rcpth.rita-code EQ "S" NO-ERROR.

  ASSIGN
   v-cst[1]  = tt-cst[1]
   v-val[1]  = tt-val[1]
   v-qohi[1] = tt-qohi[1]
   v-qohi[2] = tt-qohi[2]
   v-qohi[3] = tt-qohi[3]
   v-qohi[4] = tt-qohi[4]
   v-qohi[5] = tt-qohi[5]

   tt-days = tt-file.tt-days
   lv-last-fld = IF rd_show2 BEGINS "Day" THEN STRING(tt-days,">>>9")
                                               ELSE ""
   v-shipdt = IF AVAIL fg-rcpth AND v-sdate THEN STRING(fg-rcpth.trans-date) 
                                               ELSE "" .  

   RUN reduce_negatives.

    IF NOT v-q-or-v /* AND rd_price = "Sell" */ THEN
        v-val[1] = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].

    
    IF v-q-or-v /* fix to avg sell  AND rd_price = "Sell" */ THEN
        v-val[1] = tt-sell-price[1] + 
                   tt-sell-price[2] + 
                   tt-sell-price[3] + 
                   tt-sell-price[4] + 
                   tt-sell-price[5].

    RUN SET_column_values.

IF v-cost THEN do:
  display /* 11091201 - sales man on each line */
          tt-sman  FORMAT "x(3)"
          v-cus                   format "x(12)"
          itemfg.i-no             format "x(15)"
          itemfg.i-name           format "x(30)"
          v-qohi[1] when v-curr   format "->>>>>>9"
          v-qohi[2] when v-curr   format "->>>>>>9"
          v-qohi[3] when v-curr   format "->>>>>>9"
          v-qohi[4]               format "->>>>>>9"
          v-qohi[5]               format "->>>>>>9"
          v-cst[1]  when v-cost   format "->,>>>,>>9.99"
              v-val[1] when not v-cost @ v-cst[1]
          lv-last-fld[1] FORMAT "x(4)" 
              v-val[1] when v-cost     @ lv-last-fld[1]
          SPACE(1)
          /*lv-comm-tail WHEN rd_show2 BEGINS "COM"*/
          lv-last-fld[2] FORMAT "x(4)"                     /*Task# 02041406*/
          v-custpart[1] WHEN  v-cpart format "x(13)" 
          v-shipdt[1]  WHEN NOT v-cpart @ v-custpart[1]
          v-shipdt[2] WHEN v-cost
          /* 11091201 - customer name on 2nd line */
          SKIP          
      with frame detail no-box no-labels no-attr-space stream-io width 200 down.
  down with frame detail.
END.
ELSE DO:
    display /* 11091201 - sales man on each line */
          tt-sman  FORMAT "x(3)"
          v-cus                   format "x(12)"
          itemfg.i-no             format "x(15)"
          itemfg.i-name           format "x(30)"
          v-qohi[1] when v-curr   format "->>>>>>9"
          v-qohi[2] when v-curr   format "->>>>>>9"
          v-qohi[3] when v-curr   format "->>>>>>9"
          v-qohi[4]               format "->>>>>>9"
          v-qohi[5]               format "->>>>>>9"
          v-cst[1]  when v-cost   format "->,>>>,>>9.99"
              v-val[1] when not v-cost @ v-cst[1] 
          lv-last-fld[1] FORMAT "x(4)" 
          SPACE(1)
          lv-last-fld[2] FORMAT "x(13)"                 /*Task# 02041406*/
          v-custpart[1] WHEN  v-cpart format "x(13)" 
          v-shipdt[1]  WHEN NOT v-cpart @ v-custpart[1]
          v-shipdt[2] WHEN v-cost
          /* 11091201 - customer name on 2nd line */
          SKIP          
      with frame detail2 no-box no-labels no-attr-space stream-io width 200 down.
  down with frame detail2.
END.


  IF tb_excel THEN 
     PUT STREAM excel UNFORMATTED
         '"' tt-sman + " " + lv-sname                              '",'
         '"' v-cus                                                 '",'
         '"' itemfg.i-no                                           '",'
         '"' itemfg.i-name                                         '",'
         '"' (IF v-curr THEN STRING(v-qohi[1],"->>>>>>9") ELSE "") '",'
         '"' (IF v-curr THEN STRING(v-qohi[2],"->>>>>>9") ELSE "") '",'
         '"' (IF v-curr THEN STRING(v-qohi[3],"->>>>>>9") ELSE "") '",'
         '"' STRING(v-qohi[4],"->>>>>>9")                          '",'
         '"' STRING(v-qohi[5],"->>>>>>9")                          '",'
         '"' (IF v-cost THEN STRING(v-cst[1],"->,>>>,>>9.99")
              ELSE STRING(v-val[1],"->,>>>,>>9.99"))               '",'
         '"' (IF v-cost THEN STRING(v-val[1],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
         '"'  lv-last-fld[2]        '",'
         '"' (IF v-cpart  THEN  v-custpart[1] ELSE v-shipdt[1] )                             '",'
         '"' ( IF  v-sdate AND  v-cost  THEN  v-shipdt[2] ELSE "" )            '",'
         SKIP.

  ASSIGN
   v-cst[2]  = v-cst[2]  + v-cst[1]
   v-val[2]  = v-val[2]  + v-val[1]
   v-qohc[1] = v-qohc[1] + v-qohi[1]
   v-qohc[2] = v-qohc[2] + v-qohi[2]
   v-qohc[3] = v-qohc[3] + v-qohi[3]
   v-qohc[4] = v-qohc[4] + v-qohi[4]
   v-qohc[5] = v-qohc[5] + v-qohi[5]

   v-cus            = ""
   v-cst[1]         = 0
   v-val[1]         = 0
   v-sell-price     = 0
   v-qohi           = 0      
   lv-last-fld      = "".
  
  if last-of(tt-cust-no) then do:
    if v-sub-t then do:
      display "            Customer Subtotal:"  @ itemfg.i-name
              v-qohc[1] when v-curr             @ v-qohi[1]
              v-qohc[2] when v-curr             @ v-qohi[2]
              v-qohc[3] when v-curr             @ v-qohi[3]
              v-qohc[4]                         @ v-qohi[4]
              v-qohc[5]                         @ v-qohi[5]
              v-cst[2]  when v-cost             @ v-cst[1]
                  v-val[2] when not v-cost      @ v-cst[1]
              lv-last-fld[1]
                  v-val[2] when v-cost          @ lv-last-fld[1]

          with frame detail.
      down with frame detail.

      IF tb_excel THEN 
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' ""                          '",'
             '"' ""                          '",'
             '"' ""                          '",'
             '"' "            Customer Subtotal:"                      '",'
             '"' (IF v-curr THEN STRING(v-qohc[1],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohc[2],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohc[3],"->>>>>>9") ELSE "") '",'
             '"' STRING(v-qohc[4],"->>>>>>9")                          '",'
             '"' STRING(v-qohc[5],"->>>>>>9")                          '",'
             '"' (IF v-cost THEN STRING(v-cst[2],"->,>>>,>>9.99")
                  ELSE STRING(v-val[2],"->,>>>,>>9.99"))               '",'
             '"' (IF v-cost THEN STRING(v-val[2],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
             SKIP(1).
    end.
    
    assign
     v-cst[3]  = v-cst[3]  + v-cst[2]
     v-val[3]  = v-val[3]  + v-val[2]
     v-qohs[1] = v-qohs[1] + v-qohc[1]
     v-qohs[2] = v-qohs[2] + v-qohc[2]
     v-qohs[3] = v-qohs[3] + v-qohc[3]
     v-qohs[4] = v-qohs[4] + v-qohc[4]
     v-qohs[5] = v-qohs[5] + v-qohc[5]

     v-cst[2] = 0
     v-val[2] = 0
     v-qohc   = 0.
  end.

  if last-of(tt-sman) then do:
    IF  v-print-sls-tots THEN DO:
    /* 11091201 - option to supress sales rep totals */
    display "         Salesperson Subtotal:"    @ itemfg.i-name
            v-qohs[1] when v-sub-t and v-curr   @ v-qohi[1]
            v-qohs[2] when v-sub-t and v-curr   @ v-qohi[2]
            v-qohs[3] when v-sub-t and v-curr   @ v-qohi[3]
            v-qohs[4] when v-sub-t              @ v-qohi[4]
            v-qohs[5] when v-sub-t              @ v-qohi[5]
            v-cst[3]  when v-cost               @ v-cst[1]
            v-val[3] when not v-cost        @ v-cst[1]
            lv-last-fld[1]
            v-val[3] when v-cost            @ lv-last-fld[1]

        with frame detail.
    down with frame detail.

    IF tb_excel THEN 
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                          '",'
           '"' ""                          '",'
           '"' ""                          '",'
           '"' "         Salesperson Subtotal:"                      '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[1],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[2],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[3],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t THEN STRING(v-qohs[4],"->>>>>>9") ELSE "")                         '",'
           '"' (IF v-sub-t THEN STRING(v-qohs[5],"->>>>>>9") ELSE "")                         '",'
           '"' (IF v-cost THEN STRING(v-cst[3],"->,>>>,>>9.99")
                ELSE STRING(v-val[3],"->,>>>,>>9.99"))               '",'
           '"' (IF v-cost THEN STRING(v-val[3],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
           SKIP(1).
    END.

    assign
     v-cst[4]  = v-cst[4]  + v-cst[3]
     v-val[4]  = v-val[4]  + v-val[3]
     v-qohg[1] = v-qohg[1] + v-qohs[1]
     v-qohg[2] = v-qohg[2] + v-qohs[2]
     v-qohg[3] = v-qohg[3] + v-qohs[3]
     v-qohg[4] = v-qohg[4] + v-qohs[4]
     v-qohg[5] = v-qohg[5] + v-qohs[5]

     v-cst[3] = 0
     v-val[3] = 0
     v-qohs   = 0.
  end.
             DEF VAR v-all-tot AS DEC.
  /* 11091201 */
  if last(tt-sman) AND v-print-grand-tots then do:
    display "                  Grand Total:"    @ itemfg.i-name
            v-qohg[1]  @ v-qohi[1]
            v-qohg[2]  @ v-qohi[2]
            v-qohg[3]  @ v-qohi[3]
            v-qohg[4]  @ v-qohi[4]
            v-qohg[5]  @ v-qohi[5]
            v-cst[4]  when v-cost           @ v-cst[1]
            v-val[4] when not v-cost        @ v-cst[1]
            lv-last-fld[1]
            v-val[4] when v-cost            @ lv-last-fld[1]

        with frame detail.
    down with frame detail.

    v-all-tot = v-qohg[1] + v-qohg[2] + v-qohg[3] + v-qohg[4] + v-qohg[5].
    IF v-all-tot EQ 0 THEN
        v-all-tot = 1.
     DISPLAY
     "             % of Grand Total:"    @ itemfg.i-name 
            string(v-qohg[1] / v-all-tot * 100, "->>9.99%")  @ v-qohi[1] 
            string(v-qohg[2] / v-all-tot * 100, "->>9.99%")  @ v-qohi[2]
            string(v-qohg[3] / v-all-tot * 100, "->>9.99%")  @ v-qohi[3]
            string(v-qohg[4] / v-all-tot * 100, "->>9.99%")  @ v-qohi[4]
            string(v-qohg[5] / v-all-tot * 100, "->>9.99%")  @ v-qohi[5]

        with frame detail.
    down with frame detail.

    IF tb_excel THEN 
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                          '",'
           '"' ""                          '",'
           '"' ""                          '",'
           '"' "                  Grand Total:"                      '",'
           '"' STRING(v-qohg[1],"->>>>>>9") '",'
           '"' STRING(v-qohg[2],"->>>>>>9") '",'
           '"' STRING(v-qohg[3],"->>>>>>9") '",'
           '"' STRING(v-qohg[4],"->>>>>>9")                         '",'
           '"' STRING(v-qohg[5],"->>>>>>9")                         '",'
           '"' (IF v-cost THEN STRING(v-cst[4],"->,>>>,>>9.99")
                ELSE STRING(v-val[4],"->,>>>,>>9.99"))               '",'
           '"' (IF v-cost THEN STRING(v-val[4],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
           SKIP(1).
  end.
  
  delete tt-file.
end. /* each tt-file */

 IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
 END.


end procedure.


PROCEDURE produce-report :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
  ------------------------------------------------------------------------------*/
  /* fgrep/r-ageinv.i
  
  Notes:
  - tt-file allows for break by  salesrep, customer, item
  - tt-fg-bin allows for break by tag, job, loc, bin, i-no
  
  */
  
  DEF VAR v-rec-date AS DATE NO-UNDO.
  DEF VAR l-first-bin AS LOG NO-UNDO.
  DEF VAR tot-qty AS INT NO-UNDO.
  DEF VAR tot-val AS DEC NO-UNDO.
  DEF VAR AVG-price AS DEC NO-UNDO.
  DEF VAR v-bin-price AS DEC NO-UNDO.
  DEF VAR v-price-uom AS CHAR NO-UNDO.
  DEF VAR v-null AS DEC NO-UNDO.
  DEF VAR v-ord-slsmn AS CHAR NO-UNDO.
  DEF VAR floc AS CHAR NO-UNDO.
  DEF VAR tloc AS CHAR NO-UNDO.
  DEF VAR floc-bin AS CHAR NO-UNDO.
  DEF VAR tloc-bin AS CHAR NO-UNDO.
  DEF VAR lvlIncludeOld AS LOG NO-UNDO.
  DEF VAR v-sales-rep AS CHAR NO-UNDO.
 ASSIGN
 vdat      = as-of-date
 fslm      = begin_slm
 tslm      = END_slm
 fcus      = begin_cust-no
 tcus      = end_cust-no
 fitm      = begin_i-no
 titm      = end_i-no
 floc       = begin_whse
 tloc       = END_whse
 fjob      = fill(" ",6 - length(trim(begin_job-no))) +
                trim(begin_job-no) + string(int(begin_job-no2),"99")
 tjob      = fill(" ",6 - length(trim(end_job-no)))   +
                trim(end_job-no)   + string(int(end_job-no2),"99") 
 v-q-or-v  = rd_show EQ "Quantity"
 v-sub-t   = tb_val-cust
 v-break   = tb_break
 v-cost    = tb_cost
 v-curr    = tb_curr
 v-cpart   = tb_custpart
 v-sdate   = tb_last-ship-date

 v-label    = string(v-q-or-v,"    Qty/  Value")
 v-class    = list_class
 list_class = ""
 sort-opt    = SUBSTR(rd_sort,1,1) .

DO li = 1 TO NUM-ENTRIES(v-class):
  list_class = list_class + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
                                   ELSE ENTRY(li,v-class)).
END.
list_class = TRIM(list_class).

  /*ASSIGN
    vdat       = as-of-date
    fslm       = begin_slm
    tslm       = END_slm
    fcus       = begin_cust-no
    tcus       = end_cust-no
    fitm       = begin_i-no
    titm       = end_i-no
    /*floc       = begin_whse
    tloc       = END_whse
    floc-bin   = begin_loc-bin
    tloc-bin   = end_loc-bin*/
    fjob       = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
  TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
    tjob       = FILL(" ",6 - LENGTH(TRIM(end_job-no))) +
  TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
    v-q-or-v   = rd_show EQ "Quantity"
    v-sub-t    = tb_val-cust
    v-break    = tb_break
    v-cost     = tb_cost
    v-curr     = tb_curr
    v-cpart    = tb_custpart
    v-sdate    = tb_last-ship-date
    v-label    = STRING(v-q-or-v," Qty/ Value")
    v-class    = list_class
    list_class = ""
    sort-opt   = SUBSTR(rd_sort,1,1) 
    lvlIncludeOld = tb_include_old_items.
  
  DO li = 1 TO NUM-ENTRIES(v-class):
    list_class = list_class + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
    ELSE ENTRY(li,v-class)).
  END.
  list_class = TRIM(list_class).
  STATUS DEFAULT "Processing...".*/
  EMPTY TEMP-TABLE tt-file.
  EMPTY TEMP-TABLE tt-fg-bin.
  EMPTY TEMP-TABLE tt-itemfg.

  
  ASSIGN
  v-cst[1]     = 0
  v-val[1]     = 0
  v-dates      = ?
  ld-last      = ?
  v-qohi       = 0
  v-tot-for-item = 0
  v-sell-price = 0
  ld-last      = 01/01/0001.
  
  EMPTY TEMP-TABLE tt-fg-bin.
  EMPTY TEMP-TABLE tt-items.

  
  IF fcus EQ "" AND fslm EQ "" AND tcus BEGINS "zzz" 
       AND tslm BEGINS "zzz" THEN DO:
      FOR EACH itemfg NO-LOCK
        WHERE itemfg.company        EQ cocode
          AND itemfg.i-no           GE fitm
          AND itemfg.i-no           LE titm
          AND (itemfg.stat EQ "A" OR lvlIncludeOld)
        USE-INDEX i-no.

/*         /* Check for active status */                                                */
/*         IF NOT lvlIncludeOld AND itemStatus(itemfg.company, itemfg.i-no) NE "A" THEN */
/*             NEXT.                                                                    */
/*                                                                                      */

        CREATE tt-items.
        ASSIGN tt-items.i-no = itemfg.i-no.

      END.

  END.
  ELSE DO:
      FOR EACH cust NO-LOCK
        WHERE cust.company          EQ cocode
        AND cust.cust-no          GE fcus
        AND cust.cust-no          LE tcus
        AND cust.sman             GE fslm
        AND cust.sman             LE tslm,

        EACH itemfg NO-LOCK
          WHERE itemfg.company        EQ cust.company
            AND itemfg.cust-no        EQ cust.cust-no
            AND itemfg.i-no           GE fitm
            AND itemfg.i-no           LE titm
            AND (itemfg.stat EQ "A" OR lvlIncludeOld)
            USE-INDEX customer:

/*           /* Check for active status */                                                */
/*           IF NOT lvlIncludeOld AND itemStatus(itemfg.company, itemfg.i-no) NE "A" THEN */
/*               NEXT.                                                                    */

          CREATE tt-items.
          ASSIGN tt-items.i-no = itemfg.i-no.

      END.
  END.

  FOR EACH tt-items,
      FIRST itemfg WHERE itemfg.company = cocode
                     AND itemfg.i-no = tt-items.i-no
                   NO-LOCK:
    
    IF v-class NE "" AND
    LOOKUP(itemfg.class,v-class) EQ 0 THEN NEXT.


/********************** Start section copied from IR2 *******************/

    /* Don't know what these passed values should be */
    
    DEF VAR v-loc LIKE fg-bin.loc EXTENT 2 . /* begin/end warehouse */
    DEF VAR v-loc-bin LIKE fg-bin.loc-bin EXTENT 2. /* begin/end bin */
    DEF VAR zbal AS LOG INIT NO. /* include zero balances */
    DEF VAR fi_days-old AS INT INIT 0. /* periods to report, 0 matches IR2 */
    DEF VAR v-custown AS LOG .  /* include cust owned */.

    v-custown = tb_cust-whse.

      ASSIGN  v-loc[1]     = floc /* "" */        v-loc[2] = tloc /* "zzzzzzzzzz" */
              v-loc-bin[1] = floc-bin /* "" */    v-loc-bin[2] = "zzzzzzzz" /*tloc-bin*/ /* "zzzzzzzzzzzzzz" */.
      ASSIGN
        age-days[1] = aged-days-1
        age-days[2] = aged-days-2
        age-days[3] = aged-days-3
        age-days[4] = aged-days-4.
 
      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
                             zbal, fi_days-old, YES, v-custown).
                      
      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
          /*     wfk - 10/1- don't know why commented out */
         /*   AND (v-custown OR tb_cust-whse OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST")) */
           /* Copied from IR2 but does not apply since there is no 'cust whse only' */
           AND (  tb_cust-whse OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))   
          USE-INDEX co-ino
          BREAK BY tt-fg-bin.i-no:
   
        RUN price-calc (INPUT ROWID(tt-fg-bin), OUTPUT v-bin-price, OUTPUT v-price-uom).

        IF FIRST(tt-fg-bin.i-no)  THEN DO:            
            tt-fg-bin.spare-dec-1 = 0.
        END.

            /*
        RUN sys/ref/convcuom.p(v-price-uom, "EA", 0, 0, 0, 0,
        v-null, OUTPUT v-bin-price ). */
        
        IF v-price-uom = "M" THEN
            v-bin-price = v-bin-price / 1000.
        
        tt-fg-bin.spare-dec-1 = tt-fg-bin.spare-dec-1 + (v-bin-price * tt-fg-bin.qty).
        
        IF tt-fg-bin.qty NE 0 OR zbal THEN DO:
          CREATE tt-itemfg.
          BUFFER-COPY itemfg TO tt-itemfg
          ASSIGN
           tt-itemfg.row-id      = ROWID(itemfg)
           tt-itemfg.job-no      = tt-fg-bin.job-no
           tt-itemfg.job-no2     = tt-fg-bin.job-no2
           tt-itemfg.loc         = tt-fg-bin.loc
           tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
           tt-itemfg.tag         = tt-fg-bin.tag
           tt-itemfg.bin-cust-no = tt-fg-bin.cust-no
           tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)") +
                                   STRING(tt-itemfg.cust-no,"x(20)")
           tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")         +
                                   STRING(tt-itemfg.loc-bin,"x(10)")     +
                                   STRING(tt-itemfg.tag,"x(20)").
        END.
    
        ELSE DELETE tt-fg-bin.
     END. /* each tt-fg-bin */

  END. /* each cust ... */

/********************** End section copied from IR2 *******************/
  DEF VAR v-max-days AS INT NO-UNDO.
  v-max-days = 0.
  /* Per IR2 code, now ready to report based on tt-fg-bin and tt-itemfg */
  for each tt-itemfg use-index cust-no no-lock,
      first itemfg where rowid(itemfg) eq tt-itemfg.row-id no-lock,
         each tt-fg-bin
        where tt-fg-bin.company eq itemfg.company
          and tt-fg-bin.i-no    eq itemfg.i-no
          and tt-fg-bin.job-no  eq tt-itemfg.job-no
          and tt-fg-bin.job-no2 eq tt-itemfg.job-no2
          and tt-fg-bin.loc     eq tt-itemfg.loc
          and tt-fg-bin.loc-bin eq tt-itemfg.loc-bin
          and tt-fg-bin.tag     eq tt-itemfg.tag
          and tt-fg-bin.cust-no eq tt-itemfg.bin-cust-no
        use-index co-ino NO-LOCK
      break by tt-itemfg.cust-no
            by tt-itemfg.i-no
            BY tt-itemfg.loc
            BY tt-itemfg.loc-bin
            BY tt-itemfg.job-no
            BY tt-itemfg.job-no2
            :
        
        FIND FIRST cust WHERE cust.company EQ itemfg.company 
             AND cust.cust-no EQ itemfg.cust-no
             NO-LOCK NO-ERROR.
 
        IF NOT AVAIL cust THEN
            FIND FIRST cust WHERE cust.company = cocode NO-LOCK.
        
        DEF VAR v-buck AS INT.

        v-sales-rep = "" .
        IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:
          FOR EACH cust-part WHERE cust-part.company = itemfg.company   
              AND cust-part.i-no = itemfg.i-no
              AND cust-part.cust-no EQ cust.cust-no
               NO-LOCK, 
              FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
              AND reftable.company = cust-part.company  
              AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK:
        
              IF cust-part.spare-char-1 NE "" THEN do:
                  FIND FIRST sman WHERE sman.company = itemfg.company
                      AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                  IF AVAIL sman THEN v-sales-rep = sman.sman.
                  LEAVE .
              END.
           END. /* end of cust-part */
        
           IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
               FIND FIRST sman WHERE sman.company = cust.company
                   AND sman.sman = cust.sman NO-LOCK NO-ERROR.
               IF AVAIL sman THEN v-sales-rep = sman.sman.
           END.
        END.
        ELSE DO:
            FIND FIRST sman WHERE sman.company = cust.company
                AND sman.sman = cust.sman NO-LOCK NO-ERROR.
            IF AVAIL sman THEN v-sales-rep = sman.sman.
        END.
         
        /* change 9/24 */
        IF FIRST-OF(tt-itemfg.i-no) THEN 
            ASSIGN v-qohi = 0.

        lv-rct-date = tt-fg-bin.first-date.
        
        v-buck = IF vdat - lv-rct-date LT aged-days-1 THEN 1 ELSE
                 IF vdat - lv-rct-date LT aged-days-2 THEN 2 ELSE
                 IF vdat - lv-rct-date LT aged-days-3 THEN 3 ELSE
IF                  vdat - lv-rct-date LT aged-days-4 THEN 4 ELSE 5.
        
        v-qohi[v-buck] = v-qohi[v-buck] + tt-fg-bin.qty.
        

        v-qty-2     = v-qohi[1] + v-qohi[2] + v-qohi[3] +
                      v-qohi[4] + v-qohi[5] + v-qohi[6].

        v-sell-price[v-buck] = v-sell-price[v-buck] + tt-fg-bin.spare-dec-1.
        
        IF itemfg.prod-uom EQ "EA" THEN
        v-u-cst = tt-fg-bin.std-tot-cost.
        ELSE
        RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                               tt-fg-bin.std-tot-cost, OUTPUT v-u-cst).

        FIND LAST oe-ordl WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                    AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                    AND oe-ordl.i-no    EQ tt-fg-bin.i-no
                    AND tt-fg-bin.job-no GT ""
        USE-INDEX ITEM 
        NO-LOCK NO-ERROR.

        IF tt-fg-bin.job-no GT "" THEN DO:
            FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                                 AND job-hdr.job-no EQ tt-fg-bin.job-no
                                 AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                               NO-LOCK NO-ERROR.
            IF AVAIL job-hdr THEN
            DO:
              FIND FIRST oe-ordl WHERE
              oe-ordl.company EQ job-hdr.company AND
              oe-ordl.ord-no  EQ job-hdr.ord-no AND
              oe-ordl.i-no    EQ job-hdr.i-no
              NO-LOCK NO-ERROR.
              RELEASE job-hdr.
            END.
        END.
        IF NOT AVAIL oe-ordl THEN DO:
                    FIND LAST oe-ordl WHERE oe-ordl.company EQ cocode
                                AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                                AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                                AND oe-ordl.i-no    EQ tt-fg-bin.i-no
                    USE-INDEX ITEM 
                    NO-LOCK NO-ERROR.
        

        END.
        IF avail oe-ordl THEN DO:

          ASSIGN v-u-val  = oe-ordl.t-price / oe-ordl.qty
                 lv-case-count = oe-ordl.cas-cnt.
          /* Always blank for now unless they make it a mod */
          v-ord-slsmn = "" /* oe-ordl.s-man[1] */.
        END.

        ELSE DO:
          lv-case-count = itemfg.case-count.
          
          IF itemfg.sell-uom EQ "EA" THEN
            v-u-val = itemfg.sell-price.
          ELSE
          IF itemfg.sell-uom = "CS" AND lv-case-count <> 0 THEN
            v-u-val = itemfg.sell-price / lv-case-count.
          ELSE
            RUN sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                   itemfg.sell-price, OUTPUT v-u-val).

        END.

        IF v-u-cst EQ ? THEN v-u-cst = 0.
        IF v-u-val EQ ? THEN v-u-val = 0.
        
        IF NOT tb_neg-sale OR (v-qty-2 * v-u-val) GT 0 THEN
        ASSIGN v-cst[1] = v-cst[1] + (tt-fg-bin.qty * v-u-cst)
               v-val[1] = v-val[1] + (tt-fg-bin.qty * v-u-val).

      /* End Code from r-ageinv.w before create of tt-file */
      IF vdat - tt-fg-bin.first-date GT v-max-days 
            AND tt-fg-bin.qty NE 0 /* 9/18 - wfk - trying this since was wrong */
          THEN DO:
          
          v-max-days = vdat - tt-fg-bin.first-date.
      END.
      IF LAST-OF(tt-itemfg.i-no) THEN DO:
            
          /* Hopefully these are calculated the same way as in 
             the original IR12 */

         /* Conversion of quantity to value */
        /* change 9/24 moved from other section since v-sell-price wasn't available there */
        IF NOT v-q-or-v THEN DO:
         v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
      
         DO v = 1 TO 5:
        
            IF rd_price = "Avg" THEN
/*              v-qohi[v] = v-val[1] / v-qty * v-qohi[v]. */
                .
            ELSE 
              v-qohi[v] = v-sell-price[v].
            IF v-qohi[v] EQ ? THEN v-qohi[v] = 0.
         END.
        END.
         
         CREATE tt-file.
          ASSIGN
            tt-file.tt-sman    = (IF v-ord-slsmn GT "" THEN v-ord-slsmn ELSE v-sales-rep)
            tt-file.tt-cust-no = cust.cust-no
            tt-file.tt-i-no    = itemfg.i-no
            tt-file.tt-cst[1]  = v-cst[1]
            tt-file.tt-val[1]  = (IF v-qohi[1] NE 0 OR v-qohi[2] NE 0 OR v-qohi[3] NE 0 OR v-qohi[4] NE 0 OR v-qohi[5] NE 0 THEN v-val[1] ELSE 0)
            tt-file.tt-qohi[1] = v-qohi[1]
            tt-file.tt-qohi[2] = v-qohi[2]
            tt-file.tt-qohi[3] = v-qohi[3]
            tt-file.tt-qohi[4] = v-qohi[4]
            tt-file.tt-qohi[5] = v-qohi[5]
            tt-file.tt-days    = v-max-days /* vdat - tt-fg-bin.first-date */
            tt-file.tt-sell-price[1] = v-sell-price[1]
            tt-file.tt-sell-price[2] = v-sell-price[2]
            tt-file.tt-sell-price[3] = v-sell-price[3]
            tt-file.tt-sell-price[4] = v-sell-price[4]
            tt-file.tt-sell-price[5] = v-sell-price[5]
            .
 

         ASSIGN v-ord-slsmn = "" 
                    tot-qty = 0
                    tot-val = 0
                  avg-price = 0.

         IF  /* NOT v-q-or-v AND */ rd_price = "AVG" THEN DO:
             FOR EACH bf-tt-fg-bin WHERE bf-tt-fg-bin.i-no = tt-file.tt-i-no
                                    NO-LOCK.
                 tot-qty = tot-qty + bf-tt-fg-bin.qty.
                 tot-val = tot-val + bf-tt-fg-bin.spare-dec-1.
             END.
             IF tot-qty NE 0 THEN
               avg-price = tot-val / tot-qty.
             DO v = 1 TO 5:
                 IF NOT v-q-or-v  THEN
                   tt-file.tt-qohi[v] =  avg-price * tt-file.tt-qohi[v].
                 ELSE
                     /* test for fixing avg value */
                   tt-file.tt-sell-price[v] = avg-price * tt-file.tt-qohi[v].
             END.


         END.
            ASSIGN 
                v-cst = 0
                v-val = 0
                v-qohi = 0
                v-max-days = 0
                v-sell-price = 0.

      END.
  END. /* each tt-itemfg */

END PROCEDURE.


PROCEDURE price-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Copied directly from fg-ibtag.i      
------------------------------------------------------------------------------*/
DEF BUFFER bf-tt-fg-bin FOR tt-fg-bin.
DEF INPUT PARAMETER ipr-tt-fg-bin-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opd-sell-price   AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opc-price-uom    AS CHAR NO-UNDO.

DEF VAR v-first AS LOG EXTENT 2.
DEF VAR v-tot-sum AS DEC NO-UNDO.
DEF VAR v-ext-sum AS DEC NO-UNDO.
DEF VAR v-qoh AS DEC NO-UNDO.
DEF VAR v-procat AS CHAR NO-UNDO.
DEF VAR v-bin AS LOG NO-UNDO.
DEF VAR v-tot-bin-sum AS DEC NO-UNDO.
DEF VAR v-ext-bin-sum AS DEC NO-UNDO.
DEF VAR v-bin-qoh AS DEC NO-UNDO.
DEF VAR v-bin-arq AS DEC NO-UNDO.
DEF VAR v-sort-by-cust AS CHAR NO-UNDO.

DEF VAR v-costl AS DEC NO-UNDO.
DEF VAR v-cost1 AS DEC NO-UNDO.
DEF VAR v-costm AS DEC NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-summ-bin AS LOG NO-UNDO.
DEF VAR v-ext AS DEC NO-UNDO.
DEF VAR v-found-job AS LOG NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom   LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-case-count LIKE itemfg.case-count NO-UNDO.
DEF VAR fg-lot-val AS CHAR NO-UNDO.
DEF VAR v-dl-mat AS LOG NO-UNDO.
DEF VAR v-fgprice AS LOG NO-UNDO.
DEF VAR v-prt-msf AS LOG NO-UNDO.

DEF VAR v-tot-qty AS DEC EXTENT 10 NO-UNDO.
DEF VAR v-tot-cst AS DEC EXTENT 10 NO-UNDO.
DEF VAR v-tot-ext AS DEC EXTENT 10 NO-UNDO.

FIND bf-tt-fg-bin WHERE ROWID(bf-tt-fg-bin) EQ ipr-tt-fg-bin-row NO-LOCK NO-ERROR.


    do:
      assign
       v-first[1] = yes
       v-tot-sum  = 0
       v-ext-sum  = 0
       v-qoh      = 0.

      if v-sort-by-cust eq "Wh" then v-first[2] = yes.
    end.

    assign
     v-procat = itemfg.procat
     v-bin    = no.

    IF v-summ-bin /* AND FIRST-OF(tt-itemfg.job-no2) */ THEN DO:
        ASSIGN v-tot-bin-sum  = 0
               v-ext-bin-sum  = 0
               v-bin-qoh      = 0
               v-bin-arq      = 0.
    END.  


      lv-rct-date = bf-tt-fg-bin.first-date.

      assign
       v-costl = bf-tt-fg-bin.std-lab-cost * bf-tt-fg-bin.qty
       v-costm = bf-tt-fg-bin.std-mat-cost * bf-tt-fg-bin.qty 
       v-cost1 = if v-dl-mat then (bf-tt-fg-bin.std-lab-cost + bf-tt-fg-bin.std-mat-cost)
                             else bf-tt-fg-bin.std-tot-cost
       v-cost  = v-cost1             * bf-tt-fg-bin.qty.

                                                  /* Calculate Cost */
      if bf-tt-fg-bin.pur-uom eq "CS" and bf-tt-fg-bin.case-count ne 0 then
        assign
         v-costl = v-costl / bf-tt-fg-bin.case-count
         v-costm = v-costm / bf-tt-fg-bin.case-count
         v-cost  = v-cost  / bf-tt-fg-bin.case-count.
      else
      if bf-tt-fg-bin.pur-uom eq "L" then
        assign
         v-costl = v-costl / bf-tt-fg-bin.qty
         v-costm = v-costm / bf-tt-fg-bin.qty
         v-cost  = v-costm / bf-tt-fg-bin.qty.
      else do:
        find first uom
            where uom.uom  eq itemfg.prod-uom
              and uom.mult ne 0
            no-lock no-error.
        if avail uom then
          assign
           v-costl = v-costl / uom.mult
           v-costm = v-costm / uom.mult
           v-cost  = v-cost  / uom.mult.
        else
          assign
           v-costl = v-costl / 1000
           v-costm = v-costm / 1000
           v-cost  = v-cost  / 1000.
      end.

      ASSIGN
       lv-sell-price = itemfg.sell-price
       lv-sell-uom   = itemfg.sell-uom
       lv-case-count = itemfg.case-count.

      IF bf-tt-fg-bin.po-no NE "" AND NOT v-fgprice THEN
      DO:
         FIND FIRST po-ordl WHERE
              po-ordl.company EQ bf-tt-fg-bin.company AND
              po-ordl.po-no EQ INT(bf-tt-fg-bin.po-no) AND
              po-ordl.i-no EQ bf-tt-fg-bin.i-no
              NO-LOCK NO-ERROR.

         IF AVAIL po-ordl THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ bf-tt-fg-bin.company AND
                 oe-ordl.ord-no EQ po-ordl.ord-no AND
                 oe-ordl.i-no EQ bf-tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
            
         END.
      END.

      ELSE IF TRIM(bf-tt-fg-bin.job-no) NE "" AND NOT v-fgprice THEN
      DO:
         v-found-job = NO.

         FOR EACH job-hdr FIELDS(ord-no company i-no)
             WHERE job-hdr.company EQ bf-tt-fg-bin.company
               AND job-hdr.job-no  EQ bf-tt-fg-bin.job-no
               AND job-hdr.job-no2 EQ bf-tt-fg-bin.job-no2
               AND job-hdr.i-no    EQ bf-tt-fg-bin.i-no
               AND job-hdr.ord-no  NE 0
             USE-INDEX job-no NO-LOCK,
             FIRST oe-ordl FIELDS(ord-no price pr-uom cas-cnt disc)
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no
               AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
             USE-INDEX item-ord NO-LOCK
             BY job-hdr.ord-no DESC:

           ASSIGN
            lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
            lv-sell-uom   = oe-ordl.pr-uom
            lv-case-count = oe-ordl.cas-cnt
            v-found-job = YES.
           
           LEAVE.
         END.

         IF v-found-job = NO THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ bf-tt-fg-bin.company AND
                 oe-ordl.job-no EQ bf-tt-fg-bin.job-no AND
                 oe-ordl.job-no2 EQ bf-tt-fg-bin.job-no2 AND
                 oe-ordl.i-no EQ bf-tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
      END.
                                         /* Calculate Selling Price */
      if lv-sell-uom eq "CS" and lv-case-count ne 0 then
        v-ext = (bf-tt-fg-bin.qty * lv-sell-price) / lv-case-count.
      else do:
        find first uom
            where uom.uom  eq lv-sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-ext = bf-tt-fg-bin.qty * lv-sell-price /
                (if avail uom then uom.mult else 1000).
      end.

      if itemfg.sell-uom eq "L" then
        if bf-tt-fg-bin.qty le 0 then v-ext = 0.
        else v-ext = lv-sell-price.
        
      v-ext = round(v-ext,2).  

      if v-costl eq ? then v-costl = 0.
      if v-costm eq ? then v-costm = 0.
      if v-cost  eq ? then v-cost  = 0.
      if v-ext   eq ? then v-ext   = 0.

      assign
       v-qoh     = bf-tt-fg-bin.qty
       v-tot-sum = if v-dl-mat then v-costl else v-cost
       v-ext-sum = if v-dl-mat then v-costm else v-ext.

      IF v-prt-msf THEN v-qoh = v-qoh * itemfg.t-sqft / 1000.

      ASSIGN
       v-bin-qoh = v-bin-qoh + v-qoh
       v-tot-bin-sum = v-tot-bin-sum + v-tot-sum
       v-ext-bin-sum = v-ext-bin-sum + v-ext-sum.

      assign
       v-tot-qty[1] = v-tot-qty[1] + v-qoh
       v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
       v-tot-ext[1] = v-tot-ext[1] + v-ext-sum.
      ASSIGN
       opd-sell-price = lv-sell-price
       opc-price-uom  = lv-sell-uom.

END PROCEDURE.

PROCEDURE reduce_negatives :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Reducing negative quantities */
    DO v-q = 1 TO 5:
      IF v-qohi[v-q] LT 0 THEN DO:
          /* If can wipe out the negative then move it to [6] */
          v-tot-positive = 0.
          DO v = 1 TO 5:
            IF v-qohi[v] GT 0 THEN
               v-tot-positive = v-tot-positive + v-qohi[v].
          END.

          IF v-tot-positive GE abs(v-qohi[v-q]) THEN
            ASSIGN v-qohi[6]   = v-qohi[6] + v-qohi[v-q]
                   v-qohi[v-q] = 0.
      END.


      IF v-qohi[6] LT 0 THEN DO:
    
        ASSIGN
         v-qty     = v-qohi[6] * -1
         v-qohi[6] = 0.

        DO v = 5 TO 1 BY -1:
          IF v-qohi[v] GT 0 THEN
            ASSIGN
             v-red     = MIN(v-qty,v-qohi[v])
             v-qohi[v] = v-qohi[v] - v-red
             v-qty     = v-qty     - v-red.

          IF v-qty LE 0 THEN LEAVE.
        END.

        IF v-qty GT 0 THEN v-qohi[6] = v-qohi[6] - v-qty.
    
      END.      

    END.
END PROCEDURE.

PROCEDURE set_column_values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT v-cost THEN DO:
        IF v-cpart AND v-sdate  THEN do: 
            ASSIGN 
                lv-last-fld[2]  = itemfg.part-no 
                v-custpart[1]   = v-shipdt[1] .
         END.
         ELSE IF v-cpart AND NOT v-sdate  THEN do:
             ASSIGN
                 lv-last-fld[2]  = itemfg.part-no
                 v-custpart[1] = "" .
         END.
         ELSE IF v-sdate THEN DO:
             ASSIGN 
                 lv-last-fld[2]  = v-shipdt[1]  
                 v-shipdt[1]     = ""
                 v-shipdt[2]     = ""  .
         END.
         ELSE IF NOT v-sdate and NOT v-cpart then DO:
             ASSIGN
                 lv-last-fld[2] = "".
         END.
    END.

    IF v-cost THEN DO:
        IF v-cpart AND v-sdate THEN DO:
            ASSIGN
                v-custpart = itemfg.part-no .
        END.
        ELSE IF v-cpart AND NOT v-sdate THEN DO:
            ASSIGN
                v-custpart = itemfg.part-no .
        END.
        ELSE IF NOT v-cpart AND v-sdate THEN do:
            ASSIGN
                v-custpart  = v-shipdt[1]  
                v-shipdt[2] = "".
        END.
    END.

END PROCEDURE.

PROCEDURE which-bucket PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-days AS INT NO-UNDO.
  DEF OUTPUT PARAM op-extent AS INT NO-UNDO.


  op-extent = IF ip-days LT aged-days-1 THEN 1 ELSE
              IF ip-days LT aged-days-2 THEN 2 ELSE
              IF ip-days LT aged-days-3 THEN 3 ELSE
              IF ip-days LT aged-days-4 THEN 4 ELSE 5.

END PROCEDURE.

