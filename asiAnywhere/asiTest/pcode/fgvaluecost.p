
/*------------------------------------------------------------------------
    File        : fgvaluecost.p
    Purpose     :  FgValue/Cost by Whs/Bin/Tag

    Syntax      :

    Description : Return a Dataset For Freight Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttfgvaluecost NO-UNDO
    FIELD abc AS INT
    FIELD vfgvalue AS CHAR .

DEFINE DATASET dsfgvaluecost FOR ttfgvaluecost.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFgcost AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmDaysOld AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginWhse AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWhse AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginLocBin AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndLocBin AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginItem AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCat AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCat  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIcode AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrint AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER prmFrom AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSellPrice AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncludeZero AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncludeCustWhse AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncludeOnlyCustWhse AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintCost AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDlMat AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintCustPart AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintPo AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPoType AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintDate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintCompOnly AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintSubTot   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintActRelQty AS CHAR NO-UNDO.

   DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsfgvaluecost .
    DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

   IF prmUser = ?        THEN ASSIGN prmUser = "".
   IF prmFgcost     = ?  THEN ASSIGN prmFgcost = "".
   IF prmDaysOld = ?     THEN ASSIGN prmDaysOld = 0.
   IF prmBeginCust = ?   THEN ASSIGN prmBeginCust = "".
   IF prmEndCust = ?     THEN ASSIGN prmEndCust = "".
   IF prmBeginWhse = ?   THEN ASSIGN prmBeginWhse = "".
   IF prmEndWhse = ?     THEN ASSIGN prmEndWhse = "".
   IF prmBeginLocBin = ? THEN ASSIGN prmBeginLocBin = "".
   IF prmEndLocBin = ?   THEN ASSIGN prmEndLocBin = "".
   IF prmBeginItem = ?   THEN ASSIGN prmBeginItem = "".
   IF prmEndItem = ?     THEN ASSIGN prmEndItem = "".
   IF prmBeginCat = ?    THEN ASSIGN prmBeginCat = "".
   IF prmEndCat = ?      THEN ASSIGN prmEndCat = "".
   IF prmSort = ?        THEN ASSIGN prmSort = "".
   IF prmIcode = ?       THEN ASSIGN prmIcode = "".
   IF prmPrint = ?       THEN ASSIGN prmPrint = "".
   IF prmFrom = ?        THEN ASSIGN prmFrom = "".
   IF prmSellPrice = ?   THEN ASSIGN prmSellPrice = "".
   IF prmIncludeZero = ? THEN ASSIGN prmIncludeZero = "".
   IF prmIncludeCustWhse = ? THEN ASSIGN prmIncludeCustWhse = "".
   IF prmIncludeOnlyCustWhse = ?  THEN ASSIGN prmIncludeOnlyCustWhse = "".
   IF prmPrintCost = ?   THEN ASSIGN prmPrintCost = "".
   IF prmDlMat = ?       THEN ASSIGN prmDlMat = "".
   IF prmPrintCustPart = ? THEN ASSIGN prmPrintCustPart = "".
   IF prmPrintPo = ?     THEN ASSIGN prmPrintPo = "".
   IF prmPoType = ?      THEN ASSIGN prmPoType = "".

   IF prmPrintDate = ?      THEN ASSIGN prmPrintDate = "". 
   IF prmPrintCompOnly = ?  THEN ASSIGN prmPrintCompOnly = "".
   IF prmPrintSubTot = ?    THEN ASSIGN prmPrintSubTot =  "".
   IF prmPrintActRelQty = ? THEN ASSIGN prmPrintActRelQty = "".

   DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01  NO-UNDO.
DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U  NO-UNDO.
DEFINE VARIABLE begin_loc-bin AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)"     NO-UNDO.
DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_loc-bin AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz"  NO-UNDO.
DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /*INITIAL "c:\Inetpub\wwwroot\pdfs\FgCostValue.csv"*/  NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE rd_i-code AS CHARACTER INITIAL "All"  NO-UNDO.
DEFINE VARIABLE rd_msf AS CHARACTER INITIAL "Qty"     NO-UNDO.
DEFINE VARIABLE rd_po-type AS CHARACTER INITIAL "Line"  NO-UNDO.
DEFINE VARIABLE rd_price AS CHARACTER INITIAL "Order"   NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#"  NO-UNDO.
DEFINE VARIABLE tb_actrel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_cost AS LOGICAL INITIAL NO  NO-UNDO.
DEFINE VARIABLE tb_cost-2 AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_cust-pt AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_cust-whse-2 AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_po-num AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_rct-date AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_sell-pr AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_sets AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_subt AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_zero AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "99/99/9999" NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR excel-header-var-1 AS CHAR NO-UNDO.
DEF VAR excel-header-var-2 AS CHAR NO-UNDO.
DEF VAR excel-header-var-3 AS CHAR NO-UNDO.

{fg/rep/fg-ibtg1.i NEW SHARED}


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
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

ASSIGN cocode    = prmComp
 locode          = usercomp.loc
 v-today         = TODAY .
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
IF prmFgcost = "fgcost" THEN DO:

    assign
        as-of-date       = prmDate 
        fi_days-old      = prmDaysOld
        begin_cust-no    = prmBeginCust
        end_cust-no      = prmEndCust
        begin_whse       = prmBeginWhse 
        end_whse         = prmEndWhse
        begin_loc-bin    = prmBeginLocBin
        end_loc-bin      = prmEndLocBin
        begin_i-no       = prmBeginItem
        end_i-no         = prmEndItem 
        begin_cat        = prmBeginCat
        end_cat          = prmEndCat 
        rd_sort          = prmSort 
        rd_i-code        = prmIcode
        rd_msf           = prmPrint
        rd_price         = prmFrom 
        tb_sell-pr       = IF prmSellPrice = "yes" THEN TRUE ELSE FALSE
        tb_zero          = IF prmIncludeZero = "yes" THEN TRUE ELSE FALSE
        tb_cust-whse     = IF prmIncludeCustWhse = "yes" THEN TRUE ELSE FALSE
        tb_cust-whse-2   = IF prmIncludeOnlyCustWhse = "yes" THEN TRUE ELSE FALSE
        tb_cost          = IF prmPrintCost = "yes" THEN TRUE ELSE FALSE                
        tb_cost-2        = IF prmDlMat  = "yes" THEN TRUE ELSE FALSE                    
        tb_cust-pt       = IF prmPrintCustPart = "yes" THEN TRUE ELSE FALSE                        
        tb_po-num        = IF prmPrintPo = "yes" THEN TRUE ELSE FALSE                            
        rd_po-type       =  prmPoType
             
        tb_rct-date      = IF prmPrintDate = "yes" THEN TRUE ELSE FALSE    
        tb_sets          = IF prmPrintCompOnly = "yes" THEN TRUE ELSE FALSE   
        tb_subt          = IF prmPrintSubTot = "yes" THEN TRUE ELSE FALSE    
        tb_actrel        = IF prmPrintActRelQty = "yes" THEN TRUE ELSE FALSE    .                              



assign
    init-dir    =  v-webrootpath
            fi_file = init-dir + "FgCostValue" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99")+ REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".csv".  
        vPdfFile   = "FgCostValue" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99")+ REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".csv".       
              
    run run-report.
    CREATE ttfgvaluecost.
    ASSIGN ttfgvaluecost.vfgvalue = vPdfFile.
    

END.

/*********************************************************************************************************************/
PROCEDURE run-report :

{sys/form/r-topw.f}

form header
     "        "
     "               "
     "                         "
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top1 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "               "
     "                         "
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     "CUST PART #    "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "--------"
     "---------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top2 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "        "
     "RECEIPT "
     "                         "
     "     "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     "  DATE  "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     "--------"
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top3 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "        "
     "RECEIPT "
     "               "
     "                         "
     "     "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     "  DATE  "
     "CUST PART #    "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     "--------"
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top4 stream-io width 200
         no-labels no-box no-underline page-top.

/*
IF NOT rd_sort BEGINS "W" THEN tb_subt = YES.
*/

assign
 str-tit2 = "fgvalue"
 {sys/inc/ctrtext.i str-tit2 112}

 vdat           = as-of-date
 fcus           = begin_cust-no
 tcus           = end_cust-no
 v-loc[1]       = begin_whse
 v-loc[2]       = end_whse
 v-loc-bin[1]   = begin_loc-bin
 v-loc-bin[2]   = end_loc-bin
 fino           = begin_i-no
 tino           = end_i-no
 fcat           = begin_cat
 tcat           = END_cat
 v-type         = SUBSTR(rd_i-code,1,1)
 v-sort-by-cust = SUBSTR(rd_sort,1,2)
 zbal           = tb_zero
 v-custown      = tb_cust-whse
 v-prt-c        = tb_cost
 v-dl-mat       = tb_cost-2
 v-prt-p        = tb_sell-pr
 v-prt-cpn      = tb_cust-pt
 v-prt-po       = tb_po-num
 v-prt-arqty    = tb_actrel
 v-po-type      = SUBSTR(rd_po-type,1,1)
 v-prt-msf      = rd_msf EQ "MSF"
 v-subt         = tb_subt
 v-fgprice      = rd_price EQ "FG"
 v-sets         = tb_sets
 v-rct-date     = tb_rct-date

 v-tot-qty      = 0
 v-tot-cst      = 0
 v-tot-ext      = 0
 v-label1       = ""
 v-label2       = ""
 v-label3       = "".
    
assign
  v-file         = fi_file
  v-excel        = tb_excel
  v-runexcel     = tb_runexcel.

/*IF v-prt-c THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  ASSIGN
   v-prt-c = ll-secure
   v-prt-p = (v-prt-c and v-dl-mat) or (v-prt-p and not v-dl-mat).
END.*/

if v-prt-c then do:
  assign
   v-label1[2] = "UOM COST"
   v-label1[3] = "--------"
   v-label2[5] = "-----------".
       
  if v-dl-mat then
    assign
     v-label2[1] = "     DIRECT"
     v-label2[3] = " LABOR COST".
  else
    assign
     v-label2[1] = "      TOTAL"
     v-label2[3] = "       COST".
end.    


if v-prt-p then do:
  v-label2[6] = "-----------" + " ---------------".

  if v-dl-mat THEN DO:
    assign
      v-label2[2] = "   MATERIAL" + if v-po-type eq "L"
                                   then " LINE"
                                   else " ORDER"
      v-label2[4] = "       COST" + " PO"
      excel-header-var-1 = "MATERIAL"
      excel-header-var-2 = IF v-po-type EQ "L" THEN "LINE"
                           ELSE "ORDER"
      excel-header-var-3 = "COST".
  END.
  ELSE DO:
  
    assign
     v-label2[2] = "    SELLING" + if v-po-type eq "L"
                                   then " LINE"
                                   else " ORDER"
     v-label2[4] = "      VALUE" + " PO"
     excel-header-var-1 = "SELLING"
     excel-header-var-2 = IF v-po-type EQ "L" THEN "LINE"
                          ELSE "ORDER"
     excel-header-var-3 = "VALUE".
  END.
end.

ELSE v-prt-po = NO.

IF v-prt-msf THEN
  ASSIGN
   v-label1[4] = "     MSF"
   v-qoh-f     = "->>9.999".
ELSE
  ASSIGN
   v-label1[4] = "QUANTITY"
   v-qoh-f     = "->>>,>>9".

IF v-prt-arqty THEN
  ASSIGN
   v-label3[1] = "ACTUAL REL"
   v-label3[2] = "  QUANTITY"
   v-label3[3] = "----------".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}


VIEW FRAME r-top.

IF v-rct-date THEN DO:
  IF v-prt-cpn THEN VIEW FRAME r-top4.
               ELSE VIEW FRAME r-top3.
END.
ELSE
  IF v-prt-cpn THEN VIEW FRAME r-top2.
               ELSE VIEW FRAME r-top1.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   
   IF v-rct-date = TRUE AND v-prt-cpn = TRUE THEN DO:
      EXPORT STREAM excel DELIMITER ","   
         " "
         " "
         " "
         "RECEIPT "
         " "
         " "
         " "
         " "
         " "
         v-label1[4]
         "COST"
         v-label1[1]
         v-label2[1] 
         excel-header-var-1
         excel-header-var-2
         v-label3[1]
         SKIP.
      EXPORT STREAM excel DELIMITER ","          
         "CUSTOMER"
         "ITEM #"
         "TAG #"
         "DATE"
         "CUST PART #"
         "DESCRIPTION"
         "WHSE "
         "BIN"
         "JOB #"
         "ON HAND"
         "UOM"
         v-label1[2]
         v-label2[3] 
         excel-header-var-3
         "PO"
         v-label3[2]
         SKIP.      
   END. /*IF v-rct-date = TRUE AND v-prt-cpn = TRUE THEN DO:*/


   IF v-rct-date = TRUE AND v-prt-cpn = FALSE THEN DO:
      EXPORT STREAM excel DELIMITER ","    
          " "
          " "
          " "
          "RECEIPT "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","      
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          "DATE"
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /*IF v-rct-date TRUE AND v-prt-cpn = FALSE THEN DO:*/     

   IF v-rct-date = FALSE AND v-prt-cpn = TRUE THEN DO:
      EXPORT STREAM excel DELIMITER ","   
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","     
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          "CUST PART #"
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /* IF v-rct-date = FALSE AND v-prt-cpn = TRUE THEN DO: */

   IF v-rct-date = FALSE AND v-prt-cpn = FALSE THEN DO:
      EXPORT STREAM excel DELIMITER ","
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","     
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /*IF v-rct-date FALSE AND v-prt-cpn = FALSE THEN DO:*/
        
END. /* IF tb_excel THEN DO: */ 

/*
    STATUS DEFAULT "Processing...".*/

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.cust-no GE fcus
          AND itemfg.cust-no LE tcus
          AND itemfg.i-no    GE fino
          AND itemfg.i-no    LE tino
          AND itemfg.procat  GE fcat
          AND itemfg.procat  LE tcat
          AND (itemfg.i-code EQ v-type or v-type eq "A")
          AND (NOT v-sets    OR
               itemfg.isaset OR
               CAN-FIND(FIRST fg-set
                        WHERE fg-set.company EQ itemfg.company
                          AND fg-set.part-no EQ itemfg.i-no))
        USE-INDEX customer:
	
		IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.

      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
                             zbal, fi_days-old, YES, v-custown).

      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
            AND (v-custown OR tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
            AND (NOT tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
          USE-INDEX co-ino:

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
      END.
    END.

    if v-sort-by-cust eq "Cu" then run fg/rep/fg-ibtg1.p. else
    if v-sort-by-cust eq "FG" then run fg/rep/fg-ibtg2.p. else
    if v-sort-by-cust eq "Pr" then run fg/rep/fg-ibtg3.p. else
    if v-sort-by-cust eq "Pa" then run fg/rep/fg-ibtg4.p. else
                                   run fg/rep/fg-ibtg5.p.

    put skip(1).

    IF v-rct-date THEN
    if v-prt-cpn then do:
      PUT "GRAND TOTALS" TO 96.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 118.
      ELSE
        PUT v-tot-qty[3] TO 118.

      if v-prt-c then put v-tot-cst[3] to 144.
      if v-prt-p then put v-tot-ext[3] to 157 skip(1).
    end.

    else do:
      put "GRAND TOTALS" TO 80.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 102.
      ELSE
        PUT v-tot-qty[3] TO 102.

      if v-prt-c then put v-tot-cst[3] to 128.
      if v-prt-p then put v-tot-ext[3] to 141 skip(1).
    end.

    ELSE
    if v-prt-cpn then do:
      PUT "GRAND TOTALS" TO 87.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 109.
      ELSE
        PUT v-tot-qty[3] TO 109.

      if v-prt-c then put v-tot-cst[3] to 135.
      if v-prt-p then put v-tot-ext[3] to 148 skip(1).
    end.

    else do:
      put "GRAND TOTALS" TO 71.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 93.
      ELSE
        PUT v-tot-qty[3] TO 93.

      if v-prt-c then put v-tot-cst[3] to 119.
      if v-prt-p then put v-tot-ext[3] to 132 skip(1).
    end.

    /*STATUS DEFAULT "".*/
end procedure.
