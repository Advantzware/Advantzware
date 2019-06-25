/*------------------------------------------------------------------------
    File        : Ritmlst.p
    Purpose     : 
    Main File   : fgrep/r-itmlst.w
    Syntax      :

    Description : Return a Dataset of Commission Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRitmListReport NO-UNDO
    FIELD itmlst AS CHAR.
DEFINE DATASET dsRitmListReport FOR ttRitmListReport.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeWare     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWare    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCust     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeItem     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCat      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCat     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmZero       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustWhse   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmProdCat    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRitmListReport.
    IF prmUser     = ?  THEN ASSIGN prmUser     = "".
    IF prmAction   = ?  THEN ASSIGN prmAction   = "".
    IF prmBeWare   = ?  THEN ASSIGN prmBeWare   = "".
    IF prmEndWare  = ?  THEN ASSIGN prmEndWare  = "".
    IF prmBeCust   = ?  THEN ASSIGN prmBeCust   = "".
    IF prmEndCust  = ?  THEN ASSIGN prmEndCust  = "".
    IF prmBeItem   = ?  THEN ASSIGN prmBeItem   = "".
    IF prmEndItem  = ?  THEN ASSIGN prmEndItem  = "".
    IF prmBeCat    = ?  THEN ASSIGN prmBeCat    = "".
    IF prmEndCat   = ?  THEN ASSIGN prmEndCat   = "".
    IF prmZero     = ?  THEN ASSIGN prmZero     = "".
    IF prmCustWhse = ?  THEN ASSIGN prmCustWhse = "".
    IF prmSort     = ?  THEN ASSIGN prmSort     = "".
    IF prmProdCat  = ?  THEN ASSIGN prmProdCat  = "".




    
DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U  NO-UNDO.
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U  NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)"  NO-UNDO.
DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#"  NO-UNDO.
DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_prod-cat AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_zero AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 66 NO-UNDO. 

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF STREAM excel.

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

IF prmAction = "Finished" THEN DO:

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
    v-today       =   TODAY
    begin_cat     =   prmBeCat
    end_cat       =   prmEndCat
    begin_cust-no =   prmBeCust 
    begin_i-no    =   prmBeItem   
    begin_whse    =   prmBeWare 
    end_cust-no   =   prmEndCust
    end_i-no      =   prmEndItem
    end_whse      =   prmEndWare
    rd_sort       =   prmSort  .
    
    
   tb_cust-whse = IF prmCustWhse = "True" THEN TRUE ELSE FALSE    . 
   tb_zero = IF prmZero = "True" THEN TRUE ELSE FALSE    . 
   tb_prod-cat = IF prmProdCat = "True" THEN TRUE ELSE FALSE    . 
   tb_excel    = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
   assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "masterlist" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "masterlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "masterlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

run run-report.
   
    CREATE ttRitmListReport.
    IF tb_excel  THEN
        ASSIGN ttRitmListReport.itmlst = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttRitmListReport.itmlst = vTextFile .
END.
PROCEDURE run-report :
/* ---------------------------------------------- fg/rep/itemlist.p 9/94 rd */
/* FINISHED GOODS - INVENTORY MASTER FILE LIST                                */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-qtyoh as int format "->>,>>>,>>9" NO-UNDO.
def var v-loc like fg-bin.loc extent 2 init [" ","zzzzz"] NO-UNDO.
def var v-ino like itemfg.i-no extent 2 init [" ", "zzzzzzzzzzzzzzz"] NO-UNDO.
def var v-cat as ch format "x(5)" extent 2 init [" ", "zzzzz"] NO-UNDO.
def var v-cust as char format "x(8)" extent 2 init [" ", "zzzzzzzz"] NO-UNDO.
def var v-zbal as log format "Y/N" init "Y" NO-UNDO.
def var v-job-no as char format "x(9)" NO-UNDO.
def var v-first as log init NO NO-UNDO.
def var v-prnt as log init NO NO-UNDO.
def var v-custown as log format "Y/N" init "N" NO-UNDO.
def var sort-opt as char no-undo init "C" FORMAT "!".
def var pcat as log init NO NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

form itemfg.i-no label "ITEM #"
     itemfg.i-name label "NAME"
     itemfg.procat column-label "PROD!CAT"
     itemfg.sell-uom label "UOM"
     itemfg.cust-no label "CUST. #"
     itemfg.cust-name label "CUST NAME"
     v-qtyoh column-label "QUANTITY!ON HAND" skip

    with down STREAM-IO width 132 frame itemx.


assign
 str-tit2 = "Manufactured Item Master Lis"
 {sys/inc/ctrtext.i str-tit2 112}

 v-loc[1]    = begin_whse
 v-loc[2]    = end_whse
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-ino[1]    = begin_i-no
 v-ino[2]    = end_i-no
 v-cat[1]    = begin_cat
 v-cat[2]    = end_cat
 v-zbal      = tb_zero
 v-custown   = tb_cust-whse
 sort-opt    = SUBSTR(rd_sort,1,1)
 pcat        = tb_prod-cat.

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "FG ITEM #,NAME,PROD CAT,UOM,CUST. #,CUST NAME,QUANTITY ON HAND,"
              + "CUST PART#,DESC LINE 1,DESC LINE 2".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.



display "" with frame r-top.

    if sort-opt eq "I" then
    for each itemfg use-index i-no
      {fg/rep/itemlist.i}

    else
    for each itemfg use-index customer
      {fg/rep/itemlist.i}

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
 
END.



/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
