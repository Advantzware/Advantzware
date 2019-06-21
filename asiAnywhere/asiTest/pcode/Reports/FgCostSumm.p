/*------------------------------------------------------------------------
    File        : FgCostSumm.p
    Purpose     : Fg Item Value/Cost Summary
    Main File   : fgrep/r-fgcst1.w
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}   
{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttFgCostSummRep NO-UNDO
    FIELD vFgCostSummFile AS CHAR.

DEFINE DATASET dsFgCostSummRep FOR ttFgCostSummRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAsofdt     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginWhse  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWhse    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginItem  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCat   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCat     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIcode      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort       AS CHAR NO-UNDO.            
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgCostSummRep.
    IF prmUser       = ?  THEN ASSIGN prmUser       = "".
    IF prmAction     = ?  THEN ASSIGN prmAction     = "".
    IF prmAsofdt     = ?  THEN ASSIGN prmAsofdt     = "".
    IF prmBeginCust  = ?  THEN ASSIGN prmBeginCust  = "".
    IF prmEndCust    = ?  THEN ASSIGN prmEndCust    = "".
    IF prmBeginWhse  = ?  THEN ASSIGN prmBeginWhse  = "".
    IF prmEndWhse    = ?  THEN ASSIGN prmEndWhse    = "".
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem  = "".
    IF prmEndItem    = ?  THEN ASSIGN prmEndItem    = "".
    IF prmBeginCat   = ?  THEN ASSIGN prmBeginCat   = "". 
    IF prmEndCat     = ?  THEN ASSIGN prmEndCat     = "". 
    IF prmIcode      = ?  THEN ASSIGN prmIcode      = "". 
    IF prmSort       = ?  THEN ASSIGN prmSort       = "".         
    IF prmOut        = ?  THEN ASSIGN prmOut        = "". 

    DEF VAR ll-secure AS LOG NO-UNDO.
    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.
                                                  
    DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 NO-UNDO.
    DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U NO-UNDO.
    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_i-code AS CHARACTER INITIAL "All" NO-UNDO.    
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" NO-UNDO.    
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF NEW SHARED STREAM excel.   
 

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

{fgrep/r-fgcst1.i NEW}

ASSIGN    
 v-today = TODAY .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
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


IF prmAction = "FgcostSumm" THEN DO:
    ASSIGN
        as-of-date     =  DATE(prmAsofdt)
        begin_cat      =  prmBeginCat               
        begin_cust-no  =  prmBeginCust                 
        begin_i-no     =  prmBeginItem                
        begin_whse     =  prmBeginWhse                 
        end_cat        =  prmEndCat               
        end_cust-no    =  prmEndCust                 
        end_i-no       =  prmEndItem                
        end_whse       =  prmEndWhse                  
        rd_i-code      =  prmIcode                           
        rd_sort        =  prmSort  
        .        

    ASSIGN                                                                   
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE . 

    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "fgcostsumm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".  
        excel-file   = "fgcostsumm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".  

        vtextfile = "fgcostsumm" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttFgCostSummRep.
        ASSIGN ttFgCostSummRep.vFgCostSummFile = excel-file.
    END.
    ELSE DO:
        CREATE ttFgCostSummRep.
        ASSIGN ttFgCostSummRep.vFgCostSummFile = vtextfile.
    END.
END.


/*********************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topwas.f}

DEF VAR excelheader AS CHAR NO-UNDO.


ASSIGN
 str-tit2 = "FG Item Value/Cost Summary"
 {sys/inc/ctrtext.i str-tit2 112}

 vdat           = as-of-date
 as-of-day_str  = as-of-day_str + STRING(vdat,"99/99/99")
 fcus           = begin_cust-no
 tcus           = end_cust-no
 v-loc[1]       = begin_whse
 v-loc[2]       = end_whse
 fino           = begin_i-no
 tino           = end_i-no
 fcat           = begin_cat
 tcat           = END_cat
 v-type         = SUBSTR(rd_i-code,1,1)
 v-sort-by-cust = SUBSTR(rd_sort,1,2)
 v-excel        = tb_excel
    
 v-tot-qty      = 0
 v-tot-msf      = 0
 v-tot-cst      = 0
 v-tot-ext      = 0.

/*IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).*/

SESSION:SET-WAIT-STATE ("general").

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Customer,FG Item#,Cust Part#,Whse,Total Qty,Total MSF," +
                "Total Cost,Total Sell Value,$$$/MSF".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END. 

/*IF td-show-parm THEN DO:
  RUN show-param.
  PAGE.
END.*/

VIEW FRAME r-top.

STATUS DEFAULT "Processing...".

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    FOR EACH itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.cust-no GE fcus
          AND itemfg.cust-no LE tcus
          AND itemfg.i-no    GE fino
          AND itemfg.i-no    LE tino
          AND itemfg.procat  GE fcat
          AND itemfg.procat  LE tcat
          AND (itemfg.i-code EQ v-type OR v-type EQ "A")
          AND LOOKUP(itemfg.cust-no, custcount) <> 0
        USE-INDEX customer NO-LOCK:

      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], "", "zzzzzzzz",
                             NO, 0, YES, YES).

      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
          USE-INDEX co-ino:

        IF tt-fg-bin.qty NE 0 THEN DO:
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

    IF v-sort-by-cust EQ "Cu" THEN RUN fg/rep/fg-cst11.p.

    ELSE
    IF v-sort-by-cust EQ "FG" THEN RUN fg/rep/fg-cst12.p.

    ELSE
    IF v-sort-by-cust EQ "Pr" THEN RUN fg/rep/fg-cst13.p.

    ELSE                           RUN fg/rep/fg-cst14.p. 

STATUS DEFAULT "".

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
END.

end procedure.
