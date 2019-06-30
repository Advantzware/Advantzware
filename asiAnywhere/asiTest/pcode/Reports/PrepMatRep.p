

/*------------------------------------------------------------------------
    File        : PrepMatRep.p
    Purpose     : Prep List by Material Type
    Main File   : cerep\r-prpmat.w
    Syntax      :

    Description : Return a Dataset of Request For Prep

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttPrepMatReport NO-UNDO
        FIELD pMatFile AS CHAR
        FIELD dfjzvbsj AS CHAR
        .

    DEFINE DATASET dsPrepMatReport FOR ttPrepMatReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegLastModDate  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndLastModDate  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegLastOrdDate  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndLastOrdDate  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegPrepCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPrepCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDie             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPlate           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFolding         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmCorrugated      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSortBy          AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmprintDscr       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrepMatReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser             = ?        THEN ASSIGN     prmUser           = "".
    IF  prmAction           = ?        THEN ASSIGN     prmAction         = "".
    IF  prmBegCustomer      = ?        THEN ASSIGN     prmBegCustomer    = "".
    IF  prmEndCustomer      = ?        THEN ASSIGN     prmEndCustomer    = "".
    IF  prmBegLastModDate   = ?        THEN ASSIGN     prmBegLastModDate = "".
    IF  prmEndLastModDate   = ?        THEN ASSIGN     prmEndLastModDate = "".
    IF  prmBegLastOrdDate   = ?        THEN ASSIGN     prmBegLastOrdDate = "".
    IF  prmEndLastOrdDate   = ?        THEN ASSIGN     prmEndLastOrdDate = "".
    IF  prmBegSalrep        = ?        THEN ASSIGN     prmBegSalrep      = "".
    IF  prmEndSalrep        = ?        THEN ASSIGN     prmEndSalrep      = "".
    IF  prmBegPrepCode      = ?        THEN ASSIGN     prmBegPrepCode    = "".
    IF  prmEndPrepCode      = ?        THEN ASSIGN     prmEndPrepCode    = "".
    IF  prmDie              = ?        THEN ASSIGN     prmDie            = "".
    IF  prmPlate            = ?        THEN ASSIGN     prmPlate          = "".
    IF  prmFolding          = ?        THEN ASSIGN     prmFolding        = "".
    IF  prmCorrugated       = ?        THEN ASSIGN     prmCorrugated     = "".
    IF  prmSortBy           = ?        THEN ASSIGN     prmSortBy         = "".
    IF  prmprintDscr        = ?        THEN ASSIGN     prmprintDscr      = "".
    IF  prmOut              = ?        THEN ASSIGN     prmOut            = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.

    DEF TEMP-TABLE tt-report LIKE report.



    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_last-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_last-ord AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_prep AS CHARACTER FORMAT "X(15)" NO-UNDO.
    DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_last-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_last-ord AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_prep AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Prep Code" NO-UNDO.
    DEFINE VARIABLE tb_contr AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_corr AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_die AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_fold AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_plate AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.
    DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp.


 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 
 ASSIGN    
 v-today = TODAY . 

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCustomer NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCustomer  OR prmEndCustomer = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
     usercust.company = prmComp  NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "PrepMatRep" THEN DO:
    ASSIGN 
        begin_cust-no   = prmBegCustomer   
        begin_last-date = datetime(prmBegLastModDate)   
        begin_last-ord  = datetime(prmBegLastOrdDate)
        begin_prep      = prmBegPrepCode
        begin_slsmn     = prmBegSalrep
        end_cust-no     = prmEndCustomer
        end_last-date   = datetime(prmEndLastModDate)    
        end_last-ord    = datetime(prmEndLastOrdDate)    
        end_prep        = prmEndPrepCode   
        end_slsmn       = prmEndSalrep   
        rd_sort         = prmSortBy
        .

    ASSIGN
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_contr      = IF prmprintDscr = "yes" THEN TRUE ELSE FALSE  
        tb_corr       = IF prmCorrugated = "yes" THEN TRUE ELSE FALSE 
        tb_die        = IF prmDie = "yes" THEN TRUE ELSE FALSE
        tb_fold       = IF prmFolding = "yes" THEN TRUE ELSE FALSE
        tb_plate      = IF prmPlate = "yes" THEN TRUE ELSE FALSE
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "prepmat" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "prepmat" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "prepmat" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttPrepMatReport.
        ASSIGN ttPrepMatReport.pMatFile = excel-file.
    END.
    ELSE DO:
        CREATE ttPrepMatReport.
        ASSIGN ttPrepMatReport.pMatFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-top3w.f}

def var fmat            as   char               format "!" init "B" no-undo.
def var fcus            like eb.cust-no.
def var tcus            like fcus               initial "zzzzzzzz".
def var fsmn            like eb.sman.
def var tsmn            like fsmn               initial "zzz".
def var fdat            like est.mod-date       format "99/99/9999" 
                                                extent 2            no-undo.
def var tdat            like fdat               format "99/99/9999" 
                                                extent 2            no-undo.
def var fpre            like eb.die-no.
def var tpre            like eb.die-no          initial "zzzzzzzzzzzzzzz".
def var findus          as   char               format "!" init "B" no-undo.

def var v-sort          as   char               format "!" init "P" no-undo.
def var v-sman-lab      as   char               init "SalesRep:".
def var v-sman          like sman.sman.
def var v-sname         like sman.sname.
def VAR v-prt-desc       as   log                format "Yes/No"     no-undo.
        
assign
 str-tit2 = "Prep List by Material Type"
 {sys/inc/ctrtext.i str-tit2 112}

 fmat         = IF tb_die THEN
                  IF tb_plate THEN "B"
                  ELSE "D"
                ELSE
                  IF tb_plate THEN "P"
                ELSE ""
 findus       = IF tb_fold THEN
                  IF tb_corr THEN "B"
                  ELSE "F"
                ELSE
                  IF tb_corr THEN "C"
                ELSE ""
 fsmn         = begin_slsmn
 tsmn         = end_slsmn
 fcus         = begin_cust-no
 tcus         = end_cust-no
 fdat[1]      = begin_last-date
 tdat[1]      = end_last-date
 fdat[2]      = begin_last-ord
 tdat[2]      = end_last-ord
 fpre         = begin_prep
 tpre         = end_prep
 v-sort       = SUBSTR(rd_sort,1,1)
 v-prt-desc   = tb_contr.                    

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

if v-sort ne "S" then do:
  v-sman-lab = "".
  view frame r-top.
end.

{sa/sa-sls01.i}

/*SESSION:SET-WAIT-STATE("general").*/

    FOR EACH tt-report:
      DELETE tt-report.
    END.
 
    IF tb_excel THEN do:
      OUTPUT STREAM excel TO VALUE(fi_file).
      EXPORT STREAM excel DELIMITER "," 
          "Prep Code"
          "Description"
          "Est #"
          "Customer Part #"
          "Est Mod"
          "Ord Date"
          "Order #"
          "Customer"
          "Name"
          SKIP.
    END.

    if v-prt-desc then do:
      form header
          v-sman-lab              format "x(9)"
          v-sman
          v-sname
          skip(1)
          "               "
          "                    "
          "     "    
          "               "
          "Last      "
          "Last      "
          "Last    "
          "        "
          "                              "        
          "Prep Code      "
          "Description         "
          "Est #   "
          "Customer Part #"
          "Est Mod   " 
          "Ord Date  "
          "Order#  "
          "Customer Name                 "        
           
          SKIP

          "---------------"
          "--------------------"
          "--------"
          "---------------"
          "----------"
          "----------"
          "--------"
          "--------"
          "------------------------------"        skip

          with no-box page-top STREAM-IO width 132 frame top1.
    
      if v-sort ne "S" then do:
        view frame top1.
        page.
      end.
    end.
        
    else do:
      form header
          v-sman-lab              format "x(9)"
          v-sman
          v-sname
          skip(1)

          "               "
          "     "
          "               "
          "Date      "
          "Last      "
          "Last       "
          "        "
          "                              "        SKIP
          "Prep Code      "
          "Est #   "
          "Customer Part #"
          "Est Mod   "
          "Ord Date  "
          "Order#  "
          "Cust#   "
          "Customer Name                 "  

          SKIP

          "---------------"
          "--------"
          "---------------"
          "----------"
          "----------"
          "--------"
          "--------"
          "------------------------------"        skip

          with no-box page-top STREAM-IO width 132 frame top2.
     
      if v-sort ne "S" then do:
        view frame top2.
        page.
      end.
    end.

    if index("DB",fmat) gt 0 then
    for each eb
        where eb.company                eq cocode
          and eb.loc                    eq locode
          and eb.die-no                 gt ""
          and eb.die-no                 ge fpre
          and eb.die-no                 le tpre
         
      {sys/ref/preplst2.i die}
    end.

    if index("PB",fmat) gt 0 then
    for each eb
        where eb.company                eq cocode
          and eb.loc                    eq locode
          and eb.plate-no               gt ""
          and eb.plate-no               ge fpre
          and eb.plate-no               le tpre
         
      {sys/ref/preplst2.i plate}
    end.

    for each tt-report
        where tt-report.term-id eq v-term

        break by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03
              by tt-report.key-04

        /*with frame prep no-box no-labels STREAM-IO width 132*/

        transaction:
        
      find eb  where recid(eb) eq tt-report.rec-id no-lock.
      find FIRST est
          where est.company EQ eb.company
            AND est.est-no  eq eb.est-no
          no-lock.

      find first prep
          where (prep.company eq cocode)
            and prep.code eq tt-report.key-09
          no-lock no-error.

      if first-of(tt-report.key-01) and v-sort eq "S" then do:
        find first sman of eb no-lock no-error.
        assign
         v-sman  = caps(eb.sman)
         v-sname = if avail sman then sman.sname else "Not on file".

        if first(tt-report.key-01) then do:
          view frame r-top.
          if v-prt-desc then view frame top1.
                        else view frame top2.
        end.

        page.
      end.

      {sys/ref/preplist.i}
    end.

end procedure.
