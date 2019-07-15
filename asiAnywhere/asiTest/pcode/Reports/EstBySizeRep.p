

/*------------------------------------------------------------------------
    File        : EstBySizeRep.p
    Purpose     : Estimate By Size
    Main File   : cerep\r-estsiz.w
    Syntax      :

    Description : Return a Dataset of Request For Quote

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttEstbysizeReport NO-UNDO
        FIELD vEstSizeFile AS CHAR        
        .

    DEFINE DATASET dsEstbysizeReport FOR ttEstbysizeReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginStyle      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndStyle        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegFlute        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndFlute        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegTest         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndTest         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDie          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDie          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSortBy          AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmFold            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmCorr            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstbysizeReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegCustomer  = ?        THEN ASSIGN     prmBegCustomer  = "".
    IF  prmEndCustomer  = ?        THEN ASSIGN     prmEndCustomer  = "".
    IF  prmBeginStyle   = ?        THEN ASSIGN     prmBeginStyle   = "".
    IF  prmEndStyle     = ?        THEN ASSIGN     prmEndStyle     = "".
    IF  prmBegFlute     = ?        THEN ASSIGN     prmBegFlute     = "".
    IF  prmEndFlute     = ?        THEN ASSIGN     prmEndFlute     = "".
    IF  prmBegTest      = ?        THEN ASSIGN     prmBegTest      = "".
    IF  prmEndTest      = ?        THEN ASSIGN     prmEndTest      = "".
    IF  prmBegDie       = ?        THEN ASSIGN     prmBegDie       = "".
    IF  prmEndDie       = ?        THEN ASSIGN     prmEndDie       = "".
    IF  prmSortBy       = ?        THEN ASSIGN     prmSortBy       = "".
    IF  prmFold         = ?        THEN ASSIGN     prmFold         = "".
    IF  prmCorr         = ?        THEN ASSIGN     prmCorr         = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut          = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    
    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_die-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE begin_flute AS CHARACTER FORMAT "X(15)" NO-UNDO.
    DEFINE VARIABLE begin_style AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_test AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_die-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_flute AS CHARACTER FORMAT "X(15)" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE end_style AS CHARACTER FORMAT "X(30)" INITIAL "zzzz" NO-UNDO.
    DEFINE VARIABLE end_test AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_sortby AS CHARACTER INITIAL "Length" NO-UNDO.
    DEFINE VARIABLE tb_corr AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_fold AS LOGICAL INITIAL yes NO-UNDO.
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

IF prmAction = "EstBySizeRep" THEN DO:

 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCustomer NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCustomer  OR prmEndCustomer = "zzzzzzzz" ) NO-ERROR.
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


 
    ASSIGN 
        begin_cust-no  = prmBegCustomer                                         
        begin_die-no   = prmBegDie                               
        begin_flute    = prmBegFlute                                            
        begin_style    = prmBeginStyle                                            
        begin_test     = prmBegTest                                    
        end_cust-no    = prmEndCustomer                                            
        end_die-no     = prmEndDie                                               
        end_flute      = prmEndFlute                                                            
        end_style      = prmEndStyle                                                             
        end_test       = prmEndTest                                                             
        rd_sortby      = prmSortBy                                                             
        .                                                                              
                                                                                       
    ASSIGN                                                                              
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_corr       = IF prmCorr = "yes" THEN TRUE ELSE FALSE  
        tb_fold       = IF prmFold = "yes" THEN TRUE ELSE FALSE
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "estsize" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "estsize" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".  

        vtextfile = "estsize" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttEstbysizeReport.
        ASSIGN ttEstbysizeReport.vEstSizeFile = excel-file.
    END.
    ELSE DO:
        CREATE ttEstbysizeReport.
        ASSIGN ttEstbysizeReport.vEstSizeFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-top3w.f}

def var v-cust      like eb.cust-no extent 2 init ["","zzzzzzzz"] NO-UNDO.
def var v-styl      like eb.style   extent 2 init ["","zzzz"] NO-UNDO.
def var v-flut      like eb.flute   extent 2 init ["","zzz"] NO-UNDO.
def var v-test      like eb.test    extent 2 init ["","zzzzzz"] NO-UNDO.
def var v-sort      as   log  format "Length/Part#" init YES NO-UNDO.
def var v-indtype   as   char format "x(15)" NO-UNDO.

/* gdm - 10130806 */
DEF VAR v_part-no  LIKE eb.part-no           NO-UNDO.
DEF VAR v_ord-date AS CHAR FORMAT "99/99/99" NO-UNDO.
DEF VAR v-num-up LIKE eb.num-up NO-UNDO.

form header
     "Industry:"
     v-indtype
     skip(1)
    with frame r-top.

/*SESSION:SET-WAIT-STATE ("general").*/

assign
 str-tit2 = "Estimates by Size Report"
 {sys/inc/ctrtext.i str-tit2 112}
 
 v-cust[1] = begin_cust-no
 v-cust[2] = end_cust-no
 v-styl[1] = begin_style
 v-styl[2] = end_style
 v-flut[1] = begin_flute
 v-flut[2] = end_flute
 v-test[1] = begin_test
 v-test[2] = end_test
 v-sort    = rd_sortby EQ "Length".

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i  value(lines-per-page) }

/*if td-show-parm then run show-param.*/

/* gdm - 10130806 */
IF tb_excel THEN 
    OUTPUT STREAM excel TO VALUE(fi_file).      

for each eb
    where eb.company eq cocode
      and eb.cust-no ge v-cust[1]
      and eb.cust-no le v-cust[2]
      and eb.style   ge v-styl[1]
      and eb.style   le v-styl[2]
      and eb.flute   ge v-flut[1]
      and eb.flute   le v-flut[2]
      and eb.test    ge v-test[1]
      and eb.test    le v-test[2]
      AND eb.die-no  GE begin_die-no
      AND eb.die-no  LE end_die-no
      AND lookup(eb.cust-no,custcount) <> 0
      and ((tb_corr and eb.est-type ge 5) or
           (tb_fold and eb.est-type le 4))
      no-lock,
      
      first ef
      where ef.company eq eb.company
        AND ef.est-no  EQ eb.est-no
        and ef.form-no eq eb.form-no
      no-lock,
      
      first est
      where est.company eq eb.company
        AND est.est-no  EQ eb.est-no
      no-lock
      
      break by (if eb.est-type ge 5 then 1 else 2)
            by (if v-sort then "" else eb.part-no)
            by (if v-sort then "" else eb.cust-no)
            by eb.len
            by eb.wid
            by eb.dep
            by eb.style
            by ef.board
            by eb.cust-no
            by eb.est-no:
            
  if first-of(if eb.est-type ge 5 then 1 else 2) then do:
    hide frame r-top no-pause.
    v-indtype = if eb.est-type ge 5 then "Corrugated" else "Folding".
    view frame r-top.
    page.

    /* gdm - 10130806 */
    IF tb_excel THEN DO:

        PUT STREAM excel UNFORMATTED
          '"' v-indtype '"'
          SKIP.

        EXPORT STREAM excel DELIMITER "," 
        "Length"           
        "Width"            
        "Depth"            
        "Style"            
        "Material"         
        "Adder 1"          
        "Adder 2"          
        "Adder 3"          
        "Customer Part#"   
        "Est#"             
        "Last Ord Date"  
        "Customer Name"                    
        "Last User"
        "# Up"
        SKIP.
    END.
  end.

  /* gdm - 10130806 */
  ASSIGN
      v_part-no = REPLACE(eb.part-no,'"','')
      v_ord-date = IF est.ord-date NE ? 
                     THEN STRING(est.ord-date,"99/99/99") ELSE "".

  RUN sys/inc/numup.p (ef.company,ef.est-no,ef.form-no,OUTPUT v-num-up).
      
  display eb.len                      column-label "Length"
          eb.wid                      column-label "Width"
          eb.dep                      column-label "Depth"
          eb.style                    column-label "Style"
          ef.board                    column-label "Material"
          ef.adder[1]                 column-label "Adder 1"
          ef.adder[2]                 column-label "Adder 2"
          ef.adder[3]                 column-label "Adder 3"
          eb.part-no                  column-label "Customer Part#"
          trim(est.est-no)            format "x(8)"
                                      column-label "Est#"
          est.ord-date                FORMAT "99/99/99"            
                                      column-label "Last Ord!  Date" 
          eb.ship-name                column-label "Customer Name"
                                      format "x(24)"
          est.updated-id              COLUMN-LABEL "Last User"
          v-num-up                    COLUMN-LABEL "# Up"
                                                    
       WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 154.

  /* gdm - 10130806 */
  IF tb_excel THEN
     PUT STREAM excel UNFORMATTED
         '"' eb.len                          '",'
         '"' eb.wid                          '",'
         '"' eb.dep                          '",'
         '"' eb.style                        '",'
         '"' ef.board                        '",'
         '"' ef.adder[1]                     '",'
         '"' ef.adder[2]                     '",'
         '"' ef.adder[3]                     '",'
         '"' v_part-no                       '",'
         '"' STRING(est.est-no)              '",'
         '"' v_ord-date                      '",'
         '"' eb.ship-name                    '",' 
         '"' est.updated-id                  '",' 
         '"' STRING(eb.num-up)               '"'
       SKIP.

  /* gdm - 10130806 */
  IF LAST-OF(if eb.est-type GE 5 THEN 1 ELSE 2) THEN 
      IF tb_excel THEN 
          PUT STREAM excel UNFORMATTED SKIP(1).

end.

end procedure.
