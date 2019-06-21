/*------------------------------------------------------------------------
    File        : EsymarRep.p
    Purpose     : Estimate List w/Margins Report

    Syntax      :

    Description : Return a Dataset of Estimate List w/Margins Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstimateWmargRep NO-UNDO
    FIELD wmarginsfile AS CHAR.

DEFINE DATASET dsEstimateWmargRep FOR ttEstimateWmargRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSman    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSman    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegEst     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndEst     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAddDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndAddDate AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmModDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndModDate AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstimateWmargRep.

    IF prmUser       = ?  THEN ASSIGN prmUser       = "".
    IF prmAction     = ?  THEN ASSIGN prmAction     = "".
    IF prmBegCust    = ?  THEN ASSIGN prmBegCust    = "".
    IF prmEndCust    = ?  THEN ASSIGN prmEndCust    = "".
    IF prmBegSman    = ?  THEN ASSIGN prmBegSman    = "".
    IF prmEndSman    = ?  THEN ASSIGN prmEndSman    = "".
    IF prmBegEst     = ?  THEN ASSIGN prmBegEst     = "".
    IF prmEndEst     = ?  THEN ASSIGN prmEndEst     = "".
    IF prmAddDate    = ?  THEN ASSIGN prmAddDate    = "".
    IF prmEndAddDate = ?  THEN ASSIGN prmEndAddDate = "".
    IF prmModDate    = ?  THEN ASSIGN prmModDate    = "".
    IF prmEndModDate = ?  THEN ASSIGN prmEndModDate = "".
    
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_date-2 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-estmar.csv"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO NO-UNDO. 

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
DEF STREAM st-excel.
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


FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCust NO-ERROR.
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
IF prmAction = "magan" THEN DO:
    assign
    v-today       =   TODAY
    begin_cust-no =   prmBegCust
    begin_date    =   DATE(prmAddDate)
    begin_date-2  =   DATE(prmModDate)
    begin_est     =   prmBegEst
    begin_slsmn   =   prmBegSman   
    end_cust-no   =   prmEndCust   
    end_date      =   DATE(prmEndAddDate) 
    end_date-2    =   DATE(prmEndModDate) 
    end_est       =   prmEndEst   
    end_slsmn     =   prmEndSman     .
   
   tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

   assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "ListMargin" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "ListMargin" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "ListMargin" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

run run-report.
   
    CREATE ttEstimateWmargRep.
    IF tb_excel  THEN
        ASSIGN ttEstimateWmargRep.wmarginsfile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttEstimateWmargRep.wmarginsfile = vTextFile .
END.

PROCEDURE run-report :
{sys/form/r-topw.f}

DEF VAR fest LIKE est.est-no NO-UNDO.
DEF VAR test LIKE fest       NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-box-size LIKE quoteitm.size NO-UNDO.
DEF VAR lv-die-size LIKE quoteitm.size NO-UNDO.
DEF VAR lv-format AS CHAR NO-UNDO.
DEF VAR li-colors AS INT NO-UNDO.
DEF VAR li-qty LIKE probe.est-qty NO-UNDO.
DEF VAR ld-costm LIKE probe.full-cost NO-UNDO.
DEF VAR ld-costt AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR ld-price LIKE probe.sell-price NO-UNDO.
DEF VAR ld-mar AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 7 NO-UNDO.
DEF VAR ld-pct AS DEC NO-UNDO.
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO.


SESSION:SET-WAIT-STATE ("general").    
    
ASSIGN
 str-tit2 = "Estimates List w/Margins"
 {sys/inc/ctrtext.i str-tit2 112}

 fest = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.


{sys/inc/outprint.i  VALUE(lines-per-page)}

IF tb_excel THEN do:
  OUTPUT STREAM st-excel TO VALUE(fi_file).
  PUT STREAM st-excel UNFORMATTED
      ",,,,Carton,Die,,,Carton,,,,,,FOB Variable Profit Margins,,,,,," +
      ",Total,Total,Total,Delivered"
      SKIP
      "Est#,Item Description,Bd,Cal,Size,Size,#Up,# of Col,Quantity,F" +
      "OB Cost/M,FOB Total Cost,15%,20%,25%,30%,35%,40%,45%,FOB Price" +
      "/M,FOB Gross Rev.,FOB Gross Mar.,Plate Cost,Die Cost,Freight C" +
      "ost,Selling Price"
      SKIP.
END.


VIEW FRAME r-top.

FOR EACH est
    WHERE est.company  EQ cocode
      AND est.est-no   GE fest
      AND est.est-no   LE test
      AND est.est-date GE begin_date
      AND est.est-date LE end_date
      AND est.mod-date GE begin_date-2
      AND est.mod-date LE end_date-2
    NO-LOCK,
 
    FIRST est-qty
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    NO-LOCK,

    EACH eb
    WHERE eb.company  EQ est.company
      AND eb.est-no   EQ est.est-no
      AND eb.cust-no  GE begin_cust-no
      AND eb.cust-no  LE end_cust-no
      AND eb.sman     GE begin_slsmn
      AND eb.sman     LE end_slsmn
      AND LOOKUP(eb.cust-no,custcount) <> 0
      AND (eb.form-no EQ 0 OR (eb.est-type NE 2 AND eb.est-type NE 6))
    NO-LOCK,
    
    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK,
    
    EACH probe
    WHERE probe.company   EQ est.company
      AND probe.est-no    EQ est.est-no
      AND probe.full-cost NE ?
    NO-LOCK

    BREAK BY est.est-no
          BY probe.est-qty
          BY probe.probe-date
          BY probe.probe-time:

  IF LAST-OF(probe.est-qty) THEN DO:
    RUN sys/inc/calcsize.p (ROWID(eb), OUTPUT lv-box-size).

    li-colors = 0.

    IF est.est-type LE 4 THEN DO:
      ASSIGN
       lv-format   = ">>>>>9.9<<<<"
       lv-die-size = TRIM(STRING(ef.trim-w,lv-format)) + " x " +
                     TRIM(STRING(ef.trim-l,lv-format)).

      DO li = 1 TO EXTENT(eb.i-code2):
        IF TRIM(eb.i-code2[li]) NE "" THEN li-colors = li-colors + 1.
      END.
    END.

    ELSE DO:
      ASSIGN
       lv-format   = ">>>>9.99"
       lv-die-size = TRIM(STRING({sys/inc/k16v.i ef.trim-w},lv-format)) + " x " +
                     TRIM(STRING({sys/inc/k16v.i ef.trim-l},lv-format)).

      DO li = 1 TO EXTENT(eb.i-code):
        IF TRIM(eb.i-code[li]) NE "" THEN li-colors = li-colors + 1.
      END.
    END.

    RELEASE probeit.
    IF est.est-type EQ 3 OR est.est-type EQ 4 OR
       est.est-type EQ 7 OR est.est-type EQ 8 THEN
      FIND FIRST probeit NO-LOCK
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.line
            AND probeit.part-no EQ eb.part-no
          NO-ERROR.

    IF AVAIL probeit THEN
      ASSIGN
       li-qty   = IF probeit.yrprice THEN probeit.yld-qty ELSE probeit.bl-qty
       ld-costm = probeit.full-cost
       ld-price = probeit.sell-price.
    ELSE
      ASSIGN
       li-qty   = probe.est-qty
       ld-costm = probe.full-cost
       ld-price = probe.sell-price.
    
    ld-costt = li-qty / 1000 * ld-costm.

    ld-pct = .85.
    DO li = 1 TO EXTENT(ld-mar):
      ASSIGN
       ld-mar[li] = (ld-costt / ld-pct * 1.01) - ld-costt
       ld-pct     = ld-pct - .05.
    END.

    DISPLAY TRIM(eb.est-no)       FORMAT "x(8)"
                                  COLUMN-LABEL "Est#"
            eb.part-dscr1         COLUMN-LABEL "Item Description"
            ef.board              COLUMN-LABEL "Bd"
            ef.cal                COLUMN-LABEL "Cal"
            lv-box-size           COLUMN-LABEL "Carton Size"
            lv-die-size           COLUMN-LABEL "Die Size"
            eb.num-up             COLUMN-LABEL "Number Up!  On Die"
            li-colors             COLUMN-LABEL "# of Col"
            li-qty                COLUMN-LABEL " Carton!Quantity"
            ld-costm              COLUMN-LABEL "FOB Cost/M"
            ld-costt              COLUMN-LABEL "FOB Total Cost"
            ld-mar[1]             COLUMN-LABEL "              !           15%"
            ld-mar[2]             COLUMN-LABEL "              !           20%"
            ld-mar[3]             COLUMN-LABEL "           FOB!           25%"
            ld-mar[4]             COLUMN-LABEL "      Variable!           30%"
            ld-mar[5]             COLUMN-LABEL "        Profit!           35%"
            ld-mar[6]             COLUMN-LABEL "       Margins!           40%"
            ld-mar[7]             COLUMN-LABEL "              !           45%"
             
           WITH FRAME est DOWN NO-BOX STREAM-IO WIDTH 300.
      
    IF tb_excel THEN
      PUT STREAM st-excel UNFORMATTED
          '"'   TRIM(eb.est-no)     '",'
          '"'   eb.part-dscr1       '",'
          '"'   eb.procat           '",'
          '"'   ef.cal              '",' 
          '"'   lv-box-size         '",' 
          '"'   lv-die-size         '",'
          '"'   eb.num-up           '",'
          '"'   li-colors           '",' 
          '"'   li-qty              '",'    
          '"'   ld-costm            '",' 
          '"'   ld-costt            '",'  
          '"'   ld-mar[1]           '",' 
          '"'   ld-mar[2]           '",'            
          '"'   ld-mar[3]           '",'             
          '"'   ld-mar[4]           '",'        
          '"'   ld-mar[5]           '",'       
          '"'   ld-mar[6]           '",'        
          '"'   ld-mar[7]           '",'          
          '"'                       '",'           
          '"'                       '",'           
          '"'                       '",'           
          '"'                       '",'            
          '"'                       '",'            
          '"'                       '",'           
          '"'                       '",'
          SKIP.
  END.
END.

OUTPUT STREAM st-excel CLOSE.

end procedure.
