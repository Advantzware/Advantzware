


/*------------------------------------------------------------------------
    File        : OrdNoRep.p
    Purpose     :  Print Order NO

    Syntax      :

    Description : Return a Dataset of Request For Order No

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
{sys/inc/var.i new shared}
DEFINE TEMP-TABLE ttOrderNoRep NO-UNDO
FIELD vNoFile AS CHAR
FIELD aNo AS CHAR.
DEFINE DATASET dsOrderNoRep FOR ttOrderNoRep .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmOrderNoAct AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginOrd     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrd       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust      AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeOrdDate    AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrdDate   AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER prmPrintQty     AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmPriCont      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut          AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderNoRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmBeginOrd  = ?  THEN ASSIGN prmBeginOrd = 0.
    IF prmEndOrd  = ?  THEN ASSIGN prmEndOrd = 0.
    IF prmBeginCust   = ?   THEN ASSIGN prmBeginCust = "".
    IF prmEndCust = ? THEN ASSIGN prmEndCust = "".  
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem = "".
    IF prmEndItem  = ?  THEN ASSIGN prmEndItem = "".
    IF prmPrintQty  = ?  THEN ASSIGN prmPrintQty = "".
    IF prmPriCont  = ?  THEN ASSIGN prmPriCont = "".
    assign prmEndCust = prmBeginCust.
MESSAGE "test1"  prmUser prmBeginOrd prmEndOrd prmBeginCust prmEndCust prmBeginItem  prmEndItem  prmBeOrdDate prmPrintQty prmPriCont.
 def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE ExtList NO-UNDO
  FIELD ord-no       LIKE oe-ord.ord-no
  FIELD est-no       LIKE oe-ord.est-no 
  FIELD job-no       AS CHARACTER
  FIELD ord-date     LIKE oe-ord.ord-date
  FIELD cust-no      LIKE oe-ord.cust-no 
  FIELD cust-name    LIKE oe-ord.cust-name 
  FIELD i-no         LIKE oe-ordl.i-no     /* misc charge */
  FIELD i-name       LIKE oe-ordl.i-name   /* description */
  FIELD qty-lft      LIKE oe-ordl.qty 
  FIELD cost         LIKE oe-ordl.cost 
  FIELD price        LIKE oe-ordl.price
  FIELD pr-uom       LIKE oe-ordl.pr-uom
  FIELD ext-price    AS CHARACTER  /* price */
  FIELD margin       AS CHARACTER.


DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U         NO-UNDO.
DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL  "C:\Inetpub\wwwroot\pdfs\orderNum.csv" */ NO-UNDO.
DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Ordered" NO-UNDO.
DEFINE VARIABLE tb_contr AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
def new shared var v-fr-tax like oe-ctrl.f-tax.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.
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

IF prmOrderNoAct = "OrderNo" THEN DO:

    assign
       v-today = TODAY . 

    assign
       
         init-dir    = v-webrootpath
        fi_file = init-dir + "OrderNum" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "OrderNum" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".    

         DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "OrderNum" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".
    
       ASSIGN 
           tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
    run run-report.
    /*RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").*/
    CREATE ttOrderNoRep.
     IF tb_excel  THEN
    ASSIGN ttOrderNoRep.vNoFile = vPdfFile.
      IF NOT  tb_excel  THEN
    ASSIGN ttOrderNoRep.vNoFile = vTextFile.
END.

PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/orders.p 11/99 JLF */
/* Orders Report                                                              */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer b-oe-ordl for oe-ordl.

def var v-cust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-ord-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-date as date format "99/99/9999" extent 2 init [today,today].
def var v-item as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ord-qty as log format "Ordered/Remaining" init yes.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var v-tot-ord as dec format "->,>>>,>>9.99" extent 2.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-inv as log init no.
def var v-ship as log init no.
def var v-tot-tax like oe-ord.tax.
def var v-tot-freight like oe-ord.t-freight.
def var v-qty-lft like oe-ordl.qty.
def var v-ext-price like oe-ordl.t-price.
def var v-prt-cont as log init no.
def var v-margin as dec.
def var v-margin-tot as dec format "->>>,>>>,>>>,>>9.99".
def var v-password like sys-ctrl.char-fld label "Please Enter Password".
def var v-ext-cost as dec.
DEF VAR excelheader AS CHAR NO-UNDO.

format header
  "Order#  Est#         Job#     Date       Cust#    Name" skip
  fill("-",105) format "x(105)"
  with frame r-top .

format  /* frame ord */
   oe-ord.ord-no
   oe-ord.est-no format "x(8)"
   oe-ord.job-no space(0) "-" space(0)
   oe-ord.job-no2 format "99"
   space(3) 
   oe-ord.ord-date
   space(3) 
   oe-ord.cust-no 
   oe-ord.cust-name skip
  WITH frame ord no-labels no-box no-underline stream-io width 132.

FORMAT  /* frame ord1 */
    oe-ordl.i-no                          label "Item"  at 10
    oe-ordl.i-name format "x(25)"         label "Description"
    v-qty-lft      format "->>>,>>9"      label "Quantity"
    oe-ordl.cost   format "->>>,>>9.99"   label "Cost/M"
    oe-ordl.price  format "->>>,>>9.99"   label "Price"
    oe-ordl.pr-uom                        label "UOM"
    v-ext-price    format "->,>>>,>>9.99" label "Ext Price"
    v-margin       format "->,>>>,>>9.99" label "Margin$"
  WITH frame ordl no-labels no-box down stream-io width 132.

format /* frame ordl2 */
    oe-ordl.i-no                          label "Item"  at 10
    oe-ordl.i-name format "x(25)"         label "Description"
    v-qty-lft      format "->>>,>>9"      label "Quantity"
    oe-ordl.cost   format "->>>,>>9.99"   label "Cost/M"
    oe-ordl.price  format "->>>,>>9.99"   label "Price"
    oe-ordl.pr-uom                        label "UOM" 
    v-ext-price    format "->,>>>,>>9.99" label "Ext Price"
  WITH frame ordl2  no-labels no-box down stream-io width 132.

format /* frame ordm */
    oe-ordm.charge                       label "Charge"      at 10
    oe-ordm.dscr                         label "Description"
    oe-ordm.amt    format "->>>,>>9.99"  label "Price"       to 102 skip
  WITH frame ordm no-labels no-box down stream-io width 132.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-fr-tax = oe-ctrl.f-tax.

assign
 str-tit2 = "Order Booked By Order No"
 {sys/inc/ctrtext.i str-tit2 112}  

 v-ord-no[1] = prmBeginOrd
 v-ord-no[2] = prmEndOrd
 v-cust[1]   = prmBeginCust
 v-cust[2]   = prmEndCust
 v-item[1]   = prmBeginItem
 v-item[2]   = prmEndItem
 v-date[1]   = prmBeOrdDate
 v-date[2]   = prmEndOrdDate
 v-ord-qty   = IF   prmPrintQty = "Yes" THEN TRUE ELSE FALSE.
v-prt-cont   = IF prmPriCont = "yes" THEN TRUE ELSE FALSE.

/*IF v-prt-cont AND NOT security-flag THEN DO:
  RUN passwd (3, OUTPUT security-flag).
  v-prt-cont = security-flag.
END.*/

/*{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}*/


 if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .
      
      {sys/inc/outprint.i value(lines-per-page)}


IF tb_excel THEN 
DO:
   excelheader = "Order#,Est#,Job#,Date,Cust,Name,".
   IF v-prt-cont THEN
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price,Margin$".
   ELSE
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price".

  OUTPUT STREAM excel TO VALUE(fi_file).
  
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
display "" with frame r-top.

EMPTY TEMP-TABLE ExtList.

for each oe-ord
    where oe-ord.company  eq cocode
      and oe-ord.ord-no   ge v-ord-no[1]
      and oe-ord.ord-no   le v-ord-no[2]
      and oe-ord.cust-no  ge v-cust[1]
      and oe-ord.cust-no  le v-cust[2]
      and oe-ord.ord-date ge v-date[1]
      and oe-ord.ord-date le v-date[2]
      and (oe-ord.stat    eq "N" or
           oe-ord.stat    eq "A" or
           oe-ord.stat    eq "U" or
           oe-ord.stat    eq "H" or
           oe-ord.stat    eq "D" or
           oe-ord.stat    eq "C" or
           oe-ord.stat    eq "P")
      and oe-ord.type     ne "T"
      AND LOOKUP(oe-ord.cust-no, custcount) <> 0
    use-index ord-no no-lock,

    first b-oe-ordl
    where b-oe-ordl.company eq cocode
      and b-oe-ordl.ord-no  eq oe-ord.ord-no
      and b-oe-ordl.i-no    ge v-item[1]
      and b-oe-ordl.i-no    le v-item[2]
    no-lock,

    first cust
    WHERE cust.company = prmComp
      and cust.cust-no eq oe-ord.cust-no
    no-lock

    break by oe-ord.ord-no:

    display 
        oe-ord.ord-no
        trim(oe-ord.est-no)   @ oe-ord.est-no
        oe-ord.job-no
        oe-ord.job-no2
        oe-ord.ord-date       format "99/99/9999"
        oe-ord.cust-no
        oe-ord.cust-name
      with frame ord.
    
    v-tot-ord[1] = 0.

    for each oe-ordl
       where oe-ordl.company eq oe-ord.company
         and oe-ordl.ord-no  eq oe-ord.ord-no
         and oe-ordl.i-no    ge v-item[1]
         and oe-ordl.i-no    le v-item[2]
      no-lock
      break by oe-ordl.ord-no:

        if first-of(oe-ordl.ord-no) then put skip(1).

        assign
          v-ship      = oe-ordl.stat ne "I" and oe-ordl.stat ne "B"
          v-qty-lft   = oe-ordl.qty - (if v-ord-qty then 0 else oe-ordl.inv-qty)
          v-ext-price = 0.

        if v-qty-lft lt 0 then v-qty-lft = 0.  

        if oe-ordl.pr-uom begins "L" then
          v-ext-price = 
            oe-ordl.price - round((oe-ordl.price * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom eq "CS" then 
        do:
            find first itemfg WHERE itemfg.company = prmComp
                   and itemfg.i-no eq oe-ordl.i-no
                no-lock no-error.

            if avail itemfg and itemfg.case-count ne 0 then
              v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                      round((((v-qty-lft / itemfg.case-count) *
                              oe-ordl.price) * oe-ordl.disc) / 100, 2).
            else
              v-ext-price = (v-qty-lft * oe-ordl.price) -
                      round(((v-qty-lft * oe-ordl.price) *
                             oe-ordl.disc) / 100, 2).
        end.
        else if oe-ordl.pr-uom eq "C" then
          v-ext-price = ((v-qty-lft / 100) * oe-ordl.price) -
                    round((((v-qty-lft / 100) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).

        else if oe-ordl.pr-uom eq "EA" then
          v-ext-price = (v-qty-lft * oe-ordl.price) -
                    round(((v-qty-lft * oe-ordl.price) *
                           oe-ordl.disc) / 100, 2).

        else /** DEFAULT PER THOUSAND **/
          v-ext-price = ((v-qty-lft / 1000) * oe-ordl.price) -
                    round((((v-qty-lft / 1000) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).

       /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                    (round(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

       /** CALCULATE TAX CHARGES **/
        if oe-ordl.tax and v-tax-rate gt 0 then
          v-tot-tax = v-tot-tax + round((v-ext-price * v-tax-rate) / 100,2).

        if v-prt-cont then 
          assign
            v-ext-cost = (oe-ordl.cost * oe-ordl.qty) / 1000
            v-margin = v-ext-price - v-ext-cost.

        if v-prt-cont then 
        do:
            display 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
                v-margin 
              with frame ordl.
            down with frame ordl.
            
            IF tb_excel THEN
            DO:

               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price))
                 ExtList.margin    = TRIM(STRING(v-margin)).
            END.
        end.
        else 
        do:
            display 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
              with frame ordl2.
            down with frame ordl2.

            IF tb_excel THEN
            DO:
               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price)).
            END.
        end.     

        if v-prt-cont then 
        do:
            if v-margin ne ? then 
              v-margin-tot = v-margin-tot + v-margin.
        end.

        v-tot-ord[1] = v-tot-ord[1] + v-ext-price.
    end. /* each oe-ordl */

    for each oe-ordm
       where oe-ordm.company eq oe-ord.company
         and oe-ordm.ord-no  eq oe-ord.ord-no
       no-lock
      break by oe-ordm.ord-no:

        if first-of(oe-ordm.ord-no) then put skip(1) "Miscellaneous" at 10 skip.

        display 
            oe-ordm.charge oe-ordm.dscr oe-ordm.amt
          with frame ordm.

        if oe-ordm.bill eq "N" then
          display "       N/C" @ oe-ordm.amt with frame ordm.

        down with frame ordm.

        IF tb_excel THEN
        DO:
            CREATE ExtList.
            ASSIGN
              ExtList.ord-no    = oe-ord.ord-no
              ExtList.est-no    = TRIM(oe-ord.est-no)
              ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                  + TRIM(STRING(oe-ord.job-no2, "99"))
              ExtList.ord-date  = oe-ord.ord-date
              ExtList.cust-no   = oe-ord.cust-no
              ExtList.cust-name = oe-ord.cust-name
              ExtList.i-no      = oe-ordm.charge
              ExtList.i-name    = oe-ordm.dscr
              ExtList.ext-price = IF oe-ordm.bill eq "N"
                                  THEN "N/C"  
                                  ELSE TRIM(STRING(oe-ordm.amt)).
        END.
       
        if oe-ordm.bill eq "Y" then 
        do:
            v-tot-ord[1] = v-tot-ord[1] + oe-ordm.amt.

            if oe-ordm.tax and v-tax-rate eq 0 then
              v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        end.
    end. /* each oe-ordm */

    put skip "------------" to 102 skip.

    if oe-ord.stat eq "H" then
      put "** THIS ORDER IS ON CREDIT HOLD **" to 50 .
    else if oe-ord.stat eq "D" then
      put "** THIS ORDER IS DELETED **" to 50 .
    else if oe-ord.stat eq "C" then
      put "** THIS ORDER IS CLOSED **" to 50 .

    IF tb_excel AND LOOKUP(oe-ord.stat, "H,D,C") NE 0 THEN
    DO:
        CREATE ExtList.
        ASSIGN
          ExtList.ord-no    = oe-ord.ord-no
          ExtList.est-no    = TRIM(oe-ord.est-no)
          ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                              + TRIM(STRING(oe-ord.job-no2, "99"))
          ExtList.ord-date  = oe-ord.ord-date
          ExtList.cust-no   = oe-ord.cust-no
          ExtList.cust-name = oe-ord.cust-name.
        
        CASE oe-ord.stat:
            WHEN "H" THEN
              ExtList.i-name  = "** THIS ORDER IS ON CREDIT HOLD **".
            WHEN "D" THEN
               ExtList.i-name = "** THIS ORDER IS DELETED **".
            WHEN "C" THEN
               ExtList.i-name = "** THIS ORDER IS CLOSED **".
        END CASE.
    END.

    put "Total Order" at 75 v-tot-ord[1] to 102 skip(1).

    v-tot-ord[2] = v-tot-ord[2] + v-tot-ord[1].
    
    if last(oe-ord.ord-no) then 
    do:
        if v-prt-cont then 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 112 skip.
            put "Grand Total Contribution" at 62 v-margin-tot to 112 skip(1).
        end.
        else 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 102 skip(1).
        end.
    end.
end. /* each oe-ord */

/* if they're not here, they don't go.. */
FOR EACH ExtList:
    EXPORT STREAM excel DELIMITER "," Extlist.
END.



end procedure.
/********************************************************************************************************/

PROCEDURE passwd:
DEF INPUT  PARAMETER ip-type      AS INT         NO-UNDO.
DEF OUTPUT PARAMETER op-validated AS LOG INIT NO NO-UNDO.


IF ip-type EQ 5 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "JOBPASS"
    NO-LOCK NO-ERROR.

ELSE
IF ip-type EQ 2 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "CUSTPASS"
    NO-LOCK NO-ERROR.

ELSE
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "SECURITY"
    NO-LOCK NO-ERROR.
    
IF AVAIL sys-ctrl THEN DO:
  IF ip-type EQ 3 AND NOT sys-ctrl.log-fld THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF ip-type = 4 AND sys-ctrl.int-fld <> 1 THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF (ip-type = 1 OR ip-type = 2 OR ip-type = 5) AND
     NOT sys-ctrl.log-fld THEN DO:
    op-validated = NO.
    RETURN.
  END.
END.

/*ELSE DO:
  MESSAGE "No System Control record exists for " +
          IF ip-type EQ 5 THEN "JOBPASS"  ELSE
          IF ip-type EQ 2 THEN "CUSTPASS" ELSE "SECURITY..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN.*/
END.
