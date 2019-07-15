/*------------------------------------------------------------------------
    File        : CommissionRep.p
    Purpose     : Commission Report

    Syntax      :

    Description : Return a Dataset of Commission Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttYtdcstRep NO-UNDO
    FIELD ytdcst AS CHAR.
DEFINE DATASET dsYtdcstRep FOR ttYtdcstRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeWare     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWare    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCust     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeItem     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustOwned  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsYtdcstRep.
    IF prmUser      = ?  THEN ASSIGN prmUser      = "".
    IF prmAction    = ?  THEN ASSIGN prmAction    = "".
    IF prmBeWare    = ?  THEN ASSIGN prmBeWare    = "".
    IF prmEndWare   = ?  THEN ASSIGN prmEndWare   = "".
    IF prmBeCust    = ?  THEN ASSIGN prmBeCust    = "".
    IF prmEndCust   = ?  THEN ASSIGN prmEndCust   = "".
    IF prmBeItem    = ?  THEN ASSIGN prmBeItem    = "".
    IF prmEndItem   = ?  THEN ASSIGN prmEndItem   = "".
    IF prmCustOwned = ?  THEN ASSIGN prmCustOwned = "".
    IF prmOut       = ?  THEN ASSIGN prmOut       = "".
    
    
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no NO-UNDO.

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
    v-today     =   TODAY
    begin_cust  =   prmBeCust 
    begin_i-no  =   prmBeItem   
    begin_whse  =   prmBeWare 
    end_cust    =   prmEndCust
    end_i-no    =   prmEndItem
    end_whse    =   prmEndWare .
    
   tb_inc-cust = IF prmCustOwned = "True" THEN TRUE ELSE FALSE    . 
   tb_excel    = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
   assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "finishgood" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "finishgood" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "finishgood" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

run run-report.
   
    CREATE ttYtdcstRep.
    IF tb_excel  THEN
        ASSIGN ttYtdcstRep.ytdcst = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttYtdcstRep.ytdcst = vTextFile .
END.

PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-ystat.p 9/91 cd */
/*  finished goods ytd inventory status                                       */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-cost like itemfg.avg-cost NO-UNDO.
def var v-ext   as DEC NO-UNDO.
def var v-ext2  as DEC NO-UNDO.
def var v-subext as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var v-cust-no as ch extent 2 initial [" ", "ZZZZZZZZZ"] NO-UNDO. /* DAR */
def var v-i-no like itemfg.i-no extent 2 initial [" ", "ZZZZZZZZZZZZZZZ"] NO-UNDO.
def var v-qty-onh like itemfg.q-onh NO-UNDO.
def var v-cust-qty like itemfg.q-onh NO-UNDO.
def var v-loc like fg-bin.loc extent 2 initial [" ","ZZZZZ"] NO-UNDO.
def var v-custown as logical format "Y/N" initial "N" NO-UNDO.
def var v-print as logical initial "N" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

form
    itemfg.i-no column-label " ITEM!NUMBER"
    itemfg.i-name label "DESCRIPTION"   format "x(15)"
    itemfg.cust-no column-label "CUSTOMER!   ID"
    itemfg.beg-bal column-label "BEGINNING!BALANCE" format "->>>,>>>,>>9"
    v-cost column-label "ITEM!COST"  format ">>>9.99"
    itemfg.q-prod-ytd column-label "QUANTITY!PRODUCED" format "->>>,>>>,>>9"
    itemfg.q-ship-ytd column-label "QUANTITY!SHIPPED"  format "->>>,>>>,>>9"
    itemfg.q-adj-ytd  column-label "QUANTITY!ADJUSTED"  format "->>>,>>>,>>9"
    v-qty-onh column-label "QUANTITY!ON-HAND" format "->>>,>>>,>>9"
    v-ext column-label "TOTAL!VALUE"  format "->>>,>>>,>>9.99"
with no-box frame itemx down STREAM-IO width 132.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "FG ITEM NUMBER,DESCRIPTION,CUSTOMER ID,BEGINNING BALANCE,"
              + "ITEM COST,QUANTITY PRODUCED,QUANTITY SHIPPED,QUANTITY ADJUSTED,"
              + "QUANTITY ON-HAND,TOTAL VALUE".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

assign
 str-tit2 = "Finished Goods Sales Value By Customer By Tag"
 {sys/inc/ctrtext.i str-tit2 112}

 v-loc[1]       = begin_whse
 v-loc[2]       = end_whse
 v-cust-no[1]   = begin_cust
 v-cust-no[2]   = end_cust
 v-i-no[1]      = begin_i-no
 v-i-no[2]      = end_i-no
 v-custown      = tb_inc-cust.

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .
      
{sys/inc/outprint.i value(lines-per-page)}

display "" with frame r-top.

find fg-ctrl where fg-ctrl.company = cocode NO-LOCK.

for each itemfg no-lock where
              itemfg.company = cocode  and
             (itemfg.cust-no >= v-cust-no[1] and
              itemfg.cust-no <= v-cust-no[2]) and
             (itemfg.i-no    >= v-i-no[1] and  /* DAR */
              itemfg.i-no    <= v-i-no[2]) AND 
              LOOKUP(itemfg.cust-no, custcount) <> 0
              use-index customer
              break by itemfg.cust-no by itemfg.i-no:
         if fg-ctrl.inv-meth = "a" then v-cost = itemfg.avg-cost.
         else v-cost = itemfg.last-cost.
       if first-of(itemfg.cust-no) then do:
         put skip(1).
         assign v-subext = 0.
       end.

       if line-counter >=56 then page.

       assign v-qty-onh = 0
              v-ext  = 0.

FOR each fg-bin where fg-bin.company = itemfg.company and
             fg-bin.i-no = itemfg.i-no and
             fg-bin.loc >= v-loc[1] and fg-bin.loc <= v-loc[2]
             NO-LOCK
             use-index co-ino break by fg-bin.loc:

         if fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "" then
          assign v-qty-onh = v-qty-onh + fg-bin.qty.
         else assign v-cust-qty = v-cust-qty + fg-bin.qty.

         if last(fg-bin.loc) then do:
           if v-custown then v-qty-onh = v-qty-onh + v-cust-qty.

           if itemfg.prod-uom eq "L" then v-ext = v-cost.

           else
           if itemfg.prod-uom eq "CS" and
              itemfg.case-count ne 0  then
              v-ext = (v-qty-onh * v-cost) / itemfg.case-count.

           else do:
              v-ext = v-qty-onh * v-cost.

              find first uom
                  where uom.uom  eq itemfg.prod-uom
                    and uom.mult ne 0
                  no-lock no-error.
              if avail uom then v-ext = v-ext / uom.mult.
           end.

           /**if v-ext < 0 then assign v-ext = 0.**/

           display  itemfg.i-no
                    itemfg.i-name
                    itemfg.cust-no
                    itemfg.beg-bal
                    v-cost
                    itemfg.q-prod-ytd
                    itemfg.q-ship-ytd
                    itemfg.q-adj-ytd v-qty-onh
                    v-ext
           with frame itemx.

           IF tb_excel THEN 
             PUT STREAM excel UNFORMATTED
                 '"' itemfg.i-no                                   '",'
                 '"' itemfg.i-name                                 '",'
                 '"' itemfg.cust-no                                '",'
                 '"' STRING(itemfg.beg-bal,"->>>,>>>,>>9")         '",'
                 '"' STRING(v-cost,">>>9.99")                      '",'
                 '"' STRING(itemfg.q-prod-ytd,"->>>,>>>,>>9")      '",'
                 '"' STRING(itemfg.q-ship-ytd,"->>>,>>>,>>9")      '",'
                 '"' STRING(itemfg.q-adj-ytd,"->>>,>>>,>>9")       '",'
                 '"' STRING(v-qty-onh,"->>>,>>>,>>9")              '",'
                 '"' STRING(v-ext,"->>>,>>>,>>9.99")               '",'
                 SKIP.

           assign v-subext = v-subext + v-ext
                  v-ext2 = v-ext2 + v-ext
                  v-qty-onh = 0
                  v-cust-qty = 0
                  v-print = yes.
           down with frame itemx.
         end.
       end. /* each bin */

    if last-of(itemfg.cust-no) and v-print then do:
         put "--------------" to 129 skip.
         put "Customer Total" at 89 v-subext to 129 skip(1).
         assign v-subext = 0
                v-print = no.
       end.
        
     end. /* each itemfg */

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
END.

end procedure.
