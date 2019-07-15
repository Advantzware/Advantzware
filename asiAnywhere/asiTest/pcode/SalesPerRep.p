


/*------------------------------------------------------------------------
    File        : SalesPerRep.p
    Purpose     :  Salesman performance

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
{sys/inc/var.i new shared}
 {custom/xprint.i}

DEFINE TEMP-TABLE ttSalesmanRep NO-UNDO
    FIELD vsalesman AS CHARACTER .
DEFINE DATASET dsSalesmanRep FOR ttSalesmanRep.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSalesAct AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBeSales AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSales AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut      AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSalesmanRep.
    DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

    IF prmUser     = ? THEN ASSIGN prmUser = "".
    IF prmBeSales  = ? THEN ASSIGN prmBeSales = "".
    IF prmEndSales = ? THEN ASSIGN prmEndSales = "".

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL  "C:\Inetpub\wwwroot\pdfs\SalesPerformance.csv"*/ NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/*
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}  */

assign
 cocode = prmComp
 locode = usercomp.loc .

def workfile w-data
  field w-sman-no  as   char format "x(3)"
  field w-sname    as   char format "x(19)"
  field w-sqft     like itemfg.t-sqft
  field w-amt      as   dec format "->,>>>,>>>,>>9.99" 
  field w-ptd-sqft like w-sqft
  field w-ptd-amt  like w-amt
  field w-ytd-sqft like w-sqft format "->,>>>,>>>,>>9.99"
  field w-ytd-amt  like w-amt.
  

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEFINE VAR vTextFile AS CHAR NO-UNDO.

DEFINE STREAM excel.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
IF prmSalesAct = "Salesman" THEN DO:
    
    ASSIGN 
        v-today = TODAY 
        begin_ord-date  = prmBeDate 
        begin_slsmn     = prmBeSales
        end_slsmn       = prmEndSales  .
     
    ASSIGN tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.

 
    assign
        init-dir    = v-webrootpath
        fi_file = init-dir + "SalesPerformance" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "SalesPerformance" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
      
        vTextFile  = "SalesPerformance" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".
 
              run run-report.
    CREATE ttSalesmanRep.
 IF   tb_excel  THEN  DO:
     ASSIGN ttSalesmanRep.vsalesman = vPdfFile.
 END.
 IF NOT tb_excel  THEN
         ASSIGN ttSalesmanRep.vsalesman = vTextFile.
END.

/***********************************************************/
PROCEDURE run-report :


{sys/form/r-topw.f}

def var fdate as date format "99/99/9999".
def var tdate as date format "99/99/9999".
def var fsman as char format "x(3)".
def var tsman as char format "x(3)".
def var v-sq-ft like itemfg.t-sqft  format "->>>,>>>,>>9.999".
def var v-gtot-sqft      like w-sqft.
def var v-gtot-amt       like w-amt.
def var v-gtot-ptd-sqft  like v-gtot-sqft.
def var v-gtot-ptd-amt   like v-gtot-amt.
def var v-gtot-ytd-sqft  like v-gtot-sqft.
def var v-gtot-ytd-amt   like v-gtot-amt.
def var v-sman-no like w-sman-no.
def var v-sqft like itemfg.t-sqft.
def var v-amt  like oe-ord.t-revenue.
def var v-ptd-first as date.
def var v-ptd-last  as date.
def var v-ytd-first as date.
def var v-ytd-last  as date.
def var v-period as int.
def var v-year as int format "9999".
def var v-qty like oe-ordl.qty format "->>>,>>9.99".
def var cnt as int.
def var v-exclude as log.
def var v-pct as dec format "99.99".
def var v-misc as log.

DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

format w-sman-no    column-label "No"
       w-sname      column-label "Name"
       w-sqft       column-label "Sq Ft/M"
       w-amt        column-label "Amount"
       space(2)
       w-ptd-sqft   column-label "Sq Ft/M"
       w-ptd-amt    column-label "Amount"
       space(2)
       w-ytd-sqft   column-label "Sq Ft/M"
       w-ytd-amt    column-label "Amount"

    header skip(1)
           "    Sales Rep"
           "---------Daily-----------" at 25 space(2)
           "-----Period to date------" space(2)
           "-------Year to date------"
           
    with no-box frame itemx down stream-io width 132.
    
assign
 fdate      = today
 tdate      = today
 v-ytd-last = today.

assign
 str-tit2 = "Sales Rep Performance"
 {sys/inc/ctrtext.i str-tit2 112}
 
 fdate = begin_ord-date
 fsman = begin_slsmn
 tsman = end_slsmn.
 
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}


FOR EACH w-data:
    DELETE w-data.
END.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = 
  "No,Sales Rep Name,Daily Sq Ft/M,Amount,PTD Sq Ft/M,Amount,YTD Sq Ft/M,Amount".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

    find first period
        where period.company eq cocode
          and period.pst     le tdate
          and period.pend    ge tdate
        no-lock no-error.
    if avail period then
      assign
       v-ptd-first = period.pst
       v-ptd-last  = period.pend
       v-period    = period.pnum
       v-year      = period.yr.
    else
      assign
       v-ptd-first = tdate
       v-ptd-last  = tdate
       v-period    = month(today) 
       v-year      = year(today).

    find first company where company.company eq cocode no-lock.

    find first period
        where period.company eq cocode
          and period.yr      eq v-year
        no-lock no-error.
        
    assign
     v-ytd-first = if avail period then period.pst
                                   else date(1,1,year(tdate))
     v-ytd-last  = tdate.

    for each oe-ord
        where oe-ord.company  eq cocode
          and oe-ord.ord-date ge v-ytd-first
          and oe-ord.ord-date le v-ytd-last,
         
        each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
        no-lock:

      v-exclude = yes.
      do cnt = 1 to 3:
        if v-exclude                   and
           oe-ordl.s-man[cnt] ge fsman and
           oe-ordl.s-man[cnt] le tsman then v-exclude = no.
      end. /* do cnt.. */
      if v-exclude then next.

      /* At this point we have either 1, 2 or 3 valid salesman, in any  */
      /* combination of the array. */

      v-misc = no.
      do cnt = 1 to 3:
        if v-misc then leave.

        if oe-ordl.s-man[cnt] lt fsman or
           oe-ordl.s-man[cnt] gt tsman then next.

        /* If no salesman number then assign to misc, ie, blank no */
        if cnt eq 1               and 
           oe-ordl.s-man[1] eq "" and
           oe-ordl.s-man[2] eq "" and
           oe-ordl.s-man[3] eq "" then
          assign
           v-sman-no = "MISC"
           v-amt     = oe-ordl.t-price
           v-pct     = 1
           v-misc    = yes.
           
        else   /* If blank salesman # then ignore */
        if oe-ordl.s-man[cnt] eq "" then next.
        
        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
        else do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-ordl.i-no
              no-lock no-error.
          if avail itemfg then
            assign
             v-sman-no = oe-ordl.s-man[cnt]
             v-pct     = oe-ordl.s-pct[cnt] / 100
             v-qty     = oe-ordl.qty / 1000
             v-sqft    = itemfg.t-sqft * (v-qty * v-pct)
             v-amt     = (oe-ordl.t-price * v-pct).
        end.

        find first w-data where w-sman-no eq v-sman-no no-error.
        if not avail w-data then do:
          find first sman
              where sman.company eq cocode
                and sman.sman    eq v-sman-no
              no-lock no-error.
          create w-data.
          assign
           w-sman-no = v-sman-no
           w-sname   = if avail sman then sman.sname else "NOT ON FILE".
        end.

        assign
         w-ytd-sqft = w-ytd-sqft + v-sqft
         w-ytd-amt  = w-ytd-amt  + v-amt.
         
        if oe-ord.ord-date ge v-ptd-first and
           oe-ord.ord-date le v-ptd-last  then
          assign
           w-ptd-sqft = w-ptd-sqft + v-sqft
           w-ptd-amt  = w-ptd-amt  + v-amt.

        if tdate eq oe-ord.ord-date then
          assign
           w-sqft = w-sqft + v-sqft
           w-amt  = w-amt  + v-amt.
      end. /* do cnt = 1 to 3... */
    end.  /* for each oe-ord */

    
    for each w-data where w-sman-no ne "MISC" break by w-sname
        with frame itemx:

      put skip(1).
      display w-sman-no
              w-sname
              w-sqft
              w-amt
              w-ptd-sqft
              w-ptd-amt
              w-ytd-sqft
              w-ytd-amt
              
         with frame itemx down.
      down with frame itemx.

      IF tb_excel THEN 
        EXPORT STREAM excel DELIMITER "," w-sman-no w-sname w-sqft 
                                          w-amt w-ptd-sqft w-ptd-amt 
                                          w-ytd-sqft w-ytd-amt.

      assign
       v-gtot-sqft     = v-gtot-sqft     + w-sqft
       v-gtot-amt      = v-gtot-amt      + w-amt
       v-gtot-ptd-sqft = v-gtot-ptd-sqft + w-ptd-sqft
       v-gtot-ptd-amt  = v-gtot-ptd-amt  + w-ptd-amt
       v-gtot-ytd-sqft = v-gtot-ytd-sqft + w-ytd-sqft
       v-gtot-ytd-amt  = v-gtot-ytd-amt  + w-ytd-amt.
    end.  /* for each ... */

    /* print totals the first time */
    underline w-sqft
              w-amt
              w-ptd-sqft
              w-ptd-amt
              w-ytd-sqft
              w-ytd-amt
        with frame itemx.
      
    display "   TOTAL SALES"    @ w-sname
            v-gtot-sqft         @ w-sqft
            v-gtot-amt          @ w-amt
            v-gtot-ptd-sqft     @ w-ptd-sqft
            v-gtot-ptd-amt      @ w-ptd-amt
            v-gtot-ytd-sqft     @ w-ytd-sqft
            v-gtot-ytd-amt      @ w-ytd-amt
            
        with frame itemx.
   ASSIGN v-gtot-sqft = 0
          v-gtot-amt = 0
          v-gtot-ptd-sqft = 0
          v-gtot-ptd-amt = 0
          v-gtot-ytd-sqft = 0
          v-gtot-ytd-amt = 0.

/*IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
*/
end procedure.


