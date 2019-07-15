

/*------------------------------------------------------------------------
    File        : invsum.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttInvoiceSummary NO-UNDO
        FIELD invsum AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsInvoiceSummary FOR ttInvoiceSummary.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmchart         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPost          AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoiceSummary.

     IF prmUser       = ? THEN ASSIGN     prmUser     = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction   = "". 
     IF prmchart      = ? THEN ASSIGN     prmchart    = "". 
     IF prmOut        = ? THEN ASSIGN     prmOut      = "". 
     IF prmPost       = ? THEN ASSIGN     prmPost     = "".  
     IF cError        = ? THEN ASSIGN     cError      = "".  
     
    


DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR post-line-error AS LOGICAL NO-UNDO INITIAL NO.
DEF VAR v-jrnal-2 AS CHAR NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.


def var save_id as recid NO-UNDO.
def var tbal as dec format "->>,>>>,>>9.99" NO-UNDO.
def var deb  as dec label "DEBIT"  format "->>,>>>,>>9.99" NO-UNDO.
def var cred as dec label "CREDIT" format "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lv-rev AS CHAR LABEL " " NO-UNDO.
def var tot-deb  as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot-cred like tot-deb NO-UNDO.
def var ad like tot-deb NO-UNDO.
def var ac like tot-deb NO-UNDO.
def var gc like tot-deb NO-UNDO.
def var gd like tot-deb NO-UNDO.
def var curjnl as char format "x(8)" NO-UNDO.
def var fjrnl as int label "  From Journal Number" NO-UNDO.
def var tjrnl as int label "  Thru Journal Number" NO-UNDO.
def var xtrnum as int NO-UNDO.
def var sort-by-acct as log NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF BUFFER bgl-jrn  FOR gl-jrn.
DEF BUFFER bgl-jrnl FOR gl-jrnl.

DEF TEMP-TABLE tt-gl-jrn  LIKE gl-jrn.
DEF TEMP-TABLE tt-gl-jrnl LIKE gl-jrnl.

DEF STREAM s-temp.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF VAR ip-post AS LOG NO-UNDO.
ASSIGN ip-post = YES.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser .

ASSIGN tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

def TEMP-TABLE w-f
  field actnum   like ar-invl.actnum
  field amt      as   dec format "->>>,>>>,>>>,>>>,>>9.99"
  INDEX actnum actnum.

def stream s-temp.


  IF prmAction = "invsum" THEN DO:
    
        

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "InvSum" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "InvSum" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .


        run run-report.


   
  CREATE ttInvoiceSummary.
  IF prmOut = "Yes" THEN
    ASSIGN ttInvoiceSummary.invsum = vExcalFile .
  ELSE
    ASSIGN ttInvoiceSummary.invsum = vTextFile .

  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* ----------------------------------------------- gl/rep/gl-invs.p 11/96 JLF */
/* GL Invoice Summary Report                                                  */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

DEF VAR v-export     AS LOGICAL.
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-postrg.csv".
DEF VAR v-acct-descr AS CHAR. 

def var v-ar-acct     like ar-ctrl.receivables.
def var v-ar-freight  like ar-ctrl.freight.
def var v-ar-stax     like ar-ctrl.stax.
def var v-ar-sales    like ar-ctrl.sales.
def var v-ar-disc     like ar-ctrl.discount.

def var v-actnum like ar-invl.actnum.
def var v-disc   like w-f.amt.
def var v-frgt   like v-disc.
def var v-tax    like v-disc.
def var v-total  like v-disc.

find first ar-ctrl where ar-ctrl.company eq cocode no-lock.

assign
   str-tit2 = "GL Invoice Summary" 
   {sys/inc/ctrtext.i str-tit2 56} 
   v-ar-acct    = ar-ctrl.receivables
   v-ar-freight = ar-ctrl.freight
   v-ar-stax    = ar-ctrl.stax
   v-ar-sales   = ar-ctrl.sales
   v-ar-disc    = ar-ctrl.discount
   v-export = tb_excel
   v-exp-name = fi_file. 

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i value(lines-per-page)}

   
display "" with frame r-top.
  
IF v-export THEN DO:
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).
END.  

for each ar-inv
   where ar-inv.company eq cocode
     and ar-inv.posted  eq yes
     no-lock:
      
   assign
      v-frgt = v-frgt + if ar-inv.f-bill then ar-inv.freight else 0
      v-tax  = v-tax  + tax-amt.

   for each ar-invl
      where ar-invl.company eq cocode
        and ar-invl.x-no    eq ar-inv.x-no
        no-lock:
         
      if actnum eq "" then do:
         find first itemfg of ar-invl no-lock no-error.
         if not avail itemfg then next.
         find first fgcat of itemfg no-lock no-error.
         if not avail fgcat then next.
         v-actnum = fgcat.glacc.
      END.
      ELSE 
         v-actnum = ar-invl.actnum.

      find first w-f where w-f.actnum eq v-actnum no-error.
      if not avail w-f then 
         create w-f.

      assign
         w-f.actnum = v-actnum
         w-f.amt    = w-f.amt + ar-invl.amt
         v-disc     = v-disc +
                      (ar-invl.amt / ((100 - ar-invl.disc) / 100) - ar-invl.amt).
   END.
END.

display 
   v-ar-freight "Freight"   format "x(30)" v-frgt   skip
   v-ar-disc    "Discounts" format "x(30)" v-disc   skip
   v-ar-stax    "Sales Tax" format "x(30)" v-tax    skip
   skip(2)
   with no-box no-labels stream-io.

v-excel-hdr = "Freight,Discounts,Sales Tax".  

IF v-export THEN
   PUT STREAM s-temp UNFORMATTED
    v-excel-hdr 
       SKIP
    '"' v-ar-freight '",Freight,"'   v-frgt '",'   skip
    '"'v-ar-disc     '",Discounts,"' v-disc '",'   skip
    '"'v-ar-stax     '",Sales Tax,"' v-tax  '",'   skip
    skip(2).

v-total = v-frgt + v-disc + v-tax.

for each w-f by w-f.actnum with frame f2 no-labels no-box stream-io:
   find first account where account.actnum eq w-f.actnum no-lock no-error.
   display 
      w-f.actnum 
      account.dscr when avail account format "x(30)" 
      w-f.amt.
     
   if w-f.actnum eq v-ar-acct then display "Beginning Balance" @ account.dscr.
   v-total = v-total + w-f.amt.
     
   IF AVAIL account THEN 
      v-acct-descr = account.dscr.
   ELSE
      v-acct-descr = "".

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
      '"' w-f.actnum    '",'
      '"' v-acct-descr  '",'
      '"' w-f.amt       '",'
      SKIP .
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
 FOR EACH w-f:
     DELETE w-f.
 END.

IF v-export THEN DO:
   OUTPUT STREAM s-temp close.
   
END.



END PROCEDURE.
