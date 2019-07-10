 


/*------------------------------------------------------------------------
    File        : duereport.p
    Purpose     :  Due Date Report

    Syntax      :

    Description : Return a Dataset of Request For Due Date

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
  {custom/xprint.i}  
 {sys/inc/var.i new shared}
 
 DEFINE TEMP-TABLE ttDueDateRep NO-UNDO
    FIELD vDueFile AS CHAR
        FIELD vDueRepFile AS CHAR.

    DEFINE DATASET dsDueDateRep FOR ttDueDateRep .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmDueAct          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginDate       AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate         AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegsman         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndsman         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDueDateRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF  prmDueAct        = ?        THEN ASSIGN     prmDueAct         = "".
    IF  prmUser          = ?        THEN ASSIGN     prmUser           = "".
    IF  prmBegsman       = ?        THEN ASSIGN     prmBegsman        = "".
    IF  prmEndsman       = ?        THEN ASSIGN     prmEndsman        = "".
    IF  prmOut           = ?        THEN ASSIGN     prmOut            = "".


def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 48.
DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX"   NO-UNDO.
DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO. 
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes   NO-UNDO.
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  INITIAL "C:\Inetpub\wwwroot\pdfs\OrderDue.csv"  NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE vFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.


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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF STREAM excel.

assign
 v-today =  TODAY . 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

IF prmDueAct = "DueDateRep" THEN DO:
    ASSIGN 
        begin_ord-date = prmBeginDate
        end_ord-date   = prmEndDate 
        begin_slsmn    = prmBegsman 
        end_slsmn      = prmEndsman. 


    ASSIGN
        tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.

    ASSIGN          
        init-dir    = v-webrootpath
        fi_file = init-dir + "OrderDue" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vFile   = "OrderDue" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".

        DEFINE VAR vTextFile AS CHAR NO-UNDO .
        ASSIGN
            vTextFile = "OrderDue" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".


    run run-report.                           

    CREATE ttDueDateRep.
    IF tb_excel THEN
        ASSIGN ttDueDateRep.vDueFile = vFile.
    IF NOT  tb_excel THEN
        ASSIGN ttDueDateRep.vDueFile = vTextFile .
   
END.
    

/*********************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}   
def var fdate like oe-ord.ord-date format "99/99/9999" init 01/01/0001 no-undo.
def var tdate like fdate init 12/31/9999.
def var fsman as   char format "x(3)" no-undo.
def var tsman like fsman init "zzz" no-undo.

def var v-price     as   dec format ">>>>>9.99".
def var v-msf       as   dec format ">>>>9.999".

def var v-t-qty     as   dec.
def var v-t-price   as   dec.
def var v-t-msf     as   dec.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR i-name-no-var AS CHAR NO-UNDO.
DEF VAR style-var AS CHAR NO-UNDO.

form oe-ordl.req-date  column-label "Due Date"             format "99/99/99"
     oe-ord.cust-no    column-label "Customer"
     itemfg.i-name     column-label "Item Name"            format "x(24)"
     itemfg.style      column-label "Style"                format "x(4)"
     eb.test           column-label "Test"
     ef.adder[1 for 3] column-label "Adder"
     oe-ordl.qty       column-label "Ord Qty"              format ">>>>>>>9"
     v-price           column-label "Price/M"
     oe-ordl.t-price   column-label "Ext Price"
     v-msf             column-label "MSF"
    
    with frame det no-box no-attr-space down stream-io width 132.


assign
 str-tit2 = "Orders Booked By Due Date"
 {sys/inc/ctrtext.i str-tit2 112}

 fdate      = begin_ord-date
 tdate      = end_ord-date
 fsman      = begin_slsmn
 tsman      = end_slsmn.

/*{sys/inc/print1.i}*/

/*IF NOT tb_excel THEN DO:*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}
/*END.*/


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Due Date,Customer,Item Name,Style,Test,Adder,Adder,Adder,"
              + "Ord Qty,Price/M,Ext Price,MSF".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
display "" with frame r-top.
  
    for each oe-ord
         where oe-ord.company  eq cocode
           and oe-ord.ord-date ge fdate
           and oe-ord.ord-date le tdate
           and oe-ord.type     ne "T"
        no-lock,

         each oe-ordl
         where oe-ordl.company eq oe-ord.company
           and oe-ordl.ord-no  eq oe-ord.ord-no
           and ((oe-ordl.s-man[1] ge fsman and
                 oe-ordl.s-man[1] le tsman) or
                (oe-ordl.s-man[2] ge fsman and
                  oe-ordl.s-man[2] le tsman) or
                (oe-ordl.s-man[3] ge fsman and
                  oe-ordl.s-man[3] le tsman))
         no-lock,

         first itemfg
         where itemfg.company eq cocode
           and itemfg.i-no    eq oe-ordl.i-no
         no-lock

         break by oe-ordl.req-date
               by oe-ord.cust-no
               by oe-ordl.i-no
               by oe-ord.ord-no
               
         with frame det:

      release eb.
      release ef.
      for each eb
          where eb.company  eq oe-ordl.company
            and eb.est-no   eq oe-ordl.est-no
            and eb.stock-no eq oe-ordl.i-no
          no-lock:
        leave.
      end.
      if avail eb then
      find first ef
          where ef.company eq oe-ordl.company
            and ef.est-no  eq oe-ordl.est-no
            and ef.form-no eq eb.form-no
          no-lock no-error.

      assign
       v-price = oe-ordl.t-price / (oe-ordl.qty / 1000)
       v-msf   = itemfg.t-sqft * oe-ordl.qty / 1000.

      display oe-ordl.req-date
              oe-ord.cust-no
               itemfg.i-name     when avail itemfg
               oe-ordl.i-no      when itemfg.i-name eq ""
                            @ itemfg.i-name
               itemfg.style      when avail itemfg
               eb.style          when avail eb and itemfg.style eq ""
                                  @ itemfg.style
               eb.test           when avail eb
               ef.adder[1 for 3] when avail ef
               oe-ordl.qty
               v-price
               oe-ordl.t-price
               v-msf.
      down.

      IF tb_excel THEN DO:
      
        ASSIGN
         i-name-no-var = ""
         style-var = "".

        IF AVAIL itemfg THEN DO:
          IF itemfg.i-name NE "" THEN
            i-name-no-var = itemfg.i-name.
          ELSE
            i-name-no-var = oe-ordl.i-no.

          IF itemfg.style NE "" THEN
            style-var = itemfg.style.
          ELSE IF AVAIL eb THEN
            style-var = eb.style.
        END.

        PUT STREAM excel UNFORMATTED
            '"' (IF oe-ordl.req-date NE ? THEN STRING(oe-ordl.req-date)
                 ELSE "")                                              '",'
            '"' oe-ord.cust-no                                         '",'
            '"' i-name-no-var                                          '",'
            '"' style-var                                              '",'
            '"' (IF AVAIL eb THEN eb.test ELSE "")                     '",'
            '"' (IF avail ef THEN ef.adder[1] ELSE "")                 '",'
            '"' (IF avail ef THEN ef.adder[2] ELSE "")                 '",'
            '"' (IF avail ef THEN ef.adder[3] ELSE "")                 '",'
            '"' STRING(oe-ordl.qty,">>>>>>>9")                         '",'
            '"' STRING(v-price,">>>>>9.99")                            '",'
            '"' STRING(oe-ordl.t-price,">>>>>9.99")                    '",'
            '"' STRING(v-msf,">>>>9.999")                              '",'
            SKIP.
      END.

      assign
       v-t-qty   = v-t-qty   + oe-ordl.qty
       v-t-price = v-t-price + oe-ordl.t-price
       v-t-msf   = v-t-msf   + v-msf.
    end.

    v-price = v-t-price / (v-t-qty / 1000).

    put skip(2).

    display "Grand Totals"     @ itemfg.i-name
             v-t-qty            @ oe-ordl.qty
             v-price
             v-t-price          @ oe-ordl.t-price
             v-t-msf            @ v-msf

          with frame det.


end procedure.
