

/*------------------------------------------------------------------------
    File        : gltransrpt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttTransactionReport NO-UNDO
        FIELD trnas AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsTransactionReport FOR ttTransactionReport.
    DEFINE INPUT PARAMETER  prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdate           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddate           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegact            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtb_acpay          AS CHAR NO-UNDO.  
    DEFINE INPUT PARAMETER  prmtb_adjust         AS CHAR NO-UNDO.  
    DEFINE INPUT PARAMETER  prmtb_appurch       AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER  prmtb_apckr          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_apmem          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_apvoidck       AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_arinv          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_autodist       AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_cashr          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_cashrvd        AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER  prmtb_cdisb          AS CHAR NO-UNDO.  
    DEFINE INPUT PARAMETER  prmtb_crmem          AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER  prmtb_fgpost         AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_general        AS CHAR NO-UNDO.     
    DEFINE INPUT PARAMETER  prmtb_jcost          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_mcshrec        AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER  prmtb_oeinv          AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmtb_rmpost         AS CHAR NO-UNDO.     
    DEFINE INPUT PARAMETER  prmtb_void_checks    AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER  prmOut               AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError               AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTransactionReport.

     IF prmUser             = ? THEN ASSIGN     prmUser             = "".   
     IF prmAction           = ? THEN ASSIGN     prmAction           = "". 
     IF prmbegdate          = ? THEN ASSIGN     prmbegdate          = "". 
     IF prmenddate          = ? THEN ASSIGN     prmenddate          = "". 
     IF prmbegact           = ? THEN ASSIGN     prmbegact           = "".  
     IF prmendact           = ? THEN ASSIGN     prmendact           = "".  
     IF prmtb_acpay         = ? THEN ASSIGN     prmtb_acpay         = "". 
     IF prmtb_adjust        = ? THEN ASSIGN     prmtb_adjust        = "". 
     IF prmtb_appurch       = ? THEN ASSIGN     prmtb_appurch      = "". 
     IF prmtb_apckr         = ? THEN ASSIGN     prmtb_apckr         = "".   
     IF prmtb_apmem         = ? THEN ASSIGN     prmtb_apmem         = "". 
     IF prmtb_apvoidck      = ? THEN ASSIGN     prmtb_apvoidck      = "". 
     IF prmtb_arinv         = ? THEN ASSIGN     prmtb_arinv         = "". 
     IF prmtb_autodist      = ? THEN ASSIGN     prmtb_autodist      = "".  
     IF prmtb_cashr         = ? THEN ASSIGN     prmtb_cashr         = "".  
     IF prmtb_cashrvd       = ? THEN ASSIGN     prmtb_cashrvd       = "". 
     IF prmtb_cdisb         = ? THEN ASSIGN     prmtb_cdisb         = "". 
     IF prmtb_crmem         = ? THEN ASSIGN     prmtb_crmem         = "". 
     IF prmtb_fgpost        = ? THEN ASSIGN     prmtb_fgpost        = "".   
     IF prmtb_general       = ? THEN ASSIGN     prmtb_general       = "". 
     IF prmtb_jcost         = ? THEN ASSIGN     prmtb_jcost         = "". 
     IF prmtb_mcshrec       = ? THEN ASSIGN     prmtb_mcshrec       = "". 
     IF prmtb_oeinv         = ? THEN ASSIGN     prmtb_oeinv         = "".  
     IF prmtb_rmpost        = ? THEN ASSIGN     prmtb_rmpost        = "".  
     IF prmtb_void_checks   = ? THEN ASSIGN     prmtb_void_checks   = "". 
     IF prmOut              = ? THEN ASSIGN     prmOut              = "". 
     



DEFINE VARIABLE begin_accnt     AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE begin_date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_accnt       AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date        AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999   NO-UNDO.
DEFINE VARIABLE tb_acpay        AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_adjust       AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_ap-purch     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_apckr        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_apmem        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_apvoidck     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_arinv        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_autodist     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_cashr        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_cashrvd      AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_cdisb        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_crmem        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_fgpost       AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_general      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_jcost        AS LOGICAL INITIAL NO NO-UNDO. 
DEFINE VARIABLE tb_mcshrec      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_oeinv        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_rmpost       AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_void_checks  AS LOGICAL INITIAL NO NO-UNDO.



DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.
DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Account#" NO-UNDO.
DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_runExcel    AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

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


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .




/* gdm - 10010905 */
DEF  STREAM excel.

assign
   begin_date = date(month(today),1,year(today))
   end_date   = today.



  IF prmAction = "trnas" THEN DO:
    
        ASSIGN
       begin_accnt        = prmbegact                                                  
       begin_date         = date(prmbegdate)
       end_accnt          = prmendact                                            
       end_date           = date(prmenddate)                                                   
       tb_acpay           = IF prmtb_acpay       = "Yes" THEN TRUE ELSE FALSE                 
       tb_adjust          = IF prmtb_adjust      = "Yes" THEN TRUE ELSE FALSE                   
       tb_ap-purch        = IF prmtb_appurch    = "Yes" THEN TRUE ELSE FALSE                  
       tb_apckr           = IF prmtb_apckr       = "Yes" THEN TRUE ELSE FALSE               
       tb_apmem           = IF prmtb_apmem       = "Yes" THEN TRUE ELSE FALSE                
       tb_apvoidck        = IF prmtb_apvoidck    = "Yes" THEN TRUE ELSE FALSE               
       tb_arinv           = IF prmtb_arinv       = "Yes" THEN TRUE ELSE FALSE          
       tb_autodist        = IF prmtb_autodist    = "Yes" THEN TRUE ELSE FALSE            
       tb_cashr           = IF prmtb_cashr       = "Yes" THEN TRUE ELSE FALSE             
       tb_cashrvd         = IF prmtb_cashrvd     = "Yes" THEN TRUE ELSE FALSE            
       tb_cdisb           = IF prmtb_cdisb       = "Yes" THEN TRUE ELSE FALSE            
       tb_crmem           = IF prmtb_crmem       = "Yes" THEN TRUE ELSE FALSE          
       tb_fgpost          = IF prmtb_fgpost      = "Yes" THEN TRUE ELSE FALSE            
       tb_general         = IF prmtb_general     = "Yes" THEN TRUE ELSE FALSE              
       tb_jcost           = IF prmtb_jcost       = "Yes" THEN TRUE ELSE FALSE             
       tb_mcshrec         = IF prmtb_mcshrec     = "Yes" THEN TRUE ELSE FALSE              
       tb_oeinv           = IF prmtb_oeinv       = "Yes" THEN TRUE ELSE FALSE            
       tb_rmpost          = IF prmtb_rmpost      = "Yes" THEN TRUE ELSE FALSE              
       tb_void_checks     = IF prmtb_void_checks = "Yes" THEN TRUE ELSE FALSE .              
                                                                                   





        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "TrnasReport" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "TrnasReport" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .

        
    
        RUN run-report.
   
   
  CREATE ttTransactionReport.
  IF prmOut = "Yes" THEN
    ASSIGN ttTransactionReport.trnas = vExcalFile .
  ELSE
    ASSIGN ttTransactionReport.trnas = vTextFile .

  END.
/*****************************************************************************************/
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: ap/rep/pjgl.p
**       By: Chris Heins
** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

DEFINE VARIABLE lo_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
DEFINE VARIABLE hi_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE ws_disc LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
DEFINE VARIABLE ws_check-no LIKE ap-chk.check-no NO-UNDO format ">>>>>>>"
    column-label "Check#".
DEFINE VARIABLE ws_order-no LIKE oe-ord.ord-no NO-UNDO
    format ">>>>>>".
def var ws_jrnl like gltrans.jrnl column-label "Journal" no-undo.
DEF VAR GL_JRNL_LIST AS CHAR NO-UNDO.

def var lo_actnum like account.actnum label "From GL Acct#" no-undo.
def var hi_actnum like account.actnum label "Thru GL Acct#" no-undo.

  DEFINE VARIABLE t-amt AS DECIMAL NO-UNDO.
  DEFINE VARIABLE t-disc AS DECIMAL NO-UNDO.
  DEF VAR t-qty as decimal no-undo.
  def var t-msf as decimal no-undo.
  DEFINE VARIABLE hdg_printed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE g-amt AS DECIMAL NO-UNDO.
  DEF VAR excelheader AS CHAR NO-UNDO.
  DEF VAR viCount AS INT NO-UNDO.
  DEF VAR v-line AS CHAR FORMAT "X(60)" NO-UNDO.

form
        ws_jrnl
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.NAME FORMAT "X(35)" /* gdm - 01210902 adjstd frm 54*/
        ap-inv.inv-date FORMAT "99/99/99" COLUMN-LABEL "Date" 
        ap-inv.inv-no FORMAT "X(6)" COLUMN-LABEL "Inv#"
        ws_check-no
        ws_order-no
        ap-invl.qty FORMAT "->>,>>>,>>9.9<"
        ap-invl.amt-msf
        ws_disc
        ap-invl.amt
        WITH FRAME f-det width 158 DOWN STREAM-IO.
        

assign
 str-tit2 = "GL Transaction Report"
 {sys/inc/ctrtext.i str-tit2 112}
 
 lo_actnum    = begin_accnt
 hi_actnum    = end_accnt
 lo_trandate  = begin_date
 hi_trandate  = end_date
 gl_jrnl_list = (if tb_cashr    then "CASHR,CRDIS," else "") +
                (if tb_general  then "GENERAL,"     else "") +
                (if tb_mcshrec  then "MCSHREC,"     else "") +
                (if tb_apmem    then "APMEM,"       else "") +
                (if tb_acpay    then "ACPAY,"       else "") +
                (if tb_ap-purch then "AP-PURCH,"    else "") +
                (if tb_apckr    then "APCKR,"       else "") +
                (if tb_arinv    then "ARINV,"       else "") +
                (if tb_cdisb    then "CDISB,"       else "") +
                (if tb_crmem    then "CRMEM,DBMEM," else "") +
                (if tb_apvoidck then "APVOIDCK,"    else "") +
                (if tb_oeinv    then "OEINV,"       else "") +
                (if tb_adjust   then "ADJUST,"      else "") +
                (if tb_jcost    then "JCOST,"       else "") +
                (if tb_fgpost   then "FGPOST,"      else "") +
                (if tb_rmpost   then "RMPOST,"      else "") +
                (if tb_autodist then "AUTODIST,"    else "") +
                (if tb_void_checks then "APVOIDCK,"    else "") +
                (if tb_cashrvd  then "CASHRVD,"     else "").


/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Journal,Vendor,Name,Date,Invoice#,Check#,Order#,"
              + "Quantity,Amt MSF,Discount,Amount".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


DISPLAY "" WITH FRAME r-top.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH account NO-LOCK
      WHERE account.company = cocode
      and account.actnum >= lo_actnum
      and account.actnum <= hi_actnum:

    if line-counter >= (page-size - 2) then do:
        page.
        view frame f-det.
        down 0 with frame f-det.
    end.

    ASSIGN
      hdg_printed = FALSE
      t-amt = 0
      t-disc = 0
      t-msf = 0
      t-qty = 0
      ws_disc = 0
      ws_jrnl = ''
      ws_check-no = 0
      ws_order-no = 0.

    VIEW FRAME F-DET.
    DOWN 0 WITH FRAME F-DET.

    FOR EACH gltrans NO-LOCK
        WHERE gltrans.company = cocode
        AND gltrans.actnum = account.actnum
        AND gltrans.tr-date >= lo_trandate
        AND gltrans.tr-date <= hi_trandate
        AND CAN-DO(GL_JRNL_LIST, gltrans.jrnl)
        BY gltrans.tr-date:
      IF NOT hdg_printed THEN
      DO:

        v-line = account.actnum + ' - ' + account.dscr.
        PUT SKIP v-line SKIP.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' account.actnum          '",'
                '"' ""                      '",'
                '"' account.dscr            '",'
                SKIP.

        hdg_printed = TRUE.
      END.

      DISPLAY
        gltrans.jrnl @ ws_jrnl
        gltrans.tr-dscr @ vend.name
        gltrans.tr-date @ ap-inv.inv-date
        gltrans.tr-amt  @ ap-invl.amt
      with frame f-det.
      down 1 with frame f-det.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             '"' gltrans.jrnl            '",'
             '"' ""                      '",'
             '"' gltrans.tr-dscr         '",'
             '"' STRING(gltrans.tr-date) '",'.
             
         DO viCount = 1 TO 6:
            PUT STREAM excel UNFORMATTED '"' ""  '",'.
         END.

         PUT STREAM excel UNFORMATTED
             '"' STRING(gltrans.tr-amt,"->>>>,>>9.99") '",' SKIP.
      END.

      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + gltrans.tr-amt.
    END.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company = cocode
        AND glhist.actnum = account.actnum
        AND glhist.tr-date >= lo_trandate
        AND glhist.tr-date <= hi_trandate
        AND CAN-DO(GL_JRNL_LIST, glhist.jrnl)
        BY glhist.tr-date:
      IF NOT hdg_printed THEN
      DO:
        v-line = account.actnum + ' - ' + account.dscr.
        PUT SKIP v-line SKIP.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' account.actnum          '",'
               '"' ""                      '",'
               '"' account.dscr            '",'
               SKIP.

        hdg_printed = TRUE.
      END.
      DISPLAY
        glhist.jrnl @ ws_jrnl
        glhist.tr-dscr @ vend.name
        glhist.tr-date @ ap-inv.inv-date
        glhist.tr-amt  @ ap-invl.amt
      with frame f-det.
      down 1 with frame f-det.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             '"' glhist.jrnl            '",'
             '"' ""                      '",'
             '"' glhist.tr-dscr         '",'
             '"' STRING(glhist.tr-date) '",'.
             
         DO viCount = 1 TO 6:
            PUT STREAM excel UNFORMATTED '"' ""  '",'.
         END.

         PUT STREAM excel UNFORMATTED
             '"' STRING(glhist.tr-amt,"->>>>,>>9.99") '",' SKIP.
      END.

      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + glhist.tr-amt.
    END.

/*
      ws_jrnl = "AP-DIS".
*/
/* Commented out for duplicatation of AP-PURCH and ACPAY and non posted CDISB

      ws_jrnl = "CDISB".
    FOR EACH ap-disl NO-LOCK
        WHERE ap-disl.company = cocode
        AND ap-disl.actnum = account.actnum,
        EACH ap-dis NO-LOCK
        WHERE ap-dis.d-no = ap-disl.d-no
        AND ap-dis.check-date >= lo_trandate
        AND ap-dis.check-date <= hi_trandate
        BY ap-dis.check-date BY ap-dis.check-no:
      IF NOT hdg_printed THEN
      DO:
        PUT SKIP account.actnum ' - '
          account.dscr
          SKIP.
        hdg_printed = TRUE.
      END.

      FIND vend OF ap-dis NO-LOCK NO-ERROR.
      ws_disc = 0.
      ws_check-no = ap-dis.check-no.
      DISPLAY
        ws_jrnl
        ap-dis.vend-no    @ ap-inv.vend-no
        vend.name WHEN AVAILABLE vend
        ap-dis.check-date @ ap-inv.inv-date
        ws_check-no
        ws_order-no
        ap-disl.qty WHEN ap-disl.qty <> 0 @ ap-invl.qty
        ap-disl.amt @ ap-invl.amt
        WITH FRAME f-det.
      down 1 with frame f-det.
      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + ap-disl.amt
        t-qty = t-qty + ap-disl.qty.
    END.    /* ap-disl */

*/


if can-do(GL_JRNL_LIST, "AP-PURCH") then
do:
    ws_jrnl = "AP-PURCH".
    ws_check-no = 0.
    FOR EACH ap-invl NO-LOCK
        WHERE ap-invl.company = cocode
        AND ap-invl.actnum = account.actnum,
        EACH ap-inv NO-LOCK
        WHERE ap-inv.i-no = ap-invl.i-no
        AND ap-inv.inv-date >= lo_trandate
        AND ap-inv.inv-date <= hi_trandate
        BY ap-inv.inv-date BY ap-inv.inv-no:
      IF NOT hdg_printed THEN
      DO:
        v-line = account.actnum + ' - ' + account.dscr.
        PUT SKIP v-line
          SKIP.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' account.actnum          '",'
               '"' ""                      '",'
               '"' account.dscr            '",'
               SKIP.

        hdg_printed = TRUE.
      END.

      FIND vend OF ap-inv NO-LOCK NO-ERROR.
      ws_disc = ap-invl.amt * (ap-inv.disc-% / 100).
      ws_order-no = ap-inv.po-no.
      DISPLAY
        ws_jrnl
        ap-inv.vend-no
        vend.name WHEN AVAILABLE vend
        ap-inv.inv-date
        ap-inv.inv-no
        ws_check-no
        ws_order-no
        ap-invl.qty WHEN ap-invl.qty <> 0
        ap-invl.amt-msf WHEN ap-invl.amt-msf <> 0
        ws_disc WHEN ws_disc <> 0
        ap-invl.amt
        WITH FRAME f-det.
      down 1 with frame f-det.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             '"' ws_jrnl                              '",'
             '"' ap-inv.vend-no                       '",'
             '"' IF AVAIL vend THEN vend.name ELSE "" '",'
             '"' STRING(ap-inv.inv-date)              '",'
             '"' ap-inv.inv-no                        '",'
             '"' ws_check-no                          '",'
             '"' ws_order-no                          '",'
             '"' IF ap-invl.qty <> 0 THEN STRING(ap-invl.qty,"->>>,>>>,>>9.9<<<<<") ELSE "" '",'
             '"' IF ap-invl.amt-msf <> 0 THEN STRING(ap-invl.amt-msf,"->>,>>9.99") ELSE "" '",'
             '"' IF ws_disc <> 0 THEN STRING(ws_disc,"->>,>>9.99") ELSE "" '",'
             '"' STRING(ap-invl.amt,"->>>>,>>9.99") '",' SKIP.
      END.

      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + ap-invl.amt
        t-qty = t-qty + ap-invl.qty
        t-msf = t-msf + ap-invl.amt-msf.
    END.    /* ap-invl */
end.

    IF NOT hdg_printed THEN
    NEXT.   /* inactive account */

    do with frame f-det:
    UNDERLINE
      ap-invl.qty
      ap-invl.amt-msf
      ap-invl.amt
      ws_disc.
    DOWN 1.
    DISPLAY
      /* "* ACCOUNT TOTAL *" @ vend.name */
      t-disc @ ws_disc
      t-amt  @ ap-invl.amt
      t-qty  @ ap-invl.qty
      t-msf  @ ap-invl.amt-msf.

    IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED SKIP(1).

         DO viCount = 1 TO 7:
            PUT STREAM excel UNFORMATTED
                '"' "" '",'.
         END.

         PUT STREAM excel UNFORMATTED
             '"' STRING(t-qty,"->>>,>>>,>>9.9<<<<<") '",'
             '"' STRING(t-msf,"->>,>>9.99") '",'
             '"' STRING(t-disc,"->>,>>9.99") '",'
             '"' STRING(t-amt,"->>>>,>>9.99") '",' SKIP.
      END.
     /* down 1. */
    end.
    g-amt = g-amt + t-amt.
  END.  /* for each account */

  /*IF g-amt NE 0 THEN*/
  do with frame f-det:
    UNDERLINE ap-invl.amt.
    DOWN 3.
    DISPLAY g-amt @ ap-invl.amt.

    IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED SKIP(1).
         DO viCount = 1 TO 10:
            PUT STREAM excel UNFORMATTED
                '"' "" '",'.
         END.

         PUT STREAM excel UNFORMATTED
             '"' STRING(g-amt,"->>>>,>>9.99") '",' SKIP.
      END.
  end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
end procedure.
