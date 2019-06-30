

/*------------------------------------------------------------------------
    File        : fgpost.p
    Purpose     :  Finished Goods Post
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttCheckRegister NO-UNDO
        FIELD vchkreg AS CHAR
        FIELD extra AS CHAR.
DEFINE DATASET dsCheckRegister FOR ttCheckRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmchkreg        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvoidskip      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtacc        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmchkfile       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmapchkfile     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpstmnl        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpost          AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCheckRegister.

     IF prmUser      = ? THEN ASSIGN     prmUser       = "".   
     IF prmchkreg    = ? THEN ASSIGN     prmchkreg     = "". 
     IF prmPstDate   = ? THEN ASSIGN     prmPstDate    = "". 
     IF prmvoidskip  = ? THEN ASSIGN     prmvoidskip   = "". 
     IF prmperiod    = ? THEN ASSIGN     prmperiod     = 0.  
     IF prmprtacc    = ? THEN ASSIGN     prmprtacc     = "".  
     IF prmchkfile   = ? THEN ASSIGN     prmchkfile    = "". 
     IF prmapchkfile = ? THEN ASSIGN     prmapchkfile  = "". 
     IF prmpstmnl    = ? THEN ASSIGN     prmpstmnl     = "". 
     IF prmpost    = ? THEN ASSIGN     prmpost     = "".
     

DEFINE VARIABLE fi_CheckFile    AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-chkreg.csv" NO-UNDO.
DEFINE VARIABLE rd_sort         AS CHARACTER NO-UNDO.
DEFINE VARIABLE tran-date       AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE tran-period     AS INTEGER FORMAT ">>" NO-UNDO.
DEFINE VARIABLE tb_APcheckFile  AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_excel        AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_prt-acc      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_void         AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" NO-UNDO.

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
def var list-name as cha no-undo.
def var list-name2 as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
DEF VAR t-setup AS LOG INITIAL NO NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
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
 g_company = prmComp
 vuser     = prmUser .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
     NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    NO-ERROR.
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

def new shared var v-trnum as INT NO-UNDO.
def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---"  NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var v-postable as log init NO NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.
def var save_id as recid NO-UNDO.
def var pct-paid as dec NO-UNDO.
def var gtot0 like ap-sel.inv-bal NO-UNDO.
def var gtot1 like ap-sel.disc-amt NO-UNDO.
def var gtot2 like ap-sel.amt-paid NO-UNDO.
def var ndisc as dec NO-UNDO.
def var wckdate as date NO-UNDO.
def var bal as dec format "->>,>>>,>>9.99" NO-UNDO.
def var bal1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot-of-inv as de format "->>,>>>,>>9.99" NO-UNDO.
def var v-fst-chk as log NO-UNDO.
def var v-lst-chk as log NO-UNDO.
def var tot0 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot2 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var c1  as dec NO-UNDO.
def var op as log format "Yes/No" NO-UNDO.
def var ctr as int label "NUMBER OF CHECKS WRITTEN " NO-UNDO.
def var credit as dec NO-UNDO.
def var tdisc as dec NO-UNDO.
def var wcash as char format "x(25)" label "CASH ACCOUNT" NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
def var ap-acct like ap-ctrl.payables NO-UNDO.
def var v-loop-count as int no-undo INITIAL 10 .
def var post-manual as log format "Manual/Automatic"
  label "Post Manual or Automatic (M/A)?" INITIAL no NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v-fileFormat AS CHAR NO-UNDO.
DEF STREAM checkFile.
DEF VAR v-amt-paid LIKE ap-sel.amt-paid NO-UNDO.

DEF STREAM excel.

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id    AS ROWID
                               FIELD ex-rate   LIKE currency.ex-rate INIT 1
                               FIELD curr-bal  LIKE ap-sel.inv-bal
                               FIELD curr-disc LIKE ap-sel.disc-amt
                               FIELD curr-paid LIKE ap-sel.amt-paid
                               FIELD actnum    LIKE account.actnum.

find first ap-ctrl where ap-ctrl.company = cocode
      no-lock no-wait no-error.
IF AVAIL ap-ctrl THEN
    assign wcash   = ap-ctrl.cash-act
           ap-acct = ap-ctrl.payables
           v-frt-acct = ap-ctrl.freight.

release ap-ctrl.

{sys/inc/postdate.i}
   {sys/inc/aplockbx.i}   
   find first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "AUDITDIR"
        no-lock no-error.
   
   if not avail sys-ctrl THEN DO:
      create sys-ctrl.
      assign
         sys-ctrl.company = cocode
         sys-ctrl.name    = "AUDITDIR"
         sys-ctrl.descrip = "Audit Trails directory"
         sys-ctrl.char-fld = ".\AUDIT TRAILS".
   end.
  
   lv-audit-dir = sys-ctrl.char-fld.
  
   IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
      lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).
  
   RELEASE sys-ctrl.



   tran-date = TODAY.

   ASSIGN 
          tb_APcheckFile = IF prmapchkfile EQ "YES" 
                             THEN YES ELSE NO. 
  
  FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "APCheckFile" NO-ERROR.
 /* IF NOT AVAIL sys-ctrl THEN 
      CREATE sys-ctrl.
      ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "APCheckFile"
        sys-ctrl.descrip = "Download Text format when printing AP Checks?"
        sys-ctrl.log-fld = YES. */
  

  FIND FIRST sys-ctrl-shipto NO-LOCK
    WHERE sys-ctrl-shipto.company      EQ cocode
      AND sys-ctrl-shipto.NAME         EQ "APCheckFile"
      AND sys-ctrl-shipto.cust-vend    EQ YES 
      AND sys-ctrl-shipto.cust-vend-no EQ "" NO-ERROR.
  IF NOT AVAIL sys-ctrl-shipto THEN 
    DO TRANSACTION:
      CREATE sys-ctrl-shipto.
      ASSIGN
        sys-ctrl-shipto.company      = cocode       
        sys-ctrl-shipto.NAME         = "APCheckFile"
        sys-ctrl-shipto.cust-vend    = YES          
        sys-ctrl-shipto.cust-vend-no = "". 

      cError = "System control record not found. Update APCheckFile file path." .
     /* UPDATE sys-ctrl-shipto.char-fld FORMAT "x(50)".*/

  END.    

  ASSIGN v-fileFormat = sys-ctrl.char-fld.

  IF AVAIL sys-ctrl AND
     sys-ctrl.log-fld  AND
     TRIM(v-fileFormat) NE ""
    THEN DO:
      
     IF sys-ctrl-shipto.char-fld NE "" THEN DO:

       IF SUBSTR(TRIM(sys-ctrl-shipto.char-fld),
                 LENGTH(sys-ctrl-shipto.char-fld) - 3 ,4) NE ".txt" 
         THEN DO:

          ASSIGN fi_CheckFile = STRING(TODAY,"99999999") + 
                                STRING(TIME) + ".txt".

          IF SUBSTR(sys-ctrl-shipto.char-fld,
                    LENGTH(sys-ctrl-shipto.char-fld),1) NE "/" AND
             SUBSTR(sys-ctrl-shipto.char-fld,
                    LENGTH(sys-ctrl-shipto.char-fld),1) NE "\"
            THEN 
             ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + "\" + 
                                   fi_CheckFile.
            ELSE
             ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + fi_CheckFile.
       END.
       ELSE
        ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld).

     END.
     ELSE
      ASSIGN fi_CheckFile = "C:\tmp\" +
                            STRING(TODAY,"99999999") + 
                            STRING(TIME) + ".txt".       

     ASSIGN prmchkfile = fi_CheckFile.

     IF prmchkfile EQ "" 
       THEN 
        ASSIGN fi_CheckFile = "C:\tmp\" +
                               STRING(TODAY,"99999999") + 
                               STRING(TIME) + ".txt"
               prmchkfile = fi_CheckFile.
  END. 
  

  IF prmchkreg = "view" THEN do:
      CREATE ttCheckRegister.
      ASSIGN
          ttCheckRegister.extra = prmchkfile.
  END.



  IF prmchkreg = "validatechkreg" THEN DO:
  
    FIND FIRST period NO-LOCK                   
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(prmPstDate)
          AND period.pend    GE DATE(prmPstDate)
        NO-ERROR.
    IF AVAIL period THEN DO:
      IF NOT period.pstat THEN DO:
        cError = "Period Already Closed...".
        RETURN.
      END.
      prmperiod = (period.pnum).
    END.

    ELSE DO:
      cError = "No Defined Period Exists".
      RETURN.
    END.
  END.

  


  IF prmchkreg = "chkreg" THEN DO:
     
        ASSIGN
        v-today         = TODAY
        tran-date       = date(prmPstDate)                                         
        tran-period     = prmperiod                                          
        tb_APcheckFile  = IF prmapchkfile = "Yes" THEN TRUE ELSE FALSE
        tb_prt-acc      = IF prmprtacc = "Yes" THEN TRUE ELSE FALSE                                         
        tb_void         = IF prmvoidskip = "Yes" THEN TRUE ELSE FALSE
        rd_sort         = prmpstmnl     .
    
        ASSIGN 
            tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.

    assign
      init-dir    =  v-webrootpath
        fi_file = init-dir + "chkreg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile =  "chkreg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "chkreg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
        vTextFile2 =  "afterchkreg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        DEF VAR lv-post AS LOG NO-UNDO.
        DEF VAR lv-bank-file AS cha NO-UNDO.

        ASSIGN  prmchkfile = fi_CheckFile.

        post-manual = rd_sort EQ "Manual".
        

        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAIL gl-ctrl THEN DO:
                ASSIGN v-trnum       = gl-ctrl.trnum + 1 
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
         END. /* REPEAT */

         run run-report.
        
         
         IF v-postable THEN DO:    
             lv-post =  IF prmpost = "Yes" THEN TRUE ELSE FALSE .
             
             IF lv-post THEN do: 
                 
                 IF aplockbx-log THEN DO:
                     RUN create-bank-file (OUTPUT lv-bank-file).
                     cError = "Check Register/Lock Box file is created into " +
                         aplockbx-path + lv-bank-file .
                 END.
                 RUN post-gl.
                 RUN copy-report-to-audit-dir.
                 cError = "Posting Complete".
             END.
             ELSE RUN undo-trnum.
         END.
         ELSE do:
             cError = "No A/P Checks available for posting..." .
             RUN undo-trnum.
         END.

         CREATE ttCheckRegister.
         IF tb_excel THEN
             ASSIGN ttCheckRegister.vchkreg = vPdfFile .
         IF NOT tb_excel THEN
             ASSIGN ttCheckRegister.vchkreg = vTextFile .

         
         
  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

ASSIGN
time_stamp = STRING(TIME, "hh:mmam")
tmpstore   = FILL("_",125).

{sys/form/r-top3w.f}

DEF VAR v-line-amt LIKE ap-invl.amt NO-UNDO.
DEF VAR v-frgt-amt LIKE ap-inv.freight NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.


FORMAT HEADER
       "Check    Vendor"
       "Invoice                                 Discount" AT 55
       "   Check   Pre-Issued" AT 110 SKIP
       "Number   Number   Name       Check Date"
       "Date      Number                  Due      Taken" AT 55
       "Amt Paid   Date"       AT 110 SKIP
       FILL("_",132) FORMAT "x(130)"      SKIP
    WITH FRAME f-top WIDTH 132 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO.



ASSIGN
 str-tit  = coname + " - " + loname
 str-tit2 = "A/P CHECKS REGISTER " + STRING(v-trnum)
 str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
 x = (112 - LENGTH(str-tit)) / 2
 str-tit  = FILL(" ",x) + str-tit
 x = (114 - LENGTH(str-tit2)) / 2
 str-tit2 = FILL(" ",x) + str-tit2
 x = (132 - LENGTH(str-tit3)) / 2
 str-tit3 = FILL(" ",x) + str-tit3
 gtot0 = 0
 gtot1 = 0
 gtot2 = 0
 ctr   = 0.

EMPTY TEMP-TABLE tt-post.

{sys/inc/print1.i}
    if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
        list-name2 = tmp-dir + vTextFile2
        init-dir = tmp-dir.     
    ASSIGN lv-list-name[1] = list-name
        lv-list-name[2] = list-name2 .

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Check Number,Vendor Number,Name,Check Date,Invoice Date,"
              + "Number,Due,Discount Taken,Check Amt Paid,Pre-Issued Date".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

/* gdm - 05210901 */
if index(fi_CheckFile,'P',1) > 0 then assign
   fi_CheckFile = replace(fi_CheckFile,'P:',"D:").

IF tb_APcheckFile 
  THEN OUTPUT STREAM checkFile TO VALUE(fi_CheckFile).
/* gdm - 05210901 end */

  

  DISPLAY "" WITH FRAME r-top.
  DISPLAY "" WITH FRAME f-top.

  FOR EACH ap-sel NO-LOCK
      WHERE ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.check-no     GT 0
        AND ap-sel.man-check    EQ post-manual
      AND TRIM(ap-sel.inv-no) GT ""
      AND CAN-FIND(FIRST ap-chk
                   WHERE ap-chk.company   EQ ap-sel.company
                     AND ap-chk.check-no  EQ ap-sel.check-no
                     AND ap-chk.man-check EQ ap-sel.man-check),

    FIRST ap-inv NO-LOCK
    WHERE ap-inv.company EQ ap-sel.company
      AND ap-inv.vend-no EQ ap-sel.vend-no
      AND ap-inv.inv-no  EQ ap-sel.inv-no,

    FIRST vend NO-LOCK
    WHERE vend.company EQ ap-sel.company
      AND vend.vend-no EQ ap-sel.vend-no

    BREAK BY ap-sel.check-no WITH STREAM-IO WIDTH 132 NO-BOX NO-ATTR-SPACE NO-LABELS:

  IF FIRST-OF(ap-sel.check-no) THEN v-fst-chk = YES.
  IF LAST-OF(ap-sel.check-no)  THEN V-lst-chk = YES.

  IF ap-sel.vend-no EQ "VOID" THEN DO:
    DISPLAY ap-sel.check-no    FORMAT "zzzzzzz9"
            ap-sel.vend-no
            SKIP(1)
        WITH FRAME vv no-box no-labels NO-ATTR-SPACE STREAM-IO.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' STRING(ap-sel.check-no,"zzzzzzz9") '",'
           '"' ap-sel.vend-no                     '",'
           SKIP.
/***************************************************************************
  gdm - 05210901 - PROCEDURE NAME MUST BE THE SAME AS THE CHARACTER VALUE 
                   WITH OUT SPACES
                   E.G 
                    CHAR VALUE = "Positive Pay"
                    PROCEDURE  = "PositivePay"
******************************************************************************/
    IF tb_APcheckFile THEN  RUN VALUE(REPLACE(TRIM(v-fileFormat)," ","")).  
    ASSIGN v-amt-paid = 0.
/* gdm - 05210901 end */

    NEXT.
  END.

  CREATE tt-post.
  ASSIGN
   tt-post.row-id    = ROWID(ap-sel)
   tt-post.curr-bal  = ap-sel.inv-bal
   tt-post.curr-disc = ap-sel.disc-amt
   tt-post.curr-paid = ap-sel.amt-paid.
    
  RELEASE currency.
  IF lv-comp-curr NE "" AND lv-comp-curr NE ap-inv.curr-code[1] THEN
  FIND FIRST currency NO-LOCK
      WHERE currency.company     EQ ap-inv.company
        AND currency.c-code      EQ ap-inv.curr-code[1]
        AND currency.ar-ast-acct NE ""
        AND currency.ex-rate     GT 0
      NO-ERROR.

  IF AVAIL currency THEN
    ASSIGN
     tt-post.actnum    = currency.ar-ast-acct
     tt-post.ex-rate   = currency.ex-rate
     tt-post.curr-disc = tt-post.curr-disc * tt-post.ex-rate
     tt-post.curr-paid = tt-post.curr-paid * tt-post.ex-rate
     tt-post.curr-bal  = tt-post.curr-bal  * tt-post.ex-rate.

  ASSIGN v-postable = YES.

  IF v-fst-chk THEN
  DO:
    DISPLAY TRIM(STRING(ap-sel.check-no,">>>>>>>>")) FORMAT "x(8)"
            vend.vend-no
            vend.name
            SPACE(6)
        WITH FRAME a STREAM-IO.

    IF tb_excel THEN
    DO:
       FIND FIRST ap-chk WHERE
          ap-chk.company  EQ ap-sel.company AND
          ap-chk.check-no EQ ap-sel.check-no
          NO-LOCK NO-ERROR.

       PUT STREAM excel UNFORMATTED
           '"' TRIM(STRING(ap-sel.check-no,">>>>>>>>")) '",'
           '"' vend.vend-no                             '",'
           '"' vend.NAME                                '",'
           '"' ap-chk.check-date                        '",'.
    END.
  END.
  ELSE IF tb_excel THEN
     PUT STREAM excel UNFORMATTED
         '"' "" '",'
         '"' "" '",'
         '"' "" '",'
         '"' "" '",'.

  DISPLAY ap-inv.inv-date           FORMAT "99/99/99"
          SPACE(2)
          ap-sel.inv-no
          tt-post.curr-bal  TO 91
          tt-post.curr-disc TO 102
          tt-post.curr-paid TO 117

      WITH FRAME a STREAM-IO WIDTH 132 NO-LABELS NO-BOX.

  IF tb_excel THEN
  DO:
     PUT STREAM excel UNFORMATTED
         '"' ap-inv.inv-date                            '",'
         '"' ap-sel.inv-no                              '",'
         '"' STRING(tt-post.curr-bal,"->>,>>>,>>9.99")  '",'
         '"' STRING(tt-post.curr-disc,"->>,>>9.99 ")    '",'
         '"' STRING(tt-post.curr-paid,"->>,>>>,>>9.99") '",'.
  END.
  
  IF ap-sel.man-check THEN
  DO:
     DISPLAY ap-sel.pre-date TO 130 WITH FRAME a.
     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' ap-sel.pre-date '",'.
  END.

  IF tb_excel THEN
     PUT STREAM excel UNFORMATTED SKIP.

  ASSIGN
   tot0 = tot0 + tt-post.curr-bal 
   tot1 = tot1 + tt-post.curr-disc
   tot2 = tot2 + tt-post.curr-paid.

  /* gdm - 05210901 */
  ASSIGN v-amt-paid = v-amt-paid + ap-sel.amt-paid.

  IF v-lst-chk THEN DO:
    FIND FIRST ap-chk
        WHERE ap-chk.company  EQ ap-sel.company
          AND ap-chk.check-no EQ ap-sel.check-no
        NO-LOCK NO-ERROR.

    DISPLAY ap-chk.check-date AT 30     FORMAT "99/99/99"
            "** CHECK TOTAL"  AT 51
            tot0              TO 91
            tot1              TO 102
            tot2              TO 117 "*"
            SKIP(1)

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME ctot WIDTH 132 STREAM-IO.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                            '",'
           '"' ""                            '",'
           '"' ""                            '",'
           '"' ""                            '",'
           '"' "** CHECK TOTAL"              '",'
           '"' ""                            '",'
           '"' STRING(tot0,"->>,>>>,>>9.99") '",'
           '"' STRING(tot1,"->>,>>9.99 ")    '",'
           '"' STRING(tot2,"->>,>>>,>>9.99") '",'
           '"' "*"                           '",'
           SKIP(1).

/***************************************************************************
  gdm - 05210901 - PROCEDURE NAME MUST BE THE SAME AS THE CHARACTER VALUE 
                   WITH OUT SPACES
                   E.G 
                    CHAR VALUE = "Positive Pay"
                    PROCEDURE  = "PositivePay"
******************************************************************************/
  IF tb_APcheckFile THEN RUN VALUE(REPLACE(TRIM(v-fileFormat)," ","")).  
  ASSIGN v-amt-paid = 0.
/* gdm - 05210901 end */

    ASSIGN
     ctr   = ctr + 1
     gtot0 = gtot0 + tot0
     gtot1 = gtot1 + tot1
     gtot2 = gtot2 + tot2
     tot0  = 0
     tot1  = 0
     tot2  = 0
     v-amt-paid = 0.
  END.

  ASSIGN
   v-fst-chk = NO
   v-lst-chk = NO.
  
  ACCUM ap-sel.disc-amt (TOTAL).
  ACCUM ap-sel.amt-paid (TOTAL).
END. /* each ap-sel */

DISPLAY "*** GRAND TOTALS ***" AT 50
        gtot0                  TO 91
        gtot1                  TO 102
        gtot2                  TO 117 "**" SKIP (1)
        "NUMBER OF CHECKS WRITTEN " ctr
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME b WIDTH 132 STREAM-IO.

IF tb_APcheckFile 
  THEN OUTPUT STREAM checkFile CLOSE.

IF tb_excel THEN
   PUT STREAM excel UNFORMATTED
       '"' ""                             '",'
       '"' ""                             '",'
       '"' ""                             '",'
       '"' ""                             '",'
       '"' "*** GRAND TOTALS ***"         '",'
       '"' ""                             '",'
       '"' STRING(gtot0,"->>,>>>,>>9.99") '",'
       '"' STRING(gtot1,"->>,>>9.99 ")    '",'
       '"' STRING(gtot2,"->>,>>>,>>9.99") '",'
       '"' "**"                           '",'
       SKIP.

ASSIGN   /* For posting without currency exchange rate */
 gtot1 = (ACCUM TOTAL ap-sel.disc-amt)
 gtot2 = (ACCUM TOTAL ap-sel.amt-paid).

IF tb_prt-acc THEN DO:
  HIDE FRAME f-top.

  ASSIGN
    str-tit3 = "Period " + STRING(tran-period,"99") + " - " + "Summary by Account"
    x = (132 - LENGTH(str-tit3)) / 2
    str-tit3 = FILL(" ",x) + str-tit3.

  PAGE.

  FORM HEADER
       "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#    "
       "LINE DESCRIPTION              QTY    UNIT PRICE     AMT PAID" SKIP
       FILL("_",132) FORMAT "x(132)"
  
      WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO.

  DISPLAY "" WITH FRAME f-top2.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no
        AND ap-inv.freight NE 0,
    
      FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend
    
      BREAK BY ap-inv.vend-no:
    
    IF FIRST(ap-inv.vend-no) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ ap-inv.company
            AND account.actnum  EQ v-frt-acct
          NO-ERROR.
      PUT v-frt-acct + " - " +
          (IF AVAIL account THEN account.dscr ELSE "Not on file")
                                FORMAT "x(40)" SKIP.
    END.

    v-frgt-amt = ap-sel.amt-paid *
                 (ap-inv.freight / (ap-inv.net + ap-inv.freight)).
    
    PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Freight"                       FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        SPACE(1)
        ap-inv.freight          TO 118
        v-frgt-amt              TO 131
        SKIP.
  
    ACCUM v-frgt-amt (TOTAL).
  
    IF LAST(ap-inv.vend-no) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL v-frgt-amt) FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,
    
      FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend,
    
      EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no USE-INDEX i-no NO-LOCK
    
      BREAK BY ap-invl.actnum
            BY ap-invl.inv-no
            BY ap-invl.line
          
      WITH WIDTH 132 NO-LABELS:
      
    IF FIRST-OF(ap-invl.actnum) THEN DO:
      FIND FIRST account
          WHERE account.company eq ap-inv.company
            AND account.actnum  eq ap-invl.actnum
          NO-LOCK NO-ERROR.
        
      PUT ap-invl.actnum + " - " +
              (IF AVAIL account THEN account.dscr ELSE "Not on file")
                            FORMAT "x(40)" SKIP.
    END.

    v-line-amt = ap-sel.amt-paid * (ap-invl.amt / (ap-inv.net + ap-inv.freight)).
  
    PUT ap-invl.po-no         AT 34
        SPACE(1)
        ap-inv.inv-date       FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(1)
        {ap/invlline.i -1}    FORMAT ">>>9"
        SPACE(1)
        ap-invl.dscr          FORMAT "x(18)"
        SPACE(1)
        ap-invl.qty           FORMAT "->>,>>9.9<<"
        SPACE(1)
        ap-invl.unit-pr
        SPACE(1)
        v-line-amt
        SPACE(1)
        SKIP.
      
    ACCUM v-line-amt (TOTAL BY ap-invl.actnum).
    ACCUM v-line-amt (TOTAL).
  
    IF LAST-OF(ap-invl.actnum) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL BY ap-invl.actnum v-line-amt)
                        FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  FOR EACH tt-post WHERE tt-post.actnum NE "",

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no
    
      BREAK BY tt-post.actnum
          
      WITH WIDTH 132 NO-LABELS:
      
    IF FIRST-OF(tt-post.actnum) THEN DO:
      FIND FIRST account
          WHERE account.company eq ap-sel.company
            AND account.actnum  eq tt-post.actnum
          NO-LOCK NO-ERROR.
        
      PUT tt-post.actnum + " - " +
              (IF AVAIL account THEN account.dscr ELSE "Not on file")
                            FORMAT "x(40)" SKIP.
    END.
  
    PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Currency"                      FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        tt-post.curr-paid - ap-sel.amt-paid TO 131
        SKIP.
      
    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum).
    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL).
  
    IF LAST-OF(tt-post.actnum) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
                        FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  PUT "***** TOTAL for ALL ACCOUNTS " to 116
      (ACCUM TOTAL v-line-amt) +
       (ACCUM TOTAL v-frgt-amt) +
        (ACCUM TOTAL tt-post.curr-paid - ap-sel.amt-paid)
                        FORMAT "->>,>>>,>>9.99" to 130
      SKIP(2).
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  /*IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
END.

END PROCEDURE.

PROCEDURE create-bank-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-data-file AS cha NO-UNDO.

  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR v-account AS char NO-UNDO.
  DEF VAR v-ref AS cha NO-UNDO.
  DEF VAR v-check-date AS DATE NO-UNDO.
  DEF VAR v-check-date-string AS cha NO-UNDO.
  DEF VAR v-total-amt AS DEC NO-UNDO.
  DEF VAR v-check-total-amt AS DEC NO-UNDO.

  ASSIGN targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
         dirname1 = aplockbx-path
         .

  IF SEARCH(dirname1) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
  END.

  OUTPUT TO VALUE(targetfile).
  PUT UNFORMATTED "01021226C3648091" SKIP.
  v-total-amt = 0.
  FOR EACH tt-post,
      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,
      FIRST ap-chk NO-LOCK WHERE ap-chk.company   EQ ap-sel.company
                             AND ap-chk.check-no  EQ ap-sel.check-no,
      FIRST ap-inv NO-LOCK WHERE ap-inv.company EQ ap-sel.company
                             AND ap-inv.vend-no EQ ap-sel.vend-no
                             AND ap-inv.inv-no  EQ ap-sel.inv-no,    
      FIRST vend NO-LOCK WHERE vend.company EQ ap-inv.company
                           AND vend.vend-no EQ ap-inv.vend-no USE-INDEX vend 
         BREAK BY ap-sel.bank-code BY ap-sel.check-no:

      IF FIRST-OF(ap-sel.check-no) THEN v-check-total-amt = 0.

      v-check-total-amt = v-check-total-amt + ap-sel.amt-paid.
      IF last-of(ap-sel.check-no) THEN DO:
          find first bank where bank.company = ap-sel.company and
                             bank.bank-code = ap-sel.bank-code NO-ERROR.

          ASSIGN
          v-account = IF AVAIL bank THEN bank.bk-act ELSE ""
          v-ref = SUBSTRING(vend.name,1,12)
          v-check-date = IF ap-sel.man-check THEN ap-sel.pre-date ELSE ap-sel.check-date
          v-check-date-string = STRING(MONTH(v-check-date),"99") +
                                STRING(DAY(v-check-date),"99") + 
                                SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
          v-total-amt = v-total-amt + v-check-total-amt.
          PUT UNFORMATTED "V"
              ap-chk.check-no FORM "9999999999"
              v-account FORM "99999999999999"
              v-check-total-amt * 100 FORM "9999999999"
              v-ref FORM  "x(12)"
              v-check-date-string FORM "x(6)"
              SKIP.
      END.

  END.
  PUT UNFORMATTED "T          "
      v-account FORM "99999999999999"
      v-total-amt * 100 FORM "9999999999"
      SKIP.

  OUTPUT CLOSE.
  op-data-file = targetfile.

END PROCEDURE.

PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF BUFFER b-ap-pay FOR ap-pay.  

DEF VAR lv-check-no LIKE ap-chk.check-no NO-UNDO.

  
/** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
/*DO TRANSACTION:*/
  FOR EACH tt-post WHERE tt-post.actnum NE "",

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK

      BREAK BY tt-post.actnum:

    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum). 

    IF LAST-OF(tt-post.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = tt-post.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER CURRENCY GAIN/LOSS"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.

      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = tt-post.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER CURRENCY GAIN/LOSS"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  END.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id
            EXCLUSIVE-LOCK,

      FIRST ap-chk EXCLUSIVE-LOCK
      WHERE ap-chk.company   EQ ap-sel.company
        AND ap-chk.check-no  EQ ap-sel.check-no,

      FIRST ap-inv EXCLUSIVE-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,
    
      FIRST vend EXCLUSIVE-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend

    BREAK BY ap-sel.bank-code
          BY ap-sel.check-no:

    IF ap-sel.man-check AND ap-sel.check-date EQ ? THEN
       ap-sel.check-date = ap-sel.pre-date.

    ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.check-no).
    ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.bank-code).

    IF FIRST-OF(ap-sel.check-no) THEN i = 0.

    lv-check-no = ap-sel.check-no.

    IF NOT ap-sel.man-check AND tb_void THEN DO:
      FIND LAST ap-pay NO-LOCK
          WHERE ap-pay.company   EQ ap-sel.company
            AND ap-pay.check-act EQ ap-sel.actnum
            AND ap-pay.check-no  LT ap-sel.check-no
          NO-ERROR.
      lv-check-no = (IF AVAIL ap-pay THEN ap-pay.check-no ELSE 0) + 1.
    END.

    DO lv-check-no = lv-check-no TO ap-sel.check-no:
      FIND FIRST ap-pay
          WHERE ap-pay.company   EQ ap-sel.company
            AND ap-pay.check-act EQ ap-sel.actnum
            AND ap-pay.check-no  EQ lv-check-no
          NO-ERROR.

      IF NOT AVAIL ap-pay THEN DO:
        FIND LAST ap-pay USE-INDEX c-no NO-ERROR.
        x = IF AVAIL ap-pay THEN ap-pay.c-no ELSE 0.

        CREATE ap-pay.
        ASSIGN
         ap-pay.company   = cocode
         ap-pay.check-act = ap-sel.actnum
         ap-pay.check-no  = lv-check-no
         ap-pay.period    = tran-period
         ap-pay.c-no      = x + 1
         ap-pay.vend-no   = ap-sel.vend-no
         ap-pay.bank-code = ap-sel.bank-code.

        IF ap-pay.check-no NE ap-sel.check-no THEN
          ASSIGN
           ap-pay.posted     = YES
           ap-pay.d-no       = ap-sel.check-no
           ap-pay.cleared    = NO.
      END.
    END.

    IF ap-sel.man-check THEN
      ASSIGN
       ap-pay.check-date  = ap-sel.pre-date
       ap-pay.man-check   = YES.
    ELSE
      ap-pay.check-date = ap-sel.check-date.

    ap-pay.posted = YES.
    IF ap-pay.check-date EQ ? THEN ap-pay.check-date = TODAY.

    FOR EACH b-ap-pay
        WHERE b-ap-pay.company EQ ap-pay.company
          AND b-ap-pay.d-no    EQ ap-pay.check-no
          AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ b-ap-pay.c-no)
          EXCLUSIVE-LOCK
        USE-INDEX d-no:
      b-ap-pay.check-date = ap-pay.check-date.
    END.

    FIND LAST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no USE-INDEX c-no NO-ERROR.
    i = IF AVAIL ap-payl THEN ap-payl.line ELSE 0.

    CREATE ap-payl.
    ASSIGN
     ap-payl.posted    = YES
     ap-payl.c-no      = ap-pay.c-no
     ap-payl.check-no  = ap-sel.check-no
     ap-payl.line      = i + 1
     ap-payl.inv-no    = ap-sel.inv-no
     ap-payl.due-date  = ap-sel.due-date
     ap-payl.amt-disc  = ap-sel.disc-amt
     ap-payl.amt-paid  = ap-sel.amt-paid
     ap-payl.vend-no   = ap-sel.vend-no
     ap-payl.man-check = ap-sel.man-check
     ap-payl.actnum    = ap-sel.actnum.

    ASSIGN
     ap-inv.paid       = ap-inv.paid + ap-sel.amt-paid
     ap-inv.check-no   = ap-sel.check-no
     ap-inv.pay-date   = ap-sel.check-date
     ap-inv.disc-taken = ap-inv.disc-taken + ap-sel.disc-amt
     ap-inv.due        = (ap-inv.net + ap-inv.freight) -
                          ap-inv.paid - ap-inv.disc-taken
     ap-payl.amt-due   = ap-inv.due.

    vend.acc-bal = vend.acc-bal - ap-sel.disc-amt - ap-sel.amt-paid.

    IF ap-inv.due      LE 0 AND
       ap-inv.pay-date NE ? AND
       ap-inv.inv-date NE ? THEN
      ASSIGN
       vend.avg-pay = ((vend.avg-pay * vend.num-inv) +
                       (ap-inv.pay-date - ap-inv.inv-date)) /
                      (vend.num-inv + 1)
       vend.num-inv = vend.num-inv + 1. /* number of invoices paid */

    FIND FIRST bank WHERE
         bank.company   = cocode AND
         bank.bank-code = ap-sel.bank-code
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bank THEN bank.bal = bank.bal - ap-sel.amt-paid.

    IF LAST-OF(ap-sel.check-no) THEN DO:
      ASSIGN
       vend.last-pay    = ap-sel.check-date
       ap-pay.check-amt = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
       vend.lpay        = ap-pay.check-amt
       vend.lpay-date   = ap-sel.check-date.

      CREATE ap-ledger.
      ASSIGN
       ap-ledger.company   = ap-sel.company
       ap-ledger.vend-no   = ap-sel.vend-no
       ap-ledger.refnum    = "AC" + STRING(ap-sel.check-no, "999999")
       ap-ledger.ref-date  = ap-sel.check-date
       ap-ledger.tr-date   = tran-date
       ap-ledger.trnum     = v-trnum
       ap-ledger.period    = tran-period
       ap-ledger.amt       = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
       ap-ledger.actnum    = wcash.
      RELEASE ap-ledger.
    END.

    /*** Moved gltrans create here so bank's actnum can be used ***/
    IF LAST-OF(ap-sel.bank-code) THEN DO:
      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = bank.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = - ACCUM TOTAL BY ap-sel.bank-code ap-sel.amt-paid
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.

    DELETE ap-sel.

    IF NOT CAN-FIND(FIRST ap-sel WHERE ap-sel.company   EQ ap-chk.company
                                   AND ap-sel.check-no  EQ ap-chk.check-no
                                   AND ap-sel.man-check EQ ap-chk.man-check)
    THEN DELETE ap-chk.
  END. /* for each tt-post */

  RELEASE vend.
  RELEASE bank.
  RELEASE ap-pay.
  RELEASE ap-payl.
  RELEASE ap-inv.

  FOR EACH ap-sel
      WHERE ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.man-check    EQ post-manual
        AND TRIM(ap-sel.inv-no) LE ""
      EXCLUSIVE-LOCK:
    DELETE ap-sel.
  END.

  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

  CREATE gltrans.
  ASSIGN
    gltrans.company = cocode
    gltrans.actnum  = ap-ctrl.payables
    gltrans.jrnl    = "APCKR"
    gltrans.tr-dscr = "AP CHECK REGISTER"
    gltrans.tr-date = tran-date
    gltrans.tr-amt  = gtot1 + gtot2
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
  RELEASE gltrans.

  IF gtot1 NE 0 THEN DO:
    CREATE gltrans.
    ASSIGN
     gltrans.company = cocode
     gltrans.actnum  = ap-ctrl.discount
     gltrans.jrnl    = "APCKR"
     gltrans.tr-dscr = "AP CHECK REGISTER"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = - gtot1
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.
  END.
/*END. */

END PROCEDURE.

PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.
  
  ASSIGN targetfile = lv-audit-dir + "\AP\VC3\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VC3".

  if index(list-name,'P',1) > 0 then assign
   list-name = replace(list-name,'P:',"D:").

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
END PROCEDURE.

PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  REPEAT:
    FIND FIRST gl-ctrl EXCLUSIVE-LOCK
      WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
    IF AVAIL gl-ctrl THEN DO:

      ASSIGN v-trnum       = gl-ctrl.trnum + 1 
             gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      LEAVE.
    END. /* IF AVAIL gl-ctrl */
  END. /* REPEAT */
  


END PROCEDURE.


PROCEDURE PositivePay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: gdm - 05210901
------------------------------------------------------------------------------*/

IF tb_APcheckFile THEN DO:

    FIND FIRST bank NO-LOCK
        WHERE bank.company EQ cocode
          AND bank.bank-code EQ ap-sel.bank-code NO-ERROR.

   PUT STREAM checkFile UNFORMATTED
    "D"
     STRING(INT(ap-sel.bank-code),"999")                         /* Bank Number    */
     STRING(INT(REPLACE(bank.bk-act,"-","")), "9999999999")      /* Account Number */
     STRING(INT(ap-sel.check-no),"9999999999")                   /* Check Number   */
     STRING(INT(REPLACE(STRING(v-amt-paid,"->>,>>>,>>9.99"),".","")), "9999999999999") 
                                                                 /* Amount         */ 
     IF ap-sel.check-date NE ? 
        THEN STRING(YEAR(ap-sel.check-date),"9999") +  
             STRING(MONTH(ap-sel.check-date),"99")  +     
             STRING(DAY(ap-sel.check-date),"99")   
        ELSE "00000000"                                          /* Issue Date     */
     FILL(" ",30)                                                /* Additinal data */ 
     IF TRIM(vend.name) NE ""
       THEN
        TRIM(vend.name) + FILL(" ",(80 - LENGTH(TRIM(vend.name))))
       ELSE FILL(" ",80)                                         /* Payee NAME     */
     IF ap-sel.vend-no EQ "VOID"
          THEN "V" ELSE FILL(" ",1)                                       /* Void Indicator */
    SKIP.

END.



END PROCEDURE.
