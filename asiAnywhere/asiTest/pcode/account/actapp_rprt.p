

/*------------------------------------------------------------------------
    File        : actapp_rprt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttOnAccountApplicationReport NO-UNDO
        FIELD actapp AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsOnAccountApplicationReport FOR ttOnAccountApplicationReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcst         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcst         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOnAccountApplicationReport.

     IF  prmUser       = ? THEN ASSIGN   prmUser       = "".   
     IF  prmAction     = ? THEN ASSIGN   prmAction     = "". 
     IF  prmtrnsdt     = ? THEN ASSIGN   prmtrnsdt     = "". 
     IF  prmperiod     = ? THEN ASSIGN   prmperiod     = 0.
     IF  prmbegcst     = ? THEN ASSIGN   prmbegcst     = "". 
     IF  prmendcst     = ? THEN ASSIGN   prmendcst     = "".   
     IF  prmOut        = ? THEN ASSIGN   prmOut        = "". 
     
     
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE tran-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.

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
 v-today = TODAY 
 g_company = cocode
 g_user    = prmUser 
tran-date   =   date(prmtrnsdt) .

 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

    
def buffer b-line for ar-cashl.
/*
if v-return then return.
*/
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

def var g1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var g2 like g1 NO-UNDO.
def var t3 like g1 NO-UNDO.
def var v1 like g1 NO-UNDO.
def var v2 like g1 NO-UNDO.
def var t2 like g1 NO-UNDO.
def var invo-sub like ar-inv.net NO-UNDO.
def var disc-sub like ar-inv.disc-taken NO-UNDO.
def var xtrnum    as int NO-UNDO.
def var xar-acct  as char NO-UNDO.
def var xdis-acct as char NO-UNDO.
def var xcust like cust.cust-no NO-UNDO.
def var xchk  like ar-cashl.check-no NO-UNDO.
def var tmp-amt-paid like ar-cashl.amt-paid NO-UNDO.
def var alf-check as char format "x(10)" no-undo.
def var v-non-zero as log no-undo.
def var fcust like ar-cashl.cust-no init "" NO-UNDO.
def var tcust like fcust            init "zzzzzzzz" NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var dsc     as decimal format "->>,>>>,>>9.99" NO-UNDO.
def var net-cr  as decimal format "->>,>>>,>>9.99" NO-UNDO.
def var ck-onac as decimal format "->>,>>>,>>9.99" NO-UNDO.



        find first period                   
           where period.company eq cocode
           and period.pst     le tran-date
           and period.pend    ge tran-date
           no-lock no-error.
        if avail period then do:
            IF NOT period.pstat THEN DO:
                cError = "Period Already Closed. " .
            END.
        prmperiod = (period.pnum).
        END.
        ELSE DO:
            cError = "No Defined Period Exists..".
            RETURN.
        end.
   


  IF prmAction = "actapp" THEN DO:

   ASSIGN            
                     
      begin_cust   = prmbegcst
      end_cust     = prmendcst
      tran-date    = date(prmtrnsdt) 
      tran-period  = prmperiod   
      .
       

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "PostActApp" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        find first period                   
           where period.company eq cocode
           and period.pst     le tran-date
           and period.pend    ge tran-date
           no-lock no-error.
        if avail period then do:
            IF NOT period.pstat THEN DO:
                cError = "Period Already Closed. " .
            END.
        prmperiod = (period.pnum).
        END.
        ELSE DO:
            cError = "No Defined Period Exists..".
            RETURN.
        end.

       REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN xtrnum        = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        RELEASE gl-ctrl. 
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
       END.
     
       RUN run-report.


  IF v-postable THEN DO:
 
    IF prmOut = "Yes" THEN do:      
        RUN post-gl.
        RUN copy-report-to-audit-dir.
             cError = "Posting Complete" .
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      cError = "No Cash Receipts available for posting...".
      RUN undo-trnum.
  END.


  CREATE ttOnAccountApplicationReport.
        ASSIGN ttOnAccountApplicationReport.actapp = vTextFile .

  END.

/*****************************************************************************************/

PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */

/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i VALUE(lines-per-page)}


v-postable = NO.

form header
    "CUST.#   NAME                                    CHECK#      DATE         "
    "ON ACCOUNT       INVOICE #          APPLIED     DISCOUNT"
    skip fill("_",132) format "x(132)"

  with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

assign
 time_stamp = string(time, "HH:MMam")
 tmpstore   = fill("_",132).

{sys/form/r-top3w.f}

ASSIGN fcust = begin_cust
       tcust = end_cust.


    assign
     str-tit  = coname + " - " + loname
     str-tit2 = "CASH ON ACCOUNT APPLICATION  -  EDIT REGISTER " +
                string(xtrnum)
     str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date)
     x = (112 - length(str-tit)) / 2
     str-tit  = fill(" ",x) + str-tit
     x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.


    display "" with frame r-top.
    display "" with frame f-top.

    for each ar-cashl
        where ar-cashl.company    eq cocode
         and ar-cashl.cust-no    ge fcust
          and ar-cashl.cust-no    le tcust
          and ar-cashl.on-account eq yes
          and ar-cashl.inv-no     ne 0
          and (ar-cashl.amt-paid  ne 0 or
               ar-cashl.amt-disc  ne 0) 
        no-lock,

        first ar-cash
        where ar-cash.c-no eq ar-cashl.c-no
        NO-LOCK,

        first cust
        where cust.company eq ar-cash.company
         /* and cust.cust-no eq ar-cash.cust-no */
        no-lock 
                                                       
        break by ar-cashl.cust-no by ar-cashl.check-no 

        with frame a1:

      if first-of(ar-cashl.cust-no) then do:
         xcust = cust.cust-no.
         put cust.cust-no space(1)
             cust.name.
      end.

      ELSE if first-of(ar-cashl.check-no) then put skip(1).

      if first-of(ar-cashl.check-no) then do:
        put skip(1).
        ASSIGN
        xchk = ar-cashl.check-no
        ck-onac = 0.
        for each b-line FIELDS(amt-paid amt-disc)
            where b-line.company    eq cocode
              and b-line.cust-no    eq xcust
              and b-line.on-account eq yes
              and b-line.check-no   eq xchk:
            ck-onac = ck-onac + b-line.amt-paid - b-line.amt-disc.
        end.
        
        alf-check = string(ar-cashl.check-no,"9999999999").
        
        run alf-check.
         
        do i = 1 to length(alf-check):
          if substr(alf-check,i,1) ne "0" then v-non-zero = yes.
          else
          if not v-non-zero then substr(alf-check,i,1) = " ".
        end.
        
        put alf-check to 55
            ar-cash.check-date  at 60
            ck-onac at 72
            ar-cashl.inv-no at 95.
            
        v2 = v2 + ck-onac.
      end.

      else put ar-cashl.inv-no at 95.

      if ar-cashl.amt-paid ne 0 then put space(3) ar-cashl.amt-paid.
      else put space(17).

      if ar-cashl.amt-disc ne 0 then put space(3) ar-cashl.amt-disc skip.
      else put skip.

      assign
       invo-sub = invo-sub + ar-cashl.amt-paid
       disc-sub = disc-sub + ar-cashl.amt-disc
       dsc      = dsc      + ar-cashl.amt-disc
       net-cr   = net-cr   + ar-cashl.amt-paid - ar-cashl.amt-disc.

      if last-of(ar-cashl.cust-no) then do:
        display  "*  CUSTOMER TOTALS"  to 70
                 v2                    at 72
                 invo-sub              at 104 space(3)
                 disc-sub
                 "*"                   to 132 skip(1)

            with frame vtot no-box no-labels width 132 STREAM-IO.

        assign
         g1       = g1 + invo-sub
         g2       = g2 + v2
         invo-sub = 0
         disc-sub = 0
         v1       = 0
         v2       = 0.
      end.
      v-postable = YES.
    end. /* each cashline */

    do with frame gt width 132 no-labels NO-UNDERLINE STREAM-IO:
      display  "** BEGINNING CASH ON ACCOUNT"  to 101 g2     to 118
               "** GROSS AMOUNT APPLIED"       to 101 net-cr to 118
               "** TOTAL DISCOUNTS"            to 101 dsc    to 118
               "** NET AMOUNT APPLIED"         to 101 net-cr - dsc    to 118
               "** ENDING CASH ON ACCOUNT"     to 101 g2 - (g1 - dsc) to 118

          with no-labels no-underline width 132 frame gt.
      ASSIGN G1 = 0
             G2 = 0
             NET-CR = 0
             DSC = 0
             .
    end.

    hide frame f-top.

    assign
     str-tit3 = "Period " + string(tran-period,"99") + " " +
                string(tran-date) + " - " + "Summary by Account"
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.

    page.

    form header
         "ACCOUNT                                   DATE   CUST #   NAME"
         "                           CHECK#     INV#           AMOUNT" skip
         fill("_",132) format "x(130)"

        with no-labels no-box no-underline frame f-top2 page-top width 132 STREAM-IO.

    display "" with frame f-top2.

    for each ar-cashl NO-LOCK
        where ar-cashl.company    eq cocode
          and ar-cashl.cust-no    ge fcust
          and ar-cashl.cust-no    le tcust
          and ar-cashl.on-account eq yes
          and ar-cashl.inv-no     ne 0,

        first ar-cash
        where ar-cash.c-no eq ar-cashl.c-no
        no-lock,

        first cust
        where cust.company eq ar-cash.company
        /*  and cust.cust-no eq ar-cash.cust-no*/
        no-lock

        break by ar-cashl.actnum
              by ar-cashl.cust-no
              by ar-cashl.check-no
              by ar-cashl.inv-no

        with width 132 no-labels:

      if first-of(ar-cashl.actnum) then do:
        find first account
            where account.company eq cocode
              and account.actnum  eq ar-cashl.actnum
            no-lock no-error.
        if avail account then
           put ar-cashl.actnum + " - " + account.dscr format "x(39)" .
        else
           put ar-cashl.actnum.
      end.

      if ar-cashl.dscr = "credit" then
        tmp-amt-paid = (- ar-cashl.amt-paid).
      else
        tmp-amt-paid = ar-cashl.amt-paid.
        
      alf-check = string(ar-cash.check-no,"9999999999").
      
      run alf-check.
               
      put  ar-cash.check-date at 41 space(1)
           ar-cash.cust-no          space(1)
           cust.name                space(1)
           alf-check space(1)
           ar-cashl.inv-no format ">>>>>9"  space(1)
           tmp-amt-paid      format "->>,>>>,>>9.99"
           skip.

      accumulate ar-cashl.amt-paid (total by ar-cashl.actnum).
      accumulate ar-cashl.amt-paid (total).
      accumulate ar-cashl.amt-disc (total).

      if last-of(ar-cashl.actnum) then
        put skip
            accum total by ar-cashl.actnum ar-cashl.amt-paid
                format "->>,>>>,>>9.99" to 117 " *" skip.

      if last(ar-cashl.actnum) then
        put skip(1)
            "***** TOTAL FOR ALL ACCOUNTS " to 95
            accum total ar-cashl.amt-paid to 117 " **" skip
            "***** TOTAL DISCOUNTS " to 95
            accum total ar-cashl.amt-disc to 117.
    end.

END PROCEDURE.
PROCEDURE post-gl :
DO TRANSACTION:
      for each ar-cashl
          where ar-cashl.company    eq cocode
            and ar-cashl.cust-no    ge fcust
            and ar-cashl.cust-no    le tcust
            and ar-cashl.on-account eq yes
            and ar-cashl.inv-no     ne 0,

          first ar-cash
          where ar-cash.c-no eq ar-cashl.c-no
          no-lock

          break by ar-cashl.cust-no
                by ar-cashl.check-no
                by ar-cashl.inv-no:

        {ar/ar-oreg.i ar-cashl 1}

        ar-cashl.posted = YES.
      end.
 end. /* postit: transaction */

 FIND CURRENT ar-cashl NO-LOCK NO-ERROR.
 FIND CURRENT ar-inv NO-LOCK NO-ERROR.
 FIND CURRENT cust NO-LOCK NO-ERROR.

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
  
  ASSIGN targetfile = lv-audit-dir + "\AR\AC6\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC6".

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

  DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

END PROCEDURE.
PROCEDURE alf-check :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  assign
   alf-check  = fill(" ",10 - length(trim(alf-check))) + trim(alf-check)
   v-non-zero = no.
         
  do i = 1 to length(alf-check):
    if substr(alf-check,i,1) ne "0" and
       substr(alf-check,i,1) ne " " then v-non-zero = yes.
    else
    if not v-non-zero then substr(alf-check,i,1) = " ".
  end.

END PROCEDURE.
