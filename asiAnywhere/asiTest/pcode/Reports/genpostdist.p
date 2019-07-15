

/*------------------------------------------------------------------------
    File        : genpostdist.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttgenpostdistrib NO-UNDO
        FIELD distpost AS CHAR
        FIELD genat AS CHAR.

DEFINE DATASET dsgenpostdistrib FOR ttgenpostdistrib.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegacc        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndacc        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegindate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmReport        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPost          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmTrnsDate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPeriod        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsgenpostdistrib.

     IF prmUser         = ? THEN ASSIGN     prmUser         = "".   
     IF prmAction       = ? THEN ASSIGN     prmAction       = "". 
     IF prmBegacc       = ? THEN ASSIGN     prmBegacc       = "". 
     IF prmEndacc       = ? THEN ASSIGN     prmEndacc       = "". 
     IF prmBegindate    = ? THEN ASSIGN     prmBegindate    = "".  
     IF prmEnddate      = ? THEN ASSIGN     prmEnddate      = "".  
     IF prmOut          = ? THEN ASSIGN     prmOut          = "". 
     IF prmReport       = ? THEN ASSIGN     prmReport       = "". 
     IF prmPost         = ? THEN ASSIGN     prmPost         = "". 
     IF prmTrnsDate     = ? THEN ASSIGN     prmTrnsDate     = "". 
     IF prmPeriod       = ? THEN ASSIGN     prmPeriod       = "". 



    

DEFINE VARIABLE begin_accnt AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_accnt AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz"  NO-UNDO. 
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE rd_detsum AS CHARACTER INITIAL "S"  NO-UNDO.
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

DEF TEMP-TABLE tt-trans NO-UNDO
                        FIELD tt-recid AS RECID
                        FIELD TYPE AS cha
                        FIELD c-rate LIKE acctcost.c-rate
                        FIELD costacct LIKE acctcost.costacct
                        FIELD jrnl LIKE gltrans.jrnl
                        FIELD tr-amt LIKE gltrans.tr-amt
                        INDEX tt-trans IS PRIMARY TYPE tt-recid.

DEF VAR v-distribute AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.

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
 g_user    = prmUser .


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .




  IF prmAction = "journal" THEN DO:
      
   ASSIGN
       begin_accnt =   prmBegacc    
       end_accnt   =   prmEndacc    
       begin_date  =   date(prmBegindate) 
       end_date    =   date(prmEnddate)
       rd_detsum   =   prmReport
       tran-date   =   date(prmTrnsDate)
       tran-period =   int(prmPeriod)    . 


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "GenAuto" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .



         v-invalid = no.

    FIND first period                   
        where period.company eq cocode
          and period.pst     ge begin_date
          and period.pend    le end_date
          AND NOT period.pstat
        no-lock no-error.
    if avail period THEN DO:
       cError = "Period is Closed. " .
       v-invalid = YES.
       RETURN.
    END.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period THEN DO:
       IF NOT period.pstat /* closed */ THEN DO:
          cError = "Period is Closed. " .
          v-invalid = YES.
       END.
       ELSE prmPeriod = string(period.pnum).
    END.
    ELSE DO:
      cError = "No Defined Period Exists" .
      v-invalid = yes.
    end.



       
  RUN run-report. 

  IF v-distribute  THEN do:
     
     IF prmPost = "Yes" THEN do:
        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/

           REPEAT:
              FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              IF AVAIL gl-ctrl THEN
              DO:
                 ASSIGN v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                 LEAVE.
              END.
           END.
        END.

        FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

        IF rd_detsum = "S" THEN RUN cost-distribute.
        ELSE RUN cost-distribute-det.
     END.
  END.
  v-distribute = NO.


   
  CREATE ttgenpostdistrib.
  
    ASSIGN ttgenpostdistrib.distpost = vTextFile .

  END.
/*****************************************************************************************/
  

PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: ap/rep/pjgl.p
**      
** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

DEF VAR lo_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
DEF VAR hi_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
DEF VAR DEBUG AS LOG NO-UNDO INITIAL TRUE.
DEF VAR ws_disc LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
DEF VAR ws_check-no LIKE ap-chk.check-no NO-UNDO FORMAT ">>>>>>>"
    COLUMN-LABEL "Check#".
DEF VAR ws_order-no LIKE oe-ord.ord-no NO-UNDO
    FORMAT ">>>>>>".
DEF VAR ws_jrnl LIKE gltrans.jrnl COLUMN-LABEL "Journal" NO-UNDO.
DEF VAR gl_jrnl_list AS CHAR NO-UNDO.
DEF VAR lo_actnum LIKE account.actnum LABEL "From GL Acct#" NO-UNDO.
DEF VAR hi_actnum LIKE account.actnum LABEL "Thru GL Acct#" NO-UNDO.
DEF VAR t-amt AS DEC NO-UNDO.
DEF VAR t-disc AS DEC NO-UNDO.
DEF VAR t-qty AS DEC NO-UNDO.
DEF VAR t-msf AS DEC NO-UNDO.
DEF VAR hdg_printed AS LOG NO-UNDO.
DEF VAR v-tot-amt AS DEC NO-UNDO.
DEF VAR v-tramt AS DEC NO-UNDO.

DEF BUFFER b-tt-trans FOR tt-trans.

FORM    ws_jrnl FORM "x(15)"
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.name
        ap-inv.inv-date COLUMN-LABEL "Date"
        ap-inv.inv-no COLUMN-LABEL "Invoice#"
        ws_check-no
        ws_order-no
        ap-invl.qty
        /*ap-invl.amt-msf*/
        ws_disc
        ap-invl.amt
        WITH FRAME f-det width 144 DOWN STREAM-IO.


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = "Generate Auto Distribution"
 {sys/inc/ctrtext.i str-tit2 112}
 
 lo_actnum    = begin_accnt
 hi_actnum    = end_accnt
 lo_trandate  = begin_date
 hi_trandate  = end_date
 gl_jrnl_list = /*(if tb_cashr    then "!CASHR,"    else "") +
                (if tb_general  then "!GENERAL,,"  else "") +
                (if tb_mcshrec  then "!MCSHREC,"  else "") +
                (if tb_apmem    then "!APMEM,"    else "") +
                (if tb_acpay    then "!ACPAY,"    else "") +
                (if tb_ap-purch then "!AP-PURCH," else "") +
                (if tb_apckr    then "!APCHR,"    else "") +
                (if tb_arinv    then "!ARINV,"    else "") +
                (if tb_cdisb    then "!CDISB,"    else "") +
                (if tb_crmem    then "!CRMEM,"    else "") +
                (if tb_apvoidck then "!APVOIDCK," else "") +
                (if tb_oeinv    then "!OEINV,"    else "")*/
   "CASHR,GENERAL,MCSHREC,APMEM,ACPAY,AP-PURCH,APCHR,ARINV,CDISB,CRMEM,DBMEM,OEINV,APCKR,APVOIDCK,CRDIS".


if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}

/*IF td-show-parm THEN RUN show-param.*/

DISPLAY "" WITH FRAME r-top.

EMPTY TEMP-TABLE tt-trans.

v-distribute = NO.

  FOR EACH account NO-LOCK
      WHERE account.company EQ cocode
        AND account.actnum  GE lo_actnum
        AND account.actnum  LE hi_actnum:

    FOR EACH gltrans NO-LOCK
        WHERE gltrans.company EQ cocode
          AND gltrans.actnum  EQ account.actnum
          AND gltrans.tr-date GE lo_trandate
          AND gltrans.tr-date LE hi_trandate
          AND CAN-DO(gl_jrnl_list,gltrans.jrnl),

        EACH acctcost NO-LOCK
        WHERE acctcost.company EQ account.company
          AND acctcost.actnum  EQ account.actnum

        BREAK BY gltrans.actnum
             /* BY acctcost.costacct*/:

      IF FIRST-OF(gltrans.actnum) THEN v-tot-amt = 0.

      /* auto distribution to sub cost account */
      CREATE tt-trans.
      ASSIGN
       tt-trans.tt-recid = RECID(gltrans)
       tt-trans.type     = "GLTRANS"
       tt-trans.c-rate   = acctcost.c-rate
       tt-trans.tr-amt   = ROUND(gltrans.tr-amt * acctcost.c-rate / 100,2)
       tt-trans.costacct = acctcost.costacct
       tt-trans.jrnl     = gltrans.jrnl.
         
      v-tot-amt = v-tot-amt + tt-trans.tr-amt.

      IF LAST-OF(gltrans.actnum) THEN DO:
      /*  IF v-tot-amt NE gltrans.tr-amt THEN
          tt-trans.tr-amt = tt-trans.tr-amt + (gltrans.tr-amt - v-tot-amt).
      */
        CREATE tt-trans.
        ASSIGN
         tt-trans.tt-recid = RECID(gltrans)
         tt-trans.type     = "GLTRANS"
         tt-trans.c-rate   = 100
         tt-trans.tr-amt   = v-tot-amt * -1 /*gltrans.tr-amt * -1*/
         tt-trans.costacct = gltrans.actnum
         tt-trans.jrnl     = "Cost Distribution".           
      END.               
    END. /* gltrans */
    
    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ cocode
          AND glhist.actnum  EQ account.actnum
          AND glhist.tr-date GE lo_trandate
          AND glhist.tr-date LE hi_trandate
          AND CAN-DO(gl_jrnl_list,glhist.jrnl),

        EACH acctcost NO-LOCK
        WHERE acctcost.company EQ account.company
          AND acctcost.actnum  EQ account.actnum

        BREAK BY glhist.actnum
              BY acctcost.costacct:

      IF FIRST-OF(glhist.actnum) THEN v-tot-amt = 0.

      /* auto distribution to sub cost account */
      CREATE tt-trans.
      ASSIGN
       tt-trans.tt-recid = RECID(glhist)
       tt-trans.type     = "GLHIST"
       tt-trans.c-rate   = acctcost.c-rate
       tt-trans.tr-amt   = ROUND(glhist.tr-amt * acctcost.c-rate / 100,2)
       tt-trans.costacct = acctcost.costacct
       tt-trans.jrnl     = glhist.jrnl.
         
      v-tot-amt = v-tot-amt + tt-trans.tr-amt.

      IF LAST-OF(glhist.actnum) THEN DO:
       /* IF v-tot-amt NE glhist.tr-amt THEN
          tt-trans.tr-amt = tt-trans.tr-amt + (glhist.tr-amt - v-tot-amt).
       */
        CREATE tt-trans.
        ASSIGN
         tt-trans.tt-recid = RECID(glhist)
         tt-trans.type     = "GLHIST"
         tt-trans.c-rate   = 100
         tt-trans.tr-amt   = v-tot-amt * -1 /*glhist.tr-amt * -1*/
         tt-trans.costacct = glhist.actnum
         tt-trans.jrnl     = "Cost Distribution".           
      END.               
    END. /* glhist */
  END. /* account*/

/*======*/
  VIEW FRAME F-DET.
  DOWN 0 WITH FRAME F-DET.
  ASSIGN
    hdg_printed = FALSE
    t-amt = 0
    t-disc = 0
    t-msf = 0
    t-qty = 0
    ws_disc = 0
    ws_jrnl = ''
    ws_check-no = 0
    ws_order-no = 0
    .


  FOR EACH tt-trans,
      FIRST account WHERE account.company = cocode
                      AND account.actnum = tt-trans.costacct NO-LOCK
                      BREAK BY tt-trans.costacct:

    if line-counter >= (page-size - 2) then do:
        page.
        view frame f-det.
        down 0 with frame f-det.
    end.

    IF first-of(tt-trans.costacct) THEN DO:
         
         
         v-tot-amt = 0.  
    END.

    v-distribute = YES.

    IF NOT hdg_printed THEN
    DO:
      /*  PUT SKIP account.actnum ' - '
          account.dscr
          SKIP. */
        hdg_printed = TRUE.
    END.
            
    IF rd_detsum = "D" THEN do: /* detail*/
       DISPLAY account.actnum @ ws_jrnl 
                 account.dscr @ vend.NAME        
                 /*"Cost Distribute" @ vend.NAME   */            
                 tt-trans.tr-amt @ ap-invl.amt 
                 WITH FRAME f-det.
       DOWN WITH FRAME f-det.
    END.
      v-tramt = tt-trans.tr-amt.
      v-tot-amt = v-tot-amt + v-tramt.

      IF LAST-OF(tt-trans.costacct) THEN DO:
         IF rd_detsum = "D" THEN do: /* detail*/
            UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
            DOWN WITH FRAME f-det.
         END.
         DISPLAY account.actnum @ ws_jrnl
                 "Account Total" WHEN rd_detsum = "D" @ ws_jrnl
                 account.dscr @ vend.NAME        
                 /*"Cost Distribute" @ vend.NAME   */            
                 v-tot-amt @ ap-invl.amt 
                 WITH FRAME f-det.
         DOWN WITH FRAME f-det.

         IF NOT last(tt-trans.costacct) AND rd_detsum = "D" THEN do: /* detail*/
            UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
            DOWN WITH FRAME f-det.
         END.
        ASSIGN t-amt = t-amt + v-tot-amt . /*gltrans.tr-amt * (-1).*/
      END.
      
  END.

    UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
    DOWN WITH FRAME f-det.
    DISP
      /* "* ACCOUNT TOTAL *" @ vend.name */
      t-disc @ ws_disc
      t-amt  @ ap-invl.amt
      t-qty  @ ap-invl.qty WITH FRAME f-det.
      /*t-msf  @ ap-invl.amt-msf.*/
     /* down 1. */

  

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
end procedure.



PROCEDURE cost-distribute-det :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 SESSION:SET-WAIT-STATE("general").
  DEF BUFFER cost-trans FOR gltrans.
  DEF BUFFER cost-hist  FOR glhist.
  DEF VAR v-act-amt AS DEC NO-UNDO.

  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
      EACH gltrans WHERE RECID(gltrans) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                              AND acctcost.actnum = gltrans.actnum */
      BREAK BY gltrans.actnum:

      CREATE cost-trans.
      BUFFER-COPY gltrans TO cost-trans.
      ASSIGN cost-trans.tr-amt = tt-trans.tr-amt
             cost-trans.actnum = tt-trans.costacct
             cost-trans.tr-date = tran-date
             cost-trans.jrnl = "AUTODIST"
             cost-trans.period  = tran-period
             cost-trans.trnum   = v-trnum
             cost-trans.tr-dscr = "Auto Distribution"
             .
             /*gltrans.jrnl = "AUTODIST"*/ .
   
  END.


  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
      EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                              AND acctcost.actnum = glhist.actnum */
           BREAK BY glhist.actnum:

       CREATE cost-hist.
       BUFFER-COPY glhist TO cost-hist.
       ASSIGN cost-hist.tr-amt = tt-trans.tr-amt 
                cost-hist.actnum = tt-trans.costacct
                cost-hist.tr-date = tran-date
                cost-hist.jrnl = "AUTODIST"
                cost-hist.period  = tran-period
                cost-hist.tr-num   = v-trnum
                cost-hist.tr-dscr = "Auto Distribution"
                .
      /*
      IF LAST-OF(glhist.actnum) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                cost-hist.jrnl = "AUTODIST".
      END.
      */
      /*glhist.jrnl = "AUTODIST" */.
  END.
SESSION:SET-WAIT-STATE("").
cError =  "Auto Distribution is completed." .

END PROCEDURE.


PROCEDURE cost-distribute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE("general").
  DEF BUFFER cost-trans FOR gltrans.
  DEF BUFFER cost-hist  FOR glhist.
  DEF VAR v-act-amt AS DEC NO-UNDO.

  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
      EACH gltrans WHERE RECID(gltrans) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                              AND acctcost.actnum = gltrans.actnum */
      BREAK BY /*gltrans.actnum*/ tt-trans.costacct:

      IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

      v-act-amt = v-act-amt + tt-trans.tr-amt .
      IF LAST-OF(tt-trans.costacct) THEN DO:
         CREATE cost-trans.
         BUFFER-COPY gltrans TO cost-trans.
         ASSIGN cost-trans.tr-amt = v-act-amt /*tt-trans.tr-amt*/
             cost-trans.actnum = tt-trans.costacct
             cost-trans.tr-date = tran-date
             cost-trans.jrnl = "AUTODIST"
             cost-trans.period  = tran-period
             cost-trans.trnum   = v-trnum
             cost-trans.tr-dscr = "Auto Distribution"
             .
             /*gltrans.jrnl = "AUTODIST"*/ .
      END.
   
  END.


  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
      EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                              AND acctcost.actnum = glhist.actnum */
           BREAK BY tt-trans.costacct:

      IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

      v-act-amt = v-act-amt + tt-trans.tr-amt .

      IF LAST-OF( tt-trans.costacct) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = v-act-amt /*tt-trans.tr-amt */
                cost-hist.actnum = tt-trans.costacct
                cost-hist.tr-date = tran-date
                cost-hist.jrnl = "AUTODIST"
                cost-hist.period  = tran-period
                cost-hist.tr-num   = v-trnum
                cost-hist.tr-dscr = "Auto Distribution"
                .
      END.
      /*
      IF LAST-OF(glhist.actnum) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                cost-hist.jrnl = "AUTODIST".
      END.
      */
      /*glhist.jrnl = "AUTODIST" */.
  END.
SESSION:SET-WAIT-STATE("").
cError = "Auto Distribution is completed." .

END PROCEDURE.
