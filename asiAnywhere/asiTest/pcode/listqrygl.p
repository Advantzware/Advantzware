

/*------------------------------------------------------------------------
    File        : listqrygl.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttListQueryGeneralLedgerHi NO-UNDO
        FIELD actnum    AS CHAR
        FIELD actdscr   AS CHAR
        FIELD trdate    AS CHAR
        FIELD jrnl      AS CHAR
        FIELD trdscr    AS CHAR
        FIELD tramt     AS DEC
        FIELD trfrm     AS CHAR
        FIELD reckey    AS CHAR
        FIELD extra     AS CHAR.

DEFINE DATASET dsListQueryGeneralLedgerHi FOR ttListQueryGeneralLedgerHi .

DEFINE INPUT PARAMETER prmUser         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmbegact       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmglyear       AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmperiodfr     AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmperiodto     AS INT          NO-UNDO.
DEFINE INPUT PARAMETER prmopnbal       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmclsbal       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmextra        AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey       AS CHAR         NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsListQueryGeneralLedgerHi.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.


IF prmUser         = ? THEN ASSIGN prmUser       = "".
IF prmAction       = ? THEN ASSIGN prmAction     = "Search".
IF prmbegact       = ? THEN ASSIGN prmbegact     = "".
IF prmglyear       = ? THEN ASSIGN prmglyear     = 0.
IF prmperiodfr     = ? THEN ASSIGN prmperiodfr   = 0.
IF prmperiodto     = ? THEN ASSIGN prmperiodto   = 12.
IF prmopnbal       = ? THEN ASSIGN prmopnbal     = 0.
IF prmclsbal       = ? THEN ASSIGN prmclsbal     = 0.
IF prmextra        = ? THEN ASSIGN prmextra      = "".
IF prmRecKey       = ? THEN ASSIGN prmRecKey     = "".

DEF var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR v-max-dscr-length AS INT NO-UNDO.               
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS cha NO-UNDO.
DEF NEW SHARED VAR g_loc AS cha NO-UNDO.
DEF NEW SHARED VAR g_period AS INT NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

DEFINE VARIABLE begin_acct      AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE lv-actname      AS CHARACTER FORMAT "X(256)":U NO-UNDO.
DEFINE VARIABLE lv-close-bal    AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-open-bal     AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-period-fr    AS INTEGER FORMAT ">9":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-period-to    AS INTEGER FORMAT ">9":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lv-year         AS INTEGER FORMAT ">>>9":U INITIAL 0 NO-UNDO.
/*{sys/inc/VAR.i NEW SHARED}*/

DEF TEMP-TABLE tt-glinq NO-UNDO
    FIELD tr-date LIKE gltrans.tr-date LABEL "Date"
    FIELD jrnl LIKE gltrans.jrnl       LABEL "Ref#"
    FIELD tr-dscr AS CHAR FORMAT "X(60)" LABEL "Description"
    FIELD tr-amt LIKE gltrans.tr-amt   LABEL "Amount"
    FIELD tr-num LIKE gltrans.trnum
    FIELD tr-from AS cha      FORM "x(30)"    LABEL "Inquiry From" 
    FIELD actnum LIKE gltrans.actnum LABEL "Account#"
    INDEX tr-date IS PRIMARY tr-date.


DEF VAR lv-first AS LOG NO-UNDO.
DEF VAR lv-sort-by AS cha NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-by-lab AS cha NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */
DEF VAR totlbal AS DEC NO-UNDO.
DEF VAR v-acc-length AS INT NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


assign
 cocode = prmComp 
 v-today   = TODAY 
  .


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .



FIND FIRST company WHERE
     company.company EQ cocode
     NO-LOCK NO-ERROR.

IF AVAIL company THEN
DO:
   DO v-count = 1 TO 5:
      v-acc-length = v-acc-length + company.acc-dig[v-count].
      IF company.acc-dig[v-count] NE 0 THEN
         v-acc-length = v-acc-length + 1.
   END.

/*   IF v-acc-length GT 1 THEN
      tt-glinq.actnum = v-acc-length + 3. */
END.

find first period
      where period.company eq cocode
        and period.pst     le today
        and period.pend    ge today
      no-lock no-error.

/*  tt-glinq.tr-dscr = v-max-dscr-length + 20.*/

IF prmAction = "showall" THEN DO:
ASSIGN prmbegact = ""
             prmglyear = 0
             prmperiodfr  = 0
             prmperiodto  = 12
            prmAction = "Search".

END.

RUN build-inquiry.



 IF prmAction = "Search" THEN DO:

  
    FOR EACH tt-glinq WHERE 
           /*(tt-glinq.actnum EQ prmbegact)*/  NO-LOCK:
     
            create ttListQueryGeneralLedgerHi.
            assign
                ttListQueryGeneralLedgerHi.actnum       = tt-glinq.actnum
                ttListQueryGeneralLedgerHi.trdate       = string(tt-glinq.tr-date)
                ttListQueryGeneralLedgerHi.jrnl         = tt-glinq.jrnl
                ttListQueryGeneralLedgerHi.trdscr       = tt-glinq.tr-dscr
                ttListQueryGeneralLedgerHi.tramt        = tt-glinq.tr-amt
                ttListQueryGeneralLedgerHi.trfrm        = tt-glinq.tr-from 
                ttListQueryGeneralLedgerHi.reckey      = string(tt-glinq.tr-num) 
                ttListQueryGeneralLedgerHi.extra       = STRING(totlbal)  .

            FIND account
                WHERE account.company EQ g_company
                AND account.actnum  EQ prmbegact
                NO-LOCK NO-ERROR.
            IF AVAIL account THEN
                ASSIGN
                ttListQueryGeneralLedgerHi.actdscr = account.dscr.
           
            

    END.   /* end of for loop*/
            
            
 END. /* end search */

 IF prmAction = "Select" THEN DO:
     
    FOR EACH tt-glinq WHERE 
           (tt-glinq.actnum EQ prmbegact)  NO-LOCK:

            create ttListQueryGeneralLedgerHi.
            assign
                ttListQueryGeneralLedgerHi.actnum       = tt-glinq.actnum
                ttListQueryGeneralLedgerHi.trdate       = string(tt-glinq.tr-date)
                ttListQueryGeneralLedgerHi.jrnl         = tt-glinq.jrnl
                ttListQueryGeneralLedgerHi.trdscr       = tt-glinq.tr-dscr
                ttListQueryGeneralLedgerHi.tramt        = tt-glinq.tr-amt
                ttListQueryGeneralLedgerHi.trfrm        = tt-glinq.tr-from 
                ttListQueryGeneralLedgerHi.reckey       = string(tt-glinq.tr-num) 
                ttListQueryGeneralLedgerHi.extra       = string(totlbal)                 .

            FIND account
                WHERE account.company EQ g_company
                AND account.actnum  EQ prmbegact
                NO-LOCK NO-ERROR.
            IF AVAIL account THEN
                ASSIGN
                ttListQueryGeneralLedgerHi.actdscr = account.dscr.
    END.
     
 END.

 IF prmAction = "PrintReport" THEN DO:

     ASSIGN
         begin_acct    =    prmbegact 
         lv-close-bal  =    prmclsbal
         lv-open-bal   =    prmopnbal
         lv-period-fr  =    prmperiodfr  
         lv-period-to  =    prmperiodto  
         lv-year       =    prmglyear .
      
      

      DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "APCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "APCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        
        run run-report. 

        
        CREATE ttListQueryGeneralLedgerHi.
        ASSIGN ttListQueryGeneralLedgerHi.extra = vTextFile .
 END.

PROCEDURE build-inquiry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR tmp-start AS DATE NO-UNDO.
  DEF VAR tmp-end AS DATE NO-UNDO.
  

  ASSIGN totlbal = 0.

  EMPTY TEMP-TABLE tt-glinq.
  ASSIGN
     v-max-dscr-length = 0
     uperiod = prmperiodfr.

  FIND first account WHERE account.company = g_company
                       AND account.actnum = prmbegact NO-LOCK NO-ERROR.

  FIND FIRST period WHERE period.company = g_company
                      AND period.yr = prmglyear
                      AND period.pnum = prmperiodfr NO-LOCK NO-ERROR.
  tmp-start = IF AVAIL period THEN period.pst ELSE 01/01/01 /*12/31/9999*/ .
  FIND FIRST period WHERE period.company = g_company
                      AND period.yr = prmglyear
                      AND period.pnum = prmperiodto NO-LOCK NO-ERROR.
  tmp-end = IF AVAIL period THEN period.pend ELSE 12/31/9999 /*01/01/0001*/ .

  RUN gl/gl-opend.p (ROWID(account), tmp-start, OUTPUT prmopnbal).

  prmclsbal = prmopnbal.

 
  for each glhist NO-LOCK
      where glhist.company eq cocode
        and (glhist.actnum  eq prmbegact OR prmbegact = "")
        and glhist.period  ge prmperiodfr
        and glhist.period  le prmperiodto
        and glhist.tr-date ge tmp-start
        and glhist.tr-date le tmp-end
      by glhist.tr-date :
      CREATE tt-glinq.
      ASSIGN tt-glinq.tr-date = glhist.tr-date
             tt-glinq.jrnl = glhist.jrnl
             tt-glinq.tr-dscr = glhist.tr-dscr
             tt-glinq.tr-amt = glhist.tr-amt
             tt-glinq.tr-from = "GL History "
             tt-glinq.actnum = glhist.actnum
             tt-glinq.tr-num = glhist.tr-num
             totlbal = totlbal + glhist.tr-amt.
 MESSAGE "op cl1 " prmclsbal prmopnbal .
      IF LENGTH(glhist.tr-dscr) GT v-max-dscr-length THEN
         v-max-dscr-length = LENGTH(glhist.tr-dscr).
  end.

  for each gltrans  NO-LOCK
      where gltrans.company eq cocode
        and (gltrans.actnum  eq prmbegact OR prmbegact = "")
        and gltrans.period  ge prmperiodfr
        and gltrans.period  le prmperiodto
        and gltrans.tr-date ge tmp-start
        and gltrans.tr-date le tmp-end
      by gltrans.tr-date:
      CREATE tt-glinq.
      ASSIGN tt-glinq.tr-date = gltran.tr-date
             tt-glinq.jrnl = gltran.jrnl
             tt-glinq.tr-dscr = gltran.tr-dscr
             tt-glinq.tr-amt = gltran.tr-amt
             tt-glinq.tr-from = "GL Transaction " + string(gltran.trnum)
             tt-glinq.actnum = gltrans.actnum
             tt-glinq.tr-num = gltrans.trnum
             totlbal = totlbal + gltrans.tr-amt.
MESSAGE "op cl2 " prmclsbal prmopnbal .
      IF LENGTH(gltran.tr-dscr) GT v-max-dscr-length THEN
         v-max-dscr-length = LENGTH(gltran.tr-dscr).
  end.
 
END PROCEDURE.


/*****************************************************************/
PROCEDURE run-report :

    DEF VAR lv-acct   LIKE account.actnum                   NO-UNDO.
    DEF VAR lv-dscr   LIKE account.dscr                     NO-UNDO.        
    DEF VAR lv-yr     LIKE period.yr                        NO-UNDO.
    DEF VAR lv-per-fr LIKE period.pnum                      NO-UNDO.
    DEF VAR lv-per-to LIKE lv-per-fr                        NO-UNDO.
    DEF VAR lv-open   AS   DEC FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
    DEF VAR lv-close  LIKE lv-open                          NO-UNDO.

    FORM SKIP(1)
         lv-acct    COLON 30 LABEL "GL Account#"
         lv-dscr    AT 32    NO-LABEL
         lv-yr      COLON 30 LABEL "Year"
         lv-per-fr  COLON 30 LABEL "Beginning Period"
         lv-per-to  COLON 30 LABEL "Ending Period"
         SKIP(1)
         lv-open    COLON 30 LABEL "Opening Balance"
         SKIP(1)

        WITH STREAM-IO WIDTH 80 FRAME gl-inq SIDE-LABELS NO-UNDERLINE PAGE-TOP
             TITLE "  A C C O U N T   A C T I V I T Y  ".

    format space(4)
           tt-glinq.tr-date column-label "Date"  space(2)
           tt-glinq.jrnl    column-label "Ref#"  space(2)
           tt-glinq.tr-dscr label "Description" format "x(33)" space(2)
           tt-glinq.tr-amt  label "Amount" space(3)
          with no-box no-attr-space frame g2lines row 8 12 down stream-io WIDTH 80 centered.


      FIND FIRST tt-glinq NO-ERROR.

      IF AVAIL tt-glinq THEN DO WITH FRAME gl-inq:


        /*{sys/inc/print1.i}*/
           if tmp-dir = "" then tmp-dir = v-webrootpath .
           assign list-name = tmp-dir + vTextFile
           init-dir = tmp-dir.

          {sys/inc/outprint.i 56}

        VIEW FRAME r-top.

        DISPLAY begin_acct   @ lv-acct
                lv-actname   @ lv-dscr
                lv-year      @ lv-yr
                lv-period-fr @ lv-per-fr
                lv-period-to @ lv-per-to
                lv-open-bal  @ lv-open.

        FOR EACH tt-glinq:
          DISPLAY tt-glinq.tr-date
                  tt-glinq.jrnl
                  tt-glinq.tr-dscr
                  tt-glinq.tr-amt
              WITH FRAME g2lines.
          DOWN WITH FRAME g2lines.
        END.

        DISPLAY SKIP(1)
                lv-close-bal @ lv-close COLON 30 LABEL "Closing Balance"
                SKIP(2)
            WITH STREAM-IO WIDTH 80 FRAME gl-inq1 SIDE-LABELS NO-UNDERLINE.

        OUTPUT CLOSE.
      END.
END PROCEDURE.
