

/*------------------------------------------------------------------------
    File        : fnstmt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttFinancialStatements NO-UNDO
        FIELD fnstat AS CHAR
        FIELD lvrept AS CHAR
        FIELD vValue AS CHAR
        FIELD gettt  AS CHAR
        FIELD ext    AS CHAR.

DEFINE DATASET dsFinancialStatements FOR ttFinancialStatements.
    DEFINE INPUT PARAMETER  prmUser        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmGetTt       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdscr        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmslctrp      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpred        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmprecls      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmzeroln      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsuppze      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmact#        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdecimal     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmmulcmp      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmlist        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmactlvl      AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbesbact     AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendsbact    AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmext         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmout         AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError         AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFinancialStatements.

     IF prmUser         = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction       = ? THEN ASSIGN     prmAction      = "". 
     IF prmdscr         = ? THEN ASSIGN     prmdscr        = "". 
     IF prmtrnsdt       = ? THEN ASSIGN     prmtrnsdt      = "". 
     IF prmslctrp       = ? THEN ASSIGN     prmslctrp      = "".  
     IF prmpred         = ? THEN ASSIGN     prmpred        = 0.  
     IF prmprecls       = ? THEN ASSIGN     prmprecls      = "". 
     IF prmzeroln       = ? THEN ASSIGN     prmzeroln      = "". 
     IF prmsuppze       = ? THEN ASSIGN     prmsuppze      = "". 
     IF prmact#         = ? THEN ASSIGN     prmact#        = "". 
     IF prmdecimal      = ? THEN ASSIGN     prmdecimal     = "". 
     IF prmmulcmp       = ? THEN ASSIGN     prmmulcmp      = "". 
     IF prmlist         = ? THEN ASSIGN     prmlist        = "". 
     IF prmactlvl       = ? THEN ASSIGN     prmactlvl      = 0. 
     IF prmbesbact      = ? THEN ASSIGN     prmbesbact     = 0. 
     IF prmendsbact     = ? THEN ASSIGN     prmendsbact    = 0. 
     IF prmext          = ? THEN ASSIGN     prmext         = "".  
     IF prmout          = ? THEN ASSIGN     prmout         = "". 

DEFINE VARIABLE select-rpt      AS CHARACTER NO-UNDO.    
DEFINE VARIABLE begin_sub-acct  AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE end_sub-acct    AS INTEGER FORMAT "->>>>>>>>9" INITIAL 999999999 NO-UNDO.
DEFINE VARIABLE lv-company-list AS CHARACTER FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE sub-acct-lvl    AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE tran-date       AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period     AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-dscr          AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE tb_acct#        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_mul-comp     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_paid         AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_pre          AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_round        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_supp         AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_runExcel     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_excel        AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE rpt-codes       AS CHARACTER FORMAT "X(256)":U NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.


{gl/gl-fs.i NEW}

format
  v-hdr[1] at 1 format "x(200)" skip
  v-hdr[2] at 1 format "x(200)" skip
  v-hdr[3] at 1 format "x(200)" skip
  v-hdr[4] at 1 format "x(200)" skip
  v-hdr[5] at 1 format "x(200)" skip(2)
  r-top1   at 1 format "x(200)"
  r-top2   at 1 format "x(200)"
  with frame rpt-top page-top no-box no-labels width 200 STREAM-IO.

 DEF BUFFER bf-rpt FOR gl-rpt.
 DEF VAR is-xprint-form AS LOG NO-UNDO.
 DEF VAR ls-fax-file AS cha NO-UNDO.
 DEF NEW SHARED STREAM excel.

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

TRAN-date = TODAY.

IF prmGetTt = "gettt" THEN DO:

      FOR EACH gl-rpt
          WHERE gl-rpt.company EQ cocode
          AND gl-rpt.line    EQ 0
          NO-LOCK:

          CREATE ttFinancialStatements.
          ASSIGN
              ttFinancialStatements.vValue = STRING(gl-rpt.rpt,"x(6)") 
              ttFinancialStatements.lvrept  =  gl-rpt.dscr   .
          
        IF TRIM(v-dscr) EQ "" THEN DO:
            FIND FIRST bf-rpt
                WHERE bf-rpt.company    EQ cocode
                AND bf-rpt.rpt        EQ gl-rpt.rpt
                AND bf-rpt.line       EQ 7
                AND TRIM(bf-rpt.dscr) NE ""
                NO-LOCK NO-ERROR.
            IF AVAIL bf-rpt THEN v-dscr = bf-rpt.dscr.
         END.
      END. /*for each*/
      
      RETURN.
      /*select-rpt = ttFinancialStatements.vValue.*/
  END.


ASSIGN
       v-dscr           =   prmdscr                    
       tran-date        =   date(prmtrnsdt)
       select-rpt       =   prmslctrp               
       tran-period      =   prmpred              
       tb_pre           =   IF prmprecls = "Yes" THEN TRUE ELSE FALSE
       tb_paid          =   IF prmzeroln = "Yes" THEN TRUE ELSE FALSE                 
       tb_supp          =   IF prmsuppze = "Yes" THEN TRUE ELSE FALSE               
       tb_acct#         =   IF prmact# = "Yes" THEN TRUE ELSE FALSE                     
       tb_round         =   IF prmdecimal = "Yes" THEN TRUE ELSE FALSE                 
       tb_mul-comp      =   IF prmmulcmp = "Yes" THEN TRUE ELSE FALSE              
       lv-company-list  =   prmlist                      
       sub-acct-lvl     =   prmactlvl                     
       begin_sub-acct   =   prmbesbact                  
       end_sub-acct     =   prmendsbact  . 

    FOR EACH company:
        company-list = company-list + company.company + ",".
    END.
    IF company-list NE "" THEN
        SUBSTR(company-list,LENGTH(TRIM(company-list)),1) = "".

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
    IF AVAIL company THEN DO:
        aclevel = company.acc-level.
        IF company-list EQ "" THEN company-list = company.company.
    END.  
    ELSE aclevel = 1.

         ASSIGN
             sub-acct-lvl    = aclevel
             lv-company-list = company-list.

         FIND FIRST period                   
             WHERE period.company EQ cocode
             AND period.pst     LE DATE(prmtrnsdt)
             AND period.pend    GE DATE(prmtrnsdt) NO-LOCK NO-ERROR.
         IF AVAIL period THEN prmpred = (period.pnum).
         ELSE DO:
             cError = "No Defined Period Exists for".
             RETURN.
         END.


  

  IF prmAction = "fnstat" THEN DO:
      
      
                                                                                                                              
   ASSIGN                                                                                                                     
       v-dscr           =   prmdscr                    
       tran-date        =   date(prmtrnsdt)
       select-rpt       =   prmslctrp               
       tran-period      =   prmpred              
       tb_pre           =   IF prmprecls = "True" THEN TRUE ELSE FALSE
       tb_paid          =   IF prmzeroln = "True" THEN TRUE ELSE FALSE                 
       tb_supp          =   IF prmsuppze = "True" THEN TRUE ELSE FALSE               
       tb_acct#         =   IF prmact# = "True" THEN TRUE ELSE FALSE                     
       tb_round         =   IF prmdecimal = "True" THEN TRUE ELSE FALSE                 
       tb_mul-comp      =   IF prmmulcmp = "True" THEN TRUE ELSE FALSE              
       lv-company-list  =   prmlist                      
       sub-acct-lvl     =   prmactlvl                     
       begin_sub-acct   =   prmbesbact                  
       end_sub-acct     =   prmendsbact  .          


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "FinanceStat" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "FinanceStat" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .



        FIND FIRST period                   
             WHERE period.company EQ cocode
             AND period.pst     LE DATE(prmtrnsdt)
             AND period.pend    GE DATE(prmtrnsdt) NO-LOCK NO-ERROR.
         IF AVAIL period THEN prmpred = (period.pnum).
         ELSE DO:
             cError = "No Defined Period Exists for".
             RETURN.
         END.

         IF INT(prmactlvl) GT aclevel THEN DO:
             cError = "Sub Account Level may not be greater than Account Level" .
             RETURN. 
         END.

         ASSIGN 
           udate = tran-date
           uperiod = tran-period
           pre-close = tb_pre
           skip_zero = tb_paid
           supp_zero = tb_supp
           consolidate = tb_mul-comp
           company-list = lv-company-list
           subac-lvl = sub-acct-lvl
           fsubac = begin_sub-acct
           tsubac = END_sub-acct
           ll-acct# = tb_acct#.

        
         run run-report.



         CREATE ttFinancialStatements.
         IF prmOut = "Yes" THEN
             ASSIGN ttFinancialStatements.fnstat = vExcalFile .
         ELSE
             ASSIGN ttFinancialStatements.fnstat = vTextFile .

  END.
/*****************************************************************************************/
  
  PROCEDURE run-report :
DEF VAR lv-company-save AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rcode AS CHAR FORMAT "x(47)".
DEF VAR lv-pct-hdr AS CHAR INIT "  % Sales" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.


 /* DO li = 1 TO LENGTH(select-rpt):
    lv-rcode = lv-rcode + TRIM(SUBSTR(select-rpt:ENTRY(li),1,6)) + ",".
  END.*/
  
 lv-rcode  = prmslctrp . 

  IF LENGTH(lv-rcode) > 0 AND
    SUBSTR(lv-rcode,length(TRIM(lv-rcode)),1) EQ "," THEN
    SUBSTR(lv-rcode,length(TRIM(lv-rcode)),1) = "".
    
  rpt-codes = lv-rcode.
  
  DO li = 1 TO length(rpt-codes):
    IF SUBSTR(rpt-codes,li,1) EQ "," THEN SUBSTR(rpt-codes,li,1) = " ".
  END.

  lv-company-save = company-list.

  DO li = 1 TO LENGTH(lv-company-list):
    IF SUBSTR(lv-company-list,li,1) EQ "," THEN
      SUBSTR(lv-company-list,li,1) = " ".
  END.

  ASSIGN
   prmlist = lv-company-list
   /*rpt-codes:SCREEN-VALUE = rpt-codes*/ .
   
 
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}



IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).


  ASSIGN
   prmlist = lv-company-save
   .
 



FOR EACH gl-rpt
    WHERE gl-rpt.company EQ cocode
      AND LOOKUP(gl-rpt.rpt,lv-rcode) GT 0
      AND gl-rpt.line    EQ 0
    NO-LOCK
    BY gl-rpt.rpt:

  DO TRANSACTION:
      
    FIND FIRST bf-rpt
        WHERE bf-rpt.company EQ cocode
          AND bf-rpt.rpt     EQ gl-rpt.rpt
          AND bf-rpt.line    EQ 7
        NO-ERROR.               
    IF AVAIL bf-rpt THEN
    DO:
       bf-rpt.dscr = v-dscr.
       FIND CURRENT bf-rpt NO-LOCK.
    END.
  
  END.

  ASSIGN
   v-d-wid = 0
   r-top1  = ""
   r-top2  = ""
   r-top3  = ""
   r-top4  = ""
   v-hdr   = "".

  fil_id = recid(gl-rpt).
  run gl/gl-rptg.p (input fil_id, input no).

  IF NOT all-per AND tb_round THEN DO:
    ASSIGN
     tot-format  = "->>>,>>>,>>9"
     pct-format  = "->>,>>>,>>9%"
     pct-formats = "->>>9%"
     sul-format  = " -----------"
     sul-formats = " -----"
     dul-format  = " ==========="
     dul-formats = " ====="
     lv-pct-hdr  = " % Sls".

    DO i = 1 TO v-no-col:
      v-ch[i] = SUBSTR(TRIM(v-ch[i]),1,11).
    END.
  END.

  if consolidate and index(company-list,",") gt 0 then
    v-hdr[5] = v-hdr[5] + (if v-hdr[5] eq "" then "" else " - ") +
               "Companies: " + trim(company-list).

  /* form headers */
  do i = 1 to 5:
    assign tot2[i] = length(v-hdr[i])
      tot2[i] = int(v-col-used - int(tot2[i])) / 2
      v-hdr[i] = fill(" ", int (tot2[i])) + v-hdr[i].
  end.
  
  r-top1 = fill(" ",v-d-wid).
  r-top2 = r-top1.
  do i = 1 to v-no-col:
    assign r-top3 = FILL(" ",((IF all-per THEN 10 ELSE IF tb_round THEN 11 ELSE 14) - LENGTH(v-ch[i]))) +
                    v-ch[i] +
                    IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr ELSE "")
      r-top1 = r-top1 + " " + r-top3
      r-top4 = r-top4 + dul-format + (IF v-per[i] THEN dul-formats ELSE "").
  end.
  r-top2 = r-top2 + r-top4.

  DISPLAY v-hdr r-top1 r-top2 WITH FRAME rpt-top.

  v-rpt = gl-rpt.rpt.

  IF tb_excel THEN DO:
    ASSIGN excelheader = ",".

    DO i = 1 TO v-no-col:
      excelheader = excelheader + v-ch[i] + ","
                  + (IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr + "," ELSE "")).
    END.

    excelheader = RIGHT-TRIM(excelheader,",").

    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  RUN gl/gl-fs.p(INPUT tb_excel).

  HIDE FRAME rpt-top.
  PUT SKIP(1).
  PAGE.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  
END.


END PROCEDURE.
