


/*------------------------------------------------------------------------
    File        : PrintQuote.p
    Purpose     :  Print Quote

    Syntax      :

    Description : Return a Dataset of Request For Quote

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttPrintQuote1 NO-UNDO
FIELD QteFile AS CHAR.
DEFINE DATASET dsQuotePrint1 FOR ttPrintQuote1 .
 {methods/prgsecur.i} 
{custom/xprint.i}
{est/printquo.i NEW}
 

DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote      AS INTEGER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuotePrint1.
DEFINE OUTPUT PARAMETER vFile  AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-quote FIELD row-id AS ROWID
                        FIELD tt-seq AS INT INIT 999999999
                        INDEX row-id row-id.
DEF VAR vQuote       AS CHAR NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR v-tmp-lines-per-page AS INT NO-UNDO.
DEF VAR vlSkipRec AS LOG NO-UNDO.
DEFINE VARIABLE tb_note AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-quo-list AS CHARACTER  NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)"  NO-UNDO.
DEFINE VARIABLE begin_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0   NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" INITIAL "zz"  NO-UNDO.
DEFINE VARIABLE end_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5 NO-UNDO. 
DEFINE VARIABLE tb_posted AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VAR g_company  AS CHAR NO-UNDO.

DEFINE NEW SHARED VAR k  AS INT NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.


/*DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
  */

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" NO-UNDO.
DEFINE VARIABLE tb_boardDescription AS CHARACTER INITIAL "Est" NO-UNDO.
def var v-headers       as log  no-undo.
def var v-print-coc     as log  no-undo.
def var v-check-qty     as log  no-undo.
DEF VAR lv-quo-no LIKE quotehd.q-no NO-UNDO.
DEF VAR lv-loc LIKE quotehd.loc INIT "" NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR vcMailMode      AS CHAR NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
def var list-name         as char FORMAT "X(100)" no-undo.
def var init-dir          as char no-undo.
DEF BUFFER b1-cust FOR cust.
DEF BUFFER b-quotehd FOR quotehd.
    def NEW shared buffer xquo for quotehd.
DEFINE NEW SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHAR NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

IF prmAction     = ? THEN ASSIGN prmAction     = "".
    
/* ********************  Preprocessor Definitions  ******************** */
 MESSAGE "prmAction" prmAction  LvOutputSelection.
IF prmAction = "PrintQuote" THEN do:
    IF rd-dest = 5  THEN
        ASSIGN LvOutputSelection = "Email"
               lv-pdf-file = "C:\Inetpub\wwwroot\pdfs\" 
               is-xprint-form   = TRUE.
    FIND FIRST quotehd  WHERE quotehd.q-no = prmQuote NO-LOCK.
    IF AVAILABLE quotehd THEN 
        ASSIGN 
        begin_quo# =  quotehd.q-no
        begin_cust =  quotehd.cust-no
        end_quo#   =  quotehd.q-no
        end_cust   =  quotehd.cust-no.
    MESSAGE "hello" begin_quo# begin_cust end_quo# end_cust.
    FIND FIRST est WHERE est.company EQ quotehd.company
                    AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.
    EMPTY TEMP-TABLE tt-filelist.
    ASSIGN cocode = quotehd.company
           locode = quotehd.loc
           g_company = quotehd.company.
    MESSAGE "hello1" cocode locode g_company.
    FIND first sys-ctrl where sys-ctrl.company eq cocode
           and sys-ctrl.name    eq "QUOPRINT" no-lock no-error.
    if not avail sys-ctrl then
        do transaction:
        create sys-ctrl.
        assign
            sys-ctrl.company = cocode
            sys-ctrl.name    = "QUOPRINT"
            sys-ctrl.descrip = "Print Quote Headers on Quote Form?".
MESSAGE "hello2" sys-ctrl.company  sys-ctrl.NAME sys-ctrl.descrip.
    /*MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.*/
        end. /*do transaction:*/
        ASSIGN
            v-print-fmt = sys-ctrl.char-fld
            CallingParameter = sys-ctrl.char-fld
            v-log       = sys-ctrl.log-fld.
        MESSAGE "hello3"   v-log CallingParameter v-print-fmt.
        IF INDEX("Pacific,Xprint,SouthPak,ABox,Midwest,century,Concepts,oracle,Harwell,PremierX,Elite,Unipak,Ottpkg,Frankstn,Mirpkg,APC,FibreX,PPI",v-print-fmt) > 0
            THEN is-xprint-form = YES.     
        ELSE is-xprint-form = NO.
             IF v-print-fmt EQ "HOP" THEN
                 ASSIGN
                 v-program      = "cec/quote/quohop.p"
                 lines-per-page = 37.
        ELSE
            IF v-print-fmt EQ "LandScap" THEN
                ASSIGN
                v-program      = "ce/quote/landquo.p"
                lines-per-page = 56.
        
        ELSE
            IF v-print-fmt EQ "ContSrvc" OR
                v-print-fmt EQ "Triad"    THEN
                ASSIGN
                v-program      = "cec/quote/quocsc.p"
                lines-per-page = 56.
            
        ELSE
            IF v-print-fmt EQ "Rudd" THEN
                ASSIGN
                v-program      = "cec/quote/quorudd.p"
                lines-per-page = 66.
        ELSE
            IF v-print-fmt EQ "General" THEN
                ASSIGN
                v-program      = "cec/quote/quogener.p"
                lines-per-page = 56.
        ELSE
            IF v-print-fmt EQ "10 Pitch" /*AND AVAIL est AND est.est-type GT 4*/ THEN
                ASSIGN
                v-program      = "cec/quote/prtquo10.p"
                lines-per-page = 56.
        ELSE
            IF v-print-fmt EQ "Brick" THEN
                ASSIGN
                v-program      = "cec/quote/quobrick.p"
                lines-per-page = 38.
        ELSE
            IF v-print-fmt EQ "Fibre" THEN 
                ASSIGN
                v-program      = "cec/quote/quofibre.p"
                lines-per-page = 52.
        ELSE
            IF v-print-fmt EQ "Harwell" THEN
                ASSIGN
                v-program      = "cec/quote/quohawl.p"
                lines-per-page = 56.
        ELSE
            IF v-print-fmt EQ "Pacific" THEN
                ASSIGN
                v-program      = "cec/quote/quopacif.p"
                lines-per-page = 66.  
        ELSE
            
            IF v-print-fmt EQ "Abox" THEN
                ASSIGN
                v-program      = "cec/quote/quoabox.p"
                lines-per-page = 66.  
        ELSE IF v-print-fmt EQ "Xprint" THEN
            ASSIGN
            v-program      = "cec/quote/quoxprnt.p"
            lines-per-page = 66.  
        ELSE IF v-print-fmt EQ "FibreX" THEN
            ASSIGN
            v-program      = "cec/quote/quoxfib.p"
            lines-per-page = 66.  
     
        ELSE IF v-print-fmt EQ "Frankstn" OR v-print-fmt EQ "Mirpkg" THEN
            ASSIGN
            v-program      = "cec/quote/quofrank.p"
            lines-per-page = 66.
        ELSE
            IF v-print-fmt EQ "Elite" THEN
                ASSIGN
                v-program      = "cec/quote/quoelite.p"
                lines-per-page = 66.  
            
        ELSE
            IF v-print-fmt EQ "premierX" THEN
                ASSIGN
                v-program      = "cec/quote/quoxprem.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "SouthPak" THEN
                ASSIGN
                v-program      = "cec/quote/quosthpk.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "APC" THEN
                ASSIGN
                v-program      = "cec/quote/quoxapc.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "Midwest" THEN
                ASSIGN
                v-program      = "cec/quote/quomwest.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "Century" THEN
                ASSIGN
                v-program      = "cec/quote/quocentx.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "Unipak" THEN
                ASSIGN
                v-program      = "cec/quote/quounipk.p"
                lines-per-page = 66.  
            ELSE IF v-print-fmt EQ "Oracle" THEN
                ASSIGN
                v-program      = "cec/quote/quooracl.p"
                lines-per-page = 66.  
            
            ELSE IF v-print-fmt EQ "OTTPkg" THEN
                ASSIGN
                v-program      = "cec/quote/quoottpk.p"
                lines-per-page = 66.
            
            ELSE IF v-print-fmt EQ "CSC-EXCEL" THEN
                ASSIGN
                v-program      = "cec/quote/quocsc-xl.p"
                lines-per-page = 66.
            ELSE IF v-print-fmt EQ "TRILAKE-EXCEL" THEN
                ASSIGN
                v-program      = "cec/quote/quotri-xl.p"
                lines-per-page = 66.
            
            ELSE IF v-print-fmt EQ "Concepts" THEN
                ASSIGN
                v-program      = "cec/quote/quocorc.p"
                lines-per-page = 66.
            
            ELSE IF v-print-fmt EQ "PPI" THEN
                ASSIGN
                v-program      = "cec/quote/quoppi.p"
                lines-per-page = 66.  
            
            ELSE
                IF AVAIL est AND est.est-type GT 4 THEN
                    ASSIGN
                    v-program      = "cec/quote/quoasi.p"
                    lines-per-page = 56.
                
            /*ELSE
                ASSIGN
                    v-program      = "cec/quote/quoasi.p"
                    lines-per-page = IF v-log THEN 50 ELSE 56.
              */                                             
 MESSAGE "v-program" v-program.
 RUN BatchMail(INPUT begin_cust,INPUT begin_quo#) .
   
/*RUN custom/usrprint.p (v-prgmname).*/
  
 END.  /*IF prmAction*/
/***************************************************************************************************************/
PROCEDURE BatchMail :
  DEFINE INPUT PARAM icBegCustNo  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icBegQteNo  AS CHAR NO-UNDO.

  MESSAGE "v-print-fmt" v-print-fmt lv-pdf-file begin_quo#.
      IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" THEN
         lv-pdf-file = "C:\Inetpub\wwwroot\pdfs\" + (IF v-print-fmt EQ "Century" THEN "CBXQuote"
                       ELSE "QT") + STRING(begin_quo#).

      ELSE
          init-dir    = "C:\Inetpub\wwwroot\pdfs\".

          lv-pdf-file = init-dir + 'QUOTE' .

          lv-pdf-file = lv-pdf-file + STRING(begin_quo#) + '.pdf'.
          vPdfFile   = 'QUOTE' + STRING(begin_quo#) + '.pdf'.

      MESSAGE "quote"  .
      RUN run-report (INPUT yes, INPUT icBegCustNo).
      CREATE ttPrintQuote1.
      ASSIGN ttPrintQuote1.QteFile = vPdfFile.
      MESSAGE "ttPrintQuote" ttPrintQuote1.QteFile.
END PROCEDURE.
/**************************************************************************************************************/
PROCEDURE run-report :
    DEFINE INPUT PARAMETER ip-mail-mode AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.
    {sys/form/r-top.i}
    
DEF VAR lv-quo-no LIKE quotehd.q-no NO-UNDO.
DEF VAR lv-loc LIKE quotehd.loc INIT "" NO-UNDO.
DEF VAR li AS INT NO-UNDO.
MESSAGE "assign" begin_quo# end_quo# begin_cust end_cust.
ASSIGN 
 fquote   = begin_quo#
 tquote   = end_quo#
 /*v-prt-box = tb_prt-box
 s-sep-page = tb_page
 s-prt-quoimage = tb_prt-quoimage*/
 fcust =  begin_cust
 tcust = end_cust.

ASSIGN
 ch-inst  = NO
 ch-note  = tb_note
 ch-sort  = SUBSTR("Quote#",1,1)
 /*v-comm   = tb_comm*/
 ch-multi = NO.
 /*s-print-comp = tb_prt-comp
 v-notesPageSpan = tb_notesSpanPage
 v-boardDescription = tb_boardDescription
 s-print-2nd-dscr = tb_print-2nd-dscr.*/
 

 
{sys/inc/print1.i}
MESSAGE "print" lines-per-page. 
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/
MESSAGE "assign1" ch-inst ch-note  ch-sort ch-multi.
FOR EACH tt-quote:
  DELETE tt-quote.
END.

DO WHILE TRUE:  /* because loc is in header */
  FIND FIRST quotehd
      WHERE quotehd.company EQ cocode
        AND quotehd.loc     GT lv-loc
      USE-INDEX q-no NO-LOCK NO-ERROR.
  IF NOT AVAIL quotehd THEN LEAVE.
  lv-loc = quotehd.loc.
  MESSAGE "qte"   lv-loc cocode. 
  RELEASE quotehd.
  IF fcust EQ tcust AND fquote NE tquote THEN
      FOR EACH quotehd WHERE quotehd.cust-no GE fcust
                          AND quotehd.cust-no LE tcust
                          AND quotehd.company EQ cocode
                          AND quotehd.loc     EQ lv-loc
                          AND quotehd.q-no    GE fquote
                          AND quotehd.q-no    LE tquote /*USE-INDEX cust2 */ NO-LOCK:
      CREATE tt-quote.
      tt-quote.row-id = ROWID(quotehd).

      END.  /**FOR EACH quotehd*/

ELSE
    FOR EACH quotehd WHERE quotehd.company EQ cocode
                      AND quotehd.loc     EQ lv-loc
                      AND quotehd.cust-no GE fcust
                      AND quotehd.cust-no LE tcust
                      AND quotehd.q-no    GE fquote
                      AND quotehd.q-no    LE tquote USE-INDEX q-no NO-LOCK:
    CREATE tt-quote.
    tt-quote.row-id = ROWID(quotehd).
    END.  /*FOR EACH quotehd*/                                              
MESSAGE "jyoti1".
    DO li = 1 TO NUM-ENTRIES(v-quo-list):
        RELEASE quotehd.
        lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
            FIND FIRST quotehd WHERE quotehd.company EQ cocode
                                 AND quotehd.loc     EQ lv-loc  
                                 AND quotehd.q-no    EQ lv-quo-no USE-INDEX q-no NO-LOCK NO-ERROR.
        IF AVAIL quotehd THEN DO:
            CREATE tt-quote.
            ASSIGN
                tt-quote.row-id = ROWID(quotehd)
                tt-quote.tt-seq = li.
            MESSAGE "quote"  tt-quote.tt-seq.
        END. /*IF AVAIL quotehd THEN DO:*/
    END.  /*DO:*/
END.  /*do while*/
FOR EACH tt-quote BREAK BY tt-quote.row-id BY tt-seq:   
  IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
END.

FOR EACH tt-quote BREAK BY tt-quote.row-id:
    MESSAGE "ch-multi" ch-multi.
  ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
  LEAVE.
END.
FIND FIRST quotehd  WHERE quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
    FIND FIRST est WHERE est.company EQ quotehd.company AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.
MESSAGE "avail" prmQuote est.est-no.
{sa/sa-sls01.i}

IF NOT ch-multi AND quotehd.est-no NE "" THEN
FOR EACH tt-quote,

    FIRST quotehd
    WHERE ROWID(quotehd) EQ tt-quote.row-id
      AND (CAN-FIND(FIRST est
                    WHERE est.company EQ quotehd.company
                      AND est.est-no  EQ quotehd.est-no) OR
           lookup(v-print-fmt,"Century,Unipak,PPI") > 0 )
    NO-LOCK,

    first quoteitm OF quotehd no-lock,

    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    no-lock

    transaction:
  
  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
  report.key-02  = if ch-sort eq "E" then quotehd.est-no              else
                    if ch-sort eq "C" then quoteitm.part-no            else
                    if ch-sort eq "A" then string(tt-seq,"9999999999") else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd)
    vQuote       = STRING(quotehd.q-no) 
                        .
  MESSAGE "report" report.term-id  report.key-01 report.key-02 report.key-03 report.rec-id vQuote.
end.

ELSE    
FOR EACH tt-quote,
    FIRST quotehd WHERE ROWID(quotehd) EQ tt-quote.row-id NO-LOCK,
    first quoteitm OF quotehd no-lock,
    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    NO-LOCK
    transaction:
    IF quotehd.est-no <> "" AND lookup(v-print-fmt,"Century,Unipak,PPI") <= 0 THEN DO:
       FIND first est where est.company eq quotehd.company
                   AND est.est-no  EQ quotehd.est-no nO-LOCK NO-ERROR.
       IF NOT AVAIL est THEN  NEXT.
    END.

  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
   report.key-02  = if ch-sort eq "E" then quotehd.est-no              else
                    if ch-sort eq "C" then quoteitm.part-no            else
                    if ch-sort eq "A" then string(tt-seq,"9999999999") else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd)
    vQuote       = STRING(quotehd.q-no) .                                                                         
MESSAGE "report1"  v-term quotehd.cust-no report.key-02  report.key-03  report.rec-id vQuote .                                                                        
end.

ASSIGN
 v-term-id        = v-term
 .

/* Check for XL also */
IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" THEN.
ELSE
IF IS-xprint-form THEN DO:
    MESSAGE "rd-dest" rd-dest.
    CASE rd-dest:
        WHEN 5 THEN do:
            IF lookup(v-print-fmt,"century,unipak,ppi") > 0 THEN       
               PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
            ELSE PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
            MESSAGE "pdf" lv-pdf-file. 
            END.
    END CASE.
END.
MESSAGE "v-program" v-program.
RUN value(v-program).

for each report where report.term-id eq v-term-id:
  delete report.
end.

ASSIGN
   tb_note = YES
/*   rd_sort = "Quote#"*/
   .

END PROCEDURE.
