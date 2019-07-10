
/*------------------------------------------------------------------------
    File        : quoteprint.p(Copy of est/r-quoprt.w)
    Purpose     : Print Quotes
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mar 09 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttQuotePrint NO-UNDO
        FIELD vFile AS CHAR
        FIELD vhhhtyh AS CHAR
        FIELD vhhhfds AS CHAR.
        .
    DEFINE DATASET dsQuotePrint FOR ttQuotePrint .

    /*{methods/prgsecur.i} */
{custom/xprint.i}
{est/printquo.i NEW}


DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO .
DEFINE INPUT PARAMETER prmBeginCust     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmEndCust       AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmBeginDept     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmEndDept       AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmBeginQuote    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmEndQuote      AS INT NO-UNDO. 
DEFINE INPUT PARAMETER prmQuoteList     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmInst          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmNotesSpanPage AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmNote          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmPrtBox        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmComm          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmPrtComp       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmPrint2ndDscr  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmRsNote        AS CHARACTER NO-UNDO.  
DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuotePrint.
DEFINE OUTPUT PARAMETER vFile  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER vMailto  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER vBody  AS CHAR NO-UNDO.

DEF VAR v-name-time AS CHAR NO-UNDO.
DEFINE VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR ls-to-list AS cha NO-UNDO.
ASSIGN v-name-time =   /*STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") +*/ STRING(TIME) .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF TEMP-TABLE tt-quote FIELD row-id AS ROWID
                        FIELD tt-seq AS INT INIT 999999999
                        FIELD cust-no AS CHAR
                        INDEX row-id row-id.

DEF BUFFER b-tt-quote FOR tt-quote.

DEF VAR vQuote       AS CHAR NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR v-tmp-lines-per-page AS INT NO-UNDO.
DEF VAR vlSkipRec AS LOG NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF VAR lv-termPath AS CHAR NO-UNDO.
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

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" NO-UNDO.
DEFINE VARIABLE lv-termFile AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE rs_note AS CHARACTER INITIAL "Corr" NO-UNDO.
DEFINE VARIABLE tb_comm AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_inst AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_notesSpanPage AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_print-2nd-dscr AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_prt-box AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_prt-comp AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_prt-quoimage AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_prt-shp2 AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_terms AS LOGICAL INITIAL NO NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VAR g_company  AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE ttoutputfile AS CHARACTER.

DEFINE NEW SHARED VAR k  AS INT NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.
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
DEF BUFFER b-est FOR est.
DEF BUFFER b-quotehd-2 FOR quotehd.
def NEW shared buffer xquo for quotehd.
DEFINE NEW SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmUser          = ? THEN ASSIGN prmUser          = "".
IF prmBeginCust     = ? THEN ASSIGN prmBeginCust     = "".
IF prmEndCust       = ? THEN ASSIGN prmEndCust       = "".
IF prmBeginDept     = ? THEN ASSIGN prmBeginDept     = "".
IF prmEndDept       = ? THEN ASSIGN prmEndDept       = "".
IF prmBeginQuote    = ? THEN ASSIGN prmBeginQuote    = 0.
IF prmEndQuote      = ? THEN ASSIGN prmEndQuote      = 0.
IF prmQuoteList     = ? THEN ASSIGN prmQuoteList     = "".
IF prmInst          = ? THEN ASSIGN prmInst          = "".
IF prmNotesSpanPage = ? THEN ASSIGN prmNotesSpanPage = "".
IF prmNote          = ? THEN ASSIGN prmNote          = "".
IF prmPrtBox        = ? THEN ASSIGN prmPrtBox        = "".
IF prmComm          = ? THEN ASSIGN prmComm          = "".
IF prmPrtComp       = ? THEN ASSIGN prmPrtComp       = "".
IF prmPrint2ndDscr  = ? THEN ASSIGN prmPrint2ndDscr  = "".
IF prmRsNote        = ? THEN ASSIGN prmRsNote        = "".
IF prmOut           = ? THEN ASSIGN prmOut           = "No" .

/***************************************************************************/
ASSIGN
    v-quo-list          = prmQuoteList     
    begin_cust          = prmBeginCust
    begin_dept          = prmBeginDept
    begin_quo#          = prmBeginQuote
    end_cust            = prmEndCust
    end_dept            = prmEndDept
    end_quo#            = prmEndQuote            
    rs_note             = prmRsNote  
    tb_comm             = IF prmComm = "Yes" THEN TRUE ELSE FALSE         
    tb_inst             = IF prmInst = "Yes" THEN TRUE ELSE FALSE         
    tb_note             = IF prmNote = "Yes" THEN TRUE ELSE FALSE         
    tb_notesSpanPage    = IF prmNotesSpanPage = "Yes" THEN TRUE ELSE FALSE
    tb_print-2nd-dscr   = IF prmPrint2ndDscr = "Yes" THEN TRUE ELSE FALSE 
    tb_prt-box          = IF prmPrtBox = "Yes" THEN TRUE ELSE FALSE       
    tb_prt-comp         = IF prmPrtComp = "Yes" THEN TRUE ELSE FALSE      
    .   

    
/* ********************  Preprocessor Definitions  ******************** */
ASSIGN 
        g_company = prmComp 
        cocode = prmComp 
        locode = usercomp.loc
        .
FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN 
    ASSIGN v-webrootpath = sys-ctrl.char-fld
    CallingParameter = sys-ctrl.char-fld
    .

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "QUOPRINT"
      no-lock no-error.

ASSIGN
   v-print-fmt = sys-ctrl.char-fld
   CallingParameter = sys-ctrl.char-fld
   v-log       = sys-ctrl.log-fld
   vcDefaultForm = v-print-fmt.

RUN SetQuoForm(v-print-fmt).


IF prmAction = "PrintQuote" THEN do:

    ASSIGN 
           lv-pdf-file = v-webrootpath 
           is-xprint-form   = TRUE.

    ASSIGN
            fquote   = begin_quo#
            tquote   = end_quo#
            v-prt-box = tb_prt-box
            s-prt-quoimage = tb_prt-quoimage
            fcust = begin_cust
            tcust = end_cust
            ch-inst  = tb_inst
            fdept    = begin_dept
            tdept    = end_dept
            ch-note  = tb_note            
            v-comm   = tb_comm
            ch-multi = fquote NE tquote
            s-print-comp = tb_prt-comp
            v-notesPageSpan = tb_notesSpanPage         
            s-print-2nd-dscr = tb_print-2nd-dscr           
            .
    
    IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company = cocode AND
        sys-ctrl-shipto.NAME = "QUOPRINT") THEN
        DO:
        
           EMPTY TEMP-TABLE tt-quote.
           lv-loc = "".

           DO WHILE TRUE:

              FIND FIRST quotehd
                   WHERE quotehd.company EQ cocode
                     AND quotehd.loc     GT lv-loc
              USE-INDEX q-no NO-LOCK NO-ERROR.

              IF NOT AVAIL quotehd THEN LEAVE.
              lv-loc = quotehd.loc.
              RELEASE quotehd.

              FOR EACH quotehd WHERE
                  quotehd.company EQ cocode AND
                  quotehd.loc EQ lv-loc AND
                  quotehd.cust-no GE fcust AND
                  quotehd.cust-no LE tcust AND
                  quotehd.q-no    GE fquote AND
                  quotehd.q-no    LE tquote
                  NO-LOCK:   
                 
                 
                  
            
                  CREATE tt-quote.
                  ASSIGN
                     tt-quote.row-id = ROWID(quotehd)
                     tt-quote.cust-no = quotehd.cust-no.
                            
              END.

              /*DO li = 1 TO NUM-ENTRIES(v-quo-list):
                 RELEASE quotehd.
                 lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
                 IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
                 FIND FIRST quotehd
                     WHERE quotehd.company EQ cocode
                       AND quotehd.loc     EQ lv-loc  
                       AND quotehd.q-no    EQ lv-quo-no
                     USE-INDEX q-no NO-LOCK NO-ERROR.
                 
                 IF AVAIL quotehd THEN DO:
                   CREATE tt-quote.
                   ASSIGN
                    tt-quote.row-id = ROWID(quotehd)
                    tt-quote.tt-seq = li
                    tt-quote.cust-no = quotehd.cust-no.
                 END.
              END.
              */
           END. /*do while true*/

           FOR EACH b-tt-quote
               BREAK BY b-tt-quote.row-id BY b-tt-quote.tt-seq:   
               IF NOT FIRST-OF(b-tt-quote.row-id) THEN DELETE b-tt-quote.
           END.

           
           FOR EACH b-tt-quote
               BREAK BY b-tt-quote.cust-no:

               IF FIRST-OF (b-tt-quote.cust-no) THEN DO:
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company      = cocode AND
                       sys-ctrl-shipto.NAME         = "QUOPRINT" AND
                       sys-ctrl-shipto.cust-vend    = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-tt-quote.cust-no AND
                       sys-ctrl-shipto.char-fld > '' 
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:                      
                     RUN SetQuoForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetQuoForm (vcDefaultForm).
                     v-print-fmt = vcDefaultForm.
                  END.
                  
                   
                  
                  ASSIGN
                    is-xprint-form = YES
                    v-print-fmt = "APC" 
                    v-program      = "cec/quote/quoxapc.p"
                    lines-per-page = 66.                 


                  ASSIGN
                    init-dir = v-webrootpath
                    lv-pdf-file = init-dir + 'QUOTE' .
                    lv-pdf-file = lv-pdf-file + STRING(begin_quo#) + v-name-time.                                       
                  
                    MESSAGE "runrptcall".
                 
                  RUN run-report-sys-ctrl-shipto(b-tt-quote.cust-no).   
                  
               END.
           END.           

        END. /* if can-find sys-ctrl-shipto*/
     ELSE /* not can-find sys-ctrl-shipto*/
     DO:
        v-print-fmt = vcDefaultForm.        
        /*RUN SetGlobalVariables.*/
        RUN run-report(INPUT NO, INPUT "").              
     END.
     
     IF list-name NE '' THEN DO:       
         MESSAGE "listname" list-name.
        /*RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

        vPdfFile   = 'QUOTE' + STRING(begin_quo#) + '.pdf'.
        CREATE   ttQuotePrint.
        ASSIGN ttQuotePrint.vFile = vPdfFile.  
        */      
     END. 



   /* IF rd-dest = 5  THEN
        ASSIGN LvOutputSelection = "Email"
               lv-pdf-file = v-webrootpath 
               is-xprint-form   = TRUE.

    FIND FIRST quotehd  WHERE quotehd.q-no = prmBeginQuote NO-LOCK.
    IF AVAILABLE quotehd THEN 
        ASSIGN 
        begin_quo# =  quotehd.q-no
        begin_cust =  quotehd.cust-no
        end_quo#   =  quotehd.q-no
        end_cust   =  quotehd.cust-no.
        
    
    FIND FIRST est WHERE est.company EQ quotehd.company
                    AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.
    EMPTY TEMP-TABLE tt-filelist.
    ASSIGN cocode = quotehd.company
           locode = quotehd.loc
           g_company = quotehd.company.
    
    FIND first sys-ctrl where sys-ctrl.company eq cocode
           and sys-ctrl.name    eq "QUOPRINT" no-lock no-error.
    if not avail sys-ctrl then
        do transaction:
        create sys-ctrl.
        assign
            sys-ctrl.company = cocode
            sys-ctrl.name    = "QUOPRINT"
            sys-ctrl.descrip = "Print Quote Headers on Quote Form?".

    
        end. /*do transaction:*/
        ASSIGN
            v-print-fmt = sys-ctrl.char-fld
            CallingParameter = sys-ctrl.char-fld
            v-log       = sys-ctrl.log-fld.

        MESSAGE "v-print-fmt" v-print-fmt .
        
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
 
 RUN BatchMail(INPUT begin_cust,INPUT begin_quo#) .
 */
  
   
/*RUN custom/usrprint.p (v-prgmname).*/
  
 END.  /*IF prmAction*/
/***************************************************************************************************************/
PROCEDURE BatchMail :
    DEFINE INPUT PARAM icBegCustNo  AS CHAR NO-UNDO.
    DEFINE INPUT PARAM icBegQteNo  AS CHAR NO-UNDO.
    ASSIGN
        init-dir = v-webrootpath
        lv-pdf-file = init-dir + 'QUOTE' .
    lv-pdf-file = lv-pdf-file + STRING(begin_quo#).
    RUN run-report (INPUT yes, INPUT icBegCustNo).
   /* IF not(v-print-fmt EQ "CSC-EXCEL" OR
           v-print-fmt EQ "TRILAKE-EXCEL") THEN*/    
     
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    vPdfFile   = 'QUOTE' + STRING(begin_quo#) + v-name-time + '.pdf'.
    CREATE   ttQuotePrint.
    ASSIGN ttQuotePrint.vFile = vPdfFile.
ASSIGN v-prgmname = "R-QuoPrt.".

        FIND FIRST quotehd  WHERE quotehd.q-no = prmBeginQuote NO-LOCK NO-ERROR.
            vBody = "Quote Number " +  STRING(quotehd.q-no)
                      + " has been printed." + CHR(10).
            FOR EACH quoteitm OF quotehd NO-LOCK :
                
                vBody = vBody  + "<br>" + "<br>"
                   + "Item: " + STRING(quoteitm.i-no) + "&nbsp;&nbsp;"
                   + " Estimate: " + STRING(quotehd.est-no) + "<br>"  
                   + " Date: " + STRING(quotehd.quo-date) + "&nbsp;&nbsp;" 
                   + " Cust Part#: "  + STRING(quoteitm.part-no) + CHR(100). 
            END.
       
             FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = quotehd.cust-no NO-LOCK NO-ERROR.                 
       
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}

            ASSIGN vMailto =  ls-to-list .

    END PROCEDURE.
/**************************************************************************************************************/
PROCEDURE run-report :
    DEFINE INPUT PARAMETER ip-mail-mode AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.
    {sys/form/r-top.i}
    
DEF VAR lv-quo-no LIKE quotehd.q-no NO-UNDO.
DEF VAR lv-loc LIKE quotehd.loc INIT "" NO-UNDO.
DEF VAR li AS INT NO-UNDO.

/*
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
 
 */

ASSIGN
    fquote   = begin_quo#
    tquote   = end_quo#     
    v-prt-box = tb_prt-box    
    s-prt-quoimage = tb_prt-quoimage
    fcust = begin_cust
    tcust = end_cust
    ch-inst  = tb_inst
    fdept    = begin_dept
    tdept    = end_dept
    ch-note  = tb_note    
    v-comm   = tb_comm
    ch-multi = fquote NE tquote
    s-print-comp = tb_prt-comp
    v-notesPageSpan = tb_notesSpanPage
    s-print-2nd-dscr = tb_print-2nd-dscr           
    .


if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}
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
            
        END. /*IF AVAIL quotehd THEN DO:*/
    END.  /*DO:*/
END.  /*do while*/
FOR EACH tt-quote BREAK BY tt-quote.row-id BY tt-seq:   
  IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
END.

FOR EACH tt-quote BREAK BY tt-quote.row-id:
    
  ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
  LEAVE.
END.
FIND FIRST quotehd  WHERE quotehd.q-no = prmBeginQuote NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
    FIND FIRST est WHERE est.company EQ quotehd.company AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.

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

end.

ASSIGN
 v-term-id        = v-term
 .
/* Check for XL also */
IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" THEN.
ELSE
IF IS-xprint-form THEN DO:
    
    IF lookup(v-print-fmt,"century,unipak,ppi") > 0 THEN       
        PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
    ELSE PUT "<PDF=DIRECT><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
END.

RUN value(v-program).

for each report where report.term-id eq v-term-id:
  delete report.
end.

ASSIGN
   tb_note = YES.
END PROCEDURE.


/*************************************************************************************************************/

PROCEDURE SetQuoForm :

   DEFINE INPUT PARAM icPrintFormat AS CHAR NO-UNDO.
       
   IF INDEX("Pacific,Xprint,Hughes,SouthPak,ABox,Midwest,Axis,MWFIBRE,century,Concepts,oracle,Harwell,PremierX,Elite,Unipak,Ottpkg,Frankstn,Mirpkg,APC,Perform,FibreX,Boss,Protagon,Loylang,LoylangBSF,PPI,Packrite,Xprint30,StClair,AllWest,Soule,Sultana,SouleMed,Simkins,CCC,Peachtree,Oklahoma,Accord",icPrintFormat) > 0 THEN
      is-xprint-form = YES.     
   ELSE is-xprint-form = NO.

   CASE icPrintFormat:
       WHEN "HOP" THEN ASSIGN v-program = "cec/quote/quohop.p" lines-per-page = 37.
       WHEN "LandScap" THEN ASSIGN v-program = "ce/quote/landquo.p" lines-per-page = 56.
       WHEN "ContSrvc" OR WHEN "Triad" THEN ASSIGN v-program = "cec/quote/quocsc.p" lines-per-page = 56.
       WHEN "Rudd" THEN ASSIGN v-program = "cec/quote/quorudd.p" lines-per-page = 66.
       WHEN "General" THEN ASSIGN v-program = "cec/quote/quogener.p" lines-per-page = 56.
       WHEN "10 Pitch" THEN ASSIGN v-program = "cec/quote/prtquo10.p" lines-per-page = 56.
       WHEN "Brick" THEN ASSIGN v-program = "cec/quote/quobrick.p" lines-per-page = 38.
       WHEN "Fibre" THEN ASSIGN v-program = "cec/quote/quofibre.p" lines-per-page = 52.
       WHEN "Harwell" THEN ASSIGN v-program = "cec/quote/quohawl.p" lines-per-page = 56.
       WHEN "Pacific" THEN ASSIGN v-program = "cec/quote/quopacif.p" lines-per-page = 66.
       WHEN "Abox" THEN ASSIGN v-program = "cec/quote/quoabox.p" lines-per-page = 66.
       WHEN "Xprint" THEN ASSIGN v-program = "cec/quote/quoxprnt.p" lines-per-page = 66.
       WHEN "Hughes" THEN ASSIGN v-program = "cec/quote/quohughes.p" lines-per-page = 66.
       WHEN "Oklahoma" THEN ASSIGN v-program = "cec/quote/quookla.p" lines-per-page = 66.
       WHEN "FibreX" THEN ASSIGN v-program = "cec/quote/quoxfib.p" lines-per-page = 66.
       WHEN "Boss" THEN ASSIGN v-program = "cec/quote/quoboss.p" lines-per-page = 66.
       WHEN "Protagon" THEN ASSIGN v-program = "cec/quote/quoprotg.p" lines-per-page = 66.
       WHEN "Loylang" THEN ASSIGN v-program = "cec/quote/quolylng.p" lines-per-page = 66.
       WHEN "LoylangBSF" THEN ASSIGN v-program = "cec/quote/quolylng.p" lines-per-page = 66.
       WHEN "Frankstn" OR WHEN "Mirpkg" THEN ASSIGN v-program = "cec/quote/quofrank.p" lines-per-page = 66.
       WHEN "Elite" THEN ASSIGN v-program = "cec/quote/quoelite.p" lines-per-page = 66.
       WHEN "premierX" THEN ASSIGN v-program = "cec/quote/quoxprem.p" lines-per-page = 66.
       WHEN "Premier-Excel" THEN ASSIGN v-program = "cec/quote/quoprm-xl.p" lines-per-page = 66.
       WHEN "Bell-Excel" THEN ASSIGN v-program = "cec/quote/quobell-xl.p" lines-per-page = 66.
       WHEN "CCC-Excel" THEN ASSIGN v-program = "cec/quote/quoccc-xl.p" lines-per-page = 66.
       WHEN "SouthPak" THEN ASSIGN v-program = "cec/quote/quosthpk.p" lines-per-page = 66.   
       WHEN "APC" THEN ASSIGN v-program = "cec/quote/quoxapc.p" lines-per-page = 66.
       WHEN "Perform" THEN ASSIGN v-program = "cec/quote/quoprfrm.p" lines-per-page = 66.
       WHEN "Midwest" THEN ASSIGN v-program = "cec/quote/quomwest.p" lines-per-page = 66.
       WHEN "Axis" THEN ASSIGN v-program = "cec/quote/quoaxis.p" lines-per-page = 66.
       WHEN "MWFIBRE" THEN ASSIGN v-program = "cec/quote/quomwfib.p" lines-per-page = 66.
       WHEN "Century" THEN ASSIGN v-program = "cec/quote/quocentx.p" lines-per-page = 66.
       WHEN "Unipak" THEN ASSIGN v-program = "cec/quote/quounipk.p" lines-per-page = 66.
       WHEN "Oracle" THEN ASSIGN v-program = "cec/quote/quooracl.p" lines-per-page = 66.
       WHEN "OTTPkg" THEN ASSIGN v-program = "cec/quote/quoottpk.p" lines-per-page = 66.
       WHEN "CSC-EXCEL" THEN ASSIGN v-program = "cec/quote/quocsc-xl.p" lines-per-page = 66.
       WHEN "FIBRE-EXCEL" THEN ASSIGN v-program = "cec/quote/quofib-xl.p" lines-per-page = 66.
       WHEN "TRILAKE-EXCEL" THEN ASSIGN v-program = "cec/quote/quotri-xl.p" lines-per-page = 66.
       WHEN "Concepts" THEN ASSIGN v-program = "cec/quote/quocorc.p" lines-per-page = 66.
       WHEN "Accord" THEN ASSIGN v-program = "cec/quote/quoaccd.p" lines-per-page = 66.
       WHEN "PPI" THEN ASSIGN v-program = "cec/quote/quoppi.p" lines-per-page = 66.
       WHEN "Packrite" THEN ASSIGN v-program = "cec/quote/quopkrit.p" lines-per-page = 66.
       WHEN "KNIGHT-EXCEL" THEN ASSIGN v-program = "cec/quote/quoknight-xl.p" lines-per-page = 66. /* gdm - 11060808 */
       WHEN "Xprint30" THEN ASSIGN v-program = "cec/quote/qoxpnt30.p" lines-per-page = 66.
       WHEN "StClair" THEN ASSIGN v-program = "cec/quote/qosclair.p" lines-per-page = 66.
       WHEN "MSPACK-EXCEL" THEN ASSIGN v-program = "cec/quote/quomsp-xl.p" lines-per-page = 66.
      /* gdm - 04200908*/
       WHEN "AllWest" THEN ASSIGN v-program = "cec/quote/quoalwst.p" lines-per-page = 66.        
      /* gdm - 04300907*/
       WHEN "Soule" THEN ASSIGN v-program = "cec/quote/quosoule.p" lines-per-page = 80.
       WHEN "Sultana" THEN ASSIGN v-program = "cec/quote/quosult.p" lines-per-page = 80.
       WHEN "SouleMed" THEN ASSIGN v-program = "cec/quote/quosoulemed.p" lines-per-page = 66.    
       WHEN "Simkins" THEN ASSIGN v-program = "cec/quote/quosmkct.p" lines-per-page = 66.
       WHEN "CCC" THEN ASSIGN v-program = "cec/quote/quoccc.p" lines-per-page = 66.
       WHEN "Peachtree" THEN ASSIGN v-program = "cec/quote/quoxptree.p" lines-per-page = 66. 
       OTHERWISE DO:
          IF AVAIL est AND est.est-type GT 4 THEN
             ASSIGN
             v-program = "cec/quote/quoasi.p"
             lines-per-page = 56.

          ELSE
             ASSIGN
                v-program = "cec/quote/quoasi.p"
                lines-per-page = IF v-log THEN 50 ELSE 56.
       END.
   END.
  
  v-tmp-lines-per-page = lines-per-page.

END PROCEDURE.



/************************************************************************************************************/

PROCEDURE run-report-sys-ctrl-shipto :
DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.

{sys/form/r-top.i}

ASSIGN
   fcust = icCustNo
   tcust = icCustNo.

/*
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmp" + string(time)
init-dir = tmp-dir.
*/

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").
*/

FOR EACH tt-quote WHERE
    tt-quote.cust-no EQ icCustno
    BREAK BY tt-quote.row-id BY tt-seq:   
    IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
END.

FOR EACH tt-quote WHERE
    tt-quote.cust-no EQ icCustno
    BREAK BY tt-quote.row-id:
    ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
    LEAVE.
END.

/*
IF NOT ch-multi THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   tb_note.
END.
*/

/*FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.*/
FIND FIRST quotehd WHERE quotehd.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
   FIND FIRST est WHERE est.company EQ quotehd.company
         AND est.est-no  EQ quotehd.est-no
         NO-LOCK NO-ERROR.

{sa/sa-sls01.i}

IF NOT ch-multi AND quotehd.est-no NE "" THEN
FOR EACH tt-quote WHERE
    tt-quote.cust-no EQ icCustno,
    FIRST quotehd
    WHERE ROWID(quotehd) EQ tt-quote.row-id
      AND (CAN-FIND(FIRST est
                    WHERE est.company EQ quotehd.company
                      AND est.est-no  EQ quotehd.est-no) OR
           lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") > 0 )
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
   report.rec-id  = recid(quotehd).
end.

ELSE    
FOR EACH tt-quote WHERE
    tt-quote.cust-no EQ icCustno,
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
    IF quotehd.est-no <> "" AND lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") <= 0 THEN DO:
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
   report.rec-id  = recid(quotehd).


    MESSAGE "reportrecid" report.rec-id.
    MESSAGE "rptkey03" report.key-03.
end.

ASSIGN
 v-term-id        = v-term
 v-lines-per-page = lines-per-page.


/*
IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" OR v-print-fmt = "KNIGHT-EXCEL" OR v-print-fmt = "MSPACK-EXCEL" OR v-print-fmt = "FIBRE-EXCEL" THEN DO:
        CREATE   ttQuotePrint.
        ASSIGN ttQuotePrint.vFile = ttoutputfile.
END.
*/

/* Check for XL also */

MESSAGE IS-xprint-form v-print-fmt.

IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" OR v-print-fmt = "FIBRE-EXCEL" OR v-print-fmt = "KNIGHT-EXCEL" 
   OR v-print-fmt = "MSPACK-EXCEL" THEN.
ELSE
IF IS-xprint-form THEN DO:
    /*CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?></PROGRESS>".
        WHEN 2 THEN PUT "<PREVIEW></PROGRESS>".        
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
        END.
        WHEN 5 THEN do: */
            /*IF lookup(v-print-fmt,"century,unipak,PPI,Packrite") > 0 THEN       
               PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
            ELSE PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
              */    

            IF lookup(v-print-fmt,"century,unipak,ppi,Packrite") > 0 THEN       
             PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
            ELSE PUT "<PDF=DIRECT><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".                    

        /*END.
    END CASE. */
END.

MESSAGE "prg" v-program.

RUN value(v-program).

for each report where report.term-id eq v-term-id:
  delete report.
end.

/*
ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   rd_sort:SCREEN-VALUE = "Quote#"
   tb_note
   rd_sort.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE ("").
*/

end procedure.
