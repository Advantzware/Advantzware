/*------------------------------------------------------------------------
    File        : bolInvPrint.p
    Purpose     : BOL Print

    Syntax      :

    Description : Return a Dataset of all Order Inquiry invoice BOL

    Author(s)   : Kuldeep
    Created     : DEC 20 2007
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

{bolInvPrint.i}
{custom/xprint.i}


DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRowid      AS RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbolInvPrint.

DEFINE OUTPUT PARAMETER vFile  AS CHAR NO-UNDO.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE STREAM s1.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5 NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>"  NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9"  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE tb_posted AS LOGICAL NO-UNDO.
DEF VAR v-term as char no-undo.

DEFINE VARIABLE tb_reprint AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 44 NO-UNDO.
DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE rd_bolcert AS CHARACTER INITIAL "BOL" NO-UNDO.
DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_print-shipnote AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_print-dept AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE fi_depts AS CHARACTER FORMAT "X(100)" NO-UNDO.
def var list-name         as char FORMAT "X(100)" no-undo.
def var init-dir          as char no-undo.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

{oe/rep/oe-lad.i NEW}
{oe/oe-bolpi.i NEW}  
{oe/bolcheck.i NEW}  
{oe/closchk.i NEW}
{custom/formtext.i NEW}

/* Buffers */
DEF NEW SHARED BUFFER xoe-ord       FOR oe-ord.
DEFINE BUFFER         bf-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-cust       FOR cust.
DEFINE BUFFER         b1-oe-bolh    FOR oe-bolh.
DEFINE BUFFER         b1-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-shipto     FOR shipto.
DEFINE BUFFER         b-oe-bolh     FOR oe-bolh.
DEFINE BUFFER         b-cust        FOR cust.

DEF STREAM barcode.

DEF TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-packslip
    FIELD b-no AS INT.

def var v-print-fmt     as char no-undo format 'x'.
def var v-headers       as log  no-undo.
def var v-print-coc     as log  no-undo.
def var v-check-qty     as log  no-undo.
DEF VAR v-program       AS CHAR NO-UNDO.
DEF VAR is-xprint-form  AS LOG  NO-UNDO.
DEF VAR ls-fax-file     AS CHAR NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEF VAR vcMailMode      AS CHAR NO-UNDO.
DEF VAR vcDefaultForm   AS CHAR NO-UNDO.

DEF VAR lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */
DEF VAR lv-run-bol        AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.
DEF VAR v-packslip AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR tmp-path AS CHAR NO-UNDO .
DEF VAR v-VERSION AS CHAR NO-UNDO.
/* Build a Table to keep sequence of pdf files */
DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

DEF TEMP-TABLE tt-ci-form NO-UNDO
    FIELD form-name AS CHAR
    FIELD total-pallets LIKE oe-bolh.tot-pallets
    INDEX tt-ci-form form-name ASC.

def NEW SHARED TEMP-TABLE w-comm-bol NO-UNDO
    field bol-no as INT
    INDEX bol-no bol-no.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
/************************************************************************/
DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmComp = "001".
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ "001"
    AND usercust.user_id = prmUser NO-ERROR.

assign
  cocode = prmComp
  locode = usercomp.loc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode} /* rstark 05181205 */

FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "X-VERSION"
         sys-ctrl.descrip  = "Server Name"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "Server 2003".
   END.
   IF AVAIL sys-ctrl  THEN
        v-VERSION = sys-ctrl.char-fld .
  RELEASE sys-ctrl.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "Xspool"
         sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "c:\spool\".
   END.
   IF AVAIL sys-ctrl  THEN
        tmp-path = sys-ctrl.char-fld .
  RELEASE sys-ctrl.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.




FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ prmComp AND
     sys-ctrl.name    EQ "PACKSLIP"
     NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = prmComp
         sys-ctrl.name     = "PACKSLIP"
         sys-ctrl.descrip  = "C:\BA\Packslip\".
   END.

v-packslip = sys-ctrl.descrip.

IF v-packslip = "" THEN
   v-packslip = "c:~\ba~\label~\packslip.txt".
ELSE do:
   IF NOT(SUBSTRING(v-packslip,LENGTH(v-packslip),1) = "/" OR
      SUBSTRING(v-packslip,LENGTH(v-packslip),1) = "\") THEN
      v-packslip = v-packslip + "/".

   v-packslip = v-packslip + "packslip.txt".
END.

RELEASE sys-ctrl.

IF prmAction     = ? THEN ASSIGN prmAction     = "".

/* ********************  Preprocessor Definitions  ******************** */
IF prmAction = "printBol" THEN do:
ASSIGN LvOutputSelection = "Email".
EMPTY TEMP-TABLE tt-filelist.
FIND oe-boll WHERE RECID(oe-boll) = vRowid NO-LOCK.
 /*ASSIGN cocode = oe-boll.company
     locode = oe-boll.loc*/
     /*g_company = oe-boll.company */ .
                                      IF AVAILABLE oe-boll THEN 
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN
    ASSIGN                       
        begin_cust =  oe-bolh.cust-no
        end_cust   =  oe-bolh.cust-no
        begin_bol# =  oe-boll.bol-no
        end_bol#   =  oe-boll.bol-no
        tb_reprint =  oe-bolh.printed
        tb_posted  =  oe-bolh.posted
        begin_ord# = 0
        end_ord#   = 99999999
                                              .
 
 find first company where company.company eq cocode no-lock no-error.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLPRINT"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPRINT"
     sys-ctrl.descrip = "Print Bill of Lading Headers on Bill of Lading Form?".
    /*MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.*/
  end.
  v-print-hdgs = sys-ctrl.log-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMT"
      NO-LOCK no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLFMT"
     sys-ctrl.descrip  = "Bill of lading format"
     sys-ctrl.char-fld = "ASI".
   /* message "System control record not found. Update BOL Print format"
    update sys-ctrl.char-fld.*/
  end.
  assign
   v-print-fmt = sys-ctrl.char-fld
   v-headers   = sys-ctrl.log-fld.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLCERT"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLCERT"
     sys-ctrl.descrip  = "Print Certificate of Compliance forms?"
     sys-ctrl.log-fld  = no.
    /*MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.*/
  end.
  assign
   v-print-coc = sys-ctrl.log-fld
   v-coc-fmt   = sys-ctrl.char-fld. 

  find first sys-ctrl
       where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLPOST"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPOST"
     sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
     choice           = yes.
   
    /*MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice. */
  
    if not choice then sys-ctrl.char-fld eq "Bin>Qty".
  end.
  v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".
  
  IF NOT CAN-FIND (FIRST sys-ctrl 
                   WHERE sys-ctrl.company = cocode
                   AND sys-ctrl.NAME    = 'CINVOICE') THEN
    DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.NAME    = 'CINVOICE'
             sys-ctrl.descrip = 'Commercial Invoice Forms'.
      RELEASE sys-ctrl.
   END.
/*ASSIGN v-print-fmt = "Michcor".*/
   
ASSIGN vcDefaultForm =  v-print-fmt.
IF  rd-dest = 5 THEN
  DO:
  IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company = cocode AND
        sys-ctrl-shipto.NAME = "BOLFMT") THEN
        DO:
           IF CAN-FIND(FIRST b-oe-bolh WHERE
              b-oe-bolh.company eq cocode AND
              b-oe-bolh.bol-no  ge begin_bol# AND
              b-oe-bolh.bol-no  le end_bol# AND
              b-oe-bolh.cust-no GE begin_cust AND
              b-oe-bolh.cust-no LE end_cust AND
              b-oe-bolh.printed eq tb_reprint AND
              b-oe-bolh.posted  eq tb_posted AND
              can-find (FIRST b1-oe-boll
                        WHERE b1-oe-boll.company EQ b-oe-bolh.company AND
                              b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND
                              b1-oe-boll.ord-no  GE begin_ord# AND
                              b1-oe-boll.ord-no  LE end_ord#)) THEN
              FOR EACH b-oe-bolh WHERE
                  b-oe-bolh.company eq cocode AND
                  b-oe-bolh.bol-no  ge begin_bol# AND
                  b-oe-bolh.bol-no  le end_bol# AND
                  b-oe-bolh.cust-no GE begin_cust AND
                  b-oe-bolh.cust-no LE end_cust AND
                  b-oe-bolh.printed eq tb_reprint AND
                  b-oe-bolh.posted  eq tb_posted AND
                  can-find (FIRST b1-oe-boll WHERE
                                  b1-oe-boll.company EQ b-oe-bolh.company AND
                                  b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND
                                  b1-oe-boll.ord-no  GE begin_ord# AND
                                  b1-oe-boll.ord-no  LE end_ord#)
                  NO-LOCK
                  USE-INDEX post
                  BREAK BY b-oe-bolh.company
                        BY b-oe-bolh.cust-no:
         
                  IF FIRST-OF(b-oe-bolh.cust-no) THEN
                  DO:
                     FIND FIRST sys-ctrl-shipto WHERE
                          sys-ctrl-shipto.company = cocode AND
                          sys-ctrl-shipto.NAME = "BOLFMT" AND
                          sys-ctrl-shipto.cust-vend = YES AND
                          sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                          sys-ctrl-shipto.ship-id = b-oe-bolh.ship-id AND
                          sys-ctrl-shipto.char-fld > ''
                          NO-LOCK NO-ERROR.
                    
                     IF NOT AVAIL sys-ctrl-shipto THEN
                        FIND FIRST sys-ctrl-shipto WHERE
                             sys-ctrl-shipto.company = cocode AND
                             sys-ctrl-shipto.NAME = "BOLFMT" AND
                             sys-ctrl-shipto.cust-vend = YES AND
                             sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                             sys-ctrl-shipto.char-fld > ''
                             NO-LOCK NO-ERROR.
                    
                     IF AVAIL sys-ctrl-shipto THEN
                     DO:
                        RUN SetBolForm(sys-ctrl-shipto.char-fld).
                        v-print-fmt = sys-ctrl-shipto.char-fld.
                     END.
                     ELSE
                     DO:
                        RUN SetBolForm (vcDefaultForm).
                        v-print-fmt = vcDefaultForm.
                     END.
                     RUN output-to-mail(INPUT usercust.cust-no,YES).
                  END.
              END. /*end for each*/
        END. /*sys-ctrl-shipto found*/
         ELSE /*no sys-ctrl-shipto found*/
     DO:
              
        v-print-fmt = vcDefaultForm.
        RUN SetBolForm (vcDefaultForm).
        RUN output-to-mail(INPUT "",NO).
       /* IF lv-run-commercial = "YES" AND NOT IS-xprint-form THEN */
    /*RUN run-report-ci.*/


     END. 


  /*IF tb_barcode THEN
     RUN barcode-proc.
    */
    /*IF tb_ComInvoice THEN
     RUN CommercialInvoice.*/

  EMPTY TEMP-TABLE tt-fg-bin.
/*  IF ll AND oe-ctrl.u-inv AND v-check-qty THEN*/
     FOR EACH tt-post,
         FIRST oe-bolh NO-LOCK 
         WHERE ROWID(oe-bolh) EQ tt-post.row-id:
    
         RUN oe/bolcheck.p (ROWID(oe-bolh)).
        
         IF CAN-FIND (FIRST w-except 
                      WHERE w-except.bol-no EQ oe-bolh.bol-no) THEN DO:
        
            /*MESSAGE "BOL has insufficient inventory and cannot be posted..."
                VIEW-AS ALERT-BOX ERROR.            ll = NO.*/
                     END.
     END.



END.     /**if rd-dest  = 5******************/
END.  /*IF prmAction = "bolPrint" THEN do:*/
  /**************************************************************************************************************/
PROCEDURE output-to-mail :
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
  ASSIGN 
    v-print-bol =  YES              .
/*    init-dir    = "C:\Inetpub\wwwroot\pdfs\".
   lv-pdf-file = init-dir + "BOL".*/
 
IF AVAILABLE oe-boll THEN DO:
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN
    
assign
    v-s-cust            = begin_cust
    v-e-cust            = end_cust
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-bol         =  YES      
    lv-run-bol          = ""
    lv-run-commercial   = "".
FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE begin_cust
       AND b1-cust.cust-no LE end_cust,
    
     FIRST b1-oe-bolh
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge begin_bol#
       and b1-oe-bolh.bol-no  le end_bol#
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq tb_reprint
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE begin_ord#
                       AND b1-oe-boll.ord-no  LE end_ord#)
    USE-INDEX post:
    /* XPrint */
       
     /* IF v-print-fmt = "SouthPak-XL" THEN ASSIGN lv-pdf-file = "C:\Inetpub\wwwroot\pdfs\bol".
      ELSE ASSIGN*/

         IF AVAIL b1-oe-bolh THEN
         ASSIGN vcBOLNums  =  string(b1-oe-bolh.bol-no) .
           lv-pdf-file =  v-webrootpath + 'BOL' . 
           lv-pdf-file = lv-pdf-file + vcBOLNums + STRING(TIME)  .
           vPdfFile = 'BOL' + vcBOLNums + STRING(TIME) + '.pdf'.

                   RUN run-report-mail (INPUT b1-cust.cust-no,
                          INPUT '',
                          INPUT 1,
                          INPUT v-printed).
           /*IF list-name NE ? AND
           list-name NE ''
          THEN RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
          ELSE RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").  */

       IF v-VERSION = "Server 2008" THEN do:
           OS-COPY VALUE(list-name) VALUE (tmp-path).  
           PAUSE 15.
       END.
         ELSE
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").



          CREATE ttbolInvPrint.
      ASSIGN ttbolInvPrint.aFile = vPdfFile.
    END.
  END. /* each cust */

END PROCEDURE.


PROCEDURE run-report-mail :
/* --------------------------------------------------------*/

  DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM iLprinted AS LOG NO-UNDO.


  {sys/form/r-top.i}
  
      assign
    v-s-cust            = icCustNo
    v-e-cust            = icCustNo
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = iLprinted
    v-print-bol         =  YES
    
    
    lv-run-bol          = ""
    lv-run-commercial   = "".
  
  IF NOT v-print-bol THEN DO:
  
    IF v-coc-fmt = "Xprint" THEN
      ASSIGN 
        is-xprint-form = YES
        v-program      = "oe/rep/cocxprnt.p".
  
     ELSE 
       ASSIGN 
        is-xprint-form = NO
        v-program      = "oe/rep/cocbrick.p".
  END.
  
  /*{sys/inc/print1.i}*/
  if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmpbol" + string(time)
       init-dir = tmp-dir.
  {sys/inc/outprint.i value(lines-per-page)}
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
                    run build-work (ic2ndKey).
  
/*  IF NOT vcBOLNums > '' THEN RETURN.*/
 if can-find (first report where report.term-id eq v-term-id) then
  do:
      IF v-print-fmt = "Century" 
        THEN PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file  + ".pdf>" FORM "x(200)".
        ELSE PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-OUTPUT=" + lv-pdf-file +  ".pdf>" FORM "x(200)".
      RUN value(v-program).
     
  
    IF lv-run-commercial = "YES" AND 
       IS-xprint-form THEN DO:
       RUN oerep/runbolci.p.
    END.   
  END.
  /*else do:
    MESSAGE 'No records to process. Job aborted.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    return.
  end.
    */
  for each report 
     where report.term-id eq v-term-id:
    delete report.
  end.
  
  OUTPUT CLOSE.

end procedure.


/***************************************************************************************************************/
PROCEDURE SetBOLForm :
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.
       
   ASSIGN
     v-print-mode = ""
     is-xprint-form = NO
     lv-prt-bypass = NO.

   CASE icFormName:
       WHEN "1/2 page" THEN
          ASSIGN
             v-program = "oe/rep/bolhalfp.p"
             lines-per-page = 44.
       WHEN "Royal" THEN
          ASSIGN
             v-program = "oe/rep/bolroyal.p"
             lines-per-page = 56.
       WHEN "ContSrvc" THEN
          ASSIGN
             v-program = "oe/rep/bol-csc.p"
             lines-per-page = 66.
       WHEN "HOP" THEN
          ASSIGN
             v-program = "oe/rep/bolhop.p"
             lines-per-page = 39.
       WHEN "Superior" THEN
          ASSIGN
             v-program = "oe/rep/bolsuper.p"
             lines-per-page = 55.
       WHEN "Premier" THEN
          ASSIGN
             v-program = "oe/rep/bolprem.p"
             lines-per-page = 55.
       WHEN "PremierX" THEN
          ASSIGN
             v-program      = "oe/rep/bolpremx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierCX" THEN
          ASSIGN
             v-program      = "oe/rep/bolpremcx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierPX" THEN
          ASSIGN
             v-program      = "oe/rep/bolprempx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "KDWILLSX" THEN
          ASSIGN
             v-program      = "oe/rep/bolkdwlx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "TrilakesX" OR WHEN "TrilakesLot#" THEN
          ASSIGN
             v-program      = "oe/rep/boltrilx.p"
             is-xprint-form = YES
             lines-per-page = 80.      
       WHEN "RFC" THEN
          ASSIGN
             v-program = "oe/rep/bolrfc.p"
             lines-per-page = 55.
       WHEN "Sonoco" THEN
          ASSIGN
             v-program = "oe/rep/bolsonoc.p"
             lines-per-page = 51.
       WHEN "Warren" THEN
          ASSIGN
             v-program = "oe/rep/bolwarrn.p"
             lines-per-page = 59.
       WHEN "PAC 1/2" THEN
          ASSIGN
             v-program = "oe/rep/bolpack.p"
             lines-per-page = 44.
       WHEN "Imperial" THEN
          ASSIGN
             v-program = "oe/rep/bolimp.p"
             lines-per-page = 60.
       WHEN "P&P" THEN
          ASSIGN
             v-program = "oe/rep/bolpnp.p"
             lines-per-page = 62.
       WHEN "TriLakes" THEN
          ASSIGN
             v-program = "oe/rep/boltril.p"
             lines-per-page = 62.
       WHEN "Triad" THEN
          ASSIGN
             v-program = "oe/rep/boltriad.p"
             lines-per-page = 62.
       WHEN "TriState" THEN
          ASSIGN
             v-program = "oe/rep/boltrist.p"
             lines-per-page = 41.
       WHEN "BlueRidg" THEN
          ASSIGN
             v-program = "oe/rep/bolbluer.p"
             lines-per-page = 65.
       WHEN "BlueRidg2" THEN
          ASSIGN
             v-program = "oe/rep/bolbluer2.p"
             lines-per-page = 50.
       WHEN "Danbury" THEN
          ASSIGN
             v-program = "oe/rep/boldnbry.p"
             lines-per-page = 42.
       WHEN "Boxtech" THEN
          ASSIGN
             v-program = "oe/rep/bolboxt.p"
             lines-per-page = 55
             lv-prt-bypass = YES.
       WHEN "Empire" THEN
          ASSIGN
             v-program = "oe/rep/bolempir.p"
             lines-per-page = 54.
       WHEN "Herman" THEN
          ASSIGN
             v-program = "oe/rep/bolhermn.p"
             lines-per-page = 54.
       WHEN "pacific" THEN
          ASSIGN
             v-program      = "oe/rep/bolpacif.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "century" THEN
          ASSIGN
             v-program = "oe/rep/bolcentx.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ccc" THEN
          ASSIGN
             v-program = "oe/rep/bolccc.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Xprint" OR WHEN "bolfmt 1" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt.p"
             is-xprint-form = YES
             lines-per-page = 66.
      WHEN "bolfmt 10" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt10.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "Wingate-BOL" THEN
          ASSIGN
             v-program = "oe/rep/bolwinget.p"
             is-xprint-form = YES
             lines-per-page = 80.
        WHEN "bolfmt10-CAN" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt10can.p"
             is-xprint-form = YES
             lines-per-page = 80. 
        WHEN "Coburn" THEN
          ASSIGN
             v-program = "oe/rep/bolcobrn.p"
             is-xprint-form = YES
             lines-per-page = 66.
        WHEN "Lakeside" THEN
          ASSIGN
             v-program = "oe/rep/bollake.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Multicell" THEN
          ASSIGN
             v-program = "oe/rep/bolmcell.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Accordbc" THEN
          ASSIGN
             v-program = "oe/rep/bolacrdbc.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "PackRite" THEN
          ASSIGN
             v-program = "oe/rep/bolprite.p"
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "MidwestX" THEN
          ASSIGN
             v-program = "oe/rep/bolmidx.p"
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "Lamar Pkg" THEN
          ASSIGN
             v-program = "oe/rep/bollamarpkg.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Protagon" THEN
          ASSIGN
             v-program = "oe/rep/bolprotg.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CapCityIN" THEN
          ASSIGN
             v-program      = "oe/rep/bolcapcin.p"
             is-xprint-form = YES
             lines-per-page = 90.      
       WHEN "FibreCI" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibci.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/bolxapc.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "P&PX" THEN ASSIGN v-program = "oe/rep/bolpnpx.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Delta" THEN ASSIGN v-program = "oe/rep/boldelta.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Xprint2" OR WHEN "bolfmt 2" THEN ASSIGN v-program = "oe/rep/bolxprt2.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "bolfmt 20" THEN ASSIGN v-program = "oe/rep/bolxprt20.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "LancoYork" THEN ASSIGN v-program = "oe/rep/bollanyork.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Chillicothe" THEN ASSIGN v-program = "oe/rep/bolchict.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "NSTOCK" THEN ASSIGN v-program = "oe/rep/bolnstok.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "CSCIN" THEN ASSIGN v-program = "oe/rep/bolcscin.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak" THEN ASSIGN v-program = "oe/rep/bolsouth.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak-XL" THEN ASSIGN v-program = "oe/rep/bolsouth-xl.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "MWBox" THEN ASSIGN v-program = "oe/rep/bolmwbox.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/bolhughm.p" is-xprint-form = YES lines-per-page = 70.
       WHEN "HughesTags" THEN ASSIGN v-program = "oe/rep/bolhughs.p" is-xprint-form = YES lines-per-page = 70.
       WHEN "Inland" THEN ASSIGN v-program = "oe/rep/bolxinld.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "concepts" THEN ASSIGN v-program = "oe/rep/bolxcorc.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "Brick" THEN
       DO:
          IF v-headers THEN
             ASSIGN
                v-program = "oe/rep/bolbrck1.p"
                lines-per-page = 64.
          ELSE
             ASSIGN
                v-program = "oe/rep/bolbrick.p"
                lines-per-page = 60.
       END.
       WHEN "AllPkg" THEN
          ASSIGN
             v-program = "oe/rep/bolallpk.p"
             lines-per-page = 60.
       WHEN "Fibre" THEN
          ASSIGN
             v-program = "oe/rep/bolfibre.p"
             lines-per-page = 55. /*gdm 0506095 - ADJUSTED FROM 57*/
       WHEN "MaxPak" THEN
          ASSIGN
             v-program = "oe/rep/bolmaxpk.p"
             lines-per-page = 42.
       WHEN "Oracle" THEN
          ASSIGN
             v-program = "oe/rep/bolora2.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Knight" THEN
          ASSIGN
             v-program = "oe/rep/bolkni.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Frankstn" OR WHEN "Mirpkg" THEN
          ASSIGN
             v-program = "oe/rep/bolfrank.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "PPI" THEN
          ASSIGN
             v-program = "oe/rep/bolppi.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Indiana" THEN
          ASSIGN
             v-program = "oe/rep/bolindc.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Ottpkg" THEN
          ASSIGN
             v-program = "oe/rep/bolottpk.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "DEE" THEN
          ASSIGN
             v-program = "oe/rep/boldee.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ConsBox" THEN
          ASSIGN
             v-program = "oe/rep/bolcnsbx.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Harwell" THEN
          ASSIGN
             v-program = "oe/rep/bolharwl.p"
             lines-per-page = 62.
       WHEN "Chillic" THEN
          ASSIGN 
             v-program = "oe/rep/bolchill.p"
             lines-per-page = 60.
       WHEN "Midwest" THEN
          ASSIGN 
             v-program = "oe/rep/bolmdwst.p"
             lines-per-page = 52.
       WHEN "Intrpack" THEN
          ASSIGN 
             v-program = "oe/rep/bolinter.p"
             lines-per-page = 56.
       WHEN "Dayton" THEN
          ASSIGN
             v-program = "oe/rep/boldaytn.p"
             lines-per-page = 57.
       WHEN "Elite" THEN
          ASSIGN
             v-program      = "oe/rep/bolelite.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "ACPI" THEN
          ASSIGN
             v-program      = "oe/rep/bolacpi.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CSC-GA" THEN
          ASSIGN
             v-program      = "oe/rep/bolcscga.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Fibrex" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibrex.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "BOLfmt15" THEN
          ASSIGN
             v-program      = "oe/rep/bolfiftn.p"
             is-xprint-form = YES
             lines-per-page = 66.
      WHEN "BOLFMTX15" THEN
          ASSIGN
             v-program      = "oe/rep/bolfrftn.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Michcor" THEN
          ASSIGN
             v-program = "oe/rep/bolmich.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Express" THEN
          ASSIGN
             v-program = "oe/rep/bolexprs.p"
             lines-per-page = 62.
       WHEN "Hamilton" THEN
          ASSIGN
             v-program = "oe/rep/bolhamil.p"
             lines-per-page = 44.
       WHEN "Allwest" THEN
          ASSIGN
             v-program      = "oe/rep/bolallws.p"                                      
             is-xprint-form = YES
             lines-per-page = 66.      
      
       WHEN "COLOR" THEN  /* gdm - 07140907 */
          ASSIGN v-program = "oe/rep/bolcolor.p" 
                 is-xprint-form = YES 
                 lines-per-page = 66.
    
       WHEN "Badger" THEN /* gdm - 08270906 */
          ASSIGN
             v-program = "oe/rep/bolbadgr.p"                                 
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Loylang" THEN  /* gdm - 10200901 */
          ASSIGN
             v-program      = "oe/rep/bolloyln.p"
             is-xprint-form = YES
             lines-per-page = 66.      

       WHEN "Carded" THEN   /* gdm - 11170903 */
          ASSIGN
             v-program      = "oe/rep/bolcard.p"
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Metro" THEN
          ASSIGN
             v-program = "oe/rep/bolmet.p"
             is-xprint-form = YES
             lines-per-page = 64.
      
       WHEN "MetroTags" THEN
          ASSIGN
             v-program = "oe/rep/bolmettg.p"
             is-xprint-form = YES
             lines-per-page = 64.

       WHEN "PEACHTREE" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolptree.p" 
                  is-xprint-form = YES 
                  lines-per-page = 80.

       WHEN "PeachTreeBC" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolptreebc.p" 
                  is-xprint-form = YES 
                  lines-per-page = 80.

       WHEN "Soule" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolsoule.p" 
                  is-xprint-form = YES 
                  lines-per-page = 62.

       OTHERWISE
          ASSIGN
             v-print-mode   = "PROD"
             v-program      = "oe/rep/oe-lad" +
                              (if v-print-fmt eq "c" then "c" else "s") + ".p"
             lines-per-page = 62.
   END CASE.
  
  v-lines-per-page = lines-per-page.
  
END PROCEDURE.





/***************************************************************************************************/
PROCEDURE run-report-ci :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER  bf-oe-boll          FOR oe-boll.
DEF VAR     lv-run-bol          AS char NO-UNDO.
DEF VAR     lv-run-commercial   AS char no-undo.
DEF VAR     v-tmp-is-xprint     AS LOG  NO-UNDO.

{sys/form/r-top.i}
    FIND oe-boll WHERE RECID(oe-boll) = vRowid NO-LOCK.
    IF AVAILABLE oe-boll THEN 
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN
    ASSIGN 
        begin_cust =  oe-bolh.cust-no
        end_cust   =  oe-bolh.cust-no
        begin_bol# =  oe-boll.bol-no
        end_bol#   =  oe-boll.bol-no
        tb_reprint =  oe-bolh.printed
        tb_posted  =  oe-bolh.posted
        begin_ord# = 0
        end_ord#   = 99999999
        .



assign
  v-s-cust            = begin_cust
  v-e-cust            = end_cust
  v-s-bol             = begin_bol#
  v-e-bol             = end_bol#
  v-s-ord             = begin_ord#
  v-e-ord             = end_ord#
  v-printed           = tb_reprint
  v-print-pal         = tb_pallet
  v-print-bol         = rd_bolcert EQ "BOL"
  v-print-components  = tb_print-component
  v-print-shipnotes   = tb_print-shipnote
  v-print-dept        = tb_print-dept
  lv-run-bol          = ""
  lv-run-commercial   = ""
  v-tmp-is-xprint     = IS-xprint-form
  is-xprint-form      = YES.

IF fi_depts = "NO" THEN
   ASSIGN
     v-print-dept = LOGICAL(tb_print-dept)
     v-depts = fi_depts.

IF NOT v-print-bol THEN DO:

   IF v-coc-fmt = "Xprint" THEN 
     ASSIGN is-xprint-form  = YES
            v-program       = "oe/rep/cocxprnt.p".
   ELSE IF v-coc-fmt = "" OR v-coc-fmt EQ "Brick" THEN
     ASSIGN is-xprint-form  = NO
            v-program       = "oe/rep/cocbrick.p".
   ELSE
      ASSIGN is-xprint-form = NO
             v-program = "oe/rep/cocuni.p".
END.

/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmpbol" + string(time)
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

/*{sa/sa-sls01.i}*/

v-term-id = v-term.

build-work:
FOR EACH oe-bolh
   WHERE oe-bolh.company EQ cocode
     AND oe-bolh.bol-no  GE v-s-bol
     AND oe-bolh.bol-no  LE v-e-bol
     AND oe-bolh.cust-no GE v-s-cust
     AND oe-bolh.cust-no LE v-e-cust 
     AND oe-bolh.printed EQ v-printed
     AND oe-bolh.posted  EQ tb_posted
     AND CAN-FIND (FIRST oe-boll
                   WHERE oe-boll.company EQ oe-bolh.company
                     AND oe-boll.b-no    EQ oe-bolh.b-no
                     AND oe-boll.ord-no  GE v-s-ord
                     AND oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post.

  IF NOT oe-ctrl.p-bol THEN
  FOR EACH oe-boll
     WHERE oe-boll.company EQ oe-bolh.company
       AND oe-boll.bol-no  EQ oe-bolh.bol-no
       AND CAN-FIND (FIRST oe-ord
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                       AND oe-ord.stat    EQ "H")
      NO-LOCK:
    NEXT build-work.
  END.

  /* update loadtag status - Bill of lading task#: 10190414 */
  IF NOT oe-bolh.printed THEN
  FOR EACH bf-oe-boll NO-LOCK
     WHERE bf-oe-boll.company EQ oe-bolh.company 
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no
       AND bf-oe-boll.tag     NE "",
     FIRST loadtag
     WHERE loadtag.company    EQ bf-oe-boll.company
       AND loadtag.item-type  EQ NO
       AND loadtag.tag-no     EQ bf-oe-boll.tag USE-INDEX tag:

    loadtag.sts = "Bill of Lading".
  END.

  FIND FIRST sys-ctrl-shipto NO-LOCK
       WHERE sys-ctrl-shipto.company      EQ oe-bolh.company
         AND sys-ctrl-shipto.name         EQ "BOLFMT"
         AND sys-ctrl-shipto.cust-vend    EQ YES
         AND sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no
         AND sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id NO-ERROR.

  CREATE report.

  ASSIGN
   report.term-id  = v-term-id
   report.key-01   = oe-bolh.cust-no
   report.key-02   = oe-bolh.ship-id
   report.rec-id   = RECID(oe-bolh)
   report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
   report.key-03   =      IF AVAIL sys-ctrl-shipto AND NOT sys-ctrl-shipto.log-fld  THEN "C" /*commercial invoice only*/
                     ELSE IF AVAIL sys-ctrl-shipto AND     sys-ctrl-shipto.log-fld  THEN "B" /*commercial invoice and bol both*/
                     ELSE                                                                "N" /*BOL only*/ 
   report.key-04   =      IF AVAIL sys-ctrl-shipto THEN    sys-ctrl-shipto.char-fld ELSE "".     
                            
  IF lv-run-bol        EQ "" AND report.key-03 <> "C" THEN lv-run-bol        = "YES" .
  IF lv-run-commercial EQ "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
end.

v-lines-per-page = lines-per-page.
        IF rd-dest = 5 THEN do:
            IF v-print-fmt = "Century" THEN
                  PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file +  ".pdf>" FORM "x(200)".
                ELSE PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-OUTPUT=" + lv-pdf-file +  ".pdf>" FORM "x(200)".                                                    
        END.
    


IF lv-run-commercial = "YES" THEN DO:
   RUN oerep/runbolci.p.
END.
       

OUTPUT CLOSE.

for each report where report.term-id eq v-term-id:
    delete report.
end.
IS-xprint-form = v-tmp-is-xprint.

END PROCEDURE.

/***********************************************************************************************************/
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  EMPTY TEMP-TABLE tt-post.
  
  build-work:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode
       AND oe-bolh.bol-no  GE v-s-bol
       AND oe-bolh.bol-no  LE v-e-bol
       AND oe-bolh.cust-no GE v-s-cust
       AND oe-bolh.cust-no LE v-e-cust 
       AND oe-bolh.printed EQ v-printed
       AND oe-bolh.posted  EQ tb_posted
       AND CAN-FIND (FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                       AND oe-boll.ord-no  GE v-s-ord
                       AND oe-boll.ord-no  LE v-e-ord)
      USE-INDEX post:
  
    IF NOT oe-ctrl.p-bol THEN
    FOR EACH oe-boll
       WHERE oe-boll.company EQ oe-bolh.company
         AND oe-boll.bol-no  EQ oe-bolh.bol-no
         AND CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ oe-boll.company
                        AND oe-ord.ord-no  EQ oe-boll.ord-no
                        AND oe-ord.stat    EQ "H")
        NO-LOCK:
      NEXT build-work.
    END.
  
    /* update loadtag status - Bill of lading task#: 10190414 */
    IF NOT oe-bolh.printed THEN
    FOR EACH bf-oe-boll NO-LOCK
       WHERE bf-oe-boll.company EQ oe-bolh.company 
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no
         AND bf-oe-boll.tag     NE "",
       FIRST loadtag
       WHERE loadtag.company   EQ bf-oe-boll.company
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ bf-oe-boll.tag
   USE-INDEX tag:
      loadtag.sts = "Bill of Lading".
    END.
  
    IF ic2ndKey NE ? AND ic2ndKey NE '' THEN DO:
      FIND FIRST shipto NO-LOCK
           WHERE shipto.rec_key = ic2ndKey 
             AND shipto.ship-id = oe-bolh.ship-id NO-ERROR.
      IF NOT AVAIL shipto THEN NEXT build-work.
    END.

    FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.

    CREATE report.
    
    ASSIGN 
        report.term-id  = v-term-id
        report.key-01   = oe-bolh.cust-no
        report.key-02   = oe-bolh.ship-id
        report.rec-id   = RECID(oe-bolh)
        report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
        oe-bolh.printed = YES
        report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                          ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                          ELSE "N" /*BOL only*/ 
        report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE ""
        vcBOLNums       = vcBOLNums + '-' + STRING (oe-bolh.bol-no)
        vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

    IF vcBOLNums MATCHES '*-*' THEN
        vcBOLNums       = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).
    
    IF lv-run-bol        = "" AND report.key-03 <> "C" THEN lv-run-bol = "YES" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
    
    CREATE tt-post.
    tt-post.row-id = ROWID(oe-bolh).      
  END.
  
  v-lines-per-page = lines-per-page.

 
END PROCEDURE.


/******************************************************************************************/
PROCEDURE CommercialInvoice :

  
  EMPTY TEMP-TABLE tt-ci-form.

  RUN oerep\r-coinvbol.w(INPUT begin_bol#,
                         INPUT end_bol#).

  FOR EACH w-comm-bol,
      EACH b1-oe-bolh WHERE
           b1-oe-bolh.company eq cocode AND
           b1-oe-bolh.bol-no  EQ w-comm-bol.bol-no AND
           can-find (FIRST b1-oe-boll WHERE
                           b1-oe-boll.company EQ b1-oe-bolh.company AND
                           b1-oe-boll.b-no    EQ b1-oe-bolh.b-no)
           NO-LOCK,
      FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company      EQ cocode AND
            sys-ctrl-shipto.name         EQ "CINVOICE" AND
            sys-ctrl-shipto.cust-vend    EQ YES AND
            sys-ctrl-shipto.cust-vend-no EQ b1-oe-bolh.cust-no AND
            sys-ctrl-shipto.ship-id      EQ b1-oe-bolh.ship-id
            NO-LOCK:

      FIND FIRST tt-ci-form WHERE
           tt-ci-form.form-name = sys-ctrl-shipto.char-fld
           NO-ERROR.

      IF NOT AVAIL tt-ci-form THEN
      DO:
         CREATE tt-ci-form.
         ASSIGN tt-ci-form.form-name = sys-ctrl-shipto.char-fld.
      END.

      tt-ci-form.total-pallets = tt-ci-form.total-pallets
                               + b1-oe-bolh.tot-pallets.
  END.

  FOR EACH tt-ci-form:
      RUN oerep\d-fibreci.w (INPUT tt-ci-form.form-name,
                             INPUT tt-ci-form.total-pallets).
  END.
END PROCEDURE.
/******************************************************************************************************************/
