/*------------------------------------------------------------------------
    File        : signature_print_bol.p
    Purpose     : BOL Print

    Syntax      :

    Description : Return a Dataset of all Order Inquiry invoice BOL

    Author(s)   : 
    Created     : feb 5 2010
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttSignaturePrintBol NO-UNDO
    FIELD sign AS CHAR
    FIELD pdfpathname AS CHAR  .

DEFINE DATASET dsSignaturePrintBol FOR ttSignaturePrintBol .
    
DEFINE INPUT PARAMETER prmAction AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmcustno AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmbolno AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmprinted AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmposted AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmpostbol AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER prmBegDate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEndDate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCarrier  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER imagepath AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmBegOrder      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmEndOrder     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmPage         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPdfPath      AS CHAR NO-UNDO.

DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR  dsSignaturePrintBol .

IF prmprinted = ? THEN ASSIGN prmprinted = "Yes" .
IF prmposted  = ? THEN ASSIGN prmposted  = "Yes" .
IF prmpostbol = ? THEN ASSIGN prmpostbol = "Tes" .
IF prmBegOrder    = 0    THEN ASSIGN     prmBegOrder  = 0. 
IF prmEndOrder   = 0    THEN ASSIGN     prmEndOrder  = 99999999. 
IF prmBegOrder    = ?    THEN ASSIGN     prmBegOrder  = 0. 
IF prmEndOrder   = ?    THEN ASSIGN     prmEndOrder  = 99999999. 


DEFINE VAR ip-company AS CHAR NO-UNDO.
DEFINE VAR ip-loc AS CHAR NO-UNDO.
DEFINE VAR ip-cust-no AS CHAR NO-UNDO.
DEFINE VAR ip-bol-no AS INT NO-UNDO.
DEFINE VAR ip-printed AS LOG NO-UNDO.
DEFINE VAR ip-posted AS LOG NO-UNDO.
DEFINE VAR ip-post-bol AS LOG NO-UNDO.
DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no NO-UNDO.
DEFINE VAR pdfname  AS CHAR NO-UNDO.
DEF VAR ls-full-img AS CHAR FORMAT "x(250)"  NO-UNDO.
DEF VAR ls-image1 AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR vImage AS CHAR NO-UNDO.
DEF NEW SHARED VAR v-ship-inst AS LOG NO-UNDO.

ASSIGN v-ship-inst = YES.

    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

 ASSIGN
        ls-image1   = /*"Signatures/Bol350201021001723.jpeg"*/  imagepath 
        vImage      = ls-image1   .
ASSIGN
    ls-full-img = SEARCH(ls-image1) + ">" .


    
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmcustno NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = prmcustno NO-LOCK NO-ERROR.
IF NOT AVAIL cust  THEN  DO:
     ASSIGN cError = prmcustno + "  Invalid customer .....".
     RETURN.
END.


DEF VAR v-format-str AS CHAR INIT "BOLFMT" NO-UNDO.
def var v-print-fmt AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR lines-per-page AS INT INIT 99 NO-UNDO.
def var v-headers as log  no-undo.
def var list-name as char no-undo.
def var init-dir as char no-undo.
def var list-name2 as char no-undo.
def var init-dir2 as char no-undo.
DEF VAR tmp-path AS CHAR NO-UNDO .
DEF VAR v-VERSION AS CHAR NO-UNDO.

DEF VAR lv-run-bol AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
def var v-check-qty as log no-undo.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-webrootpath2 AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO. 
DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 NO-UNDO.

DEFINE VARIABLE lv-rcode-file AS CHAR NO-UNDO .
DEFINE VARIABLE rcodefile AS CHAR NO-UNDO.
DEF BUFFER b-oe-bolh FOR oe-bolh.
DEFINE BUFFER bf-oe-boll FOR oe-boll.
    
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF VAR dirwatchbol AS CHAR NO-UNDO.

{sys/inc/var.i new shared}
{oe/rep/oe-lad.i NEW}
{custom/xprint.i}
{oe/bolcheck.i NEW}
{oe/closchk.i NEW}
 {oerep/r-bolx.i NEW}
    ASSIGN
    cocode = prmComp .

    {XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode} /* rstark 05181205 */

ASSIGN
    ip-company  = prmComp 
    ip-loc      = /*usercomp.loc */ "MAIN" 
    ip-cust-no  = prmcustno 
    ip-bol-no   = prmbolno
    ip-printed  = IF prmprinted = "Yes" THEN TRUE ELSE FALSE 
    ip-posted   = IF prmprinted = "Yes" THEN TRUE ELSE FALSE
    tb_pallet   = IF prmposted  = "Yes" THEN TRUE ELSE FALSE
    ip-post-bol = IF prmpostbol = "Yes" THEN TRUE ELSE FALSE
    .


   ASSIGN
      begin_ord#  = prmBegOrder
      end_ord#    = prmEndOrder  .


FIND FIRST oe-bolh WHERE oe-bolh.company = prmComp AND oe-bolh.cust-no = prmcustno
      AND oe-bolh.bol-no = prmbolno NO-LOCK NO-ERROR .
IF NOT AVAIL oe-bolh THEN DO:
    ASSIGN cError = "Invalid BOL for the customer " + prmcustno .
    RETURN.
END.

FIND FIRST oe-bolh WHERE oe-bolh.company = prmComp AND oe-bolh.cust-no = prmcustno
      AND oe-bolh.bol-no = prmbolno AND oe-bolh.posted = YES NO-LOCK NO-ERROR .
IF NOT AVAIL oe-bolh THEN DO:
    ASSIGN cError = "The Bol No: " + string(prmbolno) + " is not Posted" .
    RETURN.
END.
IF AVAIL oe-bolh THEN DO:
    ASSIGN
        ip-posted = oe-bolh.posted 
        ip-printed  = oe-bolh.printed .

END.
   

   
IF prmAction = "Print" THEN DO:
    FIND FIRST sys-ctrl WHERE sys-ctrl.company = ip-company AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    
    IF prmPage = "Capture" THEN do:
    IF AVAIL sys-ctrl THEN 
        ASSIGN
         v-webrootpath = sys-ctrl.char-fld
         v-webrootpath2 = sys-ctrl.char-fld.
    FIND FIRST sys-ctrl WHERE sys-ctrl.company = ip-company AND sys-ctrl.NAME = "BOLSIGN" NO-LOCK NO-ERROR.

    ASSIGN 
        dirwatchbol = sys-ctrl.char-fld  
        rcodefile = sys-ctrl.char-fld   
        rcodefile =  REPLACE(rcodefile , "P:","D:") 
        lv-rcode-file = rcodefile  + STRING(prmbolno)  .
       
        
    
             ASSIGN
                 lv-pdf-file =  v-webrootpath + 'SignatureBOLPrint' + STRING(prmbolno)  +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")
                 + STRING(DAY(TODAY), "99")
                 pdfname     =  'SignatureBOLPrint' + STRING(prmbolno) +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")
                 + STRING(DAY(TODAY), "99") + '.pdf'          .

    END.
    IF prmPage = "SignedBol" THEN do:
    IF AVAIL sys-ctrl THEN 
        ASSIGN
         v-webrootpath = sys-ctrl.char-fld
         v-webrootpath2 = sys-ctrl.char-fld.
   
             ASSIGN
                 lv-pdf-file =  v-webrootpath + 'SignedBOLPrint' + STRING(prmbolno)  +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")
                 + STRING(DAY(TODAY), "99")
                 pdfname     =  'SignedBOLPrint' + STRING(prmbolno) +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")
                 + STRING(DAY(TODAY), "99") + '.pdf'          .

    END.
    IF prmPage = "Sign" THEN do:
    IF AVAIL sys-ctrl THEN
         ASSIGN
            v-webrootpath = /*"D:\webapps\asinet\source\Signatures\" */ prmPdfPath
            v-webrootpath2 = sys-ctrl.char-fld. 
                
             ASSIGN
                 lv-pdf-file =  v-webrootpath  
                 pdfname     =  'PrintedBOL'  + '.pdf'          .
    END.

    
   
    CREATE ttSignaturePrintBol .
    ASSIGN ttSignaturePrintBol.sign        = pdfname 
           ttSignaturePrintBol.pdfpathname = rcodefile  .


DEF TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id AS ROWID.

ASSIGN
   v-print-bol = YES
   cocode = ip-company
   locode = ip-loc.

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

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find first sys-ctrl WHERE
     sys-ctrl.company eq ip-company AND
     sys-ctrl.name    eq "BOLFMT"
     NO-LOCK no-error.

if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = ip-company
   sys-ctrl.name     = "BOLFMT"
   sys-ctrl.descrip  = "Bill of lading format"
   sys-ctrl.char-fld = "ASI".
  message "System control record not found. Update BOL Print format"
  update sys-ctrl.char-fld.
end.
assign
 v-print-fmt = sys-ctrl.char-fld
 v-headers   = sys-ctrl.log-fld.


find first sys-ctrl WHERE
     sys-ctrl.company eq cocode AND
     sys-ctrl.name    eq "BOLPOST"
     no-lock no-error.

if not avail sys-ctrl then do transaction:
   create sys-ctrl.
   assign
      sys-ctrl.company = cocode
      sys-ctrl.name    = "BOLPOST"
      sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty".
end.

v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".
IF prmPage = "Capture" OR prmPage = "SignedBol" THEN
    RUN SetBolForm(INPUT v-print-fmt).
IF prmPage = "Sign" THEN
    RUN SetBolForm2(INPUT v-print-fmt).

vcDefaultForm = v-print-fmt.

IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
   sys-ctrl-shipto.company = ip-company AND
   sys-ctrl-shipto.NAME = v-format-str) THEN
   DO:
      IF CAN-FIND(FIRST b-oe-bolh WHERE
         b-oe-bolh.company EQ ip-company AND
         b-oe-bolh.bol-no  EQ ip-bol-no AND
         b-oe-bolh.cust-no EQ ip-cust-no AND
         b-oe-bolh.printed EQ ip-printed AND
         b-oe-bolh.posted  EQ ip-posted AND
         CAN-FIND (FIRST oe-boll
                   WHERE oe-boll.company EQ b-oe-bolh.company
                     AND oe-boll.b-no    EQ b-oe-bolh.b-no
                     AND oe-boll.ord-no  GE begin_ord#
                     AND oe-boll.ord-no  LE end_ord#)) THEN
         FOR EACH b-oe-bolh WHERE
             b-oe-bolh.company EQ ip-company AND
             b-oe-bolh.bol-no  EQ ip-bol-no AND
             b-oe-bolh.cust-no EQ ip-cust-no AND
             b-oe-bolh.printed EQ ip-printed AND
             b-oe-bolh.posted  EQ ip-posted AND
             CAN-FIND (FIRST oe-boll
                       WHERE oe-boll.company EQ b-oe-bolh.company
                         AND oe-boll.b-no    EQ b-oe-bolh.b-no
                         AND oe-boll.ord-no  GE begin_ord#
                         AND oe-boll.ord-no  LE end_ord#)
             NO-LOCK
             BREAK BY b-oe-bolh.company
                   BY b-oe-bolh.cust-no:
   
             IF FIRST-OF(b-oe-bolh.cust-no) THEN
             DO:
                FIND FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company = ip-company AND
                     sys-ctrl-shipto.NAME = v-format-str AND
                     sys-ctrl-shipto.cust-vend = YES AND
                     sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                     sys-ctrl-shipto.ship-id      = b-oe-bolh.ship-id AND
                     sys-ctrl-shipto.char-fld > ''
                     NO-LOCK NO-ERROR.
   
                IF NOT AVAIL sys-ctrl-shipto THEN
                   FIND FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company = ip-company AND
                     sys-ctrl-shipto.NAME = v-format-str AND
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
                    IF prmPage = "Capture" OR prmPage = "SignedBol" THEN
                        RUN SetBolForm(vcDefaultForm).
                    IF prmPage = "Sign" THEN
                        RUN SetBolForm2(vcDefaultForm).
                   v-print-fmt = vcDefaultForm.
                END.  
               
               
                RUN run-report(b-oe-bolh.cust-no,YES).
                /*RUN GenerateReport(b-oe-bolh.cust-no,YES).*/
                
                 IF v-VERSION = "Server 2008" THEN do:
                     OS-COPY VALUE(list-name) VALUE (tmp-path).
                     PAUSE 1.
                 END.
                 ELSE
                     RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

                 IF prmPage = "Capture" THEN do:
                     
                     RUN run-report2("",NO).
                     IF v-VERSION = "Server 2008" THEN do:
                         OS-COPY VALUE(list-name2) VALUE (tmp-path).
                         PAUSE 1.
                     END.
                      ELSE
                          RUN printPDF (list-name2, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                         
                         OUTPUT TO d:\webapps\signbol.csv .
                         EXPORT DELIMITER ","
                             lv-rcode-file + ".pdf"
                             prmUser
                             prmComp  
                             dirwatchbol .
                         OUTPUT CLOSE.
                           
                       /*lv-rcode-file*/
                 END.

             END. /*first-of*/
         END. /*each b-oe-bolh*/
   END.
ELSE /*not find first sys-ctrl-shipto*/
DO:
   v-print-fmt = vcDefaultForm.
        IF prmPage = "Capture" OR prmPage = "SignedBol" THEN
            RUN SetBolForm(v-print-fmt).
        IF prmPage = "Sign" THEN
            RUN SetBolForm2(v-print-fmt).
   
   RUN run-report("",NO).
  
  /* RUN GenerateReport(ip-cust-no,NO).*/
   
   IF v-VERSION = "Server 2008" THEN do:
       OS-COPY VALUE(list-name) VALUE (tmp-path).
       PAUSE 1.
   END.
   ELSE
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

  IF prmPage = "Capture" THEN do:
       RUN run-report2("",NO).
      IF v-VERSION = "Server 2008" THEN do:
           OS-COPY VALUE(list-name2) VALUE (tmp-path).
           PAUSE 1.
      END.
       ELSE
           RUN printPDF (list-name2, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

           OUTPUT TO d:\webapps\signbol.csv .
                         EXPORT DELIMITER ","
                             lv-rcode-file + ".pdf"
                             prmUser
                             prmComp  
                             dirwatchbol .
                         OUTPUT CLOSE.


  END.


END.

ll = ip-post-bol AND NOT ip-posted  .


IF ll AND oe-ctrl.u-inv AND v-check-qty THEN
   FOR EACH tt-post,
       FIRST oe-bolh NO-LOCK 
       WHERE ROWID(oe-bolh) EQ tt-post.row-id:
   
       RUN oe/bolcheck.p (ROWID(oe-bolh)).
      
       IF CAN-FIND (FIRST w-except 
                    WHERE w-except.bol-no EQ oe-bolh.bol-no) THEN DO:
      
          MESSAGE "BOL has insufficient inventory and cannot be posted..."
              VIEW-AS ALERT-BOX ERROR.
          ll = NO.
          LEAVE.
       END.
   END.

IF ll THEN
   RUN post-bol.

END.  /*prmAction end*/

/*********************************************************************/

PROCEDURE run-report:

  DEFINE INPUT PARAMETER ip-cust-no-2 AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.
    
  {sys/form/r-top.i}
  
  assign
    v-s-bol             = ip-bol-no
    v-printed           = ip-printed
    v-print-pal         = tb_pallet
    v-print-components  = YES
    v-print-shipnotes   = YES
    v-print-dept        = NO


     v-s-ord             = begin_ord#
     v-e-ord             = end_ord#  .

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no-2.
  ELSE
     ASSIGN 
        v-s-cust = ip-cust-no.
  
  
if tmp-dir = "" then tmp-dir = v-webrootpath2 .
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.

 {sys/inc/outprint.i value(lines-per-page)} 
      
  PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><p8>" FORM "x(200)".
  
 
  {sa/sa-sls01.i}
  
  v-term-id = v-term.

  run build-work ('').


  IF lv-run-bol = "YES" THEN
     RUN value(v-program).
for each report where report.term-id eq v-term-id:
      delete report.
  end.
 
  OUTPUT CLOSE.
END.

PROCEDURE run-report2:

  DEFINE INPUT PARAMETER ip-cust-no-2 AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.
    
  {sys/form/r-top.i}
  
  assign
    v-s-bol             = ip-bol-no
    v-printed           = ip-printed
    v-print-pal         = tb_pallet
    v-print-components  = YES
    v-print-shipnotes   = YES
    v-print-dept        = NO


     v-s-ord             = begin_ord#
     v-e-ord             = end_ord#  .

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no-2.
  ELSE
     ASSIGN 
        v-s-cust = ip-cust-no.
  
  /*{sys/inc/print1.i}*/

IF prmPage = "Capture" THEN do:
       assign list-name2 = v-webrootpath2 + "\tmp2" + string(time)
          init-dir2 = v-webrootpath2 .

   {sys/inc/outprint2.i value(lines-per-page)} 
  
      PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + lv-rcode-file + ".pdf><p8>" FORM "x(200)".

END.

  {sa/sa-sls01.i}
  
  v-term-id = v-term.

  run build-work ('').


  IF lv-run-bol = "YES" THEN
     RUN value(v-program).
for each report where report.term-id eq v-term-id:
      delete report.
  end.
 
  OUTPUT CLOSE.
END.
/*********************************************************************/
PROCEDURE build-work:

   DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  build-work:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode
       AND oe-bolh.bol-no  EQ v-s-bol
       AND oe-bolh.cust-no EQ v-s-cust
       AND oe-bolh.printed EQ v-printed
       AND oe-bolh.posted  EQ ip-posted
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

      MESSAGE "Order on BOL is on hold, and BOL will not print."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
       FIND FIRST shipto WHERE
            shipto.rec_key = ic2ndKey AND
            shipto.ship-id = oe-bolh.ship-id
            NO-LOCK NO-ERROR.
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
        report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE "".

    IF lv-run-bol        = "" AND report.key-03 <> "C" THEN lv-run-bol = "YES" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
    
    CREATE tt-post.
    tt-post.row-id = ROWID(oe-bolh).
  END.
  
  v-lines-per-page = lines-per-page.
END.
/*********************************************************************/
PROCEDURE GenerateReport:

   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF v-print-bol AND v-print-fmt <> "SouthPak-XL" THEN
      run output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
END PROCEDURE.
/*********************************************************************/
PROCEDURE output-to-printer:
   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF is-xprint-form THEN DO:
      FILE-INFO:FILE-NAME = list-name.
      RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE
      RUN custom/prntproc.p (list-name,15,"P").
END.
/*********************************************************************/  
PROCEDURE post-bol:

    
   {sa/sa-sls01.i}

  FOR EACH w-ord.
      DELETE w-ord.
  END.

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh AND oe-bolh.posted EQ NO THEN
      FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
          RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).
      END.
    END.
  END.

  

  RUN oe/oe-bolp3.p (v-term).

  /* close transfer order here */
  RUN oe/closchk.p (0).

  FOR EACH w-ord:
      RUN oe/close.p (w-ord.rec-id, YES).  
  END.
END.
/*********************************************************************/
PROCEDURE SetBolForm:
   
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
             v-program = "oe/rep/signbol-csc.p"        /*signbol*/
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
             v-program      = "oe/rep/signbolpremx.p"         /*******/
             is-xprint-form = YES
             lines-per-page = 66.
           WHEN "PremierCX" THEN
          ASSIGN
             v-program      = "oe/rep/signbolpremcx.p"      /******************/
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierPX" THEN
          ASSIGN
             v-program      = "oe/rep/signbolprempx.p"   /*sign bol*/
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "KDWILLSX" THEN
          ASSIGN
             v-program      = "oe/rep/signbolkdwlx.p"   /*sign bol */
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "TrilakesX" OR WHEN "TrilakesLot#" THEN
          ASSIGN
             v-program      = "oe/rep/signboltrilx.p"              /***************/
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
             v-program = "oe/rep/signbolpnp.p"  /*signbol*/
             lines-per-page = 62.
       WHEN "TriLakes" THEN
          ASSIGN
             v-program = "oe/rep/signboltril.p"   /*signbol */
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
             v-program = "oe/rep/signbolbluer.p"    /* sign bol*/
             lines-per-page = 65.
       WHEN "BlueRidg2" THEN
          ASSIGN
             v-program = "oe/rep/signbolbluer2.p"    /* sign bol*/
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
             v-program = "oe/rep/signbolempir.p"      /*signbol*/
             lines-per-page = 54.
       WHEN "Herman" THEN
          ASSIGN
             v-program = "oe/rep/signbolhermn.p"    /*signbol*/
             lines-per-page = 54.
       WHEN "pacific" THEN
          ASSIGN
             v-program      = "oe/rep/signbolpacif.p"    /* signbol***/
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "century" THEN
          ASSIGN
             v-program = "oe/rep/signbolcentx.p"      /* signbol**/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ccc" THEN
          ASSIGN
             v-program = "oe/rep/signbolccc.p"          /* signbol**/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Xprint" THEN
          ASSIGN
             v-program = "oe/rep/signbolxprnt.p"          /* signbol**/
             is-xprint-form = YES
             lines-per-page = 66.
        WHEN "Multicell" THEN
          ASSIGN
             v-program = "oe/rep/bolmcell.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Accordbc" THEN
          ASSIGN
             v-program = "oe/rep/signbolacrdbc.p"        /*sign bol */
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "PackRite" THEN
          ASSIGN
             v-program = "oe/rep/signbolprite.p"        /*sign bol */
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "MidwestX" THEN
          ASSIGN
             v-program = "oe/rep/signbolmidx.p"     /* signbol*/
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "Lamar Pkg" THEN
          ASSIGN
             v-program = "oe/rep/signbollamarpkg.p"     /* sign bol* ?????????????????? */
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Protagon" THEN
          ASSIGN
             v-program = "oe/rep/signbolprotg.p"        /* signbol  */
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CapCityIN" THEN
          ASSIGN
             v-program      = "oe/rep/signbolcapcin.p"        /*signbol**/
             is-xprint-form = YES
             lines-per-page = 66.      
       WHEN "FibreCI" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibci.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/signbolxapc.p" is-xprint-form = YES lines-per-page = 66.     /*signbol*/
       WHEN "P&PX" THEN ASSIGN v-program = "oe/rep/signbolpnpx.p" is-xprint-form = YES lines-per-page = 66.        /*signbol*/
       WHEN "Xprint2" THEN ASSIGN v-program = "oe/rep/signbolxprt2.p" is-xprint-form = YES lines-per-page = 66.        /*signbol*/
       WHEN "NSTOCK" THEN ASSIGN v-program = "oe/rep/signbolnstok.p" is-xprint-form = YES lines-per-page = 80.      /* signbol */
       WHEN "CSCIN" THEN ASSIGN v-program = "oe/rep/signbolcscin.p" is-xprint-form = YES lines-per-page = 66.               /*signbol*/
       WHEN "SouthPak" THEN ASSIGN v-program = "oe/rep/signbolsouth.p" is-xprint-form = YES lines-per-page = 66.                /*signbol*/
       WHEN "SouthPak-XL" THEN ASSIGN v-program = "oe/rep/bolsouth-xl.p" is-xprint-form = YES lines-per-page = 66.           /***************/
       WHEN "MWBox" THEN ASSIGN v-program = "oe/rep/signbolmwbox.p" is-xprint-form = YES lines-per-page = 66.                   /*signbol*/
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/signbolhughs.p" is-xprint-form = YES lines-per-page = 64.                      /*signbol*/
       WHEN "HughesTags" THEN ASSIGN v-program = "oe/rep/signbolhughs.p" is-xprint-form = YES lines-per-page = 70.              /* signbol*/
       WHEN "Inland" THEN ASSIGN v-program = "oe/rep/signbolxinld.p" is-xprint-form = YES lines-per-page = 80.                      /*signbol*/
       WHEN "concepts" THEN ASSIGN v-program = "oe/rep/signbolxcorc.p" is-xprint-form = YES lines-per-page = 80.                       /*signbol*/
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
             v-program = "oe/rep/signbolora2.p"           /*signbol*/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Knight" THEN
          ASSIGN
             v-program = "oe/rep/signbolkni.p"              /*signbol*/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Frankstn" OR WHEN "Mirpkg" THEN
          ASSIGN
             v-program = "oe/rep/signbolfrank.p"        /* sign bol */
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "PPI" THEN
          ASSIGN
             v-program = "oe/rep/signbolppi.p"           /*signbol*/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Indiana" THEN
          ASSIGN
             v-program = "oe/rep/signbolindc.p"   /***********************************/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Ottpkg" THEN
          ASSIGN
             v-program = "oe/rep/signbolottpk.p"      /*signbol*/
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "DEE" THEN
          ASSIGN
             v-program = "oe/rep/signboldee.p"     /*sign bol  */
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ConsBox" THEN
          ASSIGN
             v-program = "oe/rep/signbolcnsbx.p"         /*signbol*/
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
             v-program = "oe/rep/signbolmdwst.p"   /*signbol  */
             lines-per-page = 52.
       WHEN "Intrpack" THEN
          ASSIGN 
             v-program = "oe/rep/bolinter.p"
             lines-per-page = 56.
       WHEN "Dayton" THEN
          ASSIGN
             v-program = "oe/rep/signboldaytn.p"         /*signbol */
             lines-per-page = 57.
       WHEN "Elite" THEN
          ASSIGN
             v-program      = "oe/rep/sign_bolelite.p"  /*sign bol*/
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "ACPI" THEN
          ASSIGN
             v-program      = "oe/rep/signbolacpi.p"        /*signbol*/
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CSC-GA" THEN
          ASSIGN
             v-program      = "oe/rep/signbolcscga.p"      /*signbol */
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Fibrex" THEN
          ASSIGN
             v-program      = "oe/rep/signbolfibrex.p"     /*signbol*/
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "Michcor" THEN
          ASSIGN
             v-program = "oe/rep/signbolmich.p"         /*signbol*/
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Express" THEN
          ASSIGN
             v-program = "oe/rep/signbolexprs.p"        /*signbol*/
             lines-per-page = 62.
       WHEN "Hamilton" THEN
          ASSIGN
             v-program = "oe/rep/bolhamil.p"
             lines-per-page = 44.
       WHEN "Allwest" THEN
          ASSIGN
             v-program      = "oe/rep/signbolallws.p"         /*signbol*/                               
             is-xprint-form = YES
             lines-per-page = 72.      
      
       WHEN "COLOR" THEN  /* gdm - 07140907 */
          ASSIGN v-program =  "oe/rep/signbolcolor.p"           /* for sign bol*/
                 is-xprint-form = YES 
                 lines-per-page = 66.
    
       WHEN "Badger" THEN /* gdm - 08270906 */
          ASSIGN
             v-program = "oe/rep/signbolbadgr.p"                   /*signbol*/                           
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Loylang" THEN  /* gdm - 10200901 */
          ASSIGN
             v-program      = "oe/rep/signbolloyln.p"               /*signbol*/      
             is-xprint-form = YES
             lines-per-page = 66.      

       WHEN "Carded" THEN   /* gdm - 11170903 */
          ASSIGN
             v-program      = "oe/rep/signbolcard.p"                   /*signbol*/
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Metro" THEN
          ASSIGN
             v-program = "oe/rep/signbolmet.p"      /*sign bol **/
             is-xprint-form = YES
             lines-per-page = 78.
      WHEN "MetroTags" THEN
          ASSIGN
             v-program = "oe/rep/signbolmettg.p"    /*signbol*/
             is-xprint-form = YES
             lines-per-page = 64.

       WHEN "PEACHTREE" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/signbolptree.p"   /*signbol*/
                  is-xprint-form = YES 
                  lines-per-page = 80.

       WHEN "PeachTreeBC" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/signbolptreebc.p"   /*signbol*/
                  is-xprint-form = YES 
                  lines-per-page = 80.

       WHEN "Soule" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/signbolsoule.p"    /*signbol*/
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

END.
/*********************************************************************/



/******************************bolform2******************************/
PROCEDURE SetBolForm2:
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
       WHEN "Xprint" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt.p"
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
       WHEN "Xprint2" THEN ASSIGN v-program = "oe/rep/bolxprt2.p" is-xprint-form = YES lines-per-page = 66.
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
END.
