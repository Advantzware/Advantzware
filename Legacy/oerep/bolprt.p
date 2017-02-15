&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
DEF VAR fi_depts-hidden AS LOG NO-UNDO.

/* ***************************  Definitions  ************************** */

/* Variables */
def var list-name         as char no-undo.
def var init-dir          as char no-undo.
DEF VAR v-EDIBOLPost-log AS LOG NO-UNDO.
DEF VAR v-EDIBOLPost-char AS CHAR FORMAT "X(200)" NO-UNDO.

DEF VAR lr-rel-lib AS HANDLE NO-UNDO.

/* Includes */
{methods/defines/hndldefs.i}
/* {methods/prgsecur.i} */
    DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.
DEF VAR g_groups AS CHAR NO-UNDO.
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{oe/rep/oe-lad.i NEW}
{oe/oe-bolpi.i NEW}  
{oe/bolcheck.i NEW}  
{oe/closchk.i NEW}
{custom/formtext.i NEW}
{oerep/r-bolx.i NEW}

ASSIGN
  cocode = gcompany
  locode = gloc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode} /* rstark 05181205 */

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ cocode AND
     sys-ctrl.name    EQ 'EDIBOLPost'
     NO-LOCK NO-ERROR.

IF AVAIL sys-ctrl THEN
   ASSIGN
      v-EDIBOLPost-log = sys-ctrl.log-fld
      v-EDIBOLPost-char = sys-ctrl.char-fld.

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

DEF TEMP-TABLE tt-packslip NO-UNDO
    FIELD b-no AS INT.


def var v-print-fmt     as char NO-UNDO.
DEF VAR v-print-fmt-int AS INT  NO-UNDO.
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
DEF VAR vcDefaultBOLX   AS CHAR NO-UNDO.
DEF VAR v-def-coc-fmt   AS CHAR NO-UNDO.

DEF VAR v-rtn-char      AS CHAR NO-UNDO.
DEF VAR v-rec-found     AS LOG  NO-UNDO.
DEF VAR invstatus-char  AS CHAR NO-UNDO.
DEF VAR invstatus-log   AS LOG  NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR cWinTitle AS CHAR NO-UNDO.
DEF VAR hCallingProcedure AS HANDLE NO-UNDO.
DEF VAR lRunFromWin     AS LOG NO-UNDO.

{custom/xprint.i}

DEF VAR lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */
DEF VAR lv-run-bol        AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.
DEF VAR v-packslip AS CHAR FORMAT "X(100)" NO-UNDO.

/* gdm - 07240906 */
DEF VAR v-tglflg   AS LOG NO-UNDO INIT YES.
DEF NEW SHARED VAR v-ship-inst AS LOG NO-UNDO.
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

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ gcompany AND
     sys-ctrl.name    EQ "PACKSLIP"
     NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = gcompany
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


/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", no, no, "", "", 
                      Output v-rtn-char, output v-rec-found).
invstatus-log = LOGICAL(v-rtn-char).
/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", no, no, "", "", 
                      Output invstatus-char, output v-rec-found).


DEF TEMP-TABLE tt-email NO-UNDO
      FIELD tt-recid AS RECID
      FIELD bol-no LIKE oe-boll.bol-no
      FIELD ord-no LIKE oe-boll.ord-no
      FIELD i-no LIKE itemfg.i-no
      FIELD qty AS INT
      FIELD cust-no AS cha
      INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF STREAM st-email.

{oe/EDIRelBL.i}
DEFINE STREAM ediBOL.

DEFINE TEMP-TABLE ediOutFile NO-UNDO
  FIELD custNo AS CHAR
  FIELD poNo AS CHAR
  FIELD poLine AS INT
  FIELD partNo AS CHAR
  FIELD qty AS DEC
  FIELD lotNo AS CHAR
  FIELD bolDate AS DATE
  FIELD relNo AS INT
  FIELD carrier AS CHAR
  FIELD trailer AS CHAR
  FIELD bolNo AS INT
    INDEX ediOutFile IS PRIMARY custNo bolNo carrier trailer.

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_depts AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 38.4 BY 1.

DEFINE VARIABLE fi_specs AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 38.4 BY 1.

DEFINE VARIABLE lbl_bolcert AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 47.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 23 BY 7.86 NO-UNDO.

DEFINE VARIABLE rd_bolcert AS CHARACTER INITIAL "BOL" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "BOL", "BOL",
"Certificate of Compliance", "Certificate of Compliance"
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.52.

DEFINE VARIABLE tb_barcode AS LOGICAL INITIAL no 
     LABEL "Print Bar Coded Pack List?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ComInvoice AS LOGICAL INITIAL no 
     LABEL "Commercial Invoice (Excel)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE tb_EMailAdvNotice AS LOGICAL INITIAL no 
     LABEL "E-Mail &Advanced Ship Notice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tb_freight-bill AS LOGICAL INITIAL no 
     LABEL "Print Freight Bill / Logo?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_MailBatchMode AS LOGICAL INITIAL no 
     LABEL "Hide E-Mail Dialog Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no 
     LABEL "Print Number Of Pallets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_post-bol AS LOGICAL INITIAL no 
     LABEL "Post BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-barcode AS LOGICAL INITIAL no 
     LABEL "Print Barcode by Part Number?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-binstags AS LOGICAL INITIAL no 
     LABEL "Print Bins/Tags?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL no 
     LABEL "Print Assembled Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-dept AS LOGICAL INITIAL no 
     LABEL "Print Dept Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-shipnote AS LOGICAL INITIAL no 
     LABEL "Print Ship Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-spec AS LOGICAL INITIAL no 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print_ship AS LOGICAL INITIAL no 
     LABEL "Print Shipping Inst?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Bill Of Ladings?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

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
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
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
    message "System control record not found. Update BOL Print format"
    update sys-ctrl.char-fld.
  end.
  assign
   v-print-fmt = sys-ctrl.char-fld
   v-headers   = sys-ctrl.log-fld
   v-print-fmt-int = sys-ctrl.int-fld.

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
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  assign
   v-print-coc = sys-ctrl.log-fld
   v-coc-fmt   = sys-ctrl.char-fld
   v-def-coc-fmt = sys-ctrl.char-fld.

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
   
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.
  
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
  DO TRANSACTION:
     {sys/inc/fgreorder.i}
  END.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN SetBolForm(INPUT v-print-fmt).
  vcDefaultForm = v-print-fmt.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMTX"
      NO-LOCK no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLFMTX"
     sys-ctrl.descrip  = "Freight Bill of lading Format"
     sys-ctrl.char-fld = "FIBREX".
  end.

  vcDefaultBOLX = sys-ctrl.char-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-removeChars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars Procedure 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AdvancedNotice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdvancedNotice Procedure 
PROCEDURE AdvancedNotice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.

  DEFINE BUFFER b1-cust       FOR cust.
  DEFINE BUFFER b1-oe-bolh    FOR oe-bolh.
  DEFINE BUFFER b1-oe-boll    FOR oe-boll.
  DEFINE BUFFER b1-in-house-cust FOR cust.
  DEFINE BUFFER bl-phone      FOR phone.
  DEFINE BUFFER bl-emaildtl   FOR emaildtl.

  DEFINE VARIABLE vcMailMode AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vlSkipRec  AS LOGICAL    NO-UNDO.

  assign
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = "".

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts-hidden = NO THEN
     ASSIGN
        v-print-dept = tb_print-dept
        v-depts = fi_depts.

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE v-s-cust
       AND b1-cust.cust-no LE v-e-cust,
     
      EACH b1-shipto NO-LOCK OF b1-cust,
    
     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  /*in-house cust shiptos*/
  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE v-s-cust
       AND b1-cust.cust-no LE v-e-cust,
     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
      USE-INDEX post,
      EACH b1-in-house-cust WHERE
           b1-in-house-cust.company EQ cocode AND
           b1-in-house-cust.active  EQ "X"
           NO-LOCK,
      FIRST b1-shipto NO-LOCK WHERE
            b1-shipto.company EQ cocode AND
            b1-shipto.cust-no EQ b1-in-house-cust.cust-no AND
            b1-shipto.ship-id EQ b1-oe-bolh.ship-id
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  status default 'Enter data or ESC to end.'.
  RUN usrprnt IN hCallingProcedure.
  /* RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */
  
  SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ASIMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ASIMail Procedure 
PROCEDURE ASIMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  def input param icMailMode  as char no-undo.
  def input param icCustNo    as char no-undo.
  def input param icSubBody   as char no-undo.
/*                                                            */
/*   {custom/asimail2.i  &TYPE           = value (icMailMode) */
/*                       &group-title    = 'r-bolprt.'        */
/*                       &begin_cust     = icCustNo           */
/*                       &END_cust       = icCustNo           */
/*                       &mail-subject   = icSubBody          */
/*                       &mail-body      = icSubBody          */
/*                       &mail-file      = lv-pdf-file}       */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-barcode-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE barcode-proc Procedure 
PROCEDURE barcode-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-ln AS INT NO-UNDO.
   DEF VAR v-ol-uom AS CHAR NO-UNDO.
   DEF VAR v-lot-no AS CHAR NO-UNDO.
   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR lv-text AS CHAR NO-UNDO.
   DEF VAR v-ps1-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
   DEF VAR v-ps2-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
   DEF VAR v-ps3-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
   DEF VAR li AS INT NO-UNDO. /* wfk - defined */
   
   {sys/form/r-top3.f}
   {sys/inc/print1.i}
   {sys/inc/outprint.i value(lines-per-page)}

   VIEW FRAME r-top.
   VIEW FRAME top.
  
   OUTPUT STREAM barcode TO VALUE(v-packslip).    
   PUT STREAM barcode UNFORMATTED
       "BOL#,Release#,CustBillTo,CustBillToName,Shipto,ShiptoName,Carrier,"
       "CustPart#,FGItem#,FGName,Desc1,Desc2,PO#,LN#,QtyShipped,Tag#,Lot#," +
       "PS11,PS12,PS13,PS14,PS15,PS16,PS17,PS18," +
       "PS21,PS22,PS23,PS24,PS25,PS26,PS27,PS28," +
       "PS31,PS32,PS33,PS34,PS35,PS36,PS37,PS38," +
       "Barcode,OrdLnUOM,BOLDate,Units,Qty/Unit,PartialCount," +
       "P/C,TotalPallets,TotalWeight,Order,Job#,ProdDate,RecDateByJob," SKIP.

   SESSION:SET-WAIT-STATE ("general").

   FOR EACH tt-packslip,
       FIRST oe-bolh WHERE 
       oe-bolh.b-no EQ tt-packslip.b-no
       NO-LOCK,
       EACH oe-boll WHERE
            oe-boll.company EQ oe-bolh.company AND
            oe-boll.b-no = oe-bolh.b-no
            NO-LOCK:
  
        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(STRING(oe-bolh.bol-no,">>>>>>>9"))  "~","
            "~"" removeChars(STRING(oe-bolh.release#)) "~","
            "~"" removeChars(oe-bolh.cust-no) "~",".

        FIND FIRST cust WHERE
             cust.company EQ oe-bolh.company AND
             cust.cust-no EQ oe-bolh.cust-no
             NO-LOCK NO-ERROR.

        IF AVAIL cust THEN
        DO:
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(cust.name) "~",".
           RELEASE cust.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        PUT STREAM barcode UNFORMATTED 
            "~"" STRING(removeChars(oe-bolh.ship-id))  "~",".

        FIND FIRST shipto WHERE
             shipto.company EQ oe-bolh.company AND
             shipto.cust-no EQ oe-bolh.cust-no AND
             shipto.ship-id EQ oe-bolh.ship-id
             NO-LOCK NO-ERROR.

        IF AVAIL shipto THEN
        DO:
           PUT STREAM barcode UNFORMATTED 
               "~"" removeChars(shipto.ship-name)  "~",".
           RELEASE shipto.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(oe-bolh.carrier)  "~",".

        FIND FIRST oe-ordl WHERE
             oe-ordl.company EQ oe-boll.company AND
             oe-ordl.ord-no EQ oe-boll.ord-no AND
             oe-ordl.LINE EQ oe-boll.LINE
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN
        DO:
           PUT STREAM barcode UNFORMATTED 
               "~"" removeChars(oe-ordl.part-no) "~","
               "~"" removeChars(oe-ordl.i-no) "~","
               "~"" removeChars(oe-ordl.i-name) "~","
               "~"" removeChars(oe-ordl.part-dscr1) "~","
               "~"" removeChars(oe-ordl.part-dscr2) "~",".

           ASSIGN
              v-i-no = oe-ordl.i-no
              v-ln = oe-ordl.e-num
              v-ol-uom = oe-ordl.pr-uom.

           RELEASE oe-ordl.
        END.
        ELSE
        DO:
           ASSIGN
              v-i-no = ""
              v-ln = 0
              v-ol-uom = "".

           PUT STREAM barcode UNFORMATTED 
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~",".
        END.

/*         FIND FIRST reftable WHERE                      */
/*              reftable.reftable EQ "oe-boll.lot-no" AND */
/*              reftable.rec_key = STRING(RECID(oe-boll)) */
/*              USE-INDEX rec_key                         */
/*              NO-LOCK NO-ERROR.                         */
/*                                                        */
/*         IF AVAIL reftable THEN                         */
/*         DO:                                            */
/*            v-lot-no = reftable.CODE.                   */
/*            RELEASE reftable.                           */
/*         END.                                           */
/*         ELSE                                           */
/*            v-lot-no = "".                              */
        v-lot-no = oe-boll.lot-no.

        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(oe-boll.po-no) "~","
            "~"" STRING(v-ln,">>>") "~","
            "~"" removeChars(STRING(oe-boll.qty)) "~","
            "~"" removeChars(oe-boll.tag) "~","
            "~"" removeChars(v-lot-no) "~",".

        EMPTY TEMP-TABLE tt-formtext.
               
        ASSIGN
           lv-text = ""
           v-ps1-note = ""
           v-ps2-note = ""
           v-ps3-note = "".

        find first itemfg where
             itemfg.company eq oe-boll.company AND
             itemfg.i-no    eq v-i-no
             no-lock no-error.

        if avail itemfg THEN DO:        
           
           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS1"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps1-note[i] = tt-formtext.tt-text.      
           END.

           EMPTY TEMP-TABLE tt-formtext.
           lv-text = "".

           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS2"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps2-note[i] = tt-formtext.tt-text.      
           END.

           EMPTY TEMP-TABLE tt-formtext.
           lv-text = "".

           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS3"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps3-note[i] = tt-formtext.tt-text.      
           END.
        END.  /* avail itemfg */

        PUT STREAM barcode UNFORMATTED
            "~"" removeChars(v-ps1-note[1]) "~","
            "~"" removeChars(v-ps1-note[2]) "~","
            "~"" removeChars(v-ps1-note[3]) "~","
            "~"" removeChars(v-ps1-note[4]) "~","
            "~"" removeChars(v-ps1-note[5]) "~","
            "~"" removeChars(v-ps1-note[6]) "~","
            "~"" removeChars(v-ps1-note[7]) "~","
            "~"" removeChars(v-ps1-note[8]) "~","
            "~"" removeChars(v-ps2-note[1]) "~","
            "~"" removeChars(v-ps2-note[2]) "~","
            "~"" removeChars(v-ps2-note[3]) "~","
            "~"" removeChars(v-ps2-note[4]) "~","
            "~"" removeChars(v-ps2-note[5]) "~","
            "~"" removeChars(v-ps2-note[6]) "~","
            "~"" removeChars(v-ps2-note[7]) "~","
            "~"" removeChars(v-ps2-note[8]) "~","
            "~"" removeChars(v-ps3-note[1]) "~","
            "~"" removeChars(v-ps3-note[2]) "~","
            "~"" removeChars(v-ps3-note[3]) "~","
            "~"" removeChars(v-ps3-note[4]) "~","
            "~"" removeChars(v-ps3-note[5]) "~","
            "~"" removeChars(v-ps3-note[6]) "~","
            "~"" removeChars(v-ps3-note[7]) "~","
            "~"" removeChars(v-ps3-note[8]) "~","
            "~"" "(k)" + removeChars(oe-boll.po-no) + "(+)" +
                 removeChars(STRING(v-ln)) + "(+)" +
                 removeChars(STRING(oe-boll.qty)) "~","
            "~"" removeChars(v-ol-uom) "~","
            "~"" removeChars(STRING(oe-bolh.bol-date)) "~","
            "~"" removeChars(STRING(oe-boll.cases)) "~","
            "~"" removeChars(STRING(oe-boll.qty-case)) "~","
            "~"" removeChars(STRING(oe-boll.partial)) "~","
            "~"" removeChars(STRING(oe-boll.p-c,"C/P")) "~","
            "~"" removeChars(STRING(oe-bolh.tot-pallets)) "~","
            "~"" removeChars(STRING(oe-bolh.tot-wt)) "~","
            "~"" removeChars(STRING(oe-boll.ord-no)) "~","
            "~"" removeChars(TRIM(oe-boll.job-no)) + "-" + removeChars(STRING(oe-boll.job-no2,"99")) "~",".

        FIND FIRST oe-ord WHERE
             oe-ord.company EQ oe-boll.company AND
             oe-ord.ord-no EQ oe-boll.ord-no
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
        DO:
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(STRING(oe-ord.prod-date)) "~",".
           RELEASE oe-ord.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        RELEASE fg-rcpth.

        IF oe-boll.job-no NE "" THEN
        DO:
           FOR EACH fg-rcpth WHERE
               fg-rcpth.company   EQ oe-boll.company AND
               fg-rcpth.job-no    EQ oe-boll.job-no AND
               fg-rcpth.job-no2   EQ oe-boll.job-no2 AND
               fg-rcpth.i-no      EQ oe-boll.i-no AND
               fg-rcpth.rita-code EQ "R"
               NO-LOCK
               USE-INDEX job
               BY fg-rcpth.trans-date
               BY fg-rcpth.r-no:
               LEAVE.
           END.
        END.

        IF AVAIL fg-rcpth THEN
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(STRING(fg-rcpth.trans-date)) "~",".
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "?" "~",".

        PUT STREAM barcode UNFORMATTED SKIP.
   END.

   OUTPUT STREAM barcode CLOSE.
   SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-build-work) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-work Procedure 
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

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
      
      IF begin_bol# EQ END_bol# THEN
          RUN showMessage IN hCallingProcedure (INPUT "Order Hold").


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
    IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no 
         NO-LOCK NO-ERROR.

    ASSIGN
       oe-bolh.printed = YES
       vcBOLNums       = vcBOLNums + '-' + STRING (oe-bolh.bol-no)
       vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

    IF vcBOLNums MATCHES '*-*' THEN 
       vcBOLNums = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).

    IF NOT CAN-FIND(FIRST report WHERE
       report.term-id = v-term-id AND
       report.rec-id  = RECID(oe-bolh)) THEN
       DO:
          CREATE report.
          
          ASSIGN 
              report.term-id  = v-term-id
              report.key-01   = oe-bolh.cust-no
              report.key-02   = oe-bolh.ship-id
              report.rec-id   = RECID(oe-bolh)
              report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
              
              report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                                ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                                ELSE "N" /*BOL only*/ 
              report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE "".
       END.
    
    status default 'Now Processing BOL: ' + string (oe-bolh.bol-no) + '....'.
    
    lv-run-bol        = IF lv-run-bol = "" AND report.key-04 = "FIBRECI" THEN  "NO" ELSE  "Yes" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
    
    IF NOT CAN-FIND(FIRST tt-post WHERE tt-post.row-id = ROWID(oe-bolh)) THEN
    DO:
       CREATE tt-post.
       tt-post.row-id = ROWID(oe-bolh).
    END.

    IF tb_barcode AND
       NOT CAN-FIND(FIRST tt-packslip where
       tt-packslip.b-no = oe-bolh.b-no) THEN
       DO:
          CREATE tt-packslip.
          tt-packslip.b-no = oe-bolh.b-no.
          RELEASE tt-packslip.
       END.
  END.
  
  v-lines-per-page = lines-per-page.

  status default ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-bol-security) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-bol-security Procedure 
PROCEDURE check-bol-security :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-prg AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-senflg AS LOG NO-UNDO INIT YES.

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO INIT "ASI".
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

FIND b-prgrms WHERE b-prgrms.prgmname = ip-prg NO-LOCK NO-ERROR.
IF AVAILABLE b-prgrms THEN
DO:

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
    IF NOT CAN-DO(b-prgrms.can_run,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_create,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_delete,ENTRY(num-groups,g_groups)) THEN
       NEXT.

    group-ok = yes.
    LEAVE.
  END.

  IF NOT CAN-DO(b-prgrms.can_run,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_update,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_create,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_delete,USERID("NOSWEAT")) AND NOT group-ok THEN
  DO:
    /*MESSAGE 
        "User access to POST BOL this Program Denied - Contact Systems Manager" 
       VIEW-AS ALERT-BOX ERROR.
    access-close = YES.    /* used later in methods/template/windows.i - local-initialize procedure */
    */
    ASSIGN op-senflg  = NO.
  END.

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CommercialInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommercialInvoice Procedure 
PROCEDURE CommercialInvoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE ("general").
  
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

      RUN get-fibreci IN hCallingProcedure (INPUT tt-ci-form.form-name,
                                            INPUT tt-ci-form.total-pallets).
  END.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-reorder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reorder Procedure 
PROCEDURE create-reorder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-oeboll-rowid AS ROWID NO-UNDO.
 DEF BUFFER bf-oeboll FOR oe-boll.

 DEF VAR v-qty-onh AS INT NO-UNDO.
 DEF VAR v-qty-avail AS INT NO-UNDO.
 DEF VAR v-reord-qty AS INT NO-UNDO.

 FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
 FIND itemfg WHERE itemfg.company = cocode AND
                   itemfg.i-no = bf-oeboll.i-no NO-LOCK.

 v-qty-onh = 0.
 FOR EACH fg-bin FIELDS(qty)
         WHERE fg-bin.company EQ itemfg.company
           AND fg-bin.i-no    EQ itemfg.i-no
           /*AND fg-bin.loc     GE begin_whse
           AND fg-bin.loc     LE end_whse*/ NO-LOCK:
      v-qty-onh = v-qty-onh + fg-bin.qty.
 END.

 ASSIGN
     v-qty-avail = v-qty-onh /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.
 
 if itemfg.ord-level gt v-qty-avail then do:
    v-reord-qty = itemfg.ord-level - v-qty-avail.

    if v-reord-qty lt itemfg.ord-min and
       itemfg.ord-min ne 0 then 
       v-reord-qty = itemfg.ord-min.

    if v-reord-qty gt itemfg.ord-max and
       itemfg.ord-max ne 0 then 
       v-reord-qty = itemfg.ord-max.
 end.
 else v-reord-qty = 0.

 IF v-reord-qty > 0 THEN DO:
    CREATE tt-email.
    ASSIGN tt-email.bol-no = bf-oeboll.bol-no
           tt-email.ord-no = bf-oeboll.ord-no
           tt-email.i-no = bf-oeboll.i-no
           tt-email.qty = v-reord-qty
           tt-email.cust-no = oe-bolh.cust-no.
                                    
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-email-reorderitems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-reorderitems Procedure 
PROCEDURE email-reorderitems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
  DEF VAR v-qty-onh AS INT NO-UNDO.
  DEF VAR v-qty-avail AS INT NO-UNDO.
  DEF VAR v-qty-alloc AS INT NO-UNDO.
  DEF VAR v-qty-onOrder AS INT NO-UNDO.

  FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.
  IF AVAIL users AND users.user_program[2] NE "" THEN v-dir = users.user_program[2] + "\".
  ELSE v-dir = "c:\tmp\".

  FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = cocode
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no BY tt-email.i-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "***** Reorder Point Item from BOL Posting *****" SKIP
                              "BOL#     Order#       FG Item#      ReOrder Qty     Avail Qty  On Hand Qty On Order Qty" SKIP
                              "======== ========== =============== ============ ============ ============ ============" SKIP.
       END.
       IF FIRST-OF(tt-email.i-no) THEN DO:
          /*v-qty-onh = 0.
          FOR EACH fg-bin WHERE fg-bin.company EQ cocode
                         AND fg-bin.i-no    EQ tt-email.i-no
                 /*AND fg-bin.loc     GE begin_whse
                 AND fg-bin.loc     LE end_whse*/  NO-LOCK:
              v-qty-onh = v-qty-onh + fg-bin.qty.
         END.
         */
         FIND itemfg WHERE itemfg.company = cocode
                       AND itemfg.i-no = tt-email.i-no NO-LOCK NO-ERROR.
         IF AVAIL itemfg THEN ASSIGN v-qty-onh = itemfg.q-onh
                                     v-qty-onOrder = itemfg.q-ono
                                     v-qty-alloc = itemfg.q-alloc.
         ELSE ASSIGN v-qty-onh = 0
                     v-qty-onOrder = 0
                     v-qty-alloc = 0.
         {sys/inc/oereordr.i}

         v-qty-avail = v-qty-onh +
                       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE v-qty-onOrder) -
                       v-qty-alloc.

       END.
       
       PUT STREAM st-email UNFORMATTED
                 STRING(tt-email.bol-no) FORM "x(9)"
                 string(tt-email.ord-no) FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 " " v-qty-avail  FORM "->>>,>>>,>>9"
                 " " v-qty-onh FORM "->>>,>>>,>>9"
                 " " v-qty-onOrder FORM "->>>,>>>,>>9"
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &prgmName='r-bolpst.' &emailList=ls-to-list}   
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailbody = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exception-rpt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt Procedure 
PROCEDURE exception-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR td-full-tag AS LOG NO-UNDO.
DEF VAR tran-date AS DATE NO-UNDO.
DEF VAR tran-period AS INT NO-UNDO.
DEF VAR vtag AS INT NO-UNDO.
DEF VAR vtag2 AS INT NO-UNDO.
DEF VAR dis-tag AS CHAR NO-UNDO.
{sys/form/r-top3w.f}

     ASSIGN td-full-tag = YES.

    RUN promptUser IN hCallingProcedure (INPUT "Full Tag", OUTPUT td-full-tag).

  FORM HEADER SKIP(1) WITH FRAME r-top.
 
  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "BOL - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}
 
   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}
  
  display with frame r-top.
  
  for each w-except,

      first oe-bolh
      where oe-bolh.company eq cocode
        and oe-bolh.bol-no  eq w-except.bol-no
      no-lock

     break by w-except.bol-no
           by w-except.ord-no
           by w-except.rel-no
           by w-except.b-ord-no:

    if first-of(w-except.bol-no) then do:
      display oe-bolh.bol-date FORMAT "99/99/9999" COLUMN-LABEL "Date"
              oe-bolh.bol-no format ">>>>>>>>" COLUMN-LABEL "   BOL #"
              oe-bolh.carrier FORMAT "X(5)" COLUMN-LABEL "Carrier"
              oe-bolh.trailer FORMAT "X(20)" COLUMN-LABEL "Trailer"
              oe-bolh.freight format "->>>,>>9.99" COLUMN-LABEL "    Freight"
              oe-bolh.cwt     COLUMN-LABEL "  Rate"
              oe-bolh.tot-wt  format "->>>,>>9" COLUMN-LABEL "   Tot WT"
              oe-bolh.cust-no COLUMN-LABEL "Cust#"
              oe-bolh.ship-id COLUMN-LABEL "Ship#"
              oe-bolh.deleted format "*DELETED*/ " COLUMN-LABEL "Deleted"
              SKIP(1)
          with frame bolh2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 150.
      down with frame bolh2.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-except.i-no
        no-lock no-error.

       vtag = 0.
       vtag2 = 0.
       vtag = LENGTH(w-except.tag).
       vtag2 = vtag - 5 .

      IF NOT td-full-tag AND vtag <> 0 THEN ASSIGN  dis-tag =  SUBSTR(w-except.tag,vtag2,6) .
      ELSE ASSIGN dis-tag  = w-except.tag .

    display SPACE(5)
            w-except.i-no  COLUMN-LABEL "Item #"   
            dis-tag COLUMN-LABEL "Tag" FORMAT "X(22)"
            itemfg.i-name  FORMAT "X(20)" when avail itemfg COLUMN-LABEL "Item Name"
            w-except.po-no COLUMN-LABEL "P.O. #"    
            w-except.ord-no COLUMN-LABEL "  Ord#"   
            STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") COLUMN-LABEL "Rel.#"    
            w-except.loc COLUMN-LABEL "Whse."
            w-except.loc-bin COLUMN-LABEL "Bin Loc"   
            
            w-except.cases format "->>>,>>9"   COLUMN-LABEL "   Cases"
            w-except.qty-case format "->>>,>>9" COLUMN-LABEL "Qty/Case" 
            w-except.partial format "->>>,>>9"  COLUMN-LABEL " Partial"
            w-except.weight format "->>>,>>9"   COLUMN-LABEL "  Weight"
        with frame boll2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 165.
    down with frame boll2.
    
    put skip(1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenerateMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail Procedure 
PROCEDURE GenerateMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM ic1stKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM icType   AS CHAR NO-UNDO.

  /* XPrint */
  IF is-xprint-form THEN DO:
    
    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    IF v-print-fmt = "SouthPak-XL" THEN
       ASSIGN lv-pdf-file = init-dir + "\" + string(b1-oe-bolh.bol-no) + ".pdf".

    ELSE DO:    
      lv-pdf-file = lv-pdf-file + vcBOLNums + '.pdf'.
      IF list-name NE ? AND list-name NE '' THEN
         RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
      ELSE
         RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
    END.
    
    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-1 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-1 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-1 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-1 (ic2ndKey, 'ShipTo'). 

    END CASE.
  END.
  
  /* Not XPrint */
  ELSE DO:

    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-2 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-2 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-2 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-2 (ic2ndKey, 'ShipTo'). 

    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GenerateReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport Procedure 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF (v-print-bol AND v-print-fmt <> "SouthPak-XL") OR
      (NOT v-print-bol AND v-coc-fmt <> "Unipak-XL" AND v-coc-fmt <> "ACPI" AND v-coc-fmt <> "CCC") THEN
      case rd-dest:
         when 1 then run output-to-printer IN hCallingProcedure (INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto, INPUT list-name).
         when 2 then run output-to-screen IN hCallingProcedure (INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto, INPUT list-name).
         when 3 then run output-to-file IN hCallingProcedure (INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto, INPUT list-name).
         when 4 then run output-to-fax IN hCallingProcedure (INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto, INPUT list-name).
         WHEN 6 THEN RUN output-to-port IN hCallingProcedure (INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto, INPUT list-name).
      end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getListName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getListName Procedure 
PROCEDURE getListName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opcListName AS CHAR NO-UNDO.
opcLIstName = list-name.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrintBarTag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintBarTag Procedure 
PROCEDURE getPrintBarTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM opPrintBarTag AS LOG .

  opPrintBarTag = tb_print-barcode.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrintBinsTags) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintBinsTags Procedure 
PROCEDURE getPrintBinsTags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM opPrintBinsTags AS LOG .

  opPrintBinsTags = tb_print-binstags.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrintSpecNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintSpecNotes Procedure 
PROCEDURE getPrintSpecNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM opPrintSpec AS LOG .
 DEF OUTPUT PARAM opSpecList AS CHAR.

  opPrintSpec = tb_print-spec.
  opSpecList = fi_specs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-main-process) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main-process Procedure 
PROCEDURE main-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* security check need {methods/prgsecur.i} in definition section */



  IF VALID-HANDLE(hCallingProcedure) THEN
    RUN new-bol# in hCallingProcedure.
  ELSE
      MESSAGE "Calling Procedure not found"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /* gdm - 07240906 */
  ASSIGN v-tglflg = YES.

  RUN check-bol-security (INPUT "r-bolpst.",
                          OUTPUT v-tglflg).

  IF v-tglflg THEN
     RUN check-bol-security (INPUT "postbol.",
                             OUTPUT v-tglflg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-passScreenValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE passScreenValues Procedure 
PROCEDURE passScreenValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER vCallProc           AS   HANDLE             NO-UNDO.
DEF INPUT PARAMETER vRunFromWin         AS   LOG                NO-UNDO.
DEF INPUT PARAMETER vbegin_cust         LIKE begin_cust         NO-UNDO.
DEF INPUT PARAMETER vend_cust           LIKE end_cust           NO-UNDO.
DEF INPUT PARAMETER vbegin_bol#         LIKE begin_bol#         NO-UNDO.
DEF INPUT PARAMETER vend_bol#           LIKE end_bol#           NO-UNDO.
DEF INPUT PARAMETER vbegin_ord#         LIKE begin_ord#         NO-UNDO.
DEF INPUT PARAMETER vend_ord#           LIKE end_ord#           NO-UNDO.
DEF INPUT PARAMETER vtb_reprint         LIKE tb_reprint         NO-UNDO.
DEF INPUT PARAMETER vtb_pallet          LIKE tb_pallet          NO-UNDO.
DEF INPUT PARAMETER vtb_posted          LIKE tb_posted          NO-UNDO.
DEF INPUT PARAMETER vtb_print-component LIKE tb_print-component NO-UNDO.
DEF INPUT PARAMETER vtb_print-shipnote  LIKE tb_print-shipnote  NO-UNDO.
DEF INPUT PARAMETER vtb_barcode         LIKE tb_barcode         NO-UNDO.
DEF INPUT PARAMETER vfi_depts           LIKE fi_depts           NO-UNDO.
DEF INPUT PARAMETER vtb_print-dept      LIKE tb_print-dept      NO-UNDO.
DEF INPUT PARAMETER vtb_print_ship      LIKE tb_print_ship      NO-UNDO.
DEF INPUT PARAMETER vtb_print-barcode   LIKE tb_print-barcode   NO-UNDO.
DEF INPUT PARAMETER vtb_print-binstags  LIKE tb_print-binstags  NO-UNDO.
DEF INPUT PARAMETER vfi_specs           LIKE fi_specs           NO-UNDO.
DEF INPUT PARAMETER vtb_print-spec      LIKE tb_print-spec      NO-UNDO.
DEF INPUT PARAMETER vlbl_bolcert        LIKE lbl_bolcert        NO-UNDO.
DEF INPUT PARAMETER vrd_bolcert         LIKE rd_bolcert         NO-UNDO.
DEF INPUT PARAMETER vtb_EMailAdvNotice  LIKE tb_EMailAdvNotice  NO-UNDO.
DEF INPUT PARAMETER vrd-dest            LIKE rd-dest            NO-UNDO.
DEF INPUT PARAMETER vtb_MailBatchMode   LIKE tb_MailBatchMode   NO-UNDO.
DEF INPUT PARAMETER vtb_ComInvoice      LIKE tb_ComInvoice      NO-UNDO.
DEF INPUT PARAMETER vtb_freight-bill    LIKE tb_freight-bill    NO-UNDO.
DEF INPUT PARAMETER vlv-ornt            LIKE lv-ornt            NO-UNDO.
DEF INPUT PARAMETER vlines-per-page     LIKE lines-per-page     NO-UNDO.
DEF INPUT PARAMETER vlv-font-no         LIKE lv-font-no         NO-UNDO.
DEF INPUT PARAMETER vlv-font-name       LIKE lv-font-name       NO-UNDO.
DEF INPUT PARAMETER vtb_post-bol        LIKE tb_post-bol        NO-UNDO.
DEF INPUT PARAMETER vtd-show-parm       LIKE td-show-parm       NO-UNDO.
DEF INPUT PARAMETER vfi_depts-hidden    LIKE fi_depts-hidden    NO-UNDO.
DEF INPUT PARAMETER vWinTitle           AS   CHAR               NO-UNDO.

  ASSIGN hCallingProcedure  = vCallProc
         lRunFromWin        = vRunFromWin
         begin_cust         = vbegin_cust
         end_cust           = vend_cust
         begin_bol#         = vbegin_bol#
         end_bol#           = vend_bol#
         begin_ord#         = vbegin_ord#
         end_ord#           = vend_ord#
         tb_reprint         = vtb_reprint
         tb_pallet          = vtb_pallet
         tb_posted          = vtb_posted
         tb_print-component = vtb_print-component
         tb_print-shipnote  = vtb_print-shipnote
         tb_barcode         = vtb_barcode
         fi_depts           = vfi_depts
         tb_print-dept      = vtb_print-dept
         tb_print_ship      = vtb_print_ship
         tb_print-barcode   = vtb_print-barcode
         tb_print-binstags  = vtb_print-binstags
         fi_specs           = vfi_specs
         tb_print-spec      = vtb_print-spec
         lbl_bolcert        = vlbl_bolcert
         rd_bolcert         = vrd_bolcert
         tb_EMailAdvNotice  = vtb_EMailAdvNotice
         rd-dest            = vrd-dest
         tb_MailBatchMode   = vtb_MailBatchMode
         tb_ComInvoice      = vtb_ComInvoice
         tb_freight-bill    = vtb_freight-bill
         lv-ornt            = vlv-ornt
         lines-per-page     = vlines-per-page
         lv-font-no         = vlv-font-no
         lv-font-name       = vlv-font-name
         tb_post-bol        = vtb_post-bol
         td-show-parm       = vtd-show-parm 
         fi_depts-hidden    = vfi_depts-hidden
         cWinTitle          = vWinTitle.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-post-bol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bol Procedure 
PROCEDURE post-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lotNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE trailerNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE outFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE d-out   AS DECIMAL NO-UNDO.
  DEF BUFFER bf-oe-ord FOR oe-ord.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  {sa/sa-sls01.i}

  FOR EACH w-ord. DELETE w-ord. END.

  EMPTY TEMP-TABLE ediOutFile.

   RUN-PROC = "sbo/oerel-recalc-act.p".
/*    {methods/smartrun.i} */
   RUN VALUE(run-proc) PERSISTENT SET phandle NO-ERROR.
   lr-rel-lib = phandle.

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh AND oe-bolh.posted EQ NO THEN DO:
          
        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
          EACH oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ oe-boll.company
               AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:
          FOR EACH oe-rel 
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                  AND oe-rel.stat = "P"
                  AND oe-rel.link-no GT 0 
                  AND oe-rel.rel-no GT 0:
    
                  
                IF AVAIL oe-rel /*AND avail(tt-rels) */ AND VALID-HANDLE(lr-rel-lib) THEN 
                   RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).
    
          END.
        END.

        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
          
          RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).
          FIND cust WHERE cust.company = oe-bolh.company
                      AND cust.cust-no = oe-bolh.cust-no NO-LOCK NO-ERROR.
          IF fgreorder-log AND cust.ACTIVE = "E" 
            THEN RUN create-reorder (ROWID(oe-boll)) .  
          

          IF v-EDIBOLPost-log THEN
          DO:
             &IF DEFINED(useLotNo) NE 0 &THEN lotNo = oe-boll.lot-no. &ELSE
             lotNo = STRING(INT(oe-bolh.ship-id),'99') NO-ERROR.
             IF ERROR-STATUS:ERROR OR INT(oe-bolh.ship-id) > 99 THEN lotNo = ''. ELSE
             lotNo = 'MG' + STRING(INT(oe-bolh.ship-id),'99') + STRING(DAY(oe-boll.bol-date),'99').
             &ENDIF
          
             FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ oe-boll.company
                    AND oe-ordl.ord-no EQ oe-boll.ord-no
                    AND oe-ordl.line EQ oe-boll.line NO-ERROR.
          
             CREATE ediOutFile.
             ASSIGN
               ediOutFile.bolNo = oe-boll.bol-no
               ediOutFile.custNo = oe-bolh.cust-no
               ediOutFile.poNo = oe-boll.po-no
               ediOutFile.poLine = IF AVAIL(oe-ordl) THEN oe-ordl.e-num ELSE 0
               ediOutFile.partNo = IF AVAIL(oe-ordl) THEN oe-ordl.part-no ELSE ''
               ediOutFile.qty = oe-boll.qty
               ediOutFile.lotNo = lotNo
               ediOutFile.bolDate = oe-boll.bol-date
               ediOutFile.relNo = oe-boll.r-no
               ediOutFile.carrier = oe-bolh.carrier
               ediOutFile.trailer = IF oe-bolh.trailer EQ '' THEN '001'
                                    ELSE oe-bolh.trailer.
             RELEASE ediOutFile.
             RELEASE oe-ordl.
          END.
        END. /* each oe-boll */
      END.
    END.
  END.
  
  RUN oe/oe-bolp3.p (v-term).

  /* close transfer order here */
  RUN oe/closchk.p (0).

  /* wfk - 5/4/12 - run cleanup routine after posting instead of */
  /* fixing the actual problems                                  */

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh THEN DO:

        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
                         
          FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-boll.company
              AND oe-ordl.ord-no EQ oe-boll.ord-no
              AND oe-ordl.line EQ oe-boll.line 
          NO-ERROR.
           RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).        

        END.
      END.
    END.
  END.
  
  FOR EACH w-ord:
      RUN oe/close.p (w-ord.rec-id, YES).  
      /*
          FIND bf-oe-ord WHERE RECID(bf-oe-ord) EQ w-ord.rec-id NO-ERROR.
      FOR EACH bf-oe-ordl WHERE bf-oe-ordl.company = bf-oe-ord.company
                         AND bf-oe-ordl.ord-no  = bf-oe-ord.ord-no
                       NO-LOCK.
        /* {util/tmsg.i ""CleanOrdOn"" bf-oe-ordl.ord-no
             bf-oe-ordl.i-no } */
         RUN oe/cleanrel.p (INPUT ROWID(bf-oe-ordl)).
      END.
      */
  END.
  
  IF v-EDIBOLPost-log THEN
  FOR EACH sys-ctrl-shipto NO-LOCK
      WHERE sys-ctrl-shipto.company EQ cocode
        AND sys-ctrl-shipto.name EQ 'EDIBOLPost'
        AND sys-ctrl-shipto.cust-vend EQ YES
        AND sys-ctrl-shipto.log-fld EQ YES:
    CASE sys-ctrl-shipto.char-fld:
      WHEN 'RHEEM' THEN
      FOR EACH ediOutFile WHERE ediOutFile.custNo EQ sys-ctrl-shipto.cust-vend-no
          BREAK BY ediOutFile.bolNo
                BY ediOutFile.carrier
                BY ediOutFile.trailer:
        IF FIRST-OF(ediOutFile.trailer) THEN DO:
          outFile = v-EDIBOLPost-char + '/' +
                    sys-ctrl-shipto.cust-vend-no + '/' +
                    STRING(ediOutFile.bolNo) + '-' +
                    ediOutFile.carrier + '-' +
                    ediOutFile.trailer + '.csv'.
          OUTPUT STREAM ediBOL TO VALUE(outFile) APPEND.
        END. /* first-of */
        PUT STREAM ediBOL UNFORMATTED
          ediOutFile.poNo ','
          ediOutFile.poLine ','
          ediOutFile.partNo ','
          ediOutFile.partNo ','
          ediOutFile.qty ','
          ediOutFile.lotNo ','
          ediOutFile.bolDate ','
          ediOutFile.bolNo ','
          ediOutFile.carrier ','
          ediOutFile.trailer SKIP.
        IF LAST-OF(ediOutFile.trailer) THEN DO:
          PUT STREAM ediBOL UNFORMATTED
              STRING(ediOutFile.relNo)  + '-' +
                     ediOutFile.carrier + '-' +
                     ediOutFile.trailer + '.csv: END' SKIP.
          OUTPUT STREAM ediBOL CLOSE.
        END. /* last-of */
      END. /* each edioutfile, when rheem */
    END CASE.
  END. /* each sys-ctrl-shipto */
 
  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processOK Procedure 
PROCEDURE processOK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR retCode     AS INT NO-UNDO.  
   DEF VAR ll          AS LOG NO-UNDO.
   DEF VAR v-format-str AS CHAR NO-UNDO.
   DEF VAR lv-exception AS LOG NO-UNDO.
   /* Initilize temp-table */
   EMPTY TEMP-TABLE tt-filelist.
   EMPTY TEMP-TABLE tt-post.
  
   IF tb_barcode THEN
      EMPTY TEMP-TABLE tt-packslip.
  

   IF invstatus-char EQ "One Bol Only" THEN
       ASSIGN END_bol# = begin_bol#
              .

   IF rd_bolcert EQ "BOL" THEN
   DO:
      IF NOT tb_freight-bill THEN
         v-format-str = "BOLFMT".
      ELSE
      DO:
         v-format-str = "BOLFMTX".
         RUN oerep/r-bolx.w.
      END.
   END.
   ELSE
      v-format-str = "BOLCERT".

  
   CASE rd-dest:
      WHEN 1 THEN ASSIGN LvOutputSelection = "Printer".
      WHEN 2 THEN ASSIGN LvOutputSelection = "Screen". 
      WHEN 3 THEN ASSIGN LvOutputSelection = "File". 
      WHEN 4 THEN ASSIGN LvOutputSelection = "Fax". 
      WHEN 5 THEN ASSIGN LvOutputSelection = "Email".
      WHEN 6 THEN ASSIGN LvOutputSelection = "Port".
   END CASE.
       
   IF NOT rd-dest = 5 THEN
   DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company = cocode AND
         sys-ctrl-shipto.NAME = v-format-str) THEN
         DO:
            IF CAN-FIND(FIRST b-oe-bolh WHERE
               b-oe-bolh.company EQ cocode AND
               b-oe-bolh.bol-no  GE begin_bol# AND
               b-oe-bolh.bol-no  LE end_bol# AND
               b-oe-bolh.cust-no GE begin_cust AND
               b-oe-bolh.cust-no LE end_cust AND
               b-oe-bolh.printed EQ tb_reprint AND
               b-oe-bolh.posted  EQ tb_posted AND
               CAN-FIND (FIRST oe-boll
                         WHERE oe-boll.company EQ b-oe-bolh.company
                           AND oe-boll.b-no    EQ b-oe-bolh.b-no
                           AND oe-boll.ord-no  GE begin_ord#
                           AND oe-boll.ord-no  LE end_ord#)) THEN
               FOR EACH b-oe-bolh WHERE
                   b-oe-bolh.company EQ cocode AND
                   b-oe-bolh.bol-no  GE begin_bol# AND
                   b-oe-bolh.bol-no  LE end_bol# AND
                   b-oe-bolh.cust-no GE begin_cust AND
                   b-oe-bolh.cust-no LE end_cust AND
                   b-oe-bolh.printed EQ tb_reprint AND
                   b-oe-bolh.posted  EQ tb_posted AND
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
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = v-format-str AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                           sys-ctrl-shipto.ship-id      = b-oe-bolh.ship-id AND
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.
      
                      IF NOT AVAIL sys-ctrl-shipto THEN
                         FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
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
                         IF rd_bolcert EQ "BOL" THEN
                         DO:
                            IF NOT tb_freight-bill THEN
                            DO:
                               RUN SetBolForm (vcDefaultForm).
                               v-print-fmt = vcDefaultForm.
                            END.
                            ELSE
                            DO:
                               RUN SetBolForm (vcDefaultBOLX).
                               v-print-fmt = vcDefaultBOLX.
                            END.
                         END.
                         ELSE
                         DO:
                            RUN SetBolForm (v-def-coc-fmt).
                            v-print-fmt = v-def-coc-fmt.
                         END.
                      END.
      
                      RUN SetVariables.
                      RUN run-report(b-oe-bolh.cust-no,YES).
                      RUN GenerateReport(b-oe-bolh.cust-no,YES).
                   END. /*first-of*/
               END. /*each b-oe-bolh*/
         END.
      ELSE /*not find first sys-ctrl-shipto*/
      DO:
         IF rd_bolcert EQ "BOL" THEN
         DO:
            IF NOT tb_freight-bill THEN
               v-print-fmt = vcDefaultForm.
            ELSE
               v-print-fmt = vcDefaultBOLX.
         END.
         ELSE
            v-print-fmt = v-def-coc-fmt.
        
         RUN SetBolForm(v-print-fmt).
         RUN SetVariables.
         RUN run-report("",NO).
         RUN GenerateReport(begin_cust,NO).
      END.
      
      if tb_EmailAdvNotice AND
         tb_MailBatchMode AND 
          lRunFromWin THEN
          RUN showMessage IN hCallingProcedure (INPUT "emails silent").
   END. /*not rd-dest eq 5*/
   ELSE /*rd-dest eq 5*/
   DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company = cocode AND
         sys-ctrl-shipto.NAME = v-format-str) THEN
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
                           sys-ctrl-shipto.NAME = v-format-str AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                           sys-ctrl-shipto.ship-id = b-oe-bolh.ship-id AND
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                      IF NOT AVAIL sys-ctrl-shipto THEN
                         FIND FIRST sys-ctrl-shipto WHERE
                              sys-ctrl-shipto.company = cocode AND
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
                         IF rd_bolcert EQ "BOL" THEN
                         DO:
                            IF NOT tb_freight-bill THEN
                            DO:
                               RUN SetBolForm (vcDefaultForm).
                               v-print-fmt = vcDefaultForm.
                            END.
                            ELSE
                            DO:
                               RUN SetBolForm (vcDefaultBOLX).
                               v-print-fmt = vcDefaultBOLX.
                            END.
                         END.
                         ELSE
                         DO:
                            RUN SetBolForm (v-def-coc-fmt).
                            v-print-fmt = v-def-coc-fmt.
                         END.
                      END.
                     
                      RUN SetVariables.
                      RUN output-to-mail IN hCallingProcedure (INPUT b-oe-bolh.cust-no,YES, INPUT list-name).
                   END.
               END. /*end for each*/
         END. /*sys-ctrl-shipto found*/
      ELSE /*no sys-ctrl-shipto found*/
      DO:
         IF rd_bolcert EQ "BOL" THEN
         DO:
            IF NOT tb_freight-bill THEN
               v-print-fmt = vcDefaultForm.
            ELSE
               v-print-fmt = vcDefaultBOLX.
         END.
         ELSE
            v-print-fmt = v-def-coc-fmt.
        
         RUN SetBOLForm(v-print-fmt).
         RUN SetVariables.
         RUN output-to-mail IN hCallingProcedure (INPUT "",NO, INPUT list-name).
      END.
  
      if tb_MailBatchMode AND lRunFromWin then
          RUN showMessage IN hCallingProcedure (INPUT "emails silent").

   END.
  
   SESSION:SET-WAIT-STATE ("").
  
   IF tb_barcode THEN
      RUN barcode-proc.
  
   IF lv-run-commercial = "YES" AND NOT IS-xprint-form THEN 
      RUN run-report-ci.
  
   IF tb_ComInvoice THEN
      RUN CommercialInvoice.
  
   EMPTY TEMP-TABLE tt-fg-bin.
   EMPTY TEMP-TABLE tt-email.
  
   ll = tb_post-bol AND NOT tb_posted.
  
   IF ll AND oe-ctrl.u-inv AND v-check-qty THEN
      FOR EACH tt-post,
          FIRST oe-bolh NO-LOCK 
          WHERE ROWID(oe-bolh) EQ tt-post.row-id:
     
          RUN oe/bolcheck.p (ROWID(oe-bolh)).
          FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no
                              NO-LOCK NO-ERROR.
          IF AVAIL(w-except) THEN DO:
         
            RUN showMessage IN hCallingProcedure (INPUT "Insufficient Inventory").

            lv-exception = YES.
            IF lRunFromWin THEN
                RUN promptUser IN hCallingProcedure (INPUT "Sufficient Inventory", OUTPUT lv-exception).

            IF lv-exception THEN do:
              run exception-rpt.
              case rd-dest:
                when 1 then run output-exception-printer IN hCallingProcedure .
                when 2 then run output-exception-screen IN hCallingProcedure. 
                when 3 then run output-exception-file IN hCallingProcedure .
              end case.
            END.

            ll = NO.
            LEAVE.
          END.
   END.
  
   IF ll THEN DO:
       FOR EACH tt-post,
           FIRST oe-bolh NO-LOCK 
           WHERE ROWID(oe-bolh) EQ tt-post.row-id,
           EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.bol-no  EQ oe-bolh.bol-no: 
  
          IF oe-bolh.stat = "H" THEN DO:  
            IF lRunFromWin THEN
              RUN showMessage IN hCallingProcedure (INPUT "BOL Hold").

            ll = NO.
            LEAVE.
          END.

          /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
          FIND cust 
            WHERE cust.company EQ oe-bolh.company
              AND cust.cust-no EQ oe-bolh.cust-no 
            NO-LOCK NO-ERROR.
                
          IF AVAIL(cust) AND cust.ACTIVE EQ "X"
              AND oe-bolh.ship-id = oe-boll.loc THEN DO:

            RUN showMessage IN hCallingProcedure (INPUT "Cannot Transfer").
            ll = NO.
            LEAVE.
          END.

       END.
   END.
   
   IF ll THEN DO:
      ll = NO.
      IF lRunFromWin THEN
        RUN promptUser IN hCallingProcedure (INPUT "Post BOL", OUTPUT ll).
   END.
  
   IF ll THEN do:
      RUN post-bol.
      FIND FIRST tt-email NO-LOCK NO-ERROR.
      IF AVAIL tt-email THEN RUN email-reorderitems.
      IF lRunFromWin THEN
        RUN showMessage IN hCallingProcedure (INPUT "Posting Complete").
    
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-run-report) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report Procedure 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.
    
  {sys/form/r-top.i}
  
  assign
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
    v-ship-inst         = tb_print_ship    
    lv-run-bol          = ""
    lv-run-commercial   = "".

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts-hidden = NO THEN
     ASSIGN
        v-print-dept = tb_print-dept
        v-depts = fi_depts.

  IF tb_print_ship = NO THEN
     ASSIGN
        v-ship-inst = tb_print_ship .
         

  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  if td-show-parm then run show-param IN hCallingProcedure.
  
  SESSION:SET-WAIT-STATE ("general").
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
  
  run build-work ('').
  
  IF IS-xprint-form THEN DO:
  
      CASE rd-dest:
          WHEN 1 THEN PUT "<PRINTER?>".
          WHEN 2 THEN PUT "<PREVIEW>".        
          WHEN 4 THEN do:
                ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
          END.
          WHEN 5 THEN do:
              IF v-print-fmt = "Century" THEN /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
                   PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2.5mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(100)".
              ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(60)".
          END.
      END CASE.
  END.

  IF lv-run-bol = "YES" THEN DO:
      lXMLOutput = rd-dest EQ iXMLOutput. /* rstark 05181205 */
      IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44).
          RUN VALUE(v-program). 
          PUT CONTROL CHR(18).
      END.
      ELSE DO:
         IF v-program = "oe/rep/cocprempkg.p" THEN
              RUN oe/rep/cocprempkg.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgu.p" THEN
              RUN oe/rep/cocprempkgu.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgm.p" THEN
              RUN oe/rep/cocprempkgm.p (?).
         ELSE RUN VALUE(v-program).
      END.
  END.
  
  IF lv-run-commercial = "YES" AND IS-xprint-form THEN
     RUN oerep/runbolci.p.
  
  for each report where report.term-id eq v-term-id:
      delete report.
  end.
  
  OUTPUT CLOSE.
  RUN usrprnt IN hCallingProcedure.
 /* RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */
  
  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-run-report-ci) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-ci Procedure 
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

IF fi_depts-HIDDEN = NO THEN
   ASSIGN
     v-print-dept = tb_print-dept
     v-depts = fi_depts.
  IF tb_print_ship = NO THEN
     ASSIGN
        v-ship-inst = tb_print_ship .

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param IN hCallingProcedure.

SESSION:SET-WAIT-STATE ("general").

{sa/sa-sls01.i}

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

  IF NOT CAN-FIND(FIRST report WHERE
     report.term-id = v-term-id AND
     report.rec-id  = RECID(oe-bolh)) THEN
     DO:
        CREATE report.
       
        ASSIGN
         report.term-id  = v-term-id
         report.key-01   = oe-bolh.cust-no
         report.key-02   = oe-bolh.ship-id
         report.rec-id   = RECID(oe-bolh)
         report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
         report.key-03   = IF AVAIL sys-ctrl-shipto AND NOT sys-ctrl-shipto.log-fld  THEN "C" /*commercial invoice only*/
                           ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld  THEN "B" /*commercial invoice and bol both*/
                           ELSE                                                                "N" /*BOL only*/ 
         report.key-04   = IF AVAIL sys-ctrl-shipto THEN    sys-ctrl-shipto.char-fld ELSE "".     
     END.
                            
  IF lv-run-bol        EQ "" AND report.key-03 <> "C" THEN lv-run-bol        = "YES" .
  IF lv-run-commercial EQ "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
end.

v-lines-per-page = lines-per-page.
/*
IF rd-dest = 2 AND is-xprint-form THEN PUT "<PREVIEW>".   
ELSE IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/
/*IF IS-xprint-form THEN */  DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?>".
        WHEN 2 THEN PUT "<PREVIEW>".        
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN do:
            IF v-print-fmt = "Century" THEN /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
                 PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2.5mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(100)".
            ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(60)".
        END.
    END CASE.
END.

IF lv-run-commercial = "YES" THEN DO:
   RUN oerep/runbolci.p.
END.
       

OUTPUT CLOSE.

DO:
  case rd-dest :
    when 1 then run output-to-printer IN hCallingProcedure (INPUT "", INPUT NO, INPUT list-name).
    when 2 then run output-to-screen IN hCallingProcedure (INPUT "", INPUT NO, INPUT list-name).
    when 3 then run output-to-file IN hCallingProcedure (INPUT "", INPUT NO, INPUT list-name).
    when 4 then do:
       
       RUN asifax IN hCallingProcedure 
          (INPUT begin_cust, INPUT END_cust, INPUT list-name).

    END. 
    when 5 then do:
       IF is-xprint-form THEN DO:
           RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
           RUN ASIMail2 IN hCallingProcedure
               (INPUT begin_cust, INPUT END_cust, INPUT lv-pdf-file).
       END.
       ELSE DO:
         RUN ASIMailr2 IN hCallingProcedure.
    
       END.
    END. 
    WHEN 6 THEN RUN output-to-port IN hCallingProcedure (INPUT "", INPUT NO, INPUT list-name).
  end case.
END.

for each report where report.term-id eq v-term-id:
    delete report.
end.
RUN usrprnt IN hCallingProcedure.
/* RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */

SESSION:SET-WAIT-STATE ("").
IS-xprint-form = v-tmp-is-xprint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-run-report-mail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail Procedure 
PROCEDURE run-report-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    v-print-dept        = tb_print-dept
    lv-run-bol          = ""
    lv-run-commercial   = "".

  IF fi_depts-hidden = NO THEN
     ASSIGN
        v-print-dept = tb_print-dept
        v-depts = fi_depts.

   IF tb_print_ship = NO THEN
     ASSIGN
        v-ship-inst = tb_print_ship .
  
  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  if td-show-parm then run show-param IN hCallingProcedure.
  
  SESSION:SET-WAIT-STATE ("general").
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
  
  run build-work (ic2ndKey).
  
  IF NOT vcBOLNums > '' THEN RETURN.

  status default 'Processing... Please wait.'.

  if can-find (first report where report.term-id eq v-term-id) then
  do:
  
    IF IS-xprint-form THEN DO:
      IF v-print-fmt = "Century"                     /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
        THEN PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2.5mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(100)".
       
        ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(60)".
    END.

    IF lv-run-bol = "YES" THEN DO:

      IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44). 
          RUN value(v-program). 
          PUT CONTROL CHR(18).
      END.

      ELSE
      DO:
         IF v-program = "oe/rep/cocprempkg.p" THEN
            RUN oe/rep/cocprempkg.p (?).
         ELSE
            RUN value(v-program).
      END.
    END.
  
    IF lv-run-commercial = "YES" AND 
       IS-xprint-form THEN DO:
       RUN oerep/runbolci.p.
    END.
  END.
  
  else do:
    MESSAGE 'No records to process. Job aborted.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    return.
  end.

  for each report 
     where report.term-id eq v-term-id:
    delete report.
  end.
  
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-send-mail-uni-xl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-mail-uni-xl Procedure 
PROCEDURE send-mail-uni-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF SEARCH (lv-pdf-file) EQ ? THEN DO:
    IF lRunFromWin THEN
    MESSAGE 'Attachment File: ' lv-pdf-file ' is missing.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN  vcSubject   = "CofC for BOL: " + vcBOLNums 
          vcMailBody  = "Please review attached CofC for BOL #: " + vcBOLNums.
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   lv-pdf-file,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMail-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 Procedure 
PROCEDURE SendMail-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.
  
  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   lv-pdf-file,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendMail-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-2 Procedure 
PROCEDURE SendMail-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF SEARCH (list-name) NE ? THEN DO:
    IF NOT list-name MATCHES '*.txt' THEN DO:
      OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').
      IF OS-ERROR NE 0 THEN DO:          
          RUN showMessage IN hCallingProcedure (INPUT "Rename Failed" + STRING(OS-ERROR)).
      END.
      ELSE list-name = list-name + '.txt'.
    END.
  END.

  ELSE DO:
    RUN showMessage IN hCallingProcedure (INPUT "Attachement Missing").
    RETURN.
  END.

  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   list-name,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetBOLForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBOLForm Procedure 
PROCEDURE SetBOLForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.
   IF rd_bolcert EQ "BOL" THEN
   DO:
      {sys/inc/bolform.i}
   END.
   ELSE
   DO:
      CASE icFormName:
         WHEN "Xprint" THEN
            ASSIGN 
               is-xprint-form = YES
               v-program      = "oe/rep/cocxprnt.p".
     
         WHEN "PremierPkg" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkg.p".

         WHEN "PremierPkgU" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkgu.p".
         
         WHEN "PremierPkgM" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkgm.p".

         WHEN "" OR WHEN "Brick" THEN
            ASSIGN 
               is-xprint-form = NO
               v-program      = "oe/rep/cocbrick.p".

          WHEN "ACPI" THEN
              ASSIGN 
                is-xprint-form = NO
                v-program = "oe/rep/cocacpi.p".
     
          WHEN "CCC" THEN
              ASSIGN 
                is-xprint-form = NO
                v-program = "oe/rep/cocccc.p".

         OTHERWISE
            ASSIGN
               is-xprint-form = NO
               v-program = "oe/rep/cocuni.p".
     END CASE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVariables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVariables Procedure 
PROCEDURE SetVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN 
      v-print-bol = rd_bolcert EQ "BOL".

   IF rd-dest = 5 THEN
   DO:
     IF NOT v-print-bol AND v-coc-fmt EQ "Unipak-XL" THEN
        lv-pdf-file = init-dir + "\cofc.pdf".
     ELSE
        lv-pdf-file = init-dir + "\BOL".
   END.
   ELSE
      IF NOT v-print-bol AND v-coc-fmt EQ "Unipak-XL" THEN
         lv-pdf-file = init-dir + "\cofc.pdf".
      ELSE
         lv-pdf-file = init-dir + "\BOL" + string(begin_bol#).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-removeChars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars Procedure 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
   DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE k AS INTEGER NO-UNDO.
  
   k = NUM-ENTRIES(invalidChars).
   DO i = 1 TO k:
     ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
   END.
   RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

