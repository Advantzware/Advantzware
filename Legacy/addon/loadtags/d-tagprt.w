&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-recid as RECID.

/* Local Variable Definitions ---                                       */

DEF BUFFER bf-tag FOR loadtag.
DEF VAR tb_16ths AS log NO-UNDO.


 
/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE scanAgain AS LOGICAL NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{custom/xprint.i}
assign
 cocode = gcompany
 locode = gloc.

DEF VAR lines-per-page AS INT NO-UNDO.

DEF var save_id AS RECID.

DEF var time_stamp AS ch.
ASSIGN time_stamp = string(TIME, "hh:mmam").

DEF var v-ford-no AS int FORMAT ">>>>>>" extent 2 no-undo.
def var v-orders as char format "x(78)" extent 10.
DEF var v-fitem AS char FORMAT "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
DEF var v-po-no-source AS char FORMAT "!" init "R".
def var v-stat as char format "!" init "O".

DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
DEF var num-rec AS int init 0 NO-UNDO.
DEF var by-release AS log init NO NO-UNDO.
DEF VAR lv-rd_print  AS CHAR NO-UNDO.

/* 9812 CAH: */
DEF VAR v-loadtag       AS CHAR NO-UNDO INIT "ASI".  /* sys ctrl option */
DEF VAR v-mult          AS INT  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-cas-lab       AS LOG  NO-UNDO.  /* sys ctrl option */
DEF VAR v-tags          AS DEC  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-count         AS INT  NO-UNDO INIT 0.
DEF VAR v-fgrecpt       AS LOG  NO-UNDO.  /* sys ctrl option */

DEF VAR tb_reprint-tag AS LOG NO-UNDO INIT YES.
DEF VAR v-ship-id AS CHAR NO-UNDO.
DEF VAR scr-auto-print AS LOG NO-UNDO.

/* mdp var used for posting to finish goods */

DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.

/* 9812 CAH: Variables for Intermec Support */
def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".

def stream s-form.
def stream s-bar.

def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
def var n               as int no-undo initial 0.
DEF VAR var-display-warning AS LOG NO-UNDO.
def var fg-uom-list  as char NO-UNDO.

/* Vars for create-text-file */
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR v-dept-note AS cha FORM "x(80)" EXTENT 18 NO-UNDO.
DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-tag-no AS INT NO-UNDO.
DEF VAR lv-how-many-tags AS INT NO-UNDO.
DEF VAR lv-total-tags AS INT NO-UNDO.

/* gdm - 10160905*/
DEF VAR v-fgdsc1 LIKE itemfg.part-dscr1 NO-UNDO.
DEF VAR v-fgdsc2 LIKE itemfg.part-dscr2 NO-UNDO.
DEF VAR v-fgdsc3 LIKE itemfg.part-dscr3 NO-UNDO.

DEF TEMP-TABLE tt-ordjobs
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2.

def workfile w-file field w-key AS ROWID.
DEF TEMP-TABLE tt-tag FIELD tt-recid AS RECID.
DEF WORKFILE w-shipto LIKE shipto
                      FIELD stat AS CHAR
                      FIELD row-id AS ROWID.

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER ref-lot-no FOR reftable.

DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD company AS CHARACTER
  FIELD job-no AS CHARACTER
  FIELD job-no2 AS INTEGER
  FIELD ord-no AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE
      company job-no job-no2 ord-no
    INDEX ord-no company ord-no.

{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

ASSIGN  
  tmpstore = FILL("_",50).

{sys/form/r-top3.f}

DEF VAR lv-ok-ran AS LOG NO-UNDO.
{custom/formtext.i NEW}

DEF WORKFILE w-fg-rctd LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD invoiced AS LOG INIT NO.

{fg/fg-post3.i NEW}
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
{jc/jcgl-sh.i NEW}

DEFINE VARIABLE ordNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo AS CHARACTER NO-UNDO.

/* gdm - 04090909 */
DEF VAR v-barflg AS LOG NO-UNDO.
DEF VAR v-auto-print AS LOG NO-UNDO.
DEF VAR UserlabelPath AS cha NO-UNDO.

/* gdm - 06100901 */
DEF VAR v-txtflg AS LOG NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/fgpofrt.i}
   {sys/inc/fgrecpt.i} /* gdm - 12010901*/
   {sys/inc/rfidtag.i}
END.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE VARIABLE cBarCodeProgram AS CHARACTER NO-UNDO .

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  

RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".


/* gdm - 09210907 */
DEF VAR v-bardir AS LOG NO-UNDO.
DEF VAR v-bardir-chr AS CHAR NO-UNDO.
DEF BUFFER bf-oe-ord  FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.

/* gdm - 0930916 */
DEF BUFFER bf-po-ord  FOR po-ord.
DEF BUFFER bf-po-ordl FOR po-ordl.

DEF BUFFER bf-jobhdr FOR job-hdr.
DEFINE TEMP-TABLE tt-word-print LIKE w-ord 
    FIELD tag-no AS CHARACTER .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-12 v-num-of-tags begin_filename ~
tgAutoPrint scr-label-file Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_tag v-num-of-tags begin_filename ~
tgAutoPrint scr-label-file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-num-of-tags 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addon D-Dialog 
FUNCTION is-from-addon RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars D-Dialog 
FUNCTION removeChars RETURNS CHARACTER

  (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Location" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE begin_tag AS CHARACTER FORMAT "X(23)":U 
     LABEL "Tag#" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE end_tag AS CHARACTER FORMAT "X(23)":U 
     LABEL "To Tag#" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Label File" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE v-num-of-tags AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Tags" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 11.19.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 3.57.

DEFINE VARIABLE tgAutoPrint AS LOGICAL INITIAL yes 
     LABEL "Auto Print Label?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_tag AT ROW 2.19 COL 19 COLON-ALIGNED
     end_tag AT ROW 3.38 COL 19 COLON-ALIGNED
     v-num-of-tags AT ROW 5.52 COL 19 COLON-ALIGNED
     begin_filename AT ROW 7.24 COL 45 COLON-ALIGNED WIDGET-ID 8
     tgAutoPrint AT ROW 7.43 COL 3 WIDGET-ID 2
     scr-label-file AT ROW 8.86 COL 13.6 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 10.76 COL 17
     Btn_Cancel AT ROW 10.76 COL 50
     RECT-11 AT ROW 1 COL 1
     RECT-12 AT ROW 6.71 COL 2 WIDGET-ID 6
     SPACE(1.19) SKIP(2.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Loadtag Print"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_tag IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_tag IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_tag:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN v-num-of-tags IN FRAME D-Dialog
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Loadtag Print */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_tag D-Dialog
ON LEAVE OF begin_tag IN FRAME D-Dialog /* Tag# */
DO:
  RUN get-label-file.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
    ASSIGN {&DISPLAYED-OBJECTS}.
    scr-auto-print = tgAutoPrint.
  /*  RUN print-loadtag. */

  FOR EACH tt-word-print:
       DELETE tt-word-print .
   END.

  ASSIGN {&displayed-objects}.
  
  v-auto-print = scr-auto-print.

  IF tb_reprint-tag AND begin_tag:SCREEN-VALUE = "" THEN DO:
     MESSAGE "Enter tag# to reprint loadtag." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO begin_tag.
     RETURN NO-APPLY.
  END.

  IF scr-auto-print AND scr-label-file = "" THEN
  DO:
     MESSAGE "Label Matrix Label File cannot be blank."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.


  IF NOT lv-ok-ran THEN RUN ok-button.
/*   lv-ok-ran = NO.                                */
/*   IF scr-label-file:SCREEN-VALUE <> ""  THEN DO: */
/*      APPLY "entry" TO begin_tag.                 */
/*      RETURN NO-APPLY.                            */
/*   END.                                           */
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file D-Dialog
ON HELP OF scr-label-file IN FRAME D-Dialog /* Label File */
DO:
DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   /* gdm - 11050804 */
   DEF VAR v-path AS CHAR NO-UNDO.

   ASSIGN v-path = TRIM(scr-label-file:SCREEN-VALUE).

    IF TRIM(v-path) EQ "" THEN DO:
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cocode
              AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).

    END.
    RUN sys\ref\char-fld-help.w(INPUT cocode,
                                INPUT v-path,
                                OUTPUT chFile).

  
      ASSIGN scr-label-file:SCREEN-VALUE = chFile.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
     DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
     DEFINE INPUT  PARAMETER VisualStyle AS LONG.
     DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE askNextPalletID D-Dialog 
PROCEDURE askNextPalletID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipc-cust AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opl-error AS LOG NO-UNDO.

 MESSAGE "The Pallet ID has reached its limit." skip
         "Please reset it for customer " ipc-cust 
     VIEW-AS ALERT-BOX .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint D-Dialog 
PROCEDURE AutoPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-int AS INT NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR v-path AS CHARACTER NO-UNDO.

IF scr-auto-print THEN
    DO:

       LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".
       USE "SOFTWARE".
       GET-KEY-VALUE SECTION "Teklynx\Label Matrix"
                     KEY "PATH"
                     VALUE v-path.
       UNLOAD "SOFTWARE".
       v-path = TRIM(v-path,"\").
       ASSIGN
          v-path = v-path + "\lmwprint.exe "
          cFileName = "/L=" + scr-label-file.

          RUN WinExec (INPUT v-path + CHR(32) + cFileName , INPUT 1, OUTPUT
                       v-int).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag D-Dialog 
PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-tag-no AS INT NO-UNDO.
  DEF INPUT PARAM ip-total-unit LIKE w-ord.total-unit NO-UNDO.

  DEF BUFFER b-loadtag FOR loadtag.
  DEF BUFFER b-po-ordl FOR po-ordl.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-got-job AS LOG NO-UNDO.
  DEF VAR lv-out-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR lv-out-qty as dec no-undo.
  DEF VAR lv-from-uom AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-ord-qty AS INT NO-UNDO.
  DEF VAR lv-ord-uom AS CHAR NO-UNDO.
  DEF VAR lv-setup-included AS LOG NO-UNDO.
  DEF VAR lv-setup-per-cost-uom AS DEC NO-UNDO.
  def var v-bwt like po-ordl.s-len no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo.
  DEF VAR dRFIDTag AS DEC NO-UNDO.
  DEF BUFFER bf-eb FOR eb.

  IF TRUE /* tb_reprint-tag */ THEN DO:
     FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ cocode
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ TRIM(begin_tag:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         USE-INDEX tag NO-ERROR.
     IF AVAIL loadtag THEN
       io-tag-no = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
     RETURN.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file D-Dialog 
PROCEDURE create-text-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR iPalletID AS INT NO-UNDO.
  DEF VAR iStartPalletID AS INT NO-UNDO.
  DEF VAR iEndPalletID AS INT NO-UNDO.
  DEF VAR cTotalUnit AS CHAR NO-UNDO.
  DEF VAR cRFIDTag AS cha NO-UNDO.
  DEF VAR vError AS LOG NO-UNDO.
  FIND FIRST w-ord NO-ERROR.
  DEF BUFFER bf-cust FOR cust.
      FIND FIRST w-ord NO-ERROR.

   IF v-loadtag = "TRIAD" THEN DO:
        if form_fid > "" then do:   /* download the form file into the printer ~*/
          input stream s-form from value(form_fid) no-echo.
          _form: do while true:
                readkey stream s-form.
            if lastkey < 0 then leave _form.
              put stream s-bar CONTROL chr(lastkey).
          end.
          input stream s-form close.
        end.
        FOR EACH w-ord:
           v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
           IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
                THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
           find first itemfg where itemfg.company = cocode
                    and itemfg.i-no = w-ord.i-no no-lock no-error.
        IF w-ord.total-tags gt -1 THEN DO:
          DO i = 1 TO (w-ord.total-tags + 1):
            /* select the form */
            put stream s-bar control stx esc "E" string(form#) ",1" can etx.
            /* 9901 CAH: done above ... 
            /* clear the variable data fields */
            put stream s-bar control stx can etx.
            */
            char_units = (if i <= w-ord.total-tags 
            then string(w-ord.total-unit) else "    ").  
            def var char_date as char format 'x(10)' no-undo.
            char_date = string(today,"99/99/9999").
            /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
            if length(w-ord.ship-name) > 19
            then w-ord.ship-name = substring(w-ord.ship-name,1,19).
            
            def var vcFGItem as char no-undo.
            vcFGItem = 
                if avail itemfg then itemfg.i-no else w-ord.i-no.
           do n = copy_count to 1 by -1:
            /* send the variable data to the printer */
            put stream s-bar unformatted
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-part-no  cr etx
                stx w-ord.cust-part-no  cr etx
                stx char_units          cr etx
                stx char_units          cr etx
                stx char_date           cr etx
                stx v-job               cr etx
                stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                stx string(i)           cr etx /* 08.20 was n */
                stx string(w-ord.total-tags + 1) cr etx /* 08.20 was copy_count */
                stx w-ord.ship-name     cr etx
                stx vcFGItem            cr etx.
            /* issue the print command */    
            put stream s-bar control     
                stx rs "1" us "1" etb etx.
           end.
          end.   /* tag count loop */
        end.  /* non zero */  
        END.    /* each w-ord */
      /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:
      OUTPUT TO VALUE(v-out).
      PUT UNFORMATTED
          "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP," +
          "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP," +
          "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
          "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
          "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"+
          "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
      IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
         PUT UNFORMATTED ",COUNTER#,RFIDTag".

      PUT UNFORMATTED ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,PalletCode,PalletID".
      
      PUT SKIP.

      FOR EACH w-ord:

        IF tb_16ths THEN
          ASSIGN
           w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
           w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
           w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0).

        ASSIGN
        lv-text = ""
        v-dept-note = ""

        /* gdm - 10160905 */
        v-fgdsc1 = ""
        v-fgdsc2 = ""
        v-fgdsc3 = "".

        find first itemfg where itemfg.company eq cocode
                            and itemfg.i-no    eq w-ord.i-no no-lock no-error.
        if avail itemfg THEN DO:        
           ASSIGN w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100
                  w-ord.sheet-wt = itemfg.weight-100 / 100 
                  w-ord.cust-part-no = itemfg.part-no.

           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.
           FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                                     AND notes.note_code = "SN" :
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
               IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.      
           END.

           /* gdm - 101610905 */
           ASSIGN v-fgdsc1 = itemfg.part-dscr1
                  v-fgdsc2 = itemfg.part-dscr2
                  v-fgdsc3 = itemfg.part-dscr3.

        END.  /* avail itemfg */
        /*
        IF tb_dept-note THEN DO:
           lv-text = "".
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.

           IF w-ord.ord-no NE 0 THEN DO:
              FOR EACH job-hdr NO-LOCK
                  WHERE job-hdr.company EQ cocode
                    AND job-hdr.ord-no  EQ w-ord.ord-no 
                    AND job-hdr.job-no  EQ w-ord.job-no
                    AND job-hdr.job-no2 EQ w-ord.job-no2
                  BREAK BY job-hdr.job
                        BY job-hdr.job-no
                        BY job-hdr.job-no2:
                 IF LAST-OF(job-hdr.job-no2) THEN
                FOR EACH job NO-LOCK
                     WHERE job.company EQ job-hdr.company
                       AND job.job     EQ job-hdr.job
                       AND job.job-no  EQ job-hdr.job-no
                       AND job.job-no2 EQ job-hdr.job-no2,
                     EACH notes NO-LOCK
                     WHERE notes.rec_key EQ job.rec_key
                       AND CAN-DO(v-dept-list,notes.note_code):
                    IF notes.note_form_no = 0 OR notes.note_form_no = w-ord.form-no THEN
                        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                 END.
              END.

/*              FOR EACH oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.ord-no  EQ w-ord.ord-no,
                  EACH notes NO-LOCK WHERE notes.rec_key EQ oe-ordl.rec_key
                       AND CAN-DO(v-dept-list,notes.note_code)
                       AND notes.note_form_no = w-ord.form-no
                  BY oe-ordl.LINE:
                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
              END.
*/
           END.
           IF lv-text NE "" THEN DO:
              DO li = 1 TO 10:
                 CREATE tt-formtext.
                 ASSIGN tt-line-no = li
                        tt-length  = 80.
              END.
              RUN custom/formtext.p (lv-text).
              i = 8.           
              FOR EACH tt-formtext:
                  i = i + 1.
                  IF i <= 18 THEN v-dept-note[i] = tt-formtext.tt-text.      

              END.
           END.
        END. /* tb_dept-note*/
        */

        ASSIGN
        w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN v-job = "".
        ASSIGN
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
         lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
         lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po).

        IF w-ord.total-tags gt 0 THEN DO:
          lv-how-many-tags =  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 OR w-ord.total-tags = 1 THEN w-ord.total-tags
                              ELSE (w-ord.total-tags - 1).
          FIND bf-cust WHERE bf-cust.company = cocode
                         AND bf-cust.cust-no EQ w-ord.cust-no
                       NO-LOCK NO-ERROR.

           RUN incrementPalletID (BUFFER bf-cust, lv-how-many-tags * w-ord.mult,
                                  OUTPUT iStartPalletID, OUTPUT iEndPalletID).
           IF iEndPalletID EQ -1 THEN DO:
              RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
              RETURN.
           END.


          iPalletId = iStartPalletID.
          DO i = 1 TO (lv-how-many-tags * w-ord.mult):
             /* loadtags generation */
             IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN DO:
                IF i = 1 THEN lv-tag-no = i.
                RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
             END.

             IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN DO:
                
                FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".
 
             END.
             cTotalUnit = string(w-ord.total-unit, ">>>>>>>9").
             RUN write-loadtag-line (INPUT cRFIDTag, INPUT cTotalUnit, INPUT iPalletID).
             iPalletID = iPalletID + 1.
          end.

          IF lookup(v-loadtag,"SSLABEL,CentBox") = 0 THEN DO:

  
              RUN incrementPalletID (BUFFER bf-cust, w-ord.mult,
                       OUTPUT iStartPalletID, OUTPUT iEndPalletID).
              IF iEndPalletID EQ -1 THEN DO:
                   RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
                   RETURN.
              END.
                     

              iPalletId = iStartPalletID.
              do v-count = 1 to w-ord.mult: /* for partial print */
                    /* loadtags generation */
                 IF v-count EQ 1 THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).
                 cTotalUnit = "".
                 RUN write-loadtag-line (INPUT cRFIDTag, cTotalUnit, INPUT iPalletID).
                 iPalletID = iPalletID + 1.
              end.
          END.

        end.
        delete w-ord.
      end.
      output close.
    end.    /* NOT TRIAD */

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-ord D-Dialog 
PROCEDURE create-w-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rel-date AS DATE NO-UNDO.
   DEF BUFFER b-job FOR job.
   DEF BUFFER b-job-hdr FOR job-hdr.
   
   FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
   FIND FIRST itemfg WHERE itemfg.company = loadtag.company
                       AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   FIND FIRST oe-ord WHERE oe-ord.company = loadtag.company
                       AND oe-ord.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

   IF AVAIL oe-ord THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
                           AND oe-ordl.ord-no = loadtag.ord-no
                           AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
      FIND FIRST cust WHERE cust.company = loadtag.company
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.

      FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
                                 AND b-job.job-no  = loadtag.job-no
                                 AND b-job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL b-job THEN
         FIND FIRST b-job-hdr WHERE b-job-hdr.company EQ b-job.company
                                AND b-job-hdr.job     EQ b-job.job
                                AND b-job-hdr.job-no  EQ b-job.job-no
                                AND b-job-hdr.job-no2 EQ b-job.job-no2
                                AND b-job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
      

      CREATE w-ord.
      ASSIGN w-ord.ord-no      = loadtag.ord-no
            w-ord.job-no       = loadtag.job-no
            w-ord.job-no2      = loadtag.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = oe-ordl.e-num
            w-ord.lot          = loadtag.misc-char[2].

      IF AVAIL b-job-hdr THEN
         w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
      IF AVAIL b-job THEN
         w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
             
      RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#).

      IF AVAIL itemfg THEN
         ASSIGN w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = loadtag.qty-case
             w-ord.bundle  = loadtag.case-bundle
             w-ord.style   = itemfg.style.

      IF w-ord.style NE "" THEN
      DO:
         FIND FIRST style WHERE
              style.company EQ cocode AND
              style.style EQ w-ord.style
              NO-LOCK NO-ERROR.

         IF AVAIL style THEN
         DO:
            w-ord.style-desc = style.dscr.
            RELEASE style.
         END.
      END.
      IF v-ship-id EQ "" THEN v-ship-id = oe-ord.cust-no.
      FIND FIRST shipto WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq v-ship-id
            USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
         ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute  = eb.flute
             w-ord.test   = eb.test
             w-ord.pcs    = eb.cas-cnt
             w-ord.bundle = eb.cas-pal
             w-ord.cas-no = eb.cas-no
             w-ord.pallt-no = eb.tr-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.  /* avail oe-ord*/
   ELSE IF loadtag.job-no <> "" THEN DO:
      FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                               AND job.job-no = loadtag.job-no
                               AND job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ loadtag.i-no
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr AND tb_reprint-tag THEN
          FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN DO:
      
         FIND FIRST cust WHERE cust.company eq cocode
                          AND cust.cust-no eq job-hdr.cust-no NO-LOCK NO-ERROR.
         FIND FIRST itemfg WHERE itemfg.company eq cocode
                            AND itemfg.i-no    eq loadtag.i-no NO-LOCK NO-ERROR.
         
         CREATE w-ord.
         ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = loadtag.i-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.lot          = loadtag.misc-char[2].

          IF AVAIL itemfg THEN
             ASSIGN
                w-ord.cust-part-no = itemfg.part-no
                w-ord.style        = itemfg.style
                w-ord.i-name       = itemfg.i-name
                w-ord.upc-no       = itemfg.upc-no
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
          IF v-ship-id EQ "" THEN v-ship-id = job-hdr.cust-no.
          FIND FIRST shipto
              WHERE shipto.company eq cocode
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no
             w-ord.pallt-no   = eb.tr-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

       END.  /* avail job*/
   END. /* job-no <> "" */
   ELSE IF loadtag.po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                           AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
         FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                                    AND po-ordl.po-no EQ po-ord.po-no
                                    AND po-ordl.i-no = loadtag.i-no
                                    USE-INDEX po-no  NO-ERROR.
      IF AVAIL po-ordl THEN DO:
         FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
         FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
         FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
         
         CREATE w-ord.
         ASSIGN
            w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
            w-ord.cust-no = po-ord.cust-no
            w-ord.due-date = po-ord.due-date
            w-ord.i-no = po-ordl.i-no
            w-ord.i-name = po-ordl.i-name
            w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                         cust.int-field[1] ELSE v-mult
            w-ord.ord-qty = po-ordl.ord-qty
            w-ord.po-no = po-ord.po-no
            w-ord.tare-wt = 10
            w-ord.uom = 'EA'
            w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE ''
            w-ord.lot    = loadtag.misc-char[2]. 
         IF AVAILABLE itemfg THEN
            ASSIGN w-ord.est-no = itemfg.est-no
                w-ord.upc-no = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute = itemfg.flute
                w-ord.test = itemfg.test
                w-ord.pcs = itemfg.case-count
                w-ord.bundle = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                w-ord.style = itemfg.style.

         IF w-ord.style NE "" THEN
         DO:
            FIND FIRST style WHERE
                 style.company EQ cocode AND
                 style.style EQ w-ord.style
                 NO-LOCK NO-ERROR.
         
            IF AVAIL style THEN
            DO:
               w-ord.style-desc = style.dscr.
               RELEASE style.
            END.
         END.

         IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
            FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                              AND eb.est-no EQ itemfg.est-no
                              AND eb.stock-no EQ itemfg.i-no NO-ERROR.
         IF AVAILABLE eb THEN
             ASSIGN w-ord.flute = eb.flute
                    w-ord.test = eb.test
                    w-ord.pcs = eb.cas-cnt
                    w-ord.bundle = eb.cas-pal
                    w-ord.cas-no = eb.cas-no
                    w-ord.pallt-no = eb.tr-no.
         IF v-ship-id EQ "" THEN v-ship-id = po-ord.cust-no.
         FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                                  AND shipto.cust-no EQ po-ord.cust-no
                                  AND shipto.ship-id EQ v-ship-id
                                USE-INDEX ship-id NO-ERROR.
         IF AVAILABLE shipto THEN
            ASSIGN w-ord.ship-name = shipto.ship-name
                    w-ord.ship-add1 = shipto.ship-add[1]
                    w-ord.ship-add2 = shipto.ship-add[2]
                    w-ord.ship-city = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-zip = shipto.ship-zip.
      
         ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

    END. /* AVAIL PO-ORDL */
   END. /* po-no <> ""*/
   ELSE DO:
       FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                 AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
       IF AVAIL itemfg THEN DO:
          FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
          FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ itemfg.cust-no NO-ERROR.
          
          CREATE w-ord.
          ASSIGN w-ord.i-no = itemfg.i-no
                 w-ord.i-name = itemfg.i-name
                 w-ord.cust-no = itemfg.cust-no
                 w-ord.cust-name = itemfg.cust-name
                 w-ord.cust-part-no = itemfg.part-no
                 w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                              cust.int-field[1] ELSE v-mult
                 w-ord.box-len = itemfg.l-score[50]
                 w-ord.box-wid = itemfg.w-score[50]
                 w-ord.box-dep = itemfg.d-score[50]
                 w-ord.flute = itemfg.flute
                 w-ord.upc-no = itemfg.upc-no
                 w-ord.test = itemfg.test
                 w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE company.name
                 w-ord.tare-wt = 10
                 w-ord.uom = "EA"
                 w-ord.pcs = itemfg.case-count
                 w-ord.bundle = itemfg.case-pall
                 w-ord.total-tags = 1
                 w-ord.ord-qty = loadtag.qty 
                 w-ord.pcs = loadtag.qty-case
                 w-ord.bundle = loadtag.case-bundle
                 w-ord.partial =loadtag.partial
                 w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial
                 w-ord.style        = itemfg.style
                 w-ord.lot          = loadtag.misc-char[2].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.
          
             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
       END. /* avail itemfg */
   END.
   /* task 11230523 */
   IF tb_reprint-tag THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
                          AND fg-bin.i-no = w-ord.i-no
                          AND fg-bin.tag = begin_tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                          AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND AVAIL w-ord THEN
         ASSIGN w-ord.pcs = fg-bin.case-count
                w-ord.bundle = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY begin_tag v-num-of-tags begin_filename tgAutoPrint scr-label-file 
      WITH FRAME D-Dialog.
  ENABLE RECT-11 RECT-12 v-num-of-tags begin_filename tgAutoPrint 
         scr-label-file Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-label-file D-Dialog 
PROCEDURE get-label-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* IF v-auto-print AND LOGICAL(scr-freeze-label) EQ NO THEN */
 
  DO WITH FRAME {&FRAME-NAME}:
    FIND bf-tag WHERE RECID(bf-tag) = ip-recid .
    DEF VAR v-cust-no AS CHAR NO-UNDO.
  

    IF bf-tag.ord-no GT 0 THEN DO:
        FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
             AND oe-ord.ord-no EQ bf-tag.ord-no
            NO-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN
            v-cust-no = oe-ord.cust-no.
    END.
    ELSE DO:
    
        IF bf-tag.job-no GT "" THEN DO:
           FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no EQ bf-tag.job-no
                AND job-hdr.job-no2 EQ bf-tag.job-no2
               NO-LOCK NO-ERROR.
           IF AVAIL job-hdr AND job-hdr.cust-no GT "" THEN
              v-cust-no = job-hdr.cust-no.
        END.
    END.

     IF v-cust-no NE "" THEN
        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ cocode AND
             reftable.loc      GE bf-tag.i-no AND
             reftable.loc      LE bf-tag.i-no AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

     IF AVAIL reftable AND reftable.dscr NE "" THEN
        scr-label-file = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
     ELSE
        IF INT(bf-tag.ord-no) NE 0 AND
           INT(bf-tag.ord-no) NE 0 THEN
        DO:
           FIND FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.i-no    GE bf-tag.i-no AND
                oe-rel.i-no    LE bf-tag.i-no AND
                oe-rel.ord-no  GE INT(bf-tag.ord-no) AND
                oe-rel.ord-no  LE INT(bf-tag.ord-no)
                NO-LOCK NO-ERROR.
           
           IF AVAIL oe-rel THEN 
              FIND FIRST shipto NO-LOCK 
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ oe-rel.cust-no 
                 AND shipto.ship-id EQ oe-rel.ship-id 
               USE-INDEX ship-id NO-ERROR.
           ELSE
              FIND FIRST shipto NO-LOCK
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ v-cust-no 
                 AND shipto.ship-id EQ v-cust-no
                  USE-INDEX ship-id NO-ERROR.
           
              IF AVAIL shipto THEN DO:
                 IF AVAIL oe-rel THEN
                    v-cust-no = oe-rel.cust-no.
           
                 FIND FIRST sys-ctrl-shipto NO-LOCK
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          scr-label-file = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             scr-label-file = sys-ctrl.char-fld.
                          ELSE
                             scr-label-file = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          scr-label-file = sys-ctrl.char-fld.
                       ELSE
                          scr-label-file = "".
                    END.
                 END.
              END.
        END.
        ELSE
        IF INT(bf-tag.ord-no) EQ 0 AND
           INT(bf-tag.ord-no) EQ 0 THEN
           DO:
              FIND FIRST shipto WHERE
                   shipto.company EQ cocode AND
                   shipto.cust-no EQ v-cust-no AND
                   shipto.ship-id EQ v-cust-no
                   NO-LOCK NO-ERROR.

              IF AVAIL shipto THEN DO:
                 
                 FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company      EQ cocode AND
                      sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                      sys-ctrl-shipto.cust-vend    EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                      sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                      sys-ctrl-shipto.char-fld     NE ''
                      NO-LOCK NO-ERROR.

                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto WHERE
                         sys-ctrl-shipto.company      EQ cocode AND
                         sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                         sys-ctrl-shipto.cust-vend    EQ YES AND
                         sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                         sys-ctrl-shipto.char-fld     NE ''
                         NO-LOCK NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          scr-label-file = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             scr-label-file = sys-ctrl.char-fld.
                          ELSE
                             scr-label-file = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          scr-label-file = sys-ctrl.char-fld.
                       ELSE
                          scr-label-file = "".
                    END.
                 END.
              END.


           END. /*bf-tag.ord-no and bf-tag.ord-no eq 0*/
           scr-label-file:SCREEN-VALUE = scr-label-file.
    END.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info D-Dialog 
PROCEDURE get-rel-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-pono LIKE w-ord.cust-po-no NO-UNDO.
  DEF OUTPUT PARAM op-date LIKE w-ord.rel-date NO-UNDO.
  DEF OUTPUT PARAM op-lot# LIKE w-ord.rel-lot# NO-UNDO.


  RELEASE oe-rell.
  RELEASE oe-rel.

  IF v-po-no-source EQ "R" THEN DO:
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
          AND oe-rell.ord-no   EQ oe-ordl.ord-no
          AND oe-rell.i-no     EQ oe-ordl.i-no
          AND oe-rell.line     EQ oe-ordl.line,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
         /* AND oe-relh.rel-date GE begin_date
          AND oe-relh.rel-date LE end_date */
        BY oe-relh.rel-date
        BY oe-relh.r-no:

      ASSIGN
       op-pono = oe-rell.po-no
       op-date = oe-relh.rel-date.
      LEAVE.
    END.

    IF AVAIL oe-rell THEN
    FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

    ELSE
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company  EQ oe-ordl.company
          AND oe-rel.ord-no   EQ oe-ordl.ord-no
          AND oe-rel.i-no     EQ oe-ordl.i-no
          AND oe-rel.line     EQ oe-ordl.line
          AND oe-rel.rel-no   EQ 0
          /* wfk - just for this version ** 
          AND oe-rel.rel-date GE begin_date
          AND oe-rel.rel-date LE end_date */
        BY oe-rel.rel-date
        BY oe-rel.r-no:

      ASSIGN
       op-pono = oe-rel.po-no
       op-date = oe-rel.rel-date.
      LEAVE.
    END.
  END.

  IF NOT AVAIL oe-rel THEN
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company  EQ oe-ordl.company
        AND oe-rel.ord-no   EQ oe-ordl.ord-no
        AND oe-rel.i-no     EQ oe-ordl.i-no
        AND oe-rel.line     EQ oe-ordl.line
      BY oe-rel.rel-date
      BY oe-rel.r-no:

    op-date = oe-rel.rel-date.
    LEAVE.
  END.

    ASSIGN op-lot# = oe-rel.lot-no.

  IF v-po-no-source NE "R"                    OR
     (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN
    op-pono = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
                                       ELSE IF AVAIL oe-ord THEN oe-ord.po-no
                                       ELSE "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incrementPalletID D-Dialog 
PROCEDURE incrementPalletID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipb-cust FOR cust.
DEFINE INPUT PARAMETER ipi-tags AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER op-start-pallet-no LIKE cust.spare-int-1.
DEFINE OUTPUT PARAMETER op-end-pallet-no LIKE cust.spare-int-1.
DEF BUFFER bf-cust FOR cust.
DEF VAR li AS INT INIT 0.
DEF VAR lj AS INT INIT 0.

FIND bf-cust WHERE ROWID(bf-cust) EQ ROWID(ipb-cust) EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-cust THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.

IF bf-cust.spare-int-1 EQ 0 THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.
li = bf-cust.spare-int-1.
IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
    op-end-pallet-no = -1.
    RETURN.
END.
op-start-pallet-no = li + 1.
DO lj = 1 TO ipi-tags:
    li = li + 1.
    IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
        op-end-pallet-no = -1.
        RETURN.
    END.
END.

ASSIGN 
    op-end-pallet-no = li
    bf-cust.spare-int-1 = li.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBarDirPath AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDB AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lUserSpecific AS LOGICAL     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST bf-tag WHERE RECID(bf-tag) = ip-recid NO-LOCK NO-ERROR .
  begin_tag = IF AVAIL bf-tag THEN bf-tag.tag-no ELSE "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ g_company
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

  FIND FIRST sys-ctrl WHERE sys-ctrl.company eq g_company
                      AND sys-ctrl.name    eq "LOADTAG" NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found.  Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  assign
   v-loadtag = sys-ctrl.char-fld
   v-mult    = sys-ctrl.int-fld
   v-cas-lab = sys-ctrl.log-fld
   v-tags    = sys-ctrl.dec-fld.
  RUN sys/ref/GetBarDir.p (INPUT g_company,
                           INPUT "",
                           OUTPUT cBarDirPath,
                           OUTPUT cDB,
                           OUTPUT lUserSpecific).
  begin_filename:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF cBarDirPath NE "" THEN cBarDirPath ELSE "c:\ba\label".
   RUN get-label-file.

  IF v-mult LE 0 THEN v-mult = 1.
  IF v-num-of-tags GT v-mult THEN v-mult = v-num-of-tags.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button D-Dialog 
PROCEDURE ok-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF begin_filename:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
     userLabelPath <> "" THEN        
     begin_filename:SCREEN-VALUE = userLabelPath.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  
  /* gdm - 04090909 */
  ASSIGN v-out = begin_filename.
  IF v-out EQ ""  THEN DO:

      FIND FIRST sys-ctrl NO-LOCK 
          WHERE sys-ctrl.company EQ gcompany
            AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN
          DO TRANSACTION:
          CREATE sys-ctrl.
          ASSIGN
              sys-ctrl.company  = gcompany
              sys-ctrl.name     = "BARDIR"
              sys-ctrl.descrip  = "C:\BA\Label\".
          FIND CURRENT sys-ctrl NO-LOCK.
      END.
      v-out = sys-ctrl.descrip.
  END.
  /* gdm - 04090909 end */

  FILE-INFO:FILE-NAME = begin_filename.
  if begin_filename <> "" AND FILE-INFO:FILE-type eq ? then do:
     message "Form file/path does not exist. Do you want to create it?" 
             view-as alert-box ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
     IF v-ans THEN OS-CREATE-DIR VALUE(begin_filename).
     ELSE do:
         MESSAGE "Loadtag file path is not valid. Can't create."
             VIEW-AS ALERT-BOX ERROR.
         return no-apply.
     END.
   end.


  /* gdm - 09290903 end*/
  /*wfk  */
  IF tb_reprint-tag THEN RUN reprint-tag.
  ELSE RUN run-report NO-ERROR. 
  
  IF NOT ERROR-STATUS:ERROR THEN lv-ok-ran = YES.

  APPLY "entry" TO begin_tag IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-loadtag D-Dialog 
PROCEDURE print-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
  DEF VAR v-tmp-mult AS INT NO-UNDO.
  DEF VAR v-ship-name AS cha NO-UNDO.
  DEF VAR v-ship-add1 AS cha NO-UNDO.
  DEF VAR v-ship-add2 AS cha NO-UNDO.
  DEF VAR v-ship-city AS cha NO-UNDO.
  DEF VAR v-ship-state AS cha NO-UNDO.
  DEF VAR v-ship-zip AS cha NO-UNDO.
  DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
  DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.
  DEF VAR v-flute LIKE eb.flute NO-UNDO.
  DEF VAR v-test LIKE eb.test NO-UNDO.
  DEF VAR v-cas-no LIKE eb.cas-no NO-UNDO.
  DEF VAR v-gross-wt AS DEC FORMAT ">>>>9.99" NO-UNDO.
  DEF VAR v-tare-wt AS DEC FORMAT ">>>>9.99" NO-UNDO.
  DEF VAR v-net-wt AS DEC FORMAT ">>>>9.99" NO-UNDO.
  DEF VAR v-sheet-wt AS DEC FORMAT ">>>9.99" NO-UNDO.
  DEF var v-uom LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VAR v-job AS cha NO-UNDO.
  DEF VAR v-due-date LIKE oe-ord.due-date NO-UNDO.
  DEF VAR v-rel-date LIKE oe-rel.rel-date NO-UNDO.
  DEF VAR v-cust-po-no LIKE oe-ordl.po-no NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  
  DEF VAR v-count AS INT NO-UNDO.

  IF v-out = "" THEN v-out = "c:~\ba~\label~\loadtag.txt".
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".

     v-out = v-out + "loadtag.txt".
  END.

  FIND FIRST company WHERE company.company EQ g_company NO-LOCK.

  OUTPUT TO VALUE(v-out).
  PUT UNFORMATTED
          "CUSTOMER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPZIP,INAME,DUEDATE," +
          "RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
          "TAREWGT,NETWGT,SHEETWGT,UOM,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
          "TAG#,PARTIAL,CASECOSE".
  PUT SKIP.

  FIND FIRST bf-tag WHERE RECID(bf-tag) = ip-recid NO-ERROR.
  IF AVAIL bf-tag THEN DO:
  /*
  FOR EACH w-ord:
        IF tb_16ths THEN
          ASSIGN
           w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
           w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
           w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0).

        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq w-ord.i-no
            no-lock no-error.
              
        if avail itemfg then
          assign
           w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100.
           w-ord.sheet-wt = itemfg.weight-100 / 100.
                   
        w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt.

        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN v-job = "".

        ASSIGN
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
         lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
         lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po).

        IF w-ord.total-tags gt 0 THEN
        DO:
          lv-how-many-tags =  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN w-ord.total-tags
                              ELSE (w-ord.total-tags - 1).
  ----*/
      FIND FIRST itemfg WHERE itemfg.company = g_company
                          AND itemfg.i-no = bf-tag.i-no NO-LOCK NO-ERROR.
      FIND FIRST cust WHERE cust.company = g_company
                       AND cust.cust-no = itemfg.cust-no
                       NO-LOCK NO-ERROR.
            v-job = bf-tag.job-no + "-" + string(bf-tag.job-no2,"99").
      IF v-job BEGINS "-" THEN v-job = "".

      FIND first job-hdr WHERE job-hdr.company EQ g_company
                 /*AND job-hdr.job     EQ job.job*/
                 AND job-hdr.job-no  EQ bf-tag.job-no
                 AND job-hdr.job-no2 EQ bf-tag.job-no2 NO-LOCK NO-ERROR.

      IF NOT AVAIL cust THEN 
          FIND FIRST cust WHERE cust.company = g_company
                       AND cust.cust-no = job-hdr.cust-no
                       NO-LOCK NO-ERROR.
      v-tmp-mult   = if AVAIL cust AND cust.int-field[1] ne 0 THEN cust.int-field[1] 
                     else v-mult.

      FIND FIRST shipto
              WHERE shipto.company eq g_company
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
          ASSIGN
            v-ship-name  = shipto.ship-name
            v-ship-add1  = shipto.ship-add[1]
            v-ship-add2  = shipto.ship-add[2]
            v-ship-city  = shipto.ship-city
            v-ship-state = shipto.ship-state
            v-ship-zip   = shipto.ship-zip.
      FIND FIRST oe-ord WHERE oe-ord.company = g_company
                         AND oe-ord.ord-no = bf-tag.ord-no NO-LOCK NO-ERROR.
      FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.ord-no = bf-tag.ord-no
                           AND oe-ordl.i-no = bf-tag.i-no NO-LOCK NO-ERROR.
      v-due-date     = if AVAIL oe-ord AND oe-ord.due-date ne ? then oe-ord.due-date
                       ELSE if AVAIL oe-ordl AND oe-ordl.req-date ne ? THEN oe-ordl.req-date
                       else today.

      IF AVAIL itemfg AND itemfg.est-no NE "" THEN
         FIND FIRST eb WHERE eb.company  EQ itemfg.company
                         AND eb.est-no   EQ itemfg.est-no
                         AND eb.stock-no EQ itemfg.i-no NO-LOCK NO-ERROR.
      ASSIGN v-flute = IF AVAIL eb THEN eb.flute ELSE ""
             v-test  = IF AVAIL eb THEN eb.test ELSE ""
             v-cas-no = IF AVAIL eb THEN eb.cas-no ELSE ""
             v-gross-wt = 0
             v-tare-wt = 10
             v-net-wt = 0
             v-sheet-wt = 0
             v-uom = "EA"
             lv-middlesex-po  = SUBSTR(TRIM(bf-tag.job-no),1,6)
             lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
             lv-middlesex-po  = SUBSTR(TRIM(v-cust-po-no),1,6)
             lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" 
                         ELSE "BNJ" + FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                              TRIM(lv-middlesex-po).

      DO i = 1 TO (v-num-of-tags * v-tmp-mult):
             PUT "~""  cust.name  "~","
              "~""  v-job  "~","
              "~""  caps(bf-tag.i-no)  FORM "x(15)" "~","
              "~""  itemfg.part-no  "~","
              "~""  v-cust-po-no  "~","
              bf-tag.qty-case  ","
              bf-tag.case-bundle  ","
              bf-tag.pallet-count FORM ">>>>>>>9" ","
              "~""  v-ship-add1  "~","
              "~""  v-ship-add2  "~","
              "~""  v-ship-city  "~","
              "~""  v-ship-state  "~","
              "~""  v-ship-zip  "~","
              "~""  itemfg.i-name  "~","
              "~""  v-due-date  "~","
              "~""  v-rel-date  "~","
              "~""  itemfg.upc-no  "~","
              "~""  itemfg.l-score[50]  "~","
              "~""  itemfg.w-score[50] "~","
              "~""  itemfg.d-score[50] "~","
              "~""  v-flute  "~","
              "~""  v-test  "~","
              "~""  company.name  "~","
              v-gross-wt  ","
              v-tare-wt  ","
              v-net-wt  ","
              v-sheet-wt  ","
              "~""  v-uom  "~","
              "~""  lv-middlesex-job  "~","
              "~""  lv-middlesex-po  "~","
              "~""  bf-tag.tag-no "~"," 
              "~""  bf-tag.partial "~","
              "~""  v-cas-no "~",".
              put skip.
      END.
      IF lookup(v-loadtag,"SSLABEL,CentBox") = 0 THEN
      do v-count = 1 to v-tmp-mult: /* for partial print */
                /* loadtags generation */
             PUT "~""  cust.name  "~","
              "~""  v-job  "~","
              "~""  caps(bf-tag.i-no)  FORM "x(15)" "~","
              "~""  itemfg.part-no  "~","
              "~""  v-cust-po-no  "~","
              bf-tag.qty-case  ","
              bf-tag.case-bundle  ","              
              "~""  v-ship-add1  "~","
              "~""  v-ship-add2  "~","
              "~""  v-ship-city  "~","
              "~""  v-ship-state  "~","
              "~""  v-ship-zip  "~","
              "~""  itemfg.i-name  "~","
              "~""  v-due-date  "~","
              "~""  v-rel-date  "~","
              "~""  itemfg.upc-no  "~","
              "~""  itemfg.l-score[50]  "~","
              "~""  itemfg.w-score[50] "~","
              "~""  itemfg.d-score[50] "~","
              "~""  v-flute  "~","
              "~""  v-test  "~","
              "~""  company.name  "~","
              v-gross-wt  ","
              v-tare-wt  ","
              v-net-wt  ","
              v-sheet-wt  ","
              "~""  v-uom  "~","
              "~""  lv-middlesex-job  "~","
              "~""  lv-middlesex-po  "~","
              "~""  bf-tag.tag-no "~"," 
              "~""  bf-tag.partial "~","
              .
              put skip.
      end.

      IF bf-tag.sts BEGINS "Transfer" THEN bf-tag.sts = "Printed".
      
  end.  /* avail bf-tag */
  output close.
  IF scr-auto-print THEN
      RUN AutoPrint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprint-tag D-Dialog 
PROCEDURE reprint-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLoadtagFile AS CHARACTER NO-UNDO.
    
    FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                 AND loadtag.item-type   EQ NO
                 AND loadtag.tag-no  eq TRIM(begin_tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO scr-label-file.
      RETURN ERROR.
  END.      

  ASSIGN
      cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
  
  RUN create-w-ord.

  SESSION:SET-WAIT-STATE ("general").
  {sys/inc/print1.i}
  {sys/inc/outprint.i value(lines-per-page)} 
      VIEW FRAME r-top.
      VIEW FRAME top.

  IF cBarCodeProgram EQ 'Loftware' then 
    cLoadtagFile = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + SUBSTRING(STRING(NOW),21,3) + '.csv'.
  ELSE cLoadtagFile = 'loadtag.txt'.

  IF v-out = "" THEN v-out = "c:~\ba~\label~\" + cLoadtagFile. 
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + cLoadtagFile.
  END.
  IF begin_filename:SCREEN-VALUE = "" THEN 
       begin_filename:SCREEN-VALUE = v-out.
  
  RUN create-text-file.
 
  IF NOT is-from-addon() THEN
  MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.
  SESSION:SET-WAIT-STATE ("").
  
  IF cBarCodeProgram EQ "" THEN do:    
     RUN AutoPrint.
 END.
 ELSE IF cBarCodeProgram EQ "xprint" AND scr-auto-print THEN do:
     PAUSE 1.
     RUN print-loadtg  .
 END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-loadtag-line D-Dialog 
PROCEDURE write-loadtag-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipc-rfid AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipc-totalUnit AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipi-pallet-id AS INT NO-UNDO.

 PUT UNFORMATTED "~""  removeChars(w-ord.cust-name)  "~","
  w-ord.ord-no  ","
  "~""  v-job  "~","
  "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~","
  "~""  removeChars(w-ord.cust-part-no) "~","
  "~""  removeChars(w-ord.cust-po-no)  "~","
  w-ord.pcs  ","
  w-ord.bundle  ","
  trim(ipc-totalUnit) ","
  "~""  removeChars(w-ord.ship-code)  "~","
  "~""  removeChars(w-ord.ship-name)  "~","
  "~""  removeChars(w-ord.ship-add1)  "~","
  "~""  removeChars(w-ord.ship-add2)  "~","
  "~""  removeChars(w-ord.ship-city)  "~","
  "~""  removeChars(w-ord.ship-state) "~","
  "~""  removeChars(w-ord.ship-ctry)  "~","
  "~""  removeChars(w-ord.ship-zip)   "~","
  "~""  removeChars(w-ord.sold-code)  "~","
  "~""  removeChars(w-ord.sold-name)  "~","
  "~""  removeChars(w-ord.sold-add1)  "~","
  "~""  removeChars(w-ord.sold-add2)  "~","
  "~""  removeChars(w-ord.sold-city)  "~","
  "~""  removeChars(w-ord.sold-state) "~","
  "~""  removeChars(w-ord.sold-ctry)  "~","
  "~""  removeChars(w-ord.sold-zip)   "~","
  "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
  "~""  w-ord.due-date  "~","
  "~""  w-ord.rel-date  "~","
  "~""  w-ord.upc-no  "~","
  "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.flute  "~","
  "~""  w-ord.test  "~","
  "~""  w-ord.vendor  "~","
  w-ord.gross-wt  ","
  w-ord.tare-wt  ","
  w-ord.net-wt  ","
  w-ord.sheet-wt  ","
  "~""  w-ord.uom  "~","
  "~""  removeChars(w-ord.style) "~","
  "~""  removeChars(w-ord.style-desc) "~","
  "~""  removeChars(w-ord.rel-lot#) "~","
  "~""  lv-middlesex-job  "~","
  "~""  lv-middlesex-po  "~","
  "~""  loadtag.tag-no "~"," 
  "~""  loadtag.partial "~","
  "~""  w-ord.cas-no  "~","
  "~""  removeChars(v-dept-note[1]) "~","
  "~""  removeChars(v-dept-note[2]) "~","
  "~""  removeChars(v-dept-note[3]) "~","
  "~""  removeChars(v-dept-note[4]) "~","
  "~""  removeChars(v-dept-note[5]) "~","
  "~""  removeChars(v-dept-note[6]) "~","
  "~""  removeChars(v-dept-note[7]) "~","
  "~""  removeChars(v-dept-note[8]) "~","
  w-ord.po-no ","
  "~""  removeChars(v-dept-note[9]) "~","
  "~""  removeChars(v-dept-note[10]) "~","
  "~""  removeChars(v-dept-note[11]) "~","
  "~""  removeChars(v-dept-note[12]) "~","
  "~""  removeChars(v-dept-note[13]) "~","
  "~""  removeChars(v-dept-note[14]) "~","
  "~""  removeChars(v-dept-note[15]) "~","
  "~""  removeChars(v-dept-note[16]) "~","   
  "~""  removeChars(v-dept-note[17]) "~","
  "~""  removeChars(v-dept-note[18]) "~","
  "~""  removeChars(w-ord.est-no) "~","
  "~""  removeChars(w-ord.ord-desc1)    "~","
  "~""  removeChars(w-ord.ord-desc2)    "~","
  .
 IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN DO:
    
/*    FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
    cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".  */

    PUT UNFORMATTED "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
                   "~"" ipc-rfid "~"," .
 END.
 PUT UNFORMATTED 
    "~"" w-ord.due-date-jobhdr "~"," 
    "~"" w-ord.due-date-job "~","
 /* gdm - 08130804 */
    "~"" w-ord.linenum "~","
 /* gdm - 07170905 */
    "~"" w-ord.unit-wt  "~","
    "~"" w-ord.pallt-wt  "~","          
 
 /* gdm - 10160905 */
    "~"" removeChars(v-fgdsc1) "~","
    "~"" removeChars(v-fgdsc2) "~","
    "~"" removeChars(v-fgdsc3) "~","
    "~"" removeChars(w-ord.lot) "~","
 /*bpv 05231205 */   
    "~"" w-ord.pallt-no "~"," 
    "~"" ipi-pallet-id "~"," .

 PUT SKIP.


 /* temp table for xprint */
IF cBarCodeProgram EQ "xprint" THEN do:
    CREATE tt-word-print .
    BUFFER-COPY w-ord TO tt-word-print .
    ASSIGN 
        tt-word-print.tag-no = loadtag.tag-no .
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-loadtg D-Dialog 
PROCEDURE print-loadtg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_print-view AS LOGICAL INIT YES NO-UNDO .
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    /*IF tb_print-view THEN DO:*/
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
   /* END.*/
    /*ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.*/

    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
                                
           IF scr-label-file:SCREEN-VALUE EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag2.xpr" THEN DO:
               {oe/rep/schcardstd.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
    END.


    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-from-addon D-Dialog 
FUNCTION is-from-addon RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEF VAR lWasFound AS LOG NO-UNDO.
lWasFound = NO.
hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):
    IF index(hProc:FILE-NAME, "addon") GT 0 THEN DO:
          lWasFound = YES.
          LEAVE. /* found it. */
    END.
    
    hProc = hProc:NEXT-SIBLING.
END.
RETURN lWasFound.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars D-Dialog 
FUNCTION removeChars RETURNS CHARACTER

  (ipField AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
  DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  /*k = NUM-ENTRIES(invalidChars).
  DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
  /*END.*/
  RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

