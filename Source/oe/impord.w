&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{cXml/ttOrd.i}
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

def buffer xop for est-op.

def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def new shared var fil_id  as recid no-undo.
def new shared var maxco   as int no-undo.
def new shared var qty     as int no-undo.
def new shared var v-qty-mod as log no-undo.
def new shared var nufile as log INITIAL YES no-undo.
def NEW shared var v-create-job as   log    no-undo.  /* for job oe/estupl.p */

DEF BUFFER oe-ord-whs-order FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.

ASSIGN
  cocode = gcompany
  locode = gloc.

DEF BUFFER bf-oe-rel FOR oe-rel.

DEF TEMP-TABLE ttHeader 
       FIELD Order# AS INT
       FIELD BillTo AS cha
       FIELD SoldTo AS cha
       FIELD ShipTo AS cha
       FIELD DueDate AS DATE
       FIELD Customer# AS cha
       FIELD CreditCard AS cha
       FIELD VCode AS cha
       FIELD CCExpDate AS DATE
       FIELD CCType AS cha
       FIELD Est# AS cha
       FIELD CustomerValid AS LOG
       FIELD po-no AS CHAR
       FIELD Quote# AS INT
       .

DEF TEMP-TABLE ttDetail
      FIELD Order# AS INT 
      FIELD FgItem AS CHAR
      FIELD CustPart AS CHAR
      FIELD ItemQty AS INT
      FIELD ItemUom AS CHAR
      FIELD ItemPrice AS DEC
      FIELD ItemPO# AS CHAR
      FIELD ItemDueDate AS DATE
      FIELD ItemEst# AS CHAR
      FIELD ItemValid AS LOG
      FIELD NoteTITLE AS CHAR
      FIELD notes AS CHAR
      FIELD ShipTo AS CHAR
      FIELD ShipFrom AS CHAR
      FIELD ItemQuote# AS INT
      FIELD ShipToName AS CHARACTER 
      FIELD ShipToStreet AS CHARACTER 
      FIELD ShipToCity AS CHARACTER 
      FIELD ShipToState AS CHARACTER 
      FIELD ShipToZip AS CHARACTER
      FIELD ShipToPhone AS CHARACTER
      FIELD ShipToContact AS CHARACTER 
      FIELD POLineNum AS INTEGER 
      .

/* rstark 05291402 */
{XMLOutput/ttNodes.i NEW}
{cXML/cXMLOrderFunc.i}
/* rstark 05291402 */

DO TRANSACTION:
    {sys/inc/oereleas.i}
    {sys/inc/oeimport.i}
END.

DEF VAR oeimportCompleted AS cha NO-UNDO.
DEF VAR gcImportError AS cha NO-UNDO.

/* for oe/oe-price.p ========*/
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */
DEF NEW SHARED VAR save_id AS RECID NO-UNDO.  /* RECORD ID FOR ORDER LINE */
DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEF NEW SHARED VAR price-ent AS LOG NO-UNDO.
DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
DEF VAR llBatchMode AS LOG NO-UNDO.
DEF VAR lcProgStack AS CHAR NO-UNDO.

{ce/print4a.i "new shared"}

find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "JOBCREAT"
                        no-lock no-error.
v-create-job = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-19 fcFileName btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fcFileName fcMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE BUTTON cXMLImport 
     LABEL "Import cXML Orders Only" 
     SIZE 33 BY 1.14.

DEFINE VARIABLE fcFileName AS CHARACTER FORMAT "X(256)" 
     LABEL "Import File:" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1.

DEFINE VARIABLE fcMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.95.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 2.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fcFileName AT ROW 2.67 COL 18 COLON-ALIGNED HELP
          "Enter file name to import order"
     cXMLImport AT ROW 5.29 COL 36 WIDGET-ID 6
     btn-process AT ROW 9.1 COL 26
     btn-cancel AT ROW 9.1 COL 57
     fcMessage AT ROW 12.43 COL 2 NO-LABEL WIDGET-ID 2
     RECT-17 AT ROW 1 COL 1
     RECT-19 AT ROW 1.95 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.8 BY 12.86
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Import Order"
         HEIGHT             = 12.86
         WIDTH              = 101.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON cXMLImport IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       cXMLImport:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       fcFileName:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fcMessage IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fcMessage:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import Order */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import Order */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
   
  IF NOT oeimport-log THEN DO:
      MESSAGE "Can't import orders. Contact System Administrator!"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.
  
  IF fcFileName <> "" AND SEARCH(fcFileName) = ? THEN DO:
      MESSAGE "Import file is not existing. "
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.

  
  IF fcFileName <> "" THEN
    MESSAGE "Are you ready to import orders from "  fcFileName "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.
  ELSE DO:
      MESSAGE "Use schedule monitor to import orders from folder "
           VIEW-AS ALERT-BOX INFORMATION.
  END.

  IF v-process THEN RUN runProcess.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cXMLImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cXMLImport C-Win
ON CHOOSE OF cXMLImport IN FRAME FRAME-A /* Import cXML Orders Only */
DO:
  RUN importOrder ('').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcFileName C-Win
ON HELP OF fcFileName IN FRAME FRAME-A /* Import File: */
DO:
    def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "Excel Comma delimited Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 initial-dir  oeimport-cha
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcFileName C-Win
ON LEAVE OF fcFileName IN FRAME FRAME-A /* Import File: */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-to_ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ASSIGN 
    lcProgStack = PROGRAM-NAME(1) 
            + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
            + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
            + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
            + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
            + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
            + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
    llBatchMode = INDEX(lcProgStack, "mainmenu")       EQ 0 AND 
        		  /* Removed to allow batch processing when launched from asiLogin       
			      INDEX(lcProgStack, "asilogin")         EQ 0 AND 
                  */
                  INDEX(lcProgStack, "SmartFrameWork") EQ 0
    &IF DEFINED(UIB_is_Running) NE 0 &THEN
    llBatchMode = NO
    &ENDIF
    CURRENT-WINDOW:HIDDEN = llBatchMode
    .

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  IF NOT llBatchMode THEN
  RUN enable_UI.

  /* 05291402 */
  cXMLImport:HIDDEN = NOT CAN-FIND(FIRST sys-ctrl
              WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name EQ 'cXMLOrder'
                AND sys-ctrl.log-fld EQ YES).

  {methods/nowait.i}  
  
  DO WITH FRAME {&frame-name}:
    IF llBatchMode THEN DO:
    /* special */ RUN runProcess.
    /* special */ RETURN.
    END.
    /* find first oe-ctrl WHERE */
    /*            oe-ctrl.company = cocode EXCLUSIVE-LOCK NO-ERROR. */
    /* iNextOrder# = oe-ctrl.n-ord. */
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{cXML/cXMLOrderProc.i} /* 05291402 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildImpTable C-Win 
PROCEDURE BuildImpTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ipFileName AS CHARACTER NO-UNDO.

 DEFINE VARIABLE cInput   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE iOrder#  AS INTEGER   NO-UNDO.
 DEFINE VARIABLE cPO#     AS CHARACTER NO-UNDO.
 DEFINE VARIABLE isPOSame AS LOGICAL   NO-UNDO.
 DEFINE VARIABLE cEstNo   AS CHARACTER NO-UNDO.

 IF ipFileName NE '' THEN DO: /* 05291402 */
   INPUT FROM VALUE(ipFileName) NO-ECHO.
   REPEAT:
       cInput = "".
        IMPORT UNFORMAT cInput.
        /*  No way to process empty lines */
        IF NUM-ENTRIES(cInput) LT 2 THEN 
          NEXT.
        IF ENTRY(1,cInput) = "H" THEN DO:
            IF NOT CAN-FIND(cust WHERE cust.company = cocode AND cust.cust-no = entry(2,cInput))
                THEN DO:
                      ASSIGN gcImportError = "Header Error - No Customer:" + entry(2,cInput).
                      RETURN.
            END.          

            CREATE ttHeader.
            ASSIGN ttHeader.Order# = GetNextOrder#()
                         ttHeader.BillTo = ENTRY(2,cInput)
                         ttHeader.SoldTo = ENTRY(3,cInput)
                           /*  ttHeader.ShipTo = ENTRY(4,cInput)*/
                         ttHeader.DueDate = DATE(ENTRY(4,cInput))
                         ttHeader.Customer# = ENTRY(5,cInput)
                         iOrder# = ttHeader.Order#.

            IF NUM-ENTRIES(cInput) >= 6 THEN
               ttHeader.CreditCard = ENTRY(6,cInput).
            IF NUM-ENTRIES(cInput) >= 7 THEN
               ttHeader.VCode = ENTRY(7,cInput).
            IF NUM-ENTRIES(cInput) >= 8 THEN
               ttHeader.CCExpDate = date(ENTRY(8,cInput)).
            IF NUM-ENTRIES(cInput) >= 9 THEN
               ttHeader.CCType = ENTRY(9,cInput).

            IF NUM-ENTRIES(cInput) >= 10 THEN
                ASSIGN
                    cEstNo        = ENTRY(10,cInput)
                    ttHeader.Est# = cEstNo
                    .
            IF NUM-ENTRIES(cInput) >= 11 THEN
                         ttHeader.Quote# = int(ENTRY(11,cInput)).

            /*iNextOrder# = iNextOrder# + 1.          */
        END.
        ELSE DO:
            IF NOT CAN-FIND(itemfg WHERE itemfg.company = cocode AND itemfg.i-no = ENTRY(2,cInput)) 
            THEN DO:
                          ASSIGN gcImportError = "Detail Error - No FG Item:" + entry(2,cInput).                        
                          RETURN.
            END.
            
            IF cEstNo NE "" THEN
                IF NOT CAN-FIND(FIRST eb 
                                WHERE eb.company      EQ cocode 
                                  AND TRIM(eb.est-no) EQ cEstNo 
                                  AND eb.part-no      EQ ENTRY(3,cInput) 
                                  AND eb.stock-no     EQ ENTRY(2,cInput)) 
                THEN DO:
                    gcImportError = "Detail Error - CustomerPart# or FG Item # is not matching on order# and estimate#".                        
                    RETURN.
                END.           
            CREATE ttDetail.
            ASSIGN ttDetail.Order# = iOrder#
                         ttDetail.FgItem = ENTRY(2,cInput)
                         ttDetail.CustPart = ENTRY(3,cInput)
                         ttDetail.ItemQty = int(ENTRY(4,cInput))
                         ttDetail.ItemUom = ENTRY(5,cInput)
                         ttDetail.ItemPrice = dec(ENTRY(6,cInput))
                         ttDetail.ItemPO# = ENTRY(7,cInput)
                        .
            IF NUM-ENTRIES(cInput) >= 8 AND ENTRY(8,cInput) <> "" THEN
                         ttDetail.ItemDueDate = DATE(ENTRY(8,cInput)).

            IF NUM-ENTRIES(cInput) >= 9 AND ENTRY(9,cInput) <> "" THEN
                         ttDetail.ItemEst# = ENTRY(9,cInput).

            IF NUM-ENTRIES(cInput) >= 10 THEN
                         ttDetail.Notes = ENTRY(10,cInput).

            IF NUM-ENTRIES(cInput) >= 11 THEN
                         ttDetail.ShipTo = ENTRY(11,cInput).
            IF NUM-ENTRIES(cInput) >= 12  THEN
                             ttDetail.ItemQuote# = INTEGER(ENTRY(12,cInput)) NO-ERROR.
            IF NUM-ENTRIES(cInput) >= 13 THEN
                         ttDetail.ShipFrom = ENTRY(13,cInput).
            IF NUM-ENTRIES(cInput) >= 14 THEN
                         ttDetail.ShipToName = ENTRY(14,cInput).
            IF NUM-ENTRIES(cInput) >= 15 THEN
                         ttDetail.ShipToStreet = ENTRY(15,cInput).
            IF NUM-ENTRIES(cInput) >= 16 THEN
                         ttDetail.ShipToCity = ENTRY(16,cInput).
            IF NUM-ENTRIES(cInput) >= 17 THEN
                         ttDetail.ShipToState = ENTRY(17,cInput).
            IF NUM-ENTRIES(cInput) >= 18 THEN
                         ttDetail.ShipToZip = ENTRY(18,cInput).
            IF NUM-ENTRIES(cInput) >= 19 THEN
                         ttDetail.ShipToPhone = ENTRY(19,cInput).
            IF NUM-ENTRIES(cInput) >= 20 THEN
                         ttDetail.ShipToContact = ENTRY(20,cInput).
            IF NUM-ENTRIES(cInput) >= 21 THEN
                         ttDetail.POLineNum = INTEGER(ENTRY(21,cInput)) NO-ERROR.
        END.
   END.

   INPUT CLOSE.
   /* end of import */
   
   FIND FIRST ttDetail NO-LOCK 
      WHERE ttDetail.Order# EQ iOrder#
      NO-ERROR.
   IF AVAILABLE ttDetail THEN 
      FIND FIRST ttHeader 
          WHERE ttHeader.order# EQ iOrder#
          NO-ERROR.
   IF AVAILABLE ttHeader THEN 
       ttHeader.shipTo = ttDetail.ShipTo.
     
 END. /* ipfilename not blank 05291402 */

 /* validate customer and item */
 FOR EACH ttHeader:
     IF CAN-FIND(cust WHERE cust.company = cocode AND cust.cust-no = ttHeader.BillTo)
         THEN ASSIGN  ttHeader.CustomerValid = YES.
     IF ttHeader.Est# = "" AND ttHeader.Quote# <> 0 THEN DO:
            FIND quotehd WHERE quotehd.company = cocode AND quote.loc = locode
                           AND quotehd.q-no = ttHeader.Quote# NO-LOCK NO-ERROR.
            IF AVAIL quotehd THEN ttHeader.Est# = quotehd.est-no.
     END.     
     /*
     IF ttHeader.est# = "" AND CAN-FIND(FIRST ttDetail WHERE ttDetail.Order# = ttHeader.Order# AND ttDetail.ItemEst# <> "") 
     THEN DO:
           FIND FIRST ttDetail WHERE ttDetail.Order# = ttHeader.Order# AND ttDetail.ItemEst# <> "" NO-LOCK NO-ERROR.
           ttHeader.Est# = ttDetail.ItemEst#.
     END.
     */
     IF ttHeader.est# <> "" AND LENGTH(ttHeader.est#) < 8 THEN DO:
        ttHeader.est# = FILL(" ",8 - LENGTH(trim(ttHeader.est#)) ) + TRIM(ttHeader.est#).
     END.
 END.

 ASSIGN cPO# = ""
        isPOSame = YES.
 FOR EACH ttDetail:
     IF CAN-FIND(itemfg WHERE itemfg.company = cocode AND itemfg.i-no = ttDetail.FgItem)
          THEN ASSIGN ttDetail.ItemValid = YES.
     IF cPO# = "" THEN cPO# = ttDetail.ItemPO.
     IF cPO# <> ttDetail.ItemPO THEN isPOSame = NO.

     IF ttDetail.ItemEst# <> "" AND LENGTH(ttDetail.ItemEst#) < 8 THEN DO:
        ttDetail.ItemEst# = FILL(" ",8 - LENGTH(trim(ttDetail.ItemEst#)) ) + TRIM(ttDetail.ItemEst#).
     END.
 END.

 IF isPoSame THEN
     FOR EACH ttHeader:
         ttHeader.po-no = cPO#.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateJob C-Win 
PROCEDURE CreateJob :
/*------------------------------------------------------------------------------
  Purpose:     copied from oe/d-oeitem.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def VAR op-recid as recid no-undo. 
  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
  DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
    
  /* === from oe/oe-ord1.p  ============= */
         

  find last job where job.company eq cocode use-index job no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.

  if oe-ord.job-no <> "" then 
     assign v-job-no = oe-ord.job-no
            v-job-no2 =  oe-ord.job-no2.
  else
  if oe-ordl.job-no eq "" then do:
    FIND FIRST est
      WHERE est.company EQ cocode
        and est.est-no  EQ oe-ordl.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN  
       FIND FIRST eb
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.cust-no  EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    IF AVAIL eb THEN 
        v-prod-cat = eb.procat.
     v-job-no = fill(" ",6 - length(trim(string(oe-ordl.ord-no)))) + string(oe-ordl.ord-no).
     RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                      INPUT-OUTPUT v-job-no2,
                      INPUT v-prod-cat, 
                      INPUT oe-ordl.est-no).
     IF v-job-no NE "" THEN DO:
       assign
        oe-ordl.job-no  = v-job-no
        oe-ordl.job-no2 = v-job-no2.
  /*     display oe-ordl.job-no oe-ordl.job-no2 with frame {&frame-name}.*/
    end.
  END.
  ELSE
     IF oe-ordl.job-no NE "" THEN
        ASSIGN v-job-no = oe-ordl.job-no
               v-job-no2 = oe-ordl.job-no2.

  IF v-job-no NE "" THEN
     FOR EACH job
         WHERE job.company EQ cocode
           AND job.job-no  EQ v-job-no
           AND job.job-no2 EQ v-job-no2:
         DELETE job.
     END.

  create job.
  assign job.job        = v-job-job
         job.company    = cocode
         job.loc        = locode
         job.est-no     = oe-ordl.est-no
         job.job-no     = oe-ordl.job-no
         job.job-no2    = oe-ordl.job-no2
         job.stat       = "P"
         op-recid = recid(job).

   find first job-hdr where job-hdr.company eq cocode
                       and job-hdr.job-no  eq oe-ordl.job-no
                       and job-hdr.job-no2 eq oe-ordl.job-no2
                       and job-hdr.ord-no  eq oe-ordl.ord-no
                       and job-hdr.i-no    eq oe-ordl.i-no no-error.

   if not avail job-hdr then do:
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.

         create job-hdr.
         assign job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.e-num        = oe-ordl.e-num
                job-hdr.est-no       = oe-ordl.est-no
                job-hdr.i-no         = oe-ordl.i-no
            /*     job-hdr.qty          = oe-ordl.qty */
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
   end.
   
   assign job-hdr.est-no  = oe-ordl.est-no
          job-hdr.job     = job.job
          job-hdr.job-no  = job.job-no
          job-hdr.job-no2 = job.job-no2
          oe-ordl.j-no = job-hdr.j-no.

   RELEASE job-hdr.
   RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateOrder C-Win 
PROCEDURE CreateOrder :
/*------------------------------------------------------------------------------
  Purpose:      create tables:  rec_key, oe-ord, oe-ordl,oe-ordm, oe-rel, reftable
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iLine# AS INT NO-UNDO.      
  DEF VAR iOrder# AS INT NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cPO# AS cha NO-UNDO.
  DEFINE VARIABLE lOeAutoApproval AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hOrderProcs AS HANDLE NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMTotal AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDL AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMFO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMVO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDM AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hdCostProcs AS HANDLE.
  RUN system\CostProcs.p PERSISTENT SET hdCostProcs. 
  
  FOR EACH ttHeader BY ttHeader.Order#:
      IF NOT ttHeader.CustomerValid THEN NEXT.

      CREATE oe-ord.
      assign oe-ord.company = g_company
         oe-ord.loc = g_loc
         oe-ord.ord-date = today
         oe-ord.ord-no = ttHeader.Order#
         oe-ord.user-id = userid("nosweat")
         oe-ord.type = "O"
         oe-ord.stat = "W"  /* OW menu */
         oe-ord.due-code = "ON"
         oe-ord.cust-no = ttHeader.BillTo
         oe-ord.sold-id = ttHeader.SoldTo
         oe-ord.due-date = ttHeader.dueDate
         oe-ord.cc-num = ttHeader.CreditCard
         oe-ord.cc-expiration = ttHeader.CcExpDate
         oe-ord.cc-type = ttHeader.CCType
         oe-ord.spare-char-1 = ttHeader.VCode
         oe-ord.po-no = ttHeader.po-no
         oe-ord.est-no = ttHeader.Est#
         oe-ord.q-no = ttHeader.Quote#
         oe-ord.ship-id = ttHeader.ShipTo
         .

       find cust WHERE cust.company = cocode
                           AND cust.cust-no = oe-ord.cust-no no-lock no-error.
       assign oe-ord.cust-name = cust.name
            oe-ord.addr[1]   = cust.addr[1]
            oe-ord.addr[2]   = cust.addr[2]
            oe-ord.city      = cust.city
            oe-ord.state     = cust.state
            oe-ord.zip       = cust.zip
            oe-ord.contact   = cust.contact
           /* oe-ord.lead-days:screen-value = cust.ship-days 
            li-lead-days = cust.ship-days */
            oe-ord.last-date = IF oe-ord.due-date <> ? THEN oe-ord.due-date ELSE (oe-ord.ord-date + cust.ship-days)
            oe-ord.due-date  = IF oe-ord.due-date = ? THEN oe-ord.last-date ELSE oe-ord.due-date
            oe-ord.terms     = cust.terms
            oe-ord.over-pct  = cust.over-pct
            oe-ord.under-pct = cust.under-pct
            oe-ord.fob-code  = cust.fob-code
            oe-ord.frt-pay   = cust.frt-pay
            oe-ord.tax-gr    = cust.tax-gr
            oe-ord.sman[1]   = cust.sman
            oe-ord.s-pct[1] = 100.00
            oe-ord.carrier = cust.carrier
           .
     /*IF cust.cr-hold THEN oe-ord.stat = "H".*/

     find sman where sman.company = oe-ord.company
                and sman.sman = cust.sman
                no-lock no-error.
    if avail sman then assign oe-ord.sname[1] = sman.sname
                              oe-ord.s-comm[1] = sman.scomm
                              .

    find first terms where terms.company eq cocode
                        and terms.t-code  eq cust.terms
               no-lock no-error.
    if avail terms then  oe-ord.terms-d = terms.dscr.


        find first soldto where soldto.company = g_company and
                            soldto.cust-no = oe-ord.cust-no AND
                            soldto.sold-id = oe-ord.sold-id no-lock no-error.
        if avail soldto then 
        ASSIGN           
             oe-ord.sold-no = soldto.sold-no
             oe-ord.sold-name    = soldto.sold-name
             oe-ord.sold-addr[1] = soldto.sold-addr[1]
             oe-ord.sold-addr[2] = soldto.sold-addr[2]
             oe-ord.sold-city = soldto.sold-city
             oe-ord.sold-state = soldto.sold-state
             oe-ord.sold-zip = soldto.sold-zip.

  IF oe-ord.frt-pay = "B" THEN oe-ord.f-bill = YES.
  ELSE oe-ord.f-bill = NO.

  {custom/rec_key.i "oe-ord"}

      /* === create item === */
      ASSIGN iLine# = 0
             cPO# = "".
      FOR EACH ttDetail WHERE ttDetail.Order# = ttHeader.Order#:
             IF NOT ttDetail.ItemValid THEN NEXT.

             iLIne# = iLine# + 1.
             CREATE oe-ordl.
             ASSIGN oe-ordl.company   = cocode
                            oe-ordl.ord-no    = oe-ord.ord-no
                            oe-ordl.type-code = oe-ord.type
                            oe-ordl.cust-no   = oe-ord.cust-no
                            oe-ordl.po-no     = IF ttDetail.ItemPO# <> "" THEN ttDetail.ItemPO# ELSE oe-ord.po-no
                            oe-ordl.req-code  = oe-ord.due-code
                            oe-ordl.req-date  = IF ttDetail.ItemDueDate <> ? THEN ttDetail.ItemDueDate ELSE oe-ord.due-date
                            oe-ordl.prom-code = oe-ord.due-code
                            oe-ordl.prom-date = oe-ord.due-date
                            oe-ordl.disc = cust.disc
                            oe-ordl.tax  = cust.sort eq "Y" and oe-ord.tax-gr ne ""
                            oe-ordl.over-pct  = oe-ord.over-pct   
                            oe-ordl.under-pct = oe-ord.under-pct
                            oe-ordl.line = iLine#
                            oe-ordl.i-no = ttDetail.FgItem
                            oe-ordl.part-no = ttDetail.CustPart
                            oe-ordl.qty = ttDetail.ItemQty
                            oe-ordl.pr-uom = ttDetail.ItemUom
                            oe-ordl.price = ttDetail.ItemPrice
                            oe-ordl.est-no = IF ttDetail.ItemEst# <> "" THEN ttDetail.ItemEst# ELSE oe-ord.est-no
                            oe-ordl.q-qty = oe-ord.t-fuel
                            oe-ordl.whsed = IF oe-ordl.est-no <> "" THEN YES ELSE NO
                            oe-ordl.q-no = IF ttDetail.ItemQuote# <> 0 THEN ttDetail.ItemQuote# ELSE oe-ord.q-no
                            oe-ordl.managed = oe-ord.managed
                            oe-ordl.e-num = ttDetail.POLineNum
                            oe-ordl.ship-id = oe-ord.ship-id
                            oe-ordl.stat = "W"
                            .

                  IF oe-ordl.price = 0 THEN DO:                      
                     FIND FIRST xoe-ord OF oe-ord NO-LOCK.
                     /* getPrice found in oe/getPrice.i */
                     RUN getPrice (ROWID(oe-ordl)).
                  END.
                  do iCount = 1 to 3:
                        ASSIGN oe-ordl.s-man[iCount]  = oe-ord.sman[iCount]
                                     oe-ordl.s-pct[iCount]  = oe-ord.s-pct[iCount]
                                     oe-ordl.s-comm[iCount] = oe-ord.s-comm[iCount].
                  end.
                  FIND FIRST itemfg WHERE itemfg.company EQ g_company
                                         AND itemfg.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.

                  assign oe-ordl.i-name = itemfg.i-name
                             oe-ordl.cas-cnt = IF oe-ordl.qty < itemfg.case-count THEN oe-ordl.qty ELSE  itemfg.case-count 
                             oe-ordl.cases-unit =  itemfg.case-pall 
                             oe-ordl.part-dscr1 =  itemfg.part-dscr1
                             oe-ordl.part-dscr2 =  itemfg.part-dscr2 
                            .
                  RUN GetCostForFGItem IN hdCostProcs(oe-ordl.company,oe-ordl.i-no, OUTPUT dCostPerUOMTotal, OUTPUT dCostPerUOMDL,OUTPUT dCostPerUOMFO,
                                             OUTPUT dCostPerUOMVO,OUTPUT dCostPerUOMDM, OUTPUT cCostUOM , OUTPUT lFound) .
                  oe-ordl.cost = dCostPerUOMTotal .
                  oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000 .          

                   
                  RUN pAutoCreateShipTo (INPUT cocode, 
                                        INPUT oe-ord.cust-no, 
                                        INPUT ttDetail.ShipTo, 
                                        INPUT ttDetail.ShipToName,
                                        INPUT ttDetail.ShipToStreet,
                                        INPUT ttDetail.ShipToCity,
                                        INPUT ttDetail.ShipToState,
                                        INPUT ttDetail.ShipToZip,
                                        INPUT ttDetail.ShipToPhone,
                                        INPUT ttDetail.ShipToContact,
                                        OUTPUT ttDetail.ShipTo ).
                                        
                  /* createRelease found in oe/createRelease.i */
                  RUN createRelease (INPUT ttDetail.ShipTo,
                                     INPUT ttDetail.ShipFrom).
                  IF ttDetail.Notes <> "" THEN RUN CreateSpecNote (RECID(itemfg)).
                  IF ttDetail.ItemEst# <> "" AND
                     can-find(first est where est.company = cocode and est.loc = locode AND trim(est.est-no) = trim(ttDetail.Itemest#) )          
                     THEN do:
                        RUN createJob.
                        fil_id = RECID(oe-ordl).
                        FIND xoe-ord OF oe-ord NO-LOCK.
                        run oe/ordlup.p.         /* Update Inventory and Job Costing */
                        RUN oe/estupl.p.
                  END.


      END.

      /*if (ttHeader.est# <> "" and 
         can-find(first est where est.company = cocode and est.loc = locode AND est.est-no = ttHeader.est#))          
      THEN DO: 
          RUN createJob . 
          run oe/ordlup.p.         /* Update Inventory and Job Costing */
      END.
      */

      
      /* Determine autoapproval for this customer/shipto */
      lOEAutoApproval = NO.
      RUN sys/ref/nk1look.p (cocode, "OEAutoApproval", "L", YES /* use shipto */, YES /* use cust*/, oe-ord.cust-no, oe-ord.ship-id, 
                              OUTPUT cResult, OUTPUT lFound).
      IF lFound THEN
        lOEAutoApproval = LOGICAL(cResult) NO-ERROR.
      
      /* 52995 DSG Automated Ship To Creation */
      IF lOeAutoApproval AND AVAIL(oe-ord) THEN DO:
        RUN oe/OrderProcs.p PERSISTENT SET hOrderProcs.
        RUN ProcessImportedOrder IN hOrderProcs (ROWID(oe-ord), OUTPUT lError, OUTPUT cMessage).
        IF VALID-HANDLE(hOrderProcs) THEN 
            DELETE OBJECT hOrderProcs.
      END.
        
  END. /* Each tt-header */
  DELETE OBJECT hdCostProcs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createOrdJob C-Win 
PROCEDURE createOrdJob :
/*------------------------------------------------------------------------------
  Purpose:     copied from oe/copyOrder.i
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFrmCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipToCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEstno AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrdno AS INT NO-UNDO.
  DEFINE INPUT PARAMETER ipJobno like oe-ord.job-no no-undo.
  DEFINE INPUT PARAMETER ipJobno2 like oe-ord.job-no2 no-undo.
  DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.
  
 
  def output param op-recid as recid no-undo.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
    
  FIND CURRENT oe-ord.

  /* === from oe/oe-ord1.p  ============= */
  
  find last job where job.company eq ipToCompany no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.
  ASSIGN
   v-job-no  = ipJobno
   v-job-no2 = ipJobno2.

  FOR EACH job
      WHERE job.company EQ ipToCompany
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  create job.
  assign job.job        = v-job-job
         job.company    = ipToCompany
         job.loc        = ipLoc
         job.est-no     = ipEstno
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid       = recid(job).

  for each oe-ordl where oe-ordl.company eq ipToCompany
                     and oe-ordl.ord-no  eq ipOrdno exclusive:
      find first job-hdr no-lock
          where job-hdr.company eq ipToCompany
            and job-hdr.job-no  eq oe-ord.job-no
            and job-hdr.job-no2 eq oe-ord.job-no2
            and job-hdr.ord-no  eq ipOrdno
            and job-hdr.i-no    eq oe-ordl.i-no
          no-error.

      if not avail job-hdr then do:
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.   
         
         create job-hdr.
         assign job-hdr.company      = ipToCompany
                job-hdr.loc          = ipLoc
                job-hdr.est-no       = ipEstno
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      end.

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END.

      assign job-hdr.est-no  = ipEstno
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
             oe-ordl.est-no  = job-hdr.est-no
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no    = job-hdr.j-no.

      FIND CURRENT job-hdr NO-LOCK.
  end.

  FIND CURRENT job NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateSpecNote C-Win 
PROCEDURE CreateSpecNote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipRecId AS RECID NO-UNDO.

  FIND itemfg WHERE RECID(itemfg) = ipRecid NO-LOCK.

  /*FIRST notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_type = "S" NO-LOCK NO-ERROR.
  */
  CREATE notes.
  /*{methods/viewers/create/notes.i}*/
  ASSIGN notes.rec_key = itemfg.rec_key
         notes.note_type = "S"
         notes.note_code = "CS"
         notes.note_title = substring(ttDetail.Notes,1,30)
         notes.note_text =  ttDetail.Notes
         notes.note_date = TODAY
         notes.note_time = TIME
         notes.user_id = USERID("NOSWEAT").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fcFileName fcMessage 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-19 fcFileName btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getUseJobQty C-Win 
PROCEDURE getUseJobQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplUseJobQty AS LOGICAL     NO-UNDO.
oplUseJobQty = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportOrder C-Win 
PROCEDURE ImportOrder :
/*------------------------------------------------------------------------------
  Purpose:     create tables:  rec_key, oe-ord, oe-ordl,oe-ordm, oe-rel, reftable
                                            job... 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ipFileName AS cha NO-UNDO.

   EMPTY TEMP-TABLE ttHeader.
   EMPTY TEMP-TABLE ttDetail.

   gcImportError = "".

   RUN BuildImpTable (ipFileName).
   IF gcImportError <> "" THEN DO:
      /* Special to impord */
      IF NOT llBatchMode THEN
        MESSAGE gcImportError
            VIEW-AS ALERT-BOX error BUTTONS OK.
      RETURN .
   END.

   RUN createOrder.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAutoCreateShipTo C-Win
PROCEDURE pAutoCreateShipTo:
    /*------------------------------------------------------------------------------
     Purpose: AutoAdds a ShipTo for a given customer and address
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToId AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipAddress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipCity AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipState AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipZip AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPhone AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcContact AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipToID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iShipNo AS INTEGER NO-UNDO .
    DEFINE VARIABLE cArea AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-default-shipto FOR shipto.
    DEFINE BUFFER bf-state-shipto FOR shipto.
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-cust FOR cust.
    
    opcShipToID = ipcShipToID.
    RUN pParsePhone (INPUT ipcPhone, OUTPUT cArea, OUTPUT ipcPhone).
    FIND FIRST bf-default-shipto NO-LOCK 
        WHERE bf-default-shipto.company EQ ipcCompany
        AND bf-default-shipto.cust-no EQ ipcCustNo
        AND bf-default-shipto.ship-id EQ ipcCustNo
        NO-ERROR.
    
    FIND FIRST shipto NO-LOCK
        WHERE shipto.company EQ ipcCompany
        AND shipto.cust-no EQ ipcCustNo
        AND shipto.ship-id EQ ipcShipToID
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN 
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ ipcCompany
            AND shipto.cust-no EQ ipcCustNo
            AND shipto.ship-addr[1] EQ ipcShipAddress
            AND shipto.ship-city EQ ipcShipCity
            AND shipto.ship-state EQ ipcShipState
        NO-ERROR.
    IF NOT AVAILABLE shipto THEN      
    DO:
        FIND LAST bf-shipto NO-LOCK USE-INDEX ship-no
            WHERE bf-shipto.company EQ ipcCompany
            AND bf-shipto.cust-no EQ ipcCustNo NO-ERROR.
        iShipNo =  IF AVAILABLE bf-shipto THEN bf-shipto.ship-no + 1 ELSE 1.
        CREATE shipto.
        ASSIGN
            shipto.company      = ipcCompany
            shipto.cust-no      = ipcCustNo
            shipto.ship-no      = iShipNo
            shipto.ship-id      = IF ipcShipToID NE '' THEN ipcShipToID ELSE STRING(shipto.ship-no)
            shipto.contact      = ipcContact
            shipto.loc          = IF AVAIL(bf-default-shipto) THEN bf-default-shipto.loc ELSE locode
            shipto.area-code    = cArea
            shipto.phone        = ipcPhone
            shipto.ship-name    = ipcShipName
            shipto.ship-addr[1] = ipcShipAddress
            shipto.ship-city    = ipcShipCity
            shipto.ship-state   = ipcShipState
            shipto.ship-zip     = ipcShipZip
            shipto.country      = IF AVAIL(bf-default-shipto) THEN bf-default-shipto.country ELSE ""
            shipto.carrier      = IF AVAIL(bf-default-shipto) THEN bf-default-shipto.carrier ELSE ""
            shipto.dest-code    = IF AVAIL(bf-default-shipto) THEN bf-default-shipto.dest-code ELSE ""
            .
        
        FIND FIRST bf-cust NO-LOCK 
            WHERE bf-cust.company EQ shipto.company
            AND bf-cust.cust-no EQ shipto.cust-no
            NO-ERROR.
        shipto.tax-code =  IF AVAILABLE bf-cust THEN bf-cust.tax-gr ELSE "".
        shipto.tax-mandatory =  IF AVAILABLE bf-cust THEN bf-cust.sort EQ "Y" ELSE NO.
        shipto.spare-char-1 = IF AVAILABLE bf-cust THEN bf-cust.sman ELSE "" .
         
 
    END. /* not avail shipto */
    opcShipToID = shipto.ship-id.    
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParsePhone C-Win
PROCEDURE pParsePhone:
/*------------------------------------------------------------------------------
 Purpose: Tests the format of a Phone Number and converts to Area Code and Phone
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcFullPhone AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcAreaCode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcPhone AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
DEFINE VARIABLE iChar AS INTEGER NO-UNDO.

ipcFullPhone = REPLACE(ipcFullPhone,"(","").
ipcFullPhone = REPLACE(ipcFullPhone,")","").
ipcFullPhone = REPLACE(ipcFullPhone,"-","").
ipcFullPhone = REPLACE(ipcFullPhone,".","").
ipcFullPhone = REPLACE(ipcFullPhone,"+","").
ipcFullPhone = REPLACE(ipcFullPhone," ","").
iLength = LENGTH(ipcFullPhone).
IF iLength GT 7 THEN 
    ASSIGN 
        opcPhone = SUBSTRING(ipcFullPhone,iLength - 6, iLength)
        opcAreaCode = SUBSTRING(ipcFullPhone,1,iLength - 7)
        .        
ELSE 
    ASSIGN 
        opcPhone = ipcFullPhone
        opcAreaCode = ""
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runProcess C-Win 
PROCEDURE runProcess :
/*------------------------------------------------------------------------------
  Purpose:     copy order record
  Parameters:  <none>
  Notes:       copyOrder procedure is contained in oe/copyOrder.i include
------------------------------------------------------------------------------*/  
    DEFINE VARIABLE cImportFileName  AS CHARACTER NO-UNDO FORMAT "x(60)".
    DEFINE VARIABLE cImportCompleted AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImportErrored   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE attrList         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdFileSys        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cCompanyMask     AS CHARACTER NO-UNDO INITIAL "$company$". /* Mask character */
    DEFINE VARIABLE lValidateCompany AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cImportDir       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImportMainDir   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPath       AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCreated         AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cGCompany        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCocode          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileFullName    AS CHARACTER NO-UNDO.
    
    IF NOT llBatchMode THEN 
        SESSION:SET-WAIT-STATE("general").    
    
    RUN system\FileSysProcs.p PERSISTENT SET hdFileSys.
    
    /* Special to impord */
    IF NOT llBatchMode THEN DO:  /* single file improt */
        cImportFileName = fcFileName. 
        RUN ImportOrder (
            INPUT cImportFileName
            ).
    END.
    ELSE DO:  /* multi files import (Schedule Monitor) */
        IF oeimport-cha EQ "" THEN DO:
            MESSAGE "There is no defined location for the importing of orders, set this value in N-K-1 = OeImport" VIEW-AS ALERT-BOX INFORMATION.
            RETURN.
        END.
        ASSIGN
            cImportMainDir = ENTRY(1,oeimport-cha,cCompanyMask)
            cImportMainDir = IF SUBSTR(cImportMainDir,LENGTH(cImportMainDir),1) EQ "/" OR   /* Checks for forward or backward slashe at the end of dir. if found removes it */
                                SUBSTR(cImportMainDir,LENGTH(cImportMainDir),1) EQ "\" THEN
                                SUBSTR(cImportMainDir,1,LENGTH(cImportMainDir) - 1)
                             ELSE
                                 cImportMainDir
            cGCompany      = g_company /* Stores global company value into a temporary variable */
            cCocode        = cocode    /* Stores cocode into a temporary variable */
            .
          
        COMPANY-BLK:                       
        FOR EACH company NO-LOCK:
            lValidateCompany = INDEX(oeimport-cha,cCompanyMask) EQ 0. /* Checks whether NK1 directory has mask and current company is global company */
            
            IF lValidateCompany AND company.company NE g_company THEN 
               NEXT COMPANY-BLK.
	    
            ASSIGN
                cImportDir       = REPLACE(oeimport-cha,cCompanyMask,company.company) /* Replaces mask with company value */
                cImportDir       = IF SUBSTR(cImportDir,LENGTH(cImportDir),1) EQ "/" OR   /* Checks for forward or backward slashes at the end of dir. if found removes it */
                                      SUBSTR(cImportDir,LENGTH(cImportDir),1) EQ "\" THEN
                                      SUBSTR(cImportDir,1,LENGTH(cImportDir) - 1)
                                   ELSE
                                       cImportDir
                cImportCompleted = cImportMainDir + "\Completed\"
                cImportErrored   = cImportMainDir + "\Errored\"
                .  

            /* Checks whether import dir exists */    
            RUN FileSys_ValidateDirectory IN hdFileSys (
                INPUT  cImportDir,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
                   
            /* Creates import dir if it does not exists */ 
            IF NOT lValidPath THEN 
                RUN FileSys_CreateDirectory IN hdFileSys (
                    INPUT  cImportDir,
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ) NO-ERROR.
                       
            /* This assignment is required to populate cocode and g_company variables 
               with company code and global company code since as these variables are being 
               used in ImportOrder procedure */    
            ASSIGN
                g_company = company.company /* assigns global company with current company value */
                cocode    = g_company       /* assigns cocode with current company value */
                .
                
            IF NOT lValidateCompany THEN
                ASSIGN
                    cImportCompleted = cImportCompleted + company.company + '\' /* Path where completed files store */
                    cImportErrored   = cImportErrored   + company.company + '\' /* Path where errored files store */
                    .
	    
            /* Checks whether completed dir exists */
            RUN FileSys_ValidateDirectory IN hdFileSys (
                INPUT  cImportCompleted,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
                   
            /* Creates completed dir if it does not exists */    
            IF NOT lValidPath THEN    
               RUN FileSys_CreateDirectory IN hdFileSys (
                   INPUT  cImportCompleted,
                   OUTPUT lCreated,
                   OUTPUT cMessage
                   ) NO-ERROR.
                           
            /* Checks whether errored dir exists */
            RUN FileSys_ValidateDirectory IN hdFileSys (
                INPUT  cImportErrored,
                OUTPUT lValidPath,
                OUTPUT cMessage
                ) NO-ERROR.
                
            /* Creates errored dir if it does not exists */    
            IF NOT lValidPath THEN    
                RUN FileSys_CreateDirectory IN hdFileSys (
                    INPUT  cImportErrored,
                    OUTPUT lCreated,
                    OUTPUT cMessage
                    ) NO-ERROR. 
					
            INPUT FROM OS-DIR(cImportDir) NO-ECHO.
            REPEAT:
                SET cImportFileName ^ attrList.
            
                /* skips directories and files which are not csv */
                IF attrList NE 'f' OR cImportFileName BEGINS '.' OR
                    INDEX(cImportFileName,'.csv') EQ 0 THEN NEXT.
                  
				cFileFullName = cImportDir + "\" + cImportFileName.

				RUN ImportOrder (
					INPUT cFileFullName
					).
				
				IF gcImportError NE "" THEN DO:      /* error */
				
					OS-COPY VALUE(cFileFullName) VALUE(cImportErrored).  
					
					IF OS-ERROR EQ 0 THEN 
						OS-DELETE VALUE(cFileFullName).
					NEXT.
					
				END.
				ELSE DO:  /* successfully imported, move files to completed folder  */
					
					OS-COPY VALUE(cFileFullName) VALUE(cImportCompleted).
					
					IF OS-ERROR EQ 0 THEN 
						OS-DELETE VALUE(cFileFullName).
				    
                        
                END.
            END. /* repeat of input importdir */
        
        END. /* each company */
        ASSIGN
            g_company = cGCompany /* Assigns back temporary variable into global company */
            cocode    = cCocode   /* Assigns back temporary variable into cocode */
            .
         
    END.  /* multi file input */
    
	IF NOT llBatchMode THEN DO:
        SESSION:SET-WAIT-STATE("").
      
        /* Special to impord */
        IF gcImportError = "" THEN
            MESSAGE TRIM(c-win:TITLE) + " Process Is Completed.   Order#: "   iNextOrder#  VIEW-AS ALERT-BOX.    
    END.
    DELETE OBJECT hdFileSys.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

