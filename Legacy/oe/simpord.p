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

DEF VAR iNextOrder# AS INT NO-UNDO.

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
      FIELD FgItem AS char
      FIELD CustPart AS char
      FIELD ItemQty AS INT
      FIELD  ItemUom AS char
      FIELD ItemPrice AS DEC
      FIELD ItemPO# AS char
      FIELD ItemDueDate AS DATE
      FIELD ItemEst# AS char
      FIELD ItemValid AS LOG
      FIELD NoteTITLE AS CHAR
      FIELD notes AS CHAR
      FIELD ShipTo AS CHAR
      FIELD ItemQuote# AS INT
      FIELD ShipFrom AS CHARACTER
      FIELD ShipToName AS CHARACTER 
      FIELD ShipToStreet AS CHARACTER 
      FIELD ShipToCity AS CHARACTER 
      FIELD ShipToState AS CHARACTER
      FIELD ShipToZip AS CHARACTER
      FIELD ShipToPhone AS CHARACTER
      FIELD ShipToContact AS CHARACTER 
      FIELD POLineNum AS INTEGER 
      .

DO TRANSACTION:
    {sys/inc/oereleas.i}
    {sys/inc/oeimport.i}    
END.



DEF VAR oeimportCompleted AS cha NO-UNDO.
DEF VAR gImportMultiFile AS LOG NO-UNDO.
DEF VAR gcImportError AS cha NO-UNDO.


/* for oe/oe-price.p ========*/
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */
DEF NEW SHARED VAR save_id AS RECID NO-UNDO.  /* RECORD ID FOR ORDER LINE */
DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEF NEW SHARED VAR price-ent AS LOG NO-UNDO.
DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.
def new shared var lv-qty as int no-undo.


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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetNextOrder# C-Win 
FUNCTION GetNextOrder# RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
ASSIGN 
       fcFileName:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fcMessage IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fcMessage:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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

  gImportMultiFile = NO.

  IF fcFileName <> "" THEN
    MESSAGE "Are you ready to import orders from "  fcFileName "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  ELSE DO:
      gImportMultiFile = YES.
      MESSAGE "Are you ready to import orders from folder "  oeimport-cha  "?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.
  END.


  IF v-process THEN RUN run-process.
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


{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
  


  RUN enable_UI.

  {methods/nowait.i}  

  DO WITH FRAME {&frame-name}:
      
  /* special */ RUN runProcess.
      
  /* special */ RETURN.
/*     find first oe-ctrl WHERE                               */
/*         oe-ctrl.company = cocode  EXCLUSIVE-LOCK NO-ERROR. */
/*      iNextOrder# = oe-ctrl.n-ord.                          */
     
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
     DEF INPUT PARAM ipFileName AS cha NO-UNDO.

     DEF VAR cInput AS cha NO-UNDO.
     DEF VAR iOrder# AS INT NO-UNDO.
     DEF VAR cPO# AS cha NO-UNDO.
     DEF VAR isPOSame AS LOG NO-UNDO.

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
                           ttHeader.DueDate = DATE( ENTRY(4,cInput))
                   
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
                           ttHeader.Est#  = ENTRY(10,cInput).
                IF NUM-ENTRIES(cInput) >= 11 THEN
                       ttHeader.Quote# = int(ENTRY(11,cInput)).
                
                /*iNextOrder# = iNextOrder# + 1.*/
    
          END.
          ELSE DO:
              IF NOT CAN-FIND(itemfg WHERE itemfg.company = cocode AND itemfg.i-no = ENTRY(2,cInput)) 
              THEN DO:
                        ASSIGN gcImportError = "Detail Error - No FG Item:" + entry(2,cInput).
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
                           ttDetail.ItemQuote# = int(ENTRY(12,cInput)) NO-ERROR.
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
    
     /* end of import */
    
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
     RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat).
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
            oe-ord.zip      = cust.zip
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
                            oe-ordl.LINE = iLine#
                            oe-ordl.i-no = ttDetail.FgItem
                            oe-ordl.part-no = ttDetail.CustPart
                            oe-ordl.qty = ttDetail.ItemQty
                            oe-ordl.pr-uom = ttDetail.ItemUom
                            oe-ordl.price = ttDetail.ItemPrice
                            oe-ordl.est-no = IF ttDetail.ItemEst# <> "" THEN ttDetail.ItemEst# ELSE oe-ord.est-no
                            oe-ordl.q-qty = oe-ord.t-fuel
                            oe-ordl.whsed = IF oe-ordl.est-no <> "" THEN YES ELSE NO
                            oe-ordl.managed = oe-ord.managed
                            oe-ordl.q-no = IF ttDetail.ItemQuote# <> 0 THEN ttDetail.ItemQuote# ELSE oe-ord.q-no
                            oe-ordl.e-num = ttDetail.POLineNum
                            oe-ordl.ship-id = ttDetail.ShipTo
                            .
                  IF oe-ordl.price = 0 THEN DO:
                     FIND FIRST xoe-ord OF oe-ord NO-LOCK.
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

                   {oe/ordltot3.i oe-ordl qty oe-ordl  }

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
                  RUN CreateRelease.
                  IF ttDetail.Notes <> "" THEN RUN CreateSpecNote (RECID(itemfg)).
                  IF ttDetail.ItemEst# <> "" AND
                     can-find(first est where est.company = cocode and est.loc = locode AND trim(est.est-no) = trim(ttDetail.ItemEst#) ) 
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
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateRelease C-Win 
PROCEDURE CreateRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR iNextRelNo AS INT NO-UNDO.
  
   find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "OECARIER"
               no-lock no-error.

    find first shipto
        where shipto.company eq oe-ord.company
          and shipto.cust-no EQ oe-ord.cust-no
          and shipto.ship-id eq ttDetail.ShipTo
        no-lock no-error.

     if not avail shipto then
    find first shipto where shipto.company eq cocode
                        and shipto.cust-no eq oe-ord.cust-no
         no-lock no-error.

    find first bf-oe-rel use-index seq-no no-lock no-error.
/*      10051225 */
/*      iNextRelNo = (if avail bf-oe-rel then bf-oe-rel.r-no else 0) + 1. */
    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT iNextRelNo).
    create oe-rel.
    assign oe-rel.company   = cocode
           oe-rel.loc       = locode
           oe-rel.ord-no    = oe-ordl.ord-no
           oe-rel.i-no      = oe-ordl.i-no
           oe-rel.cust-no   = oe-ord.cust-no
           oe-rel.po-no     = if oe-ordl.po-no ne "" then oe-ordl.po-no else oe-ord.po-no
           oe-rel.qty       = oe-ordl.qty /*- v-qty-sum */
           oe-rel.tot-qty   = oe-ordl.qty
           oe-rel.line      = oe-ordl.line
           oe-rel.s-comm[1] = oe-ord.s-comm[1]
           oe-rel.s-comm[2] = oe-ord.s-comm[2]
           oe-rel.s-comm[3] = oe-ord.s-comm[3]
           oe-rel.s-name[1] = oe-ord.sname[1]
           oe-rel.s-name[2] = oe-ord.sname[2]
           oe-rel.s-name[3] = oe-ord.sname[3]
           oe-rel.s-pct[1]  = oe-ord.s-pct[1]
           oe-rel.s-pct[2]  = oe-ord.s-pct[2]
           oe-rel.s-pct[3]  = oe-ord.s-pct[3]
           oe-rel.sman[1]   = oe-ord.sman[1]
           oe-rel.sman[2]   = oe-ord.sman[2]
           oe-rel.sman[3]   = oe-ord.sman[3]
           oe-rel.sold-no   = oe-ord.sold-no
           oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" and avail shipto then shipto.carrier
                              else oe-ord.carrier
           oe-rel.r-no      = iNextRelNo.
           
          IF oereleas-cha eq "LastShip" then
                               oe-rel.rel-date = oe-ord.last-date.
           ELSE IF oereleas-cha EQ "Due Date" THEN
                               oe-rel.rel-date = oe-ordl.req-date.
          ELSE /*DueDate+1Day*/
                            DO:
                               oe-rel.rel-date =oe-ordl.req-date + 1.
                              IF WEEKDAY(oe-rel.rel-date) EQ 7 THEN
                                  oe-rel.rel-date = oe-rel.rel-date + 2.
                               ELSE
                                  IF WEEKDAY(oe-rel.rel-date) EQ 1 THEN
                                     oe-rel.rel-date = oe-rel.rel-date + 1.
                            END.

      if avail shipto then
       assign oe-rel.ship-addr[1] = shipto.ship-addr[1]
              oe-rel.ship-city    = shipto.ship-city
              oe-rel.ship-state   = shipto.ship-state
              oe-rel.ship-zip     = shipto.ship-zip
              oe-rel.ship-no      = shipto.ship-no
              oe-rel.ship-id      = shipto.ship-id
              oe-rel.ship-i[1]    = shipto.notes[1]
              oe-rel.ship-i[2]    = shipto.notes[2]
              oe-rel.ship-i[3]    = shipto.notes[3]
              oe-rel.ship-i[4]    = shipto.notes[4]
              oe-rel.spare-char-1 = shipto.loc.
    else assign oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = oe-ord.sold-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4]
                oe-rel.spare-char-1 = oe-ord.loc.
    /* Assign itemfg-loc values */
    RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPrice C-Win 
PROCEDURE GetPrice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      oe/d-oeitem.w : get-price or get-price-hidden  
                   calls oe/oe-price.p and oe/getqpric.p  
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.

    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF VAR lv-save-xoe-ordl AS ROWID NO-UNDO.
    DEF BUFFER bf-oe-ordl FOR oe-ordl.

    def var lv-est-no as CHAR NO-UNDO.
    def var lv-price as dec no-undo.
    def var lv-pr-uom as cha no-undo.
    def var v-tmp-part as cha no-undo.
    def var v-qty as int no-undo.
    DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.

    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ip-ordl-rowid NO-ERROR.  
    IF NOT AVAIL bf-oe-ordl THEN RETURN NO-APPLY.

    FIND FIRST sys-ctrl-shipto WHERE sys-ctrl-shipto.company = cocode
                               AND sys-ctrl-shipto.NAME = "OEImport"
                               AND sys-ctrl-shipto.char-fld = bf-oe-ordl.cust NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl-shipto THEN oeimport-int = sys-ctrl-shipto.int-fld.

    DO:
      IF AVAIL bf-oe-ordl AND TRIM(bf-oe-ordl.est-no) <> "" THEN DO:

        IF NOT AVAIL xoe-ord THEN
        FIND FIRST xoe-ord WHERE xoe-ord.company EQ g_company
                             AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                             NO-LOCK NO-ERROR.

        ASSIGN
         save_id   = RECID(bf-oe-ordl)
         lv-rowid  = ROWID(bf-oe-ordl)
         v-i-item  = bf-oe-ordl.i-no
         v-i-qty   = INT(bf-oe-ordl.qty)
         v-qty-mod = YES. /* new record, so will have been modified */

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ v-i-item
            NO-LOCK NO-ERROR.

        IF AVAIL itemfg THEN DO:
          /*IF AVAIL xoe-ordl THEN lv-save-xoe-ordl= ROWID(xoe-ordl). */
          /* Depends on xoeitem */

          IF /*itemfg.i-code = "C" AND*/ bf-oe-ordl.est-no <> "" THEN DO:        
              lv-est-no = FILL(" ",8 - LENGTH(TRIM(bf-oe-ordl.est-no))) +
                   TRIM(bf-oe-ordl.est-no).
               find first xest where xest.company eq cocode and
                     xest.est-no eq lv-est-no no-lock no-error.
               assign lv-price = bf-oe-ordl.price
                   lv-pr-uom = bf-oe-ordl.pr-uom
                   lv-qty    = bf-oe-ordl.qty
                   v-tmp-part = bf-oe-ordl.i-no.

               run oe/getqprdt.p (recid(xest), bf-oe-ordl.part-no,
                                  v-tmp-part, oeimport-int,
                                  input-output lv-price,
                                  input-output lv-pr-uom,
                                  OUTPUT lv-q-no,
                                  INPUT-OUTPUT lv-qty).
               ASSIGN bf-oe-ordl.price  = lv-price
                      bf-oe-ordl.pr-uom = lv-pr-uom
                      .
          END.
       
        ELSE IF itemfg.i-code = "S" THEN DO:
          /*  Task 04151301 */
              RUN oe/oe-price.p.
          END.
          

          /*FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lv-rowid NO-ERROR. */
          /*{oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl} move to bottom */
  /*         IF lv-save-xoe-ordl NE ? THEN                              */
  /*             FIND xoe-ordl WHERE ROWID(xoe-ordl) = lv-save-xoe-ordl */
  /*                EXCLUSIVE-LOCK NO-ERROR.                            */

  /* MESSAGE "get-price: " bf-oe-ordl.i-no bf-oe-ordl.pr-uom */
  /*             bf-oe-ordl.price                            */
  /*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
        END.      
      END.  /* est-no <> "" */
      ELSE DO:
                
        IF AVAIL bf-oe-ordl AND TRIM(bf-oe-ordl.est-no) EQ "" THEN DO:
    
          IF NOT AVAIL xoe-ord THEN
          FIND FIRST xoe-ord WHERE xoe-ord.company EQ g_company
                               AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                               NO-LOCK NO-ERROR.
    
          ASSIGN
           save_id   = RECID(bf-oe-ordl)
           lv-rowid  = ROWID(bf-oe-ordl)
           v-i-item  = bf-oe-ordl.i-no
           v-i-qty   = INT(bf-oe-ordl.qty)
           v-qty-mod = YES. /* new record, so will have been modified */
    
          FIND FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ v-i-item
              NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
            /*IF AVAIL xoe-ordl THEN lv-save-xoe-ordl= ROWID(xoe-ordl). */
            /* Depends on xoeitem */
            IF itemfg.i-code = "C" AND bf-oe-ordl.est-no <> "" THEN DO:        
                lv-est-no = FILL(" ",8 - LENGTH(TRIM(bf-oe-ordl.est-no))) +
                     TRIM(bf-oe-ordl.est-no).
                 find first xest where xest.company eq cocode and
                       xest.est-no eq lv-est-no no-lock no-error.
                 assign lv-price = bf-oe-ordl.price
                     lv-pr-uom = bf-oe-ordl.pr-uom
                     lv-qty    = bf-oe-ordl.qty
                     v-tmp-part = bf-oe-ordl.i-no.
                 run oe/getqpric.p (recid(xest), bf-oe-ordl.part-no,
                                    v-tmp-part,
                                    input-output lv-price,
                                    input-output lv-pr-uom,
                                    OUTPUT lv-q-no,
                                    INPUT-OUTPUT lv-qty).
                 ASSIGN bf-oe-ordl.price  = lv-price
                        bf-oe-ordl.pr-uom = lv-pr-uom
                        .
            END.
            ELSE IF itemfg.i-code = "S" THEN DO:
                RUN oe/oe-price.p.
            END.
    
          END. /* avail itemfg */
        END. /* avail bf-oe-ordl */
      END. /* else (est-no blank) */

      IF AVAIL bf-oe-ordl AND bf-oe-ordl.q-no <> 0 THEN DO:
        FOR EACH quotehd WHERE quotehd.company EQ cocode
                          AND quotehd.loc     EQ locode
                          AND quotehd.q-no = bf-oe-ordl.q-no
                     NO-LOCK,
            EACH quoteitm OF quotehd WHERE quoteitm.part-no EQ bf-oe-ordl.part-no OR
                      (quoteitm.part-no EQ bf-oe-ordl.i-no AND bf-oe-ordl.i-no NE "") NO-LOCK,
            EACH quoteqty OF quoteitm WHERE quoteqty.qty = bf-oe-ordl.qty NO-LOCK:

            ASSIGN bf-oe-ordl.price  = quoteqty.price
                  bf-oe-ordl.pr-uom = quoteqty.uom
                  .
            LEAVE.
        END.        
      END.
      {oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl}
  END. /* Do: */


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

   EMPTY TEMP-TABLE  ttHeader.
   EMPTY TEMP-TABLE ttDetail.

   gcImportError = "".

   RUN BuildImpTable (ipFileName).
   IF gcImportError <> "" THEN RETURN .

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
        FIND FIRST bf-state-shipto
            WHERE bf-state-shipto.company EQ shipto.company
            AND bf-state-shipto.cust-no EQ shipto.cust-no
            AND bf-state-shipto.ship-id NE shipto.cust-no
            AND bf-state-shipto.ship-state EQ shipto.ship-state
            NO-LOCK NO-ERROR.
        FIND FIRST bf-cust NO-LOCK 
            WHERE bf-cust.company EQ shipto.company
            AND bf-cust.cust-no EQ shipto.cust-no
            NO-ERROR.
        shipto.tax-code = IF AVAILABLE bf-state-shipto  THEN bf-state-shipto.tax-code 
            ELSE IF AVAILABLE bf-default-shipto  THEN bf-shipto.tax-code 
            ELSE IF AVAILABLE bf-cust THEN bf-cust.tax-gr
            ELSE "".
        shipto.tax-mandatory = IF AVAIL(bf-state-shipto) THEN bf-state-shipto.tax-mandatory 
            ELSE IF AVAIL(bf-default-shipto) THEN bf-shipto.tax-mandatory 
            ELSE IF AVAILABLE bf-cust THEN bf-cust.sort EQ "Y"
            ELSE NO.
         
 
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
  DEF VAR cImportFileName AS cha FORM "x(60)"  NO-UNDO.
  DEF VAR cImportCompleted AS cha NO-UNDO.
  DEF VAR cImportErrored AS cha NO-UNDO.
  
/* Special*/  IF NOT oeimport-log THEN RETURN.    

 /* special */ DO :


    
    
    INPUT FROM OS-DIR(oeimport-cha) NO-ECHO.
    REPEAT:
         IMPORT  cImportFileName.
     
         IF cImportFileName BEGINS "." THEN NEXT.
         IF SUBSTRING(cImportFileName,LENGTH(cImportFileName) - 3,4) <> ".csv" THEN NEXT.

         /*FILE-INFO:FILE-NAME = cImportFileName.
         MESSAGE "file? " cImportFileName ":" FILE-INFO:FULL-PATHNAME ":" FILE-INFO:pathname
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         cImportFileName = FILE-INFO:FULL-PATHNAME.
         */
         
         cImportFileName =  IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR 
                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\" THEN oeimport-cha + cImportFileName
             ELSE oeimport-cha + "/" + cImportFileName.

          RUN ImportOrder (cImportFileName).

          IF gcImportError <> "" THEN DO:      /* error */
              
              IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR 
                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\"
                 THEN cImportErrored = SUBSTRING(oeimport-cha,1,LENGTH(oeimport-cha) - 1).
             ELSE cImportErrored = oeimport-cha.

             cImportErrored = cImportErrored + "Errored".
             OS-CREATE-DIR VALUE(cImportErrored).
             OS-COPY VALUE(cImportFileName) VALUE(cImportErrored).              
             IF OS-ERROR = 0 THEN 
                  OS-DELETE VALUE(cImportFileName).
             NEXT.
          END.
          ELSE DO:  /* successfully imported, move files to completed folder  */
             
             IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR 
                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\"
                 THEN cImportCompleted = SUBSTRING(oeimport-cha,1,LENGTH(oeimport-cha) - 1).
             ELSE cImportCompleted = oeimport-cha.

             cImportCompleted = cImportCompleted + "Completed".
             /*IF SEARCH(cImportCompleted) = ? THEN*/
              
              OS-CREATE-DIR VALUE(cImportCompleted).
              OS-COPY VALUE(cImportFileName) VALUE(cImportCompleted).
              
              IF OS-ERROR = 0 THEN 
                  OS-DELETE VALUE(cImportFileName).

              
          END.
     END.      /* repeat of input oeimport-cha */

  END. /* do */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetNextOrder# C-Win 
FUNCTION GetNextOrder# RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bf-oe-ord FOR oe-ord.
/*   find first oe-ctrl WHERE oe-ctrl.company = cocode NO-ERROR. */
/*   ASSIGN iNextOrder# = oe-ctrl.n-ord                          */
/*          oe-ctrl.n-ord = oe-ctrl.n-ord + 1                    */
/*          .                                                    */
/*   RELEASE oe-ctrl.                                            */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "order_seq", OUTPUT iNextOrder#) NO-ERROR.

  /* Supposed to be a new order number, so cannot be found on an existing order */
  DO WHILE CAN-FIND(FIRST bf-oe-ord
                    WHERE bf-oe-ord.company EQ cocode
                      AND bf-oe-ord.ord-no  EQ iNextOrder#):
    
      RUN sys/ref/asiseq.p (INPUT cocode, INPUT "order_seq", OUTPUT iNextOrder#) NO-ERROR.
   
    
  END.
  RETURN iNextOrder#.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

