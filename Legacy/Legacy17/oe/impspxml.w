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

/*def new shared buffer xest for est.                                            */
/*def new shared buffer xef for ef.                                              */
/*def new shared buffer xeb for eb.                                              */
/*                                                                               */
/*def buffer xop for est-op.                                                     */
/*                                                                               */
/*def new shared var xcal    as de no-undo.                                      */
/*def new shared var sh-wid  as de no-undo.                                      */
/*def new shared var sh-len  as de no-undo.                                      */
/*def new shared var fil_id  as recid no-undo.                                   */
/*def new shared var maxco   as int no-undo.                                     */
/*def new shared var qty     as int no-undo.                                     */
/*def new shared var v-qty-mod as log no-undo.                                   */
/*def new shared var nufile as log INITIAL YES no-undo.                          */
/*def NEW shared var v-create-job as   log    no-undo.  /* for job oe/estupl.p */*/
/*                                                                               */
/*DEF BUFFER oe-ord-whs-order FOR reftable.                                      */
/*DEF BUFFER oe-ordl-whs-item FOR reftable.                                      */

ASSIGN
  cocode = gcompany
  locode = gloc.

/*DEF BUFFER bf-oe-rel FOR oe-rel.                                            */
/*                                                                            */
{XMLOutput/ttNodes.i NEW}
{cXML/cXMLOrderFunc.i}
/*                                                                            */
/*                                                                            */
DO TRANSACTION:
/*    {sys/inc/oereleas.i}*/
    {sys/inc/oeimport.i}
END.
/*                                                                            */
DEF VAR oeimportCompleted AS cha NO-UNDO.
DEF VAR gImportMultiFile AS LOG NO-UNDO.
DEF VAR gcImportError AS cha NO-UNDO.

/*                                                                            */
/*/* for oe/oe-price.p ========*/                                             */
/*DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */ */
/*DEF NEW SHARED VAR save_id AS RECID NO-UNDO.  /* RECORD ID FOR ORDER LINE */*/
/*DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */     */
/*DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */   */
/*DEF NEW SHARED VAR price-ent AS LOG NO-UNDO.                                */
/*DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.                             */
/*DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.                                   */
DEF VAR llBatchMode AS LOG NO-UNDO.
DEF VAR lcProgStack AS CHAR NO-UNDO.
/*                                                                            */
/*{ce/print4a.i "new shared"}                                                 */

/*find first sys-ctrl where sys-ctrl.company eq cocode           */
/*                        and sys-ctrl.name    eq "JOBCREAT"     */
/*                        no-lock no-error.                      */
/*v-create-job = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.*/

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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
      gImportMultiFile = YES.
      MESSAGE "Are you ready to import orders from folder "  oeimport-cha  "?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.
  END.

  IF v-process THEN RUN runProcess.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
                 filters "SPS XML Files    (*.xml)" "*.xml",
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

lcProgStack =
PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
llBatchMode = INDEX(lcProgStack, "mainmenu") EQ 0.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
llBatchMode = NO.
&ENDIF

IF llBatchMode THEN
    CURRENT-WINDOW:HIDDEN = TRUE.
ELSE
    CURRENT-WINDOW:HIDDEN = FALSE.
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF NOT llBatchMode THEN
  RUN enable_UI.

/*  /* 05291402 */                                 */
/*  cXMLImport:HIDDEN = NOT CAN-FIND(FIRST sys-ctrl*/
/*              WHERE sys-ctrl.company EQ cocode   */
/*                AND sys-ctrl.name EQ 'cXMLOrder' */
/*                AND sys-ctrl.log-fld EQ YES).    */

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

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/*{cXML/cXMLOrderProc.i} /* 05291402 */*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateSpecNote C-Win 
PROCEDURE CreateSpecNote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF INPUT PARAM ipRecId AS RECID NO-UNDO.               */
/*                                                          */
/*  FIND itemfg WHERE RECID(itemfg) = ipRecid NO-LOCK.      */
/*                                                          */
/*  /*FIRST notes WHERE                                     */
/*               notes.rec_key = itemfg.rec_key AND         */
/*               notes.note_type = "S" NO-LOCK NO-ERROR.    */
/*  */                                                      */
/*  CREATE notes.                                           */
/*  /*{methods/viewers/create/notes.i}*/                    */
/*  ASSIGN notes.rec_key = itemfg.rec_key                   */
/*         notes.note_type = "S"                            */
/*         notes.note_code = "CS"                           */
/*         notes.note_title = substring(ttDetail.Notes,1,30)*/
/*         notes.note_text =  ttDetail.Notes                */
/*         notes.note_date = TODAY                          */
/*         notes.note_time = TIME                           */
/*         notes.user_id = USERID("NOSWEAT").               */

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

   RUN oe/oe850imp.p (ipFileName).

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

  IF NOT llBatchMode THEN 
  SESSION:SET-WAIT-STATE("general").    

/*  /* Special to impord */                                                                                */
/*  IF NOT llBatchMode AND NOT gImportMultiFile THEN DO:  /* single file improt */                         */
/*      cImportFileName = fcFileName.                                                                      */
/*      RUN ImportOrder (cImportFileName).                                                                 */
/*  END.                                                                                                   */
/*  ELSE DO:                                                                                               */
/*     /* Exists in sImpOrd.p */                                                                           */
/*     /* multi file import from oeimport-cha*/                                                            */
/*                                                                                                         */
/*     INPUT FROM OS-DIR(oeimport-cha) NO-ECHO.                                                            */
/*     REPEAT:                                                                                             */
/*         IMPORT cImportFileName.                                                                         */
/*                                                                                                         */
/*         IF cImportFileName BEGINS "." THEN NEXT.                                                        */
/*         IF SUBSTRING(cImportFileName,LENGTH(cImportFileName) - 3,4) <> ".csv" THEN NEXT.                */
/*                                                                                                         */
/*         /*FILE-INFO:FILE-NAME = cImportFileName.                                                        */
/*         MESSAGE "file? " cImportFileName ":" FILE-INFO:FULL-PATHNAME ":" FILE-INFO:pathname             */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                          */
/*         cImportFileName = FILE-INFO:FULL-PATHNAME.                                                      */
/*         */                                                                                              */
/*         cImportFileName =  IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR                   */
/*                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\" THEN oeimport-cha + cImportFileName*/
/*             ELSE oeimport-cha + "/" + cImportFileName.                                                  */
/*   /* Special to impord */                                                                               */
/*   IF NOT llBatchMode THEN                                                                               */
/*         fcMessage:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Import file: " + cImportFileName + " ...".     */
/*                                                                                                         */
/*          RUN ImportOrder (cImportFileName).                                                             */
/*                                                                                                         */
/*          IF gcImportError <> "" THEN DO:      /* error */                                               */
/*              IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR                                 */
/*                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\"                                    */
/*                 THEN cImportErrored = SUBSTRING(oeimport-cha,1,LENGTH(oeimport-cha) - 1).               */
/*             ELSE cImportErrored = oeimport-cha.                                                         */
/*                                                                                                         */
/*             cImportErrored = cImportErrored + "Errored".                                                */
/*             OS-CREATE-DIR VALUE(cImportErrored).                                                        */
/*              OS-COPY VALUE(cImportFileName) VALUE(cImportErrored).                                      */
/*              IF OS-ERROR = 0 THEN                                                                       */
/*                  OS-DELETE VALUE(cImportFileName).                                                      */
/*               NEXT.                                                                                     */
/*          END.                                                                                           */
/*          ELSE DO:  /* successfully imported, move files to completed folder  */                         */
/*             IF SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "/" OR                                  */
/*                 SUBSTRING(oeimport-cha,LENGTH(oeimport-cha),1) = "\"                                    */
/*                 THEN cImportCompleted = SUBSTRING(oeimport-cha,1,LENGTH(oeimport-cha) - 1).             */
/*             ELSE cImportCompleted = oeimport-cha.                                                       */
/*                                                                                                         */
/*             cImportCompleted = cImportCompleted + "Completed".                                          */
/*             /*IF SEARCH(cImportCompleted) = ? THEN*/                                                    */
/*                                                                                                         */
/*              OS-CREATE-DIR VALUE(cImportCompleted).                                                     */
/*              OS-COPY VALUE(cImportFileName) VALUE(cImportCompleted).                                    */
/*                                                                                                         */
/*              IF OS-ERROR = 0 THEN                                                                       */
/*                  OS-DELETE VALUE(cImportFileName).                                                      */
/*                                                                                                         */
/*                                                                                                         */
/*          END.                                                                                           */
/*     END.      /* repeat of input oeimport-cha */                                                        */
/*                                                                                                         */
/*                                                                                                         */
/*  END.  /* multi file input */                                                                           */

  IF NOT llBatchMode THEN DO:
    RUN ImportOrder (fcFileName).  

    /* Special to impord */
    ASSIGN  fcMessage:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    SESSION:SET-WAIT-STATE("").

    /* Special to impord */
    IF gcImportError = "" THEN
       MESSAGE TRIM(c-win:TITLE) + " EDI Import Process Is Completed.  "  VIEW-AS ALERT-BOX.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

