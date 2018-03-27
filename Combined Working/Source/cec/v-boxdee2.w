&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec\v-boxdee.w

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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
def var li-cnt as int no-undo.
def var li-line-no as int extent 99 no-undo.

def new shared var cocode as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
{cec/descalc.i new}
def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO like box-design-line.
def var lv-wscore like box-design-hdr.wscore no-undo.
def var lv-wcum-score like box-design-hdr.wcum-score no-undo.
DEF VAR ll-is-3d-displayed AS LOG NO-UNDO.
DEF VAR v-score-more AS LOG NO-UNDO.
DEF VAR v-cur-position AS INT NO-UNDO.
DEF VAR li-lscore-len AS INT INIT 80 NO-UNDO.

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES box-design-hdr
&Scoped-define FIRST-EXTERNAL-TABLE box-design-hdr


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR box-design-hdr.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS box-design-hdr.description ~
box-design-hdr.box-image box-design-hdr.lscore 
&Scoped-define ENABLED-TABLES box-design-hdr
&Scoped-define FIRST-ENABLED-TABLE box-design-hdr
&Scoped-Define ENABLED-OBJECTS box-image-2 RECT-40 btn_right btn_left ~
editor_wcum-score editor_wscore 
&Scoped-Define DISPLAYED-FIELDS box-design-hdr.design-no ~
box-design-hdr.description box-design-hdr.box-image box-design-hdr.lscore ~
box-design-hdr.lcum-score 
&Scoped-define DISPLAYED-TABLES box-design-hdr
&Scoped-define FIRST-DISPLAYED-TABLE box-design-hdr
&Scoped-Define DISPLAYED-OBJECTS editor_wcum-score editor_wscore 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS box-design-hdr.description 
&Scoped-define ADM-ASSIGN-FIELDS box-design-hdr.lcum-score ~
editor_wcum-score editor_wscore 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_left 
     IMAGE-UP FILE "adm2/image/prev.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btn_right 
     IMAGE-UP FILE "adm2/image/next.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE editor_wcum-score AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 14 BY 12.38
     FONT 0 NO-UNDO.

DEFINE VARIABLE editor_wscore AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 15 BY 12.38
     FONT 0.

DEFINE IMAGE box-image-2
     SIZE 165 BY 21.76.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200 BY 25.45.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     box-design-hdr.design-no AT ROW 1.24 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1.1
     box-design-hdr.description AT ROW 1.24 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     box-design-hdr.box-image AT ROW 1.24 COL 75 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
     box-design-hdr.box-3d-image AT ROW 1.24 COL 75 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          BGCOLOR 14 
     box-design-hdr.lscore AT ROW 2.43 COL 2 NO-LABEL FORMAT "x(210)"
          VIEW-AS FILL-IN 
          SIZE 116 BY 1
          FONT 0
     btn_right AT ROW 2.43 COL 118
     box-design-hdr.lcum-score AT ROW 3.38 COL 2 NO-LABEL FORMAT "x(210)"
          VIEW-AS FILL-IN 
          SIZE 116 BY 1
          FONT 0
     btn_left AT ROW 3.38 COL 118
     box-design-hdr.box-text AT ROW 4.57 COL 2 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 116 BY 12.62
          FONT 0
     editor_wcum-score AT ROW 4.81 COL 170 HELP
          "Enter the cumulative width score." NO-LABEL
     editor_wscore AT ROW 4.81 COL 184 NO-LABEL
     "Score:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 122
     "W Totals   W Score" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 4.1 COL 170
     "Total" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.33 COL 122
     box-image-2 AT ROW 4.57 COL 2
     RECT-40 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.box-design-hdr
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 19.86
         WIDTH              = 149.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN box-design-hdr.box-3d-image IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       box-design-hdr.box-3d-image:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR EDITOR box-design-hdr.box-text IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       box-design-hdr.box-text:HIDDEN IN FRAME F-Main           = TRUE
       box-design-hdr.box-text:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN box-design-hdr.description IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN box-design-hdr.design-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR editor_wcum-score IN FRAME F-Main
   2                                                                    */
ASSIGN 
       editor_wcum-score:RETURN-INSERTED IN FRAME F-Main  = TRUE
       editor_wcum-score:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR editor_wscore IN FRAME F-Main
   2                                                                    */
ASSIGN 
       editor_wscore:RETURN-INSERTED IN FRAME F-Main  = TRUE
       editor_wscore:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN box-design-hdr.lcum-score IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-LABEL EXP-FORMAT                             */
ASSIGN 
       box-design-hdr.lcum-score:AUTO-RESIZE IN FRAME F-Main      = TRUE.

/* SETTINGS FOR FILL-IN box-design-hdr.lscore IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
ASSIGN 
       box-design-hdr.lscore:AUTO-RESIZE IN FRAME F-Main      = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME box-design-hdr.box-3d-image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.box-3d-image V-table-Win
ON HELP OF box-design-hdr.box-3d-image IN FRAME F-Main /* 3D Image File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
    DEF VAR cInitDir AS CHARACTER NO-UNDO.
   DEF VAR llInitDir AS CHARACTER NO-UNDO.

   RUN sys/ref/nk1look.p (g_company, "DefaultDir", "C", no, no, "", "", 
                          Output cInitDir, output llInitDir).
   IF cInitDir NE "" THEN
       ASSIGN
       FILE-INFO:FILE-NAME = cInitDir
      cInitDir = FILE-INFO:FULL-PATHNAME .
   IF cInitDir = ? THEN cInitDir = "" .

   system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "JPG Files    (*.jpg)" "*.jpg",
                         "Bitmap files (*.bmp)" "*.bmp",
                         "JPEG Files   (*.jpeg)" "*.jpeg",
                         "TIF Files    (*.tif)" "*.tif",
                         "All Files    (*.*) " "*.*"
                 initial-dir cInitDir
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME box-design-hdr.box-image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.box-image V-table-Win
ON HELP OF box-design-hdr.box-image IN FRAME F-Main /* Image File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   DEF VAR cInitDir AS CHARACTER NO-UNDO.
   DEF VAR llInitDir AS CHARACTER NO-UNDO.

   RUN sys/ref/nk1look.p (g_company, "DefaultDir", "C", no, no, "", "", 
                          Output cInitDir, output llInitDir).
   IF cInitDir NE "" THEN
       ASSIGN
       FILE-INFO:FILE-NAME = cInitDir
      cInitDir = FILE-INFO:FULL-PATHNAME .
   IF cInitDir = ? THEN cInitDir = "" .

   system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "JPG Files    (*.jpg)" "*.jpg",
                         "Bitmap files (*.bmp)" "*.bmp",
                         "JPEG Files   (*.jpeg)" "*.jpeg",
                         "TIF Files    (*.tif)" "*.tif",
                         "All Files    (*.*) " "*.*"
                 initial-dir cInitDir
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_left
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_left V-table-Win
ON CHOOSE OF btn_left IN FRAME F-Main
DO:
  /* APPLY KEYCODE('home') TO box-design-hdr.lscore. */
   box-design-hdr.lscore:SCREEN-VALUE = 
               substring(box-design-hdr.lscore,1,li-lscore-len).
   box-design-hdr.lcum-score:SCREEN-VALUE = 
               substring(box-design-hdr.lcum-score,1,li-lscore-len).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_right V-table-Win
ON CHOOSE OF btn_right IN FRAME F-Main
DO:
  /* v-score-more = NOT v-score-more.
   IF v-score-more THEN   DO: 

      /*APPLY KEYCODE('end') TO box-design-hdr.lscore.*/
      box-design-hdr.lscore:SCREEN-VALUE = SUBSTRING(box-design-hdr.lscore,51,80).
      box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80).
               /*substring(box-design-hdr.lcum-score, LENGTH(substring(box-design-hdr.lcum-score,80)) )*/.
   END.
   /*
   ELSE DO: 
      APPLY KEYCODE('home') TO box-design-hdr.lscore. 
      box-design-hdr.lcum-score:SCREEN-VALUE = 
               substring(box-design-hdr.lcum-score,1,80).
   END.
   */
   */
  ASSIGN  box-design-hdr.lscore:SCREEN-VALUE = SUBSTRING(box-design-hdr.lscore,li-lscore-len + 1,li-lscore-len).
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,li-lscore-len + 1,li-lscore-len).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME editor_wcum-score
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL editor_wcum-score V-table-Win
ON ENTRY OF editor_wcum-score IN FRAME F-Main
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME box-design-hdr.lscore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON CURSOR-LEFT OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:

    /*
  v-cur-position = v-cur-position - 1.
  box-design-hdr.lcum-score:SCREEN-VALUE = 
       SUBSTRING( box-design-hdr.lcum-score,v-cur-position,1)
       +    SUBSTRING( box-design-hdr.lcum-score:SCREEN-VALUE,1,LENGTH(box-design-hdr.lcum-score:SCREEN-VALUE) - 1)
               .
  MESSAGE v-cur-position SUBSTRING( box-design-hdr.lcum-score,v-cur-position,1) 
      VIEW-AS ALERT-BOX.
      */

     IF SELF:CURSOR-OFFSET <= 20 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET <= 40 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,21,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET <= 50 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,41,80)
                     .
      END.
      ELSE  DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80)
                     .
      END.
    APPLY LASTKEY .

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON CURSOR-RIGHT OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:

    /*v-cur-position = v-cur-position + 1.

  box-design-hdr.lcum-score:SCREEN-VALUE = 
           SUBSTRING( box-design-hdr.lcum-score:SCREEN-VALUE,v-cur-position + 1,LENGTH(box-design-hdr.lcum-score:SCREEN-VALUE))
          + SUBSTRING( box-design-hdr.lcum-score,LENGTH(box-design-hdr.lcum-score:SCREEN-VALUE) + 1,1)
               .
               */
      IF SELF:CURSOR-OFFSET >= 120 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET >= 100 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,41,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET >= 80 THEN DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,21,80)
                     .
      END.
      ELSE  DO:
          box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,80)
                     .
      END.

   APPLY LASTKEY. 
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON END OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
    APPLY LASTKEY.
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,li-lscore-len + 1,li-lscore-len).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON HOME OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
    APPLY LASTKEY.
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,li-lscore-len).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON LEAVE OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
    if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    def var i as int no-undo.
    def var ls-string as cha init "0,1,2,3,4,5,6,7,8,9" no-undo.
    do i = 1 to length(self:screen-value) :
      if lookup(substring(self:screen-value,i,1),ls-string) < 0 then do:
         message "Invalid Entry. Use Numeric Value Only. " view-as alert-box error.
         return no-apply.
      end. 
    end.

   {&methods/lValidateError.i NO} 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */


   PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
       DEFINE INPUT PARAMETER programname AS cha.
       DEFINE INPUT PARAMETER visualstyle AS long.
       DEFINE RETURN PARAM statuscode AS LONG.
   END.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "box-design-hdr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "box-design-hdr"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-box V-table-Win 
PROCEDURE build-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
session:set-wait-state("general").
  /* copied from cec/est-6-re.p */
def input parameter v-rebuild as char.

def buffer xbox-design-hdr  for box-design-hdr.
def buffer xbox-design-line for box-design-line.


{est/checkuse.i}

cocode = eb.company.
find xeb where recid(xeb) = recid(eb) no-lock.               

for each box-design-hdr where box-design-hdr.design-no = 0 and
                              box-design-hdr.company = xeb.company 
                          and box-design-hdr.est-no = xeb.est-no
    /*{cec/est-6W.i box-design-hdr}*/
      and box-design-hdr.form-no   eq xeb.form-no
      and box-design-hdr.blank-no  eq xeb.blank-no
    no-lock:
  /*      
  for each box-design-line of box-design-hdr:
    create w-box-l.
    buffer-copy box-design-line to w-box-l.
  end.
  */

  create w-box-h.
  buffer-copy box-design-hdr to w-box-h.
end.

{cec/est-6del.i}

find first xest where xest.company = xeb.company and
                      xest.est-no = xeb.est-no
                      no-lock.
find first xef where xef.company = xeb.company 
                 and xef.est-no   eq xeb.est-no
                 and xef.form-no eq xeb.form-no  no-lock.

find first style where style.company eq xeb.company
                   and style.style   eq xeb.style
                 no-lock no-error.
if avail style then
  find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                               and xbox-design-hdr.est-no    eq ""
             no-lock no-error.

if avail xbox-design-hdr then do:
   run cec/descalc.p (recid(xest), recid(xeb)).
   create box-design-hdr.
   assign  box-design-hdr.design-no   = 0
           box-design-hdr.company = xeb.company
           box-design-hdr.est-no      = xeb.est-no
           box-design-hdr.form-no     = xeb.form-no
           box-design-hdr.blank-no    = xeb.blank-no
           box-design-hdr.description = if avail xbox-design-hdr then
                                          xbox-design-hdr.description else ""
           box-design-hdr.lscore      = v-lscore-c
           box-design-hdr.lcum-score  = v-lcum-score-c
/*           fil_id                     = recid(box-design-hdr). */
           box-design-hdr.wscore = xbox-design-hdr.wscore
           box-design-hdr.wcum-score = xbox-design-hdr.wcum-score
           box-design-hdr.box-text = xbox-design-hdr.box-text
           .

   for each xbox-design-line of xbox-design-hdr no-lock:
      create box-design-line.
      assign box-design-line.design-no  = box-design-hdr.design-no
             box-design-line.company = box-design-hdr.company
             box-design-line.est-no      = box-design-hdr.est-no
             box-design-line.form-no    = box-design-hdr.form-no
             box-design-line.blank-no   = box-design-hdr.blank-no
             box-design-line.line-no    = xbox-design-line.line-no
             box-design-line.line-text  = xbox-design-line.line-text.

      find first w-box-design-line
           where w-box-design-line.line-no eq box-design-line.line-no   no-error.
      if avail w-box-design-line then
         assign  box-design-line.wscore     = w-box-design-line.wscore-c
                 box-design-line.wcum-score = w-box-design-line.wcum-score-c.
   end.

   if v-rebuild ne "B" then do:
      if v-rebuild eq "S" then
         box-design-hdr.description = w-box-h.description.
      else  assign box-design-hdr.lscore      = w-box-h.lscore
                   box-design-hdr.lcum-score  = w-box-h.lcum-score
                   box-design-hdr.wscore      = w-box-h.wscore
                   box-design-hdr.wcum-score  = w-box-h.wcum-score.

      for each w-box-l of box-design-hdr no-lock,
          first box-design-line of w-box-l:

          if v-rebuild eq "S" then
             assign box-design-line.line-no    = w-box-l.line-no
                     box-design-line.line-text  = w-box-l.line-text.
          else do:
             find first w-box-design-line
                  where w-box-design-line.line-no eq w-box-l.line-no   no-error.
             if avail w-box-design-line then
                assign box-design-line.wscore     = w-box-l.wscore
                       box-design-line.wcum-score = w-box-l.wcum-score.
          end.     
      end.
   end.
end.
/*
def var char-hdl as cha no-undo.
run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).
run dispatch in widget-handle(char-hdl) ('open-query').  
*/
run build-screen.

RUN release-shared-buffers.

session:set-wait-state("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-screen V-table-Win 
PROCEDURE build-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  assign frame {&frame-name}:width = 140
         frame {&frame-name}:height = 17.

  assign li-row = 3 /*frame {&frame-name}:row */
         li-col = frame {&frame-name}:column
         li-cnt = 1
         .

  for each box-design-line of box-design-hdr where box-design-line.line-text <> ""
                no-lock by box-design-line.line-no:
      create fill-in lv-wcum-score[li-cnt] 
             assign row = li-row + 1
                    column = li-col + 1                    
                    screen-value = box-design-line.wcum-score
                    frame = frame {&frame-name}:handle
                    /*sensitive = yes
                    visible = yes */.
      create fill-in lv-wscore[li-cnt] 
             assign row = li-row + 1
                    column = li-col + 10
                    screen-value = box-design-line.wscore                    
                    frame = frame {&frame-name}:handle
                    /*sensitive = yes
                      visible = yes */.

     create fill-in lv-line-text[li-cnt] 
             assign row = li-row + 1
                    column = li-col + 20
                    width-chars =  100
                    format = "x(65)"
                    screen-value = box-design-line.line-text
                    frame = frame {&frame-name}:handle
                    /*sensitive = yes
                    visible = yes */
             triggers:
                  on leave do:
                     message self:name "," {&self-name} view-as alert-box.
                  end.
             end triggers.
                _    
      assign li-row = li-row + 1
             li-line-no[li-cnt] = box-design-line.line-no
             li-cnt = li-cnt + 1.       
  end.
  li-cnt = li-cnt - 1.

*/  

/* ===== width display =======*/
  assign lv-wscore = ""
         lv-wcum-score = ""
         li-cnt = 0
         li-line-no = 0.

  for each box-design-line of box-design-hdr no-lock by box-design-line.line-no:
      assign lv-wscore = lv-wscore + box-design-line.wscore + chr(13)
             lv-wcum-score = lv-wcum-score + box-design-line.wcum-score + chr(13)
             li-cnt = li-cnt + 1
             li-line-no[li-cnt] = box-design-line.line-no
             .

  end.
  /*
  display lv-wscore lv-wcum-score with frame {&frame-name}.
  */
  assign editor_wscore:screen-value in frame {&frame-name} = lv-wscore
         editor_wcum-score:screen-value in frame {&frame-name} = lv-wcum-score.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE die-image V-table-Win 
PROCEDURE die-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-eb-rowid AS ROWID NO-UNDO.
  DEF VAR lv-die-image AS cha NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR ll-dummy AS LOG NO-UNDO.
  DEF VAR lv-cmd AS cha NO-UNDO.
  DEF VAR tInt AS INT NO-UNDO.
  DEF VAR v-graphic-types AS CHAR NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.
  DEF VAR lv-found AS LOG NO-UNDO.
  DEF VAR lv-return AS INT NO-UNDO.

  lv-cmd = "custom\mspaint.exe".
  IF SEARCH("c:\winnt\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\winnt\system32\mspaint.exe".
  ELSE IF SEARCH("c:\windows\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\windows\system32\mspaint.exe".

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN get-eb-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-eb-rowid).
  IF lv-eb-rowid <> ? THEN
     FIND FIRST eb WHERE ROWID(eb) = lv-eb-rowid NO-LOCK NO-ERROR.
  IF AVAIL eb THEN DO:
     FIND FIRST sys-ctrl WHERE
          sys-ctrl.company = g_company AND
          sys-ctrl.NAME = "DIEFILE"
          NO-LOCK NO-ERROR.

     IF AVAIL sys-ctrl THEN
        tInt = sys-ctrl.int-fld.

     IF tInt EQ 0 THEN
        v-graphic-types = "jpg,pdf,bmp,jpeg,tif".
     ELSE IF tInt EQ 1 THEN
        v-graphic-types = "bmp,pdf,jpg,jpeg,tif".
     ELSE
        v-graphic-types = "pdf,jpg,bmp,jpeg,tif".

     FIND FIRST ef NO-LOCK WHERE
          ef.company = eb.company AND
          ef.est-no = eb.est-no AND
          ef.form-no = eb.form-no
          NO-ERROR.

     IF AVAIL ef AND ef.cad-image <> "" THEN
        lv-die-image = (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") + ef.cad-image + ".".
     ELSE
        lv-die-image = (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") + eb.die-no + ".".
  END.

  DO v-index = 1 TO 5:
     lv-die-image = SUBSTRING(lv-die-image,1,R-INDEX(lv-die-image,".") ) + ENTRY(v-index,v-graphic-types).
     IF SEARCH(lv-die-image) <> ? THEN
     DO:
        lv-found = YES.
        LEAVE.
     END.
  END.

  IF lv-found THEN
  DO:
     IF NOT(lv-die-image MATCHES "*.pdf*") THEN
        OS-COMMAND SILENT VALUE(lv-cmd + " " +  lv-die-image).
     ELSE
     DO:
        RUN ShellExecuteA(0, "open", lv-die-image, "", "", 0, OUTPUT tInt).

        IF tInt LE 32 THEN
        DO:
           RUN custom/runapdf.p (OUTPUT lv-cmd).
           lv-cmd = lv-cmd + chr(32) + lv-die-image.
           RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
        END.
     END.
  END.

  IF lv-found = NO THEN
     MESSAGE "No Die Image available. Check Die Image."
        VIEW-AS ALERT-BOX ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def var ls-ws-value as cha no-undo.
  def var li-pos as int no-undo.
  def var li-pos-nxt as int no-undo.
  def var li-ln as int no-undo.
  def var ls-wscore as cha no-undo.
  def var ls-wcum as cha no-undo.
  def var ls-key as cha no-undo.

  def var ls-prev-wscore as cha no-undo.
  def var ls-prev-wcum as cha no-undo.
  DEF VAR v-sc-fmt AS CHAR NO-UNDO.

  FIND FIRST est WHERE est.company = cocode AND est.est-no = box-design-hdr.est-no NO-LOCK NO-ERROR.                          
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "BOXDESUM" no-lock no-error.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign v-sc-fmt  = if AVAIL sys-ctrl AND (
                        sys-ctrl.char-fld eq "MM" or
                        (sys-ctrl.char-fld eq "Both" and AVAIL est AND est.metric)) then "->>>>9" else "->9.99"
         ls-prev-wscore = editor_wscore:screen-value in frame {&frame-name}
         ls-prev-wcum = editor_wcum-score:screen-value. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */        

  /* width build-screen  assign box-design-line for character */
  assign ls-ws-value = editor_wscore:screen-value in frame {&frame-name}
         li-pos = 1
         li-pos-nxt = 1
         li-ln = 1
         ls-wscore = "". 

  FOR EACH box-design-line OF box-design-hdr:
      DELETE box-design-line.
  END.

  do i = 1 to length(ls-ws-value):
     ls-key = substring(ls-ws-value,i,1).

     if asc(ls-key) < 17 then do:  /* control key chr(13) = return key but says ctrl-j */
        create box-design-line.
        assign box-design-line.company = box-design-hdr.company
               box-design-line.design-no = box-design-hdr.design-no
               box-design-line.line-no = li-ln
               box-design-line.est-no     = box-design-hdr.est-no
               box-design-line.form-no   = box-design-hdr.form-no
               box-design-line.blank-no  = box-design-hdr.blank-no
               box-design-line.wscore = ls-wscore
               ls-wscore = ""
               li-ln = li-ln + 1.       
        next.       
     end.
     ELSE
        assign ls-wscore = ls-wscore + ls-key.
  end.
  /* == Width total assignment ========*/
  def var ld-wcum as dec no-undo.
  def var ls-new-wcum as cha no-undo.
  def var ld-wcum-prev as dec no-undo.
  assign li-ln = 1
         ls-wscore = ""
         ld-wcum = 0
         ls-new-wcum = "".
  do i = 1 to length(ls-ws-value):         
     ls-key = substring(ls-ws-value,i,1).
     if asc(ls-key) < 17 then do:  /* control key */
        find box-design-line of box-design-hdr where box-design-line.line-no = li-ln.
        ld-wcum = ld-wcum + dec(ls-wscore).
        if ld-wcum - trunc(ld-wcum,0) >= 0.16 then assign ld-wcum = ld-wcum + 1 - 0.16.         
        assign box-design-line.wcum-score =  if ld-wcum <> 0 and ld-wcum <> ld-wcum-prev
                                             then string(ld-wcum,v-sc-fmt)
                                             else ""
               ls-wscore = ""
               li-ln = li-ln + 1
               ld-wcum-prev = ld-wcum.       
        next.       
     end.
     else
        assign ls-wscore = ls-wscore + ls-key.
  end.

  for each box-design-line of box-design-hdr no-lock:
      ls-new-wcum = ls-new-wcum + 
                    if box-design-line.wcum-score <> "" then box-design-line.wcum-score
                    else CHR(13).
  end.
  box-design-hdr.wcum-score = ls-new-wcum.
  /*==== lscore assignment */
  def var ls-lscore as cha no-undo.
  def var ld-ls-val as dec no-undo.
  def var li-start as int no-undo.
  def var ls-char as cha no-undo.

  assign ls-lscore = ""
         ld-ls-val = 0
         li-start = 0
         ls-char = ""
         box-design-hdr.lcum-score = "".

  do i = 1 to length(box-design-hdr.lscore:screen-value) :
     ls-char = substring(box-design-hdr.lscore,i,1).
     if ls-char <> "" then do:
        ls-lscore = ls-lscore + ls-char.
        if li-start = 0 then li-start = i.
     end.
     else if ls-lscore <> "" then do:         
          ld-ls-val = ld-ls-val + dec(ls-lscore).

          if ld-ls-val - trunc(ld-ls-val,0) >= 0.16 then
             assign ld-ls-val = ld-ls-val + 1 - 0.16.   

          if length(string(ld-ls-val)) = length(ls-lscore) then                      /*string(ld-ls-val*/
             substring(box-design-hdr.lcum-score, li-start, i - li-start + 1) = trim(string(ld-ls-val,">>>.99")).
          else do:       
              CASE INDEX(ls-lscore,".") :
                 WHEN 1 then
                     substring(box-design-hdr.lcum-score,li-start - 2,(i - li-start + 1)) = string(ld-ls-val,"z.99").        
                 WHEN 2 THEN
                       substring(box-design-hdr.lcum-score,li-start - 2,(i - li-start + 1)) = (string(ld-ls-val,"zzz.99")).        
                 WHEN 3 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = (string(ld-ls-val,">>9.99")).        
                 WHEN 4 THEN                                                                  
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = (string(ld-ls-val,">>9.99")).        

              END CASE.
          END.
          assign ls-lscore = ""
                 li-start = 0.
     end.
  end.

  if ls-lscore <> "" then do:
     ld-ls-val = ld-ls-val + dec(ls-lscore).
     if ld-ls-val - trunc(ld-ls-val,0) >= 0.16 then
             assign ld-ls-val = ld-ls-val + 1 - 0.16.

     if length(string(ld-ls-val)) = length(ls-lscore) then                           /*string(ld-ls-val*/
             substring(box-design-hdr.lcum-score, li-start, i - li-start + 1) = trim(string(ld-ls-val,"zz9.99")).     
     else do:
              CASE INDEX(ls-lscore,".") :
                 WHEN 1 then
                     substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = string(ld-ls-val,"z.99").        
                 WHEN 2 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,"zzz.99")).        
                 WHEN 3 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,">>9.99")).        
                 WHEN 4 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,">>9.99")).        

              END CASE.
     END.

     assign ls-lscore = ""
            li-start = 0.    
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-is-3d-displayed THEN
     ASSIGN box-design-hdr.box-3d-image = box-design-hdr.box-3d-image:SCREEN-VALUE
        IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */ 
  disable box-design-hdr.box-3d-image WITH FRAME {&FRAME-NAME}.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer xbox-design-hdr for box-design-hdr.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  find last xbox-design-hdr  where xbox-design-hdr.design-no gt 0
                 use-index design no-lock no-error.
  box-design-hdr.design-no = (if avail xbox-design-hdr
                              then xbox-design-hdr.design-no + 1  else 1).
  display box-design-hdr.design-no with frame {&frame-name}.                          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
   MESSAGE "Delete Currently Selected Record?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
   IF NOT response THEN  RETURN "ADM-ERROR":U.
   session:set-wait-state("general").

   for each box-design-line of box-design-hdr:
       delete box-design-line.
   end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN release-shared-buffers.

  session:set-wait-state("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li1 AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li-font AS INT NO-UNDO.

  DEF BUFFER bf-eb FOR eb.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL box-design-hdr THEN DO:
    DO li1 = 1 TO LENGTH(box-design-hdr.lscore):
      IF SUBSTR(box-design-hdr.lscore,li1,1) NE " " THEN ll = YES.
      ELSE
      IF ll THEN
        ASSIGN
         li = li + 1
         ll = NO.
    END.
    IF ll THEN li = li + 1.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     li-lscore-len                   = IF li GE 20 THEN 105 ELSE 80
     li-font                         = IF li-lscore-len GT 80 THEN 4 ELSE 0
     box-design-hdr.lscore:FONT      = li-font
     box-design-hdr.lcum-score:FONT  = li-font
     box-design-hdr.lscore:WIDTH     = 116
     box-design-hdr.lcum-score:WIDTH = 116
     NO-ERROR.
  END.



  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

 /*
  run build-screen .
  run get-link-handle in adm-broker-hdl(this-procedure,"container-source", output char-hdl).  
  run get-attribute in widget-handle(char-hdl) ("current-page").
  if return-value = "3" then   /* to solve display problem when value-chang on 1 page */
  do i = 1 to li-cnt:     
     assign lv-wcum-score[i]:visible = yes
            lv-wscore[i]:visible = yes
            lv-line-text[i]:visible = yes.
  end.
  */
/*
   if not avail box-design-hdr and avail eb then do:
      /*run build-box ("B") . */

       run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).
       run build-box in widget-handle(char-hdl) ("B").

   end. 
   */ 

   run build-screen.

   DEF VAR ll-dummy AS LOG NO-UNDO.
   IF AVAIL box-design-hdr THEN DO WITH FRAME {&FRAME-NAME}:

    /*  ll-dummy = box-image-2:load-image("") in frame {&frame-name} no-error.*/

      if NOT ll-is-3d-displayed AND 
         box-design-hdr.box-image <> "" then do:
         /*box-image:auto-resize = yes. */
         ASSIGN box-design-hdr.box-text:HIDDEN = yes
                box-image-2:HIDDEN = NO .
         ll-dummy = box-image-2:load-image(box-design-hdr.box-image) in frame {&frame-name} NO-ERROR.
         /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.

         */
      end.
      ELSE if ll-is-3d-displayed AND
              box-design-hdr.box-3d-image <> "" then do:
              /*  box-image:auto-resize = yes. */
              ll-dummy = box-image-2:load-image(box-design-hdr.box-3d-image) in frame {&frame-name} NO-ERROR.
              /*assign box-image:height-pixels = box-image:height-pixels - 10
                box-image:width-pixels =  box-image:width-pixels - 10.            
              */
      end.
      ELSE DO:
          ASSIGN box-design-hdr.box-text:HIDDEN = NO
               box-image-2:HIDDEN = YES .
          DISPLAY box-design-hdr.box-text WITH FRAME {&FRAME-NAME}.
      END.

      RELEASE style.
      IF box-design-hdr.design-no EQ 0 THEN DO:
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company  EQ box-design-hdr.company
              AND bf-eb.est-no   EQ box-design-hdr.est-no
              AND bf-eb.FORM-no  EQ box-design-hdr.form-no
              AND bf-eb.blank-no EQ box-design-hdr.blank-no
            NO-ERROR.
        IF NOT AVAIL bf-eb THEN
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ box-design-hdr.company
              AND bf-eb.est-no  EQ box-design-hdr.est-no
            NO-ERROR.
        IF AVAIL bf-eb THEN
        FIND FIRST style NO-LOCK
            WHERE style.company EQ bf-eb.company
              AND style.style   EQ bf-eb.style
            NO-ERROR.
        IF AVAIL style THEN box-design-hdr.design-no:SCREEN-VALUE = STRING(style.design-no).         
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST est
      WHERE est.company EQ box-design-hdr.company
        AND est.est-no  EQ box-design-hdr.est-no
      NO-LOCK NO-ERROR.
  {est/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  editor_wscore:READ-ONLY IN FRAME {&FRAME-NAME} = NO.

  IF ll-is-3d-displayed THEN DO:
     DISABLE {&ENABLED-FIELDS} WITH FRAME {&FRAME-NAME}.
     editor_wscore:READ-ONLY IN FRAME {&FRAME-NAME} = NO.
     ENABLE box-design-hdr.box-3d-image WITH FRAME {&FRAME-NAME}.
     APPLY "entry" TO box-design-hdr.box-3d-image .
  END.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-row-num as rowid no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run dispatch('display-fields'). 
  DISABLE box-design-hdr.box-3d-image WITH FRAME {&FRAME-NAME}.

  editor_wscore:READ-ONLY IN FRAME {&FRAME-NAME} = YES.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuild-box V-table-Win 
PROCEDURE rebuild-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-rebuild as cha no-undo.


    FIND FIRST est
        WHERE est.company EQ box-design-hdr.company
          AND est.est-no  EQ box-design-hdr.est-no
        NO-LOCK NO-ERROR.  
    {est/checkuse.i}

    v-rebuild = "B".  

    repeat:
       message "Rebuild 'S'cores Only, 'I'mages Only, 'B'oth, or 'N'either?"   /* Box 'D'esign, */
           update v-rebuild .
       if index("SBIN",v-rebuild) eq 0 then undo, retry.    
       leave.
    end.

    IF v-rebuild NE "N" THEN
    DO:
       message "This process will erase any changes" +
               (if v-rebuild eq "B" then "," else
                (" to the " + if v-rebuild eq "I" then "box image,"
                                                  else "scores,")) +
               " are you sure?"
            update choice as log.
       if choice then do:
          def var char-hdl as cha no-undo.
          run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).
          run build-box in widget-handle(char-hdl) (v-rebuild).
       end.
    END.

    RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-design V-table-Win 
PROCEDURE redisplay-design :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ll-is-3d-displayed = NO.
  ASSIGN box-design-hdr.box-3d-image:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         box-design-hdr.box-image:HIDDEN = NO.

  RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-boximg V-table-Win 
PROCEDURE refresh-boximg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ll-dummy AS LOG NO-UNDO.
   FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.
   ll-dummy = box-image-2:load-image("") in frame {&frame-name} no-error.

   if NOT ll-is-3d-displayed AND
      box-design-hdr.box-image <> "" then do:
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image-2:load-image(box-design-hdr.box-image) in frame {&frame-name} NO-ERROR.
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.

     */
   end.
   ELSE if ll-is-3d-displayed AND
      box-design-hdr.box-3d-image <> "" then do:
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image-2:load-image(box-design-hdr.box-3d-image) in frame {&frame-name} NO-ERROR.
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.

     */
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers V-table-Win 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "box-design-hdr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE swap-image V-table-Win 
PROCEDURE swap-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-2d-or-3d AS cha NO-UNDO.  /*2d or 3d*/


DO WITH FRAME {&FRAME-NAME}:
  DEF VAR ll-dummy AS LOG NO-UNDO.
  IF ip-2d-or-3d = "3d" THEN DO:
    IF NOT ll-is-3d-displayed THEN DO:
       ASSIGN box-design-hdr.box-image:HIDDEN = TRUE
              box-design-hdr.box-3d-image:HIDDEN = NO.
       DISPLAY box-design-hdr.box-3d-image .
       ll-dummy = box-image-2:load-image("") in frame {&frame-name} no-error.

       if box-design-hdr.box-3d-image <> "" then do:
          /*  box-image:auto-resize = yes. */
          ASSIGN box-design-hdr.box-text:HIDDEN = yes
                 box-image-2:HIDDEN = NO .
          ll-dummy = box-image-2:load-image(box-design-hdr.box-3d-image) in frame {&frame-name} NO-ERROR.
       END.
       ll-is-3d-displayed = YES.
    END.
    ELSE DO: /* update image*/
         RUN update-image.
    END.
  END.
  ELSE DO:
     IF ll-is-3d-displayed THEN DO:
       ll-is-3d-displayed = NO.
       ASSIGN box-design-hdr.box-image:HIDDEN = no
              box-design-hdr.box-3d-image:HIDDEN = yes.

       RUN dispatch ('display-fields').
     END.
     ELSE RUN update-image.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-fgitem-img V-table-Win 
PROCEDURE update-fgitem-img :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-fgitem AS cha NO-UNDO.
   DEF VAR lv-fgimg AS cha NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-return AS INT NO-UNDO.
   DEF VAR tInt As Int No-undo.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN get-fgitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-fgitem).

   IF lv-fgitem <> "" THEN DO:
      FIND FIRST itemfg WHERE itemfg.company = g_company
                          AND itemfg.i-no = lv-fgitem NO-LOCK NO-ERROR.
      lv-fgimg = IF AVAIL itemfg then itemfg.box-IMAGE ELSE "".
      IF lv-fgimg <> "" THEN
      DO:
         IF lv-fgimg MATCHES "*.pdf*"  THEN DO:
            RUN ShellExecuteA(0, "open", lv-fgimg, "", "", 0, OUTPUT tInt).
            IF tInt LE 32 THEN
            DO:
               RUN custom/runapdf.p (OUTPUT lv-cmd).
               lv-cmd = lv-cmd + chr(32) + lv-fgimg.      
               RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
            END.
         END.
         ELSE
            OS-COMMAND SILENT VALUE(lv-fgimg).
      END.

       ELSE IF AVAIL itemfg THEN DO:
           MESSAGE "No Graphic Image entered. Would you like to enter it?" VIEW-AS ALERT-BOX QUESTION
               BUTTON YES-NO UPDATE ll-ans AS LOG.
           IF ll-ans THEN RUN fg/d-fgimg.w (RECID(itemfg)).
           FIND CURRENT itemfg NO-LOCK NO-ERROR.
           IF itemfg.box-image <> "" THEN
           DO:
              IF itemfg.box-image MATCHES "*.pdf*"  THEN
              DO:
                 RUN ShellExecuteA(0, "open", itemfg.box-image, "", "", 0, OUTPUT tInt).
                 IF tInt LE 32 THEN
                 DO:
                    RUN custom/runapdf.p (OUTPUT lv-cmd).
                    lv-cmd = lv-cmd + chr(32) + trim(itemfg.box-image).
                    RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
                 END.
              END.
              ELSE
                 OS-COMMAND SILENT VALUE(itemfg.box-image).
           END.
       END.
       ELSE 
          MESSAGE "No Graphic Image entered..." VIEW-AS ALERT-BOX ERROR.
   END.
   ELSE
       MESSAGE "No FG Item# entered..." VIEW-AS ALERT-BOX ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-image V-table-Win 
PROCEDURE update-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-cmd2 AS cha NO-UNDO.
   DEF VAR lv-return AS INT NO-UNDO.
   DEF VAR lv-upd-what AS cha NO-UNDO.
   DEF VAR lv-image-file AS cha NO-UNDO.
   DEF VAR tInt As Int No-undo.


     IF AVAIL box-design-hdr AND NOT ll-is-3d-displayed AND box-design-hdr.box-image <> ""
     THEN DO:
      IF box-design-hdr.box-image MATCHES "*.pdf*"  THEN DO:
         RUN ShellExecuteA(0, "open", box-design-hdr.box-image, "", "", 0, OUTPUT tInt).
         IF tInt LE 32 THEN
         DO:
            RUN custom/runapdf.p (OUTPUT lv-cmd).
            lv-cmd = lv-cmd + chr(32) + box-design-hdr.box-image.      
            RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
         END.
      END.
      ELSE DO:
           /*OS-COMMAND SILENT VALUE("custom\mspaint.exe " + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} )*/
          FIND FIRST users WHERE users.USER_id = USERID('nosweat') NO-LOCK NO-ERROR.
          IF AVAIL users AND users.USER_program[1] <> "" /*AND SEARCH(users.USER_program[1]) <> ?*/
               THEN ASSIGN lv-cmd = users.USER_program[1]
                           lv-cmd2 = chr(34) + users.USER_program[1] + CHR(34) .
          ELSE DO: 
             lv-cmd = ".\custom\mspaint.exe".
             IF SEARCH("c:\winnt\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\winnt\system32\mspaint.exe".
             ELSE IF    SEARCH("c:\windows\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\windows\system32\mspaint.exe".
          END.

          ASSIGN
          lv-cmd = lv-cmd + " " + chr(34) + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34)
          lv-image-file = chr(34) + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34).

          IF lv-cmd2 <> "" THEN
             OS-COMMAND SILENT START value(lv-cmd) /*value(lv-cmd2) value(lv-image-file)*/ .          
          ELSE OS-COMMAND SILENT VALUE(lv-cmd).          

      END.
     END.
     ELSE IF AVAIL box-design-hdr AND ll-is-3d-displayed AND box-design-hdr.box-3d-image <> ""
     THEN DO:
      IF box-design-hdr.box-3d-image MATCHES "*.pdf*"  THEN DO:
         RUN ShellExecuteA(0, "open", box-design-hdr.box-3d-image, "", "", 0, OUTPUT tInt).
         IF tInt LE 32 THEN
         DO:
            RUN custom/runapdf.p (OUTPUT lv-cmd).
            lv-cmd = lv-cmd + chr(32) + box-design-hdr.box-3d-image.      
            RUN WinExec (INPUT lv-cmd, INPUT 1,OUTPUT lv-return).
         END.
      END.
      ELSE do:
          FIND FIRST users WHERE users.USER_id = USERID('nosweat') NO-LOCK NO-ERROR.
          IF AVAIL users AND users.USER_program[1] <> "" /*AND SEARCH(users.USER_program[1]) <> ?*/
             THEN ASSIGN lv-cmd = users.USER_program[1]
                         lv-cmd2 = chr(34) + users.USER_program[1] + CHR(34) .
          ELSE DO: 
            lv-cmd = "custom\mspaint.exe".
            IF SEARCH("c:\winnt\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\winnt\system32\mspaint.exe".
            ELSE IF    SEARCH("c:\windows\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\windows\system32\mspaint.exe".
          END.
          ASSIGN
          lv-cmd = lv-cmd + " " + CHR(34) + box-design-hdr.box-3d-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34)
          lv-image-file = chr(34) + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34).

          IF lv-cmd2 <> "" THEN
             OS-COMMAND SILENT START value(lv-cmd) /*value(lv-cmd2) value(lv-image-file)*/  .          
          ELSE OS-COMMAND SILENT  VALUE(lv-cmd).          
      END.
     END.

   RUN refresh-boximg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

