&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: cec\v-boxdes.w

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

def var li-cnt as int no-undo.
def var li-line-no as int extent 99 no-undo.
DEF VAR lv-old-design-no AS INT NO-UNDO.

{custom/globdefs.i}
def new shared var cocode as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
{cec/descalc.i new}
def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO LIKE box-design-line.
def var lv-wscore like box-design-hdr.wscore no-undo.
def var lv-wcum-score like box-design-hdr.wcum-score no-undo.
DEFINE VARIABLE cDesignNo AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-FIELDS box-design-hdr.design-no ~
box-design-hdr.description box-design-hdr.box-image box-design-hdr.lscore ~
box-design-hdr.wscore 
&Scoped-define ENABLED-TABLES box-design-hdr
&Scoped-define FIRST-ENABLED-TABLE box-design-hdr
&Scoped-Define ENABLED-OBJECTS box-image-2 RECT-29 btn_right btn_left 
&Scoped-Define DISPLAYED-FIELDS box-design-hdr.design-no ~
box-design-hdr.description box-design-hdr.box-image box-design-hdr.lscore ~
box-design-hdr.lcum-score box-design-hdr.wcum-score box-design-hdr.wscore 
&Scoped-define DISPLAYED-TABLES box-design-hdr
&Scoped-define FIRST-DISPLAYED-TABLE box-design-hdr


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS box-design-hdr.description 
&Scoped-define ADM-ASSIGN-FIELDS box-design-hdr.lcum-score ~
box-design-hdr.wcum-score box-design-hdr.wscore 

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
     SIZE 3.6 BY 1.

DEFINE BUTTON btn_right 
     IMAGE-UP FILE "adm2/image/next.bmp":U
     LABEL "" 
     SIZE 3.6 BY 1.

DEFINE IMAGE box-image-2
     SIZE 115 BY 12.86.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 16.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     box-design-hdr.design-no AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1.1
     box-design-hdr.description AT ROW 1.24 COL 21 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     box-design-hdr.box-image AT ROW 1.24 COL 77 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
     box-design-hdr.lscore AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL FORMAT "x(135)"
          VIEW-AS FILL-IN 
          SIZE 113 BY 1
          FONT 0
     btn_right AT ROW 2.43 COL 116
     box-design-hdr.lcum-score AT ROW 3.38 COL 1 COLON-ALIGNED NO-LABEL FORMAT "x(135)"
          VIEW-AS FILL-IN 
          SIZE 113 BY 1
          FONT 0
     btn_left AT ROW 3.38 COL 116
     box-design-hdr.box-text AT ROW 4.57 COL 3 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 115 BY 12.62
          FONT 0
     box-design-hdr.wcum-score AT ROW 5.05 COL 119 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
          SIZE 13 BY 12.14
          FONT 0
     box-design-hdr.wscore AT ROW 5.05 COL 132 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
          SIZE 15 BY 12.14
          FONT 0
     "W Score" VIEW-AS TEXT
          SIZE 11 BY .86 AT ROW 4.1 COL 134
     "W Totals" VIEW-AS TEXT
          SIZE 11 BY .86 AT ROW 4.1 COL 121
     "Score:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 120
     "Total" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.38 COL 120
     box-image-2 AT ROW 4.57 COL 3
     RECT-29 AT ROW 1 COL 1
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
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 24.29
         WIDTH              = 175.
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

/* SETTINGS FOR EDITOR box-design-hdr.box-text IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       box-design-hdr.box-text:HIDDEN IN FRAME F-Main           = TRUE
       box-design-hdr.box-text:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN box-design-hdr.description IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN box-design-hdr.lcum-score IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN box-design-hdr.lscore IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR EDITOR box-design-hdr.wcum-score IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       box-design-hdr.wcum-score:RETURN-INSERTED IN FRAME F-Main  = TRUE
       box-design-hdr.wcum-score:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR box-design-hdr.wscore IN FRAME F-Main
   2                                                                    */
ASSIGN 
       box-design-hdr.wscore:RETURN-INSERTED IN FRAME F-Main  = TRUE.

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
   IF AVAIL box-design-hdr THEN
   ASSIGN
   box-design-hdr.lscore:SCREEN-VALUE = 
               substring(box-design-hdr.lscore,1,80)
   box-design-hdr.lcum-score:SCREEN-VALUE = 
               substring(box-design-hdr.lcum-score,1,80).
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
  IF AVAIL box-design-hdr THEN
  ASSIGN  box-design-hdr.lscore:SCREEN-VALUE = SUBSTRING(box-design-hdr.lscore,51,80)
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME box-design-hdr.lscore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON CURSOR-LEFT OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
    IF SELF:CURSOR-OFFSET <= 20 THEN 
       box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,80).
      
    ELSE IF SELF:CURSOR-OFFSET <= 40 THEN
       box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,21,80).
    
    ELSE IF SELF:CURSOR-OFFSET <= 50 THEN
        box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,41,80).
    
    ELSE
       box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80).
    
    APPLY LASTKEY.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON CURSOR-RIGHT OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
   IF SELF:CURSOR-OFFSET >= 120 THEN
      box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80).
   
   ELSE IF SELF:CURSOR-OFFSET >= 100 THEN
      box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,41,80).
   
   ELSE IF SELF:CURSOR-OFFSET >= 80 THEN
      box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,21,80).
   
   ELSE  
      box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,80).

   APPLY LASTKEY. 
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON END OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
   APPLY LASTKEY.
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,51,80).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.lscore V-table-Win
ON HOME OF box-design-hdr.lscore IN FRAME F-Main /* Length!Score */
DO:
    APPLY LASTKEY.
    box-design-hdr.lcum-score:SCREEN-VALUE = SUBSTRING(box-design-hdr.lcum-score,1,80).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME box-design-hdr.design-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.design-no V-table-Win
ON ENTRY OF box-design-hdr.design-no IN FRAME F-Main /* Design # */
DO:
  cDesignNo = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL box-design-hdr.design-no V-table-Win
ON LEAVE OF box-design-hdr.design-no IN FRAME F-Main /* Design # */
DO:
  IF LASTKEY <> -1 THEN DO:
     RUN valid-design-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  /*assign box-design-hdr.wscore:screen-value in frame {&frame-name} = lv-wscore
         box-design-hdr.wcum-score:screen-value in frame {&frame-name} = lv-wcum-score.
   */      
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
  def var ls-wcum-value as cha no-undo.
  def var li-pos as int no-undo.
  def var li-pos-nxt as int no-undo.
  def var li-ln as int no-undo.
  def var ls-wscore as cha no-undo.
  def var ls-wcum as cha no-undo.
  def var ls-key as cha no-undo.
  
  def var ls-prev-wscore as cha no-undo.
  def var ls-prev-wcum as cha no-undo.
  DEF VAR ls-box-text LIKE box-design-hdr.box-text NO-UNDO.
      
  DEF BUFFER xbox-design-hdr FOR box-design-hdr.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign ls-prev-wscore = box-design-hdr.wscore:screen-value in frame {&frame-name}
         ls-prev-wcum = box-design-hdr.wcum-score:screen-value
         ls-box-text = box-design-hdr.box-text:SCREEN-VALUE. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  IF lv-old-design-no NE 0 THEN
  DO:
     FIND FIRST xbox-design-hdr WHERE
          xbox-design-hdr.design-no EQ lv-old-design-no
          NO-LOCK NO-ERROR.
    
     IF AVAIL xbox-design-hdr THEN
     DO:
        box-design-hdr.box-3d-image = xbox-design-hdr.box-3d-image.
        RELEASE xbox-design-hdr.
     END.

     lv-old-design-no = 0.
  END.

  /* Code placed here will execute AFTER standard behavior.    */        
  assign box-design-hdr.lcum-score = box-design-hdr.lscore
         box-design-hdr.wcum-score = box-design-hdr.wscore
         box-design-hdr.box-text = ls-box-text.
   
  /* with build-screen  */
  assign ls-ws-value = /*lv-wscore:screen-value in frame {&frame-name} when use var */
                         box-design-hdr.wscore
         ls-wcum-value = /* lv-wcum-score:screen-value in frame {&frame-name} */
                         box-design-hdr.wcum-score
         li-pos = 1
         li-pos-nxt = 1
         li-ln = 1
         ls-wscore = "". 

  FOR EACH box-design-line OF box-design-hdr:
      DELETE box-design-line.
  END.
         
  IF SUBSTRING(ls-ws-value,LENGTH(ls-ws-value),1) NE CHR(13) AND
     SUBSTRING(ls-ws-value,LENGTH(ls-ws-value),1) NE CHR(10) THEN
     ls-ws-value = ls-ws-value + CHR(13).

  do i = 1 to length(ls-ws-value):
     ls-key = substring(ls-ws-value,i,1).
     if asc(ls-key) < 17 then do:  /* control key chr(13) = return key but says ctrl-j */
        find box-design-line of box-design-hdr where box-design-line.line-no = li-ln no-error.
        if not avail box-design-line then do:
           create box-design-line.
           assign box-design-line.company = box-design-hdr.company
                  box-design-line.design-no = box-design-hdr.design-no
                  box-design-line.line-no = li-ln
                  box-design-line.est-no     = box-design-hdr.est-no
                  box-design-line.form-no   = box-design-hdr.form-no
                  box-design-line.blank-no  = box-design-hdr.blank-no.
        end.
        assign box-design-line.wscore =  (ls-wscore)
               ls-wscore = ""
               li-ln = li-ln + 1.       
        next.       
     end.
     ELSE
        assign ls-wscore = ls-wscore + ls-key.
  end.
  /* == Width total assignment ========*/
  assign li-ln = 1
         ls-wscore = "".
  ls-wcum-value = ls-wcum-value + CHR(13).
  do i = 1 to length(ls-wcum-value):         
     ls-key = substring(ls-wcum-value,i,1).
     if asc(ls-key) < 17 then do:  /* control key */
        find box-design-line of box-design-hdr where box-design-line.line-no = li-ln NO-ERROR.
        if not avail box-design-line then do:
           create box-design-line.
           assign box-design-line.company = box-design-hdr.company
                  box-design-line.design-no = box-design-hdr.design-no
                  box-design-line.line-no = li-ln
                  box-design-line.est-no     = box-design-hdr.est-no
                  box-design-line.form-no   = box-design-hdr.form-no
                  box-design-line.blank-no  = box-design-hdr.blank-no.
        end.
        assign box-design-line.wcum-score =  (ls-wscore)
               ls-wscore = ""
               li-ln = li-ln + 1.       
        next.       
     end.
     ELSE
        assign ls-wscore = ls-wscore + ls-key.
  end.

/**/

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer xbox-design-hdr for box-design-hdr.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  lv-old-design-no = INT(box-design-hdr.design-no:SCREEN-VALUE
                         IN FRAME {&FRAME-NAME}).

  find last xbox-design-hdr  where xbox-design-hdr.design-no gt 0
                   use-index design no-lock no-error.
  box-design-hdr.design-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(
                                (if avail xbox-design-hdr
                                then xbox-design-hdr.design-no + 1  else 1)).
  

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  box-design-hdr.company = g_company.
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
  session:set-wait-state("").
/*  run dispatch('display-fields').  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*  disable lv-wscore lv-wcum-score with frame {&frame-name}. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */

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
  
   if not avail box-design-hdr and avail eb then do:
     run build-box ("B") .
   end.   
   
   run build-screen.  

   DEF VAR ll-dummy AS LOG NO-UNDO.
   ll-dummy = box-image-2:load-image("") in frame {&frame-name} no-error.
  
   IF AVAIL box-design-hdr AND box-design-hdr.box-image <> "" then do:
      ASSIGN box-design-hdr.box-text:HIDDEN = YES
             box-image-2:HIDDEN = NO
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image-2:load-image(box-design-hdr.box-image) in frame {&frame-name} NO-ERROR .
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.
            
     */
   end.
   ELSE IF AVAIL box-design-hdr THEN DO:
        ASSIGN box-design-hdr.box-text:HIDDEN = NO
               box-image-2:HIDDEN = YES .
        DISPLAY box-design-hdr.box-text WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
       box-image-2:HIDDEN = YES .
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*  enable lv-wscore lv-wcum-score with frame {&frame-name}. */
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rebuild-box V-table-Win 
PROCEDURE Rebuild-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-rebuild as cha no-undo.
  
    v-rebuild = "B".  
   
    repeat:
       message "Rebuild 'S'cores, Box 'D'esign, or 'B'oth?" 
           update v-rebuild .
       if index("SDB",v-rebuild) eq 0 then undo, retry.    
       leave.
    end.
    message "This process will erase any changes" +
            (if v-rebuild eq "B" then "," else
             (" to the " + if v-rebuild eq "D" then "box design,"
                                               else "scores,")) +
            " are you sure?"
            update choice as log.
    if choice then do:
       def var char-hdl as cha no-undo.
       run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).
       run build-box in widget-handle(char-hdl) (v-rebuild).
    end.   

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
   ll-dummy = box-image:load-image("") in frame {&frame-name} no-error.
  
   IF AVAIL box-design-hdr AND box-design-hdr.box-image <> "" then do:
     /*  box-image:auto-resize = yes. */
     ll-dummy = box-image:load-image(box-design-hdr.box-image) in frame {&frame-name} NO-ERROR.
     /*assign box-image:height-pixels = box-image:height-pixels - 10
            box-image:width-pixels =  box-image:width-pixels - 10.
            
     */
   end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-image V-table-Win 
PROCEDURE update-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-quote AS cha INIT '"' no-undo.
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-cmd2 AS cha NO-UNDO.
   DEF VAR lv-image-file AS cha NO-UNDO.
   DEF VAR tInt As Int No-undo.

   FIND FIRST users WHERE users.USER_id = USERID('nosweat') NO-LOCK NO-ERROR.
   IF AVAIL users AND users.USER_program[1] <> "" /*AND SEARCH(users.USER_program[1]) <> ?*/
               THEN ASSIGN lv-cmd = users.USER_program[1]
                           lv-cmd2 = chr(34) + users.USER_program[1] + CHR(34) .
   ELSE DO: 
      lv-cmd = "custom\mspaint.exe".
      IF SEARCH("c:\winnt\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\winnt\system32\mspaint.exe".
      ELSE IF    SEARCH("c:\windows\system32\mspaint.exe") <> ? THEN lv-cmd = "c:\windows\system32\mspaint.exe".
   END.

   IF AVAIL box-design-hdr AND box-design-hdr.box-image <> "" THEN DO:
      IF box-design-hdr.box-image MATCHES "*.pdf*"  THEN DO:

         RUN ShellExecuteA(0, "open", box-design-hdr.box-image, "", "", 0, OUTPUT tInt).
         IF tInt LE 32 THEN
         DO:
            RUN custom/runapdf.p (OUTPUT lv-cmd).
            OS-COMMAND silent VALUE(trim(lv-quote) + lv-cmd + " " + trim(lv-quote) + box-design-hdr.box-image + trim(lv-quote) + TRIM(lv-quote)).
         END.
      END.      
      ELSE DO:
          ASSIGN
          lv-cmd = lv-cmd + " " + chr(34) + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34)
          lv-image-file = chr(34) + box-design-hdr.box-image:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(34).
          
          IF lv-cmd2 <> "" THEN
             OS-COMMAND SILENT START VALUE(lv-cmd) /* value(lv-cmd2) value(lv-image-file) */ .          
          ELSE OS-COMMAND silent value(lv-cmd) /*VALUE(trim(lv-quote) + lv-cmd + " " + trim(lv-quote) + box-design-hdr.box-image + trim(lv-quote) + TRIM(lv-quote)) */.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-design-no V-table-Win
PROCEDURE valid-design-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iDesignNo AS INTEGER NO-UNDO.
  
  iDesignNo = INTEGER (box-design-hdr.design-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  
  IF iDesignNo EQ INTEGER (cDesignNo) THEN RETURN.
  
  {methods/lValidateError.i YES}
  IF CAN-FIND (FIRST box-design-hdr
               WHERE box-design-hdr.company   EQ g_company
                 AND box-design-hdr.design-no EQ iDesignNo) THEN DO:
     MESSAGE "Design Number" iDesignNo "Already Used - Try Again" VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  {methods/lValidateError.i NO}

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


