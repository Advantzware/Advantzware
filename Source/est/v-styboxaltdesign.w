&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ce\v-stybox.w

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

DEF VAR ll-box-refreshed AS LOG NO-UNDO.

DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES box-design-hdr style
&Scoped-define FIRST-EXTERNAL-TABLE box-design-hdr


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR box-design-hdr, style.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS style.designIDAlt box-design-hdr.description ~
box-design-hdr.box-image 
&Scoped-define ENABLED-TABLES style box-design-hdr
&Scoped-define FIRST-ENABLED-TABLE style
&Scoped-define SECOND-ENABLED-TABLE box-design-hdr
&Scoped-Define ENABLED-OBJECTS box-image-2 RECT-40 btnFirst btnPrevious ~
btnNext btnLast 
&Scoped-Define DISPLAYED-FIELDS style.designIDAlt ~
box-design-hdr.description box-design-hdr.box-image 
&Scoped-define DISPLAYED-TABLES style box-design-hdr
&Scoped-define FIRST-DISPLAYED-TABLE style
&Scoped-define SECOND-DISPLAYED-TABLE box-design-hdr


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS box-design-hdr.description 

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
DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "adm2/image/first.bmp":U
     LABEL "&First" 
     SIZE 4.8 BY 1.14 TOOLTIP "First Image".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "adm2/image/last.bmp":U
     LABEL "&Last" 
     SIZE 4.8 BY 1.14 TOOLTIP "Last Image".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "adm2/image/next.bmp":U
     LABEL "&Next" 
     SIZE 4.8 BY 1.14 TOOLTIP "Next Image".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "adm2/image/prev.bmp":U
     LABEL "&Previous" 
     SIZE 4.8 BY 1.14 TOOLTIP "Previous Image".

DEFINE IMAGE box-image-2
     SIZE 111 BY 12.38.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 16.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     style.designIDAlt AT ROW 1.24 COL 15.8 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1.1
     box-design-hdr.description AT ROW 1.24 COL 22.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     box-design-hdr.box-image AT ROW 1.24 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 68 BY 1
     btnFirst AT ROW 2.43 COL 2
     btnPrevious AT ROW 2.43 COL 7
     btnNext AT ROW 2.43 COL 12
     btnLast AT ROW 2.43 COL 17
     box-design-hdr.box-3d-image AT ROW 2.43 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 67 BY 1
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
   External Tables: ASI.box-design-hdr,ASI.style
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
         HEIGHT             = 17.67
         WIDTH              = 148.4.
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

/* SETTINGS FOR FILL-IN box-design-hdr.description IN FRAME F-Main
   1                                                                    */
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
                         "Acrobat Files(*.pdf)" "*.pdf",
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
                         "Acrobat Files(*.pdf)" "*.pdf",
                         "All Files    (*.*) " "*.*"
                 initial-dir cInitDir
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst V-table-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "changeImage" "('first')"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast V-table-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "changeImage" "('last')"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext V-table-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "changeImage" "('next')"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious V-table-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "changeImage" "('prev')"}
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
  {src/adm/template/row-list.i "style"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "box-design-hdr"}
  {src/adm/template/row-find.i "style"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-boximg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-boximg.
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
   IF NOT AVAIL box-design-hdr THEN RETURN.
    ll-dummy = box-image-2:load-image("") IN FRAME {&frame-name} NO-ERROR.
  
    IF box-design-hdr.box-image <> "" THEN 
    DO:
        ll-dummy = box-image-2:load-image(box-design-hdr.box-image) IN FRAME {&frame-name}.
     
    END.
    ll-box-refreshed = YES.

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
  {src/adm/template/snd-list.i "style"}

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
  DEF VAR lv-cmd AS cha NO-UNDO.
  DEF VAR lv-quote AS cha INIT '"' NO-UNDO.
  DEF VAR lv-cmd2 AS cha NO-UNDO.
  DEF VAR lv-image-file AS cha NO-UNDO.
  DEF VAR tInt AS INT NO-UNDO.
  
  lv-cmd = "custom\mspaint.exe".

  IF AVAIL box-design-hdr AND box-design-hdr.box-image <> "" THEN DO:

     IF box-design-hdr.box-image MATCHES "*.pdf*"  THEN DO:
        RUN ShellExecuteA(0, "open", box-design-hdr.box-3d-image, "", "", 0, OUTPUT tInt).
        IF tInt LE 32 THEN
        DO:
           RUN custom/runapdf.p (OUTPUT lv-cmd).
           OS-COMMAND SILENT VALUE(lv-cmd + " " + trim(lv-quote) + box-design-hdr.box-3d-image + trim(lv-quote)).
        END.
     END.
     ELSE DO:
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
         ELSE OS-COMMAND SILENT  VALUE(lv-cmd).          
     END.
  END.
  ll-box-refreshed = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

