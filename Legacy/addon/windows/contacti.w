&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{custom/gcompany.i}
def temp-table tt-contact like contact.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-contact AS CHARACTER FORMAT "X(256)":U 
     LABEL "Importing Contact for Customer" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lv-contact AT ROW 1.48 COL 32 COLON-ALIGNED
     Btn_OK AT ROW 3.62 COL 6
     Btn_Cancel AT ROW 3.62 COL 23
     Btn_Help AT ROW 3.62 COL 60
     "Upload All Customer Contacts?" VIEW-AS TEXT
          SIZE 75 BY .62 AT ROW 1.71 COL 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.91.


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
         TITLE              = "Upload Customer Contacts"
         COLUMN             = 35.8
         ROW                = 11.38
         HEIGHT             = 6.91
         WIDTH              = 79.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 119.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN lv-contact IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-contact:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Upload Customer Contacts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Upload Customer Contacts */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  /*====================
  def var i as int initial 0 no-undo.
  def var num-imports as int initial 0 no-undo.
  def var num-update as int no-undo.
  def var init-dir as cha no-undo.
  def buffer b-contact for contact.
  def var list-name as cha no-undo.
  def var OKpressed as log no-undo.
    
  lv-contact:hidden = no.
  SESSION:SET-WAIT-STATE("GENERAL").

/*  init-dir = "users\" + USERID("ASI") + "\contactx.rpt". */
  init-dir = session:temp-directory + "\contactx.rpt".
  for each tt-contact:
      delete tt-contact.
  end. 
  list-name = "users\" + USERID("ASI") + "\contactx" .
  if search(init-dir) = ? then do:
     SYSTEM-DIALOG GET-FILE list-name
       TITLE      "Enter Listing Name to Open ..."
       FILTERS    "Listing Files (*.rpt)" "*.rpt",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir
      must-exist
      CREATE-TEST-FILE
      USE-FILENAME
      UPDATE OKpressed.
      IF NOT OKpressed THEN  do:
  /*       apply "close" to this-procedure. */
         SESSION:SET-WAIT-STATE("").
         return no-apply.
      end.  
      if list-name matches "*contactx*" then    init-dir = list-name.
      else do:
         message "It's not a valid file to upload contacts." view-as alert-box error.
         SESSION:SET-WAIT-STATE("").
         return no-apply.
      end.
  end.
  
  
  input from value(init-dir) no-echo.
  num-imports = 0.
  num-update = 0.
  repeat:
  
    insert tt-contact. 
    find first b-contact where b-contact.company eq gcompany
                         and b-contact.cust-no eq tt-contact.cust-no
                         no-error.
    if avail b-contact then do:  /* update */
       buffer-copy tt-contact to b-contact.    
       num-update = num-update + 1.
       lv-contact:screen-value = b-contact.cust-no.
    end.
    else do:  /* create */
         create contact.
         buffer-copy tt-contact to contact.    
         num-imports = num-imports + 1. 
                lv-contact:screen-value = b-contact.cust-no.

    end.
 
  end.  /* repeat */

  if search(session:temp-directory + "\contactx.old" ) <> ? 
     then os-delete value(session:temp-directory + "\contactx.old").
  os-rename value(init-dir) value(session:temp-directory + "\contactx.old") .
  
  SESSION:SET-WAIT-STATE("").
  message num-imports " Contacts Added." skip
          num-update  " Contacts Updated."
          view-as alert-box.
  APPLY "CLOSE" TO THIS-PROCEDURE.
  =======================*/
  
   def var init-dir as cha no-undo.
   def var li-num-of-rec as int no-undo.
   def var li-num-of-notes as int no-undo.
    
   SESSION:SET-WAIT-STATE("GENERAL").
   /* ======= connect to server ========*/
  /* connect -pf lapemp.pf no-error.*/
  /* {system/connect.i}*/
   run system/lapnet.p.
   
   if error-status:error then do:
      message "Not connected".
      return no-apply.
   end. 
  
   /*   output to value(init-dir). */
   if connected("emp_server") then
       run system/contup.p (gcompany, input "", input "zzzzz" , output li-num-of-rec, output li-num-of-notes) .
      
   if connected("emp_server") then disconnect  emp_server.
   if connected("nos_server") then disconnect  nos_server.
  
   SESSION:SET-WAIT-STATE("").
   message "Upload Completed. " li-num-of-rec   " Contact Updated. " skip
           "                  " li-num-of-notes " Note    Updated. "
      view-as alert-box.
   APPLY "CLOSE" TO THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{sys/inc/f3help.i}
SESSION:SET-WAIT-STATE("").

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

  /* this procedure must run from laptop only not on server */
   if index(dbparam("ASI"),"-1") <= 0 then do:
      message "This Procedure Can Run Only From Laptop or Single User." view-as alert-box error.
      APPLY "CLOSE" TO THIS-PROCEDURE.
      return.
   end.

  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

