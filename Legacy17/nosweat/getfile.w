&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File:              getfile.w

  Description:       Get Database File

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           11/22/95 - 10:10 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT-OUTPUT PARAMETER m_file AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE m_file AS CHARACTER INITIAL "NOSWEAT.prgmmstr" NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 m_dbs m_files Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS m_dbs m_files 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 1.62.

DEFINE VARIABLE m_dbs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 3.05 NO-UNDO.

DEFINE VARIABLE m_files AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 17.33 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     m_dbs AT ROW 1.95 COL 1 HELP
          "Select Database Name" NO-LABEL
     m_files AT ROW 1.95 COL 33 HELP
          "Select Table Name" NO-LABEL
     Btn_Cancel AT ROW 5.48 COL 2 HELP
          "Use this function to CANCEL file selecition"
     Btn_OK AT ROW 5.48 COL 17 HELP
          "Use this function to ACCEPT selected field"
     RECT-1 AT ROW 5.24 COL 1
     "Database Names" VIEW-AS TEXT
          SIZE 17.4 BY .62 AT ROW 1.24 COL 7
     "Table Names" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 38
     SPACE(7.99) SKIP(17.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Get File"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel DIALOG-1
ON CHOOSE OF Btn_Cancel IN FRAME DIALOG-1 /* Cancel */
DO:
  m_file = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK DIALOG-1
ON CHOOSE OF Btn_OK IN FRAME DIALOG-1 /* OK */
DO:
  m_file = m_dbs:SCREEN-VALUE + "." + m_files:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_dbs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_dbs DIALOG-1
ON DEFAULT-ACTION OF m_dbs IN FRAME DIALOG-1
DO:
  APPLY "CHOOSE" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_dbs DIALOG-1
ON VALUE-CHANGED OF m_dbs IN FRAME DIALOG-1
DO:
  RUN Get_Files.
  i = IF m_file = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_file,INDEX(m_file,".") + 1,
                  R-INDEX(m_file,".") - INDEX(m_file,".") - 1),
                  m_files:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_files:SCREEN-VALUE = ENTRY(i,m_files:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO m_files.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_files
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_files DIALOG-1
ON DEFAULT-ACTION OF m_files IN FRAME DIALOG-1
DO:
  APPLY "CHOOSE" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Get_DBs.
  i = IF m_file = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_file,1,INDEX(m_file,".") - 1),m_dbs:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_dbs:SCREEN-VALUE = ENTRY(i,m_dbs:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO m_dbs.
  {methods/nowait.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME} FOCUS m_dbs.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1 _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1 _DEFAULT-ENABLE
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
  DISPLAY m_dbs m_files 
      WITH FRAME DIALOG-1.
  ENABLE RECT-1 m_dbs m_files Btn_Cancel Btn_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_DBs DIALOG-1 
PROCEDURE Get_DBs :
/* -----------------------------------------------------------
  Purpose: Populate m_dbs selection list with connected database names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_dbs AS CHARACTER NO-UNDO.
  
  m_get_dbs = SUBSTR(m_file,1,INDEX(m_file,".") - 1).
  RUN Get_Procedure IN Persistent-Handle (INPUT "db_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT m_get_dbs).
  m_dbs:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_dbs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Files DIALOG-1 
PROCEDURE Get_Files :
/* -----------------------------------------------------------
  Purpose: Populate m_files selection list with table names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_files AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(m_dbs:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN Get_Procedure IN Persistent-Handle (INPUT "filelist.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT m_get_files).
  m_files:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_files.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


