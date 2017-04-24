&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File:              getfield.w

  Description:       Get Database Fields

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           06/20/96

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT-OUTPUT PARAMETER m_field AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE m_field AS CHARACTER INITIAL "NOSWEAT.prgrms.prgmname" NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS m_dbs m_fields m_files RECT-20 Btn_Cancel ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS m_dbs m_fields m_files 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 16.8 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 16.8 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.8 BY 2.

DEFINE VARIABLE m_dbs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 37.6 BY 5.62 NO-UNDO.

DEFINE VARIABLE m_fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 46.2 BY 19.81 NO-UNDO.

DEFINE VARIABLE m_files AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 37.6 BY 15.29 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     m_dbs AT ROW 2.24 COL 1 HELP
          "Select Database Name" NO-LABEL
     m_fields AT ROW 2.24 COL 40.2 HELP
          "Select Field Name" NO-LABEL
     m_files AT ROW 9.05 COL 1 HELP
          "Select Table Name" NO-LABEL
     Btn_Cancel AT ROW 22.67 COL 43 HELP
          "Use this function to CANCEL field selecition"
     Btn_OK AT ROW 22.67 COL 65.4 HELP
          "Use this function to ACCEPT selected field"
     "Database Names" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 1 COL 9
          FONT 6
     "Field Names" VIEW-AS TEXT
          SIZE 14.6 BY 1 AT ROW 1 COL 56
          FONT 6
     "Table Names" VIEW-AS TEXT
          SIZE 15.6 BY 1 AT ROW 7.91 COL 11
          FONT 6
     RECT-20 AT ROW 22.33 COL 40.2
     SPACE(1.40) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Get Fields"
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
  m_field = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK DIALOG-1
ON CHOOSE OF Btn_OK IN FRAME DIALOG-1 /* OK */
DO:
  m_field = m_dbs:SCREEN-VALUE + "." +
            m_files:SCREEN-VALUE + "." +
            m_fields:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_dbs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_dbs DIALOG-1
ON VALUE-CHANGED OF m_dbs IN FRAME DIALOG-1
DO:
  RUN Get_Files.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,INDEX(m_field,".") + 1,
                  R-INDEX(m_field,".") - INDEX(m_field,".") - 1),
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
ON VALUE-CHANGED OF m_files IN FRAME DIALOG-1
DO:
  RUN Get_Fields.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,R-INDEX(m_field,".") + 1),m_fields:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_fields:SCREEN-VALUE = ENTRY(i,m_fields:LIST-ITEMS).
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
  RUN Enhance IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
  RUN enable_UI.
  RUN Get_DBs.
  i = IF m_field = "" THEN 1
      ELSE LOOKUP(SUBSTR(m_field,1,INDEX(m_field,".") - 1),m_dbs:LIST-ITEMS).
  IF i = 0 THEN
  i = 1.
  m_dbs:SCREEN-VALUE = ENTRY(i,m_dbs:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO m_dbs.
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
  DISPLAY m_dbs m_fields m_files 
      WITH FRAME DIALOG-1.
  ENABLE m_dbs m_fields m_files RECT-20 Btn_Cancel Btn_OK 
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
  
  m_get_dbs = SUBSTR(m_field,1,INDEX(m_field,".") - 1).
  RUN Get_Procedure IN Persistent-Handle (INPUT "db_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT m_get_dbs).
  m_dbs:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_dbs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Fields DIALOG-1 
PROCEDURE Get_Fields :
/* -----------------------------------------------------------
  Purpose: Populate m_fields selection list with field names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE m_get_fields AS CHARACTER NO-UNDO.
  
  CREATE ALIAS dictdb FOR DATABASE VALUE(m_dbs:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (INPUT m_files:SCREEN-VALUE,OUTPUT m_get_fields).
  m_fields:LIST-ITEMS IN FRAME {&FRAME-NAME} = m_get_fields.
  
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


