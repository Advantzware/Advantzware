&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File:              login.w

  Description:       Login Procedure

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           01/25/98 -  1:28 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 m_id m_password Btn_Help Btn_Cancel ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS m_id m_password 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 12 BY 1
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 12 BY 1
     FONT 4.

DEFINE BUTTON Btn_OK 
     LABEL "&OK" 
     SIZE 12 BY 1
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE m_id AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 44.8 BY 1 NO-UNDO.

DEFINE VARIABLE m_password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 44.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.4 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     m_id AT ROW 2.43 COL 11 COLON-ALIGNED
     m_password AT ROW 3.62 COL 11 COLON-ALIGNED BLANK 
     Btn_Help AT ROW 5.05 COL 14
     Btn_Cancel AT ROW 5.05 COL 31
     Btn_OK AT ROW 5.05 COL 44
     "Enter User Login ID and Password" VIEW-AS TEXT
          SIZE 49 BY 1.24 AT ROW 1 COL 1
     RECT-9 AT ROW 4.81 COL 13
     SPACE(0.40) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "System Login"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel DIALOG-1
ON CHOOSE OF Btn_Cancel IN FRAME DIALOG-1 /* Cancel */
DO:
  quit_login = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK DIALOG-1
ON CHOOSE OF Btn_OK IN FRAME DIALOG-1 /* OK */
DO:                
  ASSIGN m_id m_password.
  IF NOT SETUSERID(m_id,m_password,"ASI") THEN
  MESSAGE "Invalid User ID / Password, Please Re-enter" 
      VIEW-AS ALERT-BOX ERROR.
  ELSE
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_id DIALOG-1
ON RETURN OF m_id IN FRAME DIALOG-1 /* User ID */
DO:
  APPLY "CHOOSE" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_password DIALOG-1
ON RETURN OF m_password IN FRAME DIALOG-1 /* Password */
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
  
  /* gdm - */
  ASSIGN 
      m_id:SCREEN-VALUE       = "asi"
      m_password:SCREEN-VALUE = "yorkie".

  IF USERID("ASI") NE "" THEN
  DO:
    m_id:SCREEN-VALUE = USERID("ASI").
    APPLY "ENTRY" TO m_password.
  END.
  ldummy = SESSION:SET-WAIT-STATE("").
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY m_id m_password 
      WITH FRAME DIALOG-1.
  ENABLE RECT-9 m_id m_password Btn_Help Btn_Cancel Btn_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

