&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: password.w

  Description: Allows user to change password

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Steve Lippard

  Created: 09/22/98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE tries AS INTEGER NO-UNDO.
DEFINE VARIABLE v-save_userid LIKE NOSWEAT._user._userid NO-UNDO.
DEFINE VARIABLE v-save_password LIKE NOSWEAT._user._password NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-current_password Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS v-username v-current_password ~
v-new_password v-reenter_password 

/* Custom List Definitions                                              */
/* Validate-Fields,Update-Fields,List-3,List-4,List-5,List-6            */
&Scoped-define Validate-Fields v-username v-current_password 
&Scoped-define Update-Fields v-new_password v-reenter_password Btn_OK 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-current_password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Current Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-new_password AS CHARACTER FORMAT "X(256)":U 
     LABEL "New Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-reenter_password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Re-enter Password" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-username AS CHARACTER FORMAT "X(256)":U 
     LABEL "Username" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-username AT ROW 1.95 COL 20 COLON-ALIGNED
     v-current_password AT ROW 3.38 COL 20 COLON-ALIGNED HELP
          "Enter current password" BLANK 
     v-new_password AT ROW 5.29 COL 20 COLON-ALIGNED BLANK 
     v-reenter_password AT ROW 6.71 COL 20.2 COLON-ALIGNED BLANK 
     Btn_OK AT ROW 8.62 COL 5
     Btn_Cancel AT ROW 8.62 COL 22
     Btn_Help AT ROW 8.62 COL 39
     SPACE(2.99) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Change Password"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_OK IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN v-current_password IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-new_password IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN v-reenter_password IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN v-username IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Change Password */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF v-reenter_password:SCREEN-VALUE NE v-new_password:SCREEN-VALUE THEN
  DO:
    MESSAGE "Passwords do not match" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO v-new_password IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  
  DO TRANSACTION:
    FIND NOSWEAT._user WHERE NOSWEAT._user._userid = v-username:SCREEN-VALUE
         EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE NOSWEAT._user OR
       NOSWEAT._user._password NE ENCODE(v-current_password:SCREEN-VALUE) THEN
    DO:
      MESSAGE "Userid/Password is incorrect." VIEW-AS ALERT-BOX ERROR.
      DISABLE {&Update-Fields} WITH FRAME {&FRAME-NAME}.
      ENABLE {&Validate-Fields} WITH FRAME {&FRAME-NAME}.
      APPLY "ENTRY" TO v-username IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
    END.
    ASSIGN NOSWEAT._user._password = ENCODE(v-new_password:SCREEN-VALUE).
  END. /* TRANSACTION */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-current_password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-current_password Dialog-Frame
ON LEAVE OF v-current_password IN FRAME Dialog-Frame /* Current Password */
DO: 
  IF NOT SETUSERID(v-username:SCREEN-VALUE,
                   {&SELF-NAME}:SCREEN-VALUE,"NOSWEAT") THEN
  DO:
    MESSAGE "Userid/Password is incorrect." VIEW-AS ALERT-BOX ERROR.
    IF tries GT 3 THEN QUIT. /* only allow 3 tries*/
    ASSIGN tries = tries + 1.
    IF v-username:SENSITIVE THEN
      APPLY "ENTRY" TO v-username IN FRAME {&FRAME-NAME}.
    ELSE
      APPLY "ENTRY" TO {&SELF-NAME} IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  
  IF NOT v-username:SENSITIVE THEN
  DO:
    ASSIGN
      v-save_userid = v-username:SCREEN-VALUE
      v-save_password = v-current_password:SCREEN-VALUE
      .
    ENABLE v-username WITH FRAME {&FRAME-NAME}.
    APPLY "ENTRY" TO v-username IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  ELSE
  DO:
    DISABLE {&Validate-Fields} WITH FRAME {&FRAME-NAME}.
    ENABLE {&Update-Fields} WITH FRAME {&FRAME-NAME}.
    APPLY "ENTRY" TO v-new_password IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-current_password Dialog-Frame
ON RETURN OF v-current_password IN FRAME Dialog-Frame /* Current Password */
DO:
  APPLY "TAB":U TO {&SELF-NAME} IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-new_password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-new_password Dialog-Frame
ON RETURN OF v-new_password IN FRAME Dialog-Frame /* New Password */
DO:
  APPLY "TAB":U TO {&SELF-NAME} IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-username
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-username Dialog-Frame
ON RETURN OF v-username IN FRAME Dialog-Frame /* Username */
DO:
  APPLY "TAB":U TO {&SELF-NAME} IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND NOSWEAT._user WHERE NOSWEAT._user._userid = USERID("NOSWEAT") NO-LOCK NO-ERROR.
  IF NOT AVAILABLE NOSWEAT._user THEN
  DO:
    MESSAGE "User:" USERID("NOSWEAT") "not on file" VIEW-AS ALERT-BOX ERROR.
    {methods/nowait.i}
    RETURN.
  END.
  ASSIGN v-username = NOSWEAT._user._userid.
  RUN enable_UI.
  {methods/nowait.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
IF USERID("NOSWEAT") NE v-save_userid THEN
  IF NOT SETUSERID(v-save_userid,v-save_password,"NOSWEAT") THEN
    MESSAGE "Current Userid is" USERID("NOSWEAT") VIEW-AS ALERT-BOX WARNING.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY v-username v-current_password v-new_password v-reenter_password 
      WITH FRAME Dialog-Frame.
  ENABLE v-current_password Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


