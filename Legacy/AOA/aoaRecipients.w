&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: aoaRecipients.w

  Description: Recipient Emails

  Input Parameters: Recipient List

  Output Parameters: Recipient List

  Author: Ron Stark

  Created: 12.7.2018
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT-OUTPUT PARAMETER iopcRecipients AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttEmail NO-UNDO
    FIELD isActive AS LOGICAL                  LABEL "Active"
    FIELD user-id  LIKE users.user_id          LABEL "User"
    FIELD userName AS CHARACTER FORMAT "x(20)" LABEL "Name"
    FIELD email    AS CHARACTER FORMAT "x(40)" LABEL "EMail"
    FIELD allData  AS CHARACTER
        INDEX user-id IS PRIMARY
            user-id
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME recipients

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttEmail

/* Definitions for BROWSE recipients                                    */
&Scoped-define FIELDS-IN-QUERY-recipients ttEmail.isActive ttEmail.user-id ttEmail.userName ttEmail.email   
&Scoped-define ENABLED-FIELDS-IN-QUERY-recipients ttEmail.isActive   
&Scoped-define ENABLED-TABLES-IN-QUERY-recipients ttEmail
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-recipients ttEmail
&Scoped-define SELF-NAME recipients
&Scoped-define QUERY-STRING-recipients FOR EACH ttEmail WHERE ttEmail.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-recipients OPEN QUERY {&SELF-NAME} FOR EACH ttEmail WHERE ttEmail.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-recipients ttEmail
&Scoped-define FIRST-TABLE-IN-QUERY-recipients ttEmail


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-recipients}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel searchBar recipients btnOK 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnCancel 
&Scoped-define List-3 btnCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY recipients FOR 
      ttEmail SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE recipients
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS recipients Dialog-Frame _FREEFORM
  QUERY recipients DISPLAY
      ttEmail.isActive LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
ttEmail.user-id LABEL-BGCOLOR 14
ttEmail.userName LABEL-BGCOLOR 14
ttEmail.email LABEL-BGCOLOR 14
ENABLE
ttEmail.isActive
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 78 BY 24.76
         TITLE "Recipient Emails".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 27.19 COL 70 HELP
          "Cancel" WIDGET-ID 28
     searchBar AT ROW 1 COL 8 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     recipients AT ROW 1.95 COL 1 WIDGET-ID 200
     btnOK AT ROW 27.19 COL 61
     RECT-1 AT ROW 26.95 COL 60 WIDGET-ID 2
     SPACE(0.00) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Recipient Emails"
         CANCEL-BUTTON btnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB recipients RECT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME Dialog-Frame
   1 3                                                                  */
ASSIGN 
       recipients:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE recipients
/* Query rebuild information for BROWSE recipients
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttEmail
WHERE ttEmail.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE recipients */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recipient Emails */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN pSetRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME recipients
&Scoped-define SELF-NAME recipients
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL recipients Dialog-Frame
ON START-SEARCH OF recipients IN FRAME Dialog-Frame /* Recipient Emails */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar Dialog-Frame
ON VALUE-CHANGED OF searchBar IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

ON "VALUE-CHANGED":U OF ttEmail.isActive
DO:
    ttEmail.isActive = NOT ttEmail.isActive.
    APPLY "TAB":U.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetRecipients.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{methods/sortByProc.i "pByIsActive" "ttEmail.isActive"}
{methods/sortByProc.i "pByUserID" "ttEmail.user-id"}
{methods/sortByProc.i "pByUserName" "ttEmail.userName"}
{methods/sortByProc.i "pByEmail" "ttEmail.email"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY searchBar 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel searchBar recipients btnOK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecipients Dialog-Frame 
PROCEDURE pGetRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FOR EACH users NO-LOCK
        WHERE users.email NE ""
        :
        CREATE ttEmail.
        ASSIGN
            ttEmail.isActive = CAN-DO(iopcRecipients,users.email)
            ttEmail.user-id  = users.user_id
            ttEmail.userName = users.user_name
            ttEmail.email    = users.email
            ttEmail.allData  = ttEmail.user-id  + "|"
                             + ttEmail.userName + "|"
                             + ttEmail.email
                             .     
    END. /* each users */
    DO idx = 1 TO NUM-ENTRIES(iopcRecipients):
        IF CAN-FIND(FIRST ttEmail
                    WHERE ttEmail.email EQ ENTRY(idx,iopcRecipients)) THEN
        NEXT.
        CREATE ttEmail.
        ASSIGN
            ttEmail.isActive = YES
            ttEmail.email    = ENTRY(idx,iopcRecipients)
            ttEmail.allData  = ttEmail.email
            .     
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse Dialog-Frame 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "isActive" THEN
        RUN pByIsActive.
        WHEN "user-id" THEN
        RUN pByUserID.
        WHEN "userName" THEN
        RUN pByUserName.
        WHEN "email" THEN
        RUN pByEmail.
        OTHERWISE
        &SCOPED-DEFINE SORTBY-PHRASE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetRecipients Dialog-Frame 
PROCEDURE pSetRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    iopcRecipients =  "".
    FOR EACH ttEmail
        WHERE ttEmail.isActive EQ YES
        :
        iopcRecipients = iopcRecipients + ttEmail.email + ",".
    END. /* each ttemail */
    iopcRecipients = TRIM(iopcRecipients,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

