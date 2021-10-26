&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynFormTarget.w

  Description: Dynamic Form Target

  Input Parameters: Form ID, Client ID, Company and Target Type

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.12.2021
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipiFormID     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiClientID   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTargetType AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttTarget NO-UNDO
    FIELD isActive    AS LOGICAL                  LABEL "Active"
    FIELD targetID    AS CHARACTER FORMAT "x(10)" LABEL "Target ID"
    FIELD targetName  AS CHARACTER FORMAT "x(30)" LABEL "Target Name"
    FIELD tableRecKey AS CHARACTER
    FIELD allData  AS CHARACTER
        INDEX targetID IS PRIMARY
            targetID
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME targetBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTarget

/* Definitions for BROWSE targetBrowse                                  */
&Scoped-define FIELDS-IN-QUERY-targetBrowse ttTarget.isActive ttTarget.targetID ttTarget.targetName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-targetBrowse ttTarget.isActive   
&Scoped-define ENABLED-TABLES-IN-QUERY-targetBrowse ttTarget
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-targetBrowse ttTarget
&Scoped-define SELF-NAME targetBrowse
&Scoped-define QUERY-STRING-targetBrowse FOR EACH ttTarget WHERE ttTarget.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-targetBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTarget WHERE ttTarget.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-targetBrowse ttTarget
&Scoped-define FIRST-TABLE-IN-QUERY-targetBrowse ttTarget


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-targetBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK searchBar targetBrowse btnCancel 
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
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 18 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY targetBrowse FOR 
      ttTarget SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE targetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS targetBrowse Dialog-Frame _FREEFORM
  QUERY targetBrowse DISPLAY
      ttTarget.isActive LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
ttTarget.targetID LABEL-BGCOLOR 14
ttTarget.targetName LABEL-BGCOLOR 14
ENABLE
ttTarget.isActive
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 53 BY 24.76
         TITLE "Target Records".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 27.19 COL 37
     searchBar AT ROW 1 COL 8 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     targetBrowse AT ROW 1.95 COL 1 WIDGET-ID 200
     btnCancel AT ROW 27.19 COL 45 HELP
          "Cancel" WIDGET-ID 28
     RECT-1 AT ROW 26.95 COL 36 WIDGET-ID 2
     SPACE(0.00) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Target Records"
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
/* BROWSE-TAB targetBrowse searchBar Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME Dialog-Frame
   1 3                                                                  */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       targetBrowse:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE targetBrowse
/* Query rebuild information for BROWSE targetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTarget
WHERE ttTarget.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE targetBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Target Records */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN pSaveTargets.
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


&Scoped-define BROWSE-NAME targetBrowse
&Scoped-define SELF-NAME targetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL targetBrowse Dialog-Frame
ON START-SEARCH OF targetBrowse IN FRAME Dialog-Frame /* Target Records */
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

ON "VALUE-CHANGED":U OF ttTarget.isActive
DO:
    ttTarget.isActive = NOT ttTarget.isActive.
    APPLY "TAB":U.
END.

{methods/template/brwcustom2.i}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetTargets.
  BROWSE {&BROWSE-NAME}:TITLE = ipcTargetType + " Records".
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{methods/sortByProc.i "pByIsActive" "ttTarget.isActive"}
{methods/sortByProc.i "pByTargetID" "ttTarget.targetID"}
{methods/sortByProc.i "pByTargetName" "ttTarget.targetName"}

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
  ENABLE btnOK searchBar targetBrowse btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTargets Dialog-Frame 
PROCEDURE pGetTargets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    CASE ipcTargetType:
        WHEN "Customer" THEN
        FOR EACH cust NO-LOCK
            WHERE cust.company EQ ipcCompany
            :
            CREATE ttTarget.
            ASSIGN
                ttTarget.isActive    = CAN-FIND(FIRST dynFormTarget
                                                WHERE dynFormTarget.tableRecKey EQ cust.rec_key)
                ttTarget.targetID    = cust.cust-no
                ttTarget.targetName  = cust.name
                ttTarget.tableRecKey = cust.rec_key
                ttTarget.allData     = ttTarget.targetID + "|" + ttTarget.targetName
                .
        END. /* each cust */
        WHEN "Vendor" THEN
        FOR EACH vend NO-LOCK
            WHERE vend.company EQ ipcCompany
            :
            CREATE ttTarget.
            ASSIGN
                ttTarget.isActive    = CAN-FIND(FIRST dynFormTarget
                                                WHERE dynFormTarget.tableRecKey EQ vend.rec_key)
                ttTarget.targetID    = vend.vend-no
                ttTarget.targetName  = vend.name
                ttTarget.tableRecKey = vend.rec_key
                ttTarget.allData     = ttTarget.targetID + "|" + ttTarget.targetName
                .
        END. /* each cust */
    END CASE.

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
        WHEN "targetID" THEN
        RUN pByTargetID.
        WHEN "targetName" THEN
        RUN pByTargetName.
        OTHERWISE
        &SCOPED-DEFINE SORTBY-PHRASE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveTargets Dialog-Frame 
PROCEDURE pSaveTargets :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH ttTarget:
        CASE ttTarget.isActive:
            WHEN NO THEN DO:
                IF NOT CAN-FIND(FIRST dynFormTarget
                                WHERE dynFormTarget.tableRecKey EQ ttTarget.tableRecKey) THEN
                NEXT.
                FIND FIRST dynFormTarget EXCLUSIVE-LOCK
                     WHERE dynFormTarget.tableRecKey EQ ttTarget.tableRecKey
                     NO-ERROR.
                IF AVAILABLE dynFormTarget THEN
                DELETE dynFormTarget.
            END. /* no */
            WHEN YES THEN DO:
                IF CAN-FIND(FIRST dynFormTarget
                            WHERE dynFormTarget.tableRecKey EQ ttTarget.tableRecKey) THEN
                NEXT.
                CREATE dynFormTarget.
                ASSIGN
                    dynFormTarget.formID      = ipiFormID
                    dynFormTarget.clientID    = ipiClientID
                    dynFormTarget.tableRecKey = ttTarget.tableRecKey
                    .
            END. /* yes */
        END CASE.
    END. /* each tttarget */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

