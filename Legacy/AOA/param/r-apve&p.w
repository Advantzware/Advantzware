&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-apve&p.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 8.25.2017

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

{aoa/includes/aoaParamVars.i}

DEFINE VARIABLE lInvalid AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSecure  AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svCompany svLocation svPostDate ~
btnCalendar-1 svPostDateOption svPeriod svAllVendNo svStartVendNo ~
svEndVendNo svStartInvoiceDate btnCalendar-2 svStartInvoiceDateOption ~
svEndInvoiceDate btnCalendar-3 svEndInvoiceDateOption svAllUserID ~
svStartUserID svEndUserID svPostOutOfPeriod svPostIntoClosedPeriod svPost ~
btnAddEmail svRecipients 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svPostDate ~
svPostDateOption svPeriod svAllVendNo svStartVendNo startVendorName ~
svEndVendNo endVendorName svStartInvoiceDate svStartInvoiceDateOption ~
svEndInvoiceDate svEndInvoiceDateOption svAllUserID svStartUserID ~
startUserIDName svEndUserID endUserIDName svPostOutOfPeriod ~
svPostIntoClosedPeriod svPost svRecipients 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE svEndInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svPostDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 2.86
     BGCOLOR 15 .

DEFINE VARIABLE endUserIDName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endVendorName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startUserIDName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startVendorName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndUserID AS CHARACTER FORMAT "X(8)" 
     LABEL "End User ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndVendNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Vendor" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svPeriod AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svPostDate AS DATE FORMAT "99/99/9999" 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartUserID AS CHARACTER FORMAT "X(8)" 
     LABEL "Start User ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartVendNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Vendor" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 76.8 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 84 BY 4.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 84 BY 2.86.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79.8 BY 4.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79.8 BY 2.86.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.57.

DEFINE VARIABLE svAllUserID AS LOGICAL INITIAL yes 
     LABEL "All User IDs" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllVendNo AS LOGICAL INITIAL yes 
     LABEL "All Vendors" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svPost AS LOGICAL INITIAL no 
     LABEL "POST" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE svPostIntoClosedPeriod AS LOGICAL INITIAL no 
     LABEL "Post Into Closed Period" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE svPostOutOfPeriod AS LOGICAL INITIAL no 
     LABEL "Post Out of Period" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 139 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 155 COLON-ALIGNED WIDGET-ID 130
     svPostDate AT ROW 2.43 COL 58 COLON-ALIGNED HELP
          "Enter Post Date" WIDGET-ID 274
     btnCalendar-1 AT ROW 2.43 COL 76 WIDGET-ID 272
     svPostDateOption AT ROW 2.43 COL 79 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 276
     svPeriod AT ROW 2.43 COL 113 COLON-ALIGNED WIDGET-ID 368
     svAllVendNo AT ROW 4.57 COL 22 HELP
          "All Vendors?" WIDGET-ID 362
     svStartVendNo AT ROW 5.76 COL 20 COLON-ALIGNED HELP
          "Enter Start Vendor" WIDGET-ID 366
     startVendorName AT ROW 5.76 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     svEndVendNo AT ROW 6.95 COL 20 COLON-ALIGNED HELP
          "Enter End Vendor" WIDGET-ID 364
     endVendorName AT ROW 6.95 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 358
     svStartInvoiceDate AT ROW 9.81 COL 20 COLON-ALIGNED HELP
          "Enter Start Invoice Date" WIDGET-ID 26
     btnCalendar-2 AT ROW 9.81 COL 38 WIDGET-ID 80
     svStartInvoiceDateOption AT ROW 9.81 COL 41 COLON-ALIGNED HELP
          "Select Start Invoice Date Option" NO-LABEL WIDGET-ID 64
     svEndInvoiceDate AT ROW 11 COL 20 COLON-ALIGNED HELP
          "Enter End Invoice Date" WIDGET-ID 24
     btnCalendar-3 AT ROW 11 COL 38 WIDGET-ID 82
     svEndInvoiceDateOption AT ROW 11 COL 41 COLON-ALIGNED HELP
          "Select End Invoice Date Option" NO-LABEL WIDGET-ID 66
     svAllUserID AT ROW 4.57 COL 103 HELP
          "All User IDs?" WIDGET-ID 158
     svStartUserID AT ROW 5.76 COL 101 COLON-ALIGNED HELP
          "Enter Start User ID" WIDGET-ID 162
     startUserIDName AT ROW 5.76 COL 117 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     svEndUserID AT ROW 6.95 COL 101 COLON-ALIGNED HELP
          "Enter End User ID" WIDGET-ID 160
     endUserIDName AT ROW 6.95 COL 117 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     svPostOutOfPeriod AT ROW 9.81 COL 103 HELP
          "Select to Post Out of Period" WIDGET-ID 370
     svPostIntoClosedPeriod AT ROW 11 COL 103 HELP
          "Select to Post Into Closed Period" WIDGET-ID 372
     svPost AT ROW 11 COL 155 HELP
          "Select to Post" WIDGET-ID 344
     btnAddEmail AT ROW 15.29 COL 49 HELP
          "Add Recipents" WIDGET-ID 636
     svRecipients AT ROW 13.62 COL 55 NO-LABEL WIDGET-ID 600
     "Email" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 13.62 COL 49 WIDGET-ID 640
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 14.33 COL 44 WIDGET-ID 602
     RECT-1 AT ROW 2.19 COL 47 WIDGET-ID 250
     RECT-2 AT ROW 4.33 COL 2 WIDGET-ID 374
     RECT-3 AT ROW 9.57 COL 2 WIDGET-ID 376
     RECT-4 AT ROW 4.33 COL 87 WIDGET-ID 378
     RECT-5 AT ROW 9.57 COL 87 WIDGET-ID 380
     RECT-6 AT ROW 13.14 COL 43 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 167.2 BY 17
         TITLE "Report Parameters".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
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
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 17
         WIDTH              = 167.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endUserIDName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endVendorName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startUserIDName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startVendorName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       svLocation:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       svPeriod:READ-ONLY IN FRAME F-Main        = TRUE.

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

&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail sObject
ON CHOOSE OF btnAddEmail IN FRAME F-Main /* Email */
DO:
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = svRecipients:SCREEN-VALUE.
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    svRecipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svPostDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 sObject
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllUserID sObject
ON VALUE-CHANGED OF svAllUserID IN FRAME F-Main /* All User IDs */
DO:
    {aoa/includes/svAllValueChanged.i svStartUserID svEndUserID}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllVendNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllVendNo sObject
ON VALUE-CHANGED OF svAllVendNo IN FRAME F-Main /* All Vendors */
DO:
    {aoa/includes/svAllValueChanged.i svStartVendNo svEndVendNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svPostDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDate sObject
ON HELP OF svEndInvoiceDate IN FRAME F-Main /* End Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDate sObject
ON LEAVE OF svEndInvoiceDate IN FRAME F-Main /* End Invoice Date */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pCheckInvDate (svCompany, {&SELF-NAME}, OUTPUT lInvalid).
    IF lInvalid THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDateOption sObject
ON VALUE-CHANGED OF svEndInvoiceDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndInvoiceDate &btnCalendar=3}
    APPLY "LEAVE":U TO svEndInvoiceDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndUserID sObject
ON LEAVE OF svEndUserID IN FRAME F-Main /* End User ID */
DO:
    endUserIDName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndVendNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndVendNo sObject
ON LEAVE OF svEndVendNo IN FRAME F-Main /* End Vendor */
DO:
    endVendorName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLocation sObject
ON ENTRY OF svLocation IN FRAME F-Main /* Location */
DO:
  APPLY "ENTRY":U TO svPostDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPeriod sObject
ON ENTRY OF svPeriod IN FRAME F-Main /* Period */
DO:
  APPLY "ENTRY":U TO svAllVendNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPostDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostDate sObject
ON HELP OF svPostDate IN FRAME F-Main /* Post Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostDate sObject
ON LEAVE OF svPostDate IN FRAME F-Main /* Post Date */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} NE ? THEN DO:
        RUN pCheckDate (svCompany, {&SELF-NAME}, OUTPUT lInvalid).
        IF lInvalid THEN RETURN /* NO-APPLY */ .
        RUN pValidateDate (svCompany, {&SELF-NAME}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPostDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostDateOption sObject
ON VALUE-CHANGED OF svPostDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svPostDate &btnCalendar=1}
    APPLY "LEAVE":U TO svPostDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPostIntoClosedPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostIntoClosedPeriod sObject
ON VALUE-CHANGED OF svPostIntoClosedPeriod IN FRAME F-Main /* Post Into Closed Period */
DO:
    ASSIGN {&SELF-NAME}.
    IF lSecure EQ NO AND {&SELF-NAME} EQ YES THEN DO:
        RUN sys/ref/d-passwd.w (1, OUTPUT lSecure).
        {&SELF-NAME}:SCREEN-VALUE = STRING(lSecure).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPostOutOfPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostOutOfPeriod sObject
ON VALUE-CHANGED OF svPostOutOfPeriod IN FRAME F-Main /* Post Out of Period */
DO:
    ASSIGN {&SELF-NAME}.
    IF lSecure EQ NO AND {&SELF-NAME} EQ YES THEN DO:
        RUN sys/ref/d-passwd.w (1, OUTPUT lSecure).
        {&SELF-NAME}:SCREEN-VALUE = STRING(lSecure).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDate sObject
ON HELP OF svStartInvoiceDate IN FRAME F-Main /* Start Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDate sObject
ON LEAVE OF svStartInvoiceDate IN FRAME F-Main /* Start Invoice Date */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pCheckInvDate (svCompany, {&SELF-NAME}, OUTPUT lInvalid).
    IF lInvalid THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDateOption sObject
ON VALUE-CHANGED OF svStartInvoiceDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartInvoiceDate &btnCalendar=2}
    APPLY "LEAVE":U TO svStartInvoiceDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartUserID sObject
ON LEAVE OF svStartUserID IN FRAME F-Main /* Start User ID */
DO:
    startUserIDName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartVendNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartVendNo sObject
ON LEAVE OF svStartVendNo IN FRAME F-Main /* Start Vendor */
DO:
    startVendorName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckDate sObject 
PROCEDURE pCheckDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtDate   AS DATE      NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.

    RUN sys/inc/valtrndt.p
        (ipcCompany, ipdtDate, OUTPUT iPeriod) NO-ERROR.
    oplInvalid = ERROR-STATUS:ERROR.
    IF NOT oplInvalid THEN
    svPeriod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iPeriod).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckInvDate sObject 
PROCEDURE pCheckInvDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtDate   AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER oplInvalid AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cErrorMsg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hObject   AS HANDLE    NO-UNDO.

    IF ipdtDate EQ ? THEN RETURN.   
   
    FIND FIRST period NO-LOCK                   
         WHERE period.company EQ ipcCompany
           AND period.pst     LE ipdtDate
           AND period.pend    GE ipdtDate
           AND period.pnum    EQ MONTH(ipdtDate)
         NO-ERROR.
    IF NOT AVAILABLE period THEN
    ASSIGN
        cErrorMsg = "Can Not POST Out Of Period"
        hObject   = svPostOutOfPeriod:HANDLE IN FRAME {&FRAME-NAME}
        .
    ELSE
    IF period.pstat EQ NO THEN
    ASSIGN
        cErrorMsg = "Period for " + STRING(ipdtDate,"99/99/9999") + " is already closed"
        hObject   = svPostIntoClosedPeriod:HANDLE IN FRAME {&FRAME-NAME}
        .
    IF cErrorMsg NE "" THEN DO:
        IF lSecure EQ NO THEN DO:
            MESSAGE
                cErrorMsg SKIP
                "Enter Security Password or Enter to Return"
                    VIEW-AS ALERT-BOX ERROR.
            RUN sys/ref/d-passwd.w (1, OUTPUT lSecure).
            IF lSecure EQ NO THEN
            oplInvalid = YES.
            ELSE hObject:SCREEN-VALUE = "yes".
        END. /* lsecure eq no */
    END. /* cerrormsg ne "" */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitialize sObject 
PROCEDURE pInitialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            hContainer = iphContainer
            svCompany:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
            svCompany
            svLocation:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetLocation' IN hContainer)
            svLocation
            .

        APPLY "VALUE-CHANGED":U TO svPostDateOption.

        APPLY "VALUE-CHANGED":U TO svAllVendNo.
        APPLY "LEAVE":U TO svStartVendNo.
        APPLY "LEAVE":U TO svEndVendNo.

        APPLY "VALUE-CHANGED":U TO svStartInvoiceDateOption.
        APPLY "VALUE-CHANGED":U TO svEndInvoiceDateOption.

        APPLY "VALUE-CHANGED":U TO svAllUserID.
        APPLY "LEAVE":U TO svStartUserID.
        APPLY "LEAVE":U TO svEndUserID.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParamValuesOverride sObject 
PROCEDURE pParamValuesOverride :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        svPost:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
        svPostOutOfPeriod:SCREEN-VALUE = "no"
        svPostIntoClosedPeriod:SCREEN-VALUE = "no"
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPopulateOptions sObject 
PROCEDURE pPopulateOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        hContainer = iphContainer.

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svPostDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartInvoiceDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndInvoiceDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateDate sObject 
PROCEDURE pValidateDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDate   AS DATE      NO-UNDO.

    DEFINE VARIABLE lRetry AS LOGICAL NO-UNDO.
    
    FOR EACH period NO-LOCK
        WHERE period.company EQ ipcCompany
          AND period.pst     LE TODAY
          AND period.pend    GE TODAY
        BY period.pst
        :
        IF period.pst  GT ipdtDate OR
           period.pend LT ipdtDate THEN DO:
            MESSAGE "Date is not in current period," SKIP
                    "would you like to re-enter..."
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lRetry.
            IF lRetry THEN DO:
                APPLY "ENTRY" TO svPostDate IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END. /* if retry */
        END. /* if not in current period */
        LEAVE.
    END. /* each period */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

