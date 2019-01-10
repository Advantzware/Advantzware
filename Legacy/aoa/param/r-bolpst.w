&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-bolpst.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 3.19.2016

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

&SCOPED-DEFINE useCustList
{aoa/includes/aoaParamVars.i}

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
btnCalendar-1 svPostDateOption svStartBOLDate btnCalendar-2 ~
svStartBOLDateOption svEndBOLDate btnCalendar-3 svEndBOLDateOption svAllLoc ~
svStartLoc svEndLoc svCustList btnCustList svAllCustNo svStartCustNo ~
svEndCustNo svAllBOL svStartBOL svEndBOL svAllLocBin svStartLocBin ~
svEndLocBin svPost btnAddEmail svRecipients 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svPostDate ~
svPostDateOption svStartBOLDate svStartBOLDateOption svEndBOLDate ~
svEndBOLDateOption svAllLoc svStartLoc startLocName svEndLoc endLocName ~
svCustList svAllCustNo svStartCustNo startCustName svEndCustNo endCustName ~
svAllBOL svStartBOL svEndBOL svAllLocBin svStartLocBin svEndLocBin svPost ~
svRecipients 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svEndBOLDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svPostDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartBOLDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 2.86
     BGCOLOR 15 .

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE endLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE startLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndBOL AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "End BOL" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE svEndBOLDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "End Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndLocBin AS CHARACTER FORMAT "X(8)" 
     LABEL "End Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svPostDate AS DATE FORMAT "99/99/9999" 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartBOL AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Start BOL" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE svStartBOLDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartLocBin AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66.8 BY 5.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66.8 BY 4.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 75 BY 5.67.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 75 BY 4.29.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 79 BY 2.86
     BGCOLOR 11 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.57.

DEFINE VARIABLE svAllBOL AS LOGICAL INITIAL yes 
     LABEL "All BOLs" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllLoc AS LOGICAL INITIAL yes 
     LABEL "All Warehouses" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllLocBin AS LOGICAL INITIAL yes 
     LABEL "All Bins" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE svPost AS LOGICAL INITIAL no 
     LABEL "POST" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 121 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 137 COLON-ALIGNED WIDGET-ID 130
     svPostDate AT ROW 2.91 COL 18 COLON-ALIGNED HELP
          "Enter Post Date" WIDGET-ID 274
     btnCalendar-1 AT ROW 2.91 COL 36 WIDGET-ID 272
     svPostDateOption AT ROW 2.91 COL 39 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 276
     svStartBOLDate AT ROW 5.29 COL 18 COLON-ALIGNED HELP
          "Enter Start BOL Date" WIDGET-ID 242
     btnCalendar-2 AT ROW 5.29 COL 36 WIDGET-ID 234
     svStartBOLDateOption AT ROW 5.29 COL 39 COLON-ALIGNED HELP
          "Select Start BOL Date Option" NO-LABEL WIDGET-ID 244
     svEndBOLDate AT ROW 6.48 COL 18 COLON-ALIGNED HELP
          "Enter End BOL Date" WIDGET-ID 238
     btnCalendar-3 AT ROW 6.48 COL 36 WIDGET-ID 236
     svEndBOLDateOption AT ROW 6.48 COL 39 COLON-ALIGNED HELP
          "Select End BOL Date Option" NO-LABEL WIDGET-ID 240
     svAllLoc AT ROW 9.1 COL 22 HELP
          "All Warehouses?" WIDGET-ID 328
     svStartLoc AT ROW 10.29 COL 20 COLON-ALIGNED HELP
          "Enter Start Warehouse" WIDGET-ID 332
     startLocName AT ROW 10.29 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 326
     svEndLoc AT ROW 11.48 COL 20 COLON-ALIGNED HELP
          "Enter End Warehouse" WIDGET-ID 330
     endLocName AT ROW 11.48 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 324
     svCustList AT ROW 2.91 COL 89 WIDGET-ID 48
     btnCustList AT ROW 2.91 COL 125 WIDGET-ID 46
     svAllCustNo AT ROW 4.1 COL 89 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.29 COL 87 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 5.29 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.48 COL 87 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 6.48 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllBOL AT ROW 9.1 COL 90 HELP
          "All BOLs?" WIDGET-ID 318
     svStartBOL AT ROW 10.29 COL 88 COLON-ALIGNED HELP
          "Enter Start BOL" WIDGET-ID 322
     svEndBOL AT ROW 11.48 COL 88 COLON-ALIGNED HELP
          "Enter End BOL" WIDGET-ID 320
     svAllLocBin AT ROW 9.1 COL 114 HELP
          "All Bins?" WIDGET-ID 282
     svStartLocBin AT ROW 10.29 COL 112 COLON-ALIGNED HELP
          "Enter Start Bin" WIDGET-ID 286
     svEndLocBin AT ROW 11.48 COL 112 COLON-ALIGNED HELP
          "Enter End Bin" WIDGET-ID 284
     svPost AT ROW 11.48 COL 135 HELP
          "Select to Post" WIDGET-ID 344
     btnAddEmail AT ROW 15.52 COL 40 HELP
          "Add Recipents" WIDGET-ID 636
     svRecipients AT ROW 13.86 COL 46 NO-LABEL WIDGET-ID 600
     "Bills of Lading MUST BE printed prior to posting!" VIEW-AS TEXT
          SIZE 58 BY .95 AT ROW 19.1 COL 47 WIDGET-ID 336
          BGCOLOR 11 FONT 5
     "The Edit List will show all available BOL to be posted to all orders" VIEW-AS TEXT
          SIZE 77 BY 1.19 AT ROW 17.67 COL 37 WIDGET-ID 338
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "Email" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 13.86 COL 40 WIDGET-ID 640
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 14.57 COL 35 WIDGET-ID 602
     RECT-5 AT ROW 17.43 COL 36 WIDGET-ID 334
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.2 BY 20.52.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-1 AT ROW 2.43 COL 2 WIDGET-ID 346
     RECT-2 AT ROW 8.62 COL 2 WIDGET-ID 348
     RECT-3 AT ROW 2.43 COL 71 WIDGET-ID 350
     RECT-4 AT ROW 8.62 COL 71 WIDGET-ID 352
     RECT-6 AT ROW 13.38 COL 34 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.2 BY 20.52
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
         HEIGHT             = 20.52
         WIDTH              = 149.2.
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
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endLocName IN FRAME F-Main
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
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startLocName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       svLocation:READ-ONLY IN FRAME F-Main        = TRUE.

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
  {methods/btnCalendar.i svStartBOLDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 sObject
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndBOLDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList sObject
ON CHOOSE OF btnCustList IN FRAME F-Main /* Preview */
DO:
    RUN sys/ref/CustListManager.w (svCompany, "AR15").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllBOL sObject
ON VALUE-CHANGED OF svAllBOL IN FRAME F-Main /* All BOLs */
DO:
    {aoa/includes/svAllValueChanged.i svStartBOL svEndBOL}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCustNo sObject
ON VALUE-CHANGED OF svAllCustNo IN FRAME F-Main /* All Customers */
DO:
    {aoa/includes/svAllValueChanged.i svStartCustNo svEndCustNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllLoc sObject
ON VALUE-CHANGED OF svAllLoc IN FRAME F-Main /* All Warehouses */
DO:
    {aoa/includes/svAllValueChanged.i svStartLoc svEndLoc}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllLocBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllLocBin sObject
ON VALUE-CHANGED OF svAllLocBin IN FRAME F-Main /* All Bins */
DO:
    {aoa/includes/svAllValueChanged.i svStartLocBin svEndLocBin}
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


&Scoped-define SELF-NAME svCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCustList sObject
ON VALUE-CHANGED OF svCustList IN FRAME F-Main /* Use Defined Customer List */
DO:
  ASSIGN {&SELF-NAME}
      svStartCustNo:READ-ONLY = {&SELF-NAME}
      svEndCustNo:READ-ONLY   = {&SELF-NAME}
      btnCustList:SENSITIVE   = {&SELF-NAME}
      .
  IF {&SELF-NAME} THEN
  ASSIGN svAllCustNo:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndBOLDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndBOLDate sObject
ON HELP OF svEndBOLDate IN FRAME F-Main /* End BOL Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndBOLDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndBOLDateOption sObject
ON VALUE-CHANGED OF svEndBOLDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndBOLDate &btnCalendar=3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    endCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndLoc sObject
ON LEAVE OF svEndLoc IN FRAME F-Main /* End Warehouse */
DO:
    endLocName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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


&Scoped-define SELF-NAME svPostDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostDate sObject
ON HELP OF svPostDate IN FRAME F-Main /* Post Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPostDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPostDateOption sObject
ON VALUE-CHANGED OF svPostDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svPostDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartBOLDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartBOLDate sObject
ON HELP OF svStartBOLDate IN FRAME F-Main /* Start BOL Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartBOLDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartBOLDateOption sObject
ON VALUE-CHANGED OF svStartBOLDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartBOLDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustNo sObject
ON LEAVE OF svStartCustNo IN FRAME F-Main /* Start Customer */
DO:
    startCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartLoc sObject
ON LEAVE OF svStartLoc IN FRAME F-Main /* Start Warehouse */
DO:
    startLocName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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

        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.

        APPLY "VALUE-CHANGED":U TO svStartBOLDateOption.
        APPLY "VALUE-CHANGED":U TO svEndBOLDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllBOL.
        
        APPLY "VALUE-CHANGED":U TO svAllLoc.
        APPLY "LEAVE":U TO svStartLoc.
        APPLY "LEAVE":U TO svEndLoc.
        
        APPLY "VALUE-CHANGED":U TO svAllLocBin.
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
    svPost:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".

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
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartBOLDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndBOLDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

