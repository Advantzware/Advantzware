&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-commcr.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 6.15.2016

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
&Scoped-Define ENABLED-OBJECTS svCompany svCustList btnCustList svAllCustNo ~
svStartCustNo svEndCustNo svAllOrderNo svStartOrderNo svEndOrderNo ~
svStartBOLDate btnCalendar-1 svStartBOLDateOption svEndBOLDate ~
btnCalendar-2 svEndBOLDateOption svPrinter svAllBOL svStartBOL svEndBOL 
&Scoped-Define DISPLAYED-OBJECTS svCompany svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svAllOrderNo ~
svStartOrderNo svEndOrderNo svStartBOLDate svStartBOLDateOption ~
svEndBOLDate svEndBOLDateOption svPrinter svAllBOL svStartBOL svEndBOL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
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

DEFINE VARIABLE svStartBOLDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndBOL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End BOL" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndBOLDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svPrinter AS CHARACTER FORMAT "X(256)" 
     LABEL "Printer ID" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE svStartBOL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start BOL" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartBOLDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start BOL Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 78.8 BY 5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 3.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 3.81.

DEFINE VARIABLE svAllBOL AS LOGICAL INITIAL yes 
     LABEL "All BOLs" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllOrderNo AS LOGICAL INITIAL yes 
     LABEL "All Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 142 COLON-ALIGNED WIDGET-ID 60
     svCustList AT ROW 3.14 COL 32 WIDGET-ID 48
     btnCustList AT ROW 3.14 COL 62 WIDGET-ID 46
     svAllCustNo AT ROW 4.33 COL 32 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.52 COL 30 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 5.52 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.71 COL 30 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 6.71 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllOrderNo AT ROW 4.33 COL 117 HELP
          "All Orders?" WIDGET-ID 196
     svStartOrderNo AT ROW 5.52 COL 115 COLON-ALIGNED HELP
          "Enter Start Order" WIDGET-ID 200
     svEndOrderNo AT ROW 6.71 COL 115 COLON-ALIGNED HELP
          "Enter End Order" WIDGET-ID 198
     svStartBOLDate AT ROW 10.29 COL 45 COLON-ALIGNED HELP
          "Enter Start BOL Date" WIDGET-ID 242
     btnCalendar-1 AT ROW 10.29 COL 63 WIDGET-ID 234
     svStartBOLDateOption AT ROW 10.29 COL 66 COLON-ALIGNED HELP
          "Select Start BOL Date Option" NO-LABEL WIDGET-ID 244
     svEndBOLDate AT ROW 11.48 COL 45 COLON-ALIGNED HELP
          "Enter End BOL Date" WIDGET-ID 238
     btnCalendar-2 AT ROW 11.48 COL 63 WIDGET-ID 236
     svEndBOLDateOption AT ROW 11.48 COL 66 COLON-ALIGNED HELP
          "Select End BOL Date Option" NO-LABEL WIDGET-ID 240
     svPrinter AT ROW 13.86 COL 45 COLON-ALIGNED WIDGET-ID 252
     svAllBOL AT ROW 10.29 COL 121 HELP
          "All BOLs?" WIDGET-ID 246
     svStartBOL AT ROW 11.48 COL 119 COLON-ALIGNED HELP
          "Enter Start BOL" WIDGET-ID 250
     svEndBOL AT ROW 12.67 COL 119 COLON-ALIGNED HELP
          "Enter End BOL" WIDGET-ID 248
     RECT-1 AT ROW 2.91 COL 15 WIDGET-ID 254
     RECT-2 AT ROW 10.05 COL 29 WIDGET-ID 256
     RECT-3 AT ROW 4.1 COL 103 WIDGET-ID 258
     RECT-4 AT ROW 10.05 COL 103 WIDGET-ID 260
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.2 BY 17
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
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

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

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartBOLDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
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


&Scoped-define SELF-NAME svAllOrderNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllOrderNo sObject
ON VALUE-CHANGED OF svAllOrderNo IN FRAME F-Main /* All Orders */
DO:
    {aoa/includes/svAllValueChanged.i svStartOrderNo svEndOrderNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svPrinter.
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
    {aoa/includes/tDateOption.i &dateObject=svEndBOLDate &btnCalendar=2}
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
    {aoa/includes/tDateOption.i &dateObject=svStartBOLDate &btnCalendar=1}
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
            .

        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllOrderNo.

        APPLY "VALUE-CHANGED":U TO svAllBOL.
    END.

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
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartBOLDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndBOLDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

