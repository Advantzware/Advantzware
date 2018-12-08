&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: edi810Xcp.w

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
&Scoped-Define ENABLED-OBJECTS svCompany svLocation svAllCustNo ~
svStartCustNo svEndCustNo svStartInvoiceDate btnCalendar-5 ~
svStartInvoiceDateOption svEndInvoiceDate btnCalendar-6 ~
svEndInvoiceDateOption svExceptionOnly 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svStartInvoiceDate ~
svStartInvoiceDateOption svEndInvoiceDate svEndInvoiceDateOption ~
svExceptionOnly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-5 btnCalendar-6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-6 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE svEndInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83.8 BY 4.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83.8 BY 3.05.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svExceptionOnly AS LOGICAL INITIAL no 
     LABEL "Exception Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 121 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 137 COLON-ALIGNED WIDGET-ID 232
     svAllCustNo AT ROW 4.33 COL 57 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.52 COL 55 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 5.52 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.71 COL 55 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 6.71 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svStartInvoiceDate AT ROW 10.05 COL 55 COLON-ALIGNED HELP
          "Enter Start Invoice Date" WIDGET-ID 26
     btnCalendar-5 AT ROW 10.05 COL 73 WIDGET-ID 80
     svStartInvoiceDateOption AT ROW 10.05 COL 76 COLON-ALIGNED HELP
          "Select Start Invoice Date Option" NO-LABEL WIDGET-ID 64
     svEndInvoiceDate AT ROW 11.24 COL 55 COLON-ALIGNED HELP
          "Enter End Invoice Date" WIDGET-ID 24
     btnCalendar-6 AT ROW 11.24 COL 73 WIDGET-ID 82
     svEndInvoiceDateOption AT ROW 11.24 COL 76 COLON-ALIGNED HELP
          "Select End Invoice Date Option" NO-LABEL WIDGET-ID 66
     svExceptionOnly AT ROW 13.14 COL 57 HELP
          "Select to Show Exception Only" WIDGET-ID 88
     RECT-1 AT ROW 4.1 COL 36 WIDGET-ID 234
     RECT-2 AT ROW 9.57 COL 36 WIDGET-ID 236
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-5 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-6 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
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

&Scoped-define SELF-NAME btnCalendar-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-5 sObject
ON CHOOSE OF btnCalendar-5 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-6 sObject
ON CHOOSE OF btnCalendar-6 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndInvoiceDate}
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


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllCustNo.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME svEndInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDate sObject
ON HELP OF svEndInvoiceDate IN FRAME F-Main /* End Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDateOption sObject
ON VALUE-CHANGED OF svEndInvoiceDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndInvoiceDate &btnCalendar=6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLocation sObject
ON ENTRY OF svLocation IN FRAME F-Main /* Location */
DO:
  APPLY "ENTRY":U TO svAllCustNo.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME svStartInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDate sObject
ON HELP OF svStartInvoiceDate IN FRAME F-Main /* Start Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDateOption sObject
ON VALUE-CHANGED OF svStartInvoiceDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartInvoiceDate &btnCalendar=5}
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
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.

        APPLY "VALUE-CHANGED":U TO svStartInvoiceDateOption.
        APPLY "VALUE-CHANGED":U TO svEndInvoiceDateOption.
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

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartInvoiceDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndInvoiceDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

