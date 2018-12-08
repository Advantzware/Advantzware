&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-custinv.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 5.2.2016

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
svStartCustNo svEndCustNo svInventoryClasses svSort ~
svIncludeInactiveCustomers svIncludeZeroQty 
&Scoped-Define DISPLAYED-OBJECTS svCompany svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svInventoryClasses ~
svSort svIncludeInactiveCustomers svIncludeZeroQty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svInventoryClasses AS CHARACTER FORMAT "X(256)" 
     LABEL "Inventory Class(es)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svSort AS CHARACTER INITIAL "Item" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item No", "Item",
"Customer Part No", "Part"
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svIncludeInactiveCustomers AS LOGICAL INITIAL no 
     LABEL "Include Inactive Customers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeZeroQty AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 60
     svCustList AT ROW 3.38 COL 21 WIDGET-ID 48
     btnCustList AT ROW 3.38 COL 57 WIDGET-ID 46
     svAllCustNo AT ROW 4.57 COL 21 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.76 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 5.76 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.95 COL 19 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 6.95 COL 36 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svInventoryClasses AT ROW 9.1 COL 19.4 COLON-ALIGNED HELP
          "Enter Inventory Class(es)" WIDGET-ID 86
     svSort AT ROW 11.24 COL 21 NO-LABEL WIDGET-ID 88
     svIncludeInactiveCustomers AT ROW 13.38 COL 21 WIDGET-ID 40
     svIncludeZeroQty AT ROW 14.57 COL 21 WIDGET-ID 84
     "Sort By?:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 11.24 COL 12 WIDGET-ID 92
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

/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
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

&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList sObject
ON CHOOSE OF btnCustList IN FRAME F-Main /* Preview */
DO:
    RUN sys/ref/CustListManager.w (svCompany, "AR15").
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


&Scoped-define SELF-NAME svCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCustList sObject
ON VALUE-CHANGED OF svCustList IN FRAME F-Main /* Use Defined Customer List */
DO:
  ASSIGN {&SELF-NAME}
      svStartCustNo:READ-ONLY  = {&SELF-NAME}
      svEndCustNo:READ-ONLY    = {&SELF-NAME}
      btnCustList:SENSITIVE    = {&SELF-NAME}
      .
  IF {&SELF-NAME} THEN
  ASSIGN svAllCustNo:SCREEN-VALUE = "no".
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


&Scoped-define SELF-NAME svInventoryClasses
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svInventoryClasses sObject
ON LEAVE OF svInventoryClasses IN FRAME F-Main /* Inventory Class(es) */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND cust.cust-no EQ {&SELF-NAME}
         NO-ERROR.
    startCustName:SCREEN-VALUE = IF AVAILABLE cust THEN cust.name
                                 ELSE "<Beginning Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSort sObject
ON VALUE-CHANGED OF svSort IN FRAME F-Main
DO:
  assign {&self-name}.
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
        
        APPLY "VALUE-CHANGED":U TO svCustList.
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

