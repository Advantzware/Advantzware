&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-commcr.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: 
  Created: 

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

DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

{sys/ref/CustList.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svCompany svStartOrd svEndOrd svEndFgItem ~
svStartFgItem svStartCustNo svEndCustNo svStartOrdDate svStartOrdDateOption ~
svEndOrdDate svEndOrdDateOption svStartReceiptDate svStartReceiptDateOption ~
svReceUse svEndReceiptDate svEndReceiptDateOption svShipUse svStartShipDate ~
svStartShipDateOption svEndShipDate svEndShipDateOption svMisc 
&Scoped-Define DISPLAYED-OBJECTS svCompany svStartOrd svEndOrd svEndFgItem ~
svStartFgItem svStartCustNo startCustName svEndCustNo endCustName ~
svStartOrdDate svStartOrdDateOption svEndOrdDate svEndOrdDateOption ~
svStartReceiptDate svStartReceiptDateOption svReceUse svEndReceiptDate ~
svEndReceiptDateOption svShipUse svStartShipDate svStartShipDateOption ~
svEndShipDate svEndShipDateOption svMisc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE svEndOrdDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndShipDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartOrdDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartShipDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

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

DEFINE VARIABLE svEndFgItem AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svEndOrd AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "End Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svEndOrdDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndShipDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartFgItem AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svStartOrd AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Start Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svStartOrdDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartShipDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svMisc AS LOGICAL INITIAL no 
     LABEL "Print Misc. Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE svReceUse AS LOGICAL INITIAL no 
     LABEL "Use?" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE svShipUse AS LOGICAL INITIAL no 
     LABEL "Use?" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 60
     svStartOrd AT ROW 2.67 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Order#" WIDGET-ID 78
     svEndOrd AT ROW 2.67 COL 55.4 COLON-ALIGNED HELP
          "Enter Ending Order" WIDGET-ID 76
     svEndFgItem AT ROW 3.91 COL 55.4 COLON-ALIGNED HELP
          "Enter Ending FGItem" WIDGET-ID 20
     svStartFgItem AT ROW 4 COL 19 COLON-ALIGNED HELP
          "Enter Beginning FGItem#" WIDGET-ID 22
     svStartCustNo AT ROW 5.24 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 5.24 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.43 COL 19 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 6.43 COL 36 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svStartOrdDate AT ROW 8.33 COL 19 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 84
     svStartOrdDateOption AT ROW 8.33 COL 37.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 86
     svEndOrdDate AT ROW 9.52 COL 19 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 80
     svEndOrdDateOption AT ROW 9.52 COL 37.2 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 82
     svStartReceiptDate AT ROW 11.19 COL 19 COLON-ALIGNED HELP
          "Enter Start Receipt Date" WIDGET-ID 72
     svStartReceiptDateOption AT ROW 11.19 COL 37.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 74
     svReceUse AT ROW 11.24 COL 66 WIDGET-ID 42
     svEndReceiptDate AT ROW 12.38 COL 19 COLON-ALIGNED HELP
          "Enter End Receipt Date" WIDGET-ID 68
     svEndReceiptDateOption AT ROW 12.38 COL 37.2 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 70
     svShipUse AT ROW 13.86 COL 66 WIDGET-ID 44
     svStartShipDate AT ROW 13.95 COL 19 COLON-ALIGNED HELP
          "Enter Start Ship Date" WIDGET-ID 26
     svStartShipDateOption AT ROW 13.95 COL 37.2 COLON-ALIGNED HELP
          "Select Start Ship Date Option" NO-LABEL WIDGET-ID 64
     svEndShipDate AT ROW 15.14 COL 19 COLON-ALIGNED HELP
          "Enter End Ship Date" WIDGET-ID 24
     svEndShipDateOption AT ROW 15.14 COL 37.2 COLON-ALIGNED HELP
          "Select End Ship Date Option" NO-LABEL WIDGET-ID 66
     svMisc AT ROW 17.43 COL 25.4 WIDGET-ID 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.4 BY 19.29
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
         HEIGHT             = 19.29
         WIDTH              = 86.4.
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

&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svStartOrd.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND cust.cust-no EQ {&SELF-NAME}
         NO-ERROR.
    endCustName:SCREEN-VALUE = IF AVAILABLE cust THEN cust.name
                               ELSE "<Ending Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndFgItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndFgItem sObject
ON LEAVE OF svEndFgItem IN FRAME F-Main /* End Item# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOrd sObject
ON LEAVE OF svEndOrd IN FRAME F-Main /* End Order# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOrdDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOrdDateOption sObject
ON VALUE-CHANGED OF svEndOrdDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndReceiptDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndReceiptDateOption sObject
ON VALUE-CHANGED OF svEndReceiptDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndShipDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndShipDateOption sObject
ON VALUE-CHANGED OF svEndShipDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndShipDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustNo sObject
ON LEAVE OF svStartCustNo IN FRAME F-Main /* Start Customer */
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


&Scoped-define SELF-NAME svStartFgItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartFgItem sObject
ON LEAVE OF svStartFgItem IN FRAME F-Main /* Start Item# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOrd sObject
ON LEAVE OF svStartOrd IN FRAME F-Main /* Start Order# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOrdDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOrdDateOption sObject
ON VALUE-CHANGED OF svStartOrdDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartReceiptDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartReceiptDateOption sObject
ON VALUE-CHANGED OF svStartReceiptDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartShipDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartShipDateOption sObject
ON VALUE-CHANGED OF svStartShipDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartShipDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
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
        APPLY "VALUE-CHANGED":U TO svStartReceiptDateOption.
        APPLY "VALUE-CHANGED":U TO svEndReceiptDateOption.
        APPLY "VALUE-CHANGED":U TO svStartShipDateOption.
        APPLY "VALUE-CHANGED":U TO svEndShipDateOption.
        /*APPLY "VALUE-CHANGED":U TO svCustList.
        APPLY "VALUE-CHANGED":U TO svAllCustomers.
        APPLY "VALUE-CHANGED":U TO svAllSalesReps.*/
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        APPLY "LEAVE":U TO svStartOrd.
        APPLY "LEAVE":U TO svEndOrd.
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
        ASSIGN
            hContainer = iphContainer
            .
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartReceiptDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndReceiptDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrdDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrdDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartShipDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndShipDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCustRange sObject 
PROCEDURE pSetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartCustNo:READ-ONLY = iplChecked
          svEndCustNo:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

