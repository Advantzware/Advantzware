&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: rd-fgexp.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 11.3.2016

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
&Scoped-Define ENABLED-OBJECTS svCompany svAllItemNo svStartItemNo ~
svEndItemNo svAllItemName svStartItemName svEndItemName svAllCustPart ~
svStartCustPart svEndCustPart svCustList btnCustList svAllCustNo ~
svStartCustNo svEndCustNo svAllEstimate svStartEstimate svEndEstimate ~
svAllStyle svStartStyle svEndStyle svAllProdCategory svStartProdCategory ~
svEndProdCategory svActive svInactive svSpecNote 
&Scoped-Define DISPLAYED-OBJECTS svCompany svAllItemNo svStartItemNo ~
startItemName svEndItemNo endItemName svAllItemName svStartItemName ~
svEndItemName svAllCustPart svStartCustPart startCustPartName svEndCustPart ~
endCustPartName svCustList svAllCustNo svStartCustNo startCustName ~
svEndCustNo endCustName svAllEstimate svStartEstimate svEndEstimate ~
svAllStyle svStartStyle startStyleDescription svEndStyle ~
endStyleDescription svAllProdCategory svStartProdCategory ~
startProdCategoryName svEndProdCategory endProdCategoryName svActive ~
svInactive svSpecNote 

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
     SIZE 45 BY 1.

DEFINE VARIABLE endCustPartName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE endProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endStyleDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startCustPartName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE startProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startStyleDescription AS CHARACTER FORMAT "X(30)" 
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

DEFINE VARIABLE svEndCustPart AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer Part" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndEstimate AS CHARACTER FORMAT "X(8)" 
     LABEL "End Estimate" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndItemName AS CHARACTER FORMAT "X(256)" 
     LABEL "End Item Name" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "End Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndStyle AS CHARACTER FORMAT "X(8)" 
     LABEL "End Style" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartCustPart AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer Part" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartEstimate AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Estimate" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartItemName AS CHARACTER FORMAT "X(256)" 
     LABEL "Start Item Name" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartStyle AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Style" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svActive AS LOGICAL INITIAL no 
     LABEL "Active" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE svAllCustPart AS LOGICAL INITIAL yes 
     LABEL "All Customer Parts" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .95 NO-UNDO.

DEFINE VARIABLE svAllEstimate AS LOGICAL INITIAL yes 
     LABEL "All Estimates" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllItemName AS LOGICAL INITIAL yes 
     LABEL "All Item Names" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllProdCategory AS LOGICAL INITIAL yes 
     LABEL "All Product Categories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE svAllStyle AS LOGICAL INITIAL yes 
     LABEL "All Styles" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svInactive AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svSpecNote AS LOGICAL INITIAL no 
     LABEL "Spec Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 20 COLON-ALIGNED WIDGET-ID 60
     svAllItemNo AT ROW 2.67 COL 22 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 3.86 COL 20 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 3.86 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 5.05 COL 20 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 5.05 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svAllItemName AT ROW 6.71 COL 22 HELP
          "All Item Names?" WIDGET-ID 142
     svStartItemName AT ROW 7.91 COL 20 COLON-ALIGNED HELP
          "Enter Start Item Name" WIDGET-ID 146
     svEndItemName AT ROW 9.1 COL 20 COLON-ALIGNED HELP
          "Enter End Item Name" WIDGET-ID 144
     svAllCustPart AT ROW 10.76 COL 22 HELP
          "All Customer Parts?" WIDGET-ID 158
     svStartCustPart AT ROW 11.95 COL 20 COLON-ALIGNED HELP
          "Enter Start Customer Part" WIDGET-ID 162
     startCustPartName AT ROW 11.95 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     svEndCustPart AT ROW 13.14 COL 20 COLON-ALIGNED HELP
          "Enter End Customer Part" WIDGET-ID 160
     endCustPartName AT ROW 13.14 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     svCustList AT ROW 14.81 COL 22 WIDGET-ID 48
     btnCustList AT ROW 14.81 COL 52 WIDGET-ID 46
     svAllCustNo AT ROW 16 COL 22 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 17.19 COL 20 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 17.19 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 18.38 COL 20 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 18.38 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllEstimate AT ROW 20.05 COL 22 HELP
          "All Estimates?" WIDGET-ID 148
     svStartEstimate AT ROW 21.24 COL 20 COLON-ALIGNED HELP
          "Enter Start Estimate" WIDGET-ID 152
     svEndEstimate AT ROW 22.43 COL 20 COLON-ALIGNED HELP
          "Enter End Estimate" WIDGET-ID 150
     svAllStyle AT ROW 24.1 COL 22 HELP
          "All Styles?" WIDGET-ID 58
     svStartStyle AT ROW 25.29 COL 20 COLON-ALIGNED HELP
          "Enter Start Style" WIDGET-ID 22
     startStyleDescription AT ROW 25.29 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     svEndStyle AT ROW 26.48 COL 20 COLON-ALIGNED HELP
          "Enter End Style" WIDGET-ID 20
     endStyleDescription AT ROW 26.48 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     svAllProdCategory AT ROW 28.38 COL 22 HELP
          "All Sales Reps?" WIDGET-ID 202
     svStartProdCategory AT ROW 29.57 COL 20 COLON-ALIGNED HELP
          "Enter Start Product Category" WIDGET-ID 206
     startProdCategoryName AT ROW 29.57 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndProdCategory AT ROW 30.76 COL 20 COLON-ALIGNED HELP
          "Enter End Product Category" WIDGET-ID 204
     endProdCategoryName AT ROW 30.76 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     svActive AT ROW 32.43 COL 22 HELP
          "Select to Show Active Items" WIDGET-ID 88
     svInactive AT ROW 32.43 COL 39 HELP
          "Select to Show Inactive Items" WIDGET-ID 324
     svSpecNote AT ROW 32.43 COL 58 HELP
          "Select to Show Spec Notes" WIDGET-ID 326
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83 BY 33.91
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
         HEIGHT             = 33.91
         WIDTH              = 83.
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
/* SETTINGS FOR FILL-IN endCustPartName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endStyleDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustPartName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startStyleDescription IN FRAME F-Main
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


&Scoped-define SELF-NAME svAllCustPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCustPart sObject
ON VALUE-CHANGED OF svAllCustPart IN FRAME F-Main /* All Customer Parts */
DO:
    {aoa/includes/svAllValueChanged.i svStartCustPart svEndCustPart}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllEstimate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllEstimate sObject
ON VALUE-CHANGED OF svAllEstimate IN FRAME F-Main /* All Estimates */
DO:
    {aoa/includes/svAllValueChanged.i svStartEstimate svEndEstimate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllItemName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllItemName sObject
ON VALUE-CHANGED OF svAllItemName IN FRAME F-Main /* All Item Names */
DO:
    {aoa/includes/svAllValueChanged.i svStartItemName svEndItemName}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllItemNo sObject
ON VALUE-CHANGED OF svAllItemNo IN FRAME F-Main /* All Items */
DO:
    {aoa/includes/svAllValueChanged.i svStartItemNo svEndItemNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllProdCategory sObject
ON VALUE-CHANGED OF svAllProdCategory IN FRAME F-Main /* All Product Categories */
DO:
    {aoa/includes/svAllValueChanged.i svStartProdCategory svEndProdCategory}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllStyle sObject
ON VALUE-CHANGED OF svAllStyle IN FRAME F-Main /* All Styles */
DO:
    {aoa/includes/svAllValueChanged.i svStartStyle svEndStyle}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllItemNo.
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


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    endCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustPart sObject
ON LEAVE OF svEndCustPart IN FRAME F-Main /* End Customer Part */
DO:
    endCustPartName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItemNo sObject
ON LEAVE OF svEndItemNo IN FRAME F-Main /* End Item */
DO:
    endItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndProdCategory sObject
ON LEAVE OF svEndProdCategory IN FRAME F-Main /* End Prod Category */
DO:
    endProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndStyle sObject
ON LEAVE OF svEndStyle IN FRAME F-Main /* End Style */
DO:
    endStyleDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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


&Scoped-define SELF-NAME svStartCustPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustPart sObject
ON LEAVE OF svStartCustPart IN FRAME F-Main /* Start Customer Part */
DO:
    startCustPartName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItemNo sObject
ON LEAVE OF svStartItemNo IN FRAME F-Main /* Start Item */
DO:
    startItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartProdCategory sObject
ON LEAVE OF svStartProdCategory IN FRAME F-Main /* Start Prod Category */
DO:
    startProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartStyle sObject
ON LEAVE OF svStartStyle IN FRAME F-Main /* Start Style */
DO:
    startStyleDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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

        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
        
        APPLY "VALUE-CHANGED":U TO svAllItemName.
        APPLY "LEAVE":U TO svStartItemName.
        APPLY "LEAVE":U TO svEndItemName.
        
        APPLY "VALUE-CHANGED":U TO svAllCustPart.
        APPLY "LEAVE":U TO svStartCustPart.
        APPLY "LEAVE":U TO svEndCustPart.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllEstimate.

        APPLY "VALUE-CHANGED":U TO svAllStyle.
        APPLY "LEAVE":U TO svStartStyle.
        APPLY "LEAVE":U TO svEndStyle.
        
        APPLY "VALUE-CHANGED":U TO svAllProdCategory.
        APPLY "LEAVE":U TO svStartProdCategory.
        APPLY "LEAVE":U TO svEndProdCategory.
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
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

