&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:

  Description: from SMART.W - Template for basic SmartObject

  Author:
  Created:

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEFINE VARIABLE cUserFieldListFormat AS CHARACTER NO-UNDO INIT "Overs|DECIMAL|999.99,Unit Weight|DECIMAL|9999999999.999999,Pallet Weight|DECIMAL|9999999999.999999,Lot Number|CHARACTER|X(20),SSCC|CHARACTER|X(30),Item Description|CHARACTER|X(30),Customer Part #|CHARACTER|X(30),Location / Bin|CHARACTER|X(20)".

DEFINE VARIABLE lDisableWidgets AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iNumUserFields  AS INTEGER NO-UNDO INITIAL 3.

DEFINE TEMP-TABLE ttUserField NO-UNDO
    FIELD fieldLabel  AS CHARACTER
    FIELD fieldFormat AS CHARACTER
    FIELD fieldType   AS CHARACTER
    .

DEFINE VARIABLE hdUserFieldValue AS HANDLE    NO-UNDO EXTENT 3.
DEFINE VARIABLE cUserFieldValue  AS CHARACTER NO-UNDO EXTENT 3.

DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 01/12/21 -  5:44 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rUserFieldsRectangle 
&Scoped-Define DISPLAYED-OBJECTS fiUserFieldsRectangleLabel cbUserField1 ~
cbUserField2 cbUserField3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbUserField1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE cbUserField2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE cbUserField3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserFieldsRectangleLabel AS CHARACTER FORMAT "X(256)":U INITIAL "User Fields" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95
     FONT 6 NO-UNDO.

DEFINE RECTANGLE rUserFieldsRectangle
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 68 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiUserFieldsRectangleLabel AT ROW 1 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     cbUserField1 AT ROW 2.62 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     cbUserField2 AT ROW 4.05 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     cbUserField3 AT ROW 5.48 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     rUserFieldsRectangle AT ROW 1.38 COL 1 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 36 WIDGET-ID 100.


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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 9.19
         WIDTH              = 68.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cbUserField1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       cbUserField1:PRIVATE-DATA IN FRAME F-Main     = 
                "User Field 1".

/* SETTINGS FOR COMBO-BOX cbUserField2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       cbUserField2:PRIVATE-DATA IN FRAME F-Main     = 
                "User Field 2".

/* SETTINGS FOR COMBO-BOX cbUserField3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       cbUserField3:PRIVATE-DATA IN FRAME F-Main     = 
                "User Field 3".

/* SETTINGS FOR FILL-IN fiUserFieldsRectangleLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiUserFieldsRectangleLabel:PRIVATE-DATA IN FRAME F-Main     = 
                "UserFieldsRectangleLabel".

ASSIGN 
       rUserFieldsRectangle:PRIVATE-DATA IN FRAME F-Main     = 
                "UserFieldsRectangle".

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

&Scoped-define SELF-NAME cbUserField1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserField1 s-object
ON VALUE-CHANGED OF cbUserField1 IN FRAME F-Main
DO:
    RUN pUpdateUserField (
        INPUT SELF:SCREEN-VALUE,
        INPUT "",
        INPUT 1
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUserField2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserField2 s-object
ON VALUE-CHANGED OF cbUserField2 IN FRAME F-Main
DO:
    RUN pUpdateUserField (
        INPUT SELF:SCREEN-VALUE,
        INPUT "",
        INPUT 2
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUserField3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserField3 s-object
ON VALUE-CHANGED OF cbUserField3 IN FRAME F-Main
DO:
    RUN pUpdateUserField (
        INPUT SELF:SCREEN-VALUE,
        INPUT "",
        INPUT 3
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableUserFields s-object 
PROCEDURE DisableUserFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
/*    ASSIGN                            */
/*        cbUserField1:SENSITIVE = FALSE*/
/*        cbUserField2:SENSITIVE = FALSE*/
/*        cbUserField3:SENSITIVE = FALSE*/
/*        .                             */
/*                                      */

    DO iCount = 1 TO iNumUserFields:
        IF VALID-HANDLE(hdUserFieldValue[iCount]) THEN
            hdUserFieldValue[iCount]:SENSITIVE = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableUserFields s-object 
PROCEDURE EnableUserFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN pUpdateUserField (
        INPUT cbUserField1:SCREEN-VALUE,
        INPUT "",
        INPUT 1
        ).
        
    RUN pUpdateUserField (
        INPUT cbUserField2:SCREEN-VALUE,
        INPUT "",
        INPUT 2
        ).
 
     RUN pUpdateUserField (
        INPUT cbUserField3:SCREEN-VALUE,
        INPUT "",
        INPUT 3
        ).
                               
    DO iCount = 1 TO iNumUserFields:
        IF VALID-HANDLE(hdUserFieldValue[iCount]) THEN
            hdUserFieldValue[iCount]:SENSITIVE = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOvers s-object 
PROCEDURE GetOvers :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opdOvers AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    DO iCount = 1 TO iNumUserFields:
        IF VALID-HANDLE(hdUserFieldValue[iCount]) AND hdUserFieldValue[iCount]:PRIVATE-DATA EQ "Overs" THEN DO:
            opdOvers = DECIMAL(hdUserFieldValue[iCount]:SCREEN-VALUE) NO-ERROR.
            LEAVE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserFields s-object 
PROCEDURE GetUserFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcUserField1      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserField2      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserField3      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue1 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserFieldValue3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        opcUserField1      = cbUserField1:SCREEN-VALUE
        opcUserField2      = cbUserField1:SCREEN-VALUE
        opcUserField3      = cbUserField1:SCREEN-VALUE
        .   
        
    DO iCount = 1 TO iNumUserFields:
        IF VALID-HANDLE(hdUserFieldValue[iCount]) THEN DO:
            IF iCount EQ 1 THEN
                opcUserFieldValue1 = hdUserFieldValue[iCount]:SCREEN-VALUE.
            ELSE IF iCount EQ 2 THEN
                opcUserFieldValue2 = hdUserFieldValue[iCount]:SCREEN-VALUE.
            ELSE IF iCount EQ 3 THEN
                opcUserFieldValue3 = hdUserFieldValue[iCount]:SCREEN-VALUE.
        END.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable s-object 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Resize s-object 
PROCEDURE No-Resize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateFillinWidget s-object 
PROCEDURE pCreateFillinWidget :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserFieldLabel  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiUserFieldCount  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldValue  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiYPosition       AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
     
    IF VALID-HANDLE(hdUserFieldValue[ipiUserFieldCount]) THEN
        DELETE OBJECT hdUserFieldValue[ipiUserFieldCount].
        
    CREATE FILL-IN hdUserFieldValue[ipiUserFieldCount]
    ASSIGN 
        FRAME         = FRAME {&FRAME-NAME}:HANDLE
        Y             = ipiYPosition
        X             = 170
        SENSITIVE     = TRUE
        HEIGHT-PIXELS = 27
        WIDTH-PIXELS  = 160
        DATA-TYPE     = ipcUserFieldType
        FORMAT        = ipcUserFieldFormat
        VISIBLE       = TRUE
        SCREEN-VALUE  = ipcUserFieldValue
        PRIVATE-DATA  = ipcUserFieldLabel
        TRIGGERS:
            ON ENTRY PERSISTENT RUN pFieldEntry IN THIS-PROCEDURE (ipiUserFieldCount).
            ON LEAVE PERSISTENT RUN pNotify IN THIS-PROCEDURE (ipiUserFieldCount, ipcUserFieldLabel).
        END TRIGGERS.
    
    cUserFieldValue[ipiUserFieldCount] = "".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFieldEntry s-object 
PROCEDURE pFieldEntry :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiUserFieldID AS INTEGER NO-UNDO.

    IF VALID-HANDLE(hdUserFieldValue[ipiUserFieldID]) THEN
        cUserFieldValue[ipiUserFieldID] = hdUserFieldValue[ipiUserFieldID]:SCREEN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE oSSLoadTagJobDesignConfig AS system.Config NO-UNDO.

    DEFINE VARIABLE iUserFieldsCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cUserFieldList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldInfo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdWidget         AS HANDLE    NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DO iUserFieldsCount = 1 TO NUM-ENTRIES(cUserFieldListFormat):
        cUserFieldInfo = ENTRY (iUserFieldsCount, cUserFieldListFormat).
        
        CREATE ttUserField.
        ASSIGN
            ttUserField.fieldLabel  = ENTRY(1, cUserFieldInfo, "|")
            ttUserField.fieldType   = ENTRY(2, cUserFieldInfo, "|")
            ttUserField.fieldFormat = ENTRY(3, cUserFieldInfo, "|")
            NO-ERROR.
            
        cUserFieldList = cUserFieldList + "," + ttUserField.fieldLabel.
    END.
    
    cUserFieldList = TRIM(cUserFieldList,",").
    
    ASSIGN
        cbUserField1:LIST-ITEMS = cUserFieldList
        cbUserField2:LIST-ITEMS = cUserFieldList
        cbUserField3:LIST-ITEMS = cUserFieldList
        .
        

    {methods/run_link.i "CONTAINER-SOURCE" "GetDesignConfig" "(OUTPUT oSSLoadTagJobDesignConfig)"}
    
    fiUserFieldsRectangleLabel:SCREEN-VALUE = "User Fields".
    
    IF VALID-OBJECT(oSSLoadTagJobDesignConfig) THEN DO:
        hdWidget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
        DO WHILE VALID-HANDLE(hdWidget):
            IF hdWidget:PRIVATE-DATA NE "" AND hdWidget:PRIVATE-DATA NE ? THEN DO:

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("UserFields", hdWidget:PRIVATE-DATA, "visible") THEN
                    hdWidget:HIDDEN = NOT LOGICAL(oSSLoadTagJobDesignConfig:GetAttributeValue("UserFields", hdWidget:PRIVATE-DATA, "visible")).

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("UserFields", hdWidget:PRIVATE-DATA, "value") THEN
                    hdWidget:SCREEN-VALUE = oSSLoadTagJobDesignConfig:GetAttributeValue("UserFields", hdWidget:PRIVATE-DATA, "value").

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("UserFields", hdWidget:PRIVATE-DATA, "sensitive") THEN
                    hdWidget:SENSITIVE = LOGICAL(oSSLoadTagJobDesignConfig:GetAttributeValue("UserFields", hdWidget:PRIVATE-DATA, "sensitive")).
            END.

            hdWidget = hdWidget:NEXT-SIBLING.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNotify s-object 
PROCEDURE pNotify PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiUserFieldID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField   AS CHARACTER NO-UNDO.

    IF VALID-HANDLE(hdUserFieldValue[ipiUserFieldID]) AND 
       cUserFieldValue[ipiUserFieldID] EQ hdUserFieldValue[ipiUserFieldID]:SCREEN-VALUE THEN
        RETURN.
            
    CASE ipcUserField:
        WHEN "Overs" THEN
            RUN new-state (
               INPUT "overs-changed"
                ).
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateUserField s-object 
PROCEDURE pUpdateUserField PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserField      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldValue AS CHARACTER NO-UNDO.  
    DEFINE INPUT PARAMETER ipiUserFieldCount AS INTEGER   NO-UNDO. 

    DEFINE VARIABLE iYPosition AS INTEGER   NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST ttUserField
         WHERE ttUserField.fieldLabel EQ ipcUserField
         NO-ERROR.
    IF ipiUserFieldCount EQ 1 THEN DO:
        IF AVAILABLE ttUserField THEN
            ASSIGN
/*                cbUserField1:HIDDEN       = lDisableWidgets*/
                cbUserField1:SCREEN-VALUE = ipcUserField
/*                cbUserField1:SENSITIVE    = NOT lDisableWidgets*/
                iYPosition                = cbUserField1:Y
                .
        
        ELSE
            ASSIGN
                cbUserField1:SENSITIVE    = FALSE
                cbUserField1:HIDDEN       = TRUE
                .            
    END.
    ELSE IF ipiUserFieldCount EQ 2 THEN DO:
        IF AVAILABLE ttUserField THEN
            ASSIGN
/*                cbUserField2:HIDDEN       = lDisableWidgets*/
                cbUserField2:SCREEN-VALUE = ipcUserField
/*                cbUserField2:SENSITIVE    = NOT lDisableWidgets*/
                iYPosition                = cbUserField2:Y
                .
        ELSE
            ASSIGN
                cbUserField2:SENSITIVE    = FALSE
                cbUserField2:HIDDEN       = TRUE
                .
    END.            
    ELSE IF ipiUserFieldCount EQ 3 THEN DO:
        IF AVAILABLE ttUserField THEN
            ASSIGN
/*                cbUserField3:HIDDEN       = lDisableWidgets*/
                cbUserField3:SCREEN-VALUE = ipcUserField
/*                cbUserField3:SENSITIVE    = NOT lDisableWidgets*/
                iYPosition                = cbUserField3:Y
                .
        ELSE
            ASSIGN
                cbUserField3:SENSITIVE    = FALSE
                cbUserField3:HIDDEN       = TRUE
                .  
    END.
    
    IF AVAILABLE ttUserField THEN DO:
        RUN pCreateFillinWidget(
            INPUT ttUserField.fieldLabel,
            INPUT ipiUserFieldCount,
            INPUT ttUserField.fieldType,
            INPUT ttUserField.fieldFormat,
            INPUT ipcUserFieldValue,
            INPUT iYPosition
            ).
    END.
    ELSE DO:
        IF VALID-HANDLE(hdUserFieldValue[ipiUserFieldCount]) THEN
            DELETE OBJECT hdUserFieldValue[ipiUserFieldCount].
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetUserFields s-object 
PROCEDURE SetUserFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserField1      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserField2      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserField3      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldValue1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldValue2 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserFieldValue3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cUserField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    DO iCount = 1 TO 3:
        IF iCount EQ 1 THEN
            ASSIGN
                cUserField = ipcUserField1
                cUserValue = ipcUserFieldValue1
                .
        ELSE IF iCount EQ 2 THEN
            ASSIGN
                cUserField = ipcUserField2
                cUserValue = ipcUserFieldValue2
                .
        ELSE IF iCount EQ 3 THEN
            ASSIGN
                cUserField = ipcUserField3
                cUserValue = ipcUserFieldValue3
                .
        
        RUN pUpdateUserField (
            INPUT cUserField,
            INPUT cUserValue,
            INPUT iCount
            ).  
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

