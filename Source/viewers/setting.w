&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/setting.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
{system/ttSetting.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSettingTypeID   AS INT64     NO-UNDO.
DEFINE VARIABLE iSettingID       AS INT64     NO-UNDO.
DEFINE VARIABLE cSettingName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSettingDesc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSettingTypeDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSettingValue    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasContext      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCategoryTags    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSecurityLevel   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValidValues     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsPassword      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cDataType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidValueMin   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidValueMax   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgramID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iScopeID         AS INT64     NO-UNDO.
DEFINE VARIABLE lInactive        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSettingUser     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeTable      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeField3     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecordSource    AS CHARACTER NO-UNDO.
DEFINE VARIABLE rittSetting      AS ROWID     NO-UNDO.
DEFINE VARIABLE lRecordAvailable AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalledProgram   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lProgramEdit     AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lIsUserSuperAdmin AS LOGICAL NO-UNDO.

DEFINE VARIABLE oSetting AS system.Setting NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAdd-2 RECT-3 RECT-4 slCategoryTags ~
edSettingDesc btnCopy-2 btnDelete-2 btnUpdate-2 
&Scoped-Define DISPLAYED-OBJECTS fiRecordSource cbScopeTable fiScopeField1 ~
fiScopeField2 fiScopeField3 fiSettingValue cbSettingValue tbInactive ~
fiSettingUser tbCurrentUser fiProgramID tbCurrentProgram slCategoryTags ~
fiSettingName edSettingDesc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ENABLED-FIELDS,List-4,List-5,List-6 */
&Scoped-define ENABLED-FIELDS cbScopeTable tbInactive edSettingDesc 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCancel-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnCopy-2 
     IMAGE-UP FILE "Graphics/32x32/element_copy.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/element_copy_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete-2 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnReset-2 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUpdate-2 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE VARIABLE cbScopeTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scope Type" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE cbSettingValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE edSettingDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 9.2 BY 1.14 NO-UNDO.

DEFINE VARIABLE fiProgramID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiRecordSource AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiScopeField1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scope Field 1" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiScopeField2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scope Field 2" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiScopeField3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scope Field 3" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiSettingName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE fiSettingUser AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiSettingValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 5.19.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 5.19.

DEFINE RECTANGLE transPanel-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

DEFINE VARIABLE slCategoryTags AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 6.2 BY 1.86 NO-UNDO.

DEFINE VARIABLE tbCurrentProgram AS LOGICAL INITIAL no 
     LABEL "Apply for this program" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbCurrentUser AS LOGICAL INITIAL no 
     LABEL "Apply this for me only" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbInactive AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnAdd-2 AT ROW 13.19 COL 20.2 HELP
          "Add" WIDGET-ID 46
     fiRecordSource AT ROW 1.14 COL 1.4 NO-LABEL WIDGET-ID 60
     cbScopeTable AT ROW 2.52 COL 15.8 COLON-ALIGNED WIDGET-ID 18
     fiScopeField1 AT ROW 3.71 COL 15.8 COLON-ALIGNED WIDGET-ID 20
     fiScopeField2 AT ROW 4.95 COL 15.8 COLON-ALIGNED WIDGET-ID 22
     fiScopeField3 AT ROW 6.19 COL 15.8 COLON-ALIGNED WIDGET-ID 24
     fiSettingValue AT ROW 7.62 COL 15.8 COLON-ALIGNED WIDGET-ID 8
     cbSettingValue AT ROW 7.62 COL 16 COLON-ALIGNED WIDGET-ID 12
     tbInactive AT ROW 8.76 COL 18 WIDGET-ID 16
     fiSettingUser AT ROW 9.71 COL 15.8 COLON-ALIGNED WIDGET-ID 28
     tbCurrentUser AT ROW 9.71 COL 40.2 WIDGET-ID 68
     fiProgramID AT ROW 10.91 COL 15.8 COLON-ALIGNED WIDGET-ID 30
     tbCurrentProgram AT ROW 11 COL 40.2 WIDGET-ID 70
     slCategoryTags AT ROW 12.67 COL 4 NO-LABEL WIDGET-ID 62
     btnCancel-2 AT ROW 13.19 COL 52.2 HELP
          "Cancel" WIDGET-ID 48
     fiSettingName AT ROW 12.91 COL 65 COLON-ALIGNED WIDGET-ID 2
     edSettingDesc AT ROW 14.57 COL 2 NO-LABEL WIDGET-ID 32
     btnCopy-2 AT ROW 13.19 COL 28.2 HELP
          "Copy" WIDGET-ID 50
     btnDelete-2 AT ROW 13.19 COL 36.2 HELP
          "Delete" WIDGET-ID 52
     btnReset-2 AT ROW 13.19 COL 44.2 HELP
          "Reset" WIDGET-ID 54
     btnUpdate-2 AT ROW 13.19 COL 12.2 HELP
          "Update/Save" WIDGET-ID 56
     RECT-3 AT ROW 7.43 COL 1 WIDGET-ID 4
     RECT-4 AT ROW 2.19 COL 1 WIDGET-ID 26
     transPanel-3 AT ROW 13 COL 11.2 WIDGET-ID 58
    WITH 1 DOWN NO-BOX NO-HIDE KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 27.14
         WIDTH              = 82.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCancel-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReset-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbScopeTable IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR COMBO-BOX cbSettingValue IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       cbSettingValue:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR EDITOR edSettingDesc IN FRAME F-Main
   3                                                                    */
ASSIGN 
       edSettingDesc:HIDDEN IN FRAME F-Main           = TRUE
       edSettingDesc:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiProgramID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRecordSource IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiScopeField1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiScopeField1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiScopeField2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiScopeField2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiScopeField3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiScopeField3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiSettingName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiSettingName:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiSettingUser IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSettingValue IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiSettingValue:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       slCategoryTags:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbCurrentProgram IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tbCurrentProgram:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbCurrentUser IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       tbCurrentUser:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbInactive IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR RECTANGLE transPanel-3 IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME btnAdd-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd-2 V-table-Win
ON CHOOSE OF btnAdd-2 IN FRAME F-Main /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel-2 V-table-Win
ON CHOOSE OF btnCancel-2 IN FRAME F-Main /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy-2 V-table-Win
ON CHOOSE OF btnCopy-2 IN FRAME F-Main /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete-2 V-table-Win
ON CHOOSE OF btnDelete-2 IN FRAME F-Main /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset-2 V-table-Win
ON CHOOSE OF btnReset-2 IN FRAME F-Main /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate-2 V-table-Win
ON CHOOSE OF btnUpdate-2 IN FRAME F-Main /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbScopeTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbScopeTable V-table-Win
ON VALUE-CHANGED OF cbScopeTable IN FRAME F-Main /* Scope Type */
DO:
    ASSIGN
        fiScopeField1:HIDDEN = TRUE
        fiScopeField2:HIDDEN = TRUE
        fiScopeField3:HIDDEN = TRUE
        .
    
    IF SELF:SCREEN-VALUE EQ "Company" THEN
        ASSIGN
            fiScopeField1:LABEL  = "Company"
            fiScopeField1:HIDDEN = FALSE
            .
    ELSE IF SELF:SCREEN-VALUE EQ "Customer" THEN
        ASSIGN
            fiScopeField1:LABEL  = "Company"
            fiScopeField1:HIDDEN = FALSE
            fiScopeField2:LABEL  = "Customer"
            fiScopeField2:HIDDEN = FALSE
            .
    ELSE IF SELF:SCREEN-VALUE EQ "Vendor" THEN
        ASSIGN
            fiScopeField1:LABEL        = "Company"
            fiScopeField1:HIDDEN       = FALSE
            fiScopeField2:LABEL        = "Vendor"
            fiScopeField2:HIDDEN       = FALSE
            .                
    ELSE IF SELF:SCREEN-VALUE EQ "ShipTo" THEN
        ASSIGN
            fiScopeField1:LABEL  = "Company"
            fiScopeField1:HIDDEN = FALSE
            fiScopeField2:LABEL  = "Customer"
            fiScopeField2:HIDDEN = FALSE
            fiScopeField3:LABEL  = "ShipTo"
            fiScopeField3:HIDDEN = FALSE
            .
    ELSE IF SELF:SCREEN-VALUE EQ "Location" THEN
        ASSIGN
            fiScopeField1:LABEL  = "Company"
            fiScopeField1:HIDDEN = FALSE
            fiScopeField2:LABEL  = "Customer"
            fiScopeField2:HIDDEN = FALSE
            fiScopeField3:LABEL  = "Location"
            fiScopeField3:HIDDEN = FALSE
            .

    IF SELF:SCREEN-VALUE NE "System" AND cMode EQ "Add" AND fiScopeField1:SCREEN-VALUE EQ "" THEN
        fiScopeField1:SCREEN-VALUE = cCompany.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeField1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField1 V-table-Win
ON HELP OF fiScopeField1 IN FRAME F-Main /* Scope Field 1 */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        41, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN
        SELF:SCREEN-VALUE = lookupField.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeField2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField2 V-table-Win
ON HELP OF fiScopeField2 IN FRAME F-Main /* Scope Field 2 */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID   AS INTEGER   NO-UNDO.
    
    CASE cbScopeTable:SCREEN-VALUE:
        WHEN "Customer" OR
        WHEN "Location" OR
        WHEN "ShipTo" THEN
            iSubjectID = 23.
        WHEN "Vendor" THEN
            iSubjectID = 32.            
    END CASE.
    iSubjectID = DYNAMIC-FUNCTION("sfSubjectID",iSubjectID).
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        iSubjectID, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF returnFields NE "" THEN DO:
        CASE cbScopeTable:SCREEN-VALUE:
            WHEN "Customer" OR 
            WHEN "ShipTo"   OR 
            WHEN "Location"  THEN
                SELF:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "cust.cust-no", returnFields).
            WHEN "Vendor" THEN
                SELF:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "vend.vend-no", returnFields).
        END CASE.
    END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeField3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField3 V-table-Win
ON HELP OF fiScopeField3 IN FRAME F-Main /* Scope Field 3 */
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID   AS INTEGER   NO-UNDO.
    
    CASE cbScopeTable:SCREEN-VALUE:
        WHEN "ShipTo" THEN
            iSubjectID = 122.
        WHEN "Location" THEN
            iSubjectID = 150.
    END CASE.
    iSubjectID = DYNAMIC-FUNCTION("sfSubjectID",iSubjectID).
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        iSubjectID, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF returnFields NE "" THEN DO:
        CASE cbScopeTable:SCREEN-VALUE:
            WHEN "ShipTo" THEN
                ASSIGN
                    fiScopeField2:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "shipto.cust-no", returnFields)
                    fiScopeField3:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "shipto.ship-id", returnFields)
                    .
            WHEN "Location" THEN
                ASSIGN
                    fiScopeField3:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "loc.loc", returnFields)
                    .
        END CASE.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCurrentProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCurrentProgram V-table-Win
ON VALUE-CHANGED OF tbCurrentProgram IN FRAME F-Main /* Apply for this program */
DO:
    IF SELF:CHECKED THEN
        fiProgramID:SCREEN-VALUE = cCalledProgram.
    ELSE
        fiProgramID:SCREEN-VALUE = "".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCurrentUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCurrentUser V-table-Win
ON VALUE-CHANGED OF tbCurrentUser IN FRAME F-Main /* Apply this for me only */
DO:
    IF SELF:CHECKED THEN
        fiSettingUser:SCREEN-VALUE = cUser.
    ELSE
        fiSettingUser:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbInactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbInactive V-table-Win
ON VALUE-CHANGED OF tbInactive IN FRAME F-Main /* Inactive */
DO:
    IF SELF:CHECKED THEN
        SELF:BGCOLOR = 12.
    ELSE
        SELF:BGCOLOR = 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AllowProgramIDEditable V-table-Win
PROCEDURE AllowProgramIDEditable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF cCalledProgram NE "" THEN
        lProgramEdit = TRUE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pUpdateFields.
        
    RUN pUpdatePanel.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD V-table-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE lContinue      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hWidget        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE rRowID         AS ROWID     NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCustVend      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUpdateReports AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iSettingId     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rwRowid        AS ROWID     NO-UNDO.
    DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        CASE iphMode:LABEL:
            WHEN "Add" OR 
            WHEN "Copy" OR 
            WHEN "Update" THEN DO:
                IF iphMode:LABEL EQ "Add" THEN DO:
                    /* Add logic goes here. Call a lookup to copy setting */
                    {methods/run_link.i "RECORD-SOURCE" "AddSetting" "(OUTPUT lError)"}
                    IF lError THEN
                        RETURN NO-APPLY.
                    
                    IF lIsUserSuperAdmin THEN    
                        ENABLE tbCurrentUser.
                        
                    IF lProgramEdit AND lIsUserSuperAdmin THEN
                        ENABLE tbCurrentProgram.
                        
                    DISABLE btnReset-2.
                END. /* add */
                ELSE IF iphMode:LABEL EQ "Update" THEN DO:                
                    APPLY "ENTRY":U TO edSettingDesc.
                END.
                ELSE IF iphMode:LABEL EQ "Copy" THEN DO:
                    /* Add logic goes here. Call a lookup to copy setting */
                    {methods/run_link.i "RECORD-SOURCE" "CopySetting" "(OUTPUT lError)"}
                    IF lError THEN
                        RETURN NO-APPLY.
                    
                    IF lIsUserSuperAdmin THEN    
                        ENABLE tbCurrentUser.
                        
                    IF lProgramEdit AND lIsUserSuperAdmin THEN
                        ENABLE tbCurrentProgram.
                    
                    DISABLE btnReset-2.
                END. /* add */
                                                
                {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
                RUN new-state ("record-update-begin").
                
                cMode = iphMode:LABEL.
                
                ENABLE btnUpdate-2 btnReset-2 btnCancel-2.
                
                ENABLE tbInactive cbScopeTable fiScopeField1 fiScopeField2 fiScopeField3.
                
                APPLY "VALUE-CHANGED" TO cbScopeTable.
                
                IF cValidValues EQ "" THEN
                    ENABLE fiSettingValue.
                ELSE
                    ENABLE cbSettingValue.
                     
                edSettingDesc:READ-ONLY = FALSE.
                
                DISABLE btnAdd-2 btnCopy-2 btnDelete-2.                  
                
                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").

                btnUpdate-2:LABEL = "Save".           
            END. /* add copy update */
            WHEN "Cancel" OR 
            WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" OR cMode EQ  "Update" THEN DO:
                        system.SharedConfig:Instance:SetValue("IsSettingUpdated", "YES").
                        {methods/run_link.i "RECORD-SOURCE" "UpdateSetting" 
                            "(INPUT  edSettingDesc:SCREEN-VALUE,
                              INPUT  IF cValidValues EQ '' THEN fiSettingValue:SCREEN-VALUE ELSE cbSettingValue:SCREEN-VALUE,
                              INPUT  fiProgramID:SCREEN-VALUE,
                              INPUT  tbInactive:CHECKED,
                              INPUT  fiSettingUser:SCREEN-VALUE,
                              INPUT  cbScopeTable:SCREEN-VALUE,
                              INPUT  fiScopeField1:SCREEN-VALUE,
                              INPUT  fiScopeField2:SCREEN-VALUE,
                              INPUT  fiScopeField3:SCREEN-VALUE,
                              INPUT  rittSetting,
                              OUTPUT lError)"}
                        IF lError THEN
                            RETURN NO-APPLY.
                        ELSE DO:
                            RUN pUpdateFields.
                            
                            RUN pUpdatePanel.
                        END.
                    END. /* if Add/Copy/Update */
                END. /* save */
                IF iphMode:LABEL EQ "Cancel" THEN DO:
                    IF cRecordSource EQ "New" THEN
                        {methods/run_link.i "RECORD-SOURCE" "DeleteSetting" "(OUTPUT lError)"}
                        IF lError THEN
                            RETURN NO-APPLY.
                END.

                DISABLE fiSettingName fiSettingValue cbSettingValue tbInactive
                     cbScopeTable fiScopeField1 fiScopeField2 fiScopeField3.
                
                edSettingDesc:READ-ONLY = TRUE.

                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                btnUpdate-2:LABEL = "Update".

                ENABLE btnAdd-2 btnCopy-2 btnDelete-2.
                DISABLE btnReset-2 btnCancel-2 .
                
                {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}  
                RUN new-state ("record-update-end"). 

                RUN pUpdateFields.
                RUN pUpdatePanel.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF lRecordAvailable THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        UPDATE lContinue.
                    IF lContinue THEN DO:
                        {methods/run_link.i "RECORD-SOURCE" "DeleteSetting" "(OUTPUT lError)"}
                        IF lError THEN
                            RETURN NO-APPLY.                        
                    END. /* if lcontinue */
                END. /* if avail */
                
                RUN pUpdateFields.
                RUN pUpdatePanel.
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pUpdateFields.
            END.
        END CASE.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit V-table-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN spGetSessionParam (
        INPUT  "Company",
        OUTPUT cCompany
        ).

    RUN spGetSessionParam (
        INPUT  "UserID",
        OUTPUT cUser
        ).

    {methods/run_link.i "CONTAINER-SOURCE" "GetSetting" "(OUTPUT oSetting)"}
    
    IF NOT VALID-OBJECT (oSetting) THEN
        oSetting = NEW system.Setting().     
    
    ASSIGN
        cCalledProgram    = oSetting:CurrentProgramHotkey
        lIsUserSuperAdmin = DYNAMIC-FUNCTION("sfIsUserSuperAdmin")
        .
    
    RUN pUpdateFields.
        
    RUN pUpdatePanel.
    
    FRAME {&FRAME-NAME}:TOP-ONLY = TRUE.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFields V-table-Win 
PROCEDURE pUpdateFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdSettingBuffer   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cScopeList        AS CHARACTER NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    {methods/run_link.i "RECORD-SOURCE" "GetSetting" "(OUTPUT hdSettingBuffer)"}
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fiRecordSource:SCREEN-VALUE = ""
            fiSettingName:SCREEN-VALUE  = ""
            edSettingDesc:SCREEN-VALUE  = ""
            fiSettingValue:SCREEN-VALUE = ""
            cbSettingValue:LIST-ITEMS   = ""
            cbSettingValue:SCREEN-VALUE = ""
            fiProgramID:SCREEN-VALUE    = ""
            tbInactive:CHECKED          = FALSE
            fiSettingUser:SCREEN-VALUE  = ""
            cbScopeTable:LIST-ITEMS     = ""
            cbScopeTable:SCREEN-VALUE   = ""
            fiScopeField1:SCREEN-VALUE  = ""
            fiScopeField2:SCREEN-VALUE  = ""
            fiScopeField3:SCREEN-VALUE  = ""
            slCategoryTags:LIST-ITEMS   = ""
            tbCurrentUser:CHECKED       = FALSE
            tbCurrentUser:HIDDEN        = TRUE
            tbCurrentProgram:CHECKED    = FALSE
            tbCurrentProgram:HIDDEN     = TRUE
            lRecordAvailable            = FALSE
            .
        
        IF VALID-HANDLE(hdSettingBuffer) AND hdSettingBuffer:AVAILABLE THEN DO:
            ASSIGN
                lRecordAvailable = TRUE
                iSettingTypeID   = hdSettingBuffer:BUFFER-FIELD("settingTypeID"):BUFFER-VALUE
                iSettingID       = hdSettingBuffer:BUFFER-FIELD("settingID"):BUFFER-VALUE
                cSettingName     = hdSettingBuffer:BUFFER-FIELD("settingName"):BUFFER-VALUE
                cSettingDesc     = hdSettingBuffer:BUFFER-FIELD("description"):BUFFER-VALUE
                cSettingTypeDesc = hdSettingBuffer:BUFFER-FIELD("settingTypeDesc"):BUFFER-VALUE
                cSettingValue    = hdSettingBuffer:BUFFER-FIELD("settingValue"):BUFFER-VALUE
                lHasContext      = hdSettingBuffer:BUFFER-FIELD("hasContext"):BUFFER-VALUE
                cCategoryTags    = hdSettingBuffer:BUFFER-FIELD("categoryTags"):BUFFER-VALUE
                iSecurityLevel   = hdSettingBuffer:BUFFER-FIELD("securityLevel"):BUFFER-VALUE
                cValidValues     = hdSettingBuffer:BUFFER-FIELD("validValues"):BUFFER-VALUE
                lIsPassword      = hdSettingBuffer:BUFFER-FIELD("isPassword"):BUFFER-VALUE
                cDataType        = hdSettingBuffer:BUFFER-FIELD("dataType"):BUFFER-VALUE
                cValidValueMin   = hdSettingBuffer:BUFFER-FIELD("validValueMin"):BUFFER-VALUE
                cValidValueMax   = hdSettingBuffer:BUFFER-FIELD("validValueMax"):BUFFER-VALUE
                cProgramID       = hdSettingBuffer:BUFFER-FIELD("programID"):BUFFER-VALUE
                iScopeID         = hdSettingBuffer:BUFFER-FIELD("scopeID"):BUFFER-VALUE
                lInactive        = hdSettingBuffer:BUFFER-FIELD("inactive"):BUFFER-VALUE
                cSettingUser     = hdSettingBuffer:BUFFER-FIELD("settingUser"):BUFFER-VALUE
                cScopeTable      = hdSettingBuffer:BUFFER-FIELD("scopeTable"):BUFFER-VALUE
                cScopeField1     = hdSettingBuffer:BUFFER-FIELD("scopeField1"):BUFFER-VALUE
                cScopeField2     = hdSettingBuffer:BUFFER-FIELD("scopeField2"):BUFFER-VALUE
                cScopeField3     = hdSettingBuffer:BUFFER-FIELD("scopeField3"):BUFFER-VALUE
                cRecordSource    = hdSettingBuffer:BUFFER-FIELD("recordSource"):BUFFER-VALUE
                rittSetting      = hdSettingBuffer:ROWID
                .        

            ASSIGN
                cScopeList                  = oSetting:GetScopeList(lHasContext)
                cScopeList                  = TRIM(cScopeList, ",")
                cbScopeTable:LIST-ITEMS     = cScopeList        
                fiSettingName:SCREEN-VALUE  = cSettingName
                edSettingDesc:SCREEN-VALUE  = cSettingDesc
                fiProgramID:SCREEN-VALUE    = cProgramID
                tbInactive:CHECKED          = lInactive
                fiSettingUser:SCREEN-VALUE  = cSettingUser
                cbScopeTable:SCREEN-VALUE   = cScopeTable
                fiScopeField1:SCREEN-VALUE  = cScopeField1
                fiScopeField2:SCREEN-VALUE  = cScopeField2
                fiScopeField3:SCREEN-VALUE  = cScopeField3  
                slCategoryTags:LIST-ITEMS   = cCategoryTags      
                .    

            IF cRecordSource EQ "SettingType" THEN
                ASSIGN
                    fiRecordSource:SCREEN-VALUE = "Default Setting"
                    fiRecordSource:FGCOLOR      = 12
                    .
            ELSE
                ASSIGN
                    fiRecordSource:SCREEN-VALUE = "Custom Setting"
                    fiRecordSource:FGCOLOR      = ?
                    .

            IF cValidValues EQ "" THEN
                ASSIGN
                    fiSettingValue:SCREEN-VALUE = cSettingValue                
                    fiSettingValue:HIDDEN       = FALSE
                    cbSettingValue:HIDDEN       = TRUE
                    .
            ELSE
                ASSIGN
                    cbSettingValue:LIST-ITEMS   = cValidValues
                    cbSettingValue:SCREEN-VALUE = cSettingValue
                    cbSettingValue:HIDDEN       = FALSE
                    fiSettingValue:HIDDEN       = TRUE
                    .

            APPLY "VALUE-CHANGED" TO cbScopeTable.
            APPLY "VALUE-CHANGED" TO tbInactive.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdatePanel V-table-Win 
PROCEDURE pUpdatePanel PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdSettingBuffer   AS HANDLE    NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF lRecordAvailable AND cRecordSource EQ "New" AND (cMode EQ "Add" OR cMode EQ "Copy") THEN DO:            
            DISABLE btnAdd-2 btnReset-2 btnCopy-2 btnDelete-2.
            ENABLE btnUpdate-2 btnCancel-2.
            RETURN.
        END.
        
        DISABLE btnAdd-2 btnUpdate-2 btnCancel-2 btnDelete-2 btnReset-2 btnCopy-2.
    
        IF lRecordAvailable THEN DO:
            ENABLE btnAdd-2 btnCopy-2.
            
            /* Default setting. Cannot be edited */
            IF cRecordSource NE "SettingType" THEN
                ENABLE btnUpdate-2 btnDelete-2.
        END.
        ELSE DO:
            {methods/run_link.i "Setting-SOURCE" "GetSetting" "(OUTPUT hdSettingBuffer)"}
        
            IF VALID-HANDLE(hdSettingBuffer) AND hdSettingBuffer:AVAILABLE THEN
                ENABLE btnAdd-2.
        
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

