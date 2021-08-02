&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/setting.w

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

{custom/globdefs.i}
{custom/gcompany.i}

DEFINE VARIABLE cMode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE scInstance     AS CLASS     system.Setting NO-UNDO.
DEFINE VARIABLE lReturnError   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iSettingTypeID AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES setting
&Scoped-define FIRST-EXTERNAL-TABLE setting


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR setting.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAdd-2 RECT-1 RECT-2 btnCopy-2 btnDelete-2 ~
cSettingName cDescription btnUpdate-2 cSettingValue lInactive cScopeTable ~
cScopeField1 cScopeField2 
&Scoped-Define DISPLAYED-OBJECTS cSettingName cDescription cSettingValue ~
lbl_sort-2 lInactive lbl_sort cScopeTable cScopeField1 cScopeField2 ~
cScopeField3 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS cScopeField3 

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

DEFINE VARIABLE cDescription  AS CHARACTER FORMAT "X(100)":U 
    LABEL "Description" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cScopeField1  AS CHARACTER FORMAT "X(30)":U 
    LABEL "Scope Field 1" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cScopeField2  AS CHARACTER FORMAT "X(30)":U 
    LABEL "Scope Field 2" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cScopeField3  AS CHARACTER FORMAT "X(30)":U 
    LABEL "Scope Field 3" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cSettingName  AS CHARACTER FORMAT "X(50)":U 
    LABEL "Name" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cSettingValue AS CHARACTER FORMAT "X(100)":U 
    LABEL "Value" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE lbl_sort      AS CHARACTER FORMAT "X(256)":U INITIAL "Scope Table:" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-2    AS CHARACTER FORMAT "X(256)":U INITIAL "Inactive:" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE cScopeTable   AS CHARACTER       
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "Company", "Company",
    "Customer", "Customer",
    "Shipto", "Shipto",
    "Location", "Location",
    "Vendor", "Vendor"
    DROP-DOWN-LIST
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 DROP-TARGET NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 85 BY 5.1.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 85 BY 5.1.

DEFINE RECTANGLE transPanel-3
    EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
    SIZE 50 BY 2.38
    BGCOLOR 15 .

DEFINE VARIABLE lInactive AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btnAdd-2 AT ROW 11.95 COL 32 HELP
    "Add" WIDGET-ID 20
    btnCancel-2 AT ROW 11.95 COL 64 HELP
    "Cancel" WIDGET-ID 28
    btnCopy-2 AT ROW 11.95 COL 40 HELP
    "Copy" WIDGET-ID 24
    btnDelete-2 AT ROW 11.95 COL 48 HELP
    "Delete" WIDGET-ID 26
    cSettingName AT ROW 1.52 COL 20.4 COLON-ALIGNED
    cDescription AT ROW 2.62 COL 20.4 COLON-ALIGNED
    btnReset-2 AT ROW 11.95 COL 56 HELP
    "Reset" WIDGET-ID 22
    btnUpdate-2 AT ROW 11.95 COL 24 HELP
    "Update/Save" WIDGET-ID 18
    cSettingValue AT ROW 3.71 COL 20.4 COLON-ALIGNED
    lbl_sort-2 AT ROW 4.76 COL 9 COLON-ALIGNED NO-LABELS WIDGET-ID 4
    lInactive AT ROW 4.86 COL 23
    lbl_sort AT ROW 6.71 COL 5 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    cScopeTable AT ROW 6.76 COL 23.2 NO-LABELS
    cScopeField1 AT ROW 8 COL 20.6 COLON-ALIGNED
    cScopeField2 AT ROW 9.14 COL 20.6 COLON-ALIGNED
    cScopeField3 AT ROW 10.29 COL 20.6 COLON-ALIGNED
    RECT-1 AT ROW 1.14 COL 2
    RECT-2 AT ROW 6.29 COL 2 WIDGET-ID 6
    transPanel-3 AT ROW 11.76 COL 23 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.setting
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 13.76
         WIDTH              = 90.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

/* SETTINGS FOR BUTTON btnCancel-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReset-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cScopeField3 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lbl_sort IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME F-Main = "rd_print".

/* SETTINGS FOR FILL-IN lbl_sort-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort-2:PRIVATE-DATA IN FRAME F-Main = "rd_print".

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

&Scoped-define SELF-NAME cSettingValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSettingValue V-table-Win
ON HELP OF cSettingValue IN FRAME F-Main
    DO:
        DEFINE VARIABLE cReturnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE riRecVal      AS RECID     NO-UNDO.
        DEFINE VARIABLE char-val      AS CHARACTER NO-UNDO.
     
        RUN system/openlookup.p (
            INPUT  "", 
            INPUT  "", /* lookup field */
            INPUT  178,   /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param value ID */
            OUTPUT cReturnFields, 
            OUTPUT char-val, 
            OUTPUT riRecVal
            ).
        cSettingValue:SCREEN-VALUE =  char-val .
   
        APPLY "Value-changed" TO cSettingValue.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSettingValue V-table-Win
ON LEAVE OF cSettingValue IN FRAME F-Main /* Value */
    DO:
        STATUS INPUT ''.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-setting-value(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
        END.   

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSettingValue V-table-Win
ON VALUE-CHANGED OF cSettingValue IN FRAME F-Main /* Value */
    DO:  
        STATUS INPUT ''.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN pSetScope.
        END.   

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

 
&Scoped-define SELF-NAME cScopeField1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cScopeField1 V-table-Win
ON LEAVE OF cScopeField1 IN FRAME F-Main
    DO:
        STATUS INPUT ''.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-company(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
//{custom/getcmpny.i}
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

    /* Create a list of all the tables that we need to get.            */
    {src/adm/template/row-list.i "setting"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "setting"}

    /* Process the newly available records (i.e. display fields,
       open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */     
    DO WITH FRAME {&FRAME-NAME}:
  
        IF AVAILABLE setting THEN
        DO:
            ASSIGN
                cSettingName:SCREEN-VALUE  = setting.settingName
                cDescription:SCREEN-VALUE  = setting.description
                cSettingValue:SCREEN-VALUE = setting.settingValue
                lInactive:SCREEN-VALUE     = STRING(setting.inactive) .
       
            FIND FIRST scope NO-LOCK
                WHERE scope.scopeID EQ setting.scopeID NO-ERROR.
       
            IF AVAILABLE scope THEN            
                ASSIGN
                    cScopeTable:SCREEN-VALUE  = scope.scopeTable
                    cScopeField1:SCREEN-VALUE = scope.scopeField1
                    cScopeField2:SCREEN-VALUE = scope.scopeField2
                    cScopeField3:SCREEN-VALUE = scope.scopeField3. 
            DISABLE  cSettingName cDescription cSettingValue lInactive
                cScopeTable cScopeField1 cScopeField2 cScopeField3 . 
        
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

    /* Define variables needed by this internal procedure.               */
    {src/adm/template/snd-head.i}

    /* For each requested table, put it's ROWID in the output list.      */
    {src/adm/template/snd-list.i "setting"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
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
    
    DEFINE BUFFER bf-setting     FOR setting.
    DEFINE BUFFER bf-settingType FOR settingType.
    
    IF AVAILABLE setting THEN
    DO:
        FIND FIRST bf-setting NO-LOCK
            WHERE bf-setting.settingID EQ setting.settingID NO-ERROR.
        
        FIND FIRST bf-settingType NO-LOCK
            WHERE bf-settingType.settingTypeID EQ bf-setting.settingTypeID
            NO-ERROR.
     
    END.    
       
    DO WITH FRAME {&FRAME-NAME}:
        CASE iphMode:LABEL:
            WHEN "Add" OR 
            WHEN "Copy" OR 
            WHEN "Update" THEN 
                DO:
                
                    ENABLE btnReset-2 btnCancel-2 .
                    ENABLE  cSettingName cDescription cSettingValue lInactive  .
                    DISABLE btnAdd-2 btnCopy-2 btnDelete-2.                  
                
                    btnUpdate-2:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
                
                    IF iphMode:LABEL EQ "Add" THEN 
                    DO:
                        ASSIGN
                            cSettingName:SCREEN-VALUE  = ""
                            cDescription:SCREEN-VALUE  = "" 
                            cSettingValue:SCREEN-VALUE = "" 
                            lInactive:SCREEN-VALUE     = "No"
                            cScopeTable:SCREEN-VALUE   = "Company"
                            cScopeField1:SCREEN-VALUE  = ""
                            cScopeField2:SCREEN-VALUE  = ""
                            cScopeField3:SCREEN-VALUE  = "".
                        DISABLE btnReset-2.
                        APPLY "ENTRY":U TO cSettingName.
                    END. /* add */
                    ELSE IF iphMode:LABEL EQ "Update" THEN 
                        DO:
                    
                            APPLY "ENTRY":U TO cSettingName .
                        END.
                    IF AVAILABLE bf-settingType AND bf-settingType.hasContext THEN
                        ENABLE cScopeTable cScopeField1 cScopeField2 cScopeField3 .
                    ASSIGN                    
                        cMode             = iphMode:LABEL
                        btnUpdate-2:LABEL = "Save"
                        .                
                END. /* add copy update */
            WHEN "Cancel" OR 
            WHEN "Save" THEN 
                DO:
                    IF iphMode:LABEL EQ "Save" THEN 
                    DO:
                        IF cMode EQ "Add" OR cMode EQ "Copy" THEN 
                        DO:
                    
                            RUN valid-setting-value(OUTPUT lReturnError) NO-ERROR.
                            IF lReturnError THEN RETURN NO-APPLY.
                         
                            RUN valid-check-entry(OUTPUT lReturnError) NO-ERROR.
                            IF lReturnError THEN RETURN NO-APPLY.                          
                         
                            scInstance = NEW system.Setting().
                         
                            scInstance:Update(INPUT iSettingTypeID,
                                INPUT 0,
                                INPUT cSettingName:SCREEN-VALUE,
                                INPUT cSettingValue:SCREEN-VALUE,
                                INPUT cDescription:SCREEN-VALUE,
                                INPUT USERID(LDBNAME(1)),
                                INPUT "setting",
                                INPUT LOGICAL(lInactive:SCREEN-VALUE),
                                INPUT cScopeTable:SCREEN-VALUE,                                           
                                INPUT cScopeField1:SCREEN-VALUE,
                                INPUT cScopeField2:SCREEN-VALUE,
                                INPUT cScopeField3:SCREEN-VALUE,
                                OUTPUT iSettingId
                                ).
                      
                            FIND FIRST setting NO-LOCK
                                WHERE setting.settingID EQ iSettingId NO-ERROR.
                           
                            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).  

                            IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
                                RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(setting)).                    
                        
                        
                        END. /* if add/copy */
                        ELSE IF cMode EQ "Update" THEN 
                            DO:
                    
                                RUN valid-setting-value(OUTPUT lReturnError) NO-ERROR.
                                IF lReturnError THEN RETURN NO-APPLY.
                          
                                scInstance = NEW system.Setting().
                         
                                scInstance:Update(INPUT int(bf-setting.settingTypeID),
                                    INPUT INTEGER(bf-setting.settingID),
                                    INPUT cSettingName:SCREEN-VALUE,
                                    INPUT cSettingValue:SCREEN-VALUE,
                                    INPUT cDescription:SCREEN-VALUE,
                                    INPUT USERID(LDBNAME(1)),
                                    INPUT "setting",
                                    INPUT LOGICAL(lInactive:SCREEN-VALUE),
                                    INPUT cScopeTable:SCREEN-VALUE,                                           
                                    INPUT cScopeField1:SCREEN-VALUE,
                                    INPUT cScopeField2:SCREEN-VALUE,
                                    INPUT cScopeField3:SCREEN-VALUE,
                                    OUTPUT iSettingId
                                    ). 
                                           
                            END.   
                   
                   
                    END. /* save */
                    IF iphMode:LABEL EQ "Cancel" THEN 
                    DO:
                        RUN pDisplaySetting.
                    END.
                    DISABLE cSettingName cDescription cSettingValue lInactive
                        cScopeTable cScopeField1 cScopeField2 cScopeField3 .
                    btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                    btnUpdate-2:LABEL = "Update".
                    ENABLE btnAdd-2 btnCopy-2 btnDelete-2.
                    DISABLE btnReset-2 btnCancel-2 .
                    
                END. /* cancel save */
            WHEN "Delete" THEN 
                DO:
                    IF AVAILABLE setting THEN 
                    DO:
                        MESSAGE
                            "Delete Currently Selected Record?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                            UPDATE lContinue.
                        IF lContinue THEN 
                        DO:
                    
                         //IF scInstance EQ ? THEN 
                            scInstance = NEW system.Setting().
                         
                            scInstance:DELETE(INPUT INTEGER(bf-setting.settingID)).
                    
                            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).  

                            IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
                                RUN repo-query IN WIDGET-HANDLE(char-hdl) (rwRowid). 
                        
                        END. /* if lcontinue */
                    END. /* if avail */
                END. /* delete */
            WHEN "Reset" THEN
            RUN pDisplaySetting.
        END CASE.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplaySetting V-table-Win 
PROCEDURE pDisplaySetting :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE setting THEN
        DO:
            ASSIGN
                cSettingName:SCREEN-VALUE  = setting.settingName
                cDescription:SCREEN-VALUE  = setting.description
                cSettingValue:SCREEN-VALUE = setting.settingValue
                lInactive:SCREEN-VALUE     = STRING(setting.inactive) .
           
            FIND FIRST scope NO-LOCK
                WHERE scope.scopeID EQ setting.scopeID NO-ERROR.
           
            IF AVAILABLE scope THEN            
                ASSIGN
                    cScopeTable:SCREEN-VALUE  = scope.scopeTable
                    cScopeField1:SCREEN-VALUE = scope.scopeField1
                    cScopeField2:SCREEN-VALUE = scope.scopeField2
                    cScopeField3:SCREEN-VALUE = scope.scopeField3. 
                   
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetScope V-table-Win 
PROCEDURE pSetScope :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DEFINE BUFFER bf-settingType FOR settingType.
 
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-settingType NO-LOCK 
            WHERE bf-settingType.settingName EQ cSettingValue:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE bf-settingType AND  bf-settingType.hasContext THEN
            ENABLE cScopeTable cScopeField1 cScopeField2 cScopeField3 .      
    END. 

    {methods/lValidateError.i NO}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company V-table-Win 
PROCEDURE valid-company :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-company FOR company.
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-company NO-LOCK 
            WHERE bf-company.company EQ cScopeField1:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE bf-company THEN
        DO:
            MESSAGE "Invalid Company, Please enter valid company" VIEW-AS ALERT-BOX ERROR.
            oplReturnError = YES .
            APPLY "entry" TO cScopeField1.
        END.        
    END. 

    {methods/lValidateError.i NO}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-setting-value V-table-Win 
PROCEDURE valid-setting-value :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-settingType FOR settingType.
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-settingType NO-LOCK 
            WHERE bf-settingType.settingName EQ cSettingValue:SCREEN-VALUE NO-ERROR.
        IF NOT AVAILABLE bf-settingType THEN
        DO:
            MESSAGE "Value is not found, Please enter valid value" VIEW-AS ALERT-BOX ERROR.
            oplReturnError = YES .
            APPLY "entry" TO cSettingValue.
        END.  
        ELSE iSettingTypeID = bf-settingType.settingTypeID.
    END. 

    {methods/lValidateError.i NO}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-check-entry V-table-Win 
PROCEDURE valid-check-entry :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-setting FOR setting.
    DEFINE BUFFER bf-scope   FOR scope.
    
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
  
        FIND FIRST bf-scope NO-LOCK
            WHERE bf-scope.scopeTable  EQ cScopeTable:SCREEN-VALUE
            AND bf-scope.scopeField1 EQ cScopeField1:SCREEN-VALUE
            AND bf-scope.scopeField2 EQ cScopeField2:SCREEN-VALUE
            AND bf-scope.scopeField3 EQ cScopeField3:SCREEN-VALUE
            NO-ERROR. 
        IF NOT AVAILABLE bf-scope THEN RETURN.       

        FIND FIRST bf-setting NO-LOCK 
            WHERE bf-setting.settingTypeID EQ iSettingTypeID 
            AND bf-setting.scopeID EQ bf-scope.scopeID
            AND bf-setting.settingUser EQ USERID(LDBNAME(1)) NO-ERROR.
           
        IF AVAILABLE bf-setting THEN
        DO:
            MESSAGE "Value and Scope Table already exist with name " + bf-setting.settingName VIEW-AS ALERT-BOX ERROR.
            oplReturnError = YES .
            APPLY "entry" TO cSettingValue.
        END.  
     
    END. 

    {methods/lValidateError.i NO}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

