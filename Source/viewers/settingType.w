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

DEFINE VARIABLE cMode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE scInstance   AS CLASS     system.SettingType NO-UNDO.
DEFINE VARIABLE lReturnError AS LOGICAL   NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES settingType
&Scoped-define FIRST-EXTERNAL-TABLE settingType


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR settingType.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAdd-2 RECT-1 lIsPassword cSettingName ~
lHasContext cDescription cDataType cSecurityLevel btnCopy-2 cDefaultValue ~
cValidValueMin btnDelete-2 btnUpdate-2 cValidValue cCategoryTags cValidValueMax
&Scoped-Define DISPLAYED-OBJECTS lIsPassword cSettingName lbl_sort-2 ~
lHasContext cDescription lbl_sort-3 lbl_sort cDataType cSecurityLevel ~
lbl_sort-4 cDefaultValue cValidValueMin cValidValueMax cValidValue ~
cCategoryTags 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */


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

DEFINE VARIABLE cDataType      AS CHARACTER FORMAT "x(20)" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "Character","Character",
    "Integer","Integer",
    "Logical","Logical",
    "Decimal","Decimal"
    DROP-DOWN-LIST
    SIZE 27.8 BY 1
    BGCOLOR 15 FONT 1 DROP-TARGET NO-UNDO.

DEFINE VARIABLE cCategoryTags  AS CHARACTER FORMAT "X(200)":U 
    VIEW-AS EDITOR SCROLLBAR-VERTICAL 
    SIZE 41.4 BY 6.14
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cValidValue    AS CHARACTER FORMAT "X(1000)":U 
    LABEL "Valid Value" 
    VIEW-AS EDITOR SCROLLBAR-VERTICAL
    SIZE 53.6 BY 2.81
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cDefaultValue  AS CHARACTER FORMAT "X(100)":U 
    LABEL "Default Value" 
    VIEW-AS FILL-IN 
    SIZE 53.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cDescription   AS CHARACTER FORMAT "X(100)":U 
    LABEL "Description" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cSecurityLevel AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Security Level" 
    VIEW-AS FILL-IN 
    SIZE 20.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cSettingName   AS CHARACTER FORMAT "X(30)":U 
    LABEL "Name" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cValidValueMax AS CHARACTER FORMAT "X(30)":U 
    LABEL "Maximum Valid Value" 
    VIEW-AS FILL-IN 
    SIZE 53.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cValidValueMin AS CHARACTER FORMAT "X(30)":U 
    LABEL "Minimun Valid Value" 
    VIEW-AS FILL-IN 
    SIZE 53.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Data Type:" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-2     AS CHARACTER FORMAT "X(256)":U INITIAL "Is Password?" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-3     AS CHARACTER FORMAT "X(256)":U INITIAL "Has Context?" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-4     AS CHARACTER FORMAT "X(256)":U INITIAL "Category Tags" 
    VIEW-AS FILL-IN 
    SIZE 18.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 131 BY 12.71.

DEFINE RECTANGLE transPanel-3
    EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
    SIZE 50 BY 2.38
    BGCOLOR 15 .

DEFINE VARIABLE lHasContext AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE lIsPassword AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btnAdd-2 AT ROW 14.52 COL 48.4 HELP
    "Add" WIDGET-ID 20       
    cSettingName AT ROW 1.52 COL 27.6 COLON-ALIGNED
    cDescription AT ROW 2.67 COL 27.6 COLON-ALIGNED
    cDataType AT ROW 3.86 COL 27.6 COLON-ALIGNED NO-LABELS
    cValidValue AT ROW 5.1 COL 27.6 COLON-ALIGNED  HELP
    "Enter comma-separated values"
    cDefaultValue AT ROW 8.29 COL 27.6 COLON-ALIGNED
    cValidValueMin AT ROW 9.48 COL 27.6 COLON-ALIGNED
    cValidValueMax AT ROW 10.67 COL 27.6 COLON-ALIGNED
    lIsPassword AT ROW 1.48 COL 123.6
    lHasContext AT ROW 2.62 COL 123.6 WIDGET-ID 32
    cSecurityLevel AT ROW 3.86 COL 104.2 COLON-ALIGNED WIDGET-ID 34
    
    lbl_sort-2 AT ROW 1.52 COL 103.6 COLON-ALIGNED NO-LABELS WIDGET-ID 4
    
    cCategoryTags AT ROW 6.29 COL 85.6 COLON-ALIGNED NO-LABELS WIDGET-ID 36 HELP
    "Enter comma-separated values"
    lbl_sort-3 AT ROW 2.67 COL 103.6 COLON-ALIGNED NO-LABELS WIDGET-ID 30
    btnCancel-2 AT ROW 14.52 COL 80.4 HELP
    "Cancel" WIDGET-ID 28
    lbl_sort AT ROW 3.81 COL 12 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    
    
    lbl_sort-4 AT ROW 5.19 COL 85.4 COLON-ALIGNED NO-LABELS WIDGET-ID 38
    btnCopy-2 AT ROW 14.52 COL 56.4 HELP
    "Copy" WIDGET-ID 24
    
    btnDelete-2 AT ROW 14.52 COL 64.4 HELP
    "Delete" WIDGET-ID 26
    btnReset-2 AT ROW 14.52 COL 72.4 HELP
    "Reset" WIDGET-ID 22
    btnUpdate-2 AT ROW 14.52 COL 40.4 HELP
    "Update/Save" WIDGET-ID 18      
    RECT-1 AT ROW 1.14 COL 2
    transPanel-3 AT ROW 14.33 COL 39.4 WIDGET-ID 16
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
         HEIGHT             = 17.76
         WIDTH              = 133.4.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

/* SETTINGS FOR BUTTON btnCancel-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReset-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_sort IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME F-Main = "rd_print".

/* SETTINGS FOR FILL-IN lbl_sort-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort-2:PRIVATE-DATA IN FRAME F-Main = "rd_print".

/* SETTINGS FOR FILL-IN lbl_sort-3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort-3:PRIVATE-DATA IN FRAME F-Main = "rd_print".

/* SETTINGS FOR FILL-IN lbl_sort-4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort-4:PRIVATE-DATA IN FRAME F-Main = "rd_print".

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


&Scoped-define SELF-NAME cCategoryTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategoryTags V-table-Win
ON HELP OF cCategoryTags IN FRAME F-Main
    DO:
  //{sys/ref/char-fld-help.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME cSettingName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSettingName V-table-Win
ON LEAVE OF cSettingName IN FRAME F-Main
    DO:
        STATUS INPUT ''.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-SettingName(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
        END.   

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cValidValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cValidValue V-table-Win
ON HELP OF cValidValue IN FRAME F-Main /* Data Type */
    DO:
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cValidValue V-table-Win
ON LEAVE OF cValidValue IN FRAME F-Main /* Data Type */
    DO:
        STATUS INPUT ''.
        IF LASTKEY NE -1 THEN 
        DO:
  
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
    {src/adm/template/row-list.i "settingType"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "settingType"}

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
  
        IF AVAILABLE settingType THEN  
        DO:    
            ASSIGN
                lIsPassword:SCREEN-VALUE    = STRING(settingType.isPassword)
                cSettingName:SCREEN-VALUE   = settingType.settingName
                lHasContext:SCREEN-VALUE    = STRING(settingType.hasContext)
                cDescription:SCREEN-VALUE   = STRING(settingType.description)        
                cDataType:SCREEN-VALUE      = settingType.dataType
                cSecurityLevel:SCREEN-VALUE = STRING(settingType.securityLevel)
                cDefaultValue:SCREEN-VALUE  = settingType.defaultValue
                cValidValueMin:SCREEN-VALUE = STRING(settingType.validValueMin)        
                cValidValueMax:SCREEN-VALUE = settingType.validValueMax
                cValidValue:SCREEN-VALUE    = settingType.validValues   
                cCategoryTags:SCREEN-VALUE  = settingType.categoryTags  
                .           
       
            DISABLE  lIsPassword cSettingName lHasContext cDescription
                cDataType cSecurityLevel cDefaultValue cValidValueMin
                cValidValueMax cValidValue cCategoryTags.          
        END.         
    END.

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
    DEFINE VARIABLE iSettingTypeId AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rwRowid        AS ROWID     NO-UNDO.      
       
    DO WITH FRAME {&FRAME-NAME}:
        CASE iphMode:LABEL:
            WHEN "Add" OR 
            WHEN "Copy" OR 
            WHEN "Update" THEN 
                DO:
                
                    ENABLE btnReset-2 btnCancel-2 .
                    ENABLE lIsPassword cSettingName lHasContext cDescription
                        cDataType cSecurityLevel cDefaultValue cValidValueMin
                        cValidValue cCategoryTags cValidValueMax.
                    DISABLE btnAdd-2 btnCopy-2 btnDelete-2.                  
                
                    btnUpdate-2:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
                
                    IF iphMode:LABEL EQ "Add" THEN 
                    DO:
                        ASSIGN
                            cSettingName:SCREEN-VALUE   = ""
                            cDescription:SCREEN-VALUE   = "" 
                            cDataType:SCREEN-VALUE      = "" 
                            lIsPassword:SCREEN-VALUE    = "No"
                            lHasContext:SCREEN-VALUE    = "No"
                            cSecurityLevel:SCREEN-VALUE = "0"
                            cDefaultValue:SCREEN-VALUE  = ""
                            cValidValueMin:SCREEN-VALUE = ""
                            cValidValue:SCREEN-VALUE    = ""
                            cCategoryTags:SCREEN-VALUE  = ""
                            cValidValueMax:SCREEN-VALUE = "".
                        DISABLE btnReset-2.
                        APPLY "ENTRY":U TO cSettingName.
                    END. /* add */
                    ELSE IF iphMode:LABEL EQ "Update" THEN 
                        DO:
                            DISABLE cSettingName. 
                            APPLY "ENTRY":U TO cDescription .                    
                        END.
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
                    
                            RUN valid-SettingName(OUTPUT lReturnError) NO-ERROR.
                            IF lReturnError THEN RETURN NO-APPLY.
                    
                         //IF scInstance EQ ? THEN 
                            scInstance = NEW system.SettingType().                    
                         
                            scInstance:Update(INPUT 0,                                             
                                INPUT cSettingName:SCREEN-VALUE,
                                INPUT cDescription:SCREEN-VALUE,
                                INPUT cDataType:SCREEN-VALUE,
                                INPUT cValidValue:SCREEN-VALUE,
                                INPUT cDefaultValue:SCREEN-VALUE,
                                INPUT cValidValueMin:SCREEN-VALUE,
                                INPUT cValidValueMax:SCREEN-VALUE,
                                INPUT LOGICAL(lIsPassword:SCREEN-VALUE),
                                INPUT LOGICAL(lHasContext:SCREEN-VALUE),                                           
                                INPUT INTEGER(cSecurityLevel:SCREEN-VALUE),                                           
                                INPUT cCategoryTags:SCREEN-VALUE,
                                OUTPUT iSettingTypeId 
                                ). 
                                           
                            FIND FIRST settingType NO-LOCK
                                WHERE settingType.settingTypeID EQ iSettingTypeId NO-ERROR.
                           
                            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).  

                            IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
                                RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(settingType)).                         
                        
                        END. /* if add/copy */
                        ELSE IF cMode EQ "Update" THEN 
                            DO:
                    
                                scInstance = NEW system.SettingType().
                     
                                scInstance:Update(INPUT INTEGER(settingType.settingTypeID),                                             
                                    INPUT cSettingName:SCREEN-VALUE,
                                    INPUT cDescription:SCREEN-VALUE,
                                    INPUT cDataType:SCREEN-VALUE,
                                    INPUT cValidValue:SCREEN-VALUE,
                                    INPUT cDefaultValue:SCREEN-VALUE,
                                    INPUT cValidValueMin:SCREEN-VALUE,
                                    INPUT cValidValueMax:SCREEN-VALUE,
                                    INPUT LOGICAL(lIsPassword:SCREEN-VALUE),
                                    INPUT LOGICAL(lHasContext:SCREEN-VALUE),                                           
                                    INPUT INTEGER(cSecurityLevel:SCREEN-VALUE),                                           
                                    INPUT cCategoryTags:SCREEN-VALUE,
                                    OUTPUT iSettingTypeId 
                                    ).                                             
                            END.   
                   
                   
                    END. /* save */
                    IF iphMode:LABEL EQ "Cancel" THEN 
                    DO:
                        RUN pDisplaySetting.
                    END.
                    DISABLE lIsPassword cSettingName lHasContext cDescription
                        cDataType cSecurityLevel cDefaultValue cValidValueMin
                        cValidValue cCategoryTags cValidValueMax .
                    btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                    btnUpdate-2:LABEL = "Update".
                    ENABLE btnAdd-2 btnCopy-2 btnDelete-2.
                    DISABLE btnReset-2 btnCancel-2 .
                    
                END. /* cancel save */
            WHEN "Delete" THEN 
                DO:
                    IF AVAILABLE settingType THEN 
                    DO:
                        MESSAGE
                            "Delete Currently Selected Record?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                            UPDATE lContinue.
                        IF lContinue THEN 
                        DO:
                    
                            scInstance = NEW system.SettingType().
                        
                            scInstance:DELETE(INPUT INTEGER(settingType.settingTypeID)).
                        
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
        IF AVAILABLE settingType THEN
        DO:
            ASSIGN           
                cSettingName:SCREEN-VALUE   = settingType.settingName
                cDescription:SCREEN-VALUE   = settingType.description
                cDataType:SCREEN-VALUE      = settingType.dataType
                cValidValue:SCREEN-VALUE    = settingType.validValues
                cDefaultValue:SCREEN-VALUE  = settingType.defaultValue
                cValidValueMin:SCREEN-VALUE = settingType.validValueMin
                cValidValueMax:SCREEN-VALUE = settingType.validValueMax
                lIsPassword:SCREEN-VALUE    = STRING(settingType.isPassword)
                lHasContext:SCREEN-VALUE    = STRING(settingType.hasContext)
                cSecurityLevel:SCREEN-VALUE = STRING(settingType.securityLevel)
                cCategoryTags:SCREEN-VALUE  = settingType.categoryTags.
          
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
    {src/adm/template/snd-list.i "settingType"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-SettingName V-table-Win 
PROCEDURE valid-SettingName :
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
            WHERE bf-settingType.settingName EQ cSettingName:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE bf-settingType THEN
        DO:
            MESSAGE "Name is already exist, Please enter another name" VIEW-AS ALERT-BOX ERROR.
            oplReturnError = YES .
            APPLY "entry" TO cSettingName.
        END.  
    END.
    {methods/lValidateError.i NO}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

