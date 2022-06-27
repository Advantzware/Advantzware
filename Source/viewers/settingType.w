&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/settingType.w

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
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.ObjectModelParser.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lcDefaultData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE oJsonParser AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonObject AS JsonObject        NO-UNDO.

FIX-CODEPAGE(lcDefaultData) = "utf-8".

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
&Scoped-Define ENABLED-FIELDS settingType.settingName ~
settingType.hasContext settingType.description settingType.isPassword ~
settingType.securityLevel settingType.dataType settingType.defaultValue ~
settingType.validValueMax settingType.validValueMin 
&Scoped-define ENABLED-TABLES settingType
&Scoped-define FIRST-ENABLED-TABLE settingType
&Scoped-Define ENABLED-OBJECTS categoryTags validValues 
&Scoped-Define DISPLAYED-FIELDS settingType.settingName ~
settingType.hasContext settingType.description settingType.isPassword ~
settingType.securityLevel settingType.dataType settingType.defaultValue ~
settingType.validValueMax settingType.validValueMin 
&Scoped-define DISPLAYED-TABLES settingType
&Scoped-define FIRST-DISPLAYED-TABLE settingType
&Scoped-Define DISPLAYED-OBJECTS fiCategoryTag fiValidValue categoryTags ~
validValues 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,SELECTION-LIST-WIDGETS,List-4,List-5,List-6 */
&Scoped-define SELECTION-LIST-WIDGETS btCategoryTagsInsert ~
btCategoryTagsRemove fiCategoryTag btValidValuesInsert btValidValuesRemove ~
fiValidValue 
&Scoped-define List-4 fiValidValue 

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
DEFINE BUTTON btCategoryTagsInsert 
     LABEL "Insert" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btCategoryTagsRemove 
     LABEL "Remove" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btValidValuesInsert 
     LABEL "Insert" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btValidValuesRemove 
     LABEL "Remove" 
     SIZE 13 BY 1.14.

DEFINE VARIABLE fiCategoryTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiValidValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 181 BY 18.1.

DEFINE VARIABLE categoryTags AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 63.4 BY 6.19 NO-UNDO.

DEFINE VARIABLE validValues AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 77 BY 6.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     settingType.settingName AT ROW 1.24 COL 26 COLON-ALIGNED WIDGET-ID 4 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 77 BY 1
     settingType.hasContext AT ROW 1.24 COL 131 WIDGET-ID 10
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     settingType.description AT ROW 2.43 COL 28 NO-LABEL WIDGET-ID 16
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 77 BY 4
     settingType.isPassword AT ROW 2.43 COL 131 WIDGET-ID 12
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     settingType.securityLevel AT ROW 3.62 COL 134 COLON-ALIGNED WIDGET-ID 2 FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     settingType.dataType AT ROW 6.62 COL 26 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Character","Integer","Decimal","Logical","Date","DateTime","DateTime-TZ","Json" 
          DROP-DOWN-LIST
          SIZE 23.6 BY 1
     btCategoryTagsInsert AT ROW 7.67 COL 154.8 WIDGET-ID 36
     btCategoryTagsRemove AT ROW 7.67 COL 168.4 WIDGET-ID 34
     fiCategoryTag AT ROW 7.76 COL 115.6 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     btValidValuesInsert AT ROW 7.81 COL 78.6 WIDGET-ID 28
     btValidValuesRemove AT ROW 7.81 COL 92.2 WIDGET-ID 30
     fiValidValue AT ROW 7.91 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     categoryTags AT ROW 8.95 COL 117.6 NO-LABEL WIDGET-ID 42
     validValues AT ROW 9.1 COL 28 NO-LABEL WIDGET-ID 24
     settingType.defaultValue AT ROW 15.48 COL 26 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 76.8 BY 1
     settingType.validValueMax AT ROW 16.62 COL 26 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     settingType.validValueMin AT ROW 17.81 COL 26 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     "Tags:" VIEW-AS TEXT
          SIZE 6.2 BY .62 AT ROW 9.81 COL 110.8 WIDGET-ID 48
     "Valid Values:" VIEW-AS TEXT
          SIZE 14.6 BY .62 AT ROW 9.05 COL 13.4 WIDGET-ID 26
     "Description:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.43 COL 14 WIDGET-ID 18
     "Category" VIEW-AS TEXT
          SIZE 10.6 BY .62 AT ROW 9.05 COL 106.4 WIDGET-ID 40
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.settingType
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
         HEIGHT             = 18.1
         WIDTH              = 181.
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
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btCategoryTagsInsert IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON btCategoryTagsRemove IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON btValidValuesInsert IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON btValidValuesRemove IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fiCategoryTag IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fiValidValue IN FRAME F-Main
   NO-ENABLE 3 4                                                        */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN settingType.securityLevel IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN settingType.settingName IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME btCategoryTagsInsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCategoryTagsInsert V-table-Win
ON CHOOSE OF btCategoryTagsInsert IN FRAME F-Main /* Insert */
DO:
    DEFINE VARIABLE cCategoryTagsList AS CHARACTER NO-UNDO.
    
    IF fiCategoryTag:SCREEN-VALUE NE "" THEN DO:
        IF LOOKUP (fiCategoryTag:SCREEN-VALUE, CategoryTags:LIST-ITEMS) EQ 0 OR CategoryTags:LIST-ITEMS EQ ? THEN DO:
            cCategoryTagsList = CategoryTags:LIST-ITEMS + "," + fiCategoryTag:SCREEN-VALUE.
            
            IF cCategoryTagsList EQ ? THEN
                cCategoryTagsList = fiCategoryTag:SCREEN-VALUE.
                
            cCategoryTagsList = TRIM(cCategoryTagsList, ",").
            
            CategoryTags:LIST-ITEMS = cCategoryTagsList.
        END.
    END.
    
    fiCategoryTag:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCategoryTagsRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCategoryTagsRemove V-table-Win
ON CHOOSE OF btCategoryTagsRemove IN FRAME F-Main /* Remove */
DO:
    categoryTags:DELETE(categoryTags:SCREEN-VALUE).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btValidValuesInsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btValidValuesInsert V-table-Win
ON CHOOSE OF btValidValuesInsert IN FRAME F-Main /* Insert */
DO:
    DEFINE VARIABLE cValidValuesList AS CHARACTER NO-UNDO.
    
    IF fiValidValue:SCREEN-VALUE NE "" THEN DO:
        IF LOOKUP (fiValidValue:SCREEN-VALUE, validValues:LIST-ITEMS) EQ 0 OR validValues:LIST-ITEMS EQ ? THEN DO:
            cValidValuesList = validValues:LIST-ITEMS + "," + fiValidValue:SCREEN-VALUE.
            
            IF cValidValuesList EQ ? THEN
                cValidValuesList = fiValidValue:SCREEN-VALUE.
                
            cValidValuesList = TRIM(cValidValuesList, ",").
            
            validValues:LIST-ITEMS = cValidValuesList.
        END.
    END.
    
    fiValidValue:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btValidValuesRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btValidValuesRemove V-table-Win
ON CHOOSE OF btValidValuesRemove IN FRAME F-Main /* Remove */
DO:
    validValues:DELETE(validValues:SCREEN-VALUE).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME settingType.dataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL settingType.dataType V-table-Win
ON VALUE-CHANGED OF settingType.dataType IN FRAME F-Main /* Data Type */
DO:
    settingType.defaultValue:READ-ONLY = SELF:SCREEN-VALUE EQ "Json".   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME settingType.defaultValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL settingType.defaultValue V-table-Win
ON ENTRY OF settingType.defaultValue IN FRAME F-Main /* Default Value */
DO:
    DEFINE VARIABLE lSave   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
    
    IF settingType.dataType:SCREEN-VALUE EQ "Json" THEN DO:
        lcDefaultData = settingType.defaultValue.
        
        IF NOT VALID-OBJECT(oJsonParser) THEN
            oJsonParser = NEW ObjectModelParser().

        oJsonObject = CAST(oJsonParser:Parse(INPUT lcDefaultData), JsonObject) NO-ERROR.

        IF VALID-OBJECT(oJsonObject) THEN
            oJsonObject:Write(INPUT-OUTPUT lcDefaultData, TRUE /* Formatted */). /* Beautifies the JSON for easy viewing */        
        
        RUN api/d-dataViewer.w (
            INPUT-OUTPUT lcDefaultData,
            INPUT        "Update",
            OUTPUT       lSave   
            ).

        IF lSave THEN DO:
            IF lcDefaultData NE "" THEN DO:
                
                oJsonObject = CAST(oJsonParser:Parse(INPUT lcDefaultData), JsonObject) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "Error parsing input json" SKIP
                        ERROR-STATUS:GET-MESSAGE (1) 
                        VIEW-AS ALERT-BOX ERROR.
                    lcDefaultData = settingType.defaultValue.
                    
                    RETURN.
                END.
                
                oJsonObject:Write(INPUT-OUTPUT lcDefaultData, FALSE /* Formatted */). /* Removed formatting. Saves memory */       
            END.

            SELF:SCREEN-VALUE = STRING(lcDefaultData).
        END.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        validValues:LIST-ITEMS    = ""
        validValues:SCREEN-VALUE  = ""
        categoryTags:LIST-ITEMS   = ""
        categoryTags:SCREEN-VALUE = ""
        lcDefaultData             = ""
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE settingType THEN DO:
        ASSIGN
            settingType.validValues = IF validValues:LIST-ITEMS EQ "" OR validValues:LIST-ITEMS EQ ? THEN
                                          ""
                                      ELSE
                                          validValues:LIST-ITEMS
            settingType.categoryTags = IF categoryTags:LIST-ITEMS EQ "" OR categoryTags:LIST-ITEMS EQ ? THEN
                                          ""
                                      ELSE
                                          categoryTags:LIST-ITEMS
            .
       
        IF settingType.dataType:SCREEN-VALUE EQ "Json" THEN
            settingType.defaultValue = lcDefaultData.                        
    END.                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    {custom/askdel.i}
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        DISABLE {&SELECTION-LIST-WIDGETS}.
    END.
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
    
    ASSIGN
        validValues:LIST-ITEMS IN FRAME {&FRAME-NAME} = ""
        categoryTags:LIST-ITEMS = ""
        lcDefaultData           = ""
        .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        fiValidValue:SCREEN-VALUE  = ""
        fiCategoryTag:SCREEN-VALUE = ""
        .
    
    IF AVAILABLE settingType THEN
        ASSIGN
            validValues:LIST-ITEMS             = settingType.validValues
            categoryTags:LIST-ITEMS            = settingType.categoryTags
            lcDefaultData                      = IF settingType.dataType EQ "Json" THEN settingType.defaultValue ELSE ""
            settingType.defaultValue:READ-ONLY = settingType.dataType EQ "Json"
            .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        ENABLE {&SELECTION-LIST-WIDGETS}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    /* Code placed here will execute PRIOR to standard behavior. */
    IF validValues:LIST-ITEMS NE "" AND validValues:LIST-ITEMS NE ? THEN DO:
        IF LOOKUP (settingType.defaultValue:SCREEN-VALUE, validValues:LIST-ITEMS) EQ 0 OR LOOKUP (settingType.defaultValue:SCREEN-VALUE, validValues:LIST-ITEMS) EQ ? THEN DO:
            MESSAGE "Default value has to one the value from Valid Values list"  
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END. 
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

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

