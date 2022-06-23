&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/destcostcat.w
  
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
DEFINE INPUT  PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}

{est/ttEstSysConfig.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fil-estCostCategoryLabel ~
fil-estCostCategoryDesc lst-Group tg-includeInBoardCost ~
tg-IncludeInFactoryCost tg-IncludeInMaterialCost tg-IncludeInLaborCost ~
tg-IncludeInNonFactoryCost tg-IncludeInNetProfit ~
tg-IncludeInVariableOverheadCost tg-IncludeInFixedOverheadCost btnCancel 
&Scoped-Define DISPLAYED-OBJECTS fil-EstCostCategoryID ~
fil-estCostCategoryLabel fil-estCostCategoryDesc lst-Group fil-grouplevel ~
tg-includeInBoardCost tg-IncludeInFactoryCost tg-IncludeInMaterialCost ~
tg-IncludeInLaborCost tg-IncludeInNonFactoryCost tg-IncludeInNetProfit ~
tg-IncludeInVariableOverheadCost tg-IncludeInFixedOverheadCost 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fil-estCostCategoryLabel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fil-estCostCategoryDesc AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Cost Category Desc" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.1 NO-UNDO.

DEFINE VARIABLE fil-EstCostCategoryID AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Est Cost CategoryID" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.1 NO-UNDO.

DEFINE VARIABLE fil-estCostCategoryLabel LIKE estCostCategory.costCategoryLabel
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fil-grouplevel AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Group Level" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.1 NO-UNDO.

DEFINE VARIABLE lst-Group AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 45 BY 3 NO-UNDO.

DEFINE VARIABLE tg-includeInBoardCost LIKE estCostCategory.includeInBoardCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInFactoryCost LIKE estCostCategory.includeInFactoryCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInFixedOverheadCost LIKE estCostCategory.IncludeInFixedOverheadCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInLaborCost LIKE estCostCategory.includeInLaborCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInMaterialCost LIKE estCostCategory.includeInMaterialCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInNetProfit LIKE estCostCategory.includeInNetProfit
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInNonFactoryCost LIKE estCostCategory.includeInNonFactoryCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-IncludeInVariableOverheadCost LIKE estCostCategory.includeInVariableOverheadCost
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fil-EstCostCategoryID AT ROW 2 COL 27 COLON-ALIGNED HELP
          "Enter the alternate design number for this style." WIDGET-ID 2
     fil-estCostCategoryLabel AT ROW 3.52 COL 27 COLON-ALIGNED HELP
          "Enter the Cost Category Label to appear on reports and forms" WIDGET-ID 4 FORMAT "x(35)"
     fil-estCostCategoryDesc AT ROW 5 COL 27 COLON-ALIGNED HELP
          "Description for Cost Group" WIDGET-ID 14
     lst-Group AT ROW 6.52 COL 29.6 NO-LABEL WIDGET-ID 12
     fil-grouplevel AT ROW 10 COL 27 COLON-ALIGNED HELP
          "Enter the alternate design number for this style." WIDGET-ID 16
     tg-includeInBoardCost AT ROW 2 COL 82 HELP
          "Does cost get included in Board Cost Total?" WIDGET-ID 20
     tg-IncludeInFactoryCost AT ROW 3.19 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 22
     tg-IncludeInMaterialCost AT ROW 4.38 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 24
     tg-IncludeInLaborCost AT ROW 5.62 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 26
     tg-IncludeInNonFactoryCost AT ROW 6.81 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 28
     tg-IncludeInNetProfit AT ROW 8 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 30
     tg-IncludeInVariableOverheadCost AT ROW 9.19 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 32
     tg-IncludeInFixedOverheadCost AT ROW 10.38 COL 82 HELP
          "Does cost get included in Factory Cost Total?" WIDGET-ID 34
     btnSave AT ROW 12.95 COL 40.8 WIDGET-ID 40
     btnCancel AT ROW 12.95 COL 58.6 WIDGET-ID 42
     "Group:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.29 COL 21 WIDGET-ID 36
     SPACE(79.79) SKIP(6.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Est Cost Category".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fil-EstCostCategoryID IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fil-estCostCategoryLabel IN FRAME D-Dialog
   1 LIKE = ASI.estCostCategory.costCategoryLabel EXP-FORMAT            */
/* SETTINGS FOR FILL-IN fil-grouplevel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-includeInBoardCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInBoardCost EXP-SIZE               */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInFactoryCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInFixedOverheadCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInLaborCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInMaterialCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInNetProfit IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInNonFactoryCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* SETTINGS FOR TOGGLE-BOX tg-IncludeInVariableOverheadCost IN FRAME D-Dialog
   LIKE = ASI.estCostCategory.includeInFactoryCost EXP-SIZE             */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Est Cost Category */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
       
        APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel D-Dialog
ON CHOOSE OF btnCancel IN FRAME D-Dialog /* Cancel */
DO:
   apply "window-close" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave D-Dialog
ON CHOOSE OF btnSave IN FRAME D-Dialog /* Save */
DO:
    DO WITH FRAME {&frame-name}:
    END.
  
    RUN pSaveRecord.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fil-estCostCategoryDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fil-estCostCategoryDesc D-Dialog
ON VALUE-CHANGED OF fil-estCostCategoryDesc IN FRAME D-Dialog /* Cost Category Desc */
DO:
    IF fil-estCostCategoryDesc:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE fil-estCostCategoryDesc:LABEL "cannot be blank"
            VIEW-AS ALERT-BOX. 
        RETURN NO-APPLY. 
    END. 
    IF btnSave:SENSITIVE = NO THEN
        RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fil-estCostCategoryLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fil-estCostCategoryLabel D-Dialog
ON VALUE-CHANGED OF fil-estCostCategoryLabel IN FRAME D-Dialog /* Cost Category Label */
DO:
    IF fil-estCostCategoryLabel:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE fil-estCostCategoryLabel:LABEL "cannot be blank"
            VIEW-AS ALERT-BOX. 
        RETURN NO-APPLY. 
    END.
    IF btnSave:SENSITIVE = NO THEN
        RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lst-Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lst-Group D-Dialog
ON VALUE-CHANGED OF lst-Group IN FRAME D-Dialog
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-includeInBoardCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-includeInBoardCost D-Dialog
ON VALUE-CHANGED OF tg-includeInBoardCost IN FRAME D-Dialog /* In Board Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInFactoryCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInFactoryCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInFactoryCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInFixedOverheadCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInFixedOverheadCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInFixedOverheadCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInLaborCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInLaborCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInLaborCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInMaterialCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInMaterialCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInMaterialCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInNetProfit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInNetProfit D-Dialog
ON VALUE-CHANGED OF tg-IncludeInNetProfit IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInNonFactoryCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInNonFactoryCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInNonFactoryCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-IncludeInVariableOverheadCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-IncludeInVariableOverheadCost D-Dialog
ON VALUE-CHANGED OF tg-IncludeInVariableOverheadCost IN FRAME D-Dialog /* In Factory Cost */
DO:
  RUN pTrackChngs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /*{src/adm/template/dialogmn.i}*/
    
    RUN enable_UI.
  
    {methods/nowait.i}     
    DO WITH FRAME {&frame-name}: 
        RUN pInit.
        RUN pDisplayRecord (INPUT ipcEstCostCategoryID).
            
    END.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fil-EstCostCategoryID fil-estCostCategoryLabel fil-estCostCategoryDesc 
          lst-Group fil-grouplevel tg-includeInBoardCost tg-IncludeInFactoryCost 
          tg-IncludeInMaterialCost tg-IncludeInLaborCost 
          tg-IncludeInNonFactoryCost tg-IncludeInNetProfit 
          tg-IncludeInVariableOverheadCost tg-IncludeInFixedOverheadCost 
      WITH FRAME D-Dialog.
  ENABLE fil-estCostCategoryLabel fil-estCostCategoryDesc lst-Group 
         tg-includeInBoardCost tg-IncludeInFactoryCost tg-IncludeInMaterialCost 
         tg-IncludeInLaborCost tg-IncludeInNonFactoryCost tg-IncludeInNetProfit 
         tg-IncludeInVariableOverheadCost tg-IncludeInFixedOverheadCost 
         btnCancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableField D-Dialog 
PROCEDURE pDisableField :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/       

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE {&ENABLED-FIELDS } .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayRecord D-Dialog 
PROCEDURE pDisplayRecord PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstCatId AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST ttEstCostCategory NO-lOCK
        WHERE ttEstCostCategory.estCostCategoryID = ipcEstCatId NO-ERROR.
        
    IF NOT AVAILABLE ttEstCostCategory THEN
    DO:
        MESSAGE "Invalid EstCostCategoryID: " ipcEstCatId
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    ASSIGN
        fil-EstCostCategoryID:SCREEN-VALUE       = ttEstCostCategory.estCostCategoryID
        fil-estCostCategoryDesc:SCREEN-VALUE     = ttEstCostCategory.estCostCategoryDesc
        fil-estCostCategoryLabel:SCREEN-VALUE    = ttEstCostCategory.costCategoryLabel
        tg-includeInBoardCost:Checked            = ttEstCostCategory.includeInBoardCost
        tg-includeInFactoryCost:Checked          = ttEstCostCategory.includeInFactoryCost
        tg-includeInMaterialCost:Checked         = ttEstCostCategory.includeInMaterialCost
        tg-includeInNonFactoryCost:Checked       = ttEstCostCategory.includeInNonFactoryCost
        tg-includeInNetProfit:Checked            = ttEstCostCategory.includeInNetProfit
        tg-includeInVariableOverheadCost:Checked = ttEstCostCategory.includeInVariableOverheadCost
        tg-includeInFixedOverheadCost:Checked    = ttEstCostCategory.includeInFixedOverheadCost
        lst-group:SCREEN-VALUE                   = ttEstCostCategory.estCostGroupID
        .
       FOR FIRST ttEstCostGroup NO-lOCK
           WHERE ttEstCostGroup.estCostGroupID = ttEstCostCategory.estCostGroupID,
           FIRST ttEstCostGroupLevel 
           WHERE ttEstCostGroupLevel.estCostGroupLevelID = ttEstCostGroup.estCostGroupLevelID:
               
        fil-grouplevel:SCREEN-VALUE = ttEstCostGroupLevel.estCostGroupLevelDesc + "( Level Id - " + STRING(ttEstCostGroupLevel.estCostGroupLevelID) + ")".
      END.
      
      btnSave:SENSITIVE = NO. 

END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveRecord D-Dialog
PROCEDURE pSaveRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-EstCostCategory       FOR EstCostCategory.
    DEFINE BUFFER bf-ExclEstCostCategory   FOR EstCostCategory.
    DEFINE BUFFER bf-EstCostCategorySystem FOR EstCostCategorySystem.
       
    DO WITH FRAME {&frame-name}:
    END.
    
    FIND FIRST ttEstCostCategory NO-lOCK
        WHERE ttEstCostCategory.estCostCategoryID = fil-EstCostCategoryID:SCREEN-VALUE NO-ERROR.
        
    IF NOT AVAILABLE ttEstCostCategory THEN
    DO:
        MESSAGE "Invalid EstCostCategoryID: " fil-EstCostCategoryID:SCREEN-VALUE
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    DO TRANSACTION:
        
        FIND FIRST bf-EstCostCategory EXCLUSIVE-LOCK
            WHERE bf-EstCostCategory.estCostCategoryID = fil-EstCostCategoryID:SCREEN-VALUE NO-ERROR.
            
        IF NOT AVAILABLE bf-EstCostCategory THEN
        DO:
            CREATE bf-EstCostCategory.
            ASSIGN
                bf-EstCostCategory.estCostCategoryID = fil-EstCostCategoryID:SCREEN-VALUE. 
        END.
           
        ASSIGN
            bf-EstCostCategory.estCostCategoryDesc           = fil-estCostCategoryDesc:SCREEN-VALUE
            bf-EstCostCategory.costCategoryLabel             = fil-estCostCategoryLabel:SCREEN-VALUE
            bf-EstCostCategory.includeInBoardCost            = tg-includeInBoardCost:Checked 
            bf-EstCostCategory.includeInFactoryCost          = tg-includeInFactoryCost:Checked 
            bf-EstCostCategory.includeInMaterialCost         = tg-includeInMaterialCost:Checked
            bf-EstCostCategory.includeInNonFactoryCost       = tg-includeInNonFactoryCost:Checked 
            bf-EstCostCategory.includeInNetProfit            = tg-includeInNetProfit:Checked
            bf-EstCostCategory.includeInVariableOverheadCost = tg-includeInVariableOverheadCost:Checked 
            bf-EstCostCategory.includeInFixedOverheadCost    = tg-includeInFixedOverheadCost:Checked  
            bf-EstCostCategory.estCostGroupID                = lst-group:SCREEN-VALUE     
            .
    END. /* DO TRANSACTION: */
        
    RELEASE bf-EstCostCategory.    
    
    APPLY "window-close" to frame {&frame-name}.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit D-Dialog 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListGroups    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cListGroupLvls AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    EMPTY TEMP-TABLE ttEstCostCategory.
    EMPTY TEMP-TABLE ttEstCostGroup.
    EMPTY TEMP-TABLE ttEstCostGroupLevel.
      
    RUN Estimate_GetSystemDataForEstimate(INPUT "",
        OUTPUT TABLE ttEstCostCategory,
        OUTPUT TABLE ttEstCostGroup,
        OUTPUT TABLE ttEstCostGroupLevel).
                   
    FOR EACH ttEstCostGroup:
        cListGroups = cListGroups + (IF cListGroups = "" THEN ""  ELSE ",")
            + ttEstCostGroup.estCostGroupID + " - "  + ttEstCostGroup.costGroupLabel + "," + ttEstCostGroup.estCostGroupID.
                   
    END.
    
    lst-Group:LIST-ITEM-PAIRS  = cListGroups.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTrackChngs D-Dialog 
PROCEDURE pTrackChngs :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
   
    IF ttEstCostCategory.estCostCategoryDesc NE fil-estCostCategoryDesc:SCREEN-VALUE
        OR ttEstCostCategory.costCategoryLabel NE fil-estCostCategoryLabel:SCREEN-VALUE
        OR ttEstCostCategory.includeInBoardCost NE tg-includeInBoardCost:Checked 
        OR ttEstCostCategory.includeInFactoryCost NE tg-includeInFactoryCost:Checked 
        OR ttEstCostCategory.includeInMaterialCost NE tg-includeInMaterialCost:Checked
        OR ttEstCostCategory.includeInNonFactoryCost NE tg-includeInNonFactoryCost:Checked 
        OR ttEstCostCategory.includeInNetProfit NE tg-includeInNetProfit:Checked
        OR ttEstCostCategory.includeInVariableOverheadCost NE tg-includeInVariableOverheadCost:Checked 
        OR ttEstCostCategory.includeInFixedOverheadCost NE tg-includeInFixedOverheadCost:Checked  
        OR ttEstCostCategory.estCostGroupID NE lst-group:SCREEN-VALUE THEN
    DO:
        btnSave:SENSITIVE = YES.  
            
    END.
    ELSE
        btnSave:SENSITIVE = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

