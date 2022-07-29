&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/destcostgrplvl.w
  
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
DEFINE INPUT  PARAMETER ipcestCostGroupLevelID AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnCancel fil-estCostGroupLevelDesc 
&Scoped-Define DISPLAYED-OBJECTS fil-estCostGroupLevelID ~
fil-estCostGroupLevelDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fil-estCostGroupLevelDesc 

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

DEFINE VARIABLE fil-estCostGroupLevelDesc LIKE estCostGroupLevel.estCostGroupLevelDesc
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fil-estCostGroupLevelID LIKE estCostGroupLevel.estCostGroupLevelID
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fil-estCostGroupLevelID AT ROW 2 COL 27 COLON-ALIGNED HELP
          "Enter ID for Cost Group Level" WIDGET-ID 2
     btnSave AT ROW 7.05 COL 34.2 WIDGET-ID 40
     btnCancel AT ROW 7.05 COL 52 WIDGET-ID 42
     fil-estCostGroupLevelDesc AT ROW 3.91 COL 27 COLON-ALIGNED HELP
          "Enter Description for Cost Group Level" WIDGET-ID 44
     SPACE(2.79) SKIP(4.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Est Cost Group".


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
/* SETTINGS FOR FILL-IN fil-estCostGroupLevelDesc IN FRAME D-Dialog
   1 LIKE = ASI.estCostGroupLevel.estCostGroupLevelDesc EXP-SIZE        */
/* SETTINGS FOR FILL-IN fil-estCostGroupLevelID IN FRAME D-Dialog
   NO-ENABLE LIKE = ASI.estCostGroupLevel.estCostGroupLevelID           */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Est Cost Group */
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


&Scoped-define SELF-NAME fil-estCostGroupLevelDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fil-estCostGroupLevelDesc D-Dialog
ON VALUE-CHANGED OF fil-estCostGroupLevelDesc IN FRAME D-Dialog /* Cost Group Level Desc */
DO:
    IF fil-estCostGroupLevelDesc:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE fil-estCostGroupLevelDesc:LABEL "cannot be blank"
            VIEW-AS ALERT-BOX. 
        RETURN NO-APPLY. 
    END.
    
    IF btnSave:SENSITIVE = NO THEN
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
        RUN pDisplayRecord (INPUT ipcestCostGroupLevelID).
            
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
  DISPLAY fil-estCostGroupLevelID fil-estCostGroupLevelDesc 
      WITH FRAME D-Dialog.
  ENABLE btnCancel fil-estCostGroupLevelDesc 
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
    DEFINE INPUT PARAMETER ipcEstGrpLvlId AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST ttEstCostGroupLevel NO-lOCK
        WHERE ttEstCostGroupLevel.estCostGroupLevelID = ipcEstGrpLvlId NO-ERROR.
        
    IF NOT AVAILABLE ttEstCostGroupLevel THEN
    DO:
        MESSAGE "Invalid estCostGroupLevelID: " ipcEstGrpLvlId
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    ASSIGN
        fil-estCostGroupLevelID:SCREEN-VALUE   = STRING(ttEstCostGroupLevel.estCostGroupLevelID)
        fil-estCostGroupLevelDesc:SCREEN-VALUE = ttEstCostGroupLevel.estCostGroupLevelDesc
        .
       
      btnSave:SENSITIVE = NO. 

END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveRecord D-Dialog
PROCEDURE pSaveRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE BUFFER bf-estCostGroupLevel       FOR estCostGroupLevel.
    DEFINE BUFFER bf-ExclestCostGroupLevel   FOR estCostGroupLevel.
       
    DO WITH FRAME {&frame-name}:
    END.
    
    FIND FIRST ttEstCostGroupLevel NO-lOCK
        WHERE ttEstCostGroupLevel.estCostGroupLevelID = INTEGER(fil-estCostGroupLevelID:SCREEN-VALUE) NO-ERROR.
        
    IF NOT AVAILABLE ttEstCostGroupLevel THEN
    DO:
        MESSAGE "Invalid estCostGroupLevelID: " fil-estCostGroupLevelID:SCREEN-VALUE
        VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    DO TRANSACTION:
        
        FIND FIRST bf-estCostGroupLevel EXCLUSIVE-LOCK
            WHERE bf-estCostGroupLevel.estCostGroupLevelID = INTEGER(fil-estCostGroupLevelID:SCREEN-VALUE) NO-ERROR.
            
        IF NOT AVAILABLE bf-estCostGroupLevel THEN
        DO:
            CREATE bf-estCostGroupLevel.
            ASSIGN
                bf-estCostGroupLevel.estCostGroupLevelID = INTEGER(fil-estCostGroupLevelID:SCREEN-VALUE). 
        END.
           
        ASSIGN
            bf-estCostGroupLevel.estCostGroupLevelDesc = fil-estCostGroupLevelDesc:SCREEN-VALUE
            .
    END. /* DO TRANSACTION: */
        
    RELEASE bf-estCostGroupLevel.

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

    IF ttEstCostGroupLevel.estCostGroupLevelDesc NE fil-estCostGroupLevelDesc:SCREEN-VALUE THEN
    DO:
        btnSave:SENSITIVE = YES.  
            
    END.
    ELSE
        btnSave:SENSITIVE = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

