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

  File: viewers/estMisc.w

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cSourceType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimateNo         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCostDescription    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSequenceID         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEstCostCategoryID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstCostCalcBy      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstCostCalcSource  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsFlatFee          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dFlatFeeCharge      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dChargeAmount       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE riSourceRowID       AS ROWID     NO-UNDO.
DEFINE VARIABLE lRecordAvailable    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iFormNo             AS INTEGER   NO-UNDO.

DEFINE VARIABLE hdEstMiscProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cMode          AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

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
&Scoped-Define ENABLED-OBJECTS btnAdd-2 RECT-1 RECT-2 btnCopy-2 btnDelete-2 ~
btnUpdate-2 
&Scoped-Define DISPLAYED-OBJECTS fiCostDescription cbEstCostCategoryID ~
rsCostMethod cbEstCostCalcBy fiFlatFeeCharge cbEstCostCalcSourceCategory ~
cbEstCostCalcSourceGroup cbEstCostCalcSourceLevel fiChargePercent ~
cbEstCostCalcSourceCustom

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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

DEFINE VARIABLE cbEstCostCalcBy AS CHARACTER FORMAT "x(32)" 
     LABEL "Calculate Cost By" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Level","Group","Category","Custom" 
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE cbEstCostCalcSourceCategory AS CHARACTER FORMAT "x(32)" 
     LABEL "Cost Source" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE cbEstCostCalcSourceGroup AS CHARACTER FORMAT "x(32)" 
     LABEL "Cost Source" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE cbEstCostCalcSourceLevel AS CHARACTER FORMAT "x(32)" 
     LABEL "Cost Source" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1.
     
DEFINE VARIABLE cbEstCostCalcSourceCustom AS CHARACTER FORMAT "x(32)" 
     LABEL "Cost Source" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1.     

DEFINE VARIABLE cbEstCostCategoryID AS CHARACTER FORMAT "x(32)" 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE fiChargePercent AS DECIMAL FORMAT ">>,>>,>>9.99<<<<" INITIAL 0 
     LABEL "Charge Percent" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1.

DEFINE VARIABLE fiCostDescription AS CHARACTER FORMAT "x(40)" 
     LABEL "Cost Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE fiFlatFeeCharge AS DECIMAL FORMAT ">>,>>,>>9.99<<<<" INITIAL 0 
     LABEL "Flat Fee Amount" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE rsCostMethod AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Flat Fee", 1,
"Charge Percent", 2
     SIZE 42 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 6.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 3.33.

DEFINE RECTANGLE transPanel-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnAdd-2 AT ROW 11.43 COL 18.8 HELP
          "Add" WIDGET-ID 46
     btnCancel-2 AT ROW 11.43 COL 50.8 HELP
          "Cancel" WIDGET-ID 48
     btnCopy-2 AT ROW 11.43 COL 26.8 HELP
          "Copy" WIDGET-ID 50
     btnDelete-2 AT ROW 11.43 COL 34.8 HELP
          "Delete" WIDGET-ID 52
     fiCostDescription AT ROW 1.76 COL 21 COLON-ALIGNED WIDGET-ID 2
     cbEstCostCategoryID AT ROW 3.14 COL 21 COLON-ALIGNED WIDGET-ID 20
     btnReset-2 AT ROW 11.43 COL 42.8 HELP
          "Reset" WIDGET-ID 54
     rsCostMethod AT ROW 5.05 COL 23 NO-LABEL WIDGET-ID 28
     btnUpdate-2 AT ROW 11.43 COL 10.8 HELP
          "Update/Save" WIDGET-ID 56
     cbEstCostCalcBy AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 14
     fiFlatFeeCharge AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 22
     cbEstCostCalcSourceCategory AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 18
     cbEstCostCalcSourceGroup AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 32
     cbEstCostCalcSourceLevel AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 34
     cbEstCostCalcSourceCustom AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 34
     fiChargePercent AT ROW 8.95 COL 21 COLON-ALIGNED WIDGET-ID 12
     RECT-1 AT ROW 4.81 COL 2 WIDGET-ID 24
     RECT-2 AT ROW 1.33 COL 2 WIDGET-ID 26
     transPanel-3 AT ROW 11.24 COL 9.8 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


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
         HEIGHT             = 13.19
         WIDTH              = 69.8.
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

/* SETTINGS FOR BUTTON btnCancel-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReset-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbEstCostCalcBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbEstCostCalcSourceCategory IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbEstCostCalcSourceGroup IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbEstCostCalcSourceLevel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbEstCostCalcSourceCustom IN FRAME F-Main
   NO-ENABLE                                                            */   
/* SETTINGS FOR COMBO-BOX cbEstCostCategoryID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiChargePercent IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiChargePercent:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiCostDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFlatFeeCharge IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiFlatFeeCharge:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET rsCostMethod IN FRAME F-Main
   NO-ENABLE                                                            */
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


&Scoped-define SELF-NAME rsCostMethod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsCostMethod V-table-Win
ON VALUE-CHANGED OF rsCostMethod IN FRAME F-Main
DO:         
    RUN pSetPanel.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME cbEstCostCalcBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbEstCostCalcBy V-table-Win
ON VALUE-CHANGED OF cbEstCostCalcBy IN FRAME F-Main
DO:         
    RUN pSetPanel.    
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
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        rsCostMethod:SENSITIVE                     = FALSE
/*        estMisc.flatFeeCharge:SENSITIVE     = FALSE*/
/*        estMisc.estCostCalcSource:SENSITIVE = FALSE*/
/*        estMisc.estCostCalcBy:SENSITIVE     = FALSE*/
/*        estMisc.chargePercent:SENSITIVE     = FALSE*/
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
/*    APPLY "VALUE-CHANGED" TO estMisc.estCostCalcBy.*/
/*    APPLY "VALUE-CHANGED" TO rsCostMethod.         */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    rsCostMethod:SENSITIVE = TRUE.
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE estMisc AND estMisc.flatFeeCharge NE 0 THEN
        rsCostMethod:SCREEN-VALUE = "1".
    ELSE
        rsCostMethod:SCREEN-VALUE = "2".
    
    APPLY "VALUE-CHANGED" TO rsCostMethod.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pUpdateFields.
        
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pValidate(OUTPUT lError, OUTPUT cMessage).
    IF lError THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
        
        RETURN ERROR.
    END.
                    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD V-table-Win
PROCEDURE pCRUD:
/*------------------------------------------------------------------------------
 Purpose:
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
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
        CASE iphMode:LABEL:
            WHEN "Add" OR 
            WHEN "Copy" OR 
            WHEN "Update" THEN DO:
                IF iphMode:LABEL EQ "Add" THEN DO:
                    ASSIGN
                    riSourceRowID      = ?
                    iSequenceID        = 0.
                    DISABLE btnReset-2.
                    {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo, OUTPUT iFormNo)"}
                     ASSIGN
                         fiCostDescription:SCREEN-VALUE = ""
                         cbEstCostCategoryID:SCREEN-VALUE = ?
                         cbEstCostCalcSourceCategory:SCREEN-VALUE = ?
                         cbEstCostCalcSourceGroup:SCREEN-VALUE = ?
                         cbEstCostCalcSourceLevel:SCREEN-VALUE = ?
                         cbEstCostCalcSourceCustom:SCREEN-VALUE = ?
                         fiFlatFeeCharge:SCREEN-VALUE = ""
                         fiChargePercent:SCREEN-VALUE = "" . 
                         
                END. /* add */
                ELSE IF iphMode:LABEL EQ "Update" THEN DO:                
                    APPLY "ENTRY":U TO fiCostDescription.
                END.
                ELSE IF iphMode:LABEL EQ "Copy" THEN DO:                     
                    ASSIGN
                    riSourceRowID      = ?
                    iSequenceID        = 0.
                    DISABLE btnReset-2.
                     {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo, OUTPUT iFormNo)"}
                END. /* add */
                                                
                //{methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
                RUN new-state ("record-update-begin").
                
                cMode = iphMode:LABEL.
                ENABLE btnUpdate-2 btnReset-2 btnCancel-2.
                ENABLE fiCostDescription cbEstCostCategoryID rsCostMethod fiFlatFeeCharge
                        cbEstCostCalcBy cbEstCostCalcSourceCategory
                        cbEstCostCalcSourceLevel fiChargePercent cbEstCostCalcSourceGroup
                        cbEstCostCalcSourceCustom.
                        
                RUN pSetPanel.     
                
                DISABLE btnAdd-2 btnCopy-2 btnDelete-2.                  
                
                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").

                btnUpdate-2:LABEL = "Save".           
            END. /* add copy update */
            WHEN "Cancel" OR 
            WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" OR cMode EQ  "Update" THEN DO: 
                    
                    IF cbEstCostCalcBy:SCREEN-VALUE EQ "Category" THEN
                    cEstCostCalcSource = cbEstCostCalcSourceCategory:SCREEN-VALUE.                       
                    ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Group" THEN                     
                    cEstCostCalcSource = cbEstCostCalcSourceGroup:SCREEN-VALUE.                                    
                    ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Level" THEN
                    cEstCostCalcSource = cbEstCostCalcSourceLevel:SCREEN-VALUE.
                    ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Custom" THEN
                    cEstCostCalcSource = cbEstCostCalcSourceCustom:SCREEN-VALUE.
                                        
                   RUN EstMisc_Update IN hdEstMiscProcs(riSourceRowID,
                                         cSourceType,         
                                         cCompany,
                                         cEstimateNo,
                                         iFormNo,
                                         fiCostDescription:SCREEN-VALUE,
                                         cbEstCostCategoryID:SCREEN-VALUE,
                                         IF rsCostMethod:SCREEN-VALUE EQ "1" THEN TRUE ELSE FALSE,
                                         cbEstCostCalcBy:SCREEN-VALUE,
                                         cEstCostCalcSource,
                                         DECIMAL(fiFlatFeeCharge:SCREEN-VALUE),
                                         DECIMAL(fiChargePercent:SCREEN-VALUE),
                                         iSequenceID,
                                         OUTPUT rwRowid,
                                         OUTPUT lError,
                                         OUTPUT cMessage).                                             
                                                                                                  
                        IF lError THEN do:  
                            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
                            RETURN NO-APPLY. 
                        END.    
                        ELSE DO:
                           {methods/run_link.i "RECORD-SOURCE" "pReOpenQuery" "(input rwRowid)" }  
                                                                                                                                        
                        END.                                                                                                 
                    END. /* if Add/Copy/Update */
                END. /* save */
                IF iphMode:LABEL EQ "Cancel" THEN DO:
                        IF lError THEN
                            RETURN NO-APPLY.
                END.

                DISABLE fiCostDescription cbEstCostCategoryID rsCostMethod fiFlatFeeCharge
                        cbEstCostCalcBy cbEstCostCalcSourceCategory
                        cbEstCostCalcSourceLevel fiChargePercent cbEstCostCalcSourceGroup
                        cbEstCostCalcSourceCustom.

                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                btnUpdate-2:LABEL = "Update".

                ENABLE btnAdd-2 btnCopy-2 btnDelete-2.
                DISABLE btnReset-2 btnCancel-2 .                   

                RUN new-state ("record-update-end"). 

                cMode = iphMode:LABEL.
                            
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
                    RUN EstMisc_Delete IN hdEstMiscProcs(riSourceRowID,                                          
                                         OUTPUT lError,
                                         OUTPUT cMessage).             

                        IF lError THEN 
                        do:
                            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
                            RETURN NO-APPLY.                        
                        END.
                        ELSE DO:
                           {methods/run_link.i "RECORD-SOURCE" "local-open-query" }
                        END.
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
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCalcByLevelList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcByGroupList    AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cCalcByCategoryList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcByCustomList   AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.

    {methods/run_link.i "CONTAINER-SOURCE" "GetEstMiscProcsHandle" "(OUTPUT hdEstMiscProcs)"}
    IF NOT VALID-HANDLE (hdEstMiscProcs) THEN
        RUN est/estMiscProcs.p PERSISTENT SET hdEstMiscProcs.
    
    RUN EstMisc_GetCategoryList IN hdEstMiscProcs (cCompany, OUTPUT cCalcByCategoryList).
    RUN EstMisc_GetGroupList IN hdEstMiscProcs (cCompany, OUTPUT cCalcByGroupList).
    RUN EstMisc_GetGroupLevelList IN hdEstMiscProcs (cCompany, OUTPUT cCalcByLevelList).
    RUN EstMisc_GetCustomList IN hdEstMiscProcs (cCompany, OUTPUT cCalcByCustomList).
    
    ASSIGN
        cbEstCostCategoryID:LIST-ITEM-PAIRS         = cCalcByCategoryList
        cbEstCostCalcSourceCategory:LIST-ITEM-PAIRS = cCalcByCategoryList
        cbEstCostCalcSourceGroup:LIST-ITEM-PAIRS    = cCalcByGroupList
        cbEstCostCalcSourceLevel:LIST-ITEM-PAIRS    = cCalcByLevelList
        cbEstCostCalcSourceCustom:LIST-ITEM-PAIRS    = cCalcByCustomList
        .
    
    RUN pUpdateFields.
    RUN pUpdatePanel. 
                                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFields V-table-Win 
PROCEDURE pUpdateFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdttEstMiscBuffer AS HANDLE NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    {methods/run_link.i "RECORD-SOURCE" "GetEstMisc" "(OUTPUT hdttEstMiscBuffer)"}     
    {methods/run_link.i "CONTAINER-SOURCE" "GetSourceType" "(OUTPUT cSourceType)"}
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN             
            cEstimateNo        = ""
            iFormNo            = 0
            cCostDescription   = ""
            iSequenceID        = 0
            cEstCostCategoryID = ?
            cEstCostCalcBy     = "Category"
            cEstCostCalcSource = ?
            lIsFlatFee         = TRUE
            dFlatFeeCharge     = 0
            dChargeAmount      = 0
            riSourceRowID      = ?
            lRecordAvailable   = FALSE
            .            
        
        IF VALID-HANDLE(hdttEstMiscBuffer) AND hdttEstMiscBuffer:AVAILABLE THEN
            ASSIGN
                cSourceType        = hdttEstMiscBuffer:BUFFER-FIELD("sourceType"):BUFFER-VALUE
                cCompany           = hdttEstMiscBuffer:BUFFER-FIELD("company"):BUFFER-VALUE
                cEstimateNo        = hdttEstMiscBuffer:BUFFER-FIELD("estimateNo"):BUFFER-VALUE
                cCostDescription   = hdttEstMiscBuffer:BUFFER-FIELD("costDescription"):BUFFER-VALUE
                iSequenceID        = hdttEstMiscBuffer:BUFFER-FIELD("sequenceID"):BUFFER-VALUE
                cEstCostCategoryID = hdttEstMiscBuffer:BUFFER-FIELD("estCostCategoryID"):BUFFER-VALUE
                cEstCostCalcBy     = hdttEstMiscBuffer:BUFFER-FIELD("estCostCalcBy"):BUFFER-VALUE
                cEstCostCalcSource = hdttEstMiscBuffer:BUFFER-FIELD("estCostCalcSource"):BUFFER-VALUE
                lIsFlatFee         = hdttEstMiscBuffer:BUFFER-FIELD("isFlatFee"):BUFFER-VALUE
                dFlatFeeCharge     = hdttEstMiscBuffer:BUFFER-FIELD("flatFeeCharge"):BUFFER-VALUE
                dChargeAmount      = hdttEstMiscBuffer:BUFFER-FIELD("chargePercent"):BUFFER-VALUE
                riSourceRowID      = hdttEstMiscBuffer:BUFFER-FIELD("sourceRowID"):BUFFER-VALUE
                iFormNo            = hdttEstMiscBuffer:BUFFER-FIELD("formNo"):BUFFER-VALUE
                lRecordAvailable   = TRUE
                NO-ERROR .
                   
        ASSIGN
            fiCostDescription:SCREEN-VALUE   = cCostDescription
            cbEstCostCategoryID:SCREEN-VALUE = cEstCostCategoryID
            cbEstCostCalcBy:SCREEN-VALUE     = cEstCostCalcBy
            fiFlatFeeCharge:SCREEN-VALUE     = STRING(dFlatFeeCharge)
            fiChargePercent:SCREEN-VALUE     = STRING(dChargeAmount)
            rsCostMethod:SCREEN-VALUE        = IF lIsFlatFee THEN "1" ELSE "2" 
            NO-ERROR.  
                       
        IF cbEstCostCalcBy:SCREEN-VALUE EQ "Category" THEN
        DO:
            cbEstCostCalcSourceCategory:SCREEN-VALUE = cEstCostCalcSource.             
        END.    
        ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Group" THEN
        do:
            cbEstCostCalcSourceGroup:SCREEN-VALUE = cEstCostCalcSource.               
        END.    
        ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Level" THEN
        do:
            cbEstCostCalcSourceLevel:SCREEN-VALUE = cEstCostCalcSource.             
        END. 
        ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Custom" THEN
        do:
            cbEstCostCalcSourceCustom:SCREEN-VALUE = cEstCostCalcSource.             
        END.
        
        APPLY "VALUE-CHANGED" TO rsCostMethod.             
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdatePanel V-table-Win
PROCEDURE pUpdatePanel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
           
    DO WITH FRAME {&FRAME-NAME}:    
        IF lRecordAvailable AND (cMode EQ "Add" OR cMode EQ "Copy") THEN DO:            
            DISABLE btnAdd-2 btnReset-2 btnCopy-2 btnDelete-2.
            ENABLE btnUpdate-2 btnCancel-2.
            RETURN.
        END.
        
        DISABLE btnAdd-2 btnUpdate-2 btnCancel-2 btnDelete-2 btnReset-2 btnCopy-2.
           
        IF lRecordAvailable THEN DO:
            ENABLE btnAdd-2 btnCopy-2 btnUpdate-2 btnDelete-2.
        END.
        ELSE
            ENABLE btnAdd-2.
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetPanel V-table-Win
PROCEDURE pSetPanel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rsCostMethod:SCREEN-VALUE = "1" THEN DO:            
            ASSIGN
            cbEstCostCalcBy:SCREEN-VALUE = ""
            cbEstCostCalcSourceCategory:SCREEN-VALUE = ""
            cbEstCostCalcSourceGroup:SCREEN-VALUE = ""
            cbEstCostCalcSourceLevel:SCREEN-VALUE = ""
            cbEstCostCalcSourceCustom:SCREEN-VALUE = ""
            fiChargePercent:SCREEN-VALUE = ""
            cbEstCostCalcBy = ""
            cbEstCostCalcSourceCategory = ""
            cbEstCostCalcSourceGroup = ""
            cbEstCostCalcSourceLevel = ""
            cbEstCostCalcSourceCustom = ""
            fiChargePercent = 0.
            HIDE cbEstCostCalcBy cbEstCostCalcSourceCategory cbEstCostCalcSourceGroup cbEstCostCalcSourceLevel cbEstCostCalcSourceCustom fiChargePercent.
            VIEW fiFlatFeeCharge.             
        END.
        ELSE DO:
            HIDE fiFlatFeeCharge cbEstCostCalcSourceCategory cbEstCostCalcSourceGroup cbEstCostCalcSourceLevel cbEstCostCalcSourceCustom.
            
            VIEW cbEstCostCalcBy fiChargePercent.
            
            ASSIGN
            fiFlatFeeCharge:SCREEN-VALUE = ""
            fiFlatFeeCharge = 0.
            
            IF cbEstCostCalcBy:SCREEN-VALUE EQ "Category" THEN
            do:
                VIEW cbEstCostCalcSourceCategory.
                HIDE cbEstCostCalcSourceGroup  cbEstCostCalcSourceLevel cbEstCostCalcSourceCustom.
            END.    
            ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Group" THEN
            do:
                VIEW cbEstCostCalcSourceGroup.
                HIDE cbEstCostCalcSourceCategory cbEstCostCalcSourceLevel cbEstCostCalcSourceCustom.
            END.    
            ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Level" THEN
            do:
                VIEW cbEstCostCalcSourceLevel.
                HIDE cbEstCostCalcSourceGroup cbEstCostCalcSourceCategory cbEstCostCalcSourceCustom.
            END.   
            ELSE IF cbEstCostCalcBy:SCREEN-VALUE EQ "Custom" THEN
            do:
                VIEW cbEstCostCalcSourceCustom.
                HIDE cbEstCostCalcSourceGroup cbEstCostCalcSourceCategory cbEstCostCalcSourceLevel.
            END.
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

