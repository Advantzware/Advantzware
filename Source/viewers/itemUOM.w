&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/itemUOM.w

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
&Scoped-define enable-proc enable-proc
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN
    cocode = g_company
    locode = g_loc
    .
    
{system/ttConversionProcs.i}

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
&Scoped-define EXTERNAL-TABLES ItemUOM
&Scoped-define FIRST-EXTERNAL-TABLE ItemUOM


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ItemUOM.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemUoM.descr itemUoM.convFactor ~
itemUoM.canPurchase itemUoM.canSell itemUoM.inactive 
&Scoped-define ENABLED-TABLES itemUoM
&Scoped-define FIRST-ENABLED-TABLE itemUoM
&Scoped-Define DISPLAYED-FIELDS itemUoM.UOM itemUoM.descr ~
itemUoM.convFactor itemUoM.canPurchase itemUoM.canSell itemUoM.inactive ~
itemUoM.createdBy itemUoM.createdDtTm itemUoM.updatedBy itemUoM.updatedDtTm 
&Scoped-define DISPLAYED-TABLES itemUoM
&Scoped-define FIRST-DISPLAYED-TABLE itemUoM


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS itemUoM.UOM 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 50 BY 17.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemUoM.UOM AT ROW 1.24 COL 14 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 12.2 BY 1
          BGCOLOR 15 
     itemUoM.descr AT ROW 2.43 COL 14 COLON-ALIGNED WIDGET-ID 8
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     itemUoM.convFactor AT ROW 3.62 COL 14 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
          BGCOLOR 15 
     itemUoM.canPurchase AT ROW 4.81 COL 16 WIDGET-ID 20
          LABEL "Valid for PO Quantity and Cost"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .81
     itemUoM.canSell AT ROW 5.81 COL 16 WIDGET-ID 22
          LABEL "Valid for Order Quantity && Price"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .81
     itemUoM.inactive AT ROW 6.71 COL 16 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     itemUoM.createdBy AT ROW 13.62 COL 14 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
          BGCOLOR 15 
     itemUoM.createdDtTm AT ROW 14.81 COL 4.8 WIDGET-ID 32
          LABEL "Date/Time"
          VIEW-AS FILL-IN 
          SIZE 34.2 BY 1
          BGCOLOR 15 
     itemUoM.updatedBy AT ROW 16 COL 14 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
          BGCOLOR 15 
     itemUoM.updatedDtTm AT ROW 17.19 COL 4.8 WIDGET-ID 38
          LABEL "Date/Time"
          VIEW-AS FILL-IN 
          SIZE 34.2 BY 1
          BGCOLOR 15 
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ItemUOM
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
         HEIGHT             = 17.38
         WIDTH              = 50.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

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

/* SETTINGS FOR TOGGLE-BOX itemUoM.canPurchase IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX itemUoM.canSell IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemUoM.createdBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemUoM.createdDtTm IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
/* SETTINGS FOR FILL-IN itemUoM.descr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemUoM.UOM IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN itemUoM.updatedBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemUoM.updatedDtTm IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
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

&Scoped-define SELF-NAME itemUoM.UOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemUoM.UOM V-table-Win
ON LEAVE OF itemUoM.UOM IN FRAME F-Main /* UOM */
DO:
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemNo  AS CHARACTER NO-UNDO.
    
    IF LASTKEY NE -1 THEN DO:
        {methods/run_link.i "RECORD-SOURCE" "Get-Values"
            "(OUTPUT cCompany, OUTPUT cItemNo)"}
       IF CAN-FIND(FIRST ItemUOM
                   WHERE ItemUOM.company  EQ cCompany
                     AND ItemUOM.itemType EQ "FG"
                     AND ItemUOM.itemID   EQ cItemNo
                     AND ItemUOM.UOM      EQ ItemUOM.UOM:SCREEN-VALUE) THEN DO:
            MESSAGE
                "UOM" ItemUOM.UOM:SCREEN-VALUE "for Item" cItemNo "already exits!"
            VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY":U TO SELF.
            RETURN NO-APPLY.
        END. /* if can-find */
    END. /* if lastkey */
    IF ItemUOM.descr:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST uom NO-LOCK
             WHERE uom.uom EQ ItemUOM.UOM:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE uom THEN
        ItemUOM.descr:SCREEN-VALUE = uom.dscr.
    END. /* if descr */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME itemUoM.UOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemUoM.UOM V-table-Win
ON HELP OF itemUoM.UOM IN FRAME F-Main /* UOM */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN pSetUomTT.
    RUN windows/l-itemuom.w (cocode, FOCUS:SCREEN-VALUE, INPUT TABLE ttUOMEffective, OUTPUT char-val). 
    IF char-val <> "" THEN ASSIGN itemUoM.UOM:SCREEN-VALUE = ENTRY(1,char-val)
                                  itemUoM.descr:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


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
  {src/adm/template/row-list.i "ItemUOM"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ItemUOM"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      DISPLAY
          ItemUOM.updatedBy
          ItemUOM.updatedDtTm
          .
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/itemUOM.i}
  DO WITH FRAME {&FRAME-NAME}:
      DISPLAY
          ItemUOM.createdBy
          ItemUOM.createdDtTm
          ItemUOM.updatedBy
          ItemUOM.updatedDtTm
          .
  END. /* with frame */

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
  {src/adm/template/snd-list.i "ItemUOM"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUomTT V-table-Win 
PROCEDURE pSetUomTT PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/    

    EMPTY TEMP-TABLE ttUOMEffective.
    
    RUN Conv_GetValidAllUOMTT(
            OUTPUT TABLE ttUOMEffective
            ).   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

