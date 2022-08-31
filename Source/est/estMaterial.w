&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: est/estMaterial.w

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO .
DEFINE VARIABLE lEnableButton AS LOGICAL NO-UNDO .
/* Local Variable Definitions ---                                       */
{methods/prgsecur.i}               
{methods/defines/hndldefs.i}               
{sys/inc/VAR.i NEW SHARED}
{methods/template/brwCustomDef.i}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE cMatName  AS CHARACTER NO-UNDO .
DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO .
DEFINE VARIABLE cQtyPer   AS CHARACTER NO-UNDO .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES eb
&Scoped-define FIRST-EXTERNAL-TABLE eb

/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb.

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estMaterial 

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 estMaterial.formNo estMaterial.blankNo estMaterial.itemID fget-mat-name() @ cMatName estMaterial.materialTypeID estMaterial.quantity fGetPerQty(estMaterial.quantityPer) @ cQtyPer
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 estMaterial
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 estMaterial
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH estMaterial NO-LOCK WHERE estMaterial.company = cocode ~
  AND estMaterial.estimateNo              = eb.est-no    /*~{&SORTBY-PHRASE}*/
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH estMaterial WHERE NO-LOCK estMaterial.company = cocode ~
  AND estMaterial.estimateNo              = eb.est-no    /* ~{&SORTBY-PHRASE}*/ .
&Scoped-define TABLES-IN-QUERY-BROWSE-1 estMaterial
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 estMaterial


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_CostPerQty BROWSE-1 btn-add btn-copy ~
btn-update btn-delete btn-ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS est-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fget-mat-name B-table-Win 
FUNCTION fget-mat-name RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fget-type-name B-table-Win 
FUNCTION fget-type-name RETURNS CHARACTER
    ( ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPerQty B-table-Win 
FUNCTION fGetPerQty RETURNS CHARACTER
    ( ipcPer AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
    LABEL "Add" 
    SIZE 9.5 BY 1.14.

DEFINE BUTTON btn-copy 
    LABEL "Copy " 
    SIZE 9.5 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "OK" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
    LABEL "Update" 
    SIZE 9.5 BY 1.14.  

DEFINE BUTTON btn-delete 
    LABEL "Delete" 
    SIZE 9.5 BY 1.14.
DEFINE BUTTON Btn_CostPerQty  NO-FOCUS
     LABEL "Override Cost Per Qty" 
     SIZE 27 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE est-no    AS CHARACTER FORMAT "X(8)":U 
    LABEL "Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 12.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 128.8 BY 2
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    estMaterial SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
    QUERY BROWSE-1 NO-LOCK DISPLAY
     estMaterial.formNo WIDTH 10
     estMaterial.blankNo WIDTH 10
     estMaterial.itemID LABEL "Material" FORMAT "x(24)" WIDTH 24
     fget-mat-name() @ cMatName COLUMN-LABEL "Material Name" FORMAT "x(25)":U
     estMaterial.materialTypeID LABEL "Type" FORMAT "x(10)" WIDTH 15
     estMaterial.quantity  FORMAT "->,>>>,>>9.99<<<<" WIDTH 15
     fGetPerQty(estMaterial.quantityPer) @ cQtyPer LABEL "Per" FORMAT "x(7)":U  WIDTH 10 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 128.8 BY 13.05
         BGCOLOR 8 FONT 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    BROWSE-1 AT ROW 5.52 COL 2.2
    btn-add AT ROW 3.95 COL 2.2 WIDGET-ID 16
    btn-copy AT ROW 3.95 COL 12.2
    btn-update AT ROW 3.95 COL 22.2
    btn-delete AT ROW 3.95 COL 32.2
    btn-ok AT ROW 19.02 COL 57
    est-no AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
    "Main Input" VIEW-AS TEXT
    SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
     Btn_CostPerQty AT ROW 3.95 COL 42.4 WIDGET-ID 208
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.2 ROW 1
    SIZE 130 BY 23.71
    FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW W-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Additional Materials"
        HEIGHT             = 19.86
        WIDTH              = 130.8
        MAX-HEIGHT         = 24.71
        MAX-WIDTH          = 156
        VIRTUAL-HEIGHT     = 23.71
        VIRTUAL-WIDTH      = 135
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 1 F-Main */
ASSIGN 
    BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.


/* SETTINGS FOR FILL-IN est-no IN FRAME F-Main
   NO-ENABLE                                                            */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
    THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estMaterial WHERE estMaterial.compamy = cocode ~
  AND estMaterial.estimateNo              = eb.est-no         ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
    _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* New Misccellanceous Product Estimate - Releases */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* New Misccellanceous Product Estimate - Releases */
    DO:
        
        APPLY "choose" TO btn-ok IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME F-Main
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        IF AVAILABLE eb THEN 
        DO:
            IF lEnableButton AND AVAILABLE estMaterial THEN 
            DO:
                RUN est/dEstMaterial.w (RECID(estMaterial),ROWID(eb),"View", OUTPUT lv-rowid) .
            END.
            ELSE IF AVAILABLE estMaterial THEN 
                DO:
                    RUN est/dEstMaterial.w (RECID(estMaterial),ROWID(eb),"Update", OUTPUT lv-rowid) . 
                    RUN repo-query (ROWID(estMaterial)).
                END.
     
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME F-Main
    DO:
   &SCOPED-DEFINE exclude-row-display true
   {methods/template/brwRowDisplay.i}    
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-ENTRY OF BROWSE-1 IN FRAME F-Main
    DO:
    /* This code displays initial values for newly added or copied rows. */
    /*{src/adm/template/brsentry.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON START-SEARCH OF BROWSE-1 IN FRAME F-Main
    DO:
    /*RUN startSearch.  */
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add W-Win
ON CHOOSE OF btn-add IN FRAME F-Main /* Add New */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
        DEFINE BUFFER bff-estMaterial FOR estMaterial .
    
        RUN est/dEstMaterial.w (?,ROWID(eb),"Add", OUTPUT lv-rowid) . 
        FIND FIRST bff-estMaterial NO-LOCK
            WHERE bff-estMaterial.company EQ cocode
            AND ROWID(bff-estMaterial) EQ lv-rowid NO-ERROR .
        IF AVAILABLE bff-estMaterial THEN
            RUN repo-query (lv-rowid).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy W-Win
ON CHOOSE OF btn-copy IN FRAME F-Main /* Copy Selected */
    DO:
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iestMaterialID   AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-rowid        AS ROWID     NO-UNDO.

        DEFINE BUFFER bff-estMaterial FOR estMaterial .
        
        IF AVAILABLE estMaterial AND AVAILABLE eb THEN
        DO:
            
            CREATE bff-estMaterial .
            ASSIGN 
                bff-estMaterial.company      = eb.company 
                bff-estMaterial.estimateNo   = eb.est-no
                bff-estMaterial.formNo       = eb.form-no
                bff-estMaterial.blankNo      = eb.blank-No .
             
            IF AVAILABLE bff-estMaterial THEN 
            DO:
                BUFFER-COPY estMaterial EXCEPT estMaterialID rec_key TO bff-estMaterial .

                RUN est/dEstMaterial.w (RECID(bff-estMaterial),ROWID(eb),"copy", OUTPUT lv-rowid) . 
                IF lv-rowid NE ? THEN
                    RUN repo-query (lv-rowid).
            END.
        END.

        
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok W-Win
ON CHOOSE OF btn-ok IN FRAME F-Main /* OK */
    DO:
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update W-Win
ON CHOOSE OF btn-update IN FRAME F-Main /* Update Selected */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
        IF AVAILABLE estMaterial THEN 
        DO:
            RUN est/dEstMaterial.w (RECID(estMaterial),ROWID(eb),"Update", OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(estMaterial)).
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete W-Win
ON CHOOSE OF btn-delete IN FRAME F-Main /* Update Selected */
    DO:
   
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        IF AVAILABLE estMaterial THEN 
        DO:
            MESSAGE "Are you sure you want to remove this additional Material?" 
                VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN RETURN NO-APPLY.
            FIND CURRENT estMaterial EXCLUSIVE-LOCK NO-ERROR .
            DELETE estMaterial .
            RUN repo-query (lv-rowid).
        END.
    END.
&Scoped-define SELF-NAME Btn_CostPerQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CostPerQty W-Win
ON CHOOSE OF Btn_CostPerQty IN FRAME F-Main /* Override Cost Per Qty */
DO:
   DEFINE VARIABLE phandle AS HANDLE.
   DEFINE VARIABLE cQtyList AS CHARACTER NO-UNDO.
   DEFINE BUFFER bf-estMaterial FOR estMaterial.
   
   IF AVAILABLE estMaterial AND estMaterial.costOverridePerUOM NE 0 THEN DO: 
        FIND FIRST bf-estMaterial EXCLUSIVE-LOCK 
            WHERE ROWID(bf-estMaterial) EQ ROWID(estMaterial)
            NO-ERROR.
        IF AVAILABLE bf-estMaterial THEN 
            bf-estMaterial.costOverridePerUOM = 0.
        RELEASE bf-estMaterial.
   END.
   IF AVAILABLE estMaterial THEN 
        FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.company  EQ cocode
        AND ITEM.i-no EQ estMaterial.itemID 
        NO-ERROR.

   IF AVAIL ITEM THEN DO:
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostSourceFrom = "MF"' ).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostEst# = ' + QUOTER(est-no:SCREEN-VALUE)).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCost = ' + item.i-no).          
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostType = "RM" ' ).      
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostVendor = ""').
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostCustomer = ""').  
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostForm# = ' + ( STRING(estMaterial.formNo)) ).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostBlank# = ' + ( STRING(estMaterial.blankNo))).

      RUN Estimate_GetQuantitiesForEstMaterial(
        ROWID(estMaterial), 
        INPUT "|",
        OUTPUT cQtyList).
      RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostQtyList = ' + cQtyList ).
      RUN windows/vendcostmtx.w  PERSISTENT SET phandle  .
      /* Set the option frame size and colour to give blue background to icons and 
         add the handle of scope define object to temptable for resizizng */
      RUN beforeinitialize IN phandle NO-ERROR.
      RUN dispatch IN phandle ('initialize':U) NO-ERROR.
      /* Add the handle of all smart object to be resized/shifted on resize to the temptable and 
         Shift all the icons towards right */
      RUN afterinitialize IN phandle NO-ERROR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustom.i}
{sys/inc/f3helpw.i}
/*{custom/yellowColumns.i}*/
SESSION:DATA-ENTRY-RETURN = YES.

/* DO WITH FRAME {&FRAME-NAME}:       */
/*       fi_disc-date:SENSITIVE = NO. */
/*       fi_discdays:SENSITIVE = NO.  */
/* /*       TEXT-2:SENSITIVE = NO. */ */
/* END.                               */

FIND FIRST eb WHERE ROWID(eb) EQ iprRowid  NO-LOCK NO-ERROR.
        

{src/adm/template/windowmn.i}
    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
        THEN DELETE WIDGET W-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY est-no 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BROWSE-1 btn-update btn-add btn-copy btn-ok  RECT-1  
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
    /* -----------------------------------------------------------
      Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
      Parameters:  <none>
      Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
    -------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll         AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-printed AS LOG       NO-UNDO.
    DEFINE VARIABLE v-msg      AS CHARACTER NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST eb WHERE ROWID(eb) EQ iprRowid  NO-LOCK NO-ERROR.
  
        IF AVAILABLE eb THEN 
            ASSIGN
                est-no    = eb.est-no . 
                        
        DISPLAY est-no .
    END.

  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    RUN local-open-query.
    GET FIRST {&browse-name}.
    IF NOT AVAILABLE estMaterial  THEN
        lEnableButton = YES .
    ELSE lEnableButton = NO .

    DO WITH FRAME {&FRAME-NAME}:
        IF lEnableButton  THEN 
        DO:
            ASSIGN
                btn-update:SENSITIVE = NO
                btn-copy:SENSITIVE   = NO 
                btn-delete:SENSITIVE = NO .
        END.
    END.

    /* Code placed here will execute AFTER standard behavior.    */
    IF access-close THEN 
    DO:
        APPLY "window-close" TO CURRENT-WINDOW.
        RETURN .
    END.
  
    {methods/nowait.i}
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query W-Win 
PROCEDURE local-open-query :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    FIND FIRST eb WHERE ROWID(eb) EQ iprRowid  NO-LOCK NO-ERROR. 

    
    /* Dispatch standard ADM method.                             */

    CLOSE QUERY BROWSE-1.
  
    OPEN QUERY BROWSE-1 FOR EACH estMaterial WHERE
        estMaterial.company = cocode 
        AND estMaterial.estimateNo              = eb.est-no 
        NO-LOCK.



/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query W-Win 
PROCEDURE repo-query :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:

        /* Needed since browse went blank after adding an item */
        RUN local-open-query.

        REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
       
        IF AVAILABLE estMaterial  THEN
            lEnableButton = NO .
        ELSE lEnableButton = YES .

        DO WITH FRAME {&FRAME-NAME}:
            IF lEnableButton  THEN 
            DO:
                ASSIGN
                    btn-update:SENSITIVE = NO 
                    btn-copy:SENSITIVE   = NO 
                    btn-delete:SENSITIVE = NO .
            END.
            ELSE
                ASSIGN
                    btn-update:SENSITIVE = YES 
                    btn-copy:SENSITIVE   = YES 
                    btn-delete:SENSITIVE = YES .
        END.
    END.

    RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
    /*------------------------------------------------------------------------------
      Purpose:     Send record ROWID's for all tables used by
                   this file.
      Parameters:  see template/snd-head.i
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.               */
    {src/adm/template/snd-head.i}

    /* For each requested table, put it's ROWID in the output list.      */
    {src/adm/template/snd-list.i "estMaterial"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */
               
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fget-mat-name B-table-Win 
FUNCTION fget-mat-name RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
    
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
    IF AVAILABLE estMaterial THEN 
    DO:
        FIND FIRST ITEM  NO-LOCK
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no EQ estMaterial.itemID NO-ERROR .
        IF AVAILABLE ITEM THEN
            ASSIGN cReturn = ITEM.i-NAME .
    END.

    RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fget-type-name B-table-Win 
FUNCTION fget-type-name RETURNS CHARACTER
    ( ipcType AS CHARACTER ) :
    /*------------------------------------------------------------------------------
    
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .

    FIND mat WHERE mat.mat EQ ipcType NO-LOCK NO-ERROR.
    IF AVAILABLE mat THEN 
        ASSIGN cReturn = mat.dscr.
    
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPerQty B-table-Win 
FUNCTION fGetPerQty RETURNS CHARACTER
    ( ipcPer AS CHARACTER ) :
    /*------------------------------------------------------------------------------
    
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
    CASE ipcPer: 
        WHEN 'C'  THEN
            cReturn = "Case".
        WHEN 'P' THEN 
            cReturn = "Pallet".
        WHEN 'L' THEN 
            cReturn = "Lot".
        WHEN 'S' THEN 
            cReturn = "Set".     
        OTHERWISE cReturn = "Each" .

    END CASE.
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

