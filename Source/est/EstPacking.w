&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: est/estPackings.w

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
&Scoped-define INTERNAL-TABLES estPacking 

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 fget-type-name(estPacking.materialType) @ cTypeName estPacking.rmItemID fGet-mat-name() @ cMatName estPacking.quantity fGetPerQty(estPacking.quantityPer) @ cQtyPer estPacking.dimLength estPacking.dimWidth estPacking.dimDepth estPacking.noCharge
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 estPacking
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 estPacking
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH estPacking NO-LOCK WHERE estPacking.company = cocode ~
  AND estPacking.estimateNo              = eb.est-no  ~
  AND estPacking.FormNo                  = eb.form-no ~
  AND estPacking.BlankNo                 = eb.blank-No         /*~{&SORTBY-PHRASE}*/
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH estPacking WHERE NO-LOCK estPacking.company = cocode ~
  AND estPacking.estimateNo              = eb.est-no ~
  AND estPacking.FormNo                  = eb.form-no ~
  AND estPacking.BlankNo                 = eb.blank-No   /* ~{&SORTBY-PHRASE}*/ .
&Scoped-define TABLES-IN-QUERY-BROWSE-1 estPacking
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 estPacking


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-update btn-add btn-copy btn-ok ~
btn-delete RECT-1  
&Scoped-Define DISPLAYED-OBJECTS est-no iForm iBlank cCustPart ~
cCase cPallet 

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
    LABEL "Add New" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-copy 
    LABEL "Copy Selected" 
    SIZE 19 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "OK" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
    LABEL "Update Selected" 
    SIZE 21.8 BY 1.14.  

DEFINE BUTTON btn-delete 
    LABEL "Delete Selected" 
    SIZE 21.8 BY 1.14.

DEFINE VARIABLE cCustPart AS CHARACTER FORMAT "X(15)":U 
    LABEL "Part#" 
    VIEW-AS FILL-IN 
    SIZE 22.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.



DEFINE VARIABLE est-no    AS CHARACTER FORMAT "X(8)":U 
    LABEL "Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 12.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iBlank    AS INTEGER   FORMAT ">>9":U INITIAL 0 
    LABEL "Blank #" 
    VIEW-AS FILL-IN 
    SIZE 5.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iForm     AS INTEGER   FORMAT ">>9":U INITIAL 0 
    LABEL "Form #" 
    VIEW-AS FILL-IN 
    SIZE 5.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.


DEFINE VARIABLE cPallet   AS CHARACTER FORMAT "X(25)":U 
    LABEL "Pallet" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCase     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Case" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 122.2 BY 3
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    estPacking SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
    QUERY BROWSE-1 NO-LOCK DISPLAY
    fget-type-name(estPacking.materialType) @ cTypeName LABEL "Type" FORMAT "x(15)":U WIDTH 18
    estPacking.noCharge LABEL "NC" WIDTH 3 FORMAT "Y/N"
    estPacking.rmItemID LABEL "Material" WIDTH 15 
    fget-mat-name() @ cMatName COLUMN-LABEL "Material Name" FORMAT "x(25)":U
    estPacking.quantity LABEL "Quantity" FORMAT ">>>,>>9.9<<":U WIDTH 14 
    fGetPerQty(estPacking.quantityPer) @ cQtyPer LABEL "Per" FORMAT "x(7)":U  WIDTH 10 
    estPacking.dimLength LABEL "Length" FORMAT ">9.9999":U WIDTH 10 
    estPacking.dimWidth LABEL "Width" FORMAT ">9.9999":U WIDTH 10 
    estPacking.dimDepth LABEL "Depth" FORMAT ">9.9999":U WIDTH 10 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148.8 BY 13.05
         BGCOLOR 8 FONT 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    BROWSE-1 AT ROW 6.52 COL 2.2
    btn-update AT ROW 4.95 COL 37.8
    btn-add AT ROW 4.95 COL 2.2 WIDGET-ID 16
    btn-copy AT ROW 4.95 COL 17.6
    btn-delete AT ROW 4.95 COL 60.8
    btn-ok AT ROW 20.62 COL 57
    est-no AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
    iForm AT ROW 2.14 COL 37.4 COLON-ALIGNED WIDGET-ID 314
    iBlank AT ROW 2.14 COL 55.2 COLON-ALIGNED WIDGET-ID 316
    cCustPart AT ROW 3.33 COL 13.4 COLON-ALIGNED WIDGET-ID 176
    cCase AT ROW 2.14 COL 95.4 COLON-ALIGNED WIDGET-ID 178
    cPallet AT ROW 3.33 COL 95.4 COLON-ALIGNED WIDGET-ID 204
    "Main Input" VIEW-AS TEXT
    SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.2 ROW 1
    SIZE 150 BY 23.71
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
        TITLE              = "Additional Packing Materials"
        HEIGHT             = 21.86
        WIDTH              = 151.8
        MAX-HEIGHT         = 24.71
        MAX-WIDTH          = 156
        VIRTUAL-HEIGHT     = 24.71
        VIRTUAL-WIDTH      = 156
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
/* SETTINGS FOR FILL-IN iBlank IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cPallet IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
    THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estPacking WHERE estPacking.compamy = cocode ~
  AND estPacking.estimateNo              = eb.est-no ~
  AND estPacking.FormNo                  = eb.form-no ~
  AND estPacking.BlankNo                 = eb.blank-No         ~{&SORTBY-PHRASE}.
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
            IF lEnableButton AND AVAILABLE estPacking THEN 
            DO:
                RUN est/dEstPacking.w (RECID(estPacking),ROWID(eb),"View", OUTPUT lv-rowid) .
            END.
            ELSE IF AVAILABLE estPacking THEN 
                DO:
                    RUN est/dEstPacking.w (RECID(estPacking),ROWID(eb),"Update", OUTPUT lv-rowid) . 
                    RUN repo-query (ROWID(estPacking)).
                END.
     
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME F-Main
    DO:
    
   
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
        DEFINE BUFFER bff-estPacking FOR estPacking .
    
        RUN est/dEstPacking.w (?,ROWID(eb),"Add", OUTPUT lv-rowid) . 
        FIND FIRST bff-estPacking NO-LOCK
            WHERE bff-estPacking.company EQ cocode
            AND ROWID(bff-estPacking) EQ lv-rowid NO-ERROR .
        IF AVAILABLE bff-estPacking THEN
            RUN repo-query (lv-rowid).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy W-Win
ON CHOOSE OF btn-copy IN FRAME F-Main /* Copy Selected */
    DO:
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iestPackingID   AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-rowid        AS ROWID     NO-UNDO.

        DEFINE BUFFER bff-estPacking FOR estPacking .
        
        IF AVAILABLE estPacking AND AVAILABLE eb THEN
        DO:
            
            CREATE bff-estPacking .
            ASSIGN 
                bff-estPacking.company      = eb.company 
                bff-estPacking.estimateNo   = eb.est-no
                bff-estPacking.FormNo       = eb.form-no
                bff-estPacking.BlankNo      = eb.blank-No .
             
            IF AVAILABLE bff-estPacking THEN 
            DO:
                BUFFER-COPY estPacking EXCEPT estPackingID rec_key TO bff-estPacking .

                RUN est/dEstPacking.w (RECID(bff-estPacking),ROWID(eb),"copy", OUTPUT lv-rowid) . 
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
        IF AVAILABLE estPacking THEN 
        DO:
            RUN est/dEstPacking.w (RECID(estPacking),ROWID(eb),"Update", OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(estPacking)).
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete W-Win
ON CHOOSE OF btn-delete IN FRAME F-Main /* Update Selected */
    DO:
   
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        IF AVAILABLE estPacking THEN 
        DO:
            MESSAGE "Are you sure you want to remove this additional packing?" 
                VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN RETURN NO-APPLY.
            FIND CURRENT estPacking EXCLUSIVE-LOCK NO-ERROR .
            DELETE estPacking .
            RUN repo-query (lv-rowid).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
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
  DISPLAY est-no iForm iBlank cCustPart cCase cPallet 
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
                est-no    = eb.est-no 
                cCustPart = eb.part-no
                cCase     = eb.cas-no
                iForm     = (eb.form-no)
                iBlank    = (eb.blank-no)
                cPallet   = eb.tr-no .
        
        DISPLAY est-no cCustPart cCase iForm iBlank cPallet.
    END.

  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    RUN local-open-query.
    GET FIRST {&browse-name}.
    IF NOT AVAILABLE estPacking  THEN
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
  
    OPEN QUERY BROWSE-1 FOR EACH estPacking WHERE
        estPacking.company = cocode 
        AND estPacking.estimateNo              = eb.est-no 
        AND estPacking.FormNo                  = eb.form-no
        AND estPacking.BlankNo                 = eb.blank-No NO-LOCK.



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
       
        IF AVAILABLE estPacking  THEN
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
    {src/adm/template/snd-list.i "estPacking"}

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
    IF AVAILABLE estPacking THEN 
    DO:
        FIND FIRST ITEM  NO-LOCK
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no EQ estPacking.rmItemID NO-ERROR .
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
        OTHERWISE cReturn = "Each" .

    END CASE.
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

