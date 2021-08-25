&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: oe/dOrderType.w
  
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
&SCOPED-DEFINE yellowColumnsName dOrderType
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


{sys/inc/var.i NEW shared}   

ASSIGN 
    cocode = g_company
    locode = g_loc.   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES orderType

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 orderType.orderTypeID orderType.orderTypeDescription orderType.orderTypeSource orderType.orderTypeColor orderType.inactive orderType.numberSequence orderType.createJob orderType.createPurchaseOrder orderType.estimateType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH orderType WHERE orderType.Company = cocode ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH orderType WHERE orderType.Company = cocode ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 orderType
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 orderType


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-add btn-copy btn-update btn-delete ~
BROWSE-1 

&Scoped-Define DISPLAYED-OBJECTS fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
    LABEL "Add " 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-copy 
    LABEL "Copy " 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
    LABEL "Delete " 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
    LABEL "Update " 
    SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-5
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 157.6 BY 17.14
    BGCOLOR 15 .
     
DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 47 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.     

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    orderType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
    QUERY BROWSE-1 DISPLAY
    orderType.orderTypeID LABEL "Type Id" WIDTH 10 LABEL-BGCOLOR 14 FORMAT ">>9"
    orderType.orderTypeDescription LABEL "Description"  LABEL-BGCOLOR 14 FORMAT "x(32)"
    orderType.orderTypeSource LABEL "Type Source" FORMAT "x(10)" WIDTH 15 LABEL-BGCOLOR 14
    orderType.orderTypeColor LABEL "Color" FORMAT ">>9"  LABEL-BGCOLOR 14
    orderType.inactive LABEL "Inactive" FORMAT "Yes/No"  LABEL-BGCOLOR 14
    orderType.numberSequence LABEL "Sequence" FORMAT ">9" LABEL-BGCOLOR 14
    orderType.createJob LABEL "Create Job" FORMAT "Yes/No" LABEL-BGCOLOR 14
    orderType.createPurchaseOrder LABEL "Create Purchase Order" FORMAT "Yes/No" LABEL-BGCOLOR 14
    orderType.estimateType LABEL "Estimate Type" LABEL-BGCOLOR 14
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 155.4 BY 14.52
         ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.
         

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
    btn-add AT ROW 17.29 COL 4.8 WIDGET-ID 16
    btn-copy AT ROW 17.29 COL 20.4 WIDGET-ID 252
    btn-update AT ROW 17.29 COL 36 WIDGET-ID 256
    btn-delete AT ROW 17.29 COL 52 WIDGET-ID 254
    BROWSE-1 AT ROW 2.19 COL 5
    "Order Type" VIEW-AS TEXT
    SIZE 16 BY .71 AT ROW 1.29 COL 6.8 WIDGET-ID 264
    RECT-5 AT ROW 1.67 COL 3.8 WIDGET-ID 262
    fi_sortby AT ROW 17.29 COL 70 COLON-ALIGNED NO-LABELS
    SPACE(1.79) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Order Type".


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
/* BROWSE-TAB BROWSE-1 btn-delete D-Dialog */
ASSIGN 
    FRAME D-Dialog:SCROLLABLE = FALSE
    FRAME D-Dialog:HIDDEN     = TRUE.
       
ASSIGN 
    BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME D-Dialog = TRUE.       

/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:HIDDEN IN FRAME D-Dialog = TRUE.
    
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH orderType WHERE orderType.Company = cocode ~
        ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Order Type */
    DO:             
                
        APPLY "END-ERROR":U TO SELF.
       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add D-Dialog
ON CHOOSE OF btn-add IN FRAME D-Dialog /* Add  */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
        DEFINE BUFFER bff-orderType FOR orderType .
        
            
        RUN oe/dAddEditOrdType.w (?,"Add",OUTPUT lv-rowid) . 
        
        FIND FIRST bff-orderType NO-LOCK
            WHERE bff-orderType.Company EQ cocode
            AND ROWID(bff-orderType) EQ lv-rowid NO-ERROR .
        
        IF AVAILABLE bff-orderType THEN
            RUN repo-query (lv-rowid). 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy D-Dialog
ON CHOOSE OF btn-copy IN FRAME D-Dialog /* Copy  */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        DEFINE BUFFER bff-orderType FOR orderType.         
    
        IF AVAILABLE orderType THEN
        DO: 
            RUN oe/dAddEditOrdType.w (ROWID(orderType),"Copy", OUTPUT lv-rowid) . 
            
            FIND FIRST bff-orderType NO-LOCK
                WHERE bff-orderType.Company EQ cocode
                AND ROWID(bff-orderType) EQ lv-rowid NO-ERROR .
        
            IF AVAILABLE bff-orderType THEN
                RUN repo-query (lv-rowid).             
        END.      
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete D-Dialog
ON CHOOSE OF btn-delete IN FRAME D-Dialog /* Delete  */
    DO:
        DEFINE VARIABLE hftp     AS HANDLE NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID  NO-UNDO.
        IF AVAILABLE orderType THEN 
        DO:
            IF orderType.orderTypeID GE 1 AND orderType.orderTypeID LE 10  THEN
            DO:
                MESSAGE "Order Type not allowed to delete " 
                    VIEW-AS ALERT-BOX INFORMATION.
                RETURN NO-APPLY.                
            END.
            
            MESSAGE "Are you sure you want to delete.. " 
                VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN RETURN NO-APPLY.  
            
            FIND CURRENT orderType EXCLUSIVE-LOCK NO-ERROR.
            
            DELETE orderType .
            RUN repo-query (lv-rowid).
        
        END.                                             
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update D-Dialog
ON CHOOSE OF btn-update IN FRAME D-Dialog /* Update  */
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
        DEFINE VARIABLE rwRowid  AS ROWID NO-UNDO. 
        IF AVAILABLE orderType THEN 
        DO:
           
            RUN oe/dAddEditOrdType.w (ROWID(orderType),"Update",OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(orderType)).
        END. 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME D-Dialog
    DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
        DEFINE VARIABLE rwRowid  AS ROWID NO-UNDO. 
        IF AVAILABLE orderType THEN 
        DO:
           
            RUN oe/dAddEditOrdType.w (ROWID(orderType),"Update",OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(orderType)).
        END.  
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON START-SEARCH OF BROWSE-1 IN FRAME D-Dialog
    DO:
        RUN startSearch.
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
    {custom/yellowColumns.i}
    
    RUN enable_UI.
    {methods/nowait.i}     
    
    DO WITH FRAME {&FRAME-NAME}:  
        fi_sortby:HIDDEN = YES.
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
    ENABLE btn-add btn-copy btn-update btn-delete BROWSE-1 
        WITH FRAME D-Dialog.
    VIEW FRAME D-Dialog.
    {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query D-Dialog 
PROCEDURE repo-query :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:           
        
        {&open-query-{&browse-name}}

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



