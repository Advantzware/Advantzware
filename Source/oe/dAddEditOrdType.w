&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditOrdType.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER iprwRowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.   /* add,update,view */ 
DEFINE OUTPUT PARAMETER oprwRowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}


ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-item-rowid   AS ROWID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE v-count         AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel item-dscr RECT-21 ~
RECT-38 type-source tb_create-job tb_create-po type-est tb_inactive ~
type-color type-seq
&Scoped-Define DISPLAYED-OBJECTS type-id item-dscr type-source ~
tb_create-job type-color type-seq tb_create-po type-est tb_inactive 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE item-dscr   AS CHARACTER FORMAT "X(32)":U 
    LABEL "Description" 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-color  AS INTEGER   FORMAT ">>9":U INITIAL 0 
    LABEL "Color" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-est    AS CHARACTER FORMAT "X(1)":U 
    LABEL "Estimate Type" 
    VIEW-AS FILL-IN 
    SIZE 21.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-id     AS INTEGER   FORMAT ">>>>>9":U INITIAL 0 
    LABEL "Type Id" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-seq    AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Sequence" 
    VIEW-AS FILL-IN
    SIZE 16 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-source AS CHARACTER FORMAT "X(10)":U 
    LABEL "Order Type Source" 
    VIEW-AS COMBO-BOX INNER-LINES 9
          LIST-ITEM-PAIRS "Customer","Customer",
                     "Estimate","Estimate",
                     "Quote","Quote",
                     "Import","Import",
                     "Repeat","Repeat",
                     "Transfer","Transfer",
                     "Web","Web",
                     "Rework","Rework",
                     "Inactive","Inactive"
          DROP-DOWN-LIST 
    SIZE 21.6 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 124.8 BY 8.62
    BGCOLOR 15 .

DEFINE VARIABLE tb_create-job AS LOGICAL INITIAL NO 
    LABEL "Create Job" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_create-po  AS LOGICAL INITIAL NO 
    LABEL "Create Purchase Order" 
    VIEW-AS TOGGLE-BOX
    SIZE 34.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inactive   AS LOGICAL INITIAL NO 
    LABEL "Inactive" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    Btn_OK AT ROW 10.57 COL 108
    Btn_Done AT ROW 10.86 COL 109
    Btn_Cancel AT ROW 10.57 COL 117
    type-id AT ROW 2 COL 18.6 COLON-ALIGNED WIDGET-ID 208
    item-dscr AT ROW 3.24 COL 18.6 COLON-ALIGNED WIDGET-ID 210
    type-source AT ROW 3.24 COL 93.2 COLON-ALIGNED WIDGET-ID 328
    type-color AT ROW 4.57 COL 18.6 COLON-ALIGNED WIDGET-ID 330
    type-est AT ROW 4.57 COL 93.2 COLON-ALIGNED WIDGET-ID 336
    type-seq AT ROW 5.95 COL 18.6 COLON-ALIGNED WIDGET-ID 332     
    tb_inactive AT ROW 7.67 COL 20.8 WIDGET-ID 338
    tb_create-job AT ROW 7.67 COL 46.4 WIDGET-ID 260
    tb_create-po AT ROW 7.67 COL 77.2 WIDGET-ID 334
    "Order Type" VIEW-AS TEXT
    SIZE 14 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
    RECT-21 AT ROW 10.33 COL 107
    RECT-38 AT ROW 1.43 COL 1.2
    SPACE(0.99) SKIP(2.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Add/Update Order Type".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

ASSIGN 
    tb_create-job:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    tb_create-po:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    tb_inactive:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".


/* SETTINGS FOR FILL-IN type-id IN FRAME Dialog-Frame
   NO-ENABLE                                                            */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Add/Update Order Type */
    DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "rmItemID" THEN 
                DO:
                    
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Order Type */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Order Type */
    DO:
            
        IF AVAILABLE orderType THEN
            oprwRowid = ROWID(orderType) .

        /* IF lv-item-rowid NE ? THEN 
         DO:
             FIND FIRST orderType EXCLUSIVE-LOCK
                 WHERE ROWID(orderType) EQ lv-item-rowid  NO-ERROR.
             IF AVAILABLE orderType THEN DELETE orderType .
             oprwRowid = ? .
         END. */
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        
    
        IF AVAILABLE orderType THEN
            oprwRowid = ROWID(orderType) .

        /*IF lv-item-rowid NE ? THEN 
        DO:
            FIND FIRST orderType EXCLUSIVE-LOCK
                WHERE ROWID(orderType) EQ lv-item-rowid  NO-ERROR.
            IF AVAILABLE orderType THEN DELETE orderType .
            oprwRowid = ? .
        END.  */
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
    DO:
        
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
    DO:
        DEFINE VARIABLE ld              AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE dCostStorage    AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE dCostHandling   AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.

        DEFINE BUFFER bf-orderType FOR orderType.
        
        IF ipcType EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        
        /*RUN valid-part-no(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
                
         */        
       
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.
        
        IF ipcType EQ "Add" OR ipcType EQ "copy" THEN
        DO:
            CREATE bf-orderType.
            ASSIGN
                bf-orderType.company = cocode
                .
        END.
        
        IF ipcType EQ "Update" THEN
            FIND FIRST bf-orderType EXCLUSIVE-LOCK
                WHERE ROWID(bf-orderType) EQ iprwRowid NO-ERROR .        
           
        ASSIGN
            bf-orderType.orderTypeDescription = item-dscr
            bf-orderType.orderTypeSource      = type-source             
            bf-orderType.orderTypeColor       = type-color             
            bf-orderType.numberSequence       = type-seq
            bf-orderType.inactive             = tb_inactive
            bf-orderType.createJob            = tb_create-job
            bf-orderType.createPurchaseOrder  = tb_create-po            
            bf-orderType.estimateType         = type-est                       
            .             
        oprwRowid = ROWID(bf-orderType).
        FIND CURRENT bf-orderType NO-LOCK NO-ERROR. 
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME type-color
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-color Dialog-Frame
ON LEAVE OF type-color IN FRAME Dialog-Frame /* Color */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-est Dialog-Frame
ON LEAVE OF type-est IN FRAME Dialog-Frame /* Estimate Type */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            
        END.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-id Dialog-Frame
ON LEAVE OF type-id IN FRAME Dialog-Frame /* Type Id */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-seq Dialog-Frame
ON LEAVE OF type-seq IN FRAME Dialog-Frame /* Sequence */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-source
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-source Dialog-Frame
ON LEAVE OF type-source IN FRAME Dialog-Frame /* Order Type Source */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            
        END.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST orderType NO-LOCK
        WHERE ROWID(orderType) EQ iprwRowid NO-ERROR .
    
    IF ipcType EQ "copy" THEN lv-item-rowid = iprwRowid.

    /*IF iprwRowid EQ ? THEN 
    DO:
        //RUN pCreateItem.
    END.
    ELSE FIND orderType NO-LOCK WHERE ROWID(orderType) EQ iprwRowid NO-ERROR.      */

    IF ipcType NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.
    
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
    HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
    /*------------------------------------------------------------------------------
                          Purpose:     
                          PARAMs:  <none>
                          Notes:       
         ------------------------------------------------------------------------------*/   
    
    FIND FIRST orderType NO-LOCK
        WHERE ROWID(orderType) EQ iprwRowid NO-ERROR. 
    IF AVAILABLE orderType THEN 
        ASSIGN
            type-id       = orderType.orderTypeID
            item-dscr     = orderType.orderTypeDescription
            type-source   = orderType.orderTypeSource
            type-color    = orderType.orderTypeColor
            type-seq      = orderType.numberSequence
            type-est      = orderType.estimateType  
            tb_inactive   = orderType.inactive
            tb_create-job = orderType.createJob 
            tb_create-po  = orderType.createPurchaseOrder .    
      
    IF ipcType EQ "Copy" THEN
        type-id = 0 .
    
    DISPLAY   
        type-id item-dscr type-source tb_create-job type-color type-seq 
        tb_create-po type-est tb_inactive
        WITH FRAME Dialog-Frame.       
                                            
    IF ipcType NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateItem Dialog-Frame 
PROCEDURE pCreateItem :
/*------------------------------------------------------------------------------
                      Purpose:     
                      PARAMs:  <none>
                      Notes:       
     ------------------------------------------------------------------------------*/   
          
        ASSIGN            
           
            tb_inactive   = NO
            tb_create-job = NO 
            tb_create-po  = NO .         
    
    DISPLAY   
          type-id item-dscr type-source tb_create-job type-color type-seq 
          tb_create-po type-est tb_inactive
        WITH FRAME Dialog-Frame.       
                                            
    IF ipcType NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
    DISPLAY type-id item-dscr type-source tb_create-job type-color type-seq 
        tb_create-po type-est tb_inactive 
        WITH FRAME Dialog-Frame.
    ENABLE Btn_OK Btn_Done Btn_Cancel item-dscr RECT-21 RECT-38 type-source 
        tb_create-job tb_create-po type-est tb_inactive type-color type-seq
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
    /*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

/* Code placed here will execute AFTER standard behavior.    */
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBoardFromStyle Dialog-Frame 
PROCEDURE pGetBoardFromStyle :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST flute NO-LOCK
            WHERE flute.company EQ cocode NO-ERROR .
        IF AVAILABLE flute THEN
            FIND FIRST reftable WHERE reftable.reftable = "STYFLU" AND reftable.company = ipcStyle 
                AND reftable.loc = flute.code
                AND reftable.code = "BOARD"
                NO-LOCK NO-ERROR. 
        board:screen-value = IF AVAILABLE reftable AND AVAILABLE flute AND reftable.dscr NE "" THEN reftable.dscr ELSE board:screen-value.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME     */


/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style Dialog-Frame 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST style
            WHERE style.company  EQ cocode
            AND style.style    EQ style-cod:SCREEN-VALUE
            AND style.industry EQ "2")  THEN 
        DO:
            MESSAGE "Invalid Style Code, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO style-cod .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   */

