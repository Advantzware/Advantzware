&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fixShipto.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 02/26/2019

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = g_company
    locode = g_loc.
DEFINE VARIABLE cChar AS CHARACTER.
DEFINE TEMP-TABLE tt-oe-shipto 
    FIELD tt-recid    AS RECID
    FIELD ship-id     LIKE shipto.ship-id
    FIELD ship-name   LIKE shipto.ship-name
    FIELD ship-add    AS CHARACTER 
    FIELD ship-city   LIKE shipto.ship-city
    FIELD i-count     AS INTEGER
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    .
DEFINE BUFFER bf-shipto FOR shipto .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME browse-machine


/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-shipto 

/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine tt-oe-shipto.IS-SELECTED tt-oe-shipto.ship-id tt-oe-shipto.ship-name tt-oe-shipto.ship-add tt-oe-shipto.ship-city   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine tt-oe-shipto.IS-SELECTED   
&Scoped-define ENABLED-TABLES-IN-QUERY-browse-machine tt-oe-shipto
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browse-machine tt-oe-shipto
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH tt-oe-shipto ~
             NO-LOCK BY tt-oe-shipto.ship-id
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto ~
            NO-LOCK BY tt-oe-shipto.ship-id.
&Scoped-define TABLES-IN-QUERY-browse-machine tt-oe-shipto 
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine tt-oe-shipto

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-browse-machine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 begin_cust ~
fi_ship-id btn-show rd_active browse-machine btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust fi_ship-id rd_active 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-show 
    LABEL "Show All ShipTo" 
    SIZE 17.4 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
    LABEL "Customer" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_ship-id AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To Id" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE rd_active  AS CHARACTER INITIAL "I" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Inactive", "I",
    "Delete", "D"
    SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 14.76.

DEFINE RECTANGLE RECT-18
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 89 BY 10.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-machine FOR 
    tt-oe-shipto
    SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
    QUERY browse-machine NO-LOCK DISPLAY
    tt-oe-shipto.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
    tt-oe-shipto.ship-id FORMAT "x(8)" COLUMN-LABEL "Ship ID" WIDTH 12
    tt-oe-shipto.ship-name FORMAT "X(20)" COLUMN-LABEL "Name" WIDTH 20
    tt-oe-shipto.ship-add COLUMN-LABEL "Address"  WIDTH 20
    tt-oe-shipto.ship-city COLUMN-LABEL "City" WIDTH 15
      ENABLE tt-oe-shipto.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 79.6 BY 9.05 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust AT ROW 6.24 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer" WIDGET-ID 10
    fi_ship-id AT ROW 6.24 COL 60 COLON-ALIGNED
    btn-show AT ROW 7.81 COL 67.6 WIDGET-ID 2
    rd_active AT ROW 7.91 COL 34.2 NO-LABELS WIDGET-ID 16
    browse-machine AT ROW 9.81 COL 6.4
    btn-process AT ROW 20 COL 21
    btn-cancel AT ROW 20 COL 53
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 5.29 COL 3.6
    "" VIEW-AS TEXT
    SIZE 2.2 BY .95 AT ROW 1.95 COL 88
    BGCOLOR 11 
    RECT-17 AT ROW 4.86 COL 1
    RECT-18 AT ROW 9.33 COL 1 WIDGET-ID 20
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.6 BY 21.76.

DEFINE FRAME FRAME-B
    "" VIEW-AS TEXT
    SIZE 88.8 BY .95 AT ROW 1 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 88.8 BY .95 AT ROW 3.76 COL 1
    BGCOLOR 11 
    "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
    SIZE 84 BY .95 AT ROW 1.95 COL 4
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
    SIZE 76 BY .95 AT ROW 2.91 COL 8
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "" VIEW-AS TEXT
    SIZE 7 BY .95 AT ROW 2.91 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 3 BY .95 AT ROW 1.95 COL 1
    BGCOLOR 11 
    "" VIEW-AS TEXT
    SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
    BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 89.2 BY 3.81
    BGCOLOR 11 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Update Ship To Number"
        HEIGHT             = 21.76
        WIDTH              = 90.2
        MAX-HEIGHT         = 21.76
        MAX-WIDTH          = 98.2
        VIRTUAL-HEIGHT     = 21.76
        VIRTUAL-WIDTH      = 98.2
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB browse-machine rd_active FRAME-A */
ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto
     NO-LOCK BY tt-oe-shipto.ship-id.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Ship To Number */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Ship To Number */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DEFINE VARIABLE v-process AS LOG NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ begin_cust
            NO-ERROR .

        IF NOT AVAILABLE cust THEN 
        DO:
            MESSAGE "You must enter a valid customer number" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_cust.
            RETURN NO-APPLY.
        END.

        FIND FIRST bf-shipto NO-LOCK
            WHERE bf-shipto.company EQ cocode
            AND bf-shipto.cust-no EQ cust.cust-no
            AND bf-shipto.ship-id EQ fi_ship-id:SCREEN-VALUE
            NO-ERROR.

        IF NOT AVAILABLE bf-shipto THEN 
        DO:
            MESSAGE "You must enter a valid Ship Id" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_ship-id.
            RETURN NO-APPLY.
        END.

        i = 0 .
        FOR EACH tt-oe-shipto WHERE tt-oe-shipto.IS-SELECTED:
            i = i + 1.
        END.

        IF i = 0  THEN 
        DO:
            MESSAGE "Please Select Record ..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
            RETURN .
        END.


        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE)
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-show C-Win
ON CHOOSE OF btn-show IN FRAME FRAME-A /* Show All ShipTo */
    DO:

        DO WITH FRAME {&FRAME-NAME}:

    
            ASSIGN fi_ship-id.
        
            CLOSE QUERY browse-machine.
            RUN build-table. 
            OPEN QUERY browse-machine FOR EACH tt-oe-shipto  NO-LOCK BY tt-oe-shipto.ship-id.

        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON HELP OF fi_ship-id IN FRAME FRAME-A /* Create BOL to Ship To */
    DO:
        DEFINE VARIABLE char-val AS cha   NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.
        ASSIGN fi_ship-id .
        RUN windows/l-shipto.w (g_company,g_loc,begin_cust:screen-value,fi_ship-id,OUTPUT char-val).
        IF char-val <> "" THEN
            ASSIGN fi_ship-id:SCREEN-VALUE = ENTRY(1,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON LEAVE OF rd_active IN FRAME FRAME-A
    DO:
        ASSIGN rd_active.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON VALUE-CHANGED OF rd_active IN FRAME FRAME-A
    DO:
    /*{custom/chgfont.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON LEAVE OF fi_ship-id IN FRAME FRAME-A
    DO:
        DO WITH FRAME {&FRAME-NAME}:
      
            ASSIGN fi_ship-id.
      
            CLOSE QUERY browse-machine.
            RUN build-table. 
            OPEN QUERY browse-machine FOR EACH tt-oe-shipto  NO-LOCK BY tt-oe-shipto.ship-id.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define BROWSE-NAME browse-machine
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN enable_UI.
    {methods/nowait.i}
    {custom/usrprint.i}

    CLOSE QUERY browse-machine.
    RUN build-table. 
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY browse-machine FOR EACH tt-oe-shipto
            NO-LOCK BY tt-oe-shipto.ship-id.
    END.


    /* DO WITH FRAME {&FRAME-NAME}:
        APPLY "entry" TO fi_ship-id.
      END.*/
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table C-Win 
PROCEDURE build-table :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        EMPTY TEMP-TABLE tt-oe-shipto .     
        FOR EACH shipto NO-LOCK
            WHERE shipto.company = cocode  
            AND shipto.ship-id NE fi_ship-id:SCREEN-VALUE 
            AND shipto.cust-no = begin_cust:screen-value 
            :
            FIND FIRST tt-oe-shipto WHERE tt-oe-shipto.tt-recid = RECID(shipto)
                NO-ERROR.
            IF NOT AVAILABLE tt-oe-shipto THEN 
            DO:
                CREATE tt-oe-shipto.
                ASSIGN 
                    tt-oe-shipto.tt-recid  = RECID(shipto)
                    tt-oe-shipto.ship-id   = shipto.ship-id  
                    tt-oe-shipto.ship-name = shipto.ship-name
                    tt-oe-shipto.ship-add  = shipto.ship-add[1]
                    tt-oe-shipto.ship-city = shipto.ship-city.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY begin_cust fi_ship-id rd_active 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 RECT-18 begin_cust fi_ship-id btn-show rd_active 
        btn-process btn-cancel browse-machine
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-B IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :

    DEFINE VARIABLE lRelease AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBol     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOrder   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInv     AS LOGICAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ begin_cust
        NO-ERROR .
  
    FIND FIRST bf-shipto NO-LOCK
        WHERE bf-shipto.company EQ cocode
        AND bf-shipto.cust-no EQ cust.cust-no
        AND bf-shipto.ship-id EQ fi_ship-id:SCREEN-VALUE
        NO-ERROR.
   
    RUN util/shipupd.w (OUTPUT lRelease ,OUTPUT lBol,OUTPUT lOrder,OUTPUT lInv ).

    IF fi_ship-id NE "" THEN 
    DO:

        FOR EACH tt-oe-shipto WHERE tt-oe-shipto.IS-SELECTED:
            FOR EACH shipto EXCLUSIVE-LOCK 
                WHERE shipto.company EQ cocode AND 
                recid(shipto) EQ tt-oe-shipto.tt-recid :
              
              
                FIND FIRST eb
                    WHERE eb.company EQ cocode
                    USE-INDEX cust NO-LOCK NO-ERROR.
            
                DO WHILE AVAILABLE eb:
                    cChar = eb.loc.
            
                    FOR EACH eb
                        WHERE eb.company EQ cocode
                        AND eb.loc     EQ cChar
                        AND eb.cust-no EQ cust.cust-no
                        AND eb.ship-id EQ shipto.ship-id
                        USE-INDEX cust
            
                        TRANSACTION:
            
                        eb.ship-id = bf-shipto.ship-id .
                        {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Estimate # ' + string(eb.est-no) "}
            
                        ASSIGN
                            eb.ship-no      = bf-shipto.ship-no
                            eb.ship-name    = bf-shipto.ship-name
                            eb.ship-addr[1] = bf-shipto.ship-addr[1]
                            eb.ship-addr[2] = bf-shipto.ship-addr[2]
                            eb.ship-city    = bf-shipto.ship-city
                            eb.ship-state   = bf-shipto.ship-state
                            eb.ship-zip     = bf-shipto.ship-zip.
                    END.
            
                    RELEASE eb.
            
                    FIND FIRST eb
                        WHERE eb.company EQ cocode
                        AND eb.loc     GT cChar
                        USE-INDEX cust NO-LOCK NO-ERROR.
                END.
            
                FOR EACH quotehd
                    WHERE quotehd.company EQ cocode
                    AND quotehd.cust-no EQ cust.cust-no
                    AND quotehd.ship-id EQ shipto.ship-id
            
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Quote # ' + string(quotehd.q-no) "}
            
                    quotehd.ship-id = bf-shipto.ship-id.
            
                    ASSIGN
                        quotehd.shipto[1] = bf-shipto.ship-name
                        quotehd.shipto[2] = bf-shipto.ship-addr[1]
                        quotehd.shipto[3] = bf-shipto.ship-addr[2]
                        quotehd.shipto[4] = bf-shipto.ship-city + ", " + bf-shipto.ship-state +
                                     "  " + bf-shipto.ship-zip.
                END.
            
                FOR EACH oe-rel
                    WHERE oe-rel.company EQ cocode
                    AND oe-rel.cust-no EQ cust.cust-no
                    AND oe-rel.ship-id EQ shipto.ship-id
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) "}
            
                    oe-rel.ship-id = bf-shipto.ship-id .
            
                    oe-rel.ship-no = bf-shipto.ship-no .
                END.
            
                FOR EACH oe-relh
                    WHERE oe-relh.company EQ cocode
                    AND (oe-relh.posted  EQ NO OR lRelease)
                    AND oe-relh.cust-no EQ cust.cust-no
                    AND oe-relh.ship-id EQ shipto.ship-id
                    USE-INDEX post
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Ship To # ' + string(shipto.ship-id) + ' Release # ' + string(oe-relh.rel-no) "}
            
                    oe-relh.ship-id = bf-shipto.ship-id.
            
                    oe-relh.ship-no = bf-shipto.ship-no.
                END.
            
                FOR EACH oe-bolh
                    WHERE oe-bolh.company EQ cocode
                    AND (oe-bolh.posted  EQ NO OR lBol)
                    AND oe-bolh.cust-no EQ cust.cust-no 
                    AND oe-bolh.ship-id EQ shipto.ship-id
                    USE-INDEX post
            
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Bol # ' + string(oe-bolh.bol-no) "}
            
                    oe-bolh.ship-id = bf-shipto.ship-id.
            
                    oe-bolh.ship-no = bf-shipto.ship-no.
                END.
            
                FOR EACH ar-inv
                    WHERE ar-inv.company EQ cocode
                    AND (ar-inv.posted  EQ NO OR lInv)
                    AND ar-inv.cust-no EQ cust.cust-no
                    AND ar-inv.ship-id EQ shipto.ship-id
                    USE-INDEX posted
            
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Invoice # ' + string(ar-inv.inv-no) "}
            
                    ar-inv.ship-id = bf-shipto.ship-id.
                END.
            
                FOR EACH oe-ord
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.cust-no EQ cust.cust-no
                    AND oe-ord.ship-id EQ shipto.ship-id
                    AND (oe-ord.opened EQ YES OR lOrder )
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Order # ' + string(oe-ord.ord-no) "}
               
                    oe-ord.ship-id = bf-shipto.ship-id .
                                                  
                END.
            
                DO TRANSACTION:
                    IF rd_active EQ "I" AND NOT DYNAMIC-FUNCTION("IsActive",shipto.rec_key) THEN 
                    DO:
                        RUN ClearTagsInactive(shipto.rec_key).
                        shipto.statusCode = "".
                    END.
                    ELSE IF rd_active EQ "D" THEN 
                        DO:
                            DELETE shipto .
                        END.
                END.

            END.  /* for each shipto */
        END.  /* for each tt-oe-shipto */
    END.  /* fi_ship-id ne "" */  
    RELEASE shipto .
    RELEASE oe-ord .
    RELEASE eb .
    RELEASE ar-inv .
    RELEASE oe-bolh .
    RELEASE oe-relh .
    RELEASE quotehd .
    RELEASE oe-rel .

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE("").

    MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

    RETURN NO-APPLY.
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
