&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ap\b-reconc.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE yellowColumnsName b-reconc
{methods/defines/winReSize.i}


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{methods/defines/hndlset.i}
{sys/inc/var.i NEW SHARED}

{ap/reconcil.i NEW}

&SCOPED-DEFINE BRWSDEFS reconcile

/* gdm - */
DEFINE VARIABLE v-can-update            AS LOG       NO-UNDO.
DEFINE VARIABLE v-called-setCellColumns AS LOG       NO-UNDO.
DEFINE VARIABLE v-col-move              AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE lv-save-char            AS CHARACTER INIT "" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES reconcile

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-number tt-date tt-amt tt-bank tt-vend tt-name tt-cleared   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-cleared   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table reconcile
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~
AND (reconcile.tt-bank EQ fi_bank-code OR fi_bank-code EQ "") ~
AND (reconcile.tt-date GE fi_start-date OR fi_start-date EQ ?) ~
AND (reconcile.tt-date LE fi_end-date OR fi_end-date EQ ?) ~
AND (reconcile.tt-number EQ fi_check OR fi_check EQ "") ~
AND (reconcile.tt-vend EQ fi_vend OR fi_vend EQ "") ~
AND (reconcile.tt-cleared EQ logical(rd-drop-list) OR rd-drop-list EQ "All") ~
 ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~
AND (reconcile.tt-bank = fi_bank-code  OR fi_bank-code EQ "") ~
AND (reconcile.tt-date GE fi_start-date OR fi_start-date EQ ?) ~
AND (reconcile.tt-date LE fi_end-date OR fi_end-date EQ ?) ~
AND (reconcile.tt-number EQ fi_check OR fi_check EQ "") ~
AND (reconcile.tt-vend EQ fi_vend OR fi_vend EQ "") ~
AND (reconcile.tt-cleared EQ logical(rd-drop-list) OR rd-drop-list EQ "All") ~
~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table reconcile
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table reconcile


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btn_go btn_clear fi_bank-code ~
fi_start-date fi_check fi_vend fi_end-date rd-drop-list Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_bank-code fi_bank-name fi_start-date ~
fi_check fi_vend fi_name fi_end-date rd-drop-list fi_sortBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_clear 
    LABEL "Clear" 
    SIZE 12 BY 1.

DEFINE BUTTON btn_go 
    LABEL "Search" 
    SIZE 12 BY 1.

DEFINE VARIABLE fi_bank-code  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Bank Code" 
    VIEW-AS FILL-IN 
    SIZE 16.2 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_bank-name  AS CHARACTER FORMAT "x(30)" 
    VIEW-AS FILL-IN 
    SIZE 31 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_check      AS CHARACTER FORMAT "X(13)":U 
    LABEL "C/J #" 
    VIEW-AS FILL-IN 
    SIZE 18.8 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_end-date   AS DATE      FORMAT "99/99/9999":U 
    LABEL "End Date" 
    VIEW-AS FILL-IN 
    SIZE 16.2 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_name       AS CHARACTER FORMAT "x(30)" 
    VIEW-AS FILL-IN 
    SIZE 31 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_sortBy     AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 28 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_start-date AS DATE      FORMAT "99/99/9999":U 
    LABEL "Start Date" 
    VIEW-AS FILL-IN 
    SIZE 16.2 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_vend       AS CHARACTER FORMAT "X(8)":U 
    LABEL "Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16.2 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 145 BY 20.48.

DEFINE VARIABLE rd-drop-list AS CHARACTER FORMAT "X(256)":U 
    LABEL "Cleared" 
    VIEW-AS COMBO-BOX INNER-LINES 3
    LIST-ITEM-PAIRS "All","All",
    "Yes","Yes",
    "No","No"
    DROP-DOWN-LIST
    SIZE 9 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
    reconcile SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
    QUERY Browser-Table NO-LOCK DISPLAY
    tt-number             FORMAT "x(13)"      LABEL "Check/Journal#" LABEL-BGCOLOR 14
    tt-date                                   LABEL "Trans Date" LABEL-BGCOLOR 14
    tt-amt                                    LABEL "Amount" LABEL-BGCOLOR 14
    tt-bank                                   LABEL "Bank" LABEL-BGCOLOR 14
    tt-vend                                   LABEL "Vendor#" LABEL-BGCOLOR 14
    tt-name                                   LABEL "Name" LABEL-BGCOLOR 14
    tt-cleared                                LABEL "Cleared?" LABEL-BGCOLOR 14
   ENABLE tt-cleared
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 17
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btn_go AT ROW 1.24 COL 117.4 WIDGET-ID 16
    btn_clear AT ROW 1.24 COL 131 WIDGET-ID 32
    fi_bank-code AT ROW 1.33 COL 11.6 COLON-ALIGNED WIDGET-ID 24
    fi_bank-name AT ROW 1.33 COL 27.8 COLON-ALIGNED HELP
    "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABELS WIDGET-ID 22
    fi_start-date AT ROW 1.33 COL 70.6 COLON-ALIGNED WIDGET-ID 26
    fi_check AT ROW 1.33 COL 94.2 COLON-ALIGNED WIDGET-ID 30
    fi_vend AT ROW 2.62 COL 11.4 COLON-ALIGNED WIDGET-ID 20
    fi_name AT ROW 2.62 COL 27.6 COLON-ALIGNED HELP
    "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABELS WIDGET-ID 18
    fi_end-date AT ROW 2.62 COL 70.6 COLON-ALIGNED WIDGET-ID 28
    rd-drop-list AT ROW 2.67 COL 97 COLON-ALIGNED NO-LABELS
    fi_sortBy AT ROW 2.67 COL 115 COLON-ALIGNED NO-LABELS WIDGET-ID 12
    Browser-Table AT ROW 4.1 COL 1 HELP
    "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    "Sort:" VIEW-AS TEXT
    SIZE 5 BY .95 AT ROW 2.71 COL 112 WIDGET-ID 14
    "Cleared:" VIEW-AS TEXT
    SIZE 9 BY .95 AT ROW 2.67 COL 90 
    RECT-1 AT ROW 1 COL 1 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 20.86
         WIDTH              = 145.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */
{src/adm/method/navbrows.i}
{custom/yellowColumns.i}
/*{src/adm/method/browser.i}
{src/adm/method/query.i}*/
/*{methods/template/browser.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sortBy F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

ASSIGN 
    Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_bank-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH reconcile WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Query            is OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
    DO:  
        /* This code displays initial values for newly added or copied rows. */
        {src/adm/template/brsentry.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
    DO:
        /* Do not disable this code or no updates will take place except
         by pressing the Save button on an Update SmartPanel. */
        /*{src/adm/template/brsleave.i}*/
        /*{brsleave.i}*/
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
    DO:
        RUN startSearch.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
    DO:
        /* This ADM trigger code must be preserved in order to notify other
           objects when the browser's current row changes. */
     
        {src/adm/template/brschnge.i}
    /*{methods/template/local/setvalue.i}*/
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_clear B-table-Win
ON CHOOSE OF btn_clear IN FRAME F-Main /* Clear */
    DO:

 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fi_bank-code:SCREEN-VALUE  = ""
                fi_vend:SCREEN-VALUE       = ""
                fi_start-date:SCREEN-VALUE = ""
                fi_end-date:SCREEN-VALUE   = ""
                fi_check:SCREEN-VALUE      = ""
                fi_bank-name:SCREEN-VALUE  = ""
                fi_name:SCREEN-VALUE       = "" .
            rd-drop-list:SCREEN-VALUE = "All" .


            ASSIGN
                fi_bank-code
                fi_vend
                fi_start-date
                fi_end-date
                fi_check
                fi_bank-name
                fi_name 
                rd-drop-list.
        END.

        RUN dispatch ("open-query").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Search */
    DO:
 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fi_bank-code
                fi_vend
                fi_start-date
                fi_end-date
                fi_check
                fi_bank-name
                fi_name 
                rd-drop-list.
        END.

        RUN dispatch ("open-query").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bank-code B-table-Win
ON ENTRY OF fi_bank-code IN FRAME F-Main /* Bank Code */
    DO:
        IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN RUN new-bank.

        lv-save-char = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bank-code B-table-Win
ON LEAVE OF fi_bank-code IN FRAME F-Main /* Bank Code */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN new-bank.

            RUN valid-bank NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bank-code B-table-Win
ON VALUE-CHANGED OF fi_bank-code IN FRAME F-Main /* Bank Code */
    DO:
        RUN new-bank.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_check
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_check B-table-Win
ON LEAVE OF fi_check IN FRAME F-Main /* C/J # */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
   
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_end-date B-table-Win
ON LEAVE OF fi_end-date IN FRAME F-Main /* End Date */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
    
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_start-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_start-date B-table-Win
ON LEAVE OF fi_start-date IN FRAME F-Main /* Start Date */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
   
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON ENTRY OF fi_vend IN FRAME F-Main /* Vendor# */
    DO:
        IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN RUN new-vend.

        lv-save-char = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON LEAVE OF fi_vend IN FRAME F-Main /* Vendor# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN new-vend.

            RUN valid-vend NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON VALUE-CHANGED OF fi_vend IN FRAME F-Main /* Vendor# */
    DO:
        RUN new-vend.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&SCOPED-DEFINE cellColumnDat b-reconc
{methods/browsers/setCellColumns.i}
  
DEFINE BUFFER b-ap-pay FOR ap-pay.

/*     Task 12111502 */
ON MOUSE-SELECT-CLICK OF tt-cleared IN BROWSE Browser-Table /* Reconciled */
    DO:
        DEFINE VARIABLE lv-bank LIKE ar-cash.bank-code NO-UNDO.


        IF tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes" THEN
            tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "No".
        ELSE
            tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".

        IF AVAILABLE reconcile THEN 
        DO:
            FIND CURRENT reconcile NO-LOCK NO-ERROR.
            tt-cleared = tt-cleared:SCREEN-VALUE IN BROWSE {&browse-name} EQ "Yes".

            IF tt-type EQ 1 THEN 
            DO /*TRANSACTION*/ :
                RELEASE b-ap-pay.

                FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.

                IF AVAILABLE ap-pay THEN
                    IF ap-pay.d-no NE 0                                              AND
                        NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN 
                    DO:
                        FIND FIRST b-ap-pay
                            WHERE b-ap-pay.company   EQ ap-pay.company
                            AND b-ap-pay.check-act EQ ap-pay.check-act
                            AND b-ap-pay.check-no  EQ ap-pay.d-no
                            EXCLUSIVE-LOCK NO-ERROR.
                    END.

                    ELSE FIND b-ap-pay WHERE ROWID(b-ap-pay) EQ ROWID(ap-pay) EXCLUSIVE-LOCK NO-ERROR.

                IF AVAILABLE b-ap-pay THEN 
                DO:
                    b-ap-pay.cleared = tt-cleared.

                    FOR EACH ap-pay
                        WHERE ap-pay.company EQ b-ap-pay.company
                        AND ap-pay.d-no    EQ b-ap-pay.check-no
                        AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
                        EXCLUSIVE-LOCK
                        USE-INDEX d-no:
                        ap-pay.cleared = b-ap-pay.cleared.
                    END.
                END.
            END. /*tt-type eq 1*/

            ELSE
                IF tt-type EQ 2 THEN 
                DO:
                    FIND ar-cash WHERE ROWID(ar-cash) EQ tt-rowid NO-LOCK NO-ERROR.
                    IF AVAILABLE ar-cash THEN 
                    DO:
                        lv-bank = ar-cash.bank-code.
                        RELEASE ar-cash.
                        FOR EACH tt-cash
                            WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10))
                            USE-INDEX tt-trnum,
                            FIRST ar-cash EXCLUSIVE-LOCK
                            WHERE ROWID(ar-cash)     EQ tt-cash.row-id
                            AND ar-cash.reconciled EQ NO
                            AND ar-cash.posted     EQ YES
                            AND ar-cash.memo       EQ NO
                            AND ar-cash.bank-code  EQ lv-bank,
                            FIRST bank NO-LOCK
                            WHERE bank.company   EQ ar-cash.company
                            AND bank.bank-code EQ ar-cash.bank-code
                            USE-INDEX bank
                            /*TRANSACTION*/ :
                            ar-cash.cleared = tt-cleared.
                        END.
                    END.
                END.

                ELSE
                    IF tt-type EQ 3 THEN 
                    DO /*TRANSACTION*/ :
                        FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE gl-jrn THEN gl-jrn.cleared = tt-cleared.
                    END.

                    ELSE
                        IF tt-type EQ 4 THEN
                            FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
                                FIRST ar-mcash-ref
                                WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
                                AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
                                AND ar-mcash-ref.company  EQ "ar-mcash"
                                USE-INDEX rec_key
                                /*TRANSACTION*/ EXCLUSIVE-LOCK:
                                ar-mcash-ref.val[2] = INT(tt-cleared).
                            END.
        END.

        RETURN NO-APPLY.
    END.

ASSIGN
    cocode = g_company
    locode = g_loc.

RUN ap/reconcil.p.

/* gdm - */
RUN get-security.

DO WITH FRAME {&FRAME-NAME}:
    rd-drop-list:SCREEN-VALUE = "All" .
    rd-drop-list = "All" .
    APPLY "entry" TO fi_bank-code .
END.


ON 'ENTRY':U OF reconcile.tt-cleared
    DO:
        IF LASTKEY NE -1 AND NOT v-can-update THEN
        DO:       
            APPLY "TAB" TO BROWSE {&browse-name}.
            RETURN NO-APPLY .
        END.
    END.
/* gdm - */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-security B-table-Win 
PROCEDURE get-security :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-prgrms FOR prgrms.

    DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
    DEFINE VARIABLE v-dirname    LIKE b-prgrms.DIR_group NO-UNDO.
    DEFINE VARIABLE Audit_File   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE period_pos   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE num-groups   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE group-ok     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE access-close AS LOGICAL   NO-UNDO.

    ASSIGN
        v-prgmname = "w-reconl."
        v-dirname  = "ap".

    FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname AND
        b-prgrms.DIR_group = v-dirname NO-LOCK NO-ERROR.

    IF NOT AVAILABLE b-prgrms THEN 
        FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.

    IF AVAILABLE b-prgrms THEN 
    DO:    

        DO num-groups = 1 TO NUM-ENTRIES(g_groups):
            IF NOT CAN-DO(TRIM(b-prgrms.can_run),ENTRY(num-groups,g_groups)) AND
                NOT CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups)) AND
                NOT CAN-DO(TRIM(b-prgrms.can_create),ENTRY(num-groups,g_groups)) AND
                NOT CAN-DO(TRIM(b-prgrms.can_delete),ENTRY(num-groups,g_groups)) THEN
                NEXT.

    
            IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups))
                THEN v-can-update = YES.
    

            group-ok = YES.
        /*LEAVE. */
        END.
        IF NOT CAN-DO(TRIM(b-prgrms.can_run),USERID("ASI")) AND
            NOT CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI")) AND
            NOT CAN-DO(TRIM(b-prgrms.can_create),USERID("ASI")) AND
            NOT CAN-DO(TRIM(b-prgrms.can_delete),USERID("ASI")) AND NOT group-ok THEN
        DO:
            MESSAGE "Program :" PROGRAM-NAME(1) SKIP 
                "Title :" b-prgrms.prgtitle SKIP(1)
                "Access to this Program Denied - Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.

            access-close = YES.  /* used later in methods/template/windows.i - local-initialize procedure */

        END.
        ELSE 
        DO:
            IF NOT v-can-update AND CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI"))
                THEN v-can-update = YES.      
        END.
    END. 
    ELSE
    DO: 
        MESSAGE "Program :" PROGRAM-NAME(1) SKIP(1)
            "Program Master Record Does Not Exist - Contact Systems Manager" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    {methods/winReSizeLocInit.i}
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF v-called-setCellColumns = NO THEN 
    DO:
     
        RUN setCellColumns.
        v-called-setCellColumns = YES.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    RUN dispatch ('row-changed').

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&BROWSE-NAME}:COLUMN-MOVABLE   = v-col-move
            {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
            v-col-move                      = NOT v-col-move .
        fi_sortBy = IF v-col-move = NO THEN "Move" ELSE "Sort".
        DISPLAY fi_sortBy. 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bank B-table-Win 
PROCEDURE new-bank :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bank NO-LOCK
            WHERE bank.company EQ cocode
            AND bank.bank-code EQ fi_bank-code:SCREEN-VALUE
            NO-ERROR.
        IF AVAILABLE bank THEN fi_bank-name:SCREEN-VALUE = bank.bank-name.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend B-table-Win 
PROCEDURE new-vend :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ cocode
            AND vend.vend-no EQ fi_vend:SCREEN-VALUE
            NO-ERROR.
        IF AVAILABLE vend THEN fi_name:SCREEN-VALUE = vend.name.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
    /*------------------------------------------------------------------------------
      Purpose:     Send record ROWID's for all tables used by
                   this file.
      Parameters:  see template/snd-head.i
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.               */
    {src/adm/template/snd-head.i}

    /* For each requested table, put it's ROWID in the output list.      */
    {src/adm/template/snd-list.i "reconcile"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
        {src/adm/template/bstates.i}
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bank B-table-Win 
PROCEDURE valid-bank :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
    
        IF fi_bank-code:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST bank WHERE bank.company EQ cocode
            AND bank.bank-code EQ fi_bank-code:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_bank-code.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend B-table-Win 
PROCEDURE valid-vend :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        fi_vend:SCREEN-VALUE = CAPS(fi_vend:SCREEN-VALUE).

        IF fi_vend:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST vend WHERE vend.company EQ cocode
            AND vend.vend-no EQ fi_vend:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_vend.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend create-tempfile 
PROCEDURE create-tempfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  

    RUN dispatch ('row-changed').

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

