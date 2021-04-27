&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipiTrnum AS INTEGER NO-UNDO .    
/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

{methods/template/brwcustomdef.i}

DEFINE VARIABLE cColumnLabel   AS CHARACTER NO-UNDO .
DEFINE VARIABLE lv-sort-by     AS cha       NO-UNDO.
DEFINE VARIABLE ll-sort-asc    AS LOG       NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab AS cha       NO-UNDO.

ASSIGN 
    cocode = g_company
    locode = g_loc.
    
DEFINE TEMP-TABLE tt-glinq NO-UNDO
    FIELD tr-date     LIKE glhist.tr-date LABEL "Date"
    FIELD jrnl        LIKE glhist.jrnl LABEL "Ref#"
    FIELD tr-dscr     AS CHARACTER FORMAT "X(60)" LABEL "Description"
    FIELD tr-amt      LIKE glhist.tr-amt LABEL "Amount"
    FIELD db-amt      LIKE glhist.tr-amt LABEL "Debit Amount"
    FIELD cr-amt      LIKE glhist.tr-amt LABEL "Credit Amount"
    FIELD net-amt     LIKE glhist.tr-amt LABEL "Net Amount"
    FIELD tr-num      LIKE glhist.tr-num FORMAT "9999999" LABEL "Run #" 
    FIELD tr-from     AS cha       FORM "x(30)" LABEL "Inquiry From" 
    FIELD actnum      AS CHARACTER FORMAT "x(20)" LABEL "Account#"
    FIELD createdBy   LIKE glhist.createdBy LABEL "Created By"
    FIELD createdDate LIKE glhist.createdDate LABEL "Created Date"
    FIELD posted      LIKE glhist.posted LABEL "Posted"
    FIELD tr-period   LIKE glhist.period LABEL "Pd"
    FIELD tr-yr       LIKE glhist.yr LABEL "Year"
    FIELD documentID  LIKE glhist.documentID LABEL "Document Id"
    FIELD sourceDate  LIKE glhist.sourceDate LABEL "Source Date" 
    FIELD riRowid     AS ROWID 
    INDEX tr-date IS PRIMARY tr-date.
    
&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING    
    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-glinq

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt-glinq.actnum tt-glinq.tr-date tt-glinq.jrnl tt-glinq.tr-dscr tt-glinq.db-amt tt-glinq.cr-amt tt-glinq.documentID tt-glinq.sourceDate tt-glinq.createdBy tt-glinq.createdDate tt-glinq.posted tt-glinq.tr-num  tt-glinq.tr-period tt-glinq.tr-yr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 tt-glinq
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 tt-glinq
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-glinq NO-LOCK 
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH tt-glinq NO-LOCK  .
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-glinq
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-glinq


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 BROWSE-3 btn-cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

               
/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY
    LABEL "&Ok" 
    SIZE 14 BY 1.14.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 199 BY 16.50.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
    tt-glinq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _STRUCTURED
    QUERY BROWSE-3 NO-LOCK DISPLAY
    tt-glinq.actnum LABEL-BGCOLOR 14 WIDTH 18
    tt-glinq.tr-date LABEL-BGCOLOR 14
    tt-glinq.jrnl LABEL-BGCOLOR 14
    tt-glinq.tr-dscr FORM "X(60)" LABEL-BGCOLOR 14 WIDTH 50
    tt-glinq.db-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14
    tt-glinq.cr-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14      
    tt-glinq.sourceDate LABEL-BGCOLOR 14
    tt-glinq.createdBy  LABEL-BGCOLOR 14
    tt-glinq.createdDate LABEL-BGCOLOR 14
    tt-glinq.posted LABEL-BGCOLOR 14
    tt-glinq.tr-num FORMAT "9999999" LABEL-BGCOLOR 14  WIDTH 12
    tt-glinq.tr-period LABEL-BGCOLOR 14
    tt-glinq.tr-yr LABEL-BGCOLOR 14
    tt-glinq.documentID LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 198 BY 16.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    BROWSE-3 AT ROW 1.44 COL 2     
    btn-cancel AT ROW 12.24 COL 54.8
    RECT-17 AT ROW 1.19 COL 1.6
    SPACE(1.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "GL Inquiry".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-3 1 Dialog-Frame */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.        
        
ASSIGN 
    BROWSE-3:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame = 2.
BROWSE-3:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME 

 
        


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Utility: Restore deleted orders */
    DO:
       

        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON MOUSE-SELECT-CLICK OF BROWSE-3 IN FRAME Dialog-Frame
    DO:
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON ROW-LEAVE OF BROWSE-3 IN FRAME Dialog-Frame
    DO:
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-3 IN FRAME Dialog-Frame
    DO: 
    &scoped-define exclude-row-display true 
        {methods/template/brwrowdisplay.i}     
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON START-SEARCH OF BROWSE-3 IN FRAME Dialog-Frame
    DO:
        {methods/template/sortindicator.i} 
        DEFINE VARIABLE lh-column     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.

  
        ASSIGN
            lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
            lv-column-nam = lh-column:NAME
            lv-column-lab = lh-column:LABEL.

        IF lv-sort-by = lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

        ASSIGN
            lv-sort-by = lv-column-nam. 

        APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

        /*APPLY "choose" TO btn-inq. */
        CASE lv-column-nam:
            WHEN "actnum" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum {&sortby-des}.           
                END.
            WHEN "tr-date" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date {&sortby-des}.           
                END.
            WHEN "jrnl" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl {&sortby-des}.
                END.
            WHEN "tr-dscr" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr {&sortby-des}.
                END.
            WHEN "db-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.db-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.db-amt {&sortby-des}.
                END.      
            WHEN "net-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.net-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.net-amt {&sortby-des}.
                END.
            WHEN "cr-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.cr-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.cr-amt {&sortby-des}.
                END.
            WHEN "createdBy" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdBy.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdBy {&sortby-des}.
                END.
            WHEN "sourceDate" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.sourceDate.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.sourceDate {&sortby-des}.
                END.  
            WHEN "documentID" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.documentID.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.documentID {&sortby-des}.
                END. 
            WHEN "createdDate" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdDate.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdDate {&sortby-des}.
                END.
            WHEN "posted" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.posted.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.posted {&sortby-des}.
                END.
            WHEN "tr-num" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-num.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-num {&sortby-des}.
                END.  
            WHEN "tr-period" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-period.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-period {&sortby-des}.
                END.
            WHEN "tr-yr" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-yr.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-yr {&sortby-des}.
                END.
        END CASE.
        {methods/template/sortindicatorend.i} 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Delete */
    DO:
       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
{sys/inc/f3helpw.i}
{methods/template/brwcustom.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    CLOSE QUERY BROWSE-3.
    RUN build-table. 
    DO WITH FRAME {&FRAME-NAME}:
      
        btn-cancel:VISIBLE = FALSE. 
        OPEN QUERY BROWSE-3 FOR EACH tt-glinq
            NO-LOCK BY tt-glinq.tr-date.        
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
    ENABLE BROWSE-3 btn-cancel RECT-17 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
/*{&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
        
    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ cocode         
        AND glhist.tr-num EQ ipiTrnum           
        BY glhist.tr-date :                   
              
        CREATE tt-glinq.
        ASSIGN 
            tt-glinq.tr-date     = glhist.tr-date
            tt-glinq.jrnl        = glhist.jrnl
            tt-glinq.tr-dscr     = glhist.tr-dscr
            tt-glinq.tr-amt      = glhist.tr-amt
            tt-glinq.tr-from     = IF glhist.posted EQ YES THEN "GL History " ELSE "GL Transaction " + string(glhist.tr-num)
            tt-glinq.actnum      = glhist.actnum
            tt-glinq.tr-num      = glhist.tr-num               
            tt-glinq.createdBy   = glhist.createdBy
            tt-glinq.createdDate = glhist.createdDate
            tt-glinq.posted      = glhist.posted
            tt-glinq.tr-num      = glhist.tr-num
            tt-glinq.tr-period   = glhist.period
            tt-glinq.tr-yr       = glhist.glYear
            tt-glinq.documentID  = glhist.documentID
            tt-glinq.sourceDate  = glhist.sourceDate
            tt-glinq.riRowid     = ROWID(glhist)                  
            .                  
        IF glhist.tr-amt GT 0 THEN
            ASSIGN
                tt-glinq.db-amt = tt-glinq.db-amt + glhist.tr-amt.
           
        IF glhist.tr-amt LT 0 THEN
            ASSIGN
                tt-glinq.cr-amt = (- tt-glinq.cr-amt + glhist.tr-amt) * - 1
                .               
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Dialog-Frame 
PROCEDURE open-query :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
    ------------------------------------------------------------------------------*/
    
    CLOSE QUERY BROWSE-3.
    OPEN QUERY BROWSE-3 FOR EACH tt-glinq
        NO-LOCK BY tt-glinq.tr-num.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */



