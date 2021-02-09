&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttGlAccBalance NO-UNDO 
    FIELD iYear      AS INTEGER   LABEL "Year" FORMAT ">>>>>"
    FIELD period     AS INTEGER   LABEL "Period" FORMAT ">>"
    FIELD perRange   AS CHARACTER LABEL "Period Range" FORMAT "x(22)" 
    FIELD perChange  AS DECIMAL   LABEL "Period Change" FORMAT "->>>,>>>,>>9.99"
    FIELD endBalance AS DECIMAL   LABEL "Ending Balance" FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD cStatus    AS CHARACTER LABEL "Status"
    .
       
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttGlAccBalance

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttGlAccBalance.iYear ttGlAccBalance.period ttGlAccBalance.perRange ttGlAccBalance.perChange ttGlAccBalance.endBalance ttGlAccBalance.cStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttGlAccBalance                             BY ttGlAccBalance.period
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttGlAccBalance                             BY ttGlAccBalance.period.
&Scoped-define TABLES-IN-QUERY-br_table ttGlAccBalance
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttGlAccBalance


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS begin_acct opening_Balance 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 begin_acct opening_Balance 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_acct      AS CHARACTER FORMAT "X(25)":U 
    LABEL "Account Number" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE opening_Balance AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
    LABEL "Opening Balance" 
    VIEW-AS FILL-IN 
    SIZE 36 BY 1
    BGCOLOR 15 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
    ttGlAccBalance SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    ttGlAccBalance.iYear         
    ttGlAccBalance.period     
    ttGlAccBalance.perRange   
    ttGlAccBalance.perChange 
    ttGlAccBalance.endBalance 
    ttGlAccBalance.cStatus
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 15.95
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    begin_acct AT ROW 1.95 COL 24 COLON-ALIGNED WIDGET-ID 2
    opening_Balance AT ROW 1.95 COL 81 COLON-ALIGNED WIDGET-ID 4
    br_table AT ROW 3.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 18.71
         WIDTH              = 143.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table opening_Balance F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN begin_acct IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN opening_Balance IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttGlAccBalance
                            BY ttGlAccBalance.period.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
    DO:
        /* This code displays initial values for newly added or copied rows. */
        {src/adm/template/brsentry.i}  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
    DO:
        /* Do not disable this code or no updates will take place except
         by pressing the Save button on an Update SmartPanel. */
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
    DO:
        /* This ADM trigger code must be preserved in order to notify other
           objects when the browser's current row changes. */
        {src/adm/template/brschnge.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-inquiry B-table-Win 
PROCEDURE build-inquiry :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCompany          LIKE glhist.company NO-UNDO.
    DEFINE VARIABLE cActNum           LIKE glhist.actnum NO-UNDO.   
    DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtStartPeriodDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndPeriodDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dPeriodAccBalance AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalAccBalance  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE opiYear           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE tmp-start         AS DATE      NO-UNDO.


    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'bal-no-source':U,OUTPUT char-hdl).
    DO li = 1 TO NUM-ENTRIES(char-hdl):
        IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
            RUN get-account IN WIDGET-HANDLE(ENTRY(li,char-hdl)) (OUTPUT cCompany,
                OUTPUT cActNum,
                OUTPUT opiYear). 
    END.

    EMPTY TEMP-TABLE ttGlAccBalance.
    begin_acct = "".
    opening_Balance = 0.
    dTotalAccBalance = 0.
    FIND FIRST account NO-LOCK
        WHERE account.company EQ cCompany
        AND account.actnum EQ cActNum NO-ERROR .
        
    IF AVAILABLE account THEN
    DO:
        FIND FIRST period WHERE period.company = cCompany
                      AND period.yr = opiYear  NO-LOCK NO-ERROR.
        tmp-start = IF AVAIL period THEN period.pst ELSE 01/01/01 .
        
        RUN gl/gl-opend.p (ROWID(account), tmp-start, OUTPUT opening_Balance).
                          
        begin_acct:SCREEN-VALUE IN FRAME {&FRAME-NAME} = account.actnum.
        opening_Balance:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(opening_Balance) /*STRING(account.cyr-open)*/.
        begin_acct = account.actnum.
        .
                     
        dTotalAccBalance = opening_Balance. 
              
        DO iCount = 1 TO 12:
           
            FIND FIRST period NO-LOCK                  
                WHERE period.company EQ cCompany
                AND period.yr     EQ opiYear 
                AND period.pnum   EQ iCount NO-ERROR .                
              
            IF AVAILABLE period THEN
            DO:
                dPeriodAccBalance = 0.
                FOR EACH glhist NO-LOCK
                    WHERE glhist.company EQ cCompany
                    AND glhist.actnum  EQ account.actnum
                    AND glhist.tr-date GE period.pst
                    AND glhist.tr-date LE period.pend:
                        
                    dPeriodAccBalance = dPeriodAccBalance + glhist.tr-amt.   
                END.
                                                
                dTotalAccBalance = dTotalAccBalance + dPeriodAccBalance.
               
                CREATE ttGlAccBalance .
                IF iCount EQ 1 THEN
                    ttGlAccBalance.iYear    = opiYear .
                ASSIGN                        
                    ttGlAccBalance.period     = period.pnum
                    ttGlAccBalance.perRange   = STRING(period.pst) + " - " + STRING(period.pend) 
                    ttGlAccBalance.perChange  = dPeriodAccBalance
                    ttGlAccBalance.endBalance = dTotalAccBalance 
                    ttGlAccBalance.cStatus    = IF period.pstat EQ TRUE THEN "Open" ELSE "Close" .                   
            END.
        END.
    END.    
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute AFTER standard behavior.    */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.   */
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    {methods/winReSizeLocInit.i}

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
    RUN build-inquiry.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

/* Code placed here will execute AFTER standard behavior.    */

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
    {src/adm/template/snd-list.i "ttGlAccBalance"}

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

