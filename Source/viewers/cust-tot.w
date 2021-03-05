&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/cust-tot.w

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

&Scoped-define CUSTOMER-TOTALS yes
{custom/gperiod.i}
{custom/persist.i}
DEF VAR ll-secure AS LOG INIT NO NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES cust
&Scoped-define FIRST-EXTERNAL-TABLE cust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cust.ytd-sales cust.lyr-sales ~
cust.accountType cust.cost[1] cust.cost[5] cust.cost[6] cust.splitType ~
cust.parentCust cust.comm[1] cust.comm[5] cust.comm[6] cust.marketSegment ~
cust.ytd-msf cust.lyytd-msf cust.naicsCode cust.hibal cust.hibal-date ~
cust.num-inv cust.industryID cust.lpay cust.lpay-date cust.avg-pay ~
cust.ord-bal cust.acc-bal cust.on-account 
&Scoped-define ENABLED-TABLES cust
&Scoped-define FIRST-ENABLED-TABLE cust
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS cust.ytd-sales cust.lyr-sales ~
cust.accountType cust.cost[1] cust.cost[5] cust.cost[6] cust.splitType ~
cust.parentCust cust.comm[1] cust.comm[5] cust.comm[6] cust.marketSegment ~
cust.ytd-msf cust.lyytd-msf cust.naicsCode cust.hibal cust.hibal-date ~
cust.num-inv cust.industryID cust.lpay cust.lpay-date cust.avg-pay ~
cust.ord-bal cust.acc-bal cust.on-account 
&Scoped-define DISPLAYED-TABLES cust
&Scoped-define FIRST-DISPLAYED-TABLE cust
&Scoped-Define DISPLAYED-OBJECTS ptd-sales ptd-profit ytd-profit lyr-profit ~
ptd-profit-pct ytd-profit-pct lyr-profit-pct total-msf 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ptd-sales total-msf 
&Scoped-define DISPLAY-FIELD ptd-sales cust.ytd-sales cust.lyr-sales ~
cust.cost[1] cust.cost[5] cust.cost[6] 

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
DEFINE VARIABLE lyr-profit AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE lyr-profit-pct AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ptd-profit AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Profits" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ptd-profit-pct AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Profit Percent" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ptd-sales AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sales" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE total-msf AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total MSF" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ytd-profit AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ytd-profit-pct AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 13.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 13.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ptd-sales AT ROW 1.95 COL 26 COLON-ALIGNED HELP
          "Enter Period to Date Sales"
     cust.ytd-sales AT ROW 1.95 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.lyr-sales AT ROW 1.95 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.accountType AT ROW 2.86 COL 126 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEMS "<None>","Split","Originated","Handed" 
          DROP-DOWN-LIST
          SIZE 19 BY 1
          BGCOLOR 15 FONT 4
     cust.cost[1] AT ROW 3.14 COL 26 COLON-ALIGNED
          LABEL "Costs"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.cost[5] AT ROW 3.14 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.cost[6] AT ROW 3.14 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.splitType AT ROW 4.24 COL 126 COLON-ALIGNED FORMAT "9"
          VIEW-AS COMBO-BOX INNER-LINES 10
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9" 
          DROP-DOWN-LIST
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     ptd-profit AT ROW 4.33 COL 26 COLON-ALIGNED
     ytd-profit AT ROW 4.33 COL 51 COLON-ALIGNED NO-LABEL
     lyr-profit AT ROW 4.33 COL 75 COLON-ALIGNED NO-LABEL
     ptd-profit-pct AT ROW 5.52 COL 26 COLON-ALIGNED
     ytd-profit-pct AT ROW 5.52 COL 51 COLON-ALIGNED NO-LABEL
     lyr-profit-pct AT ROW 5.52 COL 75 COLON-ALIGNED NO-LABEL
     cust.parentCust AT ROW 5.62 COL 126 COLON-ALIGNED
          LABEL "Parent Customer" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 14.8 BY 1
          BGCOLOR 15 FONT 4
     cust.comm[1] AT ROW 6.71 COL 26 COLON-ALIGNED
          LABEL "Commissions"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.comm[5] AT ROW 6.71 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.comm[6] AT ROW 6.71 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.marketSegment AT ROW 7.05 COL 126 COLON-ALIGNED
          LABEL "Market Segment" FORMAT "x(16)"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
          BGCOLOR 15 FONT 4
     total-msf AT ROW 7.91 COL 26 COLON-ALIGNED HELP
          "Enter Total MSF"
     cust.ytd-msf AT ROW 7.91 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.lyytd-msf AT ROW 7.95 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.naicsCode AT ROW 8.57 COL 126 COLON-ALIGNED
          LABEL "NAICS Code" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     cust.hibal AT ROW 9.57 COL 26 COLON-ALIGNED
          LABEL "High Balance"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
          BGCOLOR 15 FONT 4
     cust.hibal-date AT ROW 9.57 COL 51 COLON-ALIGNED
          LABEL "On"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cust.num-inv AT ROW 9.57 COL 94 COLON-ALIGNED
          LABEL "Total# of Inv. Paid"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     cust.industryID AT ROW 10 COL 126 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     cust.lpay AT ROW 10.76 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
          BGCOLOR 15 FONT 4
     cust.lpay-date AT ROW 10.76 COL 51 COLON-ALIGNED
          LABEL "On"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4
     cust.avg-pay AT ROW 10.76 COL 94 COLON-ALIGNED
          LABEL "Avg# Days to Pay"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     cust.ord-bal AT ROW 12.43 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
          BGCOLOR 15 FONT 4
     cust.acc-bal AT ROW 12.43 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     cust.on-account AT ROW 13.62 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
          BGCOLOR 15 FONT 4
     "Reporting Data" VIEW-AS TEXT
          SIZE 21 BY 1.19 AT ROW 1.24 COL 123 WIDGET-ID 2
     "Period to Date" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.24 COL 30
     "Prior Year" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.24 COL 81
     "Year to Date" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 56
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 1 COL 106 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.cust
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
         HEIGHT             = 14.52
         WIDTH              = 150.8.
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

/* SETTINGS FOR FILL-IN cust.avg-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.comm[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.cost[1] IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN cust.cost[5] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.cost[6] IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.hibal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.hibal-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.lpay-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lyr-profit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lyr-profit-pct IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.lyr-sales IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.marketSegment IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.naicsCode IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN cust.num-inv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cust.parentCust IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ptd-profit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ptd-profit-pct IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ptd-sales IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR COMBO-BOX cust.splitType IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN total-msf IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ytd-profit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ytd-profit-pct IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.ytd-sales IN FRAME F-Main
   4                                                                    */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var lv-handle as handle no-undo.
   def var char-val as cha no-undo.
   DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
   DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

   CASE Focus:name :
     when "parentCust" then do:
           RUN system/openlookup.p (cust.company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN focus:SCREEN-VALUE in frame {&frame-name} = cMainField.               
           return no-apply.  
     end.
     when "naicsCode" then do:
          RUN system/openlookup.p (cust.company, "naics", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN focus:SCREEN-VALUE in frame {&frame-name} = cMainField.
           return no-apply.  
     end.     
  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cost[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cost[1] V-table-Win
ON LEAVE OF cust.cost[1] IN FRAME F-Main /* Costs */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cost[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cost[5] V-table-Win
ON LEAVE OF cust.cost[5] IN FRAME F-Main /* Costs[5] */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.cost[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.cost[6] V-table-Win
ON LEAVE OF cust.cost[6] IN FRAME F-Main /* Costs[6] */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.industryID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.industryID V-table-Win
ON LEAVE OF cust.industryID IN FRAME F-Main /* Industry */
DO:
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
    
    IF LASTKEY NE -1 THEN DO:
        RUN valid-industry(OUTPUT lCheckReturnError) NO-ERROR.
        IF lCheckReturnError THEN 
            RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.lyr-sales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.lyr-sales V-table-Win
ON LEAVE OF cust.lyr-sales IN FRAME F-Main /* LYR Sales */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.naicsCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.naicsCode V-table-Win
ON LEAVE OF cust.naicsCode IN FRAME F-Main /* NAICS Code */
DO:
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-naics(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.parentCust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.parentCust V-table-Win
ON LEAVE OF cust.parentCust IN FRAME F-Main /* Parent Customer */
DO:
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-cust(OUTPUT lCheckError) NO-ERROR.
            IF lCheckError THEN RETURN NO-APPLY.
        END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ptd-sales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ptd-sales V-table-Win
ON LEAVE OF ptd-sales IN FRAME F-Main /* Sales */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust.ytd-sales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust.ytd-sales V-table-Win
ON LEAVE OF cust.ytd-sales IN FRAME F-Main /* YTD Sales */
DO:
  {methods/dispflds.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/cust.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/rowavail.i}

  RUN Display-Field ("ptd-sales").
    
  IF cust.AccountType EQ "" THEN DO:
      ASSIGN 
          cust.accountType:SCREEN-VALUE = "<None>"
          cust.accountType:MODIFIED = FALSE.
  END.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/ 
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    
    /* Code placed here will execute PRIOR to standard behavior. */
    IF cust.AccountType:SCREEN-VALUE IN FRAME {&frame-name} = "<None>" THEN ASSIGN 
        cust.accountType = "".
  
     
  RUN valid-cust(OUTPUT lCheckError) NO-ERROR.
  IF lCheckError THEN RETURN NO-APPLY.
  
  RUN valid-naics(OUTPUT lCheckError) NO-ERROR.
  IF lCheckError THEN RETURN NO-APPLY.
            
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lyytd-tot V-table-Win 
PROCEDURE lyytd-tot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
  IF ll-secure THEN DO:
    RUN ar/d-ytdbal.w (ROWID(cust)).

    FIND CURRENT cust NO-LOCK.
    RUN dispatch ("display-fields").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-tot V-table-Win 
PROCEDURE recalc-tot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).
      
  IF ll-secure THEN DO:
    RUN ar/d-rectot.w (ROWID(cust)).

    FIND CURRENT cust NO-LOCK.
    RUN dispatch ("display-fields").
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "cust"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust V-table-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
  DEFINE BUFFER bf-cust FOR cust .
  {methods/lValidateError.i YES}    

  DO WITH FRAME {&frame-name}:
    IF cust.parentCust:SCREEN-VALUE NE "" THEN do:
          FIND FIRST bf-cust NO-LOCK
               WHERE bf-cust.company EQ cust.company 
                 AND bf-cust.cust-no EQ cust.parentCust:SCREEN-VALUE NO-ERROR .
          IF AVAIL bf-cust AND bf-cust.active EQ "I" THEN DO: 
             MESSAGE "Customer is Inactive. Try Help." VIEW-AS ALERT-BOX ERROR.
             oplReturnError = TRUE .
             APPLY "entry" TO cust.parentCust.
          END.
          IF NOT AVAIL bf-cust THEN DO:
             MESSAGE "Customer is Invalid. Try Help." VIEW-AS ALERT-BOX ERROR.
             oplReturnError = TRUE .
             APPLY "entry" TO cust.parentCust. 
          END.   
    END.    
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-industry V-table-Win 
PROCEDURE valid-industry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO . 
  DEFINE BUFFER bf-customerindustry FOR customerIndustry.
  {&methods/lValidateError.i YES}

  DO WITH FRAME {&FRAME-NAME}:
    IF cust.industryID:SCREEN-VALUE NE "" THEN
    DO:
      FIND FIRST bf-customerindustry NO-LOCK
           WHERE bf-customerindustry.company    EQ cust.company
             AND bf-customerIndustry.industryID EQ cust.industryID:SCREEN-VALUE
             NO-ERROR .
        IF AVAIL bf-customerindustry THEN
        DO:
           MESSAGE "Please enter a valid industry... " VIEW-AS ALERT-BOX INFO .
           opReturnError = YES .           
        END.     
    END.
  END.
  {&methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-naics V-table-Win 
PROCEDURE valid-naics :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
  DEFINE BUFFER bf-naics FOR naics .
  {methods/lValidateError.i YES}    

  DO WITH FRAME {&frame-name}:    
          FIND FIRST bf-naics NO-LOCK
               WHERE bf-naics.naicsID EQ cust.naicsCode:SCREEN-VALUE NO-ERROR .
          IF AVAIL bf-naics AND bf-naics.inActive EQ YES THEN DO: 
             MESSAGE "NACIS Code is Inactive. Try Help." VIEW-AS ALERT-BOX ERROR.
             oplReturnError = TRUE .
             APPLY "entry" TO cust.naicsCode.
          END.
          IF NOT AVAIL bf-naics THEN DO:
             MESSAGE "NACIS Code is Invalid. Try Help." VIEW-AS ALERT-BOX ERROR.
             oplReturnError = TRUE .
             APPLY "entry" TO cust.naicsCode. 
          END.        
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

