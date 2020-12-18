&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: est/estReleases.w

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
&SCOPED-DEFINE yellowColumnsName w-seldue

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO .
DEFINE VARIABLE lEnableButton AS LOGICAL NO-UNDO .
/* Local Variable Definitions ---                                       */

IF PROGRAM-NAME(2) MATCHES "*viewers/itemfg.*"  THEN
    ASSIGN lEnableButton = YES .
{methods/prgsecur.i}               
{methods/defines/hndldefs.i}               
{sys/inc/VAR.i NEW SHARED}
{methods/template/brwCustomDef.i}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE NEW SHARED VARIABLE uperiod      AS INTEGER NO-UNDO.  /* for gl-open.p */

DEFINE            VARIABLE v-show-disc  AS LOG     NO-UNDO.
DEFINE            VARIABLE choice       AS LOG     NO-UNDO.
DEFINE            VARIABLE lv-pre-disc  AS DECIMAL NO-UNDO.
DEFINE            VARIABLE lv-pre-paid  AS DECIMAL NO-UNDO.

DEFINE            VARIABLE lv-first     AS LOG     INIT YES NO-UNDO.
DEFINE            VARIABLE lv-num-rec   AS INTEGER NO-UNDO.
DEFINE            VARIABLE lv-in-add    AS LOG     NO-UNDO.
DEFINE            VARIABLE lv-in-update AS LOG     NO-UNDO.
DEFINE            VARIABLE cShipLoc     AS CHARACTER NO-UNDO .

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estRelease

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 estRelease.quantity estRelease.quantityRelease estRelease.shipFromLocationID getShipLoc() @ cShipLoc estRelease.carrierID estRelease.carrierZone estRelease.quantityOfUnits estRelease.monthsAtShipFrom estRelease.handlingCostTotal estRelease.storageCostTotal estRelease.freightCost   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH estRelease WHERE estRelease.compamy = cocode ~   AND estRelease.estimateNo              = eb.est-no ~   AND estRelease.FormNo                  = eb.form-no ~   AND estRelease.BlankNo                 = eb.blank-No         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH estRelease WHERE estRelease.compamy = cocode ~   AND estRelease.estimateNo              = eb.est-no ~   AND estRelease.FormNo                  = eb.form-no ~   AND estRelease.BlankNo                 = eb.blank-No         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 estRelease
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 estRelease


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-update btn-add btn-copy ~
btn-delete btn-ok quantity cCustNo ship-to RECT-1 
&Scoped-Define DISPLAYED-OBJECTS est-no quantity iForm iBlank cCustNo ~
cust-name ship-to ship-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckForNegative W-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
    ( ipcCompany AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getShipLoc W-Win 
FUNCTION getShipLoc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
     LABEL "Update " 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iBlank AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Blank #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iForm AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Form #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE quantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 152.2 BY 3
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      estRelease SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      estRelease.quantity LABEL "Est Qty" WIDTH 12 LABEL-BGCOLOR 14 FORMAT ">,>>>,>>9"
    estRelease.quantityRelease LABEL "Rel Qty" WIDTH 12 LABEL-BGCOLOR 14 FORMAT ">,>>>,>>9"
    estRelease.shipFromLocationID LABEL "From" WIDTH 10 LABEL-BGCOLOR 14
    estRelease.shipToID LABEL "To" WIDTH 10 LABEL-BGCOLOR 14
    estRelease.carrierID LABEL "Carrier" WIDTH 10 LABEL-BGCOLOR 14
    estRelease.carrierZone LABEL "Zone" WIDTH 10 LABEL-BGCOLOR 14
    estRelease.quantityOfUnits LABEL "Units" WIDTH 10 LABEL-BGCOLOR 14
    estRelease.monthsAtShipFrom LABEL "Months" WIDTH 15 LABEL-BGCOLOR 14
    estRelease.handlingCostTotal LABEL "Handling" WIDTH 15 LABEL-BGCOLOR 14
    estRelease.storageCostTotal LABEL "Storage" WIDTH 15 LABEL-BGCOLOR 14   
    estRelease.freightCost LABEL "Freight" WIDTH 15 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 152.8 BY 13.05
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-1 AT ROW 6.52 COL 2.2
     btn-add AT ROW 4.95 COL 2.2 WIDGET-ID 16
     btn-copy AT ROW 4.95 COL 17.0
     btn-update AT ROW 4.95 COL 31.8
     btn-delete AT ROW 4.95 COL 46.5
     btn-ok AT ROW 20.29 COL 69
     est-no AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
     quantity AT ROW 2.14 COL 46.2 COLON-ALIGNED WIDGET-ID 198
     iForm AT ROW 2.19 COL 117.4 COLON-ALIGNED WIDGET-ID 314
     iBlank AT ROW 2.19 COL 135.2 COLON-ALIGNED WIDGET-ID 316
     cCustNo AT ROW 3.33 COL 13.4 COLON-ALIGNED WIDGET-ID 176
     cust-name AT ROW 3.33 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     ship-to AT ROW 3.38 COL 83 COLON-ALIGNED WIDGET-ID 178
     ship-name AT ROW 3.38 COL 101.4 COLON-ALIGNED NO-LABEL WIDGET-ID 204
     "Main Input" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
     RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 154 BY 21.33
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
         TITLE              = "Miscellaneous Product Estimate - Releases"
         HEIGHT             = 21.67
         WIDTH              = 154.8
         MAX-HEIGHT         = 26.38
         MAX-WIDTH          = 162.8
         VIRTUAL-HEIGHT     = 26.38
         VIRTUAL-WIDTH      = 162.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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

/* SETTINGS FOR FILL-IN cust-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN est-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBlank IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-name IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estRelease WHERE estRelease.compamy = cocode ~
  AND estRelease.estimateNo              = eb.est-no ~
  AND estRelease.FormNo                  = eb.form-no ~
  AND estRelease.BlankNo                 = eb.blank-No         ~{&SORTBY-PHRASE}.
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
ON END-ERROR OF W-Win /* Miscellaneous Product Estimate - Releases */
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
ON WINDOW-CLOSE OF W-Win /* Miscellaneous Product Estimate - Releases */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
  
        /*  APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
        */
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
            IF lEnableButton AND AVAIL estRelease THEN do:
                RUN est/dNewMiscUpd.w (RECID(estRelease),ROWID(eb),"View", OUTPUT lv-rowid) .
            END.
            ELSE IF AVAILABLE estRelease THEN 
            DO:
                RUN est/dNewMiscUpd.w (RECID(estRelease),ROWID(eb),"Update", OUTPUT lv-rowid) . 
                RUN repo-query (ROWID(estRelease)).
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
        DEFINE BUFFER bff-estRelease FOR estRelease .
    
        RUN est/dNewMiscUpd.w (?,ROWID(eb),"Add", OUTPUT lv-rowid) . 
        FIND FIRST bff-estRelease NO-LOCK
            WHERE bff-estRelease.company EQ cocode
            AND ROWID(bff-estRelease) EQ lv-rowid NO-ERROR .
        IF AVAILABLE bff-estRelease THEN
            RUN repo-query (lv-rowid).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy W-Win
ON CHOOSE OF btn-copy IN FRAME F-Main /* Copy Selected */
DO:
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iEstReleaseID   AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-rowid        AS ROWID     NO-UNDO.

        DEFINE BUFFER bff-estRelease FOR estRelease .

        RUN system/FreightProcs.p PERSISTENT SET hftp.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftp).

    
        IF AVAILABLE estRelease AND AVAILABLE eb THEN
        DO:
       
            RUN CreateEstRelease( INPUT estRelease.company,INPUT eb.est-no,INPUT eb.form-no,
                INPUT eb.blank-no,eb.eqty, OUTPUT iEstReleaseID,
                OUTPUT lCreated ,  OUTPUT cCreatedMessage) .

            FIND FIRST bff-estRelease EXCLUSIVE-LOCK
                WHERE bff-estRelease.estReleaseID EQ iEstReleaseID NO-ERROR.

            IF AVAILABLE bff-estRelease THEN 
            DO:
                BUFFER-COPY estRelease EXCEPT estReleaseID rec_key TO bff-estRelease .

                RUN est/dNewMiscUpd.w (RECID(bff-estRelease),ROWID(eb),"copy", OUTPUT lv-rowid) . 
                IF lv-rowid NE ? THEN
                    RUN repo-query (lv-rowid).
            END.
        END.


        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftp). 
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete W-Win
ON CHOOSE OF btn-delete IN FRAME F-Main /* Delete Selected */
DO:
    DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-rowid        AS ROWID     NO-UNDO.
     IF AVAILABLE estRelease THEN DO:
         MESSAGE "Are you sure you want to delete this release?" 
             view-as alert-box question
             button yes-no update ll-ans as log.
         if not ll-ans then return NO-APPLY.

         RUN system/FreightProcs.p PERSISTENT SET hftp.
         THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftp).

         RUN DeleteEstReleaseByID (INPUT estRelease.estReleaseID) .
         RUN repo-query (lv-rowid).

         THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftp). 
     END.                                             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok W-Win
ON CHOOSE OF btn-ok IN FRAME F-Main /* OK */
DO:
    DEFINE VARIABLE cQtyonEst AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lmessage AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
    DEFINE BUFFER bf-estRelease FOR estRelease .
        IF AVAIL eb THEN do:
            FIND est-qty 
                WHERE est-qty.company EQ eb.company
                AND est-qty.est-no EQ eb.est-no
                AND est-qty.eqty EQ eb.eqty 
                NO-ERROR.

            IF AVAIL est-qty THEN
                DO i = 1 TO 20:
                 IF est-qty.qty[i] NE 0 THEN do:
                    IF i EQ 1 THEN
                        cQtyonEst  = STRING(est-qty.qty[i])  NO-ERROR.
                    ELSE cQtyonEst  = cQtyonEst + "," + string(est-qty.qty[i])   NO-ERROR.
                 END.
                END.
           
           FOR EACH bf-estRelease EXCLUSIVE-LOCK WHERE bf-estRelease.company = cocode
               AND bf-estRelease.estimateNo              = eb.est-no  
               AND bf-estRelease.FormNo                  = eb.form-no 
               AND bf-estRelease.BlankNo                 = eb.blank-No  :
               
               IF LOOKUP(string(bf-estRelease.quantity),cQtyonEst) EQ 0 THEN do:
                   lmessage = TRUE .
                   DELETE bf-estRelease .
               END.
           END.
        END.
        RELEASE bf-estRelease .
        IF lMessage THEN DO:
             MESSAGE "Estimate Quantity Changed - Must re-enter releases."  VIEW-AS ALERT-BOX ERROR .
             RUN repo-query (lv-rowid).
             RETURN NO-APPLY .
        END.
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update W-Win
ON CHOOSE OF btn-update IN FRAME F-Main /* Update Selected */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO. 
        IF AVAILABLE estRelease THEN 
        DO:

            RUN est/dNewMiscUpd.w (RECID(estRelease),ROWID(eb),"Update", OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(estRelease)).
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
                est-no   = eb.est-no 
                quantity = (eb.eqty)
                cCustNo  = eb.cust-no
                ship-to  = eb.ship-id
                iForm    = (eb.form-no)
                iBlank   = (eb.blank-no)
                .
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ eb.company
            AND cust.cust-no EQ eb.cust-no NO-ERROR .
        IF AVAILABLE cust THEN 
        DO:
            cust-name = cust.NAME .
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company = eb.company
                AND shipto.cust-no EQ eb.cust-no
                AND shipto.ship-id EQ eb.ship-id  NO-ERROR .
       
            IF AVAILABLE shipto THEN
                ASSIGN ship-name = shipto.ship-name .
        END.
        DISPLAY est-no quantity cCustNo ship-to iForm iBlank cust-name ship-name.
    END.

  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    RUN local-open-query.

    DO WITH FRAME {&FRAME-NAME}:
       
        IF lEnableButton  THEN do:
            ASSIGN
                btn-update:SENSITIVE = NO 
                btn-add:SENSITIVE = NO 
                btn-copy:SENSITIVE = NO 
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
  
    OPEN QUERY BROWSE-1 FOR EACH estRelease WHERE
        estRelease.company = cocode 
        AND estRelease.estimateNo              = eb.est-no 
        AND estRelease.FormNo                  = eb.form-no
        AND estRelease.BlankNo                 = eb.blank-No NO-LOCK.

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
  {src/adm/template/snd-list.i "estRelease"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckForNegative W-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
    ( ipcCompany AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNegativeFound AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cVendor        AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ap-chk FOR ap-chk.

    ASSIGN 
        lNegativeFound = NO
        cVendor        = "".
    FOR EACH bf-ap-chk
        WHERE bf-ap-chk.company EQ ipcCompany
        AND bf-ap-chk.man-check EQ NO
        AND bf-ap-chk.check-amt LT 0
        NO-LOCK:
        lNegativeFound = YES.
        IF cVendor NE "" THEN cVendor = cVendor + ",".
        cVendor = cVendor + bf-ap-chk.vend-no.
    END.

    IF lNegativeFound THEN 
        MESSAGE "Checks with a negative total have been found for vendor(s):" SKIP
            cVendor SKIP(1)
            "Would you like to return to the selection screen to correct this?"    
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lNegativeFound.
    RETURN lNegativeFound.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getShipLoc W-Win 
FUNCTION getShipLoc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    lc-result = "".
      FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.ACTIVE EQ "X" NO-ERROR .
        
        FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ cocode 
        AND (shipto.cust-no EQ estRelease.customerID OR  shipto.cust-no EQ cust.cust-no)
        AND TRIM(shipto.ship-id) = estRelease.shipToID
        NO-ERROR.
        IF AVAILABLE shipto THEN 
        ASSIGN lc-result =  shipto.loc .

    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

