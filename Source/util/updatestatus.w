&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

{methods/prgsecur.i}

DEFINE VARIABLE cStatUpd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCheckSimulate AS LOGICAL NO-UNDO.
DEFINE VARIABLE cOrderFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPOFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRelFileName AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttOe-ord NO-UNDO 
    FIELDS ord-no      AS INTEGER   LABEL "Order Number"
    FIELDS stat        AS CHARACTER LABEL "Previous Status"
    FIELDS statDesc    AS CHARACTER LABEL "Previous Status Description"
    FIELDS newStat     AS CHARACTER LABEL "Current Status"  
    FIELDS newStatDesc AS CHARACTER LABEL "Current Status Description"
    .
DEFINE TEMP-TABLE ttPo-ord NO-UNDO 
    FIELDS po-no       AS INTEGER   LABEL "Purchase Order Number"
    FIELDS stat        AS CHARACTER LABEL "Previous Status"   
    FIELDS statDesc    AS CHARACTER LABEL "Previous Status Description"
    FIELDS newStat     AS CHARACTER LABEL "Current Status"
    FIELDS newStatDesc AS CHARACTER LABEL "Current Status Description"
    .
DEFINE TEMP-TABLE ttJob NO-UNDO 
    FIELDS job-no      AS CHARACTER LABEL "Job Number"
    FIELDS stat        AS CHARACTER LABEL "Previous Status"   
    FIELDS statDesc    AS CHARACTER LABEL "Previous Status Description"
    FIELDS newStat     AS CHARACTER LABEL "Current Status"
    FIELDS newStatDesc AS CHARACTER LABEL "Current Status Description"
    .
DEFINE TEMP-TABLE ttOe-rel NO-UNDO 
    FIELDS ord-no      AS INTEGER   LABEL "Order Number"
    FIELDS rel-no      AS INTEGER   LABEL "Release Number"
    FIELDS stat        AS CHARACTER LABEL "Previous Status"  
    FIELDS statDesc    AS CHARACTER LABEL "Previous Status Description" 
    FIELDS newStat     AS CHARACTER LABEL "Current Status"
    FIELDS newStatDesc AS CHARACTER LABEL "Current Status Description"
    .

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit RECT-1 fiBeginOrder fiEndingOrder ~
tbUpdateOrderStatus fiBeginJob fiEndingJob tbUpdateJobStatus fiBeginReleaseOrder ~
fiEndingReleaseOrder tbUpdateReleases fiBeginPO fiEndingPO ~
tbupdatePurchaseOrders tbOpenFiles btSimulate btExecute 
&Scoped-Define DISPLAYED-OBJECTS fiBeginOrder fiEndingOrder ~
tbUpdateOrderStatus fiBeginJob fiEndingJob tbUpdateJobStatus fiBeginReleaseOrder ~
fiEndingReleaseOrder tbUpdateReleases fiBeginPO fiEndingPO ~
tbupdatePurchaseOrders fiDirectory tbOpenFiles 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExecute 
     LABEL "Execute" 
     SIZE 16 BY 1.43.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "Exit" 
     SIZE 7.2 BY 1.71 TOOLTIP "Exit"
     BGCOLOR 21 FGCOLOR 21 .

DEFINE BUTTON btSimulate 
     LABEL "Simulate" 
     SIZE 16 BY 1.43.

DEFINE VARIABLE fiBeginJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin Job" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginOrder AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Begin Order" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginPO AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Begin PO" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginReleaseOrder AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Begin Order" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 56 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ending Job" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingOrder AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Ending Order" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingPO AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Ending PO" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingReleaseOrder AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Ending Order" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.48.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 0    
     SIZE 106.8 BY 2.14
     BGCOLOR 21 FGCOLOR 21 .

DEFINE VARIABLE tbOpenFiles AS LOGICAL INITIAL NO 
     LABEL "Open Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateJobStatus AS LOGICAL INITIAL NO 
     LABEL "Update Job Status" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81
     FONT 22 NO-UNDO.

DEFINE VARIABLE tbUpdateOrderStatus AS LOGICAL INITIAL NO 
     LABEL "Update Order Status" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 22 NO-UNDO.

DEFINE VARIABLE tbupdatePurchaseOrders AS LOGICAL INITIAL NO 
     LABEL "Update Purchase Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81
     FONT 22 NO-UNDO.

DEFINE VARIABLE tbUpdateReleases AS LOGICAL INITIAL NO 
     LABEL "Update Releases" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.8 BY .81
     FONT 22 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExit AT ROW 1.19 COL 91.6 WIDGET-ID 320
     fiBeginOrder AT ROW 4.38 COL 47.4 COLON-ALIGNED WIDGET-ID 18
     fiEndingOrder AT ROW 4.38 COL 78.4 COLON-ALIGNED WIDGET-ID 20
     tbUpdateOrderStatus AT ROW 4.57 COL 5.6 WIDGET-ID 2
     fiBeginJob AT ROW 6 COL 47.4 COLON-ALIGNED WIDGET-ID 22
     fiEndingJob AT ROW 6 COL 78.4 COLON-ALIGNED WIDGET-ID 24
     tbUpdateJobStatus AT ROW 6.19 COL 5.6 WIDGET-ID 6
     fiBeginReleaseOrder AT ROW 7.62 COL 47.4 COLON-ALIGNED WIDGET-ID 26
     fiEndingReleaseOrder AT ROW 7.62 COL 78.4 COLON-ALIGNED WIDGET-ID 28
     tbUpdateReleases AT ROW 7.81 COL 5.8 WIDGET-ID 8
     fiBeginPO AT ROW 9.19 COL 47.4 COLON-ALIGNED WIDGET-ID 30
     fiEndingPO AT ROW 9.19 COL 78.4 COLON-ALIGNED WIDGET-ID 32
     tbupdatePurchaseOrders AT ROW 9.38 COL 5.6 WIDGET-ID 4
     fiDirectory AT ROW 11.91 COL 18.4 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     tbOpenFiles AT ROW 11.95 COL 79 WIDGET-ID 324
     btSimulate AT ROW 13.76 COL 31.6 WIDGET-ID 34
     btExecute AT ROW 13.76 COL 53.2 WIDGET-ID 38
     "Records to view or recover purged information will be stored in directory:" VIEW-AS TEXT
          SIZE 81.8 BY .62 AT ROW 11.05 COL 9.6 WIDGET-ID 10
          FONT 22
     RECT-1 AT ROW 3.91 COL 4 WIDGET-ID 40
     RECT-14 AT ROW 1 COL 1 WIDGET-ID 322
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.8 BY 14.71
         BGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "UpdateStatus"
         HEIGHT             = 14.71
         WIDTH              = 100
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       fiBeginJob:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiBeginOrder:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiBeginPO:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiBeginReleaseOrder:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN fiDirectory IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiDirectory:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiEndingJob:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiEndingOrder:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiEndingPO:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiEndingReleaseOrder:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* UpdateStatus */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* UpdateStatus */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.
    
    CASE FOCUS:NAME :
        WHEN "fiBeginPo" OR 
        WHEN "fiEndingPo"  THEN DO:
            RUN system/openLookup.p (
            INPUT  g_company, 
            INPUT  "",  /* Lookup ID */
            INPUT  36,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.
        WHEN "fiBeginJob" OR 
        WHEN "fiEndingJob"  THEN 
            DO:
                RUN system/openLookup.p (
                    INPUT  g_company, 
                    INPUT  "",  /* Lookup ID */
                    INPUT  3,  /* Subject ID */
                    INPUT  "",  /* User ID */
                    INPUT  0,   /* Param Value ID */
                    OUTPUT cFieldsValue, 
                    OUTPUT cFoundValue, 
                    OUTPUT recFoundRecID
                    ).   
                IF cFoundValue <> "" THEN 
                    ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
            END.
        WHEN "fiBeginOrder" OR 
        WHEN "fiEndingOrder" OR 
        WHEN "fiBeginReleaseOrder" OR 
        WHEN "fiEndingReleaseOrder"  THEN 
            DO:
                RUN system/openLookup.p (
                    INPUT  g_company, 
                    INPUT  "",  /* Lookup ID */
                    INPUT  27,  /* Subject ID */
                    INPUT  "",  /* User ID */
                    INPUT  0,   /* Param Value ID */
                    OUTPUT cFieldsValue, 
                    OUTPUT cFoundValue, 
                    OUTPUT recFoundRecID
                    ).   
                IF cFoundValue <> "" THEN 
                    ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
            END.

    END CASE.
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecute C-Win
ON CHOOSE OF btExecute IN FRAME DEFAULT-FRAME /* Execute */
DO:   

   RUN pRunProcess(YES). 
     
   /* FOR EACH ttOe-rel:                      
        FOR FIRST  oe-rel EXCLUSIVE-LOCK 
            WHERE oe-rel.ord-no EQ ttOe-rel.ord-no
              AND  oe-rel.rel-no EQ ttOe-rel.rel-no:
              
              {custom/statusMsg.i " 'Processing Order Release#  '  + string(oe-rel.ord-no) "}
            ASSIGN 
                oe-rel.stat = ttOe-rel.newstat.
        END.
    END.
    FOR EACH ttPo-ord:
        FOR FIRST  po-ord EXCLUSIVE-LOCK 
            WHERE po-ord.po-no EQ ttPo-ord.po-no:
            {custom/statusMsg.i " 'Processing Purchase Order#  '  + string(po-ord.po-no) "}
            ASSIGN 
                po-ord.stat = ttPo-ord.newstat
                po-ord.opened = NO.
        END.
     END.
     FOR EACH ttOe-ord:   
        FOR FIRST  oe-ord EXCLUSIVE-LOCK 
            WHERE oe-ord.ord-no EQ ttOe-ord.ord-no:
            {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ord.ord-no) "}
            ASSIGN 
                oe-ord.stat = ttOe-ord.newstat
                oe-ord.opened = NO.
        END.                    
     END.       
     FOR EACH ttJob:
        FOR FIRST  job EXCLUSIVE-LOCK 
            WHERE job.job-no EQ ttJob.job-no:
            {custom/statusMsg.i " 'Processing Job#  '  + string(job.job-no) "}
            ASSIGN 
                job.stat = ttJob.newstat
                job.opened = NO.
        END.
    END.
    
    EMPTY TEMP-TABLE  ttOe-rel.
    EMPTY TEMP-TABLE  ttOe-ord.
    EMPTY TEMP-TABLE  ttPo-ord.
    EMPTY TEMP-TABLE  ttJob.
    RELEASE oe-rel.
    RELEASE oe-ord.
    RELEASE job.
    RELEASE po-ord.
    
    MESSAGE "Status have been updated."
    VIEW-AS ALERT-BOX.
    STATUS DEFAULT "Process Complete" .  */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
DO:          
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME DEFAULT-FRAME /* Simulate */
DO:   
  lCheckSimulate = YES.
  RUN pRunProcess(NO).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginJob C-Win
ON VALUE-CHANGED OF fiBeginJob IN FRAME DEFAULT-FRAME /* Begin Job */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginOrder C-Win
ON VALUE-CHANGED OF fiBeginOrder IN FRAME DEFAULT-FRAME /* Begin Order */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginPO C-Win
ON VALUE-CHANGED OF fiBeginPO IN FRAME DEFAULT-FRAME /* Begin PO */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginReleaseOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginReleaseOrder C-Win
ON VALUE-CHANGED OF fiBeginReleaseOrder IN FRAME DEFAULT-FRAME /* Begin Release */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndingJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndingJob C-Win
ON VALUE-CHANGED OF fiEndingJob IN FRAME DEFAULT-FRAME /* Ending Job */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndingOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndingOrder C-Win
ON VALUE-CHANGED OF fiEndingOrder IN FRAME DEFAULT-FRAME /* Ending Order */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndingPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndingPO C-Win
ON VALUE-CHANGED OF fiEndingPO IN FRAME DEFAULT-FRAME /* Ending PO */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndingReleaseOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndingReleaseOrder C-Win
ON VALUE-CHANGED OF fiEndingReleaseOrder IN FRAME DEFAULT-FRAME /* Ending Release */
DO:
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUpdateJobStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpdateJobStatus C-Win
ON VALUE-CHANGED OF tbUpdateJobStatus IN FRAME DEFAULT-FRAME /* Update Job Status */
DO:
   ASSIGN
        fiBeginJob:HIDDEN        = NOT tbUpdateJobStatus:CHECKED 
        fiEndingJob:HIDDEN       = NOT tbUpdateJobStatus:CHECKED 
        fiBeginJob:VISIBLE       = tbUpdateJobStatus:CHECKED 
        fiEndingJob:VISIBLE      = tbUpdateJobStatus:CHECKED 
        fiEndingJob:SCREEN-VALUE =  ""
        fiBeginJob:SCREEN-VALUE =  "" 
        .
    RUN SetButtons.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUpdateOrderStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpdateOrderStatus C-Win
ON VALUE-CHANGED OF tbUpdateOrderStatus IN FRAME DEFAULT-FRAME /* Update Order Status */
DO:  
 ASSIGN
        fiBeginOrder:HIDDEN        = NOT tbUpdateOrderStatus:CHECKED 
        fiEndingOrder:HIDDEN       = NOT tbUpdateOrderStatus:CHECKED 
        fiBeginOrder:VISIBLE       = tbUpdateOrderStatus:CHECKED 
        fiEndingOrder:VISIBLE      = tbUpdateOrderStatus:CHECKED 
        fiEndingOrder:SCREEN-VALUE = ""
        fiBeginOrder:SCREEN-VALUE  = ""
        .
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbupdatePurchaseOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbupdatePurchaseOrders C-Win
ON VALUE-CHANGED OF tbupdatePurchaseOrders IN FRAME DEFAULT-FRAME /* Update Purchase Orders */
DO:
   ASSIGN
        fiBeginPO:HIDDEN        = NOT tbupdatePurchaseOrders:CHECKED 
        fiEndingPO:HIDDEN       = NOT tbupdatePurchaseOrders:CHECKED 
        fiBeginPO:VISIBLE       = tbupdatePurchaseOrders:CHECKED 
        fiEndingPO:VISIBLE      = tbupdatePurchaseOrders:CHECKED 
        fiBeginPO:SCREEN-VALUE  = ""
        fiEndingPO:SCREEN-VALUE = ""
        .
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUpdateReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpdateReleases C-Win
ON VALUE-CHANGED OF tbUpdateReleases IN FRAME DEFAULT-FRAME /* Update Releases */
DO:
  ASSIGN
        fiBeginReleaseOrder:HIDDEN        = NOT tbUpdateReleases:CHECKED 
        fiEndingReleaseOrder:HIDDEN       = NOT tbUpdateReleases:CHECKED 
        fiBeginReleaseOrder:VISIBLE       = tbUpdateReleases:CHECKED 
        fiEndingReleaseOrder:VISIBLE      = tbUpdateReleases:CHECKED 
        fiBeginReleaseOrder:SCREEN-VALUE  = "" 
        fiEndingReleaseOrder:SCREEN-VALUE = ""
        .
        RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
   {sys/inc/f3helpw.i}
    btSimulate:LOAD-IMAGE("Graphics/32x32/Simulate.png").
    btExecute:LOAD-IMAGE("Graphics/32x32/Execute.png").
    btSimulate:LOAD-IMAGE-INSENSITIVE("Graphics/32x32/Simulate_disabled.png").
    btExecute:LOAD-IMAGE-INSENSITIVE("Graphics/32x32/Execute_disabled.png").
    
    RUN enable_UI.
     ASSIGN
        fiBeginPo:HIDDEN         = TRUE
        fiEndingPo:HIDDEN        = TRUE
        fiBeginPo:VISIBLE        = FALSE
        fiEndingPo:VISIBLE       = FALSE
        fiBeginJob:HIDDEN        = TRUE
        fiEndingJob:HIDDEN       = TRUE
        fiBeginJob:VISIBLE       = FALSE
        fiEndingJob:VISIBLE      = FALSE
        fiBeginOrder:HIDDEN      = TRUE
        fiEndingOrder:HIDDEN     = TRUE
        fiBeginOrder:VISIBLE     = FALSE
        fiEndingOrder:VISIBLE    = FALSE
        fiBeginReleaseOrder:HIDDEN    = TRUE
        fiEndingReleaseOrder:HIDDEN   = TRUE
        fiBeginReleaseOrder:VISIBLE   = FALSE
        fiEndingReleaseOrder:VISIBLE  = FALSE
        btSimulate:SENSITIVE     = FALSE 
        btExecute:SENSITIVE      = FALSE 
        tbOpenFiles:SENSITIVE      = FALSE
        .
    RUN FileSys_GetTempDirectory(
        OUTPUT cLocation
        ).
 
    fiDirectory:screen-value = cLocation. 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fiBeginOrder fiEndingOrder tbUpdateOrderStatus fiBeginJob fiEndingJob 
          tbUpdateJobStatus fiBeginReleaseOrder fiEndingReleaseOrder tbUpdateReleases 
          fiBeginPO fiEndingPO tbupdatePurchaseOrders fiDirectory tbOpenFiles 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btExit RECT-1 fiBeginOrder fiEndingOrder tbUpdateOrderStatus 
         fiBeginJob fiEndingJob tbUpdateJobStatus fiBeginReleaseOrder 
         fiEndingReleaseOrder tbUpdateReleases fiBeginPO fiEndingPO 
         tbupdatePurchaseOrders tbOpenFiles btSimulate btExecute 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtons C-Win 
PROCEDURE SetButtons :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
        END.
    IF tbUpdateJobStatus:CHECKED THEN 
    DO:
        IF  fiBeginJob:SCREEN-VALUE NE " " AND 
            fiEndingJob:SCREEN-VALUE NE " " THEN 
                btSimulate:SENSITIVE = TRUE.
                
        ELSE 
            btSimulate:SENSITIVE = FALSE.               
    END.
    
    IF tbUpdateOrderStatus:CHECKED THEN 
    DO:
        IF fiBeginOrder:SCREEN-VALUE NE "" AND 
           fiEndingOrder:SCREEN-VALUE NE "" THEN 
            btSimulate:SENSITIVE = TRUE.
        ELSE 
            btSimulate:SENSITIVE = FALSE.
                
    END.
    
    IF tbupdatePurchaseOrders:CHECKED THEN 
    DO:
        IF  fiBeginPO:SCREEN-VALUE NE "" AND 
            fiEndingPO:SCREEN-VALUE NE "" THEN 
            btSimulate:SENSITIVE = TRUE.
        ELSE 
            btSimulate:SENSITIVE = FALSE.
                
    END.
    
    IF tbUpdateReleases:CHECKED THEN 
    DO:
        IF  fiBeginReleaseOrder:SCREEN-VALUE NE ""  AND 
            fiEndingReleaseOrder:SCREEN-VALUE NE "" THEN 
            btSimulate:SENSITIVE = TRUE.
            
        ELSE 
            btSimulate:SENSITIVE = FALSE.
                
    END.
     
    IF  NOT tbUpdateReleases:CHECKED       AND 
        NOT tbupdatePurchaseOrders:CHECKED AND 
        NOT tbUpdateOrderStatus:CHECKED    AND 
        NOT tbUpdateJobStatus:CHECKED    
    THEN 
        btSimulate:SENSITIVE = FALSE. 
        
    IF NOT btSimulate:SENSITIVE THEN 
        tbOpenFiles:CHECKED = FALSE.
              
    ASSIGN  
        btExecute:SENSITIVE   = btSimulate:SENSITIVE 
        tbOpenFiles:SENSITIVE = btSimulate:SENSITIVE
        .
    lCheckSimulate = YES.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExecute AS LOGICAL NO-UNDO.    
        
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNewStat     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cocode       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckStatus AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-po-ordl FOR po-ordl. 
    
    RUN spGetSessionParam ("Company", OUTPUT cocode).
        
    DO WITH FRAME {&frame-name}:  
        btExecute:SENSITIVE = btSimulate:SENSITIVE.    
    
        IF lCheckSimulate THEN
        DO:
  
            EMPTY TEMP-TABLE  ttOe-rel.
            EMPTY TEMP-TABLE  ttOe-ord.
            EMPTY TEMP-TABLE  ttPo-ord.
            EMPTY TEMP-TABLE  ttJob. 
          
            IF tbUpdateReleases:CHECKED THEN 
            DO:        
                FOR EACH oe-rel NO-LOCK 
                    WHERE oe-rel.company EQ cocode
                    AND oe-rel.ord-no  GE fiBeginReleaseOrder:INPUT-VALUE   
                    AND oe-rel.ord-no  LE fiEndingReleaseOrder:INPUT-VALUE               
                    AND LOOKUP(oe-rel.stat,"C,Z") EQ 0 :
                
                    {custom/statusMsg.i " 'Processing Order Release#  '  + string(oe-rel.ord-no) "}
            
                    FIND FIRST oe-ord NO-LOCK
                        WHERE oe-ord.company EQ oe-rel.company
                        AND oe-ord.ord-no  EQ oe-rel.ord-no
                        NO-ERROR.
                    IF AVAILABLE oe-ord THEN 
                    DO:
                        {oe/rel-stat.i cNewStat}
                    END.
                    ELSE 
                        cNewStat = "S".
                
                    IF cNewStat EQ "C" OR cNewStat EQ "Z" THEN
                    DO:            
                        CREATE ttOe-rel.
                        ASSIGN 
                            ttOe-rel.ord-no = oe-rel.ord-no
                            ttOe-rel.rel-no = oe-rel.rel-no
                            ttOe-rel.stat   = oe-rel.stat
                            .                   
                    
                        ttOe-rel.newstat = cNewStat.
                        RUN oe/getReleaseStatusDesc.p( INPUT oe-rel.stat, OUTPUT cResult) .
                        ttOe-rel.statDesc = cResult.
                        RUN oe/getReleaseStatusDesc.p( INPUT ttOe-rel.newstat, OUTPUT cResult) .
                        ttOe-rel.newstatDesc = cResult.
                    END.
                END.
          
                RUN sys/ref/ExcelNameExt.p (INPUT cLocation + "\ResleaseOrder.csv",OUTPUT cRelFileName) . 
        
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttOe-rel:HANDLE,
                    INPUT cRelFileName,
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
            END.          
            IF tbupdatePurchaseOrders:CHECKED  THEN 
            DO:
                FOR EACH po-ord NO-LOCK 
                    WHERE po-ord.company EQ cocode
                    AND po-ord.po-no   GE fiBeginPO:INPUT-VALUE
                    AND po-ord.po-no   LE fiEndingPO:INPUT-VALUE
                    AND po-ord.opened EQ YES:
            
                    {custom/statusMsg.i " 'Processing Purchase Order#  '  + string(po-ord.po-no) "}
            
                    lCheckStatus = NO .
                  
                    FOR EACH bf-po-ordl NO-LOCK
                        WHERE bf-po-ordl.company EQ po-ord.company
                        AND bf-po-ordl.po-no EQ po-ord.po-no:
                        IF bf-po-ordl.opened EQ NO THEN lCheckStatus = YES.
                        ELSE lCheckStatus = NO.
                    END.
                    IF lCheckStatus THEN 
                    DO:             
                        CREATE ttPo-ord.
                        ASSIGN 
                            ttPo-ord.po-no = po-ord.po-no
                            ttPo-ord.stat  = po-ord.stat.
               
                        ttPo-ord.newstat = "C".
                        RUN oe/getStatusDesc.p( INPUT po-ord.stat, OUTPUT cResult) .
                        ttPo-ord.statDesc = cResult.
                        RUN oe/getStatusDesc.p( INPUT ttPo-ord.newstat, OUTPUT cResult) .
                        ttPo-ord.newstatDesc = cResult.
                    END.
                END. 
                RUN sys/ref/ExcelNameExt.p (INPUT cLocation + "\PurchaseOrder.csv",OUTPUT cPOFileName) .  
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttPo-ord:HANDLE,
                    INPUT cPOFileName,
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
            END.
            IF tbUpdateOrderStatus:CHECKED THEN 
            DO:      
                FOR EACH oe-ord NO-LOCK
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  GE fiBeginOrder:INPUT-VALUE 
                    AND oe-ord.ord-no  LE fiEndingOrder:INPUT-VALUE 
                    AND oe-ord.opened  EQ YES  :
                    lCheckStatus = NO .
             
                    {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ord.ord-no) "}
                  
                    FOR EACH bf-oe-ordl NO-LOCK
                        WHERE bf-oe-ordl.company EQ oe-ord.company
                        AND bf-oe-ordl.ord-no EQ oe-ord.ord-no:
                        IF bf-oe-ordl.opened EQ NO THEN lCheckStatus = YES.
                        ELSE lCheckStatus = NO.
                    END.
                    
                    IF lCheckStatus THEN 
                    DO:
                        CREATE ttOe-ord.
                
                        ASSIGN 
                            ttOe-ord.ord-no = oe-ord.ord-no
                            ttOe-ord.stat   = oe-ord.stat.
                        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
                        ttOe-ord.statDesc = cResult.
                        ttOe-ord.newstat = "C".
                        RUN oe/getStatusDesc.p( INPUT ttOe-ord.newstat, OUTPUT cResult) .
                        ttOe-ord.newstatDesc = cResult.
                    END.
                END.       
        
                RUN sys/ref/ExcelNameExt.p (INPUT cLocation + "\OrderHeader.csv",OUTPUT cOrderFileName) .
            
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttOe-ord:HANDLE,
                    INPUT cOrderFileName,
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
            END.
            IF tbUpdateJobStatus:CHECKED  THEN  
            DO:
                FOR EACH job NO-LOCK 
                    WHERE job.company EQ cocode
                    AND job.job-no  GE fiBeginOrder:SCREEN-VALUE
                    AND job.job-no  LE fiEndingOrder:SCREEN-VALUE
                    AND job-hdr.opened EQ YES:
              
                    {custom/statusMsg.i " 'Processing Job#  '  + string(job.job-no) "}
              
                    lCheckStatus = NO .
                  
                    FOR EACH bf-job-hdr NO-LOCK
                        WHERE bf-job-hdr.company EQ job.company
                        AND bf-job-hdr.job EQ job.job
                        AND bf-job-hdr.job-no EQ job.job-no
                        AND bf-job-hdr.job-no2 EQ job.job-no2:
                        IF bf-job-hdr.opened EQ NO THEN lCheckStatus = YES.
                        ELSE lCheckStatus = NO.
                    END.
                    IF lCheckStatus THEN
                    DO:
                        CREATE ttJob.
                        ASSIGN 
                            ttJob.job-no = job.job-no
                            ttJob.stat   = job.stat.
                        ttJob.newstat = "C".
                        RUN oe/getJobStatusDesc.p( INPUT job.stat, OUTPUT cResult) .
                        ttJob.statDesc = cResult.
                        RUN oe/getJobStatusDesc.p( INPUT ttJob.newstat, OUTPUT cResult) .
                        ttJob.newstatDesc = cResult.
                    END.
                END. 
        
                RUN sys/ref/ExcelNameExt.p (INPUT cLocation + "\JobHeader.csv",OUTPUT cJobFileName) .
         
                RUN Output_TempTableToCSV IN hdOutputProcs (
                    INPUT TEMP-TABLE ttJob:HANDLE,
                    INPUT cJobFileName,
                    INPUT TRUE,  /* Export Header */
                    INPUT FALSE, /* Auto increment File name */
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
            END.           

                
            lCheckSimulate = NO .
            STATUS DEFAULT "Process Complete" . 
    
        END.    
    
        IF ipcExecute THEN 
        DO:
            FOR EACH ttOe-rel:                      
                FOR FIRST  oe-rel EXCLUSIVE-LOCK 
                    WHERE oe-rel.ord-no EQ ttOe-rel.ord-no
                    AND  oe-rel.rel-no EQ ttOe-rel.rel-no:
              
                    {custom/statusMsg.i " 'Processing Order Release#  '  + string(oe-rel.ord-no) "}
                    ASSIGN 
                        oe-rel.stat = ttOe-rel.newstat.
                END.
            END.
            FOR EACH ttPo-ord:
                FOR FIRST  po-ord EXCLUSIVE-LOCK 
                    WHERE po-ord.po-no EQ ttPo-ord.po-no:
                    {custom/statusMsg.i " 'Processing Purchase Order#  '  + string(po-ord.po-no) "}
                    ASSIGN 
                        po-ord.stat   = ttPo-ord.newstat
                        po-ord.opened = NO.
                END.
            END.
            FOR EACH ttOe-ord:   
                FOR FIRST  oe-ord EXCLUSIVE-LOCK 
                    WHERE oe-ord.ord-no EQ ttOe-ord.ord-no:
                    {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ord.ord-no) "}
                    ASSIGN 
                        oe-ord.stat   = ttOe-ord.newstat
                        oe-ord.opened = NO.
                END.                    
            END.       
            FOR EACH ttJob:
                FOR FIRST  job EXCLUSIVE-LOCK 
                    WHERE job.job-no EQ ttJob.job-no:
                    {custom/statusMsg.i " 'Processing Job#  '  + string(job.job-no) "}
                    ASSIGN 
                        job.stat   = ttJob.newstat
                        job.opened = NO.
                END.
            END.
    
            EMPTY TEMP-TABLE  ttOe-rel.
            EMPTY TEMP-TABLE  ttOe-ord.
            EMPTY TEMP-TABLE  ttPo-ord.
            EMPTY TEMP-TABLE  ttJob.
            RELEASE oe-rel.
            RELEASE oe-ord.
            RELEASE job.
            RELEASE po-ord.
    
            MESSAGE "Status have been updated."
                VIEW-AS ALERT-BOX.
            STATUS DEFAULT "Process Complete" .
    
     
        END.    /* ipcExecute */
        
        IF tbOpenFiles:CHECKED THEN 
        DO:
            IF tbUpdateJobStatus:CHECKED  THEN  
                RUN OS_RunFile(
                    INPUT cJobFileName,
                    OUTPUT lSuccess,
                    OUTPUT cMessage).
            IF tbupdatePurchaseOrders:CHECKED  THEN
                RUN OS_RunFile(
                    INPUT cPOFileName,
                    OUTPUT lSuccess,
                    OUTPUT cMessage).
            IF tbUpdateOrderStatus:CHECKED THEN 
                RUN OS_RunFile(
                    INPUT cOrderFileName,
                    OUTPUT lSuccess,
                    OUTPUT cMessage).
            IF tbUpdateReleases:CHECKED THEN
                RUN OS_RunFile(
                    INPUT cRelFileName,
                    OUTPUT lSuccess,
                    OUTPUT cMessage).
        END.
           
    END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

