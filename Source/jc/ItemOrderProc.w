&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jc/ItemOrderProc.w

  Description: Display all the API Inbound events

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Sewa Singh

  Created: 27 November 2020

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the wgets created 
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

{sys/inc/var.i new shared}
{est/ttInputEst.i NEW} 

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/lastship.i} 
 
DEFINE VARIABLE lSelectTrigger           AS LOGICAL NO-UNDO.


DEFINE TEMP-TABLE ttJobEstCreation NO-UNDO
    FIELDS isSelect AS LOGICAL
    FIELDS fgItem AS CHARACTER
    FIELDS custPart AS CHARACTER
    FIELDS Customer AS CHARACTER
    FIELDS custName AS CHARACTER
    FIELDS estNo AS CHARACTER
    FIELDS style AS CHARACTER
    FIELDS cat AS CHARACTER
    FIELDS onHand AS INTEGER
    FIELD onOrder AS INTEGER
    FIELD allocated AS INTEGER
    FIELD qtyAvail AS INTEGER
    FIELD availOnHand AS INTEGER
    FIELD dueDate AS DATE
    FIELD reOrderLevel AS INTEGER
    FIELD minOrder AS INTEGER
    FIELD maxOrder AS INTEGER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJobEstCreation

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttJobEstCreation.isSelect ttJobEstCreation.fgItem ttJobEstCreation.custPart ttJobEstCreation.Customer ttJobEstCreation.custName ttJobEstCreation.estNo ttJobEstCreation.style ttJobEstCreation.cat ttJobEstCreation.onHand ttJobEstCreation.onOrder ttJobEstCreation.allocated ttJobEstCreation.qtyAvail ttJobEstCreation.availOnHand ttJobEstCreation.dueDate ttJobEstCreation.reOrderLevel ttJobEstCreation.minOrder ttJobEstCreation.maxOrder   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttJobEstCreation.isSelect   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttJobEstCreation
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttJobEstCreation
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttJobEstCreation
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttJobEstCreation.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttJobEstCreation
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttJobEstCreation


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 btExit btFilter cFGItem ~
cCustomerPart cCustomer cEst cStyle btCrtJob btCrtPo cCat cLoc BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fieventIDlb fiCustPartLabel ~
fiCustomerLabel fiEstLabel fiStyleLebel cFGItem cCustomerPart cCustomer ~
cEst cStyle fiCatLabel filocLabel cCat cLoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCrtJob 
     LABEL "Create Job" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON btCrtPo 
     LABEL "Create PO" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btFilter 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Filter" 
     SIZE 9 BY 2.14.

DEFINE VARIABLE cCat AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cCustomer AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cCustomerPart AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cEst AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 17.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cLoc AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cStyle AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiCatLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Category" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiCustomerLabel AS CHARACTER FORMAT "X(25)":U INITIAL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiCustPartLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Customer Part" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEstLabel AS CHARACTER FORMAT "X(10)":U INITIAL "Estimate" 
     VIEW-AS FILL-IN 
     SIZE 11.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "FG Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE filocLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiStyleLebel AS CHARACTER FORMAT "X(256)":U INITIAL "Style" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 140 BY 5.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttJobEstCreation SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttJobEstCreation.isSelect COLUMN-LABEL "[ ] All" 
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttJobEstCreation.fgItem COLUMN-LABEL "FG Item" FORMAT "x(15)":U
            WIDTH 23
      ttJobEstCreation.onHand COLUMN-LABEL "On Hand" FORMAT "->>,>>>,>>9":U
            WIDTH 13
      ttJobEstCreation.onOrder COLUMN-LABEL "On Order" FORMAT "->>,>>>,>>9":U
            WIDTH 15  
      ttJobEstCreation.allocated COLUMN-LABEL "Allocated" FORMAT "->>,>>>,>>9":U
            WIDTH 15
      ttJobEstCreation.qtyAvail COLUMN-LABEL "Available" FORMAT "->>,>>>,>>9":U
            WIDTH 15
      ttJobEstCreation.availOnHand COLUMN-LABEL "Available On-Hand" FORMAT "->>,>>>,>>9":U
            WIDTH 22              
      ttJobEstCreation.dueDate COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            WIDTH 15  
      ttJobEstCreation.reOrderLevel COLUMN-LABEL "Reorder Level" FORMAT "->>,>>>,>>9":U
            WIDTH 17
      ttJobEstCreation.minOrder COLUMN-LABEL "Minimum Order" FORMAT "->>,>>>,>>9":U
            WIDTH 19
      ttJobEstCreation.maxOrder COLUMN-LABEL "Maximum Order" FORMAT "->>,>>>,>>9":U
            WIDTH 19        
      ttJobEstCreation.custPart COLUMN-LABEL "Customer Part" FORMAT "x(15)":U
            WIDTH 23
      ttJobEstCreation.Customer COLUMN-LABEL "Customer" FORMAT "x(8)":U
            WIDTH 11
      ttJobEstCreation.custName COLUMN-LABEL "Cust Name" FORMAT "x(30)":U
            WIDTH 33
      ttJobEstCreation.estNo COLUMN-LABEL "Estimate" FORMAT "x(8)":U
            WIDTH 10
      ttJobEstCreation.style COLUMN-LABEL "Style" FORMAT "x(8)":U
            WIDTH 10             
      ttJobEstCreation.cat COLUMN-LABEL "Category" FORMAT "x(8)":U
            WIDTH 10
                  
      ENABLE ttJobEstCreation.isSelect
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 157.6 BY 22.38
         FONT 34 ROW-HEIGHT-CHARS .9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExit AT ROW 1.24 COL 149 WIDGET-ID 2
     btFilter AT ROW 1.71 COL 130.4 WIDGET-ID 18
     fieventIDlb AT ROW 1.76 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiCustPartLabel AT ROW 1.76 COL 32.6 NO-LABEL WIDGET-ID 6
     fiCustomerLabel AT ROW 1.76 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiEstLabel AT ROW 1.76 COL 77 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fiStyleLebel AT ROW 1.76 COL 95 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     cFGItem AT ROW 2.81 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     cCustomerPart AT ROW 2.81 COL 30.6 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     cCustomer AT ROW 2.81 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cEst AT ROW 2.81 COL 77 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     cStyle AT ROW 2.81 COL 95 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     fiCatLabel AT ROW 4 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     filocLabel AT ROW 4 COL 26.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     btCrtJob AT ROW 4.95 COL 106 WIDGET-ID 24
     btCrtPo AT ROW 4.95 COL 124.4 WIDGET-ID 78
     cCat AT ROW 5.05 COL 7.2 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     cLoc AT ROW 5.05 COL 26.4 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     BROWSE-2 AT ROW 6.95 COL 2 WIDGET-ID 200
     " Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 3.6 WIDGET-ID 52
          FONT 6
     RECT-26 AT ROW 1.24 COL 2 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


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
         TITLE              = "Item Order Processing"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 cLoc DEFAULT-FRAME */
ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       btCrtJob:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "NoWinReSize".

ASSIGN 
       btCrtPo:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "NoWinReSize".

/* SETTINGS FOR FILL-IN fiCatLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustomerLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustPartLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiEstLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fieventIDlb IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filocLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStyleLebel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttJobEstCreation
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Item Order Processing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Item Order Processing */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttJobEstCreation THEN DO:
        /*RUN api/ResponseInboundDataViewer.w (
            INPUT ttJobEstCreation.apiInboundEventID
            ).   */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF SELF:CURRENT-COLUMN:NAME EQ "isSelect" THEN DO:
        lSelectTrigger = NOT lSelectTrigger.
        
        FOR EACH ttJobEstCreation:
            ttJobEstCreation.isSelect = lSelectTrigger.
        END.
        
        SELF:CURRENT-COLUMN:LABEL = IF lSelectTrigger THEN
                                        "[*] All"
                                    ELSE
                                        "[ ] All".
                                        
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}    
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCrtJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCrtJob C-Win
ON CHOOSE OF btCrtJob IN FRAME DEFAULT-FRAME /* Create Job */
DO:
   // DEFINE VARIABLE riEb AS ROWID NO-UNDO .
   // DEFINE BUFFER bff-eb FOR eb.
   
  //  MESSAGE "Are you sure want to create Job with estimate. " 
  //   VIEW-AS ALERT-BOX QUESTION
  //   BUTTONS OK-CANCEL UPDATE lcheckflg as logical .
  //   IF lcheckflg THEN
  //   DO:
  //   
  //      RUN create-ttfrmout.
  //      
  //      RUN est/BuildEstimate.p ("C", OUTPUT riEb).
  //
  //
  //      FIND FIRST bff-eb NO-LOCK
  //           WHERE bff-eb.company EQ cocode
  //           AND ROWID(bff-eb) EQ riEb NO-ERROR .  
  //       
  //   END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCrtPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCrtPo C-Win
ON CHOOSE OF btCrtPo IN FRAME DEFAULT-FRAME /* Create PO */
DO:
    /*RUN dispatch ('open-query').*/
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


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter C-Win
ON CHOOSE OF btFilter IN FRAME DEFAULT-FRAME /* Filter */
DO:
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData AS LONGCHAR  NO-UNDO.
    
     EMPTY TEMP-TABLE ttJobEstCreation.
     
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&DISPLAYED-OBJECTS}.    
     END.
     cEst = FILL(" ",8 - LENGTH(TRIM(cEst))) + TRIM(cEst).
     FOR EACH itemfg  NO-LOCK
         WHERE itemfg.company    eq cocode
         and (itemfg.cust-no EQ cCustomer OR cCustomer EQ "")       
         and (itemfg.i-no EQ cFGItem OR cFGItem EQ "")
         and (itemfg.part-no EQ cCustomerPart OR cCustomerPart EQ "")
         and (itemfg.est-no EQ cEst OR cEst EQ "")
         and (itemfg.procat EQ cCat OR cCat EQ "")
         AND (itemfg.style EQ cStyle OR cStyle EQ "")
         AND (itemfg.loc LE cLoc OR cLoc EQ "") :
         
         CREATE ttJobEstCreation. 
         ASSIGN
            ttJobEstCreation.fgItem       = itemfg.i-no
            ttJobEstCreation.custPart     = itemfg.part-no
            ttJobEstCreation.Customer     = itemfg.cust-no
            ttJobEstCreation.custName     = itemfg.cust-name
            ttJobEstCreation.estNo        = itemfg.est-no
            ttJobEstCreation.style        = itemfg.style
            ttJobEstCreation.cat          = itemfg.procat
            ttJobEstCreation.onHand       = itemfg.q-onh
            ttJobEstCreation.onOrder      = itemfg.q-ono
            ttJobEstCreation.allocated    = itemfg.q-alloc
            ttJobEstCreation.qtyAvail     = itemfg.q-avail
            ttJobEstCreation.availOnHand  = itemfg.q-onh - itemfg.q-alloc           
            ttJobEstCreation.reOrderLevel = itemfg.ord-level
            ttJobEstCreation.minOrder     = itemfg.ord-max
            ttJobEstCreation.maxOrder     = itemfg.ord-min    .
            
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ cocode 
                 AND cust.cust-no EQ itemfg.cust-no 
                 AND cust.cust-no NE "" NO-ERROR.
             IF avail cust THEN    
             ASSIGN ttJobEstCreation.dueDate = TODAY + cust.ship-days.
            
            IF  lastship-cha = "Stock/Custom" THEN DO:
             /* If fgitem has no estimate. */
              IF itemfg.est-no = "" THEN
                 ASSIGN ttJobEstCreation.dueDate = TODAY + lastship-int.
              ELSE
                 ASSIGN ttJobEstCreation.dueDate = TODAY + INT(lastship-dec).
            END.
            IF lastship-cha eq "Fibre" then
            assign             
             ttJobEstCreation.dueDate  = TODAY + (lastship-int * 1).
            
                    
     END.                                               
  
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCat C-Win
ON HELP OF cCat IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "procat", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN DO:
        cCat:SCREEN-VALUE = lookupField.         
        
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustomer C-Win
ON HELP OF cCustomer IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "cust-no", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN DO:
        cCustomer:SCREEN-VALUE = lookupField.         
        
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem C-Win
ON HELP OF cFGItem IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "i-no", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN DO:
        cFGItem:SCREEN-VALUE = lookupField.         
        
    END.  
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
    RUN enable_UI.

    
    APPLY "entry" TO cFgItem.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{sys/inc/f3help.i}

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
  DISPLAY fieventIDlb fiCustPartLabel fiCustomerLabel fiEstLabel fiStyleLebel 
          cFGItem cCustomerPart cCustomer cEst cStyle fiCatLabel filocLabel cCat 
          cLoc 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-26 btExit btFilter cFGItem cCustomerPart cCustomer cEst cStyle 
         btCrtJob btCrtPo cCat cLoc BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChooseDate C-Win 
PROCEDURE pChooseDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCalendarDate AS CHARACTER NO-UNDO.

    RUN nosweat/popupcal2.w (OUTPUT opcCalendarDate).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

