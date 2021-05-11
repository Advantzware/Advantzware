&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/wCleanseVendorCostMatrix.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  Rahul Rawat

  Created: 8th  March, 2021

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
    DEFINE VARIABLE cLocation     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cocode        AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttVendItemCost
    FIELD company       AS CHARACTER LABEL "Company"
    FIELD itemType      AS CHARACTER LABEL "Item Type"
    FIELD itemID        AS CHARACTER LABEL "Item" FORMAT "X(15)"
    FIELD vendorID      AS CHARACTER LABEL "Vendor"
    FIELD customerID    AS CHARACTER LABEL "Customer"      
    FIELD estimateNo    AS CHARACTER LABEL "Estimate"     
    FIELD formNo        AS INTEGER   LABEL "Form"
    FIELD blankNo       AS INTEGER   LABEL "Blank"
    FIELD effectiveDate AS DATE      LABEL "Effective Date"
    FIELD oldExpiryDate AS DATE      LABEL "Old Expiry Date"
    FIELD newExpiryDate AS DATE      LABEL "New Expiry Date"
    FIELD cDescription  AS CHARACTER LABEL "Description"      
    .  
   
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

    
RUN spGetSessionParam (
    INPUT "Company", 
    OUTPUT cocode
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit RECT-15 RECT-16 cb_itemType ~
fiBeginingItem fiEndingItem fiBeginingCustomer fiEndingCustomer ~
btSimulatePurge fiBeginEst fiEndEst fiBeginVend fiEndVend rd_action ~
fiDirectory tbOpenFile btStartProcess 
&Scoped-Define DISPLAYED-OBJECTS cb_itemType fiBeginingItem fiEndingItem ~
fiBeginingCustomer fiEndingCustomer fiBeginEst fiEndEst fiBeginVend ~
fiEndVend lbl_action rd_action fiDirectory tbOpenFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 7.2 BY 1.71.

DEFINE BUTTON btSimulatePurge 
     IMAGE-UP FILE "Graphics/32x32/simulate.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Simulate Purge" 
     SIZE 19 BY 1.14
     BGCOLOR 14 .

DEFINE BUTTON btStartProcess 
     IMAGE-UP FILE "Graphics/32x32/execute.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Start Process" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE cb_itemType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","FG","RM" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Select Type Filter" NO-UNDO.

DEFINE VARIABLE fiBeginEst AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begining Estimate" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginingCustomer AS CHARACTER FORMAT "X(10)":U 
     LABEL "Begining Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginingItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "Begining Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiBeginVend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begining Vendor" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64.8 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndEst AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ending Estimate" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndingCustomer AS CHARACTER FORMAT "X(10)":U 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndingItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "Ending Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE fiEndVend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ending Vendor" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_action AS CHARACTER FORMAT "X(256)":U INITIAL "Action?" 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1 NO-UNDO.

DEFINE VARIABLE rd_action AS CHARACTER INITIAL "Update" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Update", "Update",
"Delete", "Delete"
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 86.6 BY 1.91
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 12.1.

DEFINE VARIABLE tbOpenFile AS LOGICAL INITIAL no 
     LABEL "Open File" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.4 BY .81
     FONT 22 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExit AT ROW 1.14 COL 79.4 WIDGET-ID 24
     cb_itemType AT ROW 3.81 COL 22 COLON-ALIGNED WIDGET-ID 40
     fiBeginingItem AT ROW 5.29 COL 22 COLON-ALIGNED WIDGET-ID 6
     fiEndingItem AT ROW 5.29 COL 61.2 COLON-ALIGNED WIDGET-ID 8
     fiBeginingCustomer AT ROW 6.52 COL 22 COLON-ALIGNED WIDGET-ID 2
     fiEndingCustomer AT ROW 6.52 COL 61.2 COLON-ALIGNED WIDGET-ID 4
     btSimulatePurge AT ROW 15.52 COL 22.4 WIDGET-ID 10
     fiBeginEst AT ROW 7.76 COL 22 COLON-ALIGNED WIDGET-ID 42
     fiEndEst AT ROW 7.76 COL 61.2 COLON-ALIGNED WIDGET-ID 44
     fiBeginVend AT ROW 9 COL 22 COLON-ALIGNED WIDGET-ID 46
     fiEndVend AT ROW 9 COL 61.2 COLON-ALIGNED WIDGET-ID 48
     lbl_action AT ROW 10.48 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     rd_action AT ROW 10.52 COL 34 NO-LABEL WIDGET-ID 52
     fiDirectory AT ROW 12.95 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     tbOpenFile AT ROW 13.05 COL 70.4 WIDGET-ID 26
     btStartProcess AT ROW 15.52 COL 49.6 WIDGET-ID 12
     "Records to view  will be stored in directory:" VIEW-AS TEXT
          SIZE 50.6 BY .62 AT ROW 11.95 COL 11.4 WIDGET-ID 16
          FONT 22
     RECT-15 AT ROW 1 COL 1 WIDGET-ID 20
     RECT-16 AT ROW 2.95 COL 3 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.8 BY 16.33
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
         TITLE              = "Cleanse Vendor Cost Matrix"
         HEIGHT             = 16.33
         WIDTH              = 86.6
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
ASSIGN 
       fiDirectory:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lbl_action IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lbl_action:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "rd_action".

ASSIGN 
       rd_action:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cleanse Price Matrix */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cleanse Price Matrix */
DO:
  IF VALID-HANDLE(hdOutputProcs) THEN 
      DELETE PROCEDURE hdOutputProcs.
          
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
    DEFINE VARIABLE cMainField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAllFields  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recRecordID AS RECID     NO-UNDO.
    
    CASE FOCUS:NAME :
        WHEN "fiBeginingCustomer" OR 
        WHEN "fiEndingCustomer"  THEN DO:
            RUN system/openLookup.p (
            INPUT  cocode, 
            INPUT  "",  /* Lookup ID */
            INPUT  23,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.
        WHEN "fiBeginingItem" OR 
        WHEN "fiEndingItem"  THEN 
            DO:
                IF cb_itemType:SCREEN-VALUE EQ "FG" THEN do:
                      RUN system/openlookup.p (cocode, "i-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
                      IF cMainField <> "" THEN FOCUS:SCREEN-VALUE = cMainField. 
                END.
                ELSE IF cb_itemType:SCREEN-VALUE EQ "RM" THEN DO:
                      RUN system/openlookup.p (cocode, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
                      IF cMainField <> "" THEN FOCUS:SCREEN-VALUE = cMainField. 
                END.
                     
            END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulatePurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulatePurge C-Win
ON CHOOSE OF btSimulatePurge IN FRAME DEFAULT-FRAME /* Simulate Purge */
DO:
    RUN pRunProcess(
        INPUT NO
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess C-Win
ON CHOOSE OF btStartProcess IN FRAME DEFAULT-FRAME /* Start Process */
DO:
    RUN pRunProcess(
        INPUT YES
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_action
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_action C-Win
ON VALUE-CHANGED OF rd_action IN FRAME DEFAULT-FRAME
DO:
  assign {&self-name}.
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
  RUN enable_UI.
  pfSetDirectory(). 
  cb_itemType:SCREEN-VALUE = "ALL".
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
  DISPLAY cb_itemType fiBeginingItem fiEndingItem fiBeginingCustomer 
          fiEndingCustomer fiBeginEst fiEndEst fiBeginVend fiEndVend lbl_action 
          rd_action fiDirectory tbOpenFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btExit RECT-15 RECT-16 cb_itemType fiBeginingItem fiEndingItem 
         fiBeginingCustomer fiEndingCustomer btSimulatePurge fiBeginEst 
         fiEndEst fiBeginVend fiEndVend rd_action fiDirectory tbOpenFile 
         btStartProcess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplExpire AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lInactiveFound AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttVendItemCost.
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    MESSAGE "Do you want to start the process with the selected parameters?"
        VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO
        UPDATE lResponse AS LOGICAL.
    
    IF NOT lResponse THEN 
        RETURN.
            
    DO WITH FRAME {&FRAME-NAME}:
    END.

    SESSION:SET-WAIT-STATE("General"). 
    STATUS DEFAULT "Processing...".  
    
    IF NOT iplExpire THEN    
        RUN FileSys_CreateDirectory(
            INPUT  cLocation,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ). 
              
    FOR EACH vendItemCost NO-LOCK 
        WHERE (vendItemCost.itemType BEGINS cb_itemType:SCREEN-VALUE OR cb_itemType:SCREEN-VALUE EQ "ALL")
          AND vendItemCost.customerID GE fiBeginingCustomer:SCREEN-VALUE
          AND vendItemCost.customerID LE fiEndingCustomer:SCREEN-VALUE 
          AND vendItemCost.itemID     GE fiBeginingItem:SCREEN-VALUE
          AND vendItemCost.itemID     LE fiEndingItem:SCREEN-VALUE
          AND vendItemCost.vendorID   GE fiBeginVend:SCREEN-VALUE
          AND vendItemCost.vendorID   LE fiEndVend:SCREEN-VALUE
        BREAK BY vendItemCost.itemType               
              BY vendItemCost.itemID              
              BY vendItemCost.effectiveDate
              BY vendItemCost.expirationDate:
        lInactiveFound = NO.      
        IF vendItemCost.customerID NE "" THEN DO: 
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ vendItemCost.company 
                   AND cust.cust-no EQ vendItemCost.customerID NO-ERROR.
            IF (avail cust AND cust.ACTIVE EQ "I") OR NOT AVAIL cust THEN DO:
               lInactiveFound = YES.
               IF NOT iplExpire THEN
               DO:
                     CREATE ttVendItemCost.
                     BUFFER-COPY vendItemCost TO ttVendItemCost.
                     ASSIGN
                     ttVendItemCost.oldExpiryDate  =  vendItemCost.expirationDate
                     ttVendItemCost.newExpiryDate  =  TODAY
                     ttVendItemCost.cDescription   =  IF AVAIL cust THEN "Customer Inactive" ELSE "Customer not exist"                     
                     .
               END.                             
                     
            END.                         
        END.  
        IF vendItemCost.vendorID NE "" THEN DO: 
           FIND FIRST vend NO-LOCK
                 WHERE vend.company EQ vendItemCost.company  
                   AND vend.vend-no EQ vendItemCost.vendorID NO-ERROR.
                   
            IF (avail vend AND vend.active EQ "I") OR NOT AVAIL vend THEN DO: 
               lInactiveFound = YES.
               IF NOT iplExpire THEN
               DO:
                     CREATE ttVendItemCost.
                     BUFFER-COPY vendItemCost TO ttVendItemCost.
                     ASSIGN
                     ttVendItemCost.oldExpiryDate  =  vendItemCost.expirationDate
                     ttVendItemCost.newExpiryDate  =  TODAY
                     ttVendItemCost.cDescription   =  IF AVAIL vend THEN "Vendor Inactive" ELSE "Vendor not exist "                      
                     .
               END.      
            END.             
        END.
        IF vendItemCost.itemID NE "" THEN DO: 
           IF vendItemCost.itemType EQ "RM" THEN
           DO:
              FIND FIRST ITEM NO-LOCK
                   WHERE ITEM.company EQ vendItemCost.company 
                     AND ITEM.i-no EQ vendItemCost.itemID NO-ERROR.
              IF (AVAIL ITEM AND ITEM.stat EQ "I") OR NOT AVAIL ITEM THEN DO:  
                 lInactiveFound = YES.
                 IF NOT iplExpire THEN
                 DO:
                     CREATE ttVendItemCost.
                     BUFFER-COPY vendItemCost TO ttVendItemCost.
                     ASSIGN
                     ttVendItemCost.oldExpiryDate  = vendItemCost.expirationDate
                     ttVendItemCost.newExpiryDate  = TODAY
                     ttVendItemCost.cDescription   = IF AVAIL ITEM THEN "RM Item Inactive" ELSE "RM Item not exist"                     
                     .
                 END.   
              END.
           END.
           ELSE IF vendItemCost.itemType EQ "FG" THEN
           DO:
              FIND FIRST itemfg NO-LOCK
                   WHERE itemfg.company EQ vendItemCost.company 
                     AND itemfg.i-no EQ vendItemCost.itemID NO-ERROR.
              IF (AVAIL itemfg AND itemfg.stat EQ "I") OR NOT AVAIL itemfg THEN DO:
                 lInactiveFound = YES.
                 IF NOT iplExpire THEN
                 DO:
                     CREATE ttVendItemCost.
                     BUFFER-COPY vendItemCost TO ttVendItemCost.
                     ASSIGN
                     ttVendItemCost.oldExpiryDate  =  vendItemCost.expirationDate
                     ttVendItemCost.newExpiryDate  =  TODAY
                     ttVendItemCost.cDescription   =  IF AVAIL itemfg THEN "FG Item Inactive" ELSE "FG Item not exist"                      
                     .
                 END.    
              END.
           END.            
                     
        END. /* vendItemCost.itemID NE ""*/    
        
        IF lInactiveFound AND iplExpire THEN
        DO:
           FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
                WHERE rowid(bf-vendItemCost) EQ rowid(vendItemCost) NO-ERROR.
           IF rd_action:SCREEN-VALUE EQ "Update" THEN
           bf-vendItemCost.expirationDate = TODAY.
           ELSE DELETE bf-vendItemCost.
           RELEASE bf-vendItemCost.            
        END.
        
    END.
    IF NOT iplExpire THEN DO:
        RUN Output_TempTableToCSV IN hdOutputProcs (
            INPUT TEMP-TABLE ttVendItemCost:HANDLE,
            INPUT cLocation + "\VendorCostMatrix.csv",
            INPUT TRUE,  /* Export Header */
            INPUT FALSE, /* Auto increment File name */
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        MESSAGE "Simulation Completed." SKIP
            "Check " + TRIM(cLocation) + " directory for the CSV file."
            VIEW-AS ALERT-BOX INFORMATION.   
       IF tbOpenFile:CHECKED THEN 
            RUN OS_RunFile(
                INPUT cLocation + "\VendorCostMatrix.csv",
                OUTPUT lSuccess,
                OUTPUT cMessage).        
    END.   
    ELSE 
        MESSAGE "Process Completed"
            VIEW-AS ALERT-BOX INFORMATION.  
    
    pfSetDirectory().  
    EMPTY TEMP-TABLE ttVendItemCost.    
    SESSION:SET-WAIT-STATE("").   
    STATUS DEFAULT "".                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pfSetDirectory C-Win 
FUNCTION pfSetDirectory RETURNS LOGICAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN FileSys_GetTempDirectory(
        OUTPUT cLocation
        ).
    cLocation = cLocation + "\ExpireVendorCostMatrix-" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")+ STRING(DAY(TODAY),"99") + "-" + STRING(TIME,"99999").  
    DO WITH FRAME {&FRAME-NAME}:
        fiDirectory:SCREEN-VALUE = cLocation.
    END.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

