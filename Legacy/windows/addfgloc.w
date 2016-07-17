&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
DEF INPUT PARAMETER ipItemfgRow AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opAddedRecord AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}



{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.


/* Local Variable Definitions ---                                       */
  DEF BUFFER bf-itemfg FOR itemfg.
  DEF BUFFER bf-itemfgloc FOR itemfg-loc.
DEFINE VARIABLE fiCreateFor AS CHARACTER   NO-UNDO.
DEFINE VARIABLE fiCopyReOrder AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tgCopyReOrder AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg-loc loc

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 itemfg-loc.loc loc.dscr ~
itemfg-loc.ord-level itemfg-loc.ord-min itemfg-loc.ord-max 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH itemfg-loc WHERE itemfg-loc.company = itemfg.company ~
and itemfg-loc.i-no = itemfg.i-no NO-LOCK, ~
      EACH loc WHERE loc.company = itemfg-loc.company and loc.loc = itemfg-loc.loc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH itemfg-loc WHERE itemfg-loc.company = itemfg.company ~
and itemfg-loc.i-no = itemfg.i-no NO-LOCK, ~
      EACH loc WHERE loc.company = itemfg-loc.company and loc.loc = itemfg-loc.loc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 itemfg-loc loc
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 itemfg-loc
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 loc


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btOk btCancel btCopy BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Exit" 
     SIZE 15 BY 1.43.

DEFINE BUTTON btCopy 
     LABEL "Copy" 
     SIZE 14 BY 1.43.

DEFINE BUTTON btDelete 
     LABEL "Delete" 
     SIZE 15 BY 1.43.

DEFINE BUTTON btOk 
     LABEL "Add" 
     SIZE 14 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      itemfg-loc, 
      loc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      itemfg-loc.loc COLUMN-LABEL "Loc" FORMAT "x(5)":U WIDTH 6.2
      loc.dscr FORMAT "x(30)":U
      itemfg-loc.ord-level FORMAT ">>>,>>>,>>9.999":U WIDTH 14.2
      itemfg-loc.ord-min FORMAT ">>>,>>>,>>9.999":U
      itemfg-loc.ord-max FORMAT ">>>,>>>,>>9.99":U WIDTH 15.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 17.86
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btOk AT ROW 20.71 COL 24 WIDGET-ID 8
     btCancel AT ROW 20.71 COL 77.2 WIDGET-ID 10
     btDelete AT ROW 20.71 COL 52 WIDGET-ID 12
     btCopy AT ROW 20.71 COL 38 WIDGET-ID 14
     BROWSE-2 AT ROW 1.48 COL 3 WIDGET-ID 200
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.8 BY 23.52.


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
         TITLE              = "Add an FG Location"
         HEIGHT             = 23.95
         WIDTH              = 94.6
         MAX-HEIGHT         = 47.67
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 47.67
         VIRTUAL-WIDTH      = 159.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-2 btCopy FRAME-A */
ASSIGN 
       btCopy:AUTO-RESIZE IN FRAME FRAME-A      = TRUE.

/* SETTINGS FOR BUTTON btDelete IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       btOk:AUTO-RESIZE IN FRAME FRAME-A      = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "asi.itemfg-loc WHERE asi.itemfg ...,asi.loc WHERE asi.itemfg-loc ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[1]      = "asi.itemfg-loc.company = asi.itemfg.company
and itemfg-loc.i-no = itemfg.i-no"
     _JoinCode[2]      = "asi.loc.company = asi.itemfg-loc.company and loc.loc = itemfg-loc.loc"
     _FldNameList[1]   > asi.itemfg-loc.loc
"itemfg-loc.loc" "Loc" ? "character" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.loc.dscr
     _FldNameList[3]   > asi.itemfg-loc.ord-level
"itemfg-loc.ord-level" ? ? "decimal" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = asi.itemfg-loc.ord-min
     _FldNameList[5]   > asi.itemfg-loc.ord-max
"itemfg-loc.ord-max" ? ? "decimal" ? ? ? ? ? ? no ? no no "15.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add an FG Location */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add an FG Location */
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
ON ROW-ENTRY OF BROWSE-2 IN FRAME FRAME-A
DO:
  /*   IF fiCopyReOrder:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN 
      APPLY 'value-changed' TO BROWSE browse-2. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME FRAME-A
DO:
    DO WITH FRAME {&FRAME-NAME}:
       /* fiCopyReOrder:SCREEN-VALUE = itemfg-loc.loc:SCREEN-VALUE IN BROWSE browse-2. */
        ENABLE btDelete.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel C-Win
ON CHOOSE OF btCancel IN FRAME FRAME-A /* Exit */
DO:
 
  APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCopy C-Win
ON CHOOSE OF btCopy IN FRAME FRAME-A /* Copy */
DO:
    DEF VAR lCancel AS LOG NO-UNDO.


  
  lCancel = NO.
  RUN promptAddValues (INPUT "COPY", OUTPUT lCancel, OUTPUT fiCreateFor, OUTPUT tgCopyReOrder,
                 OUTPUT fiCopyReOrder).
  IF lCancel THEN
    RETURN NO-APPLY.

    fiCopyReOrder = "".
    ASSIGN tgCopyReOrder = TRUE
           fiCopyReOrder = itemfg-loc.loc:SCREEN-VALUE IN BROWSE browse-2 NO-ERROR.
    IF fiCopyReOrder EQ "" THEN DO:
      MESSAGE "Please select a location from which to copy"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
    END.

  FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company = itemfg.company
        AND itemfg-loc.i-no    = itemfg.i-no
        AND itemfg-loc.loc     = fiCreateFor
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg-loc THEN
    MESSAGE "Inventory already exists for the selected location."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE DO:
    RUN createRecord.
    opAddedRecord = TRUE.
    browse-2:REFRESH() NO-ERROR.
    RUN enable_ui.
 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete C-Win
ON CHOOSE OF btDelete IN FRAME FRAME-A /* Delete */
DO:
  DEF VAR lcLoc AS CHAR.
  DEF VAR v-process AS LOG.
  DEF BUFFER bf-itemfg-loc FOR itemfg-loc.

  fiCopyReOrder = itemfg-loc.loc:SCREEN-VALUE IN BROWSE browse-2 NO-ERROR.
  lcLoc = fiCopyReOrder.

  IF lcLoc EQ "" THEN
    RETURN.

  MESSAGE "Are you sure you want to delete inventory location: " lcLoc +
          " ?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE v-process.

  IF v-process THEN DO:
      IF AVAIL itemfg THEN
      FIND bf-itemfg-loc WHERE bf-itemfg-loc.company EQ itemfg.company
                           AND bf-itemfg-loc.i-no    EQ itemfg.i-no
                           AND bf-itemfg-loc.loc     EQ lcLoc
                         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bf-itemfg-loc THEN DO:
          FIND FIRST fg-bin 
            WHERE fg-bin.company EQ bf-itemfg-loc.company
              AND fg-bin.i-no    EQ bf-itemfg-loc.i-no
              AND fg-bin.loc     EQ bf-itemfg-loc.loc
            NO-LOCK NO-ERROR.
          IF NOT AVAIL fg-bin THEN
              FIND FIRST fg-rctd 
                WHERE fg-rctd.company EQ bf-itemfg-loc.company
                  AND fg-rctd.i-no    EQ bf-itemfg-loc.i-no
                  AND fg-rctd.loc     EQ bf-itemfg-loc.i-no
                NO-LOCK NO-ERROR.
          IF AVAIL fg-bin OR AVAIL fg-rctd THEN DO:
              MESSAGE "Inventory or history exists for this item/location." SKIP
                      "Cannot be deleted."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.
          ELSE do:            
            DELETE bf-itemfg-loc.
             browse-2:REFRESH() NO-ERROR.
            RUN ENABLE_ui IN THIS-PROCEDURE.
            opAddedRecord = TRUE. 
          END.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk C-Win
ON CHOOSE OF btOk IN FRAME FRAME-A /* Add */
DO:
  
  DEF VAR lCancel AS LOG NO-UNDO.
  lCancel = NO.
  RUN promptAddValues (INPUT "ADD", OUTPUT lCancel, OUTPUT fiCreateFor, OUTPUT tgCopyReOrder,
                 OUTPUT fiCopyReOrder).
  IF lCancel THEN
    RETURN NO-APPLY.


  FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company = itemfg.company
        AND itemfg-loc.i-no    = itemfg.i-no
        AND itemfg-loc.loc     = fiCreateFor
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg-loc THEN
    MESSAGE "Inventory already exists for the selected location."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE DO:
    RUN createRecord.
    opAddedRecord = TRUE.
    
    browse-2:REFRESH() NO-ERROR.
/*     RUN adm-initialize. */

    RUN enable_ui.
    /* APPLY 'close' TO THIS-PROCEDURE. */
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
FIND FIRST itemfg WHERE rowid(itemfg) = ipItemfgrow NO-LOCK NO-ERROR.
SUBSCRIBE TO "SelectLoc" ANYWHERE.

/* This is in main block because local initilize is used by the framework */
/* Should not be any itemfg-loc records with a blank loc. If there is one, remove it */
FIND FIRST bf-itemfg WHERE rowid(bf-itemfg) = ipItemfgrow NO-LOCK NO-ERROR.
IF AVAIL bf-itemfg THEN
  FIND FIRST bf-itemfgloc WHERE bf-itemfgloc.company EQ bf-itemfg.company
      AND bf-itemfgloc.i-no EQ bf-itemfg.i-no
      AND bf-itemfgloc.loc EQ "" 
  EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL bf-itemfgloc THEN
   DELETE bf-itemfgloc.

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



  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createRecord C-Win 
PROCEDURE createRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bfItemfg-loc FOR itemfg-loc.
DEF BUFFER bfItemfg FOR itemfg.
FIND FIRST itemfg WHERE ROWID(itemfg) EQ ipItemfgRow NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg THEN
    RETURN.

CREATE itemfg-loc.
ASSIGN itemfg-loc.company = itemfg.company
       itemfg-loc.i-no    = itemfg.i-no
       itemfg-loc.loc     = fiCreateFor.


IF tgCopyReOrder THEN DO:
    FIND FIRST bfItemfg-loc 
        WHERE bfItemfg-loc.company EQ itemfg.company
          AND bfItemfg-loc.i-no    EQ itemfg.i-no
          AND bfItemfg-loc.loc     EQ fiCopyReOrder
        NO-LOCK NO-ERROR.         
    
    IF AVAIL bfItemfg-loc THEN
      ASSIGN 
        itemfg-loc.ord-min      = bfItemfg-loc.ord-min
        itemfg-loc.ord-max      = bfItemfg-loc.ord-max
        itemfg-loc.ord-level    = bfItemfg-loc.ord-level
        itemfg-loc.lead-days    = bfItemfg-loc.lead-days.

    IF fiCopyReOrder = "ALL" THEN DO:
 

          ASSIGN
            itemfg-loc.ord-min      = Itemfg.ord-min
            itemfg-loc.ord-max      = Itemfg.ord-max
            itemfg-loc.ord-level    = Itemfg.ord-level
            itemfg-loc.lead-days    = Itemfg.lead-days.
  
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
  ENABLE btOk btCancel btCopy BROWSE-2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptAddValues C-Win 
PROCEDURE promptAddValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcMode AS CHARACTER   NO-UNDO.
DEF OUTPUT PARAMETER oplCancel   AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opclocation   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oplCopyLevels  AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opcCopyFrom AS CHAR NO-UNDO.

DEFINE VARIABLE lcUserPrompt AS CHARACTER INIT "".

DEFINE VARIABLE lclocation     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE choice       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcCopyFrom   AS CHAR      NO-UNDO.
DEFINE VARIABLE llReplace    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE opiNewOrder  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lvErrMsg     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lvcDefaultlocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvcDefaultPo AS CHARACTER NO-UNDO.


IF ipcMode EQ "ADD" THEN
ip-parms = 
   /* Box Title */
   "type=literal,name=label11,row=2,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)" 
      
    /* Set Attributes */
    + "|type=attrib,name=cust-no,row=1,col=1,enable=false,width=2,inpval=" + itemfg.i-no
    + "|type=attrib,name=company,row=1,col=1,enable=false,width=2,inpval=" + cocode
    + "|type=attrib,name=loc,row=1,col=1,enable=false,width=2,inpval=" + locode
    

    /* Warehouse Location Code */
    + "|type=literal,name=label6,row=2.2,col=31,enable=false,width=58,scrval=" + "Add Location:" + ",FORMAT=X(58)"
    + "|type=fill-in,name=locField,row=2,col=45,enable=true,width=15,initial=" + lvcDefaultlocation


    /* Copy Reorder Levels toggle box */
 /*   + "|type=literal,name=label9,row=3.5,col=28,enable=false,width=38,scrval=" + "Copy Re-order Level?        From:" + ",FORMAT=X(58)" */
    + "|type=toggle,name=tb_replace,row=3.5,col=25,enable=true,width=32,data-type=logical,label=Copy Re-order level"

    /* Copy levels from location fill-in */
   /* + "|type=literal,name=label10,row=6,col=52,enable=false,width=9,scrval=" + "Del Date:" + ",FORMAT=X(9)" */
    + "|type=literal,name=label9,row=3.5,col=53,enable=false,width=30,depfield=tb_replace,scrval=" + "From:" + ",FORMAT=X(38)" 
    + "|type=fill-in,name=fi_FromLoc,row=3.3,col=62,enable=true,width=15,depfield=tb_replace"
    

    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
    /* Box Title */
    + "|type=win,name=fi3,enable=true,label=         Add Warehouse Location,FORMAT=X(30),height=11".
ELSE
  ip-parms = 
     /* Box Title */
     "type=literal,name=label11,row=2,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)" 

      /* Set Attributes */
      + "|type=attrib,name=cust-no,row=1,col=1,enable=false,width=2,inpval=" + itemfg.i-no
      + "|type=attrib,name=company,row=1,col=1,enable=false,width=2,inpval=" + cocode
      + "|type=attrib,name=loc,row=1,col=1,enable=false,width=2,inpval=" + locode


      /* Warehouse Location Code */
      + "|type=literal,name=label6,row=2.2,col=31,enable=false,width=58,scrval=" + "Add Location:" + ",FORMAT=X(58)"
      + "|type=fill-in,name=locField,row=2,col=45,enable=true,width=15,initial=" + lvcDefaultlocation

      + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
      /* Box Title */
      + "|type=win,name=fi3,enable=true,label=         Add Warehouse Location,FORMAT=X(30),height=11".

prompt-loop:
DO WHILE TRUE:

    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    
    /* Process values using names given above */
    DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
        IF ENTRY(i, op-values) EQ "default" THEN
          choice = ENTRY(i + 1, op-values) NO-ERROR.

        /* Ship To */
        IF ENTRY(i, op-values) EQ "locField" THEN
          lclocation = ENTRY(i + 1, op-values) NO-ERROR.            

        /* Replace Existing */
        IF ENTRY(i, op-values) EQ "tb_replace" THEN
          llReplace = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 

        /* Delete Date */
        IF ENTRY(i, op-values) EQ "fi_FromLoc" THEN
          lcCopyFrom = ENTRY(i + 1, op-values) NO-ERROR. 

    END.

    lvErrMsg = "".
    IF choice NE "CANCEL" THEN DO:
    
        FIND FIRST loc WHERE loc.company = cocode             
              AND loc.loc EQ lclocation
            NO-LOCK NO-ERROR.
        IF NOT AVAIL loc THEN
            lvErrMsg = "Please enter a valid location id".
        
        IF llReplace THEN DO:
          FIND FIRST loc WHERE loc.company = cocode             
                AND loc.loc EQ lcCopyFrom
              NO-LOCK NO-ERROR.
          IF NOT AVAIL loc THEN
              lvErrMsg = "Please enter a valid location id from which to copy reorder levels.".          
        END.

    END.

    IF lvErrMsg GT "" THEN 
      MESSAGE lvErrMsg VIEW-AS ALERT-BOX.
    ELSE
        LEAVE.
END.

ASSIGN
 oplCancel = IF choice EQ "CANCEL" THEN TRUE ELSE FALSE
 opclocation = lclocation
 opcCopyFrom = lcCopyFrom
 oplCopyLevels  = llReplace.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectLoc C-Win 
PROCEDURE selectLoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-loc AS CHAR NO-UNDO.

fiCreateFor = ip-loc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

