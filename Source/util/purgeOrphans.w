&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
{custom/globdefs.i}
{sys/inc/var.i "new shared"}

DEF VAR cMessage AS CHAR NO-UNDO.
DEF VAR daTargetDate AS DATE NO-UNDO.
DEF VAR hPurge AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR lError AS LOG NO-UNDO.

{src/adm2/widgetprto.i}

ASSIGN
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 eInstructions bPreview bPurge ~
fiEndDate fiOutputDir tbInventory slCompleted tbOrders btnExit 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiEndDate fiOutputDir ~
tbInventory slCompleted tbOrders tbPurchasing tbEstimating tbAccounting ~
fiGroups fiCompleted 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bPreview 
     LABEL "PREVIEW" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE BUTTON bPurge 
     LABEL "PURGE" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 99 BY 5.24 NO-UNDO.

DEFINE VARIABLE fiCompleted AS CHARACTER FORMAT "X(256)":U INITIAL "Completed:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Delete Orphan Records Created Prior To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiGroups AS CHARACTER FORMAT "X(256)":U INITIAL " File Groups to be Purged:" 
      VIEW-AS TEXT 
     SIZE 26 BY .62 NO-UNDO.

DEFINE VARIABLE fiOutputDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Purge data directory" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 6.91.

DEFINE VARIABLE slCompleted AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5.24 NO-UNDO.

DEFINE VARIABLE tbAccounting AS LOGICAL INITIAL no 
     LABEL "Accounting Files (AP/AR/GL txns, Journals, Cash Receipts, Disbursemts, etc.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 80 BY .81 NO-UNDO.

DEFINE VARIABLE tbEstimating AS LOGICAL INITIAL no 
     LABEL "Estimating/Job Files (Estimates, Job Lines)" 
     VIEW-AS TOGGLE-BOX
     SIZE 83 BY .81 NO-UNDO.

DEFINE VARIABLE tbInventory AS LOGICAL INITIAL yes 
     LABEL "Inventory Files (FG, RM, Locations, Bins, Txns, etc.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 56 BY .81 NO-UNDO.

DEFINE VARIABLE tbOrders AS LOGICAL INITIAL no 
     LABEL "Order Processing Files (Order Lines, Releases, BoLs, Invc Lines, etc.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 71 BY .81 NO-UNDO.

DEFINE VARIABLE tbPurchasing AS LOGICAL INITIAL no 
     LABEL "Purchasing Files (PO Lines, Vendor Invoice Lines)" 
     VIEW-AS TOGGLE-BOX
     SIZE 83 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     eInstructions AT ROW 1.48 COL 5 NO-LABEL NO-TAB-STOP 
     bPreview AT ROW 1.48 COL 111
     bPurge AT ROW 3.38 COL 111
     fiEndDate AT ROW 7.19 COL 43 COLON-ALIGNED
     fiOutputDir AT ROW 7.19 COL 82 COLON-ALIGNED
     tbInventory AT ROW 9.57 COL 16
     slCompleted AT ROW 10.05 COL 108 NO-LABEL
     tbOrders AT ROW 10.76 COL 16
     tbPurchasing AT ROW 11.95 COL 16
     tbEstimating AT ROW 13.14 COL 16
     btnExit AT ROW 1.48 COL 137
     tbAccounting AT ROW 14.33 COL 16
     fiGroups AT ROW 8.62 COL 6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiCompleted AT ROW 9.33 COL 106 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     RECT-1 AT ROW 8.86 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 14.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Orphan Records"
         HEIGHT             = 14.95
         WIDTH              = 148
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 148
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 148
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiCompleted IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiCompleted:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiGroups IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbAccounting IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbEstimating IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbPurchasing IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Purge Orphan Records */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Purge Orphan Records */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPreview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPreview wWin
ON CHOOSE OF bPreview IN FRAME fMain /* PREVIEW */
OR CHOOSE OF bPurge
DO:
    DEF VAR cMode AS CHAR NO-UNDO.
    DEF VAR cFileList AS CHAR NO-UNDO.
    DEF VAR iProcessedCount AS INT NO-UNDO.
    
    ASSIGN 
        cMode = IF SELF:NAME EQ "bPreview" THEN "Preview" ELSE "Purge"
        cFileList = (IF tbAccounting:CHECKED THEN "" ELSE "") +
                    (IF tbEstimating:CHECKED THEN "" ELSE "") +  
                    (IF tbInventory:CHECKED THEN  
                        "fg-act,fg-bin,fg-hist,fg-rcpth,fg-rcpts,fg-rctd,fg-rdtl,fg-rdtlh,fg-set,fgcat," +     
                        "item,itemfg,itemfg-loc,itemfgdtl," + 
                        "rm-bin,rm-rcpt,rm-rctd,"
                        ELSE "") +    
                    (IF tbOrders:CHECKED THEN 
                        "oe-bolh,oe-boll,oe-boll-qty," +
                        "oe-ordl,oe-ordm," + 
                        "oe-rel,oe-relh,oe-rell,oe-ship,"
                        ELSE "") + 
                    (IF tbPurchasing:CHECKED THEN "" ELSE "")
        cFileList = REPLACE(cFileList,",,",",")
        cFileList = TRIM(cFileList,","). 

    DO iCtr = 1 TO  NUM-ENTRIES(cFileList):
        STATUS INPUT "Identifying orphans in table: " + ENTRY(iCtr,cFileList) + "...".
        RUN purgeOrphans IN hPurge (cMode,
                                 ENTRY(iCtr,cFileList),
                                 fiEndDate:SCREEN-VALUE,
                                 fiOutputDir:SCREEN-VALUE,
                                 cocode,
                                 OUTPUT iProcessedCount,
                                 OUTPUT lError,
                                 OUTPUT cMessage).
        slCompleted:ADD-LAST(ENTRY(iCtr,cFileList) + " - " + STRING(iProcessedCount)).
        STATUS INPUT "".
    END.
    
    IF lError THEN DO:
        MESSAGE 
            cMessage VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    
    IF cMode EQ "Purge" THEN 
        STATUS INPUT "Generating report and Purging records...".
    ELSE 
        STATUS INPUT "Generating report...".
    
    RUN purgeComplete IN hPurge.
    
    ASSIGN 
        fiOutputDir:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + "OrphanPurge-" + 
                                    STRING(YEAR(TODAY),"9999") +
                                    STRING(MONTH(TODAY),"99") +
                                    STRING(DAY(TODAY),"99") +
                                    "-" + STRING(TIME).  
    STATUS INPUT "Purge Processing Complete".
                                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain
OR MOUSE-SELECT-DOWN OF btnExit
DO:
    APPLY 'window-close' TO wWin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndDate wWin
ON LEAVE OF fiEndDate IN FRAME fMain /* Delete Orphan Records Created Prior To */
DO:
    IF DATE(SELF:SCREEN-VALUE) GT (TODAY - 30) THEN DO:
        MESSAGE 
            "Advantzware recommends keeping ALL records less than 30 days old.  Are you sure?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = STRING(TODAY - 30,"99/99/9999").
            RETURN.
        END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


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
    
    RUN util/purgeProcs.p PERSISTENT SET hPurge.

    ASSIGN 
        eInstructions:SCREEN-VALUE IN FRAME {&frame-name} = 
            "This function will locate and purge selected records in the database if no 'Parent' record can be located, or if certain " +
            "key data is blank." + CHR(10) +
            "You have the option of running this function in 'PREVIEW Mode' if you ONLY want to see a list of records that " +
            "can be purged.  These record lists can be found in the directory specified below." + CHR(10) +
            "In 'PURGE mode', this directory will also contain data files which can be used to restore purged records."
        fiEndDate:SCREEN-VALUE = STRING(TODAY - 30,"99/99/9999")
        fiOutputDir:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + "OrphanPurge-" + 
                                   STRING(YEAR(TODAY),"9999") +
                                   STRING(MONTH(TODAY),"99") +
                                   STRING(DAY(TODAY),"99") +
                                   "-" + STRING(TIME).  
        
    APPLY 'entry' TO fiEndDate.
  
    /* IF NOT THIS-PROCEDURE:PERSISTENT THEN */
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY eInstructions fiEndDate fiOutputDir tbInventory slCompleted tbOrders 
          tbPurchasing tbEstimating tbAccounting fiGroups fiCompleted 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 eInstructions bPreview bPurge fiEndDate fiOutputDir tbInventory 
         slCompleted tbOrders btnExit 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

