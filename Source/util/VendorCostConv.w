&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:             util/fgHistoryPurge.w
  Description:      Utility to Purge fg-rcpth and fg-rdtl records with summary txns 
  Input Parms:      <none>
  Output Parms:     <none>
  Author:           Brad Vigrass
  Created:          00/00/2018
------------------------------------------------------------------------*/
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

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}

DEFINE VARIABLE ghConverter      AS HANDLE.     
DEFINE VARIABLE gcTimeString     AS CHARACTER.

ASSIGN 
    cocode = gcompany
    locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiOutputFile fiOutputFolder tgPurge ~
btn-process btn-cancel btnBrowseFolder tgConvertFarm tgConvertRM ~
tgConvertFG tgIncludeInactiveFG tgIncludeInactiveVend tgIncludeInactiveCust ~
RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fiOutputFile fiOutputFolder tgPurge ~
tgConvertFarm tgConvertRM tgConvertFG tgIncludeInactiveFG ~
tgIncludeInactiveVend tgIncludeInactiveCust 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btnBrowseFolder 
     LABEL "Select Log Folder" 
     SIZE 30 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE VARIABLE fiOutputFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output File Name" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE fiOutputFolder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Output Folder" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 11.43.

DEFINE VARIABLE tgConvertFarm AS LOGICAL INITIAL yes 
     LABEL "Convert Estimate Farm Tab Data?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgConvertFG AS LOGICAL INITIAL yes 
     LABEL "Convert Finished Good Vend Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgConvertRM AS LOGICAL INITIAL yes 
     LABEL "Convert Raw Material Vend Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgIncludeInactiveCust AS LOGICAL INITIAL no 
     LABEL "Include Inactive Customers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgIncludeInactiveFG AS LOGICAL INITIAL no 
     LABEL "Include Inactive Finished Goods?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgIncludeInactiveVend AS LOGICAL INITIAL no 
     LABEL "Include Inactive Vendors?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgPurge AS LOGICAL INITIAL no 
     LABEL "Purge Data Before Conversion?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiOutputFile AT ROW 14.33 COL 19 COLON-ALIGNED WIDGET-ID 20
     fiOutputFolder AT ROW 13.1 COL 19 COLON-ALIGNED WIDGET-ID 18
     tgPurge AT ROW 6 COL 21 WIDGET-ID 14
     btn-process AT ROW 16.95 COL 34
     btn-cancel AT ROW 16.95 COL 66.6
     btnBrowseFolder AT ROW 13.1 COL 94.2 WIDGET-ID 24
     tgConvertFarm AT ROW 6.95 COL 21 WIDGET-ID 28
     tgConvertRM AT ROW 7.91 COL 21 WIDGET-ID 30
     tgConvertFG AT ROW 8.86 COL 21 WIDGET-ID 32
     tgIncludeInactiveFG AT ROW 9.81 COL 21 WIDGET-ID 34
     tgIncludeInactiveVend AT ROW 10.76 COL 21 WIDGET-ID 36
     tgIncludeInactiveCust AT ROW 11.71 COL 21 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.05 COL 6
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 17.71.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 26
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 21
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Convert Vendor Cost Tables"
         HEIGHT             = 17.71
         WIDTH              = 126.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 126.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 126.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-AFTER-TAB-ITEM (tgPurge:HANDLE IN FRAME FRAME-A)
       XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (btn-process:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Convert Vendor Cost Tables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Convert Vendor Cost Tables */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:

        IF cocode = "" THEN cocode = "001".
        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CLOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
    DELETE OBJECT ghConverter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder C-Win
ON CHOOSE OF btnBrowseFolder IN FRAME FRAME-A /* Select Log Folder */
DO:
        RUN pFolderBrowse(fiOutputFolder:HANDLE, "").  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgPurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgPurge C-Win
ON VALUE-CHANGED OF tgPurge IN FRAME FRAME-A /* Purge Data Before Conversion? */
DO:

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
   
    RUN util\dev\VendorCostConvProcs.p PERSISTENT SET ghConverter.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghConverter).
  
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
  
        ASSIGN
            gcTimeString = STRING(MTIME, "HH:MM:SS")
            fiOutputFolder:SCREEN-VALUE = "c:\tmp"
            fiOutputFile:SCREEN-VALUE  = "VendorCostConv_" 
                                              + STRING(DAY(TODAY), "99") 
                                              + STRING(MONTH(TODAY), "99") 
                                              + STRING(YEAR(TODAY), "9999") + "_" 
                                              + SUBSTRING(gcTimeString, 1, 2)
                                              + SUBSTRING(gcTimeString, 4, 2)
                                              + SUBSTRING(gcTimeString, 7, 2)
                                              + ".csv"

            .
    END.
    {methods/nowait.i}
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
  DISPLAY fiOutputFile fiOutputFolder tgPurge tgConvertFarm tgConvertRM 
          tgConvertFG tgIncludeInactiveFG tgIncludeInactiveVend 
          tgIncludeInactiveCust 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fiOutputFile fiOutputFolder tgPurge btn-process btn-cancel 
         btnBrowseFolder tgConvertFarm tgConvertRM tgConvertFG 
         tgIncludeInactiveFG tgIncludeInactiveVend tgIncludeInactiveCust 
         RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFolderBrowse C-Win 
PROCEDURE pFolderBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.

    SYSTEM-DIALOG GET-DIR cFolder 
        TITLE "Select Folder for Log File"
        UPDATE lOK.
 
    IF lOK THEN iphdFolderEntry:SCREEN-VALUE = cFolder.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE cFolderSlash AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimer AS INTEGER NO-UNDO. 
    DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            fiOutputFile 
            fiOutputFolder  
            tgPurge
            tgConvertFarm
            tgConvertRM
            tgConvertFG
            tgIncludeInactiveFG
            tgIncludeInactiveVend
            tgIncludeInactiveCust
            .
    END.
    IF fiOutputFile EQ "" THEN 
    DO:
        MESSAGE "Output file name cannot be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    IF fiOutputFolder EQ "" THEN 
    DO:
        MESSAGE "Output folder name cannot be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
        
    OS-CREATE-DIR VALUE(fiOutputFolder).
    FILE-INFO:FILE-NAME = fiOutputFolder.
    IF FILE-INFO:FULL-PATHNAME = ? THEN 
    DO:
        MESSAGE "Output Folder could not be created." VIEW-AS ALERT-BOX.
        RETURN.  
    END.
    
    ASSIGN 
        iTimer         = TIME
        lPurge         = tgPurge
        cOutputFile    = fiOutputFolder + "/" + fiOutputFile
        .
    IF tgPurge THEN DO:
        MESSAGE 
            "Are you sure you want to purge all new Vendor Cost records?" 
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lPurge.
        IF lPurge THEN RUN PurgeAllNewForCompany(cocode, OUTPUT lError, OUTPUT cMessage).
        IF lError THEN 
            MESSAGE "Error occurred during purge:" SKIP 
            cMessage
            VIEW-AS ALERT-BOX.
        ELSE 
            MESSAGE cMessage VIEW-AS ALERT-BOX.
    END.
    RUN ConvertLegacyToNew(cocode, tgConvertFarm, tgConvertRM, tgConvertFG, tgIncludeInactiveFG, tgIncludeInactiveVend, tgIncludeInactiveCust, cOutputFile, OUTPUT lError, OUTPUT cMessage).
    IF lError THEN 
        MESSAGE "Error occurred during conversion:" SKIP 
        cMessage
        VIEW-AS ALERT-BOX.
    ELSE 
        MESSAGE cMessage VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

