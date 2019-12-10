&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg/r-fgPhys.w

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
DEFINE INPUT  PARAMETER ipcInventoryType AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha NO-UNDO.

DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSnapshotFolder AS CHARACTER NO-UNDO.
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.



DEFINE VARIABLE ll-secure AS LOG NO-UNDO.

DEFINE STREAM excel.
DEFINE STREAM excel2 .
DEFINE VARIABLE fi_file AS CHARACTER NO-UNDO.
IF ipcInventoryType EQ "FG" THEN 
    fi_file = ".\custfiles\invSnapShotFG.csv".
ELSE 
    fi_file = "c:\tmp\r-rmPhys.csv".
     
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cNk1Char AS CHARACTER NO-UNDO.
IF ipcInventoryType EQ "FG" THEN 
    RUN sys/ref/nk1look.p (cocode, "FGCountDefaultPath", "C", NO, NO, "", "", 
                              OUTPUT cNk1Char, OUTPUT lfound).
ELSE                           
    RUN sys/ref/nk1look.p (cocode, "RMCountDefaultPath", "C", NO, NO, "", "", 
                              OUTPUT cNk1Char, OUTPUT lfound).
IF lFound THEN
    cSnapshotFolder = cNk1Char NO-ERROR.    
ELSE
    cSnapshotFolder = ".\custfiles".
    
cSnapshotFolder = RIGHT-TRIM(cSnapshotFolder, "/").
cSnapshotFolder = RIGHT-TRIM(cSnapshotFolder, "\").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 fiTransDate fiFromItem ~
fiEndItem fiFromBin fiToBin fiWhseList tb_fullReport fiSnapshotFile ~
btChooseFile tb_show-fo tb_show-vo tb_prep tb_excel tb_exclude_prep ~
tb_excel2 tb_runExcel fi_file2 btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiTransDate fiFromItem fiEndItem fiFromBin ~
fiToBin fiWhseList tb_fullReport fiSnapshotFolder fiSnapshotFile tb_show-fo ~
tb_show-vo tb_prep tb_excel tb_exclude_prep tb_excel2 tb_runExcel fi_file2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAddSpaceToList C-Win
FUNCTION fAddSpaceToList RETURNS CHARACTER 
  ( ipcList AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChooseFile 
     LABEL "Choose" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiEndItem AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromBin AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Item" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiSnapshotFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "SnapshotFile" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiSnapshotFolder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34.4 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fiToBin AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzz" 
     LABEL "To Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTransDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhseList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Warehouse List" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file2 AS CHARACTER FORMAT "X(50)" INITIAL "c:~\tmp~\r-fgPhys.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 18.81.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.38.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Include location changed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel2 AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_exclude_prep AS LOGICAL INITIAL no 
     LABEL "Include duplicates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fullReport AS LOGICAL INITIAL yes 
     LABEL "Full Report?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prep AS LOGICAL INITIAL no 
     LABEL "Include Qty Changed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_show-fo AS LOGICAL INITIAL no 
     LABEL "Include not in snapshot?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-vo AS LOGICAL INITIAL no 
     LABEL "Include complete match?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiTransDate AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 42
     fiFromItem AT ROW 4.81 COL 19 COLON-ALIGNED WIDGET-ID 26
     fiEndItem AT ROW 4.86 COL 55 COLON-ALIGNED WIDGET-ID 28
     fiFromBin AT ROW 5.76 COL 19 COLON-ALIGNED WIDGET-ID 18
     fiToBin AT ROW 5.81 COL 55 COLON-ALIGNED WIDGET-ID 20
     fiWhseList AT ROW 7.19 COL 19 COLON-ALIGNED WIDGET-ID 30
     tb_fullReport AT ROW 9.57 COL 23 WIDGET-ID 2
     fiSnapshotFolder AT ROW 10.76 COL 37.6 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiSnapshotFile AT ROW 11.76 COL 20 COLON-ALIGNED WIDGET-ID 38
     btChooseFile AT ROW 11.76 COL 75 WIDGET-ID 34
     tb_show-fo AT ROW 15.05 COL 22
     tb_show-vo AT ROW 16 COL 22
     tb_prep AT ROW 16.95 COL 22 WIDGET-ID 4
     tb_excel AT ROW 17.86 COL 53 RIGHT-ALIGNED
     tb_exclude_prep AT ROW 18.81 COL 22 WIDGET-ID 12
     tb_excel2 AT ROW 23.62 COL 22.2
     tb_runExcel AT ROW 23.62 COL 64 RIGHT-ALIGNED
     fi_file2 AT ROW 24.71 COL 20 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.95 COL 22
     btn-cancel AT ROW 26.95 COL 54
     "(Date on the history tab)" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 2.91 COL 40 WIDGET-ID 44
          FGCOLOR 9 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 22.24 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Exceptions" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.86 COL 17 WIDGET-ID 22
          FGCOLOR 9 
     "(Comma separated List)" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 8.38 COL 22 WIDGET-ID 32
     "Snapshot File In:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.95 COL 22 WIDGET-ID 40
          FGCOLOR 12 
     RECT-7 AT ROW 1.24 COL 1
     RECT-8 AT ROW 13.38 COL 1 WIDGET-ID 24
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.19
         SIZE 94.4 BY 27.67.


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
         TITLE              = "FG Physical Analysis & Posting"
         HEIGHT             = 27.95
         WIDTH              = 95
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN fiSnapshotFolder IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiSnapshotFolder:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       fi_file2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fullReport:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-fo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-vo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Physical Analysis  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Physical Analysis  Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChooseFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChooseFile C-Win
ON CHOOSE OF btChooseFile IN FRAME FRAME-A /* Choose */
DO:
      DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
      DEFINE VARIABLE iSlashPos AS INTEGER NO-UNDO.
      DEFINE VARIABLE cFolderFullPath AS CHARACTER NO-UNDO.
   
      file-info:file-name = cSnapshotFolder .
      cFolderFullPath = file-info:full-pathname.
      IF cFolderFullPath = ? OR cFolderFullPath = "" THEN 
        cFolderFullPath = cSnapshotFolder.
      SYSTEM-DIALOG GET-FILE cFileName
        TITLE "Choose Snapshot..."
        FILTERS "CSV Files (*.csv)"  "*.csv"
        USE-FILENAME        
        DEFAULT-EXTENSION ".csv"
        INITIAL-DIR cFolderFullPath.

      IF cFileName NE "" AND cFileName NE ? THEN DO:
        iSlashPos = R-INDEX(cFileName, "\").
        IF iSlashPos GT 0 THEN 
          cFileName = SUBSTRING(cFileName, iSlashPos + 1).
        fiSnapshotFile:SCREEN-VALUE IN FRAME frame-a = cFileName.
      END.
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEFINE VARIABLE lValidEntry AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cInvalidValues AS CHARACTER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
        ASSIGN   fi_file2        
                 fiFromItem
                 fiEndItem
                 fiWhseList                        
                 fiFromBin  
                 fiToBin 
                 tb_show-vo
                 tb_prep   
                 tb_show-fo
                 tb_excel  
                 tb_exclude_prep
                 fiTransDate
                 .
  RUN validateSnapshotFolder (INPUT fiSnapshotFolder, OUTPUT lValidEntry).
  IF NOT lValidEntry THEN DO:
    MESSAGE "The snapshot folder specified is not valid, please correct NK1 value." 
    VIEW-AS ALERT-BOX.
    RETURN.
  END.  
  RUN validateSnapshotFile (INPUT fiSnapshotFolder + "\" + fiSnapshotFile, OUTPUT lValidEntry).
  IF NOT lValidEntry THEN DO:
    MESSAGE "The snapshot file specified could not be found." 
    VIEW-AS ALERT-BOX.
    RETURN.
  END.   
  RUN validateWhseList (INPUT fiWhseList,  OUTPUT cInvalidValues, OUTPUT lValidEntry).
  IF NOT lValidEntry THEN DO:
    MESSAGE "Entry of warehouse list is not valid: " + cInvalidValues
    VIEW-AS ALERT-BOX.
    RETURN.
  END.
         
  RUN run-report. 
  STATUS DEFAULT "Processing Complete". 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndItem C-Win
ON HELP OF fiEndItem IN FRAME FRAME-A /* To Item */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        DEFINE VARIABLE recid-val AS recid NO-UNDO.
        IF ipcInventoryType EQ "FG" THEN DO:
            RUN windows/l-itemfg.w (cocode,"", {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
            DO :
                ASSIGN 
                    {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                    .
            END. 
        END.
        ELSE DO:
            RUN windows/l-itmre.w (cocode,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT recid-val).
            FIND item WHERE RECID(item) EQ recid-val NO-LOCK NO-ERROR.
            IF AVAILABLE item AND item.i-no NE FOCUS:SCREEN-VALUE THEN 
            DO:
                FOCUS:SCREEN-VALUE = item.i-no.                        
            END.  
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromBin C-Win
ON HELP OF fiFromBin IN FRAME FRAME-A /* From Bin */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        IF ipcInventoryType EQ "FG" THEN DO:
            RUN windows/l-fgbin.w (cocode,ENTRY(1, fiWhseList:SCREEN-VALUE), {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
            DO :
                ASSIGN 
                {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                    .
            END. 
        END.
        ELSE DO:
            RUN windows/l-locbin.w (cocode,ENTRY(1, fiWhseList:SCREEN-VALUE),FOCUS:SCREEN-VALUE, OUTPUT char-val).
            FOCUS:SCREEN-VALUE = char-val.            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromItem C-Win
ON HELP OF fiFromItem IN FRAME FRAME-A /* From Item */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        DEFINE VARIABLE recid-val AS recid NO-UNDO.
        IF ipcInventoryType EQ "FG" THEN DO:
            RUN windows/l-itemfg.w (cocode,"", {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN 
            DO :
                ASSIGN 
                    {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                    .
            END. 
        END.
        ELSE DO: 
            RUN windows/l-itmre.w (cocode,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT recid-val).
            FIND item WHERE RECID(item) EQ recid-val NO-LOCK NO-ERROR.
            IF AVAILABLE item AND item.i-no NE FOCUS:SCREEN-VALUE THEN 
            DO:
                FOCUS:SCREEN-VALUE = item.i-no.                        
            END.              
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiToBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiToBin C-Win
ON HELP OF fiToBin IN FRAME FRAME-A /* To Bin */
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.
    IF ipcInventoryType EQ "FG" THEN DO:
        RUN windows/l-fgbin.w (cocode,ENTRY(1,fiWhseList:SCREEN-VALUE), {&SELF-NAME}:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" THEN 
        DO :
            ASSIGN 
                {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .
        END. 
    END.
    ELSE DO:
            RUN windows/l-locbin.w (cocode,ENTRY(1, fiWhseList:SCREEN-VALUE),FOCUS:SCREEN-VALUE, OUTPUT char-val).
            FOCUS:SCREEN-VALUE = char-val.      
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiWhseList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiWhseList C-Win
ON HELP OF fiWhseList IN FRAME FRAME-A /* Warehouse List */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN windows/l-locMulti.w (cocode,ENTRY(1, fiWhseList:SCREEN-VALUE), OUTPUT char-val).
        IF char-val <> "" THEN 
        DO :
            ASSIGN 
            {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val, "|")
                .
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file2 C-Win
ON LEAVE OF fi_file2 IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Include location changed? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel2 C-Win
ON VALUE-CHANGED OF tb_excel2 IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude_prep C-Win
ON VALUE-CHANGED OF tb_exclude_prep IN FRAME FRAME-A /* Include duplicates? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fullReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fullReport C-Win
ON VALUE-CHANGED OF tb_fullReport IN FRAME FRAME-A /* Full Report? */
DO:
        ASSIGN {&self-name}.
    END.
    IF tb_fullReport THEN 
      ASSIGN  
      tb_show-fo:SENSITIVE = FALSE 
      tb_show-vo:SENSITIVE = FALSE
      tb_prep:SENSITIVE = FALSE
      tb_exclude_prep:SENSITIVE = FALSE
 .
    ELSE 
      ASSIGN      tb_show-fo:SENSITIVE = TRUE 
            tb_show-vo:SENSITIVE = TRUE
            tb_prep:SENSITIVE = TRUE
            
            tb_exclude_prep:SENSITIVE = TRUE .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A /* Include Qty Changed? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-fo C-Win
ON VALUE-CHANGED OF tb_show-fo IN FRAME FRAME-A /* Include not in snapshot? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-vo C-Win
ON VALUE-CHANGED OF tb_show-vo IN FRAME FRAME-A /* Include complete match? */
DO:
  ASSIGN {&self-name}.
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
    ASSIGN  
        tb_show-fo:SENSITIVE      = FALSE 
        tb_show-vo:SENSITIVE      = FALSE
        tb_prep:SENSITIVE         = FALSE
        tb_exclude_prep:SENSITIVE = FALSE
        .
/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  
   IF ipcInventoryType EQ "RM" THEN
     c-win:TITLE =  "RM Physical Analysis & Posting".
    
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    fiTransDate:SCREEN-VALUE = STRING(today, "99/99/9999").
    APPLY "entry" TO tb_prep.
    ASSIGN fiSnapshotFolder:SCREEN-VALUE = cSnapshotFolder
           fi_file2:SCREEN-VALUE = (if ipcInventoryType eq "FG" then "c:\tmp\FGPhysCountAnalysis_" ELSE  "c:\tmp\RMPhysCountAnalysis_")
                                   + STRING(YEAR(TODAY), "9999") + STRING(MONTH(today)) + STRING(DAY(TODAY)) 
                                   + REPLACE(STRING(time, "hh:mm"), ":","") + ".csv"            
           .
 
  END.

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
  DISPLAY fiTransDate fiFromItem fiEndItem fiFromBin fiToBin fiWhseList 
          tb_fullReport fiSnapshotFolder fiSnapshotFile tb_show-fo tb_show-vo 
          tb_prep tb_excel tb_exclude_prep tb_excel2 tb_runExcel fi_file2 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 fiTransDate fiFromItem fiEndItem fiFromBin fiToBin 
         fiWhseList tb_fullReport fiSnapshotFile btChooseFile tb_show-fo 
         tb_show-vo tb_prep tb_excel tb_exclude_prep tb_excel2 tb_runExcel 
         fi_file2 btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- jc/rep/job-sum.p 08/94 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */



DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.
DEFINE VARIABLE fg-str-tit AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit2 AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit3 AS cha FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-line AS cha FORM "x(170)" NO-UNDO.

DEFINE VARIABLE mach-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE mach-str-line AS cha FORM "x(150)" NO-UNDO.

DEFINE VARIABLE item-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE item-str-line AS cha FORM "x(150)" NO-UNDO.

DEFINE VARIABLE misc-str-tit AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-tit2 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-tit3 AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE misc-str-line AS cha FORM "x(150)" NO-UNDO.
DEFINE VARIABLE exelHeader AS CHARACTER NO-UNDO.
DEFINE VARIABLE setFromHistory AS LOGICAL NO-UNDO.

SESSION:SET-WAIT-STATE ("general").
    DEFINE VARIABLE h AS HANDLE.  
    IF ipcInventoryType EQ "FG" THEN 
        RUN inventory/cyclecountcompare.p PERSISTENT SET h.
    ELSE 
        RUN inventory/cyclecountcompareRM.p PERSISTENT SET h.
 
    RUN reportComparison IN h
        (INPUT fi_file2,
        INPUT cocode,
        INPUT fiTransDate,
        INPUT fiFromItem,
        INPUT fiEndItem,
        INPUT fAddSpaceToList(fiWhseList),        /* st whse */        
        INPUT fiFromBin,        /* start bin */
        INPUT fiToBin, /* end bin */
        INPUT YES,        /* scans only */
        INPUT tb_show-vo,        /* complete only */
        INPUT tb_prep,        /* qty changed */
        INPUT tb_show-fo,        /* snapshot only */
        INPUT tb_excel,        /* loc changed */
        INPUT tb_exclude_prep,         /* dups in scan */
        INPUT cSnapshotFolder + "\" + fiSnapshotFile
        ).

    DELETE OBJECT h.  
IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.

END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateSnapshotFile C-Win 
PROCEDURE validateSnapshotFile :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFile AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

FILE-INFO:FILE-NAME = cFile.

IF FILE-INFO:FULL-PATHNAME EQ ? OR FILE-INFO:FULL-PATHNAME EQ "" THEN
  oplValid = NO.
ELSE
  oplValid = YES.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateSnapshotFolder C-Win 
PROCEDURE validateSnapshotFolder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFolder AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

FILE-INFO:FILE-NAME = cFolder.
IF FILE-INFO:FULL-PATHNAME EQ ? OR FILE-INFO:FULL-PATHNAME EQ "" THEN 
  OS-CREATE-DIR VALUE(cFolder).
IF FILE-INFO:FULL-PATHNAME EQ ? OR FILE-INFO:FULL-PATHNAME EQ "" THEN
  oplValid = NO.
ELSE
  oplValid = YES.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateWhseList C-Win 
PROCEDURE validateWhseList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcWhseList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcInvalidList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE cInvalidList AS CHARACTER NO-UNDO.
lValid = YES.
DO iIndex = 1 TO NUM-ENTRIES(ipcWhseList):
    IF NOT CAN-FIND(FIRST loc NO-LOCK WHERE loc.company EQ cocode AND loc.loc EQ ENTRY(iIndex, ipcWhseList)) THEN 
       ASSIGN cInvalidList = cInvalidList + "," +  ENTRY(iIndex, ipcWhseList)
              lValid = NO
              .
END.
ASSIGN oplValid = lValid
       opcInvalidList = cInvalidList
       .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAddSpaceToList C-Win
FUNCTION fAddSpaceToList RETURNS CHARACTER 
  ( ipcList AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Needed because some loc values contained spaces at the end
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cList3 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIter AS INTEGER NO-UNDO.
    
    DO iIter = 1 to NUM-ENTRIES(ipcList):
      cList2 = cList2 + TRIM(ENTRY(iIter, ipcList)) + ",".
      cList3 = cList3 + TRIM(ENTRY(iIter, ipcList)) + " " + ",".
    END.
    
    cList2 = trim(cList2, ",").
    cList3 = trim(cList3, ",").
    cResult = cList2 + "," + cList3.
    RETURN cResult.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


