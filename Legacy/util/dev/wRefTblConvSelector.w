&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
  File:         wRefTblConvSelector.w
  Description:  Interactive Reftable counter/converter
  Input Parameters: <none>
  Output Parameters: <none>
  History:      Original version - 3/17/19 - MYT
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
DEF VAR cRefTableEntry AS CHAR NO-UNDO.
DEF VAR cRefTableList AS CHAR NO-UNDO.
DEF VAR hConvert AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR hTable AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hBufferField AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBrowse AS HANDLE NO-UNDO.
DEF VAR hCol AS HANDLE NO-UNDO.

RUN util/dev/spRefTable.p PERSISTENT SET hConvert.


{src/adm2/widgetprto.i}

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
&Scoped-Define ENABLED-OBJECTS eInstructions bSelectAll bCount bConvert ~
bExit 
&Scoped-Define DISPLAYED-OBJECTS eInstructions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bConvert 
     LABEL "Count and Convert" 
     SIZE 22 BY 1.14.

DEFINE BUTTON bCount 
     LABEL "Count Only" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 19 BY 1.14.

DEFINE BUTTON bSelectAll 
     LABEL "&Select All" 
     SIZE 41 BY .81.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 67 BY 3.33 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     eInstructions AT ROW 1.24 COL 4 NO-LABEL
     bSelectAll AT ROW 19.62 COL 17
     bCount AT ROW 21 COL 5
     bConvert AT ROW 21 COL 26
     bExit AT ROW 21 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.2 BY 21.76.


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
         TITLE              = "Interactive RefTable Analysis and Conversion"
         HEIGHT             = 21.76
         WIDTH              = 73.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Interactive RefTable Analysis and Conversion */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Interactive RefTable Analysis and Conversion */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCount wWin
ON CHOOSE OF bCount IN FRAME fMain /* Count Only */
OR CHOOSE OF bConvert
DO:
    DEFINE VAR cRefTableEntry AS CHAR NO-UNDO.
    DEFINE VAR cProcessList AS CHAR NO-UNDO.
    DEFINE VAR iCount AS INT NO-UNDO.
    DEFINE VAR iProcessed AS INT NO-UNDO.
    
    DO iCtr = 1 TO hBrowse:NUM-SELECTED-ROWS:
        hBrowse:FETCH-SELECTED-ROW(iCtr).
        hBufferField = hBuffer:BUFFER-FIELD(1).
        ASSIGN 
            cRefTableEntry = hBufferField:BUFFER-VALUE.
        IF SELF:NAME EQ "bCount" THEN DO: 
            RUN _epConvert IN hConvert (INPUT cRefTableEntry,
                                        INPUT "Count",
                                        OUTPUT iCount,
                                        OUTPUT iProcessed). 
            hBufferField = hBuffer:BUFFER-FIELD(2).
            ASSIGN 
                hBufferField:BUFFER-VALUE = iCount.
        END.
        ELSE DO: 
            RUN _epConvert IN hConvert (INPUT cRefTableEntry,
                                        INPUT "Convert",
                                        OUTPUT iCount,
                                        OUTPUT iProcessed). 
            hBufferField = hBuffer:BUFFER-FIELD(2).
            ASSIGN 
                hBufferField:BUFFER-VALUE = iCount.
            hBufferField = hBuffer:BUFFER-FIELD(3).
            ASSIGN 
                hBufferField:BUFFER-VALUE = iProcessed.
        END.
    END.
    
    hBrowse:REFRESH().
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME bSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSelectAll wWin
ON CHOOSE OF bSelectAll IN FRAME fMain /* Select All */
DO:
    IF SELF:LABEL = "Deselect All" THEN DO:
        hBrowse:DESELECT-ROWS().
        ASSIGN 
            SELF:LABEL = "Select All".
    END.  
    ELSE DO:
        hBrowse:SELECT-ALL().
        ASSIGN 
            SELF:LABEL = "Deselect All".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

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
  DISPLAY eInstructions 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE eInstructions bSelectAll bCount bConvert bExit 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  RUN SUPER.

CREATE TEMP-TABLE hTable.
hTable:ADD-NEW-FIELD("fcRefTable","CHARACTER",0,"x(32)",?,"RefTable","Reftable Name").
hTable:ADD-NEW-FIELD("fiRecords","Integer",0,">>>>>>>9",0,"Counted","Nbr Records").
hTable:ADD-NEW-FIELD("fiProcessed","Integer",0,">>>>>>>9",0,"Counted","Nbr Processed").
hTable:TEMP-TABLE-PREPARE("ttRTList").

CREATE BUFFER hBuffer FOR TABLE hTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS (hBuffer).
hQuery:QUERY-PREPARE("FOR EACH ttRTList NO-LOCK").

CREATE BROWSE hBrowse 
    ASSIGN QUERY = hQuery
    FRAME = FRAME fMain:HANDLE
    VISIBLE = NO
    MULTIPLE = TRUE 
    SEPARATORS = TRUE 
    SENSITIVE = YES 
    ROW = 4
    COL = 4
    WIDTH = 66
    HEIGHT = 15.53.

DO iCtr = 1 TO 3:
    hBufferField = hBuffer:BUFFER-FIELD (iCtr).
    hCol = hBrowse:ADD-LIKE-COLUMN(hBufferField).
END.

DO iCtr = 1 TO NUM-ENTRIES(hConvert:INTERNAL-ENTRIES):
    IF NOT ENTRY(iCtr,hConvert:INTERNAL-ENTRIES) BEGINS "_" THEN DO:
        hBuffer:BUFFER-CREATE.
        
        hBufferField = hBuffer:BUFFER-FIELD(1).
        hBufferField:BUFFER-VALUE = ENTRY(iCtr,hConvert:INTERNAL-ENTRIES).
        
        hBufferField = hBuffer:BUFFER-FIELD(2).
        hBufferField:BUFFER-VALUE = 0.
        
        hBufferField = hBuffer:BUFFER-FIELD(3).
        hBufferField:BUFFER-VALUE = 0.
    END.
END.

hBrowse:VISIBLE = TRUE.
hQuery:QUERY-OPEN.    

ASSIGN 
    eInstructions:SCREEN-VALUE IN FRAME fMain =
        "Use this function to count remaining RefTable entries in this database and, optionally, convert " +
        "one or more Reftables to their correct values while deleting the selected Reftables.  You may " +
        "select any one or more Reftables to count/convert at one time.".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

