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

{src/adm2/widgetprto.i}
{util/ttPurge.i NEW}

/*
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
*/
{sys/inc/var.i new shared}

DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cOutDir AS CHAR NO-UNDO.
DEF VAR lVerbose AS LOG NO-UNDO INITIAL FALSE.
DEFINE VARIABLE hPurgeProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE hParmBegin AS HANDLE EXTENT 10 NO-UNDO.
DEFINE VARIABLE hParmEnd AS HANDLE EXTENT 10 NO-UNDO.
DEFINE VARIABLE hBeginLabel AS HANDLE EXTENT 10 NO-UNDO.

/*ASSIGN               */
/*    cocode = gcompany*/
/*    locode = gloc.   */
    
IF cocode EQ "" THEN ASSIGN 
    cocode = "001"
    locode = "MAIN".    


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
&Scoped-Define ENABLED-OBJECTS RECT-36 RECT-37 RECT-38 btExit cbGroup ~
BUTTON-1 slPurges btAnalyze btPurge rsCompany tbOrphan 
&Scoped-Define DISPLAYED-OBJECTS cbGroup slPurges rsCompany tbOrphan ~
fiOutputDir fiFileName fiToPurge fiFilter fiBegin fiEnd fiOptions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAnalyze 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U
     LABEL "Analyze" 
     SIZE 11 BY 2.62 TOOLTIP "Analyze".

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U
     LABEL "Exit" 
     SIZE 11 BY 2.62 TOOLTIP "Exit".

DEFINE BUTTON btPurge 
     IMAGE-UP FILE "Graphics/32x32/cut.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U
     LABEL "Purge" 
     SIZE 11 BY 2.62 TOOLTIP "Purge".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "Graphics/16x16/folder_open.png":U
     LABEL "Open/Select" 
     SIZE 6 BY 1.14 TOOLTIP "Open/Select a File".

DEFINE VARIABLE cbGroup AS CHARACTER FORMAT "X(256)":U INITIAL "EST" 
     LABEL "Group" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEM-PAIRS "Estimating/Quotes","EST",
                     "Orders/Accts Receivable","ORD",
                     "Materials/Finished Goods","INV",
                     "Jobs/Scheduling","JOB",
                     "Purchasing/Accounts Payable","PUR",
                     "General Ledger","FIN",
                     "System","SYS"
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiBegin AS CHARACTER FORMAT "X(256)":U INITIAL "BEGIN" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE fiEnd AS CHARACTER FORMAT "X(256)":U INITIAL "END" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Review File Name" 
      VIEW-AS TEXT 
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE fiFilter AS CHARACTER FORMAT "X(256)":U INITIAL "  Filter By:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "  Options:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE fiOutputDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Purge Files Location" 
      VIEW-AS TEXT 
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE fiToPurge AS CHARACTER FORMAT "X(256)":U INITIAL "  Purge:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE rsCompany AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "This Company", "This",
"All Companies", "All"
     SIZE 39 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 91 BY 12.38.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 54 BY 16.91.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 91 BY 4.05.

DEFINE VARIABLE slPurges AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 52 BY 16.19 NO-UNDO.

DEFINE VARIABLE tbOrphan AS LOGICAL INITIAL yes 
     LABEL "Include Orphan Purge" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btExit AT ROW 1.24 COL 157 WIDGET-ID 36 NO-TAB-STOP 
     cbGroup AT ROW 1.48 COL 9 COLON-ALIGNED WIDGET-ID 18
     BUTTON-1 AT ROW 2.43 COL 126 WIDGET-ID 54
     slPurges AT ROW 4.33 COL 5 NO-LABEL WIDGET-ID 16
     btAnalyze AT ROW 4.57 COL 157 WIDGET-ID 50 NO-TAB-STOP 
     btPurge AT ROW 7.91 COL 157 WIDGET-ID 52 NO-TAB-STOP 
     rsCompany AT ROW 17.19 COL 70 NO-LABEL WIDGET-ID 44
     tbOrphan AT ROW 18.14 COL 70 WIDGET-ID 48
     fiOutputDir AT ROW 1.71 COL 82 COLON-ALIGNED WIDGET-ID 20
     fiFileName AT ROW 2.67 COL 82 COLON-ALIGNED WIDGET-ID 22
     fiToPurge AT ROW 3.62 COL 6 NO-LABEL WIDGET-ID 14
     fiFilter AT ROW 3.62 COL 65 NO-LABEL WIDGET-ID 30
     fiBegin AT ROW 4.29 COL 97 NO-LABEL WIDGET-ID 24
     fiEnd AT ROW 4.29 COL 131 NO-LABEL WIDGET-ID 26
     fiOptions AT ROW 16.48 COL 65 NO-LABEL WIDGET-ID 40
     RECT-36 AT ROW 3.86 COL 62 WIDGET-ID 32
     RECT-37 AT ROW 3.86 COL 4 WIDGET-ID 38
     RECT-38 AT ROW 16.71 COL 62 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 181.2 BY 28.57
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Utilities"
         HEIGHT             = 20.14
         WIDTH              = 175.6
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 200.2
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 200.2
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
/* SETTINGS FOR FILL-IN fiBegin IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiBegin:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiEnd IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiEnd:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiFileName IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiFileName:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiFilter IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiFilter:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiOptions IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiOptions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiOutputDir IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiOutputDir:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiToPurge IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiToPurge:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Purge Utilities */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Purge Utilities */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAnalyze
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAnalyze wWin
ON CHOOSE OF btAnalyze IN FRAME fMain /* Analyze */
DO:    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wWin
ON CHOOSE OF btExit IN FRAME fMain /* Exit */
DO:    
    APPLY 'close':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPurge wWin
ON CHOOSE OF btPurge IN FRAME fMain /* Purge */
DO:    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbGroup wWin
ON VALUE-CHANGED OF cbGroup IN FRAME fMain /* Group */
DO:
    DEF VAR cPurgeSelList AS CHAR NO-UNDO.
    DEF VAR lCanPurge AS LOG NO-UNDO.
    
    RUN GetPurgeListByGroup IN hPurgeProcs (INPUT SELF:SCREEN-VALUE, OUTPUT cPurgeSelList). 
    
    ASSIGN 
        slPurges:LIST-ITEMS = TRIM(cPurgeSelList,",")   
        slPurges:SCREEN-VALUE = ENTRY(1,slPurges:LIST-ITEMS).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slPurges
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slPurges wWin
ON VALUE-CHANGED OF slPurges IN FRAME fMain
DO:
    DEF VAR iParmCt AS INT INITIAL 1 NO-UNDO.
    DO iParmCt = 1 TO 10:
        IF VALID-HANDLE(hBeginLabel[iParmCt]) THEN DELETE WIDGET hBeginLabel[iParmCt].
        IF VALID-HANDLE(hParmBegin[iParmCt]) THEN DELETE WIDGET hParmBegin[iParmCt].
        IF VALID-HANDLE(hParmEnd[iParmCt]) THEN DELETE WIDGET hParmEnd[iParmCt].
    END.
    
    ASSIGN 
        iParmCt = 1.

    RUN BuildDetailedPurgeParms IN hPurgeProcs (INPUT SELF:SCREEN-VALUE).

    FOR EACH ttParmsByPurge WHERE 
        ttParmsByPurge.ttcPurge = SELF:SCREEN-VALUE:
            
        CREATE TEXT hBeginLabel[iParmCt] ASSIGN 
            FRAME = FRAME fMain:HANDLE  
            FORMAT = IF NOT ttParmsByPurge.ttlRange THEN "x(46)" else "x(16)"
            ROW = 4.24 + iParmCt
            HEIGHT = 1
            COLUMN = 68
            WIDTH = IF NOT ttParmsByPurge.ttlRange THEN 49.5 ELSE 19.5 
            SCREEN-VALUE = ttParmsByPurge.ttcLabel + ":"
            VISIBLE = TRUE  
            .
             
        IF ttParmsByPurge.ttlRange THEN 
        CREATE FILL-IN hParmBegin[iParmCt] ASSIGN
            FRAME = FRAME fMain:HANDLE  
            NAME = ttParmsByPurge.ttcParm + "BEGINx"
            DATA-TYPE = ttParmsByPurge.ttcDataType
            FORMAT = ttParmsByPurge.ttcFormat
            ROW = 4.24 + iParmCt
            COLUMN = 88
            HEIGHT = 1
            WIDTH = 30
            SCREEN-VALUE =ttParmsByPurge.ttcStartValue
            VISIBLE = TRUE 
            SENSITIVE = TRUE 
            TRIGGERS:
                ON LEAVE PERSISTENT RUN ipLeaveFieldTrigger.
                ON HELP PERSISTENT RUN ipLookupFieldTrigger (INPUT ttParmsByPurge.ttcParm).
            END TRIGGERS 
            .

        CREATE FILL-IN hParmEnd[iParmCt] ASSIGN 
            FRAME = FRAME fMain:HANDLE  
            NAME = ttParmsByPurge.ttcParm + "ENDx"
            DATA-TYPE = ttParmsByPurge.ttcDataType
            FORMAT = ttParmsByPurge.ttcFormat
            ROW = 4.24 + iParmCt
            COLUMN = 120
            HEIGHT = 1
            WIDTH = 30
            SIDE-LABEL-HANDLE = hBeginLabel[iParmCt]
            LABEL = hBeginLabel[iParmCt]:SCREEN-VALUE
            SCREEN-VALUE = ttParmsByPurge.ttcEndValue
            VISIBLE = TRUE
            SENSITIVE = TRUE
            TRIGGERS:
                ON LEAVE PERSISTENT RUN ipLeaveFieldTrigger.
                ON HELP PERSISTENT RUN ipLookupFieldTrigger (INPUT ttParmsByPurge.ttcParm).
            END TRIGGERS
            .
        ASSIGN 
            iParmCt = iParmCt + 1.
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
  DISPLAY cbGroup slPurges rsCompany tbOrphan fiOutputDir fiFileName fiToPurge 
          fiFilter fiBegin fiEnd fiOptions 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-36 RECT-37 RECT-38 btExit cbGroup BUTTON-1 slPurges btAnalyze 
         btPurge rsCompany tbOrphan 
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

    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT VALID-HANDLE(hPurgeProcs) THEN 
        RUN util/PurgeProcs.p PERSISTENT SET hPurgeProcs.
    RUN initializeProc IN hPurgeProcs.

  RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
        APPLY 'value-changed' TO cbGroup IN FRAME {&frame-name}.
        APPLY 'entry' TO cbGroup.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLeaveFieldTrigger wWin
PROCEDURE ipLeaveFieldTrigger:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cParmName AS CHAR NO-UNDO.
    DEF VAR cStartValue AS CHAR NO-UNDO.
    DEF VAR cEndValue AS CHAR NO-UNDO.
    DEF VAR iParmCt AS INT NO-UNDO.
    
    DO iParmCt = 1 TO 10:
        ASSIGN 
            cParmName = ""
            cStartValue = ""
            cEndValue = "".    

        IF NOT VALID-HANDLE(hParmBegin[iParmCt])
        AND NOT VALID-HANDLE(hParmEnd[iParmCt]) THEN LEAVE.
         
        IF VALID-HANDLE(hParmBegin[iParmCt]) THEN ASSIGN 
            cParmName = REPLACE(hParmBegin[iParmCt]:NAME,"BEGINx","")
            cStartValue = hParmBegin[iParmCt]:SCREEN-VALUE.
        IF VALID-HANDLE(hParmEnd[iParmCt]) THEN ASSIGN 
            cParmName = REPLACE(hParmEnd[iParmCt]:NAME,"ENDx","")
            cEndValue = hParmEnd[iParmCt]:SCREEN-VALUE.
        
        FIND FIRST ttParmsByPurge WHERE 
            ttParmsByPurge.ttcPurge EQ slPurges:SCREEN-VALUE IN FRAME fMain AND 
            ttParmsByPurge.ttcParm EQ cParmName
            NO-ERROR.
        IF NOT AVAIL ttParmsByPurge THEN DO:
            MESSAGE 
                "Fatal Error in ipLeaveFieldTrigger." skip
                iParmCt SKIP 
                cParmName 
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
        ELSE ASSIGN 
            ttParmsByPurge.ttcStartValue = cStartValue
            ttParmsByPurge.ttcEndValue = cEndValue
            .
    END.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLookupFieldTrigger wWin
PROCEDURE ipLookupFieldTrigger:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFieldName AS CHAR.
    DEF VAR cReturn AS CHAR.
    
    CASE ipcFieldName:
        WHEN "job-no" THEN DO:
            RUN windows/l-jobhdr.w (INPUT "001", INPUT FOCUS:screen-value, OUTPUT cReturn).
            ASSIGN 
                FOCUS:SCREEN-VALUE = ENTRY(1,cReturn).
        END.
    END CASE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



