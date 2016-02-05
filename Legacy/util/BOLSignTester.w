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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 fi_company rd_Printed ~
rd_OutputType tb_BuildFTPFile tb_ExecuteFTP btnProcess tb_BuildFTPFile-2 ~
btnFetch 
&Scoped-Define DISPLAYED-OBJECTS fi_company rd_Printed rd_OutputType ~
tb_BuildFTPFile tb_ExecuteFTP tb_BuildFTPFile-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFetch 
     LABEL "Fetch BOL Signatures" 
     SIZE 29 BY 1.19.

DEFINE BUTTON btnProcess 
     LABEL "Process" 
     SIZE 29 BY 1.19.

DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(256)":U INITIAL "001" 
     LABEL "Company Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE rd_OutputType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "JSON", "JSON",
"XML", "XML"
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE rd_Printed AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", 1,
"No", 2,
"Either", 3
     SIZE 37 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 4.52.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 7.14.

DEFINE VARIABLE tb_BuildFTPFile AS LOGICAL INITIAL no 
     LABEL "Build FTP Instruction File" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1.19 NO-UNDO.

DEFINE VARIABLE tb_BuildFTPFile-2 AS LOGICAL INITIAL no 
     LABEL "Build FTP Instruction File" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1.19 NO-UNDO.

DEFINE VARIABLE tb_ExecuteFTP AS LOGICAL INITIAL no 
     LABEL "Execute FTP" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi_company AT ROW 1.71 COL 25 COLON-ALIGNED WIDGET-ID 26
     rd_Printed AT ROW 4.1 COL 17 NO-LABEL WIDGET-ID 16
     rd_OutputType AT ROW 5.29 COL 17 NO-LABEL WIDGET-ID 12
     tb_BuildFTPFile AT ROW 6.48 COL 17 WIDGET-ID 4
     tb_ExecuteFTP AT ROW 7.67 COL 17 WIDGET-ID 2
     btnProcess AT ROW 8.86 COL 13 WIDGET-ID 10
     tb_BuildFTPFile-2 AT ROW 11.95 COL 16 WIDGET-ID 32
     btnFetch AT ROW 13.14 COL 13 WIDGET-ID 30
     "For testing the building and sending of BOL data" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 3.38 COL 4 WIDGET-ID 42
     "Output?:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 5.29 COL 6 WIDGET-ID 24
     "For testing the retrieval of signature images" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 11 COL 4 WIDGET-ID 38
     "Printed?:" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 4.1 COL 6 WIDGET-ID 22
     RECT-2 AT ROW 10.29 COL 2 WIDGET-ID 36
     RECT-3 AT ROW 3.14 COL 2 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.6 BY 14.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "BOL Sign Data Tester"
         HEIGHT             = 14.14
         WIDTH              = 58.6
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
ON END-ERROR OF wWin /* BOL Sign Data Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* BOL Sign Data Tester */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFetch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFetch wWin
ON CHOOSE OF btnFetch IN FRAME fMain /* Fetch BOL Signatures */
DO:
  RUN ExecuteFetch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcess wWin
ON CHOOSE OF btnProcess IN FRAME fMain /* Process */
DO:
  RUN ExecuteProcess.
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
  DISPLAY fi_company rd_Printed rd_OutputType tb_BuildFTPFile tb_ExecuteFTP 
          tb_BuildFTPFile-2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 fi_company rd_Printed rd_OutputType tb_BuildFTPFile 
         tb_ExecuteFTP btnProcess tb_BuildFTPFile-2 btnFetch 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteFetch wWin 
PROCEDURE ExecuteFetch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iTimer AS INTEGER NO-UNDO.
DEFINE VARIABLE lPrinted AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFile AS CHAR NO-UNDO.

iTimer = TIME.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        tb_BuildFTPFile-2.

    RUN oe/GetBOLSign.p(INPUT fi_Company,
                        INPUT ?,
                        INPUT YES,
                        INPUT tb_BuildFTPFile-2,
                        OUTPUT cFile).
END.
MESSAGE "Process Completed.  Time: " TIME - iTimer " seconds." SKIP
    "Last File: " cFile
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteProcess wWin 
PROCEDURE ExecuteProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iTimer AS INTEGER NO-UNDO.
DEFINE VARIABLE lPrinted AS LOGICAL NO-UNDO.

iTimer = TIME.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        rd_OutputType
        rd_Printed
        fi_Company
        tb_BuildFTPFile
        tb_ExecuteFTP.

    CASE rd_printed :
        WHEN 1 THEN lPrinted = YES.
        WHEN 2 THEN lPrinted = NO.
        WHEN 3 THEN lPrinted = ?.
    END CASE.
    RUN oe/BuildBOLSignData.p(INPUT fi_Company,
                              INPUT rd_OutputType,
                              INPUT rd_Printed,
                              INPUT tb_ExecuteFTP,
                              INPUT tb_BuildFTPFile).
END.
MESSAGE "Process Completed.  Time: " TIME - iTimer " seconds."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

