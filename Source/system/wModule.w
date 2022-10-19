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
DEFINE VARIABLE cCurlLoc AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-1 rsSubLevel tCRM tArtios tEsko ~
tAdvSch tDMI 
&Scoped-Define DISPLAYED-OBJECTS fiExpDate fiUserCount fiSubLevel ~
rsSubLevel fiOptions tCRM tArtios tEsko tAdvSch tDMI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_brwmodule AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sdomodule AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 10 BY 1.91.

DEFINE VARIABLE fiExpDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Current Expiration Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Optional Modules Licensed:" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE fiSubLevel AS CHARACTER FORMAT "X(256)":U INITIAL "Current Subscription Level:" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserCount AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Current Licensed Users" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE rsSubLevel AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Estimating", "EST",
"Base System", "BASE",
"Full System", "FULL",
"Full+API", "API",
"All Access", "ALL"
     SIZE 22 BY 5.95
     FONT 6 NO-UNDO.

DEFINE VARIABLE tAdvSch AS LOGICAL INITIAL no 
     LABEL "Advanced Scheduling" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tArtios AS LOGICAL INITIAL no 
     LABEL "Artios Integration" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tCRM AS LOGICAL INITIAL no 
     LABEL "CRM Integration" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tDMI AS LOGICAL INITIAL no 
     LABEL "DMI Integration" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tEsko AS LOGICAL INITIAL no 
     LABEL "Esko Integration" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiExpDate AT ROW 1.48 COL 25 COLON-ALIGNED WIDGET-ID 22
     fiUserCount AT ROW 2.67 COL 25 COLON-ALIGNED WIDGET-ID 24
     BUTTON-1 AT ROW 1.48 COL 185
     fiSubLevel AT ROW 4.33 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 8 NO-TAB-STOP 
     rsSubLevel AT ROW 5.52 COL 6 NO-LABEL WIDGET-ID 2
     fiOptions AT ROW 11.48 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 20 NO-TAB-STOP 
     tCRM AT ROW 12.67 COL 6 WIDGET-ID 10
     tArtios AT ROW 13.86 COL 6 WIDGET-ID 12
     tEsko AT ROW 15.05 COL 6 WIDGET-ID 14
     tAdvSch AT ROW 16.24 COL 6 WIDGET-ID 16
     tDMI AT ROW 17.43 COL 6 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 196.8 BY 23.57
         CANCEL-BUTTON BUTTON-1.


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
         TITLE              = "System Licensing Information"
         HEIGHT             = 23.57
         WIDTH              = 196.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 196.8
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 196.8
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
/* SETTINGS FOR FILL-IN fiExpDate IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOptions IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiOptions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiSubLevel IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiSubLevel:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiUserCount IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiUserCount:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* System Licensing Information */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* System Licensing Information */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/sdomodule.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamesdomoduleOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_sdomodule ).
       RUN repositionObject IN h_sdomodule ( 19.81 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.62 , 10.80 ) */

       RUN constructObject (
             INPUT  'system/brwmodule.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brwmodule ).
       RUN repositionObject IN h_brwmodule ( 1.48 , 46.00 ) NO-ERROR.
       RUN resizeObject IN h_brwmodule ( 22.86 , 136.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brwmodule. */
       RUN addLink ( h_sdomodule , 'Data':U , h_brwmodule ).
       RUN addLink ( h_brwmodule , 'Update':U , h_sdomodule ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_brwmodule ,
             fiExpDate:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY fiExpDate fiUserCount fiSubLevel rsSubLevel fiOptions tCRM tArtios 
          tEsko tAdvSch tDMI 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-1 rsSubLevel tCRM tArtios tEsko tAdvSch tDMI 
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
  FIND FIRST module NO-LOCK WHERE 
    module.isGlobal EQ TRUE 
    NO-ERROR.
  IF NOT AVAIL module THEN DO:
      CREATE module.
      ASSIGN 
        module.isGlobal = TRUE 
        module.moduleGroup = "Full"
        module.expDate = DATE(12,31,year(TODAY)).
        .
  END.
  ASSIGN 
    fiExpDate:SCREEN-VALUE IN FRAME {&frame-name} = STRING(module.expDate,"99/99/9999")
    rsSubLevel:SCREEN-VALUE = module.moduleGroup.
  FIND FIRST userControl NO-LOCK
    NO-ERROR.
  ASSIGN 
    fiUserCount:SCREEN-VALUE = STRING(userControl.numLicensedUsers).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

