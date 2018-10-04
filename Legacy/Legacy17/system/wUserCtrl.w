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
DEFINE VARIABLE cVersion AS CHARACTER NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-5 btRefresh 
&Scoped-Define DISPLAYED-OBJECTS fiActive fiLicensed fiAvailable fiUnique 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_brsuserctrl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brsuserloghist AS HANDLE NO-UNDO.
DEFINE VARIABLE h_duserloghist AS HANDLE NO-UNDO.
DEFINE VARIABLE h_duserlogopen AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vuserctrl AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btRefresh 
     LABEL "Refresh" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiActive AS CHARACTER FORMAT "X(256)":U 
     LABEL "Active" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiAvailable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Available" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiLicensed AS CHARACTER FORMAT "X(256)":U 
     LABEL "Licensed" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiUnique AS CHARACTER FORMAT "X(256)":U 
     LABEL "Unique Users" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiActive AT ROW 1.48 COL 10 COLON-ALIGNED WIDGET-ID 14
     fiLicensed AT ROW 1.48 COL 43 COLON-ALIGNED WIDGET-ID 16
     fiAvailable AT ROW 1.48 COL 74 COLON-ALIGNED WIDGET-ID 18
     fiUnique AT ROW 1.48 COL 105 COLON-ALIGNED WIDGET-ID 20
     btRefresh AT ROW 1.48 COL 129 WIDGET-ID 22
     RECT-5 AT ROW 1.24 COL 4 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.4 BY 20.33 WIDGET-ID 100.


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
         TITLE              = "Session Administration"
         HEIGHT             = 20.33
         WIDTH              = 150.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 153.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 153.2
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
/* SETTINGS FOR FILL-IN fiActive IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAvailable IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLicensed IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnique IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Session Administration */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Session Administration */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh wWin
ON CHOOSE OF btRefresh IN FRAME fMain /* Refresh */
DO:
 RUN showCounts.
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
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Active Sessions|Session History|Configuration' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 3.19 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 17.86 , 145.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             btRefresh:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'system/duserlogopen.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameduserlogopenOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_duserlogopen ).
       RUN repositionObject IN h_duserlogopen ( 2.19 , 138.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'system/brsuserctrl.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brsuserctrl ).
       RUN repositionObject IN h_brsuserctrl ( 5.05 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_brsuserctrl ( 15.48 , 139.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brsuserctrl. */
       RUN addLink ( h_duserlogopen , 'Data':U , h_brsuserctrl ).
       RUN addLink ( h_brsuserctrl , 'Update':U , h_duserlogopen ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_brsuserctrl ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'system/duserloghist.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameduserloghistOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_duserloghist ).
       RUN repositionObject IN h_duserloghist ( 1.71 , 132.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'system/brsuserloghist.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brsuserloghist ).
       RUN repositionObject IN h_brsuserloghist ( 5.05 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_brsuserloghist ( 15.00 , 138.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brsuserloghist. */
       RUN addLink ( h_duserloghist , 'Data':U , h_brsuserloghist ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_brsuserloghist ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'system/vuserctrl.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vuserctrl ).
       RUN repositionObject IN h_vuserctrl ( 4.43 , 8.40 ) NO-ERROR.
       /* Size in AB:  ( 14.76 , 105.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vuserctrl ,
             h_folder , 'AFTER':U ).
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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
  DISPLAY fiActive fiLicensed fiAvailable fiUnique 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-5 btRefresh 
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
    DEFINE VARIABLE cInputLine   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEntireText  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lVersionRead AS LOGICAL   NO-UNDO.



    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.
    RUN showCounts.

/* Code placed here will execute AFTER standard behavior.    */
RUN disableFolderPage IN h_folder
    ( INPUT 3).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showCounts wWin 
PROCEDURE showCounts :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iConcurUsers AS INTEGER NO-UNDO.
DEFINE VARIABLE iUniqueUsers AS INTEGER NO-UNDO.
RUN system/userCount.p (INPUT "asi", OUTPUT iConcurUsers, OUTPUT iUniqueUsers).
    FIND FIRST asi._license NO-LOCK.
   
    DO WITH FRAME {&frame-name}:
        fiLicensed:SCREEN-VALUE = STRING(asi._license._Lic-ValidUsers).   
        fiActive:screen-value = STRING(iConcurUsers)  .
        /* _Lic-CurrConns */  
        fiAVailable:SCREEN-VALUE = STRING(asi._license._lic-ValidUsers - iConcurUsers).
        fiUnique:SCREEN-VALUE = STRING(iUniqueUsers).
        
    END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

