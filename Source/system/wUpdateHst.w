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
&Scoped-Define ENABLED-OBJECTS BUTTON-1 bHotfix 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_brwupdatehst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sdoupdatehist AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vwrupdatehst AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bHotfix 
     LABEL "Install latest patches" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-1 AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 10 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 19.33 COL 160
     bHotfix AT ROW 19.81 COL 113
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170.6 BY 20.43
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
         TITLE              = "Update History"
         HEIGHT             = 20.43
         WIDTH              = 170.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 178.8
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 178.8
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
ON END-ERROR OF wWin /* Update History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Update History */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bHotfix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bHotfix wWin
ON CHOOSE OF bHotfix IN FRAME fMain /* Install latest patches */
DO:
    
    RUN ipInstallPatch.
       
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
             INPUT  'sdo/sdoupdatehist.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamesdoupdatehistOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_sdoupdatehist ).
       RUN repositionObject IN h_sdoupdatehist ( 19.33 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 1.67 , 11.00 ) */

       RUN constructObject (
             INPUT  'system/vwrupdatehst.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vwrupdatehst ).
       RUN repositionObject IN h_vwrupdatehst ( 1.24 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 18.10 , 71.00 ) */

       RUN constructObject (
             INPUT  'system/brwupdatehst.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brwupdatehst ).
       RUN repositionObject IN h_brwupdatehst ( 1.48 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_brwupdatehst ( 18.10 , 95.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vwrupdatehst. */
       RUN addLink ( h_sdoupdatehist , 'Data':U , h_vwrupdatehst ).

       /* Links to SmartDataBrowser h_brwupdatehst. */
       RUN addLink ( h_sdoupdatehist , 'Data':U , h_brwupdatehst ).
       RUN addLink ( h_brwupdatehst , 'Update':U , h_sdoupdatehist ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_brwupdatehst ,
             h_vwrupdatehst , 'AFTER':U ).
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
  ENABLE BUTTON-1 bHotfix 
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
PROCEDURE initializeObject:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    FIND users NO-LOCK WHERE 
        users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.
    IF users.securityLevel LT 900 THEN ASSIGN 
        bHotFix:VISIBLE IN FRAME {&frame-name} = FALSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInstallPatch wWin
PROCEDURE ipInstallPatch:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cCmd AS CHAR NO-UNDO.
    DEF VAR cUserTempDir AS CHAR NO-UNDO.
    DEF VAR cRawLine AS CHAR NO-UNDO.
    DEF VAR cMyVersion AS CHAR NO-UNDO.
    DEF VAR cMyLongVersion AS CHAR NO-UNDO.
    DEF VAR cMyPatch AS CHAR NO-UNDO.
    DEF VAR cNewVersion AS CHAR NO-UNDO.
    DEF VAR cNewPatch AS CHAR NO-UNDO.
    DEF VAR lUpgrade AS LOG NO-UNDO.
    DEF VAR lUpdate AS LOG NO-UNDO.
    DEF VAR cPatchToProcess AS CHAR NO-UNDO.
    DEF VAR cLongName AS CHAR NO-UNDO.
    DEF VAR cOverrideDir AS CHAR NO-UNDO.
    DEF VAR cUpdatesDir AS CHAR NO-UNDO.
    DEF VAR iIndex AS INT NO-UNDO.
    DEF VAR deMyVersion AS DECIMAL NO-UNDO.
    DEF VAR deNewVersion AS DECIMAL NO-UNDO.
    DEF VAR iMyPatch AS INT NO-UNDO.
    DEF VAR iNewPatch AS INT NO-UNDO.
    DEF VAR lCanUpdate AS LOG NO-UNDO.
    DEF VAR lCanUpgrade AS LOG NO-UNDO.
    DEF VAR cHotfixLog AS CHAR NO-UNDO.
    
    
    /* Get the current version already installed */
    FIND LAST updateHist NO-LOCK NO-ERROR.
    ASSIGN 
        cMyLongVersion = updateHist.toVersion
        cMyVersion = SUBSTRING(updateHist.toVersion,1,5)
        cMyPatch = SUBSTRING(updateHist.toVersion,7,2)
        deMyVersion = DECIMAL(cMyVersion)
        iMyPatch = INTEGER(cMyPatch).
        
    /* Depending on choices, we need to know Override folder or Updates folder location */
    ASSIGN 
        FILE-INFO:FILE-NAME = PROGRAM-NAME(3)
        cLongName = FILE-INFO:FULL-PATHNAME 
        iIndex = INDEX(cLongName,"Programs")
        iIndex = IF iIndex EQ 0 THEN INDEX(cLongName,"Override") ELSE iIndex
        cOverrideDir = SUBSTRING(cLongName,1,iIndex - 1) + "Override"
        iIndex = INDEX(cLongName,"Environments")
        cUpdatesDir = SUBSTRING(cLongName,1,iIndex - 1) + "Updates".

    /* first, pull the current patch list from the website */
    ASSIGN 
        cCmd = 'CALL CURL "helpsvr.advantzware.com/Patches/hotfixlist.txt"  -o c:\tmp\hotfixlist.txt -s'.
    OS-COMMAND SILENT VALUE(cCmd).
    PAUSE 5 BEFORE-HIDE NO-MESSAGE.
        
    /* Now parse the hotfixlist to see if there is a new hotfix (or upgrade) available */
    IF SEARCH("c:\tmp\hotfixlist.txt") NE ? THEN DO:
        INPUT FROM VALUE("c:\tmp\hotfixlist.txt").
        SEARCH-BLOCK:
        REPEAT:
            IMPORT UNFORMATTED cRawLine.
            ASSIGN 
                deNewVersion = DECIMAL(SUBSTRING(cRawLine,1,5)) 
                iNewPatch = INTEGER(SUBSTRING(cRawLine,7,2)).             
            IF deNewVersion LT deMyVersion THEN NEXT.
            ELSE IF deNewVersion GT deMyVersion THEN DO:
                ASSIGN 
                    lCanUpgrade = TRUE.
                /* Newer VERSION available */
            END.   
            ELSE IF deNewVersion EQ deMyVersion 
            AND iNewPatch GT iMyPatch THEN DO:
                ASSIGN 
                    lCanUpdate = TRUE.
/*                MESSAGE                                                      */
/*                    "There is a new PATCH available (" + cRawLine + ")" SKIP */
/*                    "for this version.  Would you like to install it?"       */
/*                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lUpdate.*/
                ASSIGN 
                    lUpdate = TRUE.
                IF lUpdate THEN DO:
                    ASSIGN 
                        cPatchToProcess = cRawLine.
                    LEAVE SEARCH-BLOCK.
                END.
            END.
        END.
        INPUT CLOSE.
        OS-DELETE "c:\tmp\hotfixlist.txt".
    END. 
    ELSE DO:
        MESSAGE 
            "Unable to access the Advantzware servers to" SKIP 
            "check for patches.  Please try again later."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    /* If user wants to UPDATE current version, download Hotfix file and open Explorer to allow unzip */
    IF lUpdate THEN DO:
        ASSIGN 
            cCmd =  'CALL CURL "helpsvr.advantzware.com/Patches/Hotfix' + cPatchToProcess + '.zip"  -o ' + cOverrideDir + 
                    '\Hotfix' + cPatchToProcess + '.zip -s'.
        OS-COMMAND SILENT VALUE(cCmd).
        ASSIGN 
            cCmd =  'CALL "c:\program files\7-zip\7z.exe"  x ' + cOverrideDir + '\Hotfix' + cPatchToProcess + '.zip -o' + cOverrideDir + ' -aoa'. 
        OS-COMMAND SILENT VALUE(cCmd).

        INPUT FROM VALUE (cOverrideDir + "\HotfixList.txt").
        REPEAT:
            IMPORT UNFORMATTED cRawLine.
            ASSIGN 
                cHotFixLog = cHotFixLog + cRawLine + CHR(10).
        END. 
        INPUT CLOSE.
        CREATE updateHist.
        ASSIGN 
            updateHist.fromVersion = cMyLongVersion
            updateHist.toVersion = cPatchToProcess
            updateHist.applyDate = today
            updateHist.startTimeInt = time
            updateHist.startTime = STRING(time,"HH:MM:SS")
            updateHist.endTimeInt = time
            updateHist.endTime = STRING(time,"HH:MM:SS")
            updateHist.user_id = USERID(LDBNAME(1))
            updateHist.success = TRUE 
            updateHist.updLog = cHotFixLog.
        MESSAGE 
            "Version updated to " + cPatchToProcess + ".  Please log off and back on to use changes."
            VIEW-AS ALERT-BOX.
        RUN refreshQuery IN h_brwupdatehst.
        RETURN.
    END.
    /* If user wants to UPGRADE current version, download Patch file and open Explorer to allow unzip/processing */
    ELSE IF lUpgrade THEN DO:
        /* Process upgrade here */
        RETURN.
    END.
    
    MESSAGE 
        "Your Advantzware system is up to date!"
        VIEW-AS ALERT-BOX INFO.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


