&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME MAINMENU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS MAINMENU 
/*------------------------------------------------------------------------
 
  File:              mainMenu2.w
 
  Description:       Main Menu v2 (image icons and language translation)
 
  Input Parameters:  <none>
 
  Output Parameters: <none>
 
  Author:            Ron Stark
 
  Created:           3.21.2018
 
--------------------------------------------------------------------*/
 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
 
CREATE WIDGET-POOL.

/* *************************** Set Function ************************** */

ON F1 HELP.
ON CTRL-F HELP.
ON CTRL-P HELP.

ON 'CTRL-ALT-D':U ANYWHERE
    DO:
        RUN aoa/aoaLauncher.w PERSISTENT ("Dashboard").
        RETURN.
    END.

ON 'CTRL-ALT-R':U ANYWHERE
    DO:
        RUN aoa/aoaLauncher.w PERSISTENT ("Report").
        RETURN.
    END.
   
/* ***************************  Definitions  ************************** */
 
/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

&Scoped-define FGColor ?
&Scoped-define BGColor 8
&Scoped-define highlightColor 1
&Scoped-define startObjectCol 2
&Scoped-define startObjectRow 3.5
&Scoped-define objectWidth 49
&Scoped-define objectGap .2
&Scoped-define minWindowHeight 28.57
&Scoped-define winWindowWidth 160
&Scoped-define maxWindow {&startObjectRow} - .7 + ~
(dObjectHeight + {&objectGap}) * iHiCount

OS-DELETE VALUE("users\" + USERID(LDBNAME(1)) + "\sysCtrlFind.dat") NO-ERROR.

{methods/defines/mainmenu.i}
{system/translations.i}

/* System Constant Values */
{system/sysconst.i}

DEFINE TEMP-TABLE ttPersistent NO-UNDO
    FIELD prgmTitle AS CHARACTER
        INDEX ttPersistent IS PRIMARY prgmTitle
        .
DEFINE TEMP-TABLE ttblMenu NO-UNDO 
    FIELD menuName  AS CHARACTER 
    FIELD menuCount AS INTEGER 
    FIELD menuGuid  AS CHARACTER 
    FIELD mneumonic AS CHARACTER 
        INDEX ttblMenu IS PRIMARY UNIQUE menuName
        INDEX menuCount menuCount
        .
DEFINE TEMP-TABLE ttblItem NO-UNDO 
    FIELD menuOrder  AS INTEGER 
    FIELD menu1      AS CHARACTER FORMAT "x(12)"
    FIELD menu2      AS CHARACTER 
    FIELD seq        AS INTEGER 
    FIELD mneumonic  AS CHARACTER
    FIELD level      AS INTEGER
    FIELD mainMenu   AS LOGICAL
    FIELD imageFile  AS CHARACTER
    FIELD hRectangle AS HANDLE
    FIELD hImage     AS HANDLE
    FIELD hEditor    AS HANDLE
        INDEX ttblItems IS PRIMARY UNIQUE menuOrder menu2
        INDEX menu2 menu2 menuOrder
        INDEX mneumonic menu2 mneumonic
        INDEX seq menu2 seq
        .
DEFINE TEMP-TABLE ttblImage NO-UNDO
    FIELD prgmName  AS CHARACTER
    FIELD imageFile AS CHARACTER
        INDEX ttblImage IS PRIMARY prgmName
        .
DEFINE VARIABLE closeMenu     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hButtonWidget AS HANDLE    NO-UNDO.
DEFINE VARIABLE dObjectRow    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dObjectCol    AS DECIMAL   NO-UNDO INITIAL {&startObjectCol}.
DEFINE VARIABLE iObjectCount  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEulaFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEulaAccepted AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEulaVersion  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUserExit     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSourceMenu   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMneumonic    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImageFolder  AS CHARACTER NO-UNDO.
DEFINE VARIABLE dObjectHeight AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iHiCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMenuSize     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLanguage     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOK           AS LOGICAL   NO-UNDO.

ASSIGN
    g_company = ""
    g_loc     = ""
    .
RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,YES).
IF g_company = "" OR g_loc = "" THEN DO:
    MESSAGE "No Company and/or Location found for your login ID." SKIP
        "Please Contact System's Administrator." VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
END.
cEulaFile = SEARCH("{&EulaFile}").

DELETE WIDGET-POOL "MainMenuPool" NO-ERROR.
CREATE WIDGET-POOL "MainMenuPool".

{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-USER

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS imageSettings imageCompany svLink 
&Scoped-Define DISPLAYED-OBJECTS company_name loc_loc users_user_id ~
Mneumonic svLink 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fItemImage MAINMENU 
FUNCTION fItemImage RETURNS CHARACTER
  (ipcPrgmName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetColor MAINMENU 
FUNCTION fSetColor RETURNS LOGICAL
  (iphObject AS HANDLE, iplSet AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR MAINMENU AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE company_name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE loc_loc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Mneumonic AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 5 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE svLink AS CHARACTER FORMAT "X(256)":U INITIAL "www.Advantzware.com" 
      VIEW-AS TEXT 
     SIZE 23 BY .62
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE users_user_id AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     FONT 6 NO-UNDO.

DEFINE IMAGE boxes
     FILENAME "Graphics/advantzware_logo.jpg":U
     SIZE 97 BY 20.48.

DEFINE IMAGE imageCompany
     FILENAME "Graphics/32x32/office_building.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Change Company/Location".

DEFINE IMAGE imageSettings
     FILENAME "Graphics/32x32/gearwheels.ico":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Settings".

DEFINE IMAGE menu-image
     FILENAME "Graphics/logo1.bmp":U CONVERT-3D-COLORS
     SIZE 95 BY 4.52.

DEFINE RECTANGLE linkRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 27 BY 1.19
     BGCOLOR 1 FGCOLOR 15 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 97 BY 5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 159.6 BY 2.14
     BGCOLOR 1 FGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 48 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 38 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 40 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 159.6 BY .05
     BGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-USER
     company_name AT ROW 1.71 COL 13 COLON-ALIGNED NO-LABEL
     loc_loc AT ROW 1.71 COL 76 COLON-ALIGNED NO-LABEL
     users_user_id AT ROW 1.71 COL 117 COLON-ALIGNED NO-LABEL
     Mneumonic AT ROW 1.71 COL 140 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     svLink AT ROW 4.1 COL 15 NO-LABEL WIDGET-ID 34
     "Location:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.71 COL 68
     "User ID:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.71 COL 110
     "Company:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.71 COL 4
     boxes AT ROW 8.86 COL 52
     menu-image AT ROW 3.86 COL 53
     RECT-2 AT ROW 1 COL 1
     linkRect AT ROW 3.86 COL 13 WIDGET-ID 36
     RECT-5 AT ROW 1.48 COL 3 WIDGET-ID 38
     RECT-6 AT ROW 1.48 COL 101 WIDGET-ID 40
     RECT-7 AT ROW 1.48 COL 60 WIDGET-ID 42
     RECT-8 AT ROW 1.48 COL 140 WIDGET-ID 44
     RECT-9 AT ROW 3.29 COL 1 WIDGET-ID 46
     RECT-10 AT ROW 3.62 COL 52 WIDGET-ID 48
     imageSettings AT ROW 1.24 COL 152 WIDGET-ID 52
     imageCompany AT ROW 1.24 COL 52 WIDGET-ID 54
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW MAINMENU ASSIGN
         HIDDEN             = YES
         TITLE              = "Main Menu - Advantzware version {&awversion}"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 160
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         PRIVATE-DATA       = "Main Menu - Advantzware version"
         KEEP-FRAME-Z-ORDER = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT MAINMENU:LOAD-ICON("Graphics/asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW MAINMENU
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-USER
   FRAME-NAME                                                           */
/* SETTINGS FOR IMAGE boxes IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN company_name IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       imageCompany:PRIVATE-DATA IN FRAME FRAME-USER     = 
                "Change Company/Location".

ASSIGN 
       imageSettings:PRIVATE-DATA IN FRAME FRAME-USER     = 
                "Settings".

/* SETTINGS FOR RECTANGLE linkRect IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN loc_loc IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE menu-image IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mneumonic IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svLink IN FRAME FRAME-USER
   ALIGN-L                                                              */
ASSIGN 
       svLink:READ-ONLY IN FRAME FRAME-USER        = TRUE.

/* SETTINGS FOR FILL-IN users_user_id IN FRAME FRAME-USER
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(MAINMENU)
THEN MAINMENU:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-USER
/* Query rebuild information for FRAME FRAME-USER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-USER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME FRAME-USER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-USER MAINMENU
ON END-ERROR OF FRAME FRAME-USER
ANYWHERE
    DO:
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-USER MAINMENU
ON HELP OF FRAME FRAME-USER
DO:
        RUN Get_Procedure IN Persistent-Handle ("popups.",OUTPUT run-proc,YES).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageCompany MAINMENU
ON MOUSE-SELECT-CLICK OF imageCompany IN FRAME FRAME-USER
DO:
    RUN custom/comp_loc.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageSettings MAINMENU
ON MOUSE-SELECT-CLICK OF imageSettings IN FRAME FRAME-USER
DO:
    /*
    RUN system/sysCtrlUsage.w.
    */
    RUN system/userSettings.w (
        INPUT-OUTPUT iMenuSize,
        INPUT-OUTPUT iLanguage,
        OUTPUT lOK
        ).
    IF lOK THEN DO:
        cLabelLanguage = ENTRY(iLanguage,cLanguageList).
        RUN pSetUserSettings.
        RUN pReset.
    END. /* if ok */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLink
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLink MAINMENU
ON LEFT-MOUSE-CLICK OF svLink IN FRAME FRAME-USER
DO:
    OS-COMMAND NO-WAIT START VALUE(svLink).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK MAINMENU 


/* ***************************  Main Block  *************************** */
 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
 
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} 
DO:  
    closeMenu = YES.
    IF USERID("ASI") NE "Nosweat" THEN
        MESSAGE 'Exit AdvantzWare?' VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE closeMenu.
    IF NOT closeMenu THEN RETURN NO-APPLY.        
    RUN system/userLogOut.p.        
    QUIT. /* kills all processes */
END.

ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE 
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

ON ANY-PRINTABLE OF FRAME {&FRAME-NAME} ANYWHERE
DO:
    RUN pKeyPress (LASTKEY).
END.

{sys/inc/f3helpm.i} /* ASI F3 key include */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
 
/* Now enable the interface and wait for the exit condition.*/
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN pGetUserSettings.
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ g_company
           AND sys-ctrl.name    EQ "bitmap"
         NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = g_company
            sys-ctrl.name    = "bitmap"
            sys-ctrl.descrip = "Graphics\bigboxes"
            .
    END.
    IF AVAILABLE sys-ctrl AND sys-ctrl.descrip NE "" THEN
        boxes:LOAD-IMAGE(sys-ctrl.descrip).
    {methods/mainmenu.i}
    RUN pInit.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI MAINMENU  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(MAINMENU)
  THEN DELETE WIDGET MAINMENU.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI MAINMENU  _DEFAULT-ENABLE
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
  DISPLAY company_name loc_loc users_user_id Mneumonic svLink 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  ENABLE imageSettings imageCompany svLink 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-USER}
  VIEW MAINMENU.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick MAINMENU 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphObjectHandle AS WIDGET-HANDLE NO-UNDO.
    
    DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bttblItem FOR ttblItem.
    
    IF VALID-HANDLE(iphObjectHandle) THEN DO:
        IF iphObjectHandle:NAME EQ 'Exit' THEN
        APPLY 'WINDOW-CLOSE':U TO {&WINDOW-NAME}.

        FIND FIRST ttblItem
             WHERE ttblItem.menu1     EQ iphObjectHandle:NAME
               AND ttblItem.mneumonic EQ iphObjectHandle:TOOLTIP.
        ASSIGN
            boxes:HIDDEN IN FRAME {&FRAME-NAME} = YES
            menu-image:HIDDEN = YES
            RECT-10:HIDDEN = YES
            .
        FOR EACH bttblItem WHERE bttblItem.level GT ttblItem.level:
            DELETE OBJECT bttblItem.hImage NO-ERROR.
            DELETE OBJECT bttblItem.hEditor NO-ERROR.
            DELETE OBJECT bttblItem.hRectangle NO-ERROR.
        END.
        FOR EACH bttblItem WHERE bttblItem.level EQ ttblItem.level:
            fSetColor(bttblItem.hEditor,NO).
            fSetColor(bttblItem.hRectangle,NO).
        END.
        fSetColor(ttblItem.hEditor,YES).
        fSetColor(ttblItem.hRectangle,YES).
    
        ASSIGN
            dObjectCol = ttblItem.hRectangle:COLUMN + {&objectWidth} + {&objectGap}
            cMneumonic = ttblItem.mneumonic
            Mneumonic:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMneumonic
            .    
        IF INDEX(iphObjectHandle:NAME,'.') EQ 0 THEN
        RUN pCreateMenuObjects (iphObjectHandle:NAME).
        ELSE DO:
            /* check module license first before run it YSK 08/24/04 TASK# 08060406 */
            RUN util/CheckModule.p ("ASI", iphObjectHandle:NAME, YES, OUTPUT lAccess) NO-ERROR.
            IF lAccess THEN 
            RUN Get_Procedure IN Persistent-Handle(iphObjectHandle:NAME,OUTPUT run-proc,YES).        
        END. /* else */
    END. /* if valid handle */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateEditor MAINMENU 
PROCEDURE pCreateEditor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcText     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

    CREATE EDITOR hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            COL = ipdCol + 7.4
            ROW = ipdRow + .24
            SENSITIVE = YES
            WIDTH = ipdWidth - 7.6
            HEIGHT = ipdHeight - .34
            FGCOLOR = ipiFGColor
            BGCOLOR = ipiBGColor
            SCROLLBAR-HORIZONTAL = NO
            SCROLLBAR-VERTICAL = NO
            WORD-WRAP = YES
            READ-ONLY = YES
            BOX = NO
            PRIVATE-DATA = ipcText
            SCREEN-VALUE = ipcText
            TOOLTIP = ipcTooltip
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttblItem.hEditor = hWidget.
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateImage MAINMENU 
PROCEDURE pCreateImage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    CREATE IMAGE hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            COL = ipdCol + 1
            ROW = ipdRow + .24
            SENSITIVE = YES
            WIDTH = ipdWidth - 35.2
            HEIGHT = ipdHeight - .53
            TRANSPARENT = YES
            TOOLTIP = ipcTooltip
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttblItem.hImage = hWidget.
        hWidget:LOAD-IMAGE(SEARCH(ipcImage)).
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateMenuObjects MAINMENU 
PROCEDURE pCreateMenuObjects :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMenuItem AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cObjectLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFirstChar   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    dObjectRow = {&startObjectRow}.
    FOR EACH ttblItem WHERE ttblItem.menu2 EQ ipcMenuItem:
        FIND prgrms NO-LOCK
             WHERE prgrms.prgmname EQ ttblItem.menu1
             NO-ERROR.
        IF NOT AVAILABLE prgrms AND ttblItem.menu1 NE 'Exit' THEN NEXT.
        ASSIGN
            cFirstChar   = "[" + (IF AVAILABLE prgrms THEN SUBSTR(prgrms.prgtitle,1,1) ELSE ttblItem.menu1) + "] "
            cObjectLabel = (IF AVAILABLE prgrms THEN fTranslate(prgrms.prgtitle,NO) ELSE fTranslate(ttblItem.menu1,NO))
                         + (IF INDEX(ttblItem.menu1,'.') NE 0 OR ttblItem.menu1 EQ 'Exit' THEN ''
                            ELSE '...')
                         .
        FIND FIRST users NO-LOCK
             WHERE users.user_id EQ USERID(LDBNAME(1)) 
             NO-ERROR.
        IF AVAILABLE users AND users.securityLevel LE 999 AND
           AVAILABLE prgrms AND prgrms.prgmname EQ "file." THEN NEXT.

        IF AVAILABLE prgrms AND prgrms.prgmname EQ "custproc." AND
           USER(LDBNAME(1)) NE "ASI" THEN DO: /*NEXT .*/ /* ticket - 23865  */
            IF AVAILABLE users AND users.securityLevel LE 899 THEN NEXT.
        END.

        IF ttblItem.level GT 2 THEN
        cObjectLabel = STRING(ttblItem.seq) + ". " + cObjectLabel.
        
        IF cLabelLanguage NE "English" AND ttblItem.level LE 2 THEN
        cObjectLabel = cFirstChar + cObjectLabel.

        RUN pCreateObject (
            "MainMenuPool",
            FRAME {&FRAME-NAME}:HANDLE,
            ttblItem.menu1,
            dObjectCol,
            dObjectRow,
            {&objectWidth},
            dObjectHeight,
            {&FGColor},
            {&BGColor},
            cObjectLabel,
            ttblItem.mneumonic,
            "pClick",
            ttblItem.imageFile
            ).
            
        dObjectRow = dObjectRow + dObjectHeight + {&objectGap}.       
    END. /* each ttblItem */
    FRAME {&FRAME-NAME}:HIDDEN = NO.

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateObject MAINMENU 
PROCEDURE pCreateObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcText     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage    AS CHARACTER NO-UNDO.

    RUN pCreateRectangle (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipiFGColor,
        ipiBGColor,
        ipcTooltip,
        ipcClick
        ).            
    IF ipcText NE ? THEN
    RUN pCreateEditor (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipiFGColor,
        ipiBGColor,
        ipcText,
        ipcTooltip,
        ipcClick
        ).
    RUN pCreateImage (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipcTooltip,
        ipcClick,
        ipcImage
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateRectangle MAINMENU 
PROCEDURE pCreateRectangle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

    CREATE RECTANGLE hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            COL = ipdCol
            ROW = ipdRow
            SENSITIVE = YES
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            FGCOLOR = ipiFGColor
            BGCOLOR = ipiBGColor
            TOOLTIP = ipcTooltip
            ROUNDED = YES
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (hWidget:HANDLE).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttblItem.hRectangle = hWidget.
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMenu MAINMENU 
PROCEDURE pGetMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrgrm     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMenu      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMneumonic AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMaxWindow AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lMainMenu  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cNumChars  AS CHARACTER NO-UNDO INITIAL "1,2,3,4,5,6,7,8,9,),!,@,#,$,%,^,~&,*,(".

    EMPTY TEMP-TABLE ttblMenu.
    EMPTY TEMP-TABLE ttblItem.
    
    REPEAT:
        IMPORT cPrgrm cMenu.
        
        IF CAN-DO ("mainmenu,file,help",cPrgrm) THEN NEXT.
        IF CAN-DO ("rule,skip",cPrgrm) THEN NEXT.

        lMainMenu = cMenu EQ "mainmenu".
        IF lMainMenu THEN cMenu = "file".
        
        FIND FIRST prgrms NO-LOCK WHERE prgrms.prgmname EQ cPrgrm NO-ERROR.
        ASSIGN cMneumonic = SUBSTRING (prgrms.prgtitle,1,1) WHEN AVAILABLE prgrms.

        FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cMenu NO-ERROR.
        ASSIGN cMneumonic = ttblMenu.mneumonic + cMneumonic WHEN AVAILABLE ttblMenu.

        IF INDEX (cPrgrm,".") EQ 0 THEN DO:
            FIND FIRST ttblMenu WHERE ttblMenu.menuName EQ cPrgrm NO-ERROR.
            IF NOT AVAILABLE ttblMenu THEN DO:         
                CREATE ttblMenu.
                ASSIGN
                    ttblMenu.menuName  = cPrgrm
                    ttblMenu.mneumonic = cMneumonic 
                   .
            END.
        END.
    
        IF NOT AVAILABLE ttblMenu THEN NEXT.
        
        IF NOT CAN-FIND(FIRST prgrms WHERE prgrms.prgmname EQ cPrgrm) THEN NEXT.

        IF LENGTH (cMneumonic) EQ 3 THEN
        ASSIGN cMneumonic = SUBSTRING (cMneumonic,1,2) + ENTRY(ttblMenu.menuCount,cNumChars).

        CREATE ttblItem.
        ASSIGN 
            idx                = idx + 1
            ttblMenu.menuCount = ttblMenu.menuCount + 1
            ttblItem.menuOrder = idx
            ttblItem.menu1     = cPrgrm
            ttblItem.menu2     = cMenu
            ttblItem.mneumonic = cMneumonic
            ttblItem.mainMenu  = lMainMenu
            ttblItem.imageFile = fItemImage(cPrgrm)
            .            
    END. /* repeat */
    IF AVAILABLE ttblMenu THEN
    ttblMenu.menuCount = ttblMenu.menuCount + 1.
    CREATE ttblItem.
    ASSIGN 
        idx                = idx + 1
        ttblItem.menuOrder = idx
        ttblItem.menu1     = "Exit"
        ttblItem.menu2     = "file"
        ttblItem.mneumonic = "X"
        ttblItem.mainMenu  = NO
        ttblItem.imageFile = fItemImage("Exit")
        .            
    FOR EACH ttblItem USE-INDEX menu2 BREAK BY ttblItem.menu2:
        IF FIRST-OF(ttblItem.menu2) THEN idx = 0.
        ASSIGN
            idx           = idx + 1
            ttblItem.seq  = idx
            ttblItem.level = LENGTH(ttblItem.mneumonic)
           .
        IF ttblItem.level GT 3 THEN
        ttblItem.level = 3.
        IF idx GT iHiCount THEN
        iHiCount = idx.
    END. /* each ttblitem */

    ASSIGN
        dMaxWindow = IF {&maxWindow} GE {&minWindowHeight} THEN {&maxWindow}
                     ELSE {&minWindowHeight}
        {&WINDOW-NAME}:COLUMN = 3
        {&WINDOW-NAME}:ROW = 1.5
        {&WINDOW-NAME}:VIRTUAL-HEIGHT = dMaxWindow
        {&WINDOW-NAME}:HEIGHT = dMaxWindow
        {&WINDOW-NAME}:MAX-HEIGHT = dMaxWindow
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = dMaxWindow
        FRAME {&FRAME-NAME}:HEIGHT = dMaxWindow
        linkRect:ROW = {&WINDOW-NAME}:HEIGHT - .43
        linkRect:COL = {&WINDOW-NAME}:WIDTH - 27
        svLink:ROW = {&WINDOW-NAME}:HEIGHT - .19
        svLink:COL = {&WINDOW-NAME}:WIDTH - 25
        iObjectCount = 0
        .
    RUN pCreateMenuObjects ("file").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserSettings MAINMENU 
PROCEDURE pGetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLanguage AS INTEGER NO-UNDO.
    
    ASSIGN
        iLanguage = 1 /* english */
        iMenuSize = 1 /* small menu */
        .
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "MainMenu"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN
    ASSIGN
        iLanguage = INTEGER(user-print.field-value[1])
        iMenuSize = INTEGER(user-print.field-value[2])
        .
    IF iLanguage LT 1 THEN iLanguage = 1.
    IF iMenuSize LT 1 THEN iMenuSize = 1.
    cLabelLanguage = ENTRY(iLanguage,cLanguageList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImages MAINMENU 
PROCEDURE pImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrgmName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttblImage.
    
    INPUT FROM VALUE(SEARCH("menuImages.dat")) NO-ECHO.
    REPEAT:
        IMPORT cPrgmName cImageFile.
        CREATE ttblImage.
        ASSIGN
            ttblImage.prgmName  = cPrgmName
            ttblImage.imageFile = cImageFolder + cImageFile
            .
    END.
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit MAINMENU 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMenuExt AS CHARACTER NO-UNDO INITIAL "lst".
    
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ g_company
          AND sys-ctrl.name    EQ "CEMENU"
        NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        IF sys-ctrl.char-fld EQ "Corrware" THEN cMenuExt = "cor".
        ELSE
        IF sys-ctrl.char-fld EQ "Foldware" THEN cMenuExt = "fol".

    CASE iMenuSize:
        WHEN 1 THEN
        ASSIGN
            cImageFolder  = "Graphics/16X16/"
            dObjectHeight = 1.29
            .
        WHEN 2 THEN
        ASSIGN
            cImageFolder  = "Graphics/24X24/"
            dObjectHeight = 1.67
            .
        WHEN 3 THEN
        ASSIGN
            cImageFolder  = "Graphics/32X32/"
            dObjectHeight = 2.05
            .
    END CASE.
    
    RUN pImages.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cSourceMenu = SEARCH("usermenu/" + USERID("ASI") + "/menu." + cMenuExt)
            iLanguage = LOOKUP(cLabelLanguage,cLanguageList)
            .
        IF cSourceMenu EQ ? THEN cSourceMenu = SEARCH("menu." + cMenuExt).
        INPUT FROM VALUE(cSourceMenu) NO-ECHO.
        RUN pGetMenu.
        INPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKeyPress MAINMENU 
PROCEDURE pKeyPress :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&Scoped-define validKeys ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()

    DEFINE INPUT PARAMETER ipiLastKey AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cSave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKey  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx   AS INTEGER   NO-UNDO.

    ASSIGN
        cSave      = SUBSTR(cMneumonic,1,2)
        cKey       = CAPS(KEYLABEL(ipiLastKey)) WHEN INDEX("{&validKeys}",KEYLABEL(ipiLastKey)) NE 0
        cMneumonic = IF cKey NE "" THEN cSave + cKey ELSE ""
        .
    DO WHILE cMneumonic NE "":
        idx = idx + 1.
        /* prevents endless loop */
        IF idx GT 5 THEN LEAVE.
        FIND FIRST ttblItem
             WHERE ttblItem.mneumonic EQ cMneumonic
             NO-ERROR.
        IF AVAILABLE ttblItem THEN DO:
            Mneumonic:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cmneumonic.
            RUN pClick (ttblItem.hRectangle).
            cMneumonic = SUBSTR(cMneumonic,1,2).
            LEAVE.
        END.
        ELSE
        ASSIGN
            cSave = SUBSTR(cSave,1,LENGTH(cSave) - 1)
            cMneumonic = cSave + cKey
            .
    END. /* do idx */
    
    IF cMneumonic EQ "" OR idx GT 5 THEN DO:
        ASSIGN
            boxes:HIDDEN IN FRAME {&FRAME-NAME} = NO
            menu-image:HIDDEN = NO
            RECT-10:HIDDEN = NO
            Mneumonic:SCREEN-VALUE = ""
            .
        FOR EACH ttblItem WHERE ttblItem.level GT 1:
            DELETE OBJECT ttblItem.hImage NO-ERROR.
            DELETE OBJECT ttblItem.hEditor NO-ERROR.
            DELETE OBJECT ttblItem.hRectangle NO-ERROR.
        END.
        FOR EACH ttblItem:
            fSetColor(ttblItem.hEditor,NO).
            fSetColor(ttblItem.hRectangle,NO).
        END.
    END. /* if blank */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMenuBar MAINMENU 
PROCEDURE pMenuBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH wk-ptrs
        WHERE wk-ptrs.menu-name NE "popup"
          AND wk-ptrs.menu-name NE "mainmenu"
        :
        wk-ptrs.smenu-ptr:LABEL = fTranslate(wk-ptrs.smenu-ptr:PRIVATE-DATA,NO).
    END. /* each wk-ptrs */
    FOR EACH ttblMenuBar:
        ttblMenuBar.menuBarPtr:LABEL = fTranslate(ttblMenuBar.menuBarPtr:PRIVATE-DATA,NO).
    END. /* each ttblmenubar */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset MAINMENU 
PROCEDURE pReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        Mneumonic:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        dObjectCol = {&startObjectCol}
        {&WINDOW-NAME}:TITLE = fTranslate({&WINDOW-NAME}:PRIVATE-DATA,NO)
                             + " {&awversion}"
                             .
    FOR EACH ttblItem:
        DELETE OBJECT ttblItem.hImage NO-ERROR.
        DELETE OBJECT ttblItem.hEditor NO-ERROR.
        DELETE OBJECT ttblItem.hRectangle NO-ERROR.
    END.
    RUN pMenuBar.
    RUN pCreateMenuObjects ("file").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUserSettings MAINMENU 
PROCEDURE pSetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "MainMenu"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "MainMenu"
            user-print.user-id    = USERID("ASI")
            .
    END. /* if not avail */
    ASSIGN
        user-print.field-value[1] = STRING(iLanguage)
        user-print.field-value[2] = STRING(iMenuSize)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-comp_loc MAINMENU 
PROCEDURE Set-comp_loc :
/*------------------------------------------------------------------------------
      Purpose:     Set Global and Screen Company/Location Values.
      Parameters:  INPUT company & location values
      Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLoc         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocDscr    AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            company_name:SCREEN-VALUE = ipcCompanyName
            loc_loc:SCREEN-VALUE      = ipcLoc
            company_name
            loc_loc
            g_company                 = ipcCompany
            g_loc                     = ipcLoc
            .
    END.
    FIND FIRST sys-ctrl
         WHERE sys-ctrl.company EQ g_company
           AND sys-ctrl.NAME    EQ "bitmap"
         NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = g_company
            sys-ctrl.name    = "bitmap"
            sys-ctrl.descrip = "Graphics\bigboxes"
            .
    END.
    IF AVAILABLE sys-ctrl AND sys-ctrl.descrip<> "" THEN
    boxes:LOAD-IMAGE(sys-ctrl.descrip).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fItemImage MAINMENU 
FUNCTION fItemImage RETURNS CHARACTER
  (ipcPrgmName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST ttblImage
         WHERE ttblImage.prgmName EQ ipcPrgmName
         NO-ERROR.
    RETURN IF AVAILABLE ttblImage THEN ttblImage.imageFile
           ELSE cImageFolder + "error.png".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetColor MAINMENU 
FUNCTION fSetColor RETURNS LOGICAL
  (iphObject AS HANDLE, iplSet AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(iphObject) THEN DO:
        ASSIGN
            iphObject:BGCOLOR = IF iplSet THEN {&highlightColor} ELSE {&BGColor}
            iphObject:FGCOLOR = IF iplSet THEN 15 ELSE ?
            .
        IF iphObject:TYPE EQ "Editor" THEN
        iphObject:FONT = IF iplSet THEN 6 ELSE ?.
    END.
    
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

