&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME MAINMENU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS MAINMENU 
/*------------------------------------------------------------------------
 
  File:              mainmenu.w
 
  Description:       Main Menu
 
  Input Parameters:  <none>
 
  Output Parameters: <none>
 
  Author:            Ron Stark
 
  Created:           01/25/98 -  12:36 am
 
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
 
&Scoped-define start-button-col 2
&Scoped-define start-button-row 3.5
&Scoped-define button-height 1.1
&Scoped-define button-width 40
&Scoped-define button-gap .05
&Scoped-define min-window-height 6.5
&Scoped-define min-window-width 122
&Scoped-define max-window {&start-button-row} - .5 + ~
({&button-height} + {&button-gap}) * ttbl-menu.menu-count

{methods/defines/mainmenu.i}

/* System Constant Values */
{system/sysconst.i}

DEFINE TEMP-TABLE ttPersistent NO-UNDO
    FIELD prgmTitle AS CHARACTER
        INDEX ttPersistent IS PRIMARY prgmTitle.

DEFINE TEMP-TABLE ttbl NO-UNDO
    FIELD menu-order AS INTEGER
    FIELD menu1      AS CHARACTER
    FIELD menu2      AS CHARACTER
    INDEX ttbl IS PRIMARY UNIQUE menu-order
    INDEX menu2                  menu2      menu-order.

DEFINE TEMP-TABLE ttbl-menu NO-UNDO
    FIELD menu-name  AS CHARACTER
    FIELD menu-count AS INTEGER
    INDEX ttbl-menu IS PRIMARY UNIQUE menu-name
    INDEX menu-count                  menu-count.

DEFINE VARIABLE closeMenu        AS LOGICAL       NO-UNDO.
DEFINE VARIABLE button-widget    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE button-row       AS DECIMAL       NO-UNDO.
DEFINE VARIABLE button-col       AS DECIMAL       NO-UNDO INITIAL {&start-button-col}.
DEFINE VARIABLE button-count     AS INTEGER       NO-UNDO.
DEFINE VARIABLE button-mneumonic AS CHARACTER     NO-UNDO INITIAL
    /*'A,B,C,D,E,N,G,H,O,J,K,L,M,S,X,1,2,3,4,5,6,7,8,9,P,Q,S,T,U,V,W,X,Y,Z'.*/
    'A,B,C,D,E,G,H,I,J,K,L,M,O,S,X,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P'.

DEFINE VARIABLE cEulaFile        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lEulaAccepted    AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cEulaVersion     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lUserExit        AS LOGICAL NO-UNDO.
DELETE WIDGET-POOL "dyn-buttons" NO-ERROR.

ASSIGN
    g_company = ""
    g_loc     = "".
RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,YES).
IF g_company = "" OR g_loc = "" THEN
DO:
    MESSAGE "No Company and/or Location found for your login ID." SKIP
        "Please Contact System's Administrator." VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
END.
cEulaFile = SEARCH("{&EulaFile}").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-USER

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS boxes menu-image RECT-2 
&Scoped-Define DISPLAYED-OBJECTS users_user_id company_name loc_loc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR MAINMENU AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE company_name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 43 BY .71
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE loc_loc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE users_user_id AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE IMAGE boxes
     FILENAME "Graphics/advantzware_logo.jpg":U
     SIZE 79 BY 17.38.

DEFINE IMAGE menu-image
     FILENAME "Graphics/logo1.bmp":U CONVERT-3D-COLORS
     SIZE 79 BY 4.52.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8    
     SIZE 120 BY 1.91
     BGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-USER
     users_user_id AT ROW 1.95 COL 14 COLON-ALIGNED NO-LABEL
     company_name AT ROW 1.95 COL 41 COLON-ALIGNED NO-LABEL
     loc_loc AT ROW 1.95 COL 97 COLON-ALIGNED NO-LABEL
     "Company:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.95 COL 31
          BGCOLOR 0 FGCOLOR 15 FONT 6
     " User ID:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.95 COL 5
          BGCOLOR 0 FGCOLOR 15 FONT 6
     "Location:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.95 COL 87
          BGCOLOR 0 FGCOLOR 15 FONT 6
     boxes AT ROW 8.14 COL 43
     menu-image AT ROW 3.38 COL 43
     RECT-2 AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200 BY 40
         BGCOLOR 7 FGCOLOR 15 .


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
         HEIGHT             = 24.52
         WIDTH              = 122.6
         MAX-HEIGHT         = 40
         MAX-WIDTH          = 235.6
         VIRTUAL-HEIGHT     = 40
         VIRTUAL-WIDTH      = 235.6
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT MAINMENU:LOAD-ICON("adeicon/progress.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/progress.ico"
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
/* SETTINGS FOR FILL-IN company_name IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN loc_loc IN FRAME FRAME-USER
   NO-ENABLE                                                            */
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
        IF USERID('nosweat') NE "Nosweat" THEN
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

{sys/inc/f3helpm.i} /* ASI F3 key include */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
 
/* Now enable the interface and wait for the exit condition.*/
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
        sys-ctrl.NAME = "bitmap" NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = g_company
            sys-ctrl.name    = "bitmap"
            sys-ctrl.descrip = "Graphics\bigboxes".
    END.
    IF AVAILABLE sys-ctrl AND sys-ctrl.DESCrip <> "" THEN
        boxes:LOAD-IMAGE(sys-ctrl.DESCrip).
    
          
    {methods/mainmenu.i}
    
    RUN Read_Menus.
  
  
  
  
    /*run Consultingwerk/WindowIntegrationKit/Samples/start.p */
  
  
  
     
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
/*WAIT-FOR System.Windows.Forms.Application:Run() . */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_Buttons MAINMENU 
PROCEDURE Create_Buttons :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER menu-item AS CHARACTER NO-UNDO.
    DEFINE VARIABLE button-label AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPgmSecurity AS HANDLE NO-UNDO.
    DEFINE VARIABLE lResult AS LOG NO-UNDO.

    button-row = {&start-button-row}.
    FOR EACH ttbl NO-LOCK WHERE ttbl.menu2 = menu-item:
        FIND prgrms WHERE prgrms.prgmname = ttbl.menu1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE prgrms AND ttbl.menu1 NE 'Exit' THEN
            NEXT.
        button-label = (IF AVAILABLE prgrms THEN prgrms.prgtitle ELSE ttbl.menu1) +
            (IF INDEX(ttbl.menu1,'.') NE 0 OR ttbl.menu1 = 'Exit' THEN ''
            ELSE ' >').

          IF AVAILABLE prgrms AND prgrms.prgmname EQ "file."  THEN DO
              RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
              RUN epCanAccess IN hPgmSecurity ("system/mainmenu.w", "", OUTPUT lResult).
              DELETE OBJECT hPgmSecurity.
  
              IF NOT lResult 
                  AND AVAILABLE prgrms AND prgrms.prgmname EQ "file." THEN NEXT.
          END.

         IF AVAILABLE prgrms AND prgrms.prgmname EQ "custproc."  THEN DO: /*NEXT .*/ /* ticket - 23865  */
             RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
             RUN epCanAccess IN hPgmSecurity ("system/mainmenu.w", "Access1", OUTPUT lResult).
             DELETE OBJECT hPgmSecurity. 
             IF NOT lResult THEN
                 NEXT.
         END.
        
        RUN Mneumonic (INPUT-OUTPUT button-label).

        CREATE BUTTON button-widget IN WIDGET-POOL "dyn-buttons"
            ASSIGN
            FRAME = FRAME {&FRAME-NAME}:HANDLE
            NAME = ttbl.menu1
            SENSITIVE = TRUE
            LABEL = button-label
            COLUMN = button-col
            ROW = button-row
            HEIGHT-CHARS = {&button-height}
            WIDTH-CHARS = {&button-width}
            HIDDEN = NO
            /*  MANUAL-HIGHLIGHT = TRUE */
            TRIGGERS:
                ON CHOOSE 
                    PERSISTENT RUN Run_Button IN THIS-PROCEDURE (button-widget:HANDLE).
            END TRIGGERS.
    
        button-row = button-row + {&button-height} + {&button-gap}.       
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY users_user_id company_name loc_loc 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  ENABLE boxes menu-image RECT-2 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-USER}
  VIEW MAINMENU.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mneumonic MAINMENU 
PROCEDURE Mneumonic :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iop-label AS CHARACTER NO-UNDO.

    IF iop-label = "Exit" THEN iop-label = "E&xit".
    ELSE IF button-col > 80 THEN  /* third column */
            ASSIGN
                button-count = button-count + 1
                iop-label    = '&' + string(button-count) + ' ' + iop-label. 
        ELSE
            ASSIGN
                iop-label    = '&' + iop-label
                button-count = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Read_Menus MAINMENU 
PROCEDURE Read_Menus :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE m           AS CHARACTER EXTENT 3 NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-menu-lst AS cha       EXTENT 2 NO-UNDO.
    DEFINE VARIABLE ll-est-only AS LOG       NO-UNDO.

  
    FOR EACH ttbl EXCLUSIVE-LOCK:
        DELETE ttbl.
    END.
    FOR EACH ttbl-menu EXCLUSIVE-LOCK:
        DELETE ttbl-menu.
    END.

    CREATE WIDGET-POOL "dyn-buttons" PERSISTENT.

    /* ============= dynamic menu for foldware/corrware ============*/
    ASSIGN
        ll-est-only    = SEARCH("menuest.r") NE ?
        ls-menu-lst[1] = "menu"
        ls-menu-lst[2] = "lst".

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ g_company
        AND sys-ctrl.name    EQ "cemenu"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        IF sys-ctrl.char-fld EQ "Corrware" THEN ls-menu-lst[2] = "cor".
        ELSE
            IF sys-ctrl.char-fld EQ "Foldware" THEN ls-menu-lst[2] = "fol".

    ls-menu-lst[1] = TRIM(ls-menu-lst[1]) + "." + TRIM(ls-menu-lst[2]).
    /* ========== end of mods =========================*/

    IF SEARCH("usermenu\" + USERID("nosweat") + "\" + ls-menu-lst[1]) <> ? THEN
        ls-menu-lst[1] = SEARCH("usermenu\" + USERID("nosweat") + "\" + ls-menu-lst[1]).
    ELSE IF SEARCH(ls-menu-lst[1]) <> ? THEN
        ls-menu-lst[1] = SEARCH(ls-menu-lst[1]) .

    INPUT FROM VALUE(ls-menu-lst[1]) NO-ECHO.
    REPEAT:
        m = "".
        IMPORT m[1] m[2] m[3].
        IF CAN-DO('RULE,SKIP',m[1]) OR
            (ll-est-only AND m[3] NE "est") THEN NEXT.
        CREATE ttbl.
        ASSIGN
            i               = i + 1
            ttbl.menu-order = i
            ttbl.menu1      = m[1]
            ttbl.menu2      = m[2].
        FIND ttbl-menu WHERE ttbl-menu.menu-name = m[2] EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE ttbl-menu THEN
        DO:
            CREATE ttbl-menu.
            ttbl-menu.menu-name = m[2].
            IF m[2] = 'file' THEN
                ttbl-menu.menu-count = 1.      
        END.
        ttbl-menu.menu-count = ttbl-menu.menu-count + 1.
    /*===== make this program slower a little bit  : run util/loadmod.p instead
    /* build modules for liscense */
    FIND FIRST asi.module WHERE asi.module.module = ttbl.menu1 NO-LOCK NO-ERROR.
    IF NOT AVAIL asi.module THEN DO:
       FIND prgrms WHERE prgrms.prgmname = ttbl.menu1 NO-LOCK NO-ERROR.
       CREATE asi.module.
       ASSIGN asi.module.db-name = "ASI"
              asi.module.module = ttbl.menu1
              asi.module.dscr = IF AVAIL prgrms THEN prgrms.prgmname ELSE ""
              asi.module.is-used = YES.
    END.
    =============*/
    END.
    INPUT CLOSE.
    CREATE ttbl.
    ASSIGN
        i               = i + 1
        ttbl.menu-order = i
        ttbl.menu1      = 'Exit'
        ttbl.menu2      = 'file'
        button-col      = {&start-button-col}.

    FIND LAST ttbl-menu NO-LOCK USE-INDEX menu-count.
    ASSIGN
        {&WINDOW-NAME}:ROW          = 1
        {&WINDOW-NAME}:HEIGHT-CHARS = {&max-window}
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-CHARS = {&max-window}
    {&WINDOW-NAME}:MAX-HEIGHT-CHARS = {&max-window}
    button-count = 0.
  

    RUN Create_Buttons('file').
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run_Button MAINMENU 
PROCEDURE Run_Button :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER button-handle AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE save-widget    AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hDelete AS HANDLE  NO-UNDO EXTENT 100.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.
    
    IF button-handle:NAME EQ 'Exit' THEN
    APPLY 'WINDOW-CLOSE':U TO {&WINDOW-NAME}.

    ASSIGN
        current-widget = FRAME {&FRAME-NAME}:HANDLE
        current-widget = current-widget:FIRST-CHILD
        current-widget = current-widget:FIRST-CHILD.
    DO WHILE current-widget NE ?:
        IF current-widget:TYPE = 'BUTTON' THEN DO:
            IF current-widget:COLUMN GT button-handle:COLUMN AND
                current-widget:DYNAMIC THEN DO:
                IF NOT CAN-DO('&F File Maintenance >,&I Inquiries >,&R Reports >',
                    current-widget:LABEL) AND
           INDEX(current-widget:LABEL,'&') NE 0 THEN button-count = button-count - 1.
                save-widget = current-widget:NEXT-SIBLING.
                DELETE WIDGET current-widget.
                current-widget = save-widget.
            END.
            ELSE
                ASSIGN
                    current-widget:FONT = IF current-widget:COLUMN = button-handle:COLUMN THEN ? ELSE current-widget:FONT
                    current-widget      = current-widget:NEXT-SIBLING.
        END.
        ELSE
            current-widget = current-widget:NEXT-SIBLING.
    END.
    ASSIGN
        button-handle:FONT = 6
        button-col         = button-handle:COLUMN + {&button-width} + {&button-gap}.
    IF INDEX(button-handle:NAME,'.') = 0 THEN RUN Create_Buttons(button-handle:NAME).
    ELSE DO:
        /* check module license first before run it YSK 08/24/04 TASK# 08060406 */
        RUN util/CheckModule.p ("ASI", button-handle:NAME, YES, OUTPUT lAccess) NO-ERROR.
        IF lAccess THEN 
        RUN Get_Procedure IN Persistent-Handle(button-handle:NAME,OUTPUT run-proc,YES).
        
    END.

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
    DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-company_name AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-loc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-loc_dscr AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            /*company_company:SCREEN-VALUE = ip-company  */
            company_name:SCREEN-VALUE = ip-company_name
            loc_loc:SCREEN-VALUE      = ip-loc
            /*      loc_dscr:SCREEN-VALUE = ip-loc_dscr  */
            /* company_company */
            company_name
            loc_loc
            /*  loc_dscr */
            g_company                 = ip-company
            g_loc                     = ip-loc.
    END.
    FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
        sys-ctrl.NAME = "bitmap" NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = g_company
            sys-ctrl.name    = "bitmap"
            sys-ctrl.descrip = "Graphics\bigboxes".
    END.
    IF AVAILABLE sys-ctrl AND sys-ctrl.DESCrip <> "" THEN
        boxes:LOAD-IMAGE(sys-ctrl.DESCrip).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

