&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
{custom/globdefs.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cWIPMenuList        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE hdDynamicWidgetPool AS WIDGET-HANDLE NO-UNDO.

cWIPMenuList = "WIP Create,WIP Issue,WIP Transfer".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btnMenu fiText 
&Scoped-Define DISPLAYED-OBJECTS fiText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnMenu 
     LABEL "Menu" 
     SIZE 20 BY 1.57.

DEFINE VARIABLE fiText AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55.4 BY 1.57
     BGCOLOR 3 FGCOLOR 15 FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 2.38
     BGCOLOR 3 .

DEFINE BUTTON btnFG 
     LABEL "Finished Goods" 
     SIZE 75 BY 3.

DEFINE BUTTON btnRM 
     LABEL "Raw Materials" 
     SIZE 75 BY 3.

DEFINE BUTTON btnWIP 
     LABEL "WIP" 
     SIZE 75 BY 3
     FONT 36.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79.6 BY 2.38
     BGCOLOR 3 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnMenu AT ROW 1.33 COL 3.5 WIDGET-ID 8
     fiText AT ROW 1.33 COL 21.4 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 27.29
         BGCOLOR 15 FGCOLOR 1 FONT 36 WIDGET-ID 100.

DEFINE FRAME frMenu
     btnWIP AT ROW 3.57 COL 2.8 WIDGET-ID 2
     btnFG AT ROW 6.76 COL 2.8 WIDGET-ID 10
     btnRM AT ROW 9.95 COL 2.8 WIDGET-ID 14
     "SharpShooter Menu" VIEW-AS TEXT
          SIZE 34 BY .91 AT ROW 1.71 COL 24 WIDGET-ID 16
          BGCOLOR 3 FGCOLOR 15 FONT 37
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 27.29
         BGCOLOR 15 FGCOLOR 1 FONT 36 WIDGET-ID 200.


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
         TITLE              = "Sharpshooter Menu"
         HEIGHT             = 27.29
         WIDTH              = 80
         MAX-HEIGHT         = 27.29
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 27.29
         VIRTUAL-WIDTH      = 80
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frMenu:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       btnMenu:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Menu".

/* SETTINGS FOR FRAME frMenu
                                                                        */
ASSIGN 
       FRAME frMenu:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sharpshooter Menu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sharpshooter Menu */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMenu
&Scoped-define SELF-NAME btnFG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFG C-Win
ON CHOOSE OF btnFG IN FRAME frMenu /* Finished Goods */
DO:
    HIDE FRAME frMenu.
  
    fiText:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "  Finished Goods Menu".
    
    RUN pCreateDynamicWidgets( 
        INPUT "FG", 
        INPUT "FG Inquiry,Job Inquiry"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMenu C-Win
ON CHOOSE OF btnMenu IN FRAME DEFAULT-FRAME /* Menu */
DO:
    VIEW FRAME frMenu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMenu
&Scoped-define SELF-NAME btnRM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRM C-Win
ON CHOOSE OF btnRM IN FRAME frMenu /* Raw Materials */
DO:
    HIDE FRAME frMenu.
    
    fiText:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "  Raw Materials Menu".
    
    RUN pCreateDynamicWidgets( 
        INPUT "RM", 
        INPUT "RM Inquiry"
        ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWIP C-Win
ON CHOOSE OF btnWIP IN FRAME frMenu /* WIP */
DO:
    HIDE FRAME frMenu.
    
    fiText:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "          WIP Menu".
    
    RUN pCreateDynamicWidgets( 
        INPUT "WIP", 
        INPUT cWIPMenuList
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  RUN enable_UI.
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
  DISPLAY fiText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 btnMenu fiText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE RECT-2 btnWIP btnFG btnRM 
      WITH FRAME frMenu IN WINDOW C-Win.
  VIEW FRAME frMenu IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMenu}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDynamicWidgets C-Win 
PROCEDURE pCreateDynamicWidgets PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMenuType    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMenuList    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdDynamicWidget   AS HANDLE NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWidgetYCordniate AS INTEGER NO-UNDO INIT 60.
    
    DELETE WIDGET-POOL "hdWidgetPool" NO-ERROR.
    CREATE WIDGET-POOL "hdWidgetPool" PERSISTENT.
    
    DO iIndex = 1 TO NUM-ENTRIES(ipcMenuList):
        CREATE BUTTON hdDynamicWidget IN WIDGET-POOL "hdWidgetPool"
            ASSIGN
                FRAME     = FRAME DEFAULT-FRAME:HANDLE
                LABEL     = ENTRY(iIndex,ipcMenuList)
                X         = 10
                Y         = iWidgetYCordniate 
                WIDTH     = 75
                HEIGHT    = 2.14
                SENSITIVE = YES
                VISIBLE   = YES
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN pProgramList IN THIS-PROCEDURE (INPUT ipcMenuType,INPUT ENTRY(iIndex,ipcMenuList)) .
        END TRIGGERS.
        .     
        iWidgetYCordniate = iWidgetYCordniate + 50.        
    END.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProgramList C-Win 
PROCEDURE pProgramList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMenuType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubMenu  AS CHARACTER NO-UNDO.
    
    CASE ipcMenuType:   
        WHEN "WIP" THEN DO:
            CASE ipcSubMenu:
                WHEN "WIP Create" THEN
                    RUN wip\wip-create.w(
                        INPUT g_company,      /* Company Code */
                        INPUT g_loc,          /* Location Code */
                        INPUT "",             /* Primary Job number */
                        INPUT "",             /* Machine Code */
                        INPUT 0,              /* Second Job number */
                        INPUT 0,              /* Form number of the Job */
                        INPUT 0               /* Blank number of the Job */
                        ).
                WHEN "WIP Issue" THEN 
                    RUN wip\wip-issue.w(
                        INPUT g_company,  /* Company Code */
                        INPUT g_loc,      /* Location Code */
                        INPUT "",         /* Primary Job number */
                        INPUT 0,          /* Second Job number */
                        INPUT 0,          /* Form number of the Job */
                        INPUT 0           /* Blank number of the Job */ 
                        ).         
                WHEN "WIP Transfer" THEN 
                    RUN wip\wip-transfer.w(
                        INPUT g_company,    /* Company Code */
                        INPUT g_loc,        /* Location Code */
                        INPUT "",           /* Primary Job number */
                        INPUT 0,            /* Second Job number */
                        INPUT 0,            /* Form number of the Job */
                        INPUT 0             /* Blank number of the Job */
                        ).                                                    
            END CASE.
        END.
        WHEN "FG" THEN DO:
            CASE ipcSubMenu:
                WHEN "FG Inquiry" THEN 
                    RUN sharpshooter/w-fgInquiry.w.
                WHEN "Job Inquiry" THEN 
                    RUN sharpshooter/w-jobInquiry.w.        
            END CASE.
        END.
        WHEN "RM" THEN DO:
            CASE ipcSubMenu:
                WHEN "RM Inquiry" THEN 
                    RUN sharpshooter/w-rmInquiry.w.        
            END CASE.        
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

