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
DEFINE VARIABLE cNewLicense AS CHARACTER .
DEFINE VARIABLE iCnt        AS INTEGER.
FIND FIRST asi._license NO-LOCK.
DEFINE VARIABLE cExpireDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE dExpireDate AS DATE      NO-UNDO.
DEFINE VARIABLE cNumUsers   AS CHARACTER NO-UNDO.
DEFINE VARIABLE inumUsers   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSite       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSite       AS INTEGER   NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS fiNewLic btAccept 
&Scoped-Define DISPLAYED-OBJECTS fiNewLic 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAccept 
    LABEL "Accept New License" 
    SIZE 23 BY 1.14.

DEFINE VARIABLE fiNewLic AS CHARACTER FORMAT "X(256)":U 
    LABEL "New License Code" 
    VIEW-AS FILL-IN 
    SIZE 48 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    fiNewLic AT ROW 4.57 COL 20 COLON-ALIGNED WIDGET-ID 2
    btAccept AT ROW 8.38 COL 31 WIDGET-ID 4
    "Enter the license code obtained from Advantzware" VIEW-AS TEXT
    SIZE 49 BY .62 AT ROW 2.19 COL 13 WIDGET-ID 6
    FGCOLOR 1 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 80 BY 10.81 WIDGET-ID 100.


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
        TITLE              = "Advantzware License Code Entry"
        HEIGHT             = 10.81
        WIDTH              = 80
        MAX-HEIGHT         = 28.81
        MAX-WIDTH          = 146.2
        VIRTUAL-HEIGHT     = 28.81
        VIRTUAL-WIDTH      = 146.2
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
    THEN wWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Advantzware License Code Entry */
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
ON WINDOW-CLOSE OF wWin /* Advantzware License Code Entry */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAccept btAccept
ON CHOOSE OF btAccept /* Advantzware License Code Entry */
    DO:
     

    
        cNewLicense = CAPS(fiNewLic:SCREEN-VALUE).
        DO iCnt = 1 TO LENGTH(cNewLicense):
            IF ASC(SUBSTRING(cNewLicense, iCnt, 1)) GE 65 AND
                ASC(SUBSTRING(cNewLicense, iCnt, 1)) LE 91 THEN 
                SUBSTRING(cNewLicense, iCnt, 1) = "9".
            SUBSTRING(cNewLicense, iCnt, 1) = STRING(9 - INTEGER(SUBSTRING(cNewlicense, iCnt, 1))).
        END.
        ASSIGN 
            cNumUsers   = SUBSTRING(cNewLicense, 6, 4)
            cExpireDate = SUBSTRING(cNewLicense, 10, 8)
            cSite       = SUBSTRING(cnewLicense, 1, 5).
        
        iNumUsers = INTEGER (cNumUsers) NO-ERROR.
        dExpireDate = DATE (cExpireDate)  NO-ERROR. 
        iSite = INTEGER (cSite) NO-ERROR.
    
        FIND FIRST sys-ctrl WHERE sys-ctrl.name = "site number" NO-LOCK NO-ERROR.
        FIND FIRST userControl NO-LOCK. 
        IF AVAILABLE sys-ctrl AND AVAILABLE userControl THEN 
        DO:
            
            IF iSite EQ  sys-ctrl.int-fld 
                AND iNumUsers = userControl.maxAllowedUsers THEN 
            DO:
                FOR EACH module EXCLUSIVE-LOCK WHERE:
                    module.expire-date = dExpireDate.
                END.                
                  
                MESSAGE "The new license code has been accepted."
                    VIEW-AS ALERT-BOX.
            END.
            ELSE 
            DO:
                MESSAGE "Incorrect license code entered."
                    VIEW-AS ALERT-BOX.
            END. 
            
            
        END.       
        ELSE 
        DO:
            MESSAGE "Error - missing NK1 for site number."
                VIEW-AS ALERT-BOX.
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
    DISPLAY fiNewLic 
        WITH FRAME fMain IN WINDOW wWin.
    ENABLE fiNewLic btAccept 
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

