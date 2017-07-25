&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT  PARAMETER iprEstRowid    AS ROWID         NO-UNDO.
DEFINE OUTPUT PARAMETER oplChangesMade AS LOGICAL       NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btSaveAll btReset 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win        AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-multiv   AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE        NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btRecalc DEFAULT  NO-FOCUS
    LABEL "Recalc All" 
    SIZE 18 BY 1.19.

DEFINE BUTTON btReset 
    LABEL "Reset All" 
    SIZE 34 BY 1.19.

DEFINE BUTTON btSaveAll 
    LABEL "Save All" 
    SIZE 32.6 BY 1.19.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 68.4 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btRecalc AT ROW 20.52 COL 78 WIDGET-ID 2
    btSaveAll AT ROW 20.57 COL 34.4 WIDGET-ID 8
    btReset AT ROW 20.57 COL 67 WIDGET-ID 4
    RECT-1 AT ROW 20.33 COL 33.6 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 105 BY 21.05 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW W-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Estimate Form"
        HEIGHT             = 21.05
        WIDTH              = 105
        MAX-HEIGHT         = 25.81
        MAX-WIDTH          = 216.8
        VIRTUAL-HEIGHT     = 25.81
        VIRTUAL-WIDTH      = 216.8
        ALWAYS-ON-TOP      = YES
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btRecalc IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    btRecalc:HIDDEN IN FRAME F-Main = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
    THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Estimate Form */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Estimate Form */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        DEFINE VARIABLE lAllSaved AS LOGICAL NO-UNDO.

        RUN checkAllSaved IN h_b-multiv (OUTPUT lAllSaved).
        IF NOT lAllSaved THEN 
        DO:

            MESSAGE "You have made changes that are not saved."  SKIP(1)    
                "Do you want to save your changes?"    
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL    TITLE "" UPDATE lChoice AS LOGICAL.
            CASE lChoice:
                WHEN YES THEN
                RUN saveAll IN h_b-multiv.
                WHEN ? THEN
                    RETURN NO-APPLY.
            END CASE.
        /*IF NOT lChoice THEN 
          RETURN NO-APPLY. */
        END.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRecalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRecalc W-Win
ON CHOOSE OF btRecalc IN FRAME F-Main /* Recalc All */
    DO:
        RUN recalcAll IN h_b-multiv.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset W-Win
ON CHOOSE OF btReset IN FRAME F-Main /* Reset All */
    DO:
        RUN resetAll IN h_b-multiv.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSaveAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSaveAll W-Win
ON CHOOSE OF btSaveAll IN FRAME F-Main /* Save All */
    DO:
        RUN saveAll IN h_b-multiv.
        /* btSaveAll:SENSITIVE = NO. */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
    /*------------------------------------------------------------------------------
      Purpose:     Create handles for all SmartObjects used in this procedure.
                   After SmartObjects are initialized, then SmartLinks are added.
      Parameters:  <none>
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    ASSIGN 
        adm-current-page = INTEGER(RETURN-VALUE).

    CASE adm-current-page: 

        WHEN 0 THEN 
            DO:
                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'est/b-multiv.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Layout = ':U ,
                    OUTPUT h_b-multiv ).
                RUN set-position IN h_b-multiv ( 1.24 , 2.00 ) NO-ERROR.
                RUN set-size IN h_b-multiv ( 19.05 , 100.00 ) NO-ERROR.

                RUN init-object IN THIS-PROCEDURE (
                    INPUT  'p-updcan.w':U ,
                    INPUT  FRAME F-Main:HANDLE ,
                    INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
                    OUTPUT h_p-updcan-2 ).
                RUN set-position IN h_p-updcan-2 ( 20.29 , 2.00 ) NO-ERROR.
                RUN set-size IN h_p-updcan-2 ( 1.76 , 31.00 ) NO-ERROR.

                /* Links to SmartBrowser h_b-multiv. */
                RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_b-multiv ).
                RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'release':U , h_b-multiv ).

                /* Adjust the tab order of the smart objects. */
                RUN adjust-tab-order IN adm-broker-hdl ( h_b-multiv ,
                    btSaveAll:HANDLE IN FRAME F-Main , 'BEFORE':U ).
                RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan-2 ,
                    h_b-multiv , 'AFTER':U ).
            END. /* Page 0 */

    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
    {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
        THEN DELETE WIDGET W-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
    ENABLE RECT-1 btSaveAll btReset 
        WITH FRAME F-Main IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEstRow W-Win 
PROCEDURE getEstRow :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprEb AS ROWID NO-UNDO.

    FIND FIRST eb WHERE ROWID(eb) EQ iprEstRowid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE eb THEN
        FIND FIRST eb WHERE eb.company EQ '001' AND eb.est-no = "   12786" NO-LOCK NO-ERROR.

    IF AVAILABLE eb THEN DO:
       ASSIGN oprEb = ROWID(eb) .
              w-win:TITLE = "Estimate Form for " + TRIM(eb.est-no).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
    /* -----------------------------------------------------------
      Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
      Parameters:  <none>
      Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
    -------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

/* SEND-RECORDS does nothing because there are no External
   Tables specified for this SmartWindow, and there are no
   tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setChangesMade W-Win
PROCEDURE setChangesMade:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplChangesMade AS LOGICAL NO-UNDO.
oplChangesMade = iplChangesMade.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

