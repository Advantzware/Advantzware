&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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

  File: inventory\job-details.w

  Description: Displays Job Header, Materials and Machine Details

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

/* Local Variable Definitions ---                                       */
{system/sysconst.i}

DEFINE VARIABLE hdJobProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cJobNo2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormNoListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlankNoListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.

RUN jc\JobProcs.p PERSISTENT SET hdJobProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-31 RECT-32 rSelected btFGItems ~
btMaterials btRoutings btnFirst btnPrevious btnNext btnLast 
&Scoped-Define DISPLAYED-OBJECTS fiJobNo cbJobNo2 cbFormNo cbBlankNo ~
fiJoblabel fiFormlabel fiBlankLabel fiStatusLabel fiStatus fiCreatedLabel ~
fiCreated fiDueLabel fiDue fiCSRLabel fiCSR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-job-hdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartssheader AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btFGItems 
     LABEL "FG Items" 
     SIZE 30 BY 2.52
     FONT 37.

DEFINE BUTTON btMaterials 
     LABEL "Materials" 
     SIZE 30 BY 2.52
     FONT 37.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U
     LABEL "First" 
     SIZE 11 BY 2.62 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U
     LABEL "Last" 
     SIZE 11 BY 2.62 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U
     LABEL "Next" 
     SIZE 11 BY 2.62 TOOLTIP "Next".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U
     LABEL "Prev" 
     SIZE 11 BY 2.62 TOOLTIP "Previous".

DEFINE BUTTON btRoutings 
     LABEL "Routings" 
     SIZE 30 BY 2.52
     FONT 37.

DEFINE VARIABLE cbBlankNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbFormNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiBlankLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Blank #:" 
     VIEW-AS FILL-IN 
     SIZE 14.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiCreated AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiCreatedLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Created:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiCSR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiCSRLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CSR:" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiDue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiDueLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Due:" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiFormlabel AS CHARACTER FORMAT "X(256)":U INITIAL "Form #:" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiJoblabel AS CHARACTER FORMAT "X(256)":U INITIAL "Job #:" 
     VIEW-AS FILL-IN 
     SIZE 11.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiStatusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Status:" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 132 BY 4.76.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 46 BY 4.76.

DEFINE RECTANGLE rSelected
     EDGE-PIXELS 0    
     SIZE 33 BY 3.29
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiJobNo AT ROW 5.29 COL 19.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbJobNo2 AT ROW 5.33 COL 61.6 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     cbFormNo AT ROW 5.33 COL 87.8 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cbBlankNo AT ROW 5.33 COL 116.2 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiJoblabel AT ROW 5.43 COL 7.6 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     fiFormlabel AT ROW 5.43 COL 73.4 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fiBlankLabel AT ROW 5.43 COL 101.2 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     fiStatusLabel AT ROW 5.43 COL 148 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     fiStatus AT ROW 5.43 COL 160.6 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiCreatedLabel AT ROW 7.67 COL 8 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     fiCreated AT ROW 7.67 COL 23.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     fiDueLabel AT ROW 7.67 COL 48.8 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiDue AT ROW 7.67 COL 58 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiCSRLabel AT ROW 7.67 COL 151.4 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiCSR AT ROW 7.67 COL 160.4 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     btFGItems AT ROW 10.81 COL 7.6 WIDGET-ID 118
     btMaterials AT ROW 10.81 COL 39.2 WIDGET-ID 122
     btRoutings AT ROW 10.81 COL 70.8 WIDGET-ID 120
     btnFirst AT ROW 14.33 COL 192 WIDGET-ID 44
     btnPrevious AT ROW 17.14 COL 192 WIDGET-ID 40
     btnNext AT ROW 28.14 COL 192 WIDGET-ID 42
     btnLast AT ROW 31 COL 192 WIDGET-ID 46
     "-" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 5.62 COL 61.6 WIDGET-ID 86
          FONT 37
     RECT-31 AT ROW 4.81 COL 6 WIDGET-ID 114
     RECT-32 AT ROW 4.81 COL 146 WIDGET-ID 116
     rSelected AT ROW 10.43 COL 6 WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 36.19
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.job
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Job Details"
         HEIGHT             = 32.81
         WIDTH              = 204.8
         MAX-HEIGHT         = 48.76
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 48.76
         VIRTUAL-WIDTH      = 273.2
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cbBlankNo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbFormNo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbJobNo2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBlankLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCreated IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCreatedLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCSR IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCSRLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDue IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDueLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormlabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJoblabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJobNo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusLabel IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Details */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFGItems
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFGItems W-Win
ON CHOOSE OF btFGItems IN FRAME F-Main /* FG Items */
DO:
    RUN pHighlightButton (
        "FGItems"
        ).        

    RUN select-page(1).        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMaterials
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMaterials W-Win
ON CHOOSE OF btMaterials IN FRAME F-Main /* Materials */
DO:
    RUN pHighlightButton (
        "Materials"
        ).            
        
    RUN select-page(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Prev */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRoutings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRoutings W-Win
ON CHOOSE OF btRoutings IN FRAME F-Main /* Routings */
DO:
    RUN pHighlightButton (
        "Routings"
        ).  
    
    RUN select-page(3).
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartssheader.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartssheader ).
       RUN set-position IN h_smartssheader ( 1.24 , 3.80 ) NO-ERROR.
       /* Size in UIB:  ( 3.14 , 200.00 ) */

       /* Links to SmartObject h_smartssheader. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Header':U , h_smartssheader ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_smartssheader ,
             fiJobNo:HANDLE IN FRAME F-Main , 'BEFORE':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'inventory/b-job-hdr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-hdr ).
       RUN set-position IN h_b-job-hdr ( 14.33 , 7.40 ) NO-ERROR.
       RUN set-size IN h_b-job-hdr ( 19.29 , 181.60 ) NO-ERROR.

       /* Links to SmartBrowser h_b-job-hdr. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_1':U , h_b-job-hdr ).
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-hdr ,
             btRoutings:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'inventory/b-job-mat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mat ).
       RUN set-position IN h_b-job-mat ( 14.29 , 7.40 ) NO-ERROR.
       RUN set-size IN h_b-job-mat ( 19.33 , 181.60 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-job-mat. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_2':U , h_b-job-mat ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-mat ,
             btRoutings:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'inventory/b-job-mch.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mch ).
       RUN set-position IN h_b-job-mch ( 14.33 , 7.40 ) NO-ERROR.
       RUN set-size IN h_b-job-mch ( 19.29 , 181.60 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-job-mch. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , h_b-job-mch ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_3':U , h_b-job-mch ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-mch ,
             btRoutings:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

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
  DISPLAY fiJobNo cbJobNo2 cbFormNo cbBlankNo fiJoblabel fiFormlabel 
          fiBlankLabel fiStatusLabel fiStatus fiCreatedLabel fiCreated 
          fiDueLabel fiDue fiCSRLabel fiCSR 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-31 RECT-32 rSelected btFGItems btMaterials btRoutings btnFirst 
         btnPrevious btnNext btnLast 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHighlightButton W-Win 
PROCEDURE pHighlightButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcButtonType AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    CASE ipcButtonType:
        WHEN "FGItems" THEN
            rSelected:COL = btFGItems:COL - 1.5.
        WHEN "Materials" THEN
            rSelected:COL = btMaterials:COL - 1.5.
        WHEN "Routings" THEN
            rSelected:COL = btRoutings:COL - 1.5.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cCurrentPgmName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE char-hdl        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle         AS HANDLE    NO-UNDO.
    
    SESSION:SET-WAIT-STATE("GENERAL").   
    
    ASSIGN
        cCurrentPgmName = IF INDEX(THIS-PROCEDURE:NAME,"/") GT 0 THEN
                              SUBSTRING(THIS-PROCEDURE:NAME, R-INDEX(THIS-PROCEDURE:NAME, "/") + 1)
                          ELSE
                              SUBSTRING(THIS-PROCEDURE:NAME, R-INDEX(THIS-PROCEDURE:NAME, "\") + 1)
        cCurrentPgmName = SUBSTRING(cCurrentPgmName,1,INDEX(cCurrentPgmName,"."))
        .

    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmName EQ cCurrentPgmName
         NO-ERROR.
    IF AVAILABLE prgrms THEN
        {&WINDOW-NAME}:TITLE = prgrms.prgtitle.
        
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany NO-ERROR .
    IF AVAILABLE company THEN DO:    
        {methods\run_link.i 
            "Header-TARGET" 
            "UpdateHeaderDetails" 
            "(STRING(company.name),ipcLocation,{&WINDOW-NAME}:TITLE)"}
            
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                             + STRING(company.name) + " - " + ipcLocation.                
    END.    

    RUN pJobScan (
        ipcCompany,
        ipcJobNo,
        ipiJobNo2,
        ipiFormNo,
        ipiBlankNo
        ).

    APPLY "CHOOSE" TO btFGItems IN FRAME {&FRAME-NAME}.

    IF AVAILABLE job THEN
        ASSIGN
            fiStatus:SCREEN-VALUE  = job.stat
            fiCreated:SCREEN-VALUE = IF job.create-date EQ ? THEN
                                         ""
                                     ELSE
                                         STRING(job.create-date)
            fiDue:SCREEN-VALUE     = IF job.due-date EQ ? THEN
                                         ""
                                     ELSE
                                         STRING(job.due-date)
            fiCSR:SCREEN-VALUE     = csrUser_id
            .
    APPLY "ENTRY" TO btFGitems.
    
    SESSION:SET-WAIT-STATE("").            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobScan W-Win 
PROCEDURE pJobScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN ValidateJobHdr IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipiJobno2,
        OUTPUT lValidJob
        ).
        
    IF NOT lValidJob THEN 
    DO: 
        MESSAGE "Invalid Job number, please enter a valid Job number" 
            VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO fiJobNo.
        RETURN ERROR.
    END.    

    fiJobNo:SCREEN-VALUE = ipcJobno.
    
    RUN pUpdateComboBoxes (
        INPUT ipcCompany
        ).
       
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cbJobNo2:LIST-ITEMS    = cJobno2ListItems
            cbFormNo:LIST-ITEMS    = cFormnoListItems
            cbBlankNo:LIST-ITEMS   = cBlanknoListItems
            cbJobNo2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
            cbFormNo:SCREEN-VALUE  = STRING(ipiFormno,"99")
            cbBlankNo:SCREEN-VALUE = STRING(ipiBlankno,"99")
            NO-ERROR.
    END.
    
    RUN pUpdateBrowse (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT ipiJobNo2
        ) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hdCurrentBrowse AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cHandle         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLink           AS CHARACTER NO-UNDO.

    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).   

    cLink = "PAGE_" + STRING(RETURN-VALUE) + "-TARGET".
 
    RUN get-link-handle IN adm-broker-hdl (
        INPUT THIS-PROCEDURE,
        INPUT cLink,
        OUTPUT cHandle
        ).
  
    IF NUM-ENTRIES(cHandle) LT 2 THEN
        hdCurrentBrowse = WIDGET-HANDLE(cHandle).
   
    IF VALID-HANDLE(hdCurrentBrowse) THEN
        RUN dispatch IN hdCurrentBrowse ("get-" + iphNavPanel:LABEL).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateBrowse W-Win 
PROCEDURE pUpdateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO. 

    DO WITH FRAME {&FRAME-NAME}:
    END.
   
    {methods\run_link.i 
         "Record-SOURCE" 
         "pOpenQuery" 
         "(ipcCompany,ipcJobNo,ipiJobNo2)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateComboBoxes W-Win 
PROCEDURE pUpdateComboBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListItems = ""
        .
        
    RUN GetSecondaryJobForJob IN hdJobProcs (
        ipcCompany,
        fiJobNo:SCREEN-VALUE,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            ipcCompany,
            fiJobNo:SCREEN-VALUE,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            ipcCompany,
            fiJobNo:SCREEN-VALUE,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).
    END.
    
    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobNo2ListItems      = "00"
            cbJobNo2:LIST-ITEMS   = cJobno2ListItems 
            cbJobNo2:SCREEN-VALUE = "00".
    ELSE
        cbJobNo2:LIST-ITEMS = cJobno2ListItems.
 
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems      = "00"
            cbFormNo:LIST-ITEMS   = cFormnoListItems 
            cbFormNo:SCREEN-VALUE = "00".
    ELSE
        cbFormNo:LIST-ITEMS = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems      = "00"
            cbBlankNo:LIST-ITEMS   = cBlanknoListItems
            cbBlankNo:SCREEN-VALUE = "00".
    ELSE
        cbBlankNo:LIST-ITEMS = cBlanknoListItems.
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

