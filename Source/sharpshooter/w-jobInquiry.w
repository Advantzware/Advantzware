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

  File: sharpshooter\job-details.w

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
/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE hdJobProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cJobNo2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFormattedJobno   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation         AS CHARACTER NO-UNDO.

DEFINE VARIABLE oJobHeader AS jc.JobHeader NO-UNDO.

RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

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
&Scoped-Define ENABLED-OBJECTS btFGItems btMaterials btRoutings btExit ~
btnFirst btnLast btnNext btnPrevious btnExitText btnPrintJobText ~
btnAdjustQtyText btnIssueQtyText btnViewRM btnViewFG btClear btnClearText 
&Scoped-Define DISPLAYED-OBJECTS fiStatusLabel fiStatus fiCreatedLabel ~
fiCreated fiDueLabel fiDue fiCSRLabel fiCSR btnExitText statusMessage ~
btnPrintJobText btnAdjustQtyText btnIssueQtyText btnViewRM btnViewFG ~
btnClearText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-hdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_issueqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_printjob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewfginquiry AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewrminquiry AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btFGItems  NO-FOCUS
     LABEL "FG ITEMS" 
     SIZE 30 BY 2.52
     FONT 17.

DEFINE BUTTON btMaterials  NO-FOCUS
     LABEL "MATERIALS" 
     SIZE 30 BY 2.52
     FONT 17.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btRoutings  NO-FOCUS
     LABEL "ROUTINGS" 
     SIZE 30 BY 2.52
     FONT 17.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 9 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnIssueQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ISSUE QTY" 
      VIEW-AS TEXT 
     SIZE 19 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnPrintJobText AS CHARACTER FORMAT "X(256)":U INITIAL "PRINT JOB" 
      VIEW-AS TEXT 
     SIZE 19 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnViewFG AS CHARACTER FORMAT "X(256)":U INITIAL "FG INQUIRY" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnViewRM AS CHARACTER FORMAT "X(256)":U INITIAL "RM INQUIRY" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiCreated AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiCreatedLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CREATED:" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiCSR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiCSRLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CSR:" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiDue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiDueLabel AS CHARACTER FORMAT "X(256)":U INITIAL "DUE:" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     FONT 38 NO-UNDO.

DEFINE VARIABLE fiStatusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS:" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btFGItems AT ROW 7.33 COL 2 WIDGET-ID 118
     btMaterials AT ROW 7.33 COL 33.6 WIDGET-ID 122
     btRoutings AT ROW 7.33 COL 65.2 WIDGET-ID 120
     btExit AT ROW 1 COL 197 WIDGET-ID 126
     btnFirst AT ROW 10.67 COL 197 WIDGET-ID 44
     btnLast AT ROW 16.38 COL 197 WIDGET-ID 46
     btnNext AT ROW 14.48 COL 197 WIDGET-ID 42
     btnPrevious AT ROW 12.57 COL 197 WIDGET-ID 40
     fiStatusLabel AT ROW 5.52 COL 2 NO-LABEL WIDGET-ID 94
     fiStatus AT ROW 5.52 COL 15.6 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiCreatedLabel AT ROW 5.52 COL 46 NO-LABEL WIDGET-ID 112
     fiCreated AT ROW 5.52 COL 62.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     fiDueLabel AT ROW 5.52 COL 122 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiDue AT ROW 5.52 COL 131.6 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiCSRLabel AT ROW 5.52 COL 85 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiCSR AT ROW 5.52 COL 94.2 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     statusMessage AT ROW 30.76 COL 3 NO-LABEL WIDGET-ID 28
     btnPrintJobText AT ROW 30.76 COL 176 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btnAdjustQtyText AT ROW 30.76 COL 145 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     btnIssueQtyText AT ROW 30.76 COL 147 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     btnViewRM AT ROW 7.91 COL 98.2 COLON-ALIGNED NO-LABEL WIDGET-ID 138
     btnViewFG AT ROW 7.91 COL 98.2 COLON-ALIGNED NO-LABEL WIDGET-ID 140
     btClear AT ROW 3.14 COL 197 WIDGET-ID 146
     btnClearText AT ROW 3.33 COL 184.2 NO-LABEL WIDGET-ID 148
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 36.19
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


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
         TITLE              = "Job Inquiry"
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiCreated IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCreatedLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCSR IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCSRLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDue IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDueLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Inquiry */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Inquiry */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(hdJobProcs) THEN
  DELETE PROCEDURE hdJobProcs.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    {methods/run_link.i "JOB-SOURCE" "SetJob" "(?,?,?,?)"}
    RUN pJobScan.

    {methods/run_link.i "JOB-SOURCE" "Reset"}
    
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    
    RUN pStatusMessage ("", 0).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFGItems
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFGItems W-Win
ON CHOOSE OF btFGItems IN FRAME F-Main /* FG ITEMS */
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
ON CHOOSE OF btMaterials IN FRAME F-Main /* MATERIALS */
DO:
    RUN pHighlightButton (
        "Materials"
        ).            
        
    RUN select-page(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText IN FRAME F-Main
DO:
    RUN AdjustQuantity IN h_b-job-hdr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearText W-Win
ON MOUSE-SELECT-CLICK OF btnClearText IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btClear.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btnIssueQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnIssueQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnIssueQtyText IN FRAME F-Main
DO:
    RUN IssueQuantity IN h_b-job-mat.
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


&Scoped-define SELF-NAME btnPrintJobText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintJobText W-Win
ON MOUSE-SELECT-CLICK OF btnPrintJobText IN FRAME F-Main
DO:
    RUN pPrintJob IN h_printjob.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewFG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewFG W-Win
ON MOUSE-SELECT-CLICK OF btnViewFG IN FRAME F-Main
DO:
    RUN pChooseBtViewFGInquiry IN h_viewfginquiry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewRM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewRM W-Win
ON MOUSE-SELECT-CLICK OF btnViewRM IN FRAME F-Main
DO:
    RUN ChooseBtViewRMInquiry IN h_viewrminquiry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRoutings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRoutings W-Win
ON CHOOSE OF btRoutings IN FRAME F-Main /* ROUTINGS */
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

{sharpshooter/pStatusMessage.i}
{sharpshooter/ChangeWindowSize.i}

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
             INPUT  'sharpshooter/smartobj/adjustwindowsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustwindowsize ).
       RUN set-position IN h_adjustwindowsize ( 1.00 , 155.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       RUN set-position IN h_jobfilter ( 2.76 , 4.40 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 143.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/printjob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_printjob ).
       RUN set-position IN h_printjob ( 30.52 , 197.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_printjob. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PRINTJOB':U , h_printjob ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewfginquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewfginquiry ).
       RUN set-position IN h_viewfginquiry ( 7.71 , 121.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 31.71 , 170.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-job-hdr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-hdr ).
       RUN set-position IN h_b-job-hdr ( 10.33 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-hdr ( 20.24 , 195.00 ) NO-ERROR.

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'ADJUST':U , h_adjustqty ).

       /* Links to SmartBrowser h_b-job-hdr. */
       RUN add-link IN adm-broker-hdl ( h_viewfginquiry , 'FGInq':U , h_b-job-hdr ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'FGInq':U , h_b-job-hdr ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_1':U , h_b-job-hdr ).
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_adjustqty ,
             fiCSR:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-hdr ,
             h_adjustqty , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewrminquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewrminquiry ).
       RUN set-position IN h_viewrminquiry ( 7.71 , 121.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/issueqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_issueqty ).
       RUN set-position IN h_issueqty ( 31.24 , 169.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-job-mat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mat ).
       RUN set-position IN h_b-job-mat ( 10.29 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mat ( 20.48 , 195.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_issueqty. */
       RUN add-link IN adm-broker-hdl ( h_b-job-mat , 'ISSUE':U , h_issueqty ).

       /* Links to SmartBrowser h_b-job-mat. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( h_viewrminquiry , 'RMInq':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_2':U , h_b-job-mat ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'RMInq':U , h_b-job-mat ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_issueqty ,
             fiCSR:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-mat ,
             h_issueqty , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'inventory/b-job-mch.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mch ).
       RUN set-position IN h_b-job-mch ( 10.33 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mch ( 19.05 , 195.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-job-mch. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , h_b-job-mch ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PAGE_3':U , h_b-job-mch ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-job-mch ,
             fiCSR:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY fiStatusLabel fiStatus fiCreatedLabel fiCreated fiDueLabel fiDue 
          fiCSRLabel fiCSR btnExitText statusMessage btnPrintJobText 
          btnAdjustQtyText btnIssueQtyText btnViewRM btnViewFG btnClearText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btFGItems btMaterials btRoutings btExit btnFirst btnLast btnNext 
         btnPrevious btnExitText btnPrintJobText btnAdjustQtyText 
         btnIssueQtyText btnViewRM btnViewFG btClear btnClearText 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.
  DEFINE VARIABLE dCol             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dColTmp          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dRow             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dHeight          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dWidth           AS DECIMAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  DO WITH FRAME {&FRAME-NAME}:
      CASE adm-current-page:
          WHEN 1 THEN
          ASSIGN
              btnAdjustQtyText:HIDDEN = NO
              btnIssueQtyText:HIDDEN  = YES
              btnViewRM:VISIBLE       = FALSE
              btnViewFG:VISIBLE       = TRUE
              .
          WHEN 2 THEN DO:
            RUN get-position IN h_b-job-mat ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
            RUN get-size IN h_b-job-mat ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
            ASSIGN
                dCol    = {&WINDOW-NAME}:WIDTH  - 8
                dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                dWidth  = dCol - 3
                .
            RUN set-size IN h_b-job-mat ( dHeight , dWidth ) NO-ERROR.
            ASSIGN
                dRow = {&WINDOW-NAME}:HEIGHT - 1
                btnAdjustQtyText:HIDDEN = YES
                btnIssueQtyText:HIDDEN  = NO
                btnViewRM:VISIBLE       = TRUE
                btnViewFG:VISIBLE       = FALSE
                .
            RUN set-position IN h_issueqty ( dRow , btnIssueQtyText:COL + btnIssueQtyText:WIDTH ) NO-ERROR.
          END.
          WHEN 3 THEN DO:
            ASSIGN
                btnAdjustQtyText:HIDDEN = YES
                btnIssueQtyText:HIDDEN  = YES
                btnViewRM:VISIBLE       = FALSE
                btnViewFG:VISIBLE       = FALSE
                .
            RUN get-position IN h_b-job-mch ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
            RUN get-size IN h_b-job-mch ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
            ASSIGN
                dCol    = {&WINDOW-NAME}:WIDTH  - 8
                dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                dWidth  = dCol - 3
                .
            RUN set-size IN h_b-job-mch ( dHeight , dWidth ) NO-ERROR.
          END.
      END CASE.
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hdJobProcs) THEN
  DELETE PROCEDURE hdJobProcs.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pWinReSize.

    RUN spGetSessionParam("Company", OUTPUT cCompany).
    RUN spGetSessionParam("Location", OUTPUT cLocation).

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany NO-ERROR .
    IF AVAILABLE company THEN            
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                             + STRING(company.name) + " - " + cLocation.

    btnIssueQtyText:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    
    RUN pStatusMessage ("", 0).
    
    {methods/run_link.i "JOB-SOURCE" "HideJobDetails"}
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    {methods/run_link.i "JOB-SOURCE" "ValidateSameJobScan" "(FALSE)"}    
    {methods/run_link.i "JOB-SOURCE" "AllowEmptyFormAndBlank"}
    {methods/run_link.i "JOB-SOURCE" "DisableErrorAlerts"}
    
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
    
    ASSIGN
        btFGItems:FONT   = 17
        btMaterials:FONT = 17
        btRoutings:FONT  = 17
        .
        
    CASE ipcButtonType:
        WHEN "FGItems" THEN
            btFGItems:FONT = 38.
        WHEN "Materials" THEN
            btMaterials:FONT = 38.
        WHEN "Routings" THEN
            btRoutings:FONT = 38.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobError W-Win 
PROCEDURE pJobError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
    
    {methods/run_link.i "JOB-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
    
    RUN pStatusMessage (cStatusMessage, iStatusMessageType).
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
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.
    
    {methods/run_link.i "JOB-SOURCE" "GetJob" "(OUTPUT cJobNo, OUTPUT iJobNo2, OUTPUT iFormNo, OUTPUT iBlankNo)"}

    RUN pUpdateBrowse (
        INPUT cCompany,
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo,
        INPUT iBlankNo
        ) NO-ERROR.
        
    APPLY "CHOOSE" TO btFGItems IN FRAME {&FRAME-NAME}.
    
    IF AVAILABLE job THEN
        ASSIGN
            fiStatus:SCREEN-VALUE  = IF job.stat EQ "C" OR job.stat EQ "Z" THEN
                                         "Closed"
                                     ELSE IF job.stat EQ "H" THEN
                                         "Hold"
                                     ELSE
                                         "Open"
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
    DEFINE INPUT PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO. 

    DO WITH FRAME {&FRAME-NAME}:
    END.
   
    {methods\run_link.i "Record-SOURCE" "OpenQuery" "(ipcCompany,ipcJobNo,ipiJobNo2,ipiFormNo,ipiBlankNo)"}

    RUN pStatusMessage ("", 0).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btExit:COL                         = dCol
            btnFirst:COL                       = dCol - 1
            btnPrevious:COL                    = dCol - 1
            btnNext:COL                        = dCol - 1
            btnLast:COL                        = dCol - 1
            btnExitText:COL                    = dCol - 9
            btnClearText:COL                   = dCol - 13
            btClear:COL                        = dCol - 1           
            btnPrintJobText:COL                = dCol - btnPrintJobText:WIDTH - 1
            btnPrintJobText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            btnIssueQtyText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            .
        dRow = {&WINDOW-NAME}:HEIGHT - 1.
        RUN set-position IN h_printjob ( dRow , dCol ) NO-ERROR.
        RUN set-position IN h_adjustqty ( dRow , btnAdjustQtyText:COL + btnAdjustQtyText:WIDTH ) NO-ERROR.
        RUN set-position IN h_issueqty ( dRow , btnIssueQtyText:COL + btnIssueQtyText:WIDTH ) NO-ERROR.
        RUN get-position IN h_b-job-hdr ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN get-size IN h_b-job-hdr ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        ASSIGN
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            dWidth  = dCol - 3
            .
        RUN set-size IN h_b-job-hdr ( dHeight , dWidth ) NO-ERROR.
        RUN set-position IN h_adjustwindowsize ( 1.00 , dCol - 45 ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanJob W-Win 
PROCEDURE ScanJob :
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

    SESSION:SET-WAIT-STATE("GENERAL").   
    
    {methods/run_link.i "JOB-SOURCE" "SetJob" "(ipcJobNo,ipiJobNo2,?,?)"}
    
    {methods/run_link.i "JOB-SOURCE" "DisableAll"}
            
    SESSION:SET-WAIT-STATE("").            
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
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "job-valid" THEN
            RUN pJobScan.
        WHEN "job-error" THEN
            RUN pJobError.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

