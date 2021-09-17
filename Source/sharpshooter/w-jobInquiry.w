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
// {system/sysconst.i}
{custom/globdefs.i}
{sys/inc/var.i}
{sys/inc/varasgn.i}
{methods/defines/hndlset.i}

DEFINE VARIABLE hdJobProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cJobNo2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFormattedJobno   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btFGInq btFGItems btMaterials btRoutings ~
btExit btnFirst btnLast btnNext btnPrevious fiJobNo cbJobNo2 btnExitText ~
btnPrintJobText btnAdjustQtyText btnIssueQtyText rSelected 
&Scoped-Define DISPLAYED-OBJECTS fiJobNo cbJobNo2 fiJoblabel fiStatusLabel ~
fiStatus fiCreatedLabel fiCreated fiDueLabel fiDue fiCSRLabel fiCSR ~
btnExitText statusMessage btnPrintJobText btnAdjustQtyText btnIssueQtyText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-hdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-job-mch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_issueqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_printjob AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btFGInq  NO-FOCUS
     LABEL "VIEW FG" 
     SIZE 19 BY 2.52
     FONT 37.

DEFINE BUTTON btFGItems  NO-FOCUS
     LABEL "FG ITEMS" 
     SIZE 30 BY 2.52
     FONT 37.

DEFINE BUTTON btMaterials  NO-FOCUS
     LABEL "MATERIALS" 
     SIZE 30 BY 2.52
     FONT 37.

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

DEFINE BUTTON btRMInq  NO-FOCUS
     LABEL "VIEW RM" 
     SIZE 19 BY 2.52
     FONT 37.

DEFINE BUTTON btRoutings  NO-FOCUS
     LABEL "ROUTINGS" 
     SIZE 30 BY 2.52
     FONT 37.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
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

DEFINE VARIABLE fiCreated AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.19
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiCreatedLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CREATED:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.19 NO-UNDO.

DEFINE VARIABLE fiCSR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.6 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiCSRLabel AS CHARACTER FORMAT "X(256)":U INITIAL "CSR:" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiDue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1.19
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiDueLabel AS CHARACTER FORMAT "X(256)":U INITIAL "DUE:" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE fiJoblabel AS CHARACTER FORMAT "X(256)":U INITIAL "JOB:" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 37 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiStatusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS:" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE rSelected
     EDGE-PIXELS 0    
     SIZE 33 BY 3.29
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btFGInq AT ROW 5.76 COL 118 WIDGET-ID 130
     btFGItems AT ROW 5.67 COL 3.6 WIDGET-ID 118
     btMaterials AT ROW 5.67 COL 35.2 WIDGET-ID 122
     btRMInq AT ROW 5.76 COL 138 WIDGET-ID 132
     btRoutings AT ROW 5.67 COL 66.8 WIDGET-ID 120
     btExit AT ROW 1 COL 197 WIDGET-ID 126
     btnFirst AT ROW 10.29 COL 197 WIDGET-ID 44
     btnLast AT ROW 16 COL 197 WIDGET-ID 46
     btnNext AT ROW 14.1 COL 197 WIDGET-ID 42
     btnPrevious AT ROW 12.19 COL 197 WIDGET-ID 40
     fiJobNo AT ROW 2.19 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbJobNo2 AT ROW 2.24 COL 59.6 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiJoblabel AT ROW 2.19 COL 8 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     fiStatusLabel AT ROW 2.19 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     fiStatus AT ROW 2.19 COL 127.6 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiCreatedLabel AT ROW 3.86 COL 2 NO-LABEL WIDGET-ID 112
     fiCreated AT ROW 3.86 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     fiDueLabel AT ROW 3.86 COL 40.8 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiDue AT ROW 3.86 COL 50 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiCSRLabel AT ROW 3.86 COL 118.4 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiCSR AT ROW 3.86 COL 127.4 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     statusMessage AT ROW 31.95 COL 3 NO-LABEL WIDGET-ID 28
     btnPrintJobText AT ROW 31.95 COL 176 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btnAdjustQtyText AT ROW 31.95 COL 145 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     btnIssueQtyText AT ROW 31.95 COL 147 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     "-" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 2.67 COL 60 WIDGET-ID 86
          FONT 37
     rSelected AT ROW 5.29 COL 2 WIDGET-ID 124
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
   Design Page: 1
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
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR BUTTON btRMInq IN FRAME F-Main
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN fiJoblabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusLabel IN FRAME F-Main
   NO-ENABLE                                                            */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btFGInq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFGInq W-Win
ON CHOOSE OF btFGInq IN FRAME F-Main /* VIEW FG */
DO:
    {methods/run_link.i "FGInq-TARGET" "ViewFGInquiry"}      
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
    
    ASSIGN
        btRMInq:SENSITIVE = FALSE
        btFGInq:SENSITIVE = TRUE
        .
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
    
    ASSIGN
        btRMInq:SENSITIVE = TRUE
        btFGInq:SENSITIVE = FALSE
        .
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
    RUN AdjustQuantity IN h_issueqty.
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


&Scoped-define SELF-NAME btRMInq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRMInq W-Win
ON CHOOSE OF btRMInq IN FRAME F-Main /* VIEW RM */
DO:
    {methods/run_link.i "RMInq-TARGET" "ViewRMInquiry"}      
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

    ASSIGN
        btRMInq:SENSITIVE = FALSE
        btFGInq:SENSITIVE = FALSE
        .    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbJobNo2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbJobNo2 W-Win
ON VALUE-CHANGED OF cbJobNo2 IN FRAME F-Main
DO:
    RUN pJobScan (
        INPUT  cocode,
        INPUT  cFormattedJobno,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  0,
        INPUT  0
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON LEAVE OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    
    RUN pStatusMessage ("", 0).
    RUN JobParser IN hdJobProcs (
        SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).

    IF NOT lParse THEN
        cJobNo = SELF:SCREEN-VALUE.

    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                      ).
    
    cJobno2ListItems = "".
    
    RUN GetSecondaryJobForJob IN hdJobProcs (
        INPUT        cCompany,
        INPUT        cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).

    ASSIGN
        cbJobNo2:LIST-ITEMS   = cJobno2ListItems.
        cbJobNo2:SCREEN-VALUE = IF cJobNo2 EQ "" THEN 
                                    ENTRY(1,cJobno2ListItems)
                                ELSE
                                    STRING(INTEGER(cJobNo2),"99")
        NO-ERROR.       
    
    RUN pJobScan (
        INPUT  cocode,
        INPUT  cFormattedJobno,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  0,
        INPUT  0
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}

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
             INPUT  'sharpshooter/smartobj/printjob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_printjob ).
       RUN set-position IN h_printjob ( 31.71 , 197.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_printjob. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'PRINTJOB':U , h_printjob ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_printjob ,
             fiJobNo:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
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
       RUN set-position IN h_b-job-hdr ( 8.86 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-hdr ( 21.91 , 195.00 ) NO-ERROR.

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'ADJUST':U , h_adjustqty ).

       /* Links to SmartBrowser h_b-job-hdr. */
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
             INPUT  'sharpshooter/smartobj/issueqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_issueqty ).
       RUN set-position IN h_issueqty ( 31.71 , 169.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-job-mat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-job-mat ).
       RUN set-position IN h_b-job-mat ( 8.86 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mat ( 21.91 , 195.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_issueqty. */
       RUN add-link IN adm-broker-hdl ( h_b-job-mat , 'ISSUE':U , h_issueqty ).

       /* Links to SmartBrowser h_b-job-mat. */
       RUN add-link IN adm-broker-hdl ( h_b-job-hdr , 'Record':U , h_b-job-mat ).
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
       RUN set-position IN h_b-job-mch ( 8.86 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-job-mch ( 21.91 , 195.00 ) NO-ERROR.

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
  DISPLAY fiJobNo cbJobNo2 fiJoblabel fiStatusLabel fiStatus fiCreatedLabel 
          fiCreated fiDueLabel fiDue fiCSRLabel fiCSR btnExitText statusMessage 
          btnPrintJobText btnAdjustQtyText btnIssueQtyText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btFGInq btFGItems btMaterials btRoutings btExit btnFirst btnLast 
         btnNext btnPrevious fiJobNo cbJobNo2 btnExitText btnPrintJobText 
         btnAdjustQtyText btnIssueQtyText rSelected 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetJob W-Win 
PROCEDURE GetJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcJobNo  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2 AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        opcJobNo  = cFormattedJobno
        opiJobNo2 = INTEGER(cbJobNo2:SCREEN-VALUE)
        .

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
                dRow = {&WINDOW-NAME}:HEIGHT - 1.62
                btnAdjustQtyText:HIDDEN = YES
                btnIssueQtyText:HIDDEN  = NO
                .
            RUN set-position IN h_issueqty ( dRow , btnIssueQtyText:COL + btnIssueQtyText:WIDTH ) NO-ERROR.
          END.
          WHEN 3 THEN DO:
            ASSIGN
                btnAdjustQtyText:HIDDEN = YES
                btnIssueQtyText:HIDDEN  = YES
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pWinReSize.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ g_company NO-ERROR .
    IF AVAILABLE company THEN            
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                             + STRING(company.name) + " - " + g_loc.
    ASSIGN
        cCompany = cocode
        btnIssueQtyText:HIDDEN IN FRAME {&FRAME-NAME} = YES
        .
    RUN pStatusMessage ("", 0).

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

    SESSION:SET-WAIT-STATE("GENERAL").   
    
    RUN pJobScan (
        ipcCompany,
        ipcJobNo,
        ipiJobNo2,
        ipiFormNo,
        ipiBlankNo
        ).
    
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

    IF NOT lValidJob THEN DO:
        RUN pStatusMessage ("INVALID JOB NUMBER, PLEASE ENTER A VALID JOB NUMBER", 3).
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
            cbJobNo2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
            NO-ERROR.
    END.
    
    RUN pUpdateBrowse (
        INPUT ipcCompany,
        INPUT ipcJobNo,
        INPUT ipiJobNo2
        ) NO-ERROR.

    APPLY "CHOOSE" TO btFGItems IN FRAME {&FRAME-NAME}.
    
    cCompany = ipcCompany.
    
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
        .
        
    RUN GetSecondaryJobForJob IN hdJobProcs (
        ipcCompany,
        fiJobNo:SCREEN-VALUE,
        INPUT-OUTPUT cJobno2ListItems
        ).    
    
    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobNo2ListItems      = "00"
            cbJobNo2:LIST-ITEMS   = cJobno2ListItems 
            cbJobNo2:SCREEN-VALUE = "00".
    ELSE
        cbJobNo2:LIST-ITEMS = cJobno2ListItems.

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
            {&WINDOW-NAME}:ROW                 = 1
            {&WINDOW-NAME}:COL                 = 1
            {&WINDOW-NAME}:VIRTUAL-HEIGHT      = SESSION:HEIGHT - 1
            {&WINDOW-NAME}:VIRTUAL-WIDTH       = SESSION:WIDTH  - 1
            {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            {&WINDOW-NAME}:WIDTH               = {&WINDOW-NAME}:VIRTUAL-WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btExit:COL                         = dCol - 1
            btnFirst:COL                       = dCol - 1
            btnPrevious:COL                    = dCol - 1
            btnNext:COL                        = dCol - 1
            btnLast:COL                        = dCol - 1
            btnExitText:COL                    = dCol - 9
            btnPrintJobText:COL                = dCol - btnPrintJobText:WIDTH - 1
            btnPrintJobText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            btnIssueQtyText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            .
        dRow = {&WINDOW-NAME}:HEIGHT - 1.62.
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
    END. /* do with */
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

