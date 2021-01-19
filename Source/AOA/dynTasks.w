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

  File: dynTasks.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Paraeters: <none>

  Output Parameters: <none>

  History: Ron Stark
  Created: 1.14.2019
          
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

&Scoped-define defaultUser _default
&Scoped-define program-id dynTasks.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE cMnemonic          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFormat      AS CHARACTER NO-UNDO INITIAL
    "Grid,LocalCSV,CSV,XLS,DOCX,PDF,HTML,Print -d,View".
DEFINE VARIABLE cOutputImage       AS CHARACTER NO-UNDO INITIAL
    "table.png,CSV.png,spreadsheet_sum.png,XLS.png,DOCX.png,PDF.png,html_tag.png,print_new.png,table.png".
DEFINE VARIABLE cUserID            AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO.

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).

iUserSecurityLevel = DYNAMIC-FUNCTION("sfUserSecurityLevel").

{methods/lockWindowUpdate.i}

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
&Scoped-Define ENABLED-OBJECTS btnTaskerHTML RECT-1 RECT-OPTIONS RECT-2 ~
RECT-3 btnExit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fWindowTitle W-Win 
FUNCTION fWindowTitle RETURNS CHARACTER
  (ipcTitle    AS CHARACTER,
   ipcMnemonic AS CHARACTER,
   ipcPrgmName AS CHARACTER,
   ipcUserID   AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_usercols AS HANDLE NO-UNDO.
DEFINE VARIABLE h_userparam AS HANDLE NO-UNDO.
DEFINE VARIABLE h_usertasks AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "Graphics/32x32/spreadsheet_sum.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/spreadsheet_sum_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDeleteTask 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete Task" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Task".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "Graphics/32x32/docx.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/docx_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Word DOCX".

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btnFavorite 
     IMAGE-UP FILE "Graphics/32x32/star_black.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/star_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Favorite" 
     SIZE 8 BY 1.91 TOOLTIP "Favorite".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "Graphics/32x32/html_tag.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/html_tag_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "HTML".

DEFINE BUTTON btnLocalCSV 
     IMAGE-UP FILE "Graphics/32x32/csv.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/csv_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 8 BY 1.91 TOOLTIP "Local Excel CSV".

DEFINE BUTTON btnPageFormat 
     IMAGE-UP FILE "Graphics/32x32/document_gear.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/document_gear_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Page Format".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "Graphics/32x32/pdf.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pdf_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "PDF".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/printer_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Printer".

DEFINE BUTTON btnRunResults 
     IMAGE-UP FILE "Graphics/32x32/table.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/table_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Results Grid" 
     SIZE 8 BY 1.91 TOOLTIP "Results Grid".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/floppy_disk_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE BUTTON btnSaveAs 
     IMAGE-UP FILE "Graphics/32x32/floppy_disks.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/floppy_disks_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Save As" 
     SIZE 8 BY 1.91 TOOLTIP "Save As".

DEFINE BUTTON btnScheduleTask 
     IMAGE-UP FILE "Graphics/32x32/calendar_clock.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/calendar_clock_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Schedule Task" 
     SIZE 8 BY 1.91 TOOLTIP "Schedule Task".

DEFINE BUTTON btnSubjctAttr 
     IMAGE-UP FILE "Graphics/32x32/udf.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/udf_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Subject Attributes" 
     SIZE 8 BY 1.91 TOOLTIP "Set External Form".

DEFINE BUTTON btnTaskerHTML 
     IMAGE-UP FILE "Graphics/32x32/folder.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Tasker HTML Page".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/32x32/jss_icon.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/jss_icon_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "Graphics/32x32/xls.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/xls_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Excel XLS".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 2.1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 2.1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 2.1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE RECTANGLE RECT-OPTIONS
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 2.1
     BGCOLOR 15 FGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnTaskerHTML AT ROW 1.24 COL 53 HELP
          "Tasker HTML Page" WIDGET-ID 680
     btnCSV AT ROW 1.24 COL 93 HELP
          "Excel CSV" WIDGET-ID 140
     btnDOCX AT ROW 1.24 COL 109 HELP
          "Word DOCX" WIDGET-ID 142
     btnHTML AT ROW 1.24 COL 125 HELP
          "HTML" WIDGET-ID 144
     btnLocalCSV AT ROW 1.24 COL 85 HELP
          "Local Excel CSV" WIDGET-ID 656
     btnPDF AT ROW 1.24 COL 117 HELP
          "PDF" WIDGET-ID 146
     btnPrint AT ROW 1.24 COL 133 HELP
          "Printer" WIDGET-ID 644
     btnRunResults AT ROW 1.24 COL 77 HELP
          "Results Grid" WIDGET-ID 254
     btnView AT ROW 1.24 COL 141 HELP
          "Jasper Viewer" WIDGET-ID 148
     btnXLS AT ROW 1.24 COL 101 HELP
          "Excel XLS" WIDGET-ID 150
     btnExit AT ROW 1.24 COL 152 HELP
          "Exit" WIDGET-ID 288
     btnPageFormat AT ROW 1.24 COL 65 HELP
          "Page Format" WIDGET-ID 652
     btnSave AT ROW 1.24 COL 2 HELP
          "Save" WIDGET-ID 248
     btnSaveAs AT ROW 1.24 COL 10 HELP
          "Save As" WIDGET-ID 674
     btnScheduleTask AT ROW 1.24 COL 18 HELP
          "Schedule Task" WIDGET-ID 252
     btnFavorite AT ROW 1.24 COL 42 HELP
          "Favorite" WIDGET-ID 672
     btnDeleteTask AT ROW 1.24 COL 34 HELP
          "Delete Task" WIDGET-ID 260
     btnSubjctAttr AT ROW 1.24 COL 26 HELP
          "Set Subject Attributes" WIDGET-ID 286
     RECT-1 AT ROW 1.24 COL 51
     RECT-OPTIONS AT ROW 1.24 COL 151
     RECT-2 AT ROW 1.24 COL 63 WIDGET-ID 676
     RECT-3 AT ROW 1.24 COL 75 WIDGET-ID 678
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.62
         BGCOLOR 21 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic User Tasks"
         HEIGHT             = 28.62
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR BUTTON btnCSV IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnCSV:PRIVATE-DATA IN FRAME F-Main     = 
                "CSV".

/* SETTINGS FOR BUTTON btnDeleteTask IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDOCX IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnDOCX:PRIVATE-DATA IN FRAME F-Main     = 
                "DOCX".

/* SETTINGS FOR BUTTON btnFavorite IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnHTML IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnHTML:PRIVATE-DATA IN FRAME F-Main     = 
                "HTML".

/* SETTINGS FOR BUTTON btnLocalCSV IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLocalCSV:PRIVATE-DATA IN FRAME F-Main     = 
                "LocalCSV".

/* SETTINGS FOR BUTTON btnPageFormat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnPDF IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnPDF:PRIVATE-DATA IN FRAME F-Main     = 
                "PDF".

/* SETTINGS FOR BUTTON btnPrint IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnPrint:PRIVATE-DATA IN FRAME F-Main     = 
                "Print -d".

/* SETTINGS FOR BUTTON btnRunResults IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnRunResults:PRIVATE-DATA IN FRAME F-Main     = 
                "Grid".

/* SETTINGS FOR BUTTON btnSave IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSaveAs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnScheduleTask IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSubjctAttr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnView IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnView:PRIVATE-DATA IN FRAME F-Main     = 
                "View".

/* SETTINGS FOR BUTTON btnXLS IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnXLS:PRIVATE-DATA IN FRAME F-Main     = 
                "XLS".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Dynamic User Tasks */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Dynamic User Tasks */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN pSaveSettings IN h_usertasks (USERID("ASI")).
  RUN pSaveSettings (USERID("ASI")).
  RUN pDeleteProcedure.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-RESIZED OF W-Win /* Dynamic User Tasks */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV W-Win
ON CHOOSE OF btnCSV IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTask W-Win
ON CHOOSE OF btnDeleteTask IN FRAME F-Main /* Delete Task */
DO:
    RUN pDeleteTask IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX W-Win
ON CHOOSE OF btnDOCX IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit W-Win
ON CHOOSE OF btnExit IN FRAME F-Main /* Exit */
DO:
    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFavorite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFavorite W-Win
ON CHOOSE OF btnFavorite IN FRAME F-Main /* Favorite */
DO:
    RUN pSetFavorite IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML W-Win
ON CHOOSE OF btnHTML IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLocalCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLocalCSV W-Win
ON CHOOSE OF btnLocalCSV IN FRAME F-Main /* csv */
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPageFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageFormat W-Win
ON CHOOSE OF btnPageFormat IN FRAME F-Main
DO:
    RUN pPageFormat IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF W-Win
ON CHOOSE OF btnPDF IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint W-Win
ON CHOOSE OF btnPrint IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunResults W-Win
ON CHOOSE OF btnRunResults IN FRAME F-Main /* Results Grid */
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave W-Win
ON CHOOSE OF btnSave IN FRAME F-Main /* Save */
DO:
    RUN pSave IN h_userparam.
    RUN pSave IN h_usercols.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveAs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveAs W-Win
ON CHOOSE OF btnSaveAs IN FRAME F-Main /* Save As */
DO:
    RUN select-page IN THIS-PROCEDURE ( 1 ).
    RUN pCopyTask IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScheduleTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScheduleTask W-Win
ON CHOOSE OF btnScheduleTask IN FRAME F-Main /* Schedule Task */
DO:
    RUN pScheduleTask IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubjctAttr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubjctAttr W-Win
ON CHOOSE OF btnSubjctAttr IN FRAME F-Main /* Subject Attributes */
DO:
    RUN pAttribute IN h_usertasks.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTaskerHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTaskerHTML W-Win
ON CHOOSE OF btnTaskerHTML IN FRAME F-Main
DO:
    RUN pTaskerHTML.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView W-Win
ON CHOOSE OF btnView IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS W-Win
ON CHOOSE OF btnXLS IN FRAME F-Main
DO:
    RUN pRunTask (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

&Scoped-define mFileItems ~
MENU-ITEM m_Save LABEL "Save" ~
MENU-ITEM m_SaveAs LABEL "Save As" ~
MENU-ITEM m_ScheduleTask LABEL "Schedule" ~
MENU-ITEM m_SubjctAttr LABEL "Attributes" ~
MENU-ITEM m_DeleteTask LABEL "Delete" ~
MENU-ITEM m_Favorite LABEL "Favorite" ~
RULE

&Scoped-define mOptionsItems ~
DEFINE SUB-MENU m_Options ~
    MENU-ITEM m_PageFormat LABEL "Page Format" ~
    RULE ~
    MENU-ITEM m_RunResults LABEL "Grid" ~
    MENU-ITEM m_LocalCSV LABEL "Local CSV" ~
    RULE ~
    MENU-ITEM m_CSV LABEL "Excel CSV" ~
    MENU-ITEM m_XLS LABEL "Excel XLS" ~
    MENU-ITEM m_DOCX LABEL "Word DOCX" ~
    MENU-ITEM m_PDF LABEL "PDF" ~
    MENU-ITEM m_HTML LABEL "HTML" ~
    MENU-ITEM m_Print LABEL "Printer" ~
    RULE ~
    MENU-ITEM m_View LABEL "Jasper Viewer" ~
    .

&Scoped-define mOptionsTriggers ~
ON CHOOSE OF MENU-ITEM m_Save ~
DO: ~
    APPLY "CHOOSE":U TO btnSave IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_SaveAs ~
DO: ~
    APPLY "CHOOSE":U TO btnSaveAs IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_ScheduleTask ~
DO: ~
    APPLY "CHOOSE":U TO btnScheduleTask IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_SubjctAttr ~
DO: ~
    APPLY "CHOOSE":U TO btnSubjctAttr IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_DeleteTask ~
DO: ~
    APPLY "CHOOSE":U TO btnDeleteTask IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_Favorite ~
DO: ~
    APPLY "CHOOSE":U TO btnFavorite IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_RunResults ~
DO: ~
    APPLY "CHOOSE":U TO btnRunResults IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_LocalCSV ~
DO: ~
    APPLY "CHOOSE":U TO btnLocalCSV IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_CSV ~
DO: ~
    APPLY "CHOOSE":U TO btnCSV IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_XLS ~
DO: ~
    APPLY "CHOOSE":U TO btnXLS IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_DOCX ~
DO: ~
    APPLY "CHOOSE":U TO btnDOCX IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_PDF ~
DO: ~
    APPLY "CHOOSE":U TO btnPDF IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_HTML ~
DO: ~
    APPLY "CHOOSE":U TO btnHTML IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_Print ~
DO: ~
    APPLY "CHOOSE":U TO btnPrint IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_PageFormat ~
DO: ~
    APPLY "CHOOSE":U TO btnPageFormat IN FRAME {&FRAME-NAME}. ~
END. ~
ON CHOOSE OF MENU-ITEM m_View ~
DO: ~
    APPLY "CHOOSE":U TO btnView IN FRAME {&FRAME-NAME}. ~
END.

{methods/menus/stdHelpMenu.i}

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
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Tasks|Parameters|Columns' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.52 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 26.00 , 160.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/usertasks.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_usertasks ).
       RUN set-position IN h_usertasks ( 4.91 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 24.67 , 160.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_usertasks ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/userparam.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_userparam ).
       RUN set-position IN h_userparam ( 4.91 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 24.71 , 158.00 ) */

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aoa/usercols.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_usercols ).
       RUN set-position IN h_usercols ( 4.91 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 24.52 , 158.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_usercols ,
             h_folder , 'AFTER':U ).
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
  ENABLE btnTaskerHTML RECT-1 RECT-OPTIONS RECT-2 RECT-3 btnExit 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
  RUN spGetTaskFilter (OUTPUT cMnemonic, OUTPUT cPrgmName, OUTPUT cUserID).
  {&WINDOW-NAME}:TITLE = fWindowTitle({&WINDOW-NAME}:TITLE, cMnemonic, cPrgmName, cUserID).
  RUN pGetSettings (USERID("ASI")).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallAudit W-Win 
PROCEDURE pCallAudit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pCallAudit IN h_usertasks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteProcedure W-Win 
PROCEDURE pDeleteProcedure :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(hAppSrvBin) THEN
   DELETE PROCEDURE hAppSrvBin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCompany W-Win 
PROCEDURE pGetCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    
    opcCompany = g_company.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGethAppSrvBin W-Win 
PROCEDURE pGethAppSrvBin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophAppSrvBin AS HANDLE NO-UNDO.
    
    OPhAppSrvBin = hAppSrvBin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValueRowID W-Win 
PROCEDURE pGetParamValueRowID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprRowID AS ROWID NO-UNDO.
    
    RUN pGetParamValueRowID IN h_userTasks (OUTPUT oprRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings W-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&program-id}"
                      AND user-print.user-id    EQ "{&defaultUser}") THEN
    RUN pSaveSettings ("{&defaultUser}").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunTask W-Win 
PROCEDURE pRunTask :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER hOutputOption AS HANDLE NO-UNDO.

    RUN pRunTask IN h_usertasks (hOutputOption:PRIVATE-DATA, NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings W-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .
    FIND CURRENT user-print NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetButtons W-Win 
PROCEDURE pSetButtons :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplAvailable    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplFavorite     AS LOGICAL   NO-UNDO.

    IF iplAvailable THEN DO:
        RUN enable-folder-page IN h_folder (2).
        RUN enable-folder-page IN h_folder (3).
    END. /* if */
    ELSE DO:
        RUN disable-folder-page IN h_folder (2).
        RUN disable-folder-page IN h_folder (3).
    END. /* else */
    DO WITH FRAME {&FRAME-NAME}:
        RUN pSetFavorite (iplFavorite).
        ASSIGN
            btnCSV:SENSITIVE          = iplAvailable
            btnDOCX:SENSITIVE         = iplAvailable
            btnHTML:SENSITIVE         = iplAvailable
            btnLocalCSV:SENSITIVE     = iplAvailable
            btnPageFormat:SENSITIVE   = iplAvailable
            btnPDF:SENSITIVE          = iplAvailable
            btnPrint:SENSITIVE        = iplAvailable
            btnRunResults:SENSITIVE   = iplAvailable
            btnSaveAs:SENSITIVE       = iplAvailable
            btnScheduleTask:SENSITIVE = iplAvailable
            btnView:SENSITIVE         = iplAvailable
            btnXLS:SENSITIVE          = iplAvailable
            iplAvailable              = iplAvailable AND ipcUserID EQ USERID("ASI")
            btnDeleteTask:SENSITIVE   = iplAvailable
            btnFavorite:SENSITIVE     = iplAvailable
            btnSave:SENSITIVE         = iplAvailable
            btnSubjctAttr:SENSITIVE   = iplAvailable
            MENU-ITEM m_SubjctAttr:SENSITIVE   IN MENU MENU-BAR-{&WINDOW-NAME} = btnSubjctAttr:SENSITIVE
            MENU-ITEM m_CSV:SENSITIVE          IN MENU MENU-BAR-{&WINDOW-NAME} = btnCSV:SENSITIVE
            MENU-ITEM m_DeleteTask:SENSITIVE   IN MENU MENU-BAR-{&WINDOW-NAME} = btnDeleteTask:SENSITIVE
            MENU-ITEM m_DOCX:SENSITIVE         IN MENU MENU-BAR-{&WINDOW-NAME} = btnDOCX:SENSITIVE
            MENU-ITEM m_Favorite:SENSITIVE     IN MENU MENU-BAR-{&WINDOW-NAME} = btnFavorite:SENSITIVE
            MENU-ITEM m_HTML:SENSITIVE         IN MENU MENU-BAR-{&WINDOW-NAME} = btnHTML:SENSITIVE
            MENU-ITEM m_LocalCSV:SENSITIVE     IN MENU MENU-BAR-{&WINDOW-NAME} = btnLocalCSV:SENSITIVE
            MENU-ITEM m_PageFormat:SENSITIVE   IN MENU MENU-BAR-{&WINDOW-NAME} = btnPAgeFormat:SENSITIVE
            MENU-ITEM m_PDF:SENSITIVE          IN MENU MENU-BAR-{&WINDOW-NAME} = btnPDF:SENSITIVE
            MENU-ITEM m_Print:SENSITIVE        IN MENU MENU-BAR-{&WINDOW-NAME} = btnPrint:SENSITIVE
            MENU-ITEM m_RunResults:SENSITIVE   IN MENU MENU-BAR-{&WINDOW-NAME} = btnRunResults:SENSITIVE
            MENU-ITEM m_Save:SENSITIVE         IN MENU MENU-BAR-{&WINDOW-NAME} = btnSave:SENSITIVE
            MENU-ITEM m_SaveAs:SENSITIVE       IN MENU MENU-BAR-{&WINDOW-NAME} = btnSaveAs:SENSITIVE
            MENU-ITEM m_ScheduleTask:SENSITIVE IN MENU MENU-BAR-{&WINDOW-NAME} = btnScheduleTask:SENSITIVE
            MENU-ITEM m_View:SENSITIVE         IN MENU MENU-BAR-{&WINDOW-NAME} = btnView:SENSITIVE
            MENU-ITEM m_XLS:SENSITIVE          IN MENU MENU-BAR-{&WINDOW-NAME} = btnXLS:SENSITIVE
            .
    END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFavorite W-Win 
PROCEDURE pSetFavorite :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplFavorite AS LOGICAL NO-UNDO.

    btnFavorite:LOAD-IMAGE("Graphics/32x32/"
        + IF iplFavorite THEN "star.png"
          ELSE "star_black.png") IN FRAME {&FRAME-NAME}
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTaskerHTML W-Win
PROCEDURE pTaskerHTML:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

    FIND FIRST config NO-LOCK.
    ASSIGN
        cFile = config.taskerHTMLFolder + "\tasker-" + USERID("ASI") + ".htm"
        cFile = IF SEARCH(cFile) NE ? THEN cFile ELSE REPLACE(cFile,USERID("ASI"),"ALL")
        FILE-INFO:FILE-NAME = SEARCH(cFile)
        cFile = FILE-INFO:FULL-PATHNAME
        .
    OS-COMMAND NO-WAIT start VALUE(cFile).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 33 THEN
        {&WINDOW-NAME}:HEIGHT = 33.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            dHeight                            = FRAME {&FRAME-NAME}:HEIGHT
            dWidth                             = FRAME {&FRAME-NAME}:WIDTH
            btnExit:COL                        = dWidth - btnExit:WIDTH
            RECT-OPTIONS:COL                   = btnExit:COL - 1
            .
        RUN set-size IN h_folder (dHeight, dWidth).
        ASSIGN
            dHeight = dHeight - 4.1
            dWidth  = dWidth  - 1.1
            .
        IF NOT VALID-HANDLE(h_userParam) THEN
        RUN init-pages ("2").
        RUN pWinReSize IN h_userParam (dHeight, dWidth).
        IF NOT VALID-HANDLE(h_userCols) THEN
        RUN init-pages ("3").
        RUN pWinReSize IN h_userCols (dHeight, dWidth).
        RUN pWinReSize IN h_userTasks (dHeight, dWidth).
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */
    RUN select-page (1).
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fWindowTitle W-Win 
FUNCTION fWindowTitle RETURNS CHARACTER
  (ipcTitle    AS CHARACTER,
   ipcMnemonic AS CHARACTER,
   ipcPrgmName AS CHARACTER,
   ipcUserID   AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTitle AS CHARACTER NO-UNDO.
    
    IF ipcMnemonic  NE "" OR
       ipcPrgmName  NE "" OR
       ipcUserID    NE "" THEN DO:
        cTitle = " - Filter [ ".
        IF ipcMnemonic NE "" THEN
        cTitle = cTitle + "Module: "     + ipcMnemonic + " ".
        IF ipcPrgmName NE "" THEN
        cTitle = cTitle + "Program ID: " + ipcPrgmName + " ".
        IF ipcUserID   NE "" THEN
        cTitle = cTitle + "User ID: "    + ipcUserID   + " ".
        cTitle = cTitle + "]".
    END.
    cTitle = ipcTitle + cTitle.
    RETURN cTitle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

