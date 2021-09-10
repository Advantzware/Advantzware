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

  File: sharpshooter/w-relbol.w

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
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}
// {system/sysconst.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompanyName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSSBOLPrint  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iSSBOLPrint  AS INTEGER   NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

DEFINE VARIABLE glScanTrailer            AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE glScanTrailerWithRelease AS LOGICAL NO-UNDO INITIAL TRUE.

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
&Scoped-Define ENABLED-OBJECTS btReset fiTag btDelete btPrintBOL ~
btnExitText btnViewInquiryText btnDeleteText btnPrintBOLText 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiTrailer statusMessage btnExitText ~
btnViewInquiryText btnDeleteText btnPrintBOLText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-releaseitems AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-releasetags AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_releasefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewfginquiry AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChange 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Change" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Selected Tag".

DEFINE BUTTON btPrintBOL 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Print BOL" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btReset 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE btnDeleteText AS CHARACTER FORMAT "X(256)":U INITIAL "DELETE" 
      VIEW-AS TEXT 
     SIZE 15 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnPrintBOLText AS CHARACTER FORMAT "X(256)":U INITIAL "PRINT BOL" 
      VIEW-AS TEXT 
     SIZE 20 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnViewInquiryText AS CHARACTER FORMAT "X(256)":U INITIAL "VIEW INQUIRY" 
      VIEW-AS TEXT 
     SIZE 26 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "TAG" 
     VIEW-AS FILL-IN 
     SIZE 64.2 BY 1.38 TOOLTIP "Enter Tag #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTrailer AS CHARACTER FORMAT "X(256)":U 
     LABEL "TRAILER" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.38 TOOLTIP "Enter Trailer #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btReset AT ROW 2.91 COL 87 WIDGET-ID 18
     fiTag AT ROW 3.14 COL 20 COLON-ALIGNED WIDGET-ID 8
     fiTrailer AT ROW 3.14 COL 119 COLON-ALIGNED WIDGET-ID 10
     btDelete AT ROW 31.71 COL 17 WIDGET-ID 16 NO-TAB-STOP 
     btChange AT ROW 1 COL 59 WIDGET-ID 14 NO-TAB-STOP 
     btPrintBOL AT ROW 31.71 COL 187 WIDGET-ID 12 NO-TAB-STOP 
     statusMessage AT ROW 1.24 COL 66 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     btnViewInquiryText AT ROW 3.38 COL 162 NO-LABEL WIDGET-ID 26
     btnDeleteText AT ROW 31.95 COL 2 NO-LABEL WIDGET-ID 20
     btnPrintBOLText AT ROW 31.95 COL 165 COLON-ALIGNED NO-LABEL WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.81
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


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
         TITLE              = "Create BOL"
         HEIGHT             = 32.81
         WIDTH              = 202
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("Graphics/asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/asiicon.ico"
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
/* SETTINGS FOR BUTTON btChange IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btChange:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN btnDeleteText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnViewInquiryText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiTrailer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create BOL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create BOL */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pStatusMessage ("", 0).
    RUN pInvalidRelease.
    {methods/run_link.i "RELEASE-SOURCE" "EmptyRelease"}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    {methods/run_link.i "REL-TAGS-SOURCE" "DeleteReleaseTag"}    
    RUN pScanRelease.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteText W-Win
ON MOUSE-SELECT-CLICK OF btnDeleteText IN FRAME F-Main
DO:
  APPLY "CHOOSE":U TO btDelete.
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


&Scoped-define SELF-NAME btnPrintBOLText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintBOLText W-Win
ON MOUSE-SELECT-CLICK OF btnPrintBOLText IN FRAME F-Main
DO:
    APPLY "CHOOSE":U TO btPrintBOL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewInquiryText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewInquiryText W-Win
ON MOUSE-SELECT-CLICK OF btnViewInquiryText IN FRAME F-Main
DO:
    RUN pChooseBtViewFGInquiry IN h_viewfginquiry.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrintBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrintBOL W-Win
ON CHOOSE OF btPrintBOL IN FRAME F-Main /* Print BOL */
DO:
    RUN state-changed (THIS-PROCEDURE, "print-bol").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset W-Win
ON CHOOSE OF btReset IN FRAME F-Main /* Reset */
DO:
    fiTag:SCREEN-VALUE = "".    
    APPLY "ENTRY" TO fiTag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main /* TAG */
DO:
    SELF:SET-SELECTION (1, -1).      
    ASSIGN
        fiTrailer:BGCOLOR = 15
        SELF:BGCOLOR      = 30
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main /* TAG */
DO:
    IF LASTKEY EQ -1 OR SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
    
    IF SELF:SCREEN-VALUE EQ "EXIT" THEN DO:
        RUN state-changed (THIS-PROCEDURE, "tag-exit").        
        RETURN.
    END.
    
    IF SELF:SCREEN-VALUE EQ "PRINT" THEN DO:
        RUN state-changed (THIS-PROCEDURE, "print-bol").         
        SELF:SCREEN-VALUE = "".
        
        RETURN.
    END.
    
    IF glScanTrailer THEN DO:
        SELF:BGCOLOR = 15.
        RETURN.        
    END.
    
    RUN state-changed (THIS-PROCEDURE, "scan-tag").  
    
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTrailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailer W-Win
ON ENTRY OF fiTrailer IN FRAME F-Main /* TRAILER */
DO:
    SELF:SET-SELECTION (1, -1).      
    ASSIGN
        fiTag:BGCOLOR = 15
        SELF:BGCOLOR  = 30
        .     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailer W-Win
ON LEAVE OF fiTrailer IN FRAME F-Main /* TRAILER */
DO:
    RUN state-changed (THIS-PROCEDURE, "scan-tag").       
    SELF:BGCOLOR = 15.      
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
             INPUT  'sharpshooter/smartobj/releasefilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_releasefilter ).
       RUN set-position IN h_releasefilter ( 1.00 , 1.80 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 56.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewfginquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewfginquiry ).
       RUN set-position IN h_viewfginquiry ( 3.14 , 188.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releaseitems.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releaseitems ).
       RUN set-position IN h_b-releaseitems ( 5.05 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-releaseitems ( 10.24 , 193.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-2 ).
       RUN set-position IN h_navigatefirst-2 ( 6.95 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-2 ).
       RUN set-position IN h_navigateprev-2 ( 8.86 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-2 ).
       RUN set-position IN h_navigatenext-2 ( 10.76 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-2 ).
       RUN set-position IN h_navigatelast-2 ( 12.67 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releasetags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releasetags ).
       RUN set-position IN h_b-releasetags ( 15.52 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-releasetags ( 16.19 , 193.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 17.43 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 19.57 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 21.48 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 23.38 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_releasefilter. */
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'RELEASE':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-releaseitems. */
       RUN add-link IN adm-broker-hdl ( h_viewfginquiry , 'FGInq':U , h_b-releaseitems ).
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'REL-ITEMS':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-releasetags. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'REL-TAGS':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_releasefilter ,
             fiTag:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_releasefilter , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_viewfginquiry ,
             fiTrailer:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-releaseitems ,
             h_viewfginquiry , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst-2 ,
             h_b-releaseitems , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev-2 ,
             h_navigatefirst-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext-2 ,
             h_navigateprev-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast-2 ,
             h_navigatenext-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-releasetags ,
             h_navigatelast-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             h_b-releasetags , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigateprev , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatenext , 'AFTER':U ).
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
  DISPLAY fiTag fiTrailer statusMessage btnExitText btnViewInquiryText 
          btnDeleteText btnPrintBOLText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btReset fiTag btDelete btPrintBOL btnExitText btnViewInquiryText 
         btnDeleteText btnPrintBOLText 
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
    RUN pWinReSize.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN spGetSessionParam ("Company", OUTPUT cCompany).
        RUN spGetSessionParam ("CompanyName", OUTPUT cCompanyName).
        RUN spGetSessionParam ("Location", OUTPUT cLocation).
        RUN pStatusMessage ("", 0).
    
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                             + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                             + cCompanyName + " - " + cLocation
                             .
                             
        fiTrailer:HIDDEN = NOT glScanTrailer.
    
        RUN sys/ref/nk1look.p (
            cCompany,
            "SSBOLPRINT",
            "L",
            NO,
            NO,
            "",
            "",
            OUTPUT cResult,
            OUTPUT lFound
            ).
        lSSBOLPrint = IF NOT lFound THEN ? ELSE LOGICAL(cResult).            
        RUN sys/ref/nk1look.p (
            cCompany,
            "SSBOLPRINT",
            "I",
            NO,
            NO,
            "",
            "",
            OUTPUT cResult,
            OUTPUT lFound
            ).
        
        iSSBOLPrint = INTEGER(cResult).
            
        RUN pInvalidRelease.
        
        /* If scan trailer is enabled */
        IF glScanTrailer THEN
            btReset:TAB-STOP = FALSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInValidRelease W-Win 
PROCEDURE pInValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    ASSIGN
        fiTag:SENSITIVE     = FALSE
        fiTrailer:SENSITIVE = FALSE
        btChange:HIDDEN     = TRUE
        btChange:SENSITIVE  = FALSE
        btReset:SENSITIVE   = FALSE
        fiTag:BGCOLOR       = 15
        fiTrailer:BGCOLOR   = 15
        .

    {methods/run_link.i "RELEASE-SOURCE" "EnableRelease"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintBOL W-Win 
PROCEDURE pPrintBOL PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBOLID             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLPrintValueList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLPrintFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPostBOL           AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE oBOLHeader AS bol.BOLHeader NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateBOLFromRelease" "(INPUT cCompany, INPUT iReleaseID, OUTPUT lSuccess, OUTPUT cMessage, OUTPUT iBOLID)"}

    IF NOT lSuccess THEN DO:
        RUN pStatusMessage (cMessage, 3).
/*        MESSAGE cMessage        */
/*        VIEW-AS ALERT-BOX ERROR.*/
        RETURN.
    END.    
    
    oBOLHeader = NEW bol.BOLHeader().
    
    oBOLHeader:SetContext(cCompany, iBOLID).
    
    IF NOT oBOLHeader:IsAvailable() THEN DO:
        RUN pStatusMessage ("INVALID BOL '" + STRING(iBOLID) + "'", 3).
/*        MESSAGE "Invalid BOL # '" + STRING(iBOLID) + "'"*/
/*            VIEW-AS ALERT-BOX ERROR.                    */        
        RETURN.        
    END.
    
    IF lSSBOLPrint NE ? THEN DO:
        IF lSSBOLPrint EQ YES THEN DO:
            SESSION:SET-WAIT-STATE ("GENERAL").

            ASSIGN
                cBOLPrintFieldList = 'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert,begin_date,end_date'
                cBOLPrintValueList = oBOLHeader:GetValue("CustomerID") + ',' + oBOLHeader:GetValue("CustomerID") + ',' 
                                   + oBOLHeader:GetValue("BOLID") + ',' + oBOLHeader:GetValue("BOLID")
                                   + ',,99999999,' 
                                   + oBOLHeader:GetValue("Printed") + ',' 
                                   + oBOLHeader:GetValue("Posted") + ',BOL' + ',' 
                                   + oBOLHeader:GetValue("BOLDate") + ',' 
                                   + oBOLHeader:GetValue("BOLDate")
                .
                
            RUN custom/setUserPrint.p (
                oBOLHeader:GetValue("Company"),
                'oe-boll_.',
                cBOLPrintFieldList,
                cBOLPrintValueList
                ).
      
            RUN listobjs/oe-boll_.w.

            SESSION:SET-WAIT-STATE ("").
        END.
        ELSE DO:
            SESSION:SET-WAIT-STATE ("GENERAL").
            
            lPostBOL = iSSBOLPrint EQ 1.
             
            RUN bol/printBol.p (
                oBOLHeader:GetValue("Company"),
                cLocation,
                oBOLHeader:GetValue("CustomerID"),
                oBOLHeader:GetValue("BOLID"),
                oBOLHeader:GetValue("Printed"),
                oBOLHeader:GetValue("Posted"),
                lPostBOL
                ).

            SESSION:SET-WAIT-STATE ("").
        END.
    END.

    RUN pInValidRelease.
    
    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}

    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanRelease W-Win 
PROCEDURE pScanRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReleaseID AS INTEGER NO-UNDO.    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}
    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    RUN pStatusMessage ("", 0).

    IF NOT VALID-OBJECT(oReleaseHeader) THEN DO:
        RUN pStatusMessage ("INVALID RELEASE", 3).
/*        MESSAGE "Invalid Release#"  */
/*            VIEW-AS ALERT-BOX ERROR.*/
        RETURN.
    END.

    IF NOT oReleaseHeader:IsAvailable() THEN DO:
        RUN pStatusMessage ("INVALID RELEASE", 3).
/*        MESSAGE "Invalid Release#"  */
/*            VIEW-AS ALERT-BOX ERROR.*/
        RETURN.
    END.    

    IF NOT LOGICAL(oReleaseHeader:GetValue("Printed")) THEN DO:
        RUN pStatusMessage ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS NOT PRINTED", 3).
/*        MESSAGE "Release# '" + oReleaseHeader:GetValue("ReleaseID") + "' is not printed"*/
/*            VIEW-AS ALERT-BOX ERROR.                                                    */
        RETURN.
    END.

    IF LOGICAL(oReleaseHeader:GetValue("Posted")) THEN DO:
        RUN pStatusMessage ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS ALREADY POSTED", 3).
/*        MESSAGE "Release# '" + oReleaseHeader:GetValue("ReleaseID") + "' is already posted"*/
/*            VIEW-AS ALERT-BOX ERROR.                                                       */
        RETURN.
    END.

    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-ITEMS-SOURCE" "BuildReleaseItems" "(INPUT cCompany, INPUT iReleaseID)"}
    {methods/run_link.i "REL-TAGS-SOURCE" "BuildReleaseTags" "(INPUT cCompany, INPUT iReleaseID)"}
    
    ASSIGN
        fiTag:SCREEN-VALUE     = ""
        fiTrailer:SCREEN-VALUE = ""
        .
    
    APPLY "ENTRY":U TO fiTag.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanTag W-Win 
PROCEDURE pScanTag PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateReleaseTag" "(INPUT cCompany, INPUT iReleaseID, INPUT fiTag:SCREEN-VALUE, INPUT fiTrailer:SCREEN-VALUE, OUTPUT lError, OUTPUT cMessage)"}

    IF lError THEN DO:
        RUN pStatusMessage (cMessage, 3).
/*        MESSAGE cMessage        */
/*        VIEW-AS ALERT-BOX ERROR.*/
        
        ASSIGN
            fiTag:SCREEN-VALUE     = ""
            fiTrailer:SCREEN-VALUE = ""
            .

        APPLY "ENTRY" TO fiTag.

        RETURN.
    END.    
    
    RUN pScanRelease.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStatusMessage W-Win
PROCEDURE pStatusMessage:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStatusMessage AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiStatusMessage AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        statusMessage:SCREEN-VALUE = " " + CAPS(ipcStatusMessage).
        CASE ipiStatusMessage:
            WHEN 0 THEN
            ASSIGN
                statusMessage:BGCOLOR = ?
                statusMessage:FGCOLOR = ?
                .
            WHEN 1 THEN
            ASSIGN
                statusMessage:BGCOLOR = 10
                statusMessage:FGCOLOR = 0
                .
            WHEN 2 THEN
            ASSIGN
                statusMessage:BGCOLOR = 11
                statusMessage:FGCOLOR = 0
                .
            WHEN 3 THEN
            ASSIGN
                statusMessage:BGCOLOR = 12
                statusMessage:FGCOLOR = 15
                .
        END CASE.
    END. /* with frame */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidRelease W-Win 
PROCEDURE pValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    ASSIGN
        fiTag:SENSITIVE     = TRUE
        fiTrailer:SENSITIVE = TRUE
        btChange:HIDDEN     = FALSE
        btChange:SENSITIVE  = TRUE
        btReset:SENSITIVE   = TRUE
        .

    {methods/run_link.i "RELEASE-SOURCE" "DisableRelease"}

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
            btPrintBOL:ROW                     = {&WINDOW-NAME}:HEIGHT - 1.1
            btPrintBOL:COL                     = {&WINDOW-NAME}:WIDTH  - btPrintBOL:WIDTH - 1
            btnPrintBOLText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnPrintBOLText:COL                = btPrintBOL:COL - btnPrintBOLText:WIDTH - 1
            btDelete:ROW                       = {&WINDOW-NAME}:HEIGHT - 1.1
            btnDeleteText:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                    = dCol - 9
            .
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN get-position IN h_navigatefirst-2 ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast-2 ( dRow , dCol ) NO-ERROR.
        RUN get-size IN h_b-releaseitems ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        dWidth = dCol - 2.
        RUN set-size IN h_b-releaseitems ( dHeight , dWidth ) NO-ERROR.
        ASSIGN
            dRow    = dHeight + 5.25
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            .
        RUN set-size IN h_b-releasetags ( 6 , dWidth ) NO-ERROR.
        RUN set-position IN h_b-releasetags ( dRow , dCol ) NO-ERROR.
        RUN set-size IN h_b-releasetags ( dHeight , dWidth ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Exit W-Win 
PROCEDURE Select_Exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN dispatch ("exit").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "RELEASE-SOURCE" "Set-Focus"}
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
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE cStatusMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessage AS INTEGER   NO-UNDO.

    IF p-state BEGINS "Status-Message" THEN
    ASSIGN
        iStatusMessage = INTEGER(ENTRY(3,p-state,"|"))
        cStatusMessage = ENTRY(2,p-state,"|")
        p-state        = ENTRY(1,p-state,"|")
        .
    CASE p-state:
        WHEN "Status-Message" THEN
        RUN pStatusMessage (cStatusMessage, iStatusMessage).
        WHEN "release-invalid" THEN DO:
            RUN pInValidRelease.
        END.
        WHEN "release-valid" THEN DO:
            RUN pValidRelease.            
            RUN pScanRelease.
        END.
        WHEN "scan-tag" THEN DO:
            RUN pScanTag.
        END.
        WHEN "print-bol" THEN DO:
            RUN pPrintBOL.
        END.
        WHEN "release-exit" OR WHEN "tag-exit" THEN DO:
            RUN Select_Exit.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

