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

  File: sharpshooter/w-createLoadtag.w

  Description: Creates a load tag from different sources

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
{api/inbound/ttItem.i}
DEFINE TEMP-TABLE ttPOLine NO-UNDO
    LIKE po-ordl
    FIELD   quantity            AS INTEGER     
    FIELD   quantityInSubUnit   AS INTEGER
    FIELD   subUnitsPerUnit     AS INTEGER
    .
    
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser    AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.
 
DEFINE VARIABLE oJobHeader          AS jc.JobHeader       NO-UNDO.
DEFINE VARIABLE oItemFG             AS fg.ItemFG          NO-UNDO.
DEFINE VARIABLE oCustomer           AS Inventory.Customer NO-UNDO.
DEFINE VARIABLE oLoadTag            AS Inventory.Loadtag  NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

DEFINE VARIABLE glAutoCreateLoadtagOnJobScan AS LOGICAL NO-UNDO.
DEFINE VARIABLE glAutoPrintLoadtagOnJobScan  AS LOGICAL NO-UNDO.
DEFINE VARIABLE giDefaultPrintCopies         AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE giNK1PrintCopies             AS INTEGER NO-UNDO INITIAL 1.
    
oCustomer = NEW Inventory.Customer().

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
&Scoped-Define ENABLED-OBJECTS btJob btPO btRelease btReturn btDelete ~
btReprint btPrint btSplit btBOL RECT-1 RECT-34 rHighlight 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-loadtags-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-boltag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-poprint AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-releasetag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-reprint AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-return AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-splittag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_printcopies AS HANDLE NO-UNDO.
DEFINE VARIABLE h_qtyunits AS HANDLE NO-UNDO.
DEFINE VARIABLE h_userfields AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btBOL 
     LABEL "BOL" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btCreate 
     LABEL "Create" 
     SIZE 20 BY 2.24.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     LABEL "Delete" 
     SIZE 7.6 BY 1.81 TOOLTIP "Delete currently selected record".

DEFINE BUTTON btJob 
     LABEL "Job" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btPO 
     LABEL "PO" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btPrint 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U
     LABEL "Print" 
     SIZE 13 BY 10.33.

DEFINE BUTTON btRelease 
     LABEL "Release" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReprint 
     LABEL "Re-Print" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReturn 
     LABEL "Return" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btSplit 
     LABEL "Split" 
     SIZE 22.4 BY 1.43.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 208 BY 20.1
     BGCOLOR 1 FGCOLOR 1 .

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 208 BY 1.81
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE rHighlight
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24.4 BY 1.81
     BGCOLOR 15 FGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJob AT ROW 1.19 COL 5.4 WIDGET-ID 2 NO-TAB-STOP 
     btPO AT ROW 1.19 COL 29.8 WIDGET-ID 24 NO-TAB-STOP 
     btRelease AT ROW 1.19 COL 53.8 WIDGET-ID 48 NO-TAB-STOP 
     btCreate AT ROW 2.95 COL 188.4 WIDGET-ID 58
     btReturn AT ROW 1.19 COL 77.8 WIDGET-ID 50 NO-TAB-STOP 
     btDelete AT ROW 23.19 COL 185 WIDGET-ID 62
     btReprint AT ROW 1.19 COL 101.6 WIDGET-ID 52 NO-TAB-STOP 
     btPrint AT ROW 23.19 COL 194.4 WIDGET-ID 30
     btSplit AT ROW 1.19 COL 125.4 WIDGET-ID 54 NO-TAB-STOP 
     btBOL AT ROW 1.19 COL 149.2 WIDGET-ID 64 NO-TAB-STOP 
     RECT-1 AT ROW 2.81 COL 1 WIDGET-ID 4
     RECT-34 AT ROW 1 COL 1 WIDGET-ID 18
     rHighlight AT ROW 1 COL 4.4 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 208 BY 32.95
         BGCOLOR 15 FONT 17 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 2
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create LoadTag"
         HEIGHT             = 32.95
         WIDTH              = 208
         MAX-HEIGHT         = 47.38
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 47.38
         VIRTUAL-WIDTH      = 384
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR BUTTON btCreate IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create LoadTag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create LoadTag */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBOL W-Win
ON CHOOSE OF btBOL IN FRAME F-Main /* BOL */
DO:
    RUN select-page(7).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate W-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* Create */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "create-tags"
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    {methods/run_link.i "LOADTAG-SOURCE" "DeleteSelected"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJob W-Win
ON CHOOSE OF btJob IN FRAME F-Main /* Job */
DO:
    RUN select-page(1).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPO W-Win
ON CHOOSE OF btPO IN FRAME F-Main /* PO */
DO:
    RUN select-page(2).
    
    rHighlight:X = SELF:X - 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrint W-Win
ON CHOOSE OF btPrint IN FRAME F-Main /* Print */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "print-tags"
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRelease W-Win
ON CHOOSE OF btRelease IN FRAME F-Main /* Release */
DO:
    RUN select-page(3).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprint W-Win
ON CHOOSE OF btReprint IN FRAME F-Main /* Re-Print */
DO:
    RUN select-page(5).
    
    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReturn W-Win
ON CHOOSE OF btReturn IN FRAME F-Main /* Return */
DO:
    RUN select-page(4).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSplit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSplit W-Win
ON CHOOSE OF btSplit IN FRAME F-Main /* Split */
DO:
    RUN select-page(6).

    rHighlight:X = SELF:X - 5.    
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
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 201.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/b-loadtags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-loadtags-3 ).
       RUN set-position IN h_b-loadtags-3 ( 23.24 , 4.20 ) NO-ERROR.
       RUN set-size IN h_b-loadtags-3 ( 10.29 , 179.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-loadtags-3. */
       RUN add-link IN adm-broker-hdl ( h_b-loadtags-3 , 'LOADTAG':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-loadtags-3 ,
             btPrint:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/qtyunits.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_qtyunits ).
       RUN set-position IN h_qtyunits ( 10.95 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.29 , 131.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/printcopies.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_printcopies ).
       RUN set-position IN h_printcopies ( 7.14 , 98.40 ) NO-ERROR.
       /* Size in UIB:  ( 2.19 , 46.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       RUN set-position IN h_jobfilter ( 2.95 , 5.20 ) NO-ERROR.
       /* Size in UIB:  ( 3.52 , 76.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/fgfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_fgfilter ).
       RUN set-position IN h_fgfilter ( 6.91 , 5.20 ) NO-ERROR.
       /* Size in UIB:  ( 3.76 , 85.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/userfields.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_userfields ).
       RUN set-position IN h_userfields ( 11.05 , 138.60 ) NO-ERROR.
       /* Size in UIB:  ( 9.19 , 68.00 ) */

       /* Links to SmartObject h_printcopies. */
       RUN add-link IN adm-broker-hdl ( h_printcopies , 'COPIES':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_fgfilter. */
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'FGITEM':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_fgfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_userfields. */
       RUN add-link IN adm-broker-hdl ( h_userfields , 'State':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_userfields , 'USERFIELD':U , THIS-PROCEDURE ).

    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-poprint.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-poprint ).
       RUN set-position IN h_f-poprint ( 3.14 , 20.40 ) NO-ERROR.
       /* Size in UIB:  ( 19.52 , 151.60 ) */

       /* Links to SmartFrame h_f-poprint. */
       RUN add-link IN adm-broker-hdl ( h_f-poprint , 'PO':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-poprint , 'State':U , THIS-PROCEDURE ).

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-releasetag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-releasetag ).
       RUN set-position IN h_f-releasetag ( 3.14 , 71.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.86 , 60.40 ) */

       /* Links to SmartFrame h_f-releasetag. */
       RUN add-link IN adm-broker-hdl ( h_f-releasetag , 'RELEASE':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-releasetag , 'State':U , THIS-PROCEDURE ).

    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-return.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-return ).
       RUN set-position IN h_f-return ( 3.14 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.05 , 83.80 ) */

       /* Links to SmartFrame h_f-return. */
       RUN add-link IN adm-broker-hdl ( h_f-return , 'RETURN':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-return , 'State':U , THIS-PROCEDURE ).

    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-reprint.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-reprint ).
       RUN set-position IN h_f-reprint ( 3.00 , 63.60 ) NO-ERROR.
       /* Size in UIB:  ( 6.48 , 81.60 ) */

       /* Links to SmartFrame h_f-reprint. */
       RUN add-link IN adm-broker-hdl ( h_f-reprint , 'REPRINT':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-reprint , 'State':U , THIS-PROCEDURE ).

    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-splittag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-splittag ).
       RUN set-position IN h_f-splittag ( 3.14 , 71.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.95 , 81.00 ) */

       /* Links to SmartFrame h_f-splittag. */
       RUN add-link IN adm-broker-hdl ( h_f-splittag , 'SPLIT':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-splittag , 'State':U , THIS-PROCEDURE ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-boltag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-boltag ).
       RUN set-position IN h_f-boltag ( 3.14 , 73.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.86 , 61.40 ) */

       /* Links to SmartFrame h_f-boltag. */
       RUN add-link IN adm-broker-hdl ( h_f-boltag , 'BOL':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-boltag , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 7 */

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
  ENABLE btJob btPO btRelease btReturn btDelete btReprint btPrint btSplit btBOL 
         RECT-1 RECT-34 rHighlight 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetConfig W-Win 
PROCEDURE GetConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoConfig AS system.Config NO-UNDO.
    
    DEFINE VARIABLE iCurrentPage AS INTEGER NO-UNDO.
    
    RUN get-attribute IN THIS-PROCEDURE (
        'Current-Page':U
        ).
    
    iCurrentPage = INTEGER(RETURN-VALUE).
    
    CASE iCurrentPage:
        WHEN 1 THEN
            opoConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTag").
        
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDesignConfig W-Win 
PROCEDURE GetDesignConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoDesignConfig AS system.Config NO-UNDO.
    
    DEFINE VARIABLE iCurrentPage AS INTEGER NO-UNDO.
    
    RUN get-attribute IN THIS-PROCEDURE (
        'Current-Page':U
        ).
    
    iCurrentPage = INTEGER(RETURN-VALUE).
    
    CASE iCurrentPage:
        WHEN 1 THEN
            opoDesignConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTagJobDesign").
        
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLTagPrint W-Win 
PROCEDURE pBOLTagPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBOLID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCopies  AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE oBOLHeader AS bol.BOLHeader NO-UNDO.
    
    {methods/run_link.i "BOL-SOURCE" "GetBOLHeader" "(OUTPUT oBOLHeader)"}
    
    IF VALID-OBJECT(oBOLHeader) THEN DO:
        ASSIGN
            cCompany = oBOLHeader:GetValue("Company")
            iBOLID   = INTEGER(oBOLHeader:GetValue("BOLID"))
            .
    
        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromBOL" "(INPUT cCompany, INPUT iBOLID, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE oSSLoadTagConfig AS system.Config NO-UNDO.
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
    

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam("UserID", OUTPUT cUser).
    
    oSSLoadTagConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTag").

    IF VALID-OBJECT(oSSLoadTagConfig) THEN DO:
        IF oSSLoadTagConfig:IsAttributeAvailable("AutoCreateLoadtagOnJobScan", "Active") THEN
            ASSIGN
                glAutoCreateLoadtagOnJobScan = LOGICAL(oSSLoadTagConfig:GetAttributeValue("AutoCreateLoadtagOnJobScan", "Active"))
                btCreate:HIDDEN              = glAutoCreateLoadtagOnJobScan
                .
        
        IF oSSLoadTagConfig:IsAttributeAvailable("AutoPrintLoadtagOnJobScan", "Active") THEN
            ASSIGN
                glAutoPrintLoadtagOnJobScan  = LOGICAL(oSSLoadTagConfig:GetAttributeValue("AutoPrintLoadtagOnJobScan", "Active"))
                btDelete:HIDDEN              = glAutoPrintLoadtagOnJobScan
                .
        
        IF oSSLoadTagConfig:IsAttributeAvailable("DefaultPrintCopies", "Copies") THEN
            giDefaultPrintCopies = INTEGER(oSSLoadTagConfig:GetAttributeValue("DefaultPrintCopies", "Copies")).
    END.

    RUN sys/ref/nk1look.p (
        INPUT  cCompany,           /* Company Code */ 
        INPUT  "LOADTAG",          /* sys-ctrl name */
        INPUT  "I",                /* Output return value */
        INPUT  NO,                 /* Use ship-to */
        INPUT  NO,                 /* ship-to vendor */
        INPUT  "",                 /* ship-to vendor value */
        INPUT  "",                 /* shi-id value */
        OUTPUT cReturnValue, 
        OUTPUT lRecFound
        ).    
    IF lRecFound THEN 
        giNK1PrintCopies = INTEGER(cReturnValue).
    
    IF giNK1PrintCopies GT 0 THEN
        {methods/run_link.i "COPIES-SOURCE" "SetCopies" "(INPUT giNK1PrintCopies)"}.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPOTagPrint W-Win 
PROCEDURE pPOTagPrint PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPOID              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPOLine            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantity          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cQuantityUOM       AS CHARACTER NO-UNDO.
    
    {methods/run_link.i "PO-SOURCE" "GetPOLineTT" "(OUTPUT TABLE ttPOLine)"}
    
    FOR EACH ttPOLine:
        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromPO" "(INPUT ttPOLine.company, INPUT ttPOLine.po-no, INPUT ttPOLine.line, INPUT ttPOLine.quantity, INPUT ttPOLine.quantityInSubUnit, INPUT ttPOLine.subUnitsPerUnit, INPUT ttPOLine.pr-qty-uom, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReleaseTagPrint W-Win 
PROCEDURE pReleaseTagPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCopies    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetRelease" "(OUTPUT oReleaseHeader)"}

    IF VALID-OBJECT(oReleaseHeader) THEN DO:
        ASSIGN
            cCompany   = oReleaseHeader:GetValue("Company")
            iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID"))
            .

        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromRelease" "(INPUT cCompany, INPUT iReleaseID, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReprintTag W-Win 
PROCEDURE pReprintTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTag     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    
    {methods/run_link.i "REPRINT-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}

    IF VALID-OBJECT(oLoadtag) THEN DO:
        ASSIGN
            cCompany = oLoadTag:GetValue("Company")
            cTag     = oLoadTag:GetValue("Tag")
            .

        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}         
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReturnTag W-Win 
PROCEDURE pReturnTag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dReturnQuantity  AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lError           AS LOG.
    DEFINE VARIABLE cMessage         AS CHARACTER.
    DEFINE VARIABLE cTag             AS CHARACTER.
    
    {methods/run_link.i "RETURN-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}   

    IF VALID-OBJECT(oLoadTag) THEN 
    DO:        
        {methods/run_link.i "RETURN-SOURCE" "GetQuantity" "(OUTPUT dReturnQuantity)"}   
    
        ASSIGN
            cCompany = oLoadtag:GetValue("Company")
            cTag     = oLoadtag:GetValue("Tag")
            .   

        RUN inventory/inventoryprocs.p PERSISTENT SET hdInventoryProcs.
    
        RUN Inventory_CreateReturnFromTag IN hdInventoryProcs(
            INPUT  cCompany,
            INPUT  cTag,
            INPUT  dReturnQuantity,
            INPUT  TRUE,
            OUTPUT lError,
            OUTPUT cMessage
            ).

        DELETE PROCEDURE hdInventoryProcs.

        IF lError THEN
            MESSAGE cMessage
                VIEW-AS ALERT-BOX ERROR.
        ELSE
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSplitTag W-Win 
PROCEDURE pSplitTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTag              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSplitQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemType         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO. 

    {methods/run_link.i "SPLIT-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}   

    IF VALID-OBJECT(oLoadTag) THEN DO:        
        {methods/run_link.i "SPLIT-SOURCE" "GetQuantity" "(OUTPUT dSplitQuantity)"}   
    
        ASSIGN
            cCompany       = oLoadtag:GetValue("Company")
            cTag           = oLoadtag:GetValue("Tag")
            cItemID        = oLoadtag:GetValue("ItemID")
            cItemType      = STRING(LOGICAL(oLoadtag:GetValue("ItemType")), "RM/FG")
            .   

        RUN api\inbound\SplitTag.p (
            INPUT  cCompany, 
            INPUT  cTag,
            INPUT  cItemID,
            INPUT  dSplitQuantity,
            INPUT  cItemType,
            INPUT  cUser,
            OUTPUT lSuccess,
            OUTPUT cMessage,
            OUTPUT TABLE ttItem
            ) NO-ERROR. 

        IF NOT lSuccess THEN DO:
            MESSAGE cMessage 
                VIEW-AS ALERT-BOX ERROR.
        END.    
        ELSE DO:
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}
            FOR EACH ttItem:
                {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT ttItem.tag, INPUT 1)"}
            END.
        END.       
    END.  
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
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.    
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
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
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

    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustID  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuantity          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCopies            AS INTEGER NO-UNDO.

    DEFINE VARIABLE cUserField1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue3 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dOvers AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "job-invalid" THEN DO:
            {methods/run_link.i "QTY-SOURCE" "DisableQuantities"}
            {methods/run_link.i "FGITEM-SOURCE" "DisableFGItem"}   
            {methods/run_link.i "USERFIELD-SOURCE" "DisableUserFields"}
            {methods/run_link.i "COPIES-SOURCE" "DisableCopies"}
            
            btCreate:SENSITIVE = FALSE.         
        END.
        WHEN "job-valid" THEN DO:
            
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                cJobNo   = oJobHeader:GetValue("JobNo")
                iJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
                iFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
                iBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
                cCustID  = oJobHeader:GetValue("Customer")
                .

            oCustomer:SetContext(cCompany, cCustID).
            
            IF VALID-OBJECT(oCustomer) THEN
                iCopies = INTEGER(oCustomer:GetValue("PrintCopies")).
                        
            IF iCopies LE 0 THEN
                iCopies = giNK1PrintCopies.
                
            IF iCopies LE 0 THEN
                iCopies = giDefaultPrintCopies.

            IF iCopies LE 0 THEN
                iCopies = 1.
                
            {methods/run_link.i "COPIES-SOURCE" "SetCopies" "(INPUT iCopies)"}

            {methods/run_link.i "FGITEM-SOURCE" "UpdateItemForJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo)"}
            
            {methods/run_link.i "FGITEM-SOURCE" "EnableFGItem"} 
            {methods/run_link.i "COPIES-SOURCE" "EnableCopies"}
        END.
        WHEN "fgitem-changed" THEN DO:
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            
            cItemID = oItemFG:GetValue("ItemID").
            
            {methods/run_link.i "JOB-SOURCE" "JobFGItemChanged" "(INPUT cItemID)"}
        END.
        WHEN "fgitem-valid" THEN DO:
            
            {methods/run_link.i "QTY-SOURCE" "EnableQuantities"}
            {methods/run_link.i "USERFIELD-SOURCE" "EnableUserFields"}
            
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                iQuantityInSubUnit = INTEGER(oItemFG:GetValue("QuantityInSubUnit"))
                iSubUnitsPerUnit   = INTEGER(oItemFG:GetValue("SubUnitsPerUnit"))
/*                iQuantity        = INTEGER(oJobHeader:GetValue("Quantity"))*/
                iQuantity          = iQuantityInSubUnit * iSubUnitsPerUnit
                .
            
            {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT 0)"}
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT 0)"} 
            
            IF NOT glAutoCreateLoadtagOnJobScan THEN           
                btCreate:SENSITIVE = TRUE.
            ELSE
                RUN state-changed (
                    INPUT THIS-PROCEDURE,
                    INPUT "create-tags"
                    ).          
        END.
        WHEN "overs-changed" THEN DO:
            {methods/run_link.i "USERFIELD-SOURCE" "GetOvers" "(OUTPUT dOvers)" }
            
            {methods/run_link.i "QTY-SOURCE" "SetOvers" "(INPUT dOvers)"}
        END.
        WHEN "create-tags" THEN DO:
            {methods/run_link.i "JOB-SOURCE" "GetJobHeader" "(OUTPUT oJobHeader)"}
            
            ASSIGN
                cJobNo   = oJobHeader:GetValue("JobNo")
                iJobNo2  = INTEGER(oJobHeader:GetValue("JobNo2"))
                iFormNo  = INTEGER(oJobHeader:GetValue("FormNo"))
                iBlankNo = INTEGER(oJobHeader:GetValue("BlankNo"))
                .
            
            {methods/run_link.i "FGITEM-SOURCE" "GetItemFG" "(OUTPUT oItemFG)"}
            
            cItemID = oItemFG:GetValue("ItemID").
            
            {methods/run_link.i "QTY-SOURCE" "GetQuantities" "(OUTPUT iQuantity, OUTPUT iQuantityInSubUnit, OUTPUT iSubUnitsPerUnit)"}
            
            {methods/run_link.i "COPIES-SOURCE" "GetCopies" "(OUTPUT iCopies)"}
            
            {methods/run_link.i "USERFIELD-SOURCE" "GetUserFields" "(OUTPUT cUserField1, OUTPUT cUserField2, OUTPUT cUserField3, OUTPUT cUserFieldValue1, OUTPUT cUserFieldValue2, OUTPUT cUserFieldValue3)" }
            
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo, INPUT cItemID, INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT iCopies, INPUT cUserField1, INPUT cUserField2, INPUT cUserField3, INPUT cUserFieldValue1, INPUT cUserFieldValue2, INPUT cUserFieldValue3)" }
            
            IF glAutoPrintLoadtagOnJobScan THEN DO:
                RUN state-changed (
                    INPUT THIS-PROCEDURE,
                    INPUT "print-tags"
                    ).
                
                RUN state-changed (
                    INPUT THIS-PROCEDURE,
                    INPUT "job-invalid"
                    ).
                    
                /* Makes the cursor stay in Job No field in JobFilter.w */
                {methods/run_link.i "JOB-SOURCE" "ScanNextJob"}
            END.
        END. 
        WHEN "print-tags" THEN DO:
            SESSION:SET-WAIT-STATE ("GENERAL").
            
            {methods/run_link.i "LOADTAG-SOURCE" "CreateLoadTagFromTT"}

            SESSION:SET-WAIT-STATE ("").
        END.
        WHEN "create-tag-reprint" THEN DO:
            RUN pReprintTag.
        END.
        WHEN "split-tag" THEN DO:
            RUN pSplitTag.
        END.
        WHEN "bol-tag-print" THEN DO:
            RUN pBOLTagPrint.
        END.
        WHEN "release-tag-print" THEN DO:
            RUN pReleaseTagPrint.
        END.
        WHEN "create-tag-po" THEN DO:
            RUN pPOTagPrint.
        END.
        WHEN "return-tag" THEN DO:
            RUN pReturnTag.
        END.
    END CASE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

