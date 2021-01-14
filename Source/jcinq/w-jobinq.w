&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: jcinq/w-jobinq.w
          
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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_b-jobinq

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR lv-current-page AS INT NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES job job-hdr
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job, job-hdr.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-jhdrin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmatin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchci AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchfi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchli AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchqi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchvi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchwi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobinq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_expxls AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_miscflds AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optonote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-11 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-9 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-9 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-nav2-10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_xferjobdata AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 163.2 BY 24
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 34 BY 2.14
         BGCOLOR 15 .

DEFINE FRAME frame-misc
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 159.6 ROW 2.91
         SIZE 3.8 BY 1.43
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45.2 ROW 1
         SIZE 115 BY 2.14
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.job,asi.job-hdr
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
         TITLE              = "Job Variance Inquiry"
         HEIGHT             = 24
         WIDTH              = 159.2
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
IF NOT W-Win:LOAD-ICON("adeicon\progress":U) THEN
    MESSAGE "Unable to load icon: adeicon\progress"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frame-misc:FRAME = FRAME F-Main:HANDLE
       FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME frame-misc:HANDLE)
       XXTABVALXX = FRAME message-frame:MOVE-BEFORE-TAB-ITEM (FRAME OPTIONS-FRAME:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME frame-misc
   UNDERLINE                                                            */
/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FRAME OPTIONS-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-misc
/* Query rebuild information for FRAME frame-misc
     _Query            is NOT OPENED
*/  /* FRAME frame-misc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame
/* Query rebuild information for FRAME message-frame
     _Query            is NOT OPENED
*/  /* FRAME message-frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME OPTIONS-FRAME
/* Query rebuild information for FRAME OPTIONS-FRAME
     _Query            is NOT OPENED
*/  /* FRAME OPTIONS-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Variance Inquiry */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Variance Inquiry */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

&SCOPED-DEFINE sysCtrlCompany g_company
&SCOPED-DEFINE sysCtrlName MiscJobCL
&SCOPED-DEFINE mfRecKey misc_rec_key_value
&SCOPED-DEFINE mfHeader misc_header_value
{methods/miscflds.i}
{custom/initializeprocs.i}
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
             INPUT  'smartobj/xferjobdata.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_xferjobdata ).
       RUN set-position IN h_xferjobdata ( 1.00 , 12.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/expxls.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_expxls ).
       RUN set-position IN h_expxls ( 1.00 , 28.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Material|MachHrs|MachQtys|Waste|MachCosts|D.L.|Var OH|Fixed OH|Misc' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 159.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/miscflds.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_miscflds ).
       RUN set-position IN h_miscflds ( 1.00 , 44.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 51.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 107.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_expxls. */
       RUN add-link IN adm-broker-hdl ( h_expxls , 'sort-data':U , THIS-PROCEDURE ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'spec':U , h_options ).
	   RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options ).
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_expxls ,
             h_xferjobdata , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME frame-misc:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_miscflds ,
             h_expxls , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
             h_miscflds , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.48 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.00 , 20.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.00 , 36.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jobinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jobinq ).
       RUN set-position IN h_b-jobinq ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.76 , 148.00 ) */

       /* Links to SmartViewer h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'move-columns':U , h_movecol ).

       /* Links to SmartNavBrowser h_b-jobinq. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol ,
             h_xferjobdata , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_optonote ,
             h_expxls , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobinq ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.00 , 36.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job ).
       RUN set-position IN h_v-job ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2 ).
       RUN set-position IN h_v-nav2 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jhdrin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jhdrin ).
       RUN set-position IN h_b-jhdrin ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jhdrin ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job ).

       /* Links to SmartViewer h_v-nav2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2 ).

       /* Links to SmartBrowser h_b-jhdrin. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jhdrin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2 ,
             h_v-job , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jhdrin ,
             h_v-nav2 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-2 ).
       RUN set-position IN h_v-job-2 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-2 ).
       RUN set-position IN h_v-nav2-2 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmatin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmatin ).
       RUN set-position IN h_b-jmatin ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmatin ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-2 ).

       /* Links to SmartViewer h_v-nav2-2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-2 ).

       /* Links to SmartBrowser h_b-jmatin. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmatin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-2 ,
             h_v-job-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmatin ,
             h_v-nav2-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-4 ).
       RUN set-position IN h_v-job-4 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-3 ).
       RUN set-position IN h_v-nav2-3 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchin ).
       RUN set-position IN h_b-jmchin ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchin ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

        /* Links to SmartViewer h_v-job-2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-4 ).

       /* Links to SmartViewer h_v-nav2-3. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-3 ).

       /* Links to SmartBrowser h_b-jmchin. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-3 ,
             h_v-job-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchin ,
             h_v-nav2-3 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-5 ).
       RUN set-position IN h_v-job-5 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-4 ).
       RUN set-position IN h_v-nav2-4 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchqi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchqi ).
       RUN set-position IN h_b-jmchqi ( 11.43 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchqi ( 13.14 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-5. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-5 ).

       /* Links to SmartViewer h_v-nav2-4. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-4 ).

       /* Links to SmartBrowser h_b-jmchqi. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchqi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-4 ,
             h_v-job-5 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchqi ,
             h_v-nav2-4 , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-6 ).
       RUN set-position IN h_v-job-6 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-5 ).
       RUN set-position IN h_v-nav2-5 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchwi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchwi ).
       RUN set-position IN h_b-jmchwi ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchwi ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-6. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-6 ).

       /* Links to SmartViewer h_v-nav2-5. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-5 ).

       /* Links to SmartBrowser h_b-jmchwi. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchwi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-6 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-5 ,
             h_v-job-6 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchwi ,
             h_v-nav2-5 , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-7 ).
       RUN set-position IN h_v-job-7 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-6 ).
       RUN set-position IN h_v-nav2-6 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchci.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchci ).
       RUN set-position IN h_b-jmchci ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchci ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-7. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-7 ).

       /* Links to SmartViewer h_v-nav2-6. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-6 ).

       /* Links to SmartBrowser h_b-jmchci. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchci ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-7 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-6 ,
             h_v-job-7 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchci ,
             h_v-nav2-6 , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-8 ).
       RUN set-position IN h_v-job-8 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-7 ).
       RUN set-position IN h_v-nav2-7 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchli.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchli ).
       RUN set-position IN h_b-jmchli ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchli ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-8. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-8 ).

       /* Links to SmartViewer h_v-nav2-7. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-7 ).

       /* Links to SmartBrowser h_b-jmchli. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchli ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-8 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-7 ,
             h_v-job-8 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchli ,
             h_v-nav2-7 , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-9 ).
       RUN set-position IN h_v-job-9 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-8 ).
       RUN set-position IN h_v-nav2-8 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchvi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchvi ).
       RUN set-position IN h_b-jmchvi ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchvi ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-9. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-9 ).

       /* Links to SmartViewer h_v-nav2-8. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-8 ).

       /* Links to SmartBrowser h_b-jmchvi. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchvi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-9 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-8 ,
             h_v-job-9 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchvi ,
             h_v-nav2-8 , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-10 ).
       RUN set-position IN h_v-job-10 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-nav2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-nav2-9 ).
       RUN set-position IN h_v-nav2-9 ( 8.86 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchfi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchfi ).
       RUN set-position IN h_b-jmchfi ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchfi ( 13.10 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-10. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-10 ).

       /* Links to SmartViewer h_v-nav2-9. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-nav2-9 ).

       /* Links to SmartBrowser h_b-jmchfi. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_b-jmchfi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-10 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-nav2-9 ,
             h_v-job-10 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchfi ,
             h_v-nav2-9 , 'AFTER':U ).
    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-11 ).
       RUN set-position IN h_v-job-11 ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-4 ).
       RUN set-position IN h_v-navest-4 ( 9.10 , 45.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/bi-jobprp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jobprp ).
       RUN set-position IN h_b-jobprp ( 10.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-jobprp ( 11.43 , 143.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-3 ).
       RUN set-position IN h_p-updsav-3 ( 22.67 , 47.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-3 ( 1.76 , 55.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-10. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job-11 ).
       
       /* Links to SmartViewer h_v-navest-4. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-navest-4 ).

       /* Links to SmartBrowser h_b-jobprp. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_b-jobprp ).
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobprp ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-11 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-4 ,
             h_v-job-11 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobprp ,
             h_v-navest-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-3 ,
             h_b-jobprp , 'AFTER':U ).
    END. /* Page 11 */

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
  {src/adm/template/row-list.i "job-hdr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}
  {src/adm/template/row-find.i "job-hdr"}

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
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME frame-misc IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frame-misc}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/winReSizePgChg.i}

  

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute ('Current-Page':U).
  lv-current-page = int(RETURN-VALUE).

  IF lv-current-page EQ 1 THEN
     RUN value-changed-proc IN h_b-jobinq.
  ELSE
  IF lv-current-page EQ 2 THEN
     RUN value-changed-proc IN h_b-jhdrin.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available W-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE job THEN
  RUN custom/setUserPrint.p (job.company,'job_.',
                             'begin_job1,begin_job2,end_job1,end_job2,tb_reprint,fl-jobord',
                             job.job-no + ',' + STRING(job.job-no2) + ',' +
                             job.job-no + ',' + STRING(job.job-no2) + ',' +
                             STRING(job-hdr.ftick-prnt) + ',' +  "0" ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_ONote W-Win 
PROCEDURE select_ONote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/select_ONote.i rec_key_value cHeaderValue """" 1}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-page-data W-Win 
PROCEDURE send-page-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcPageFrom AS CHAR NO-UNDO INIT "".
  ASSIGN pcPageFrom = STRING(lv-current-page) .
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "job-hdr"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xferJobData W-Win 
PROCEDURE xferJobData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE job-hdr THEN DO:
    RUN est/xferJobData.w (job-hdr.company
                          ,job-hdr.est-no
                          ,job-hdr.job-no
                          ,job-hdr.job-no2
                          ,h_b-jmchin
                           ).
    IF VALID-HANDLE(h_b-jmchin) THEN
    RUN dispatch IN h_b-jmchin ('open-query').
  END. /* avail job-hdr */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

