&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: jc/w-jobcst.w
          
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
&SCOPED-DEFINE h_Object01 h_p-jobhdr
&SCOPED-DEFINE h_Object02 h_p-jobmat
&SCOPED-DEFINE h_Object03 h_p-jobmch
&SCOPED-DEFINE h_Object04 h_p-updsav-3
&SCOPED-DEFINE h_Object05 h_p-updsav-2
&SCOPED-DEFINE h_Object06 h_p-farmh


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE ITEM_spec fgitem

def var li-prev-page as int init 1 no-undo.
def var li-cur-page as int init 1 no-undo.
def var h-detail as handle no-undo.
DEF VAR li-last-page AS INT NO-UNDO.  /* for folding estimate page */
DEF VAR ll-farm-visible AS LOG NO-UNDO INIT YES. /* adjustment for inactive farmout tab */
DEF VAR llJobFarmSec AS LOG NO-UNDO.
DEF VAR llDummy AS LOG NO-UNDO.
DEF VAR lcAccessList AS CHAR NO-UNDO.
DEF VAR chk-date AS LOG NO-UNDO INIT NO .

/* gdm - 05290901 */
DEF VAR v-start-flag AS LOG NO-UNDO INIT YES.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "JobFarm",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT llJobFarmSec, /* Allowed? Yes/NO */
     OUTPUT llDummy, /* used in template/windows.i  */
     OUTPUT lcAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

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
DEFINE VARIABLE h_b-jobfarm-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobhdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobinq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobmat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobmch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jobprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_capacityPage AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_expxls AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-add AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_miscflds AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-farmh AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-jobhdr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-jobmat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-jobmch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-hldjc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-jobesf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-jobest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-jobfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optonote AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.2 BY 23.81
         BGCOLOR 15 .

DEFINE FRAME FRAME-B
    WITH 1 DOWN /*KEEP-TAB-ORDER OVERLAY */
      /*   SIDE-LABELS NO-UNDERLINE THREE-D */
         AT COL 118 ROW 2.91
         SIZE 33 BY 1.43
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.24
         SIZE 31 BY 1.43
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 39 ROW 1
         SIZE 111.8 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.job,asi.job-hdr
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 8
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Job Costing"
         HEIGHT             = 23.81
         WIDTH              = 150.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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
ASSIGN FRAME FRAME-B:FRAME = FRAME F-Main:HANDLE
       FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME message-frame:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-B:HANDLE)
       XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME message-frame:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-B
    NOT-VISIBLE UNDERLINE                                                                    */
ASSIGN 
       FRAME FRAME-B:HIDDEN        = TRUE.
       FRAME FRAME-B:SENSITIVE        = FALSE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-B
/* Query rebuild information for FRAME FRAME-B
     _Query            is NOT OPENED
*/  /* FRAME FRAME-B */
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
ON END-ERROR OF W-Win /* Job Costing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Costing */
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
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-capacityPage.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_capacityPage ).
       RUN set-position IN h_capacityPage ( 1.00 , 17.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/expxls.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_expxls ).
       RUN set-position IN h_expxls ( 1.00 , 25.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/miscflds.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_miscflds ).
       RUN set-position IN h_miscflds ( 1.00 , 33.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Job|View Job|Materials|Routings|Preps|Estimate|FG Item|Farm' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-add.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-add ).
       RUN set-position IN h_f-add ( 1.00 , 41.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options2.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options2 ).
       RUN set-position IN h_options2 ( 1.00 , 49.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 105.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_expxls. */
       RUN add-link IN adm-broker-hdl ( h_expxls , 'sort-data':U , THIS-PROCEDURE ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_options2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'spec':U , h_options2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_miscflds ,
             h_expxls , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-add ,
             h_miscflds , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME FRAME-B:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_options2 ,
             h_f-add , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options2 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.00 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.00 , 1.00 ) NO-ERROR.
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
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobinq ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
        RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.00 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-job ).
       RUN set-position IN h_v-job ( 5.05 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 9.81 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-job ).
       RUN set-position IN h_p-job ( 9.81 , 36.00 ) NO-ERROR.
       RUN set-size IN h_p-job ( 1.91 , 101.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/vp-hldjc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-hldjc ).
       RUN set-position IN h_vp-hldjc ( 9.81 , 137.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 13.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobhdr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jobhdr ).
       RUN set-position IN h_b-jobhdr ( 12.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-jobhdr ( 9.29 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-jobhdr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-jobhdr ).
       RUN set-position IN h_p-jobhdr ( 22.67 , 41.00 ) NO-ERROR.
       RUN set-size IN h_p-jobhdr ( 1.76 , 74.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'job-hdr':U , h_v-job ).
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_v-job ).
       RUN add-link IN adm-broker-hdl ( h_p-job , 'TableIO':U , h_v-job ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-job':U , h_v-job ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-navest ).

       /* Links to SmartViewer h_vp-hldjc. */
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_vp-hldjc ).

       /* Links to SmartNavBrowser h_b-jobhdr. */
       RUN add-link IN adm-broker-hdl ( h_p-jobhdr , 'TableIO':U , h_b-jobhdr ).
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobhdr ).
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'p2':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_v-job , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-job ,
             h_v-navest , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-hldjc ,
             h_p-job , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobhdr ,
             h_vp-hldjc , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-jobhdr ,
             h_b-jobhdr , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobmat.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-jobmat ).
       RUN set-position IN h_b-jobmat ( 10.95 , 2.80 ) NO-ERROR.
       RUN set-size IN h_b-jobmat ( 11.62 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-2 ).
       RUN set-position IN h_v-job-2 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-2 ).
       RUN set-position IN h_v-navest-2 ( 9.10 , 57.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-jobmat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-jobmat ).
       RUN set-position IN h_p-jobmat ( 22.67 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-jobmat ( 1.76 , 144.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-jobmat. */
       RUN add-link IN adm-broker-hdl ( h_p-jobmat , 'TableIO':U , h_b-jobmat ).
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobmat ).

       /* Links to SmartViewer h_v-job-2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_v-job-2 ).
       RUN add-link IN adm-broker-hdl ( h_b-jobmat , 'job':U , h_v-job-2 ).

       /* Links to SmartViewer h_v-navest-2. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-navest-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-2 ,
             h_v-job-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-jobmat ,
             h_v-navest-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-3 ).
       RUN set-position IN h_v-job-3 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-3 ).
       RUN set-position IN h_v-navest-3 ( 9.10 , 49.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobmch.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-jobmch ).
       RUN set-position IN h_b-jobmch ( 10.76 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-jobmch ( 11.81 , 144.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/p-jobmch.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-jobmch ).
       RUN set-position IN h_p-jobmch ( 22.67 , 19.00 ) NO-ERROR.
       RUN set-size IN h_p-jobmch ( 1.76 , 101.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartViewer h_v-job-3. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_v-job-3 ).
       RUN add-link IN adm-broker-hdl ( h_b-jobmch , 'job':U , h_v-job-3 ).

       /* Links to SmartViewer h_v-navest-3. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-navest-3 ).

       /* Links to SmartBrowser h_b-jobmch. */
       RUN add-link IN adm-broker-hdl ( h_p-jobmch , 'TableIO':U , h_b-jobmch ).
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobmch ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-3 ,
             h_v-job-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobmch ,
             h_v-navest-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-jobmch ,
             h_b-jobmch , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-4 ).
       RUN set-position IN h_v-job-4 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-4 ).
       RUN set-position IN h_v-navest-4 ( 9.10 , 45.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobprp.w':U ,
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

       /* Links to SmartViewer h_v-job-4. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_v-job-4 ).

       /* Links to SmartViewer h_v-navest-4. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'nav-itm':U , h_v-navest-4 ).

       /* Links to SmartBrowser h_b-jobprp. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_b-jobprp ).
       RUN add-link IN adm-broker-hdl ( h_v-job , 'Record':U , h_b-jobprp ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-4 ,
             h_v-job-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobprp ,
             h_v-navest-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-3 ,
             h_b-jobprp , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-jobest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-jobest ).
       /* Position in AB:  ( 5.57 , 4.60 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartWindow h_w-jobest. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_w-jobest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Estimate':U , h_w-jobest ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/w-jobfg.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-jobfg ).
       /* Position in AB:  ( 6.71 , 18.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartWindow h_w-jobfg. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_w-jobfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-jobfg ).

    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/b-jobfarm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jobfarm-3 ).
       RUN set-position IN h_b-jobfarm-3 ( 5.76 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-jobfarm-3 ( 11.67 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-farmh.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-farmh ).
       RUN set-position IN h_p-farmh ( 19.10 , 113.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 19.33 , 22.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 1.76 , 82.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartBrowser h_b-jobfarm-3. */
       RUN add-link IN adm-broker-hdl ( h_b-jobhdr , 'Record':U , h_b-jobfarm-3 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_b-jobfarm-3 ).

       /* Links to SmartViewer h_p-farmh. */
       RUN add-link IN adm-broker-hdl ( h_b-jobfarm-3 , 'history':U , h_p-farmh ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jobfarm-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-farmh ,
             h_b-jobfarm-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-2 ,
             h_p-farmh , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-jobesf.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-jobesf ).
       /* Position in AB:  ( 8.38 , 38.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartWindow h_w-jobesf. */
       RUN add-link IN adm-broker-hdl ( h_b-jobinq , 'Record':U , h_w-jobesf ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Estimate':U , h_w-jobesf ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 9 */

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE capacityPage W-Win
PROCEDURE capacityPage:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN schedule/capacityPage.w ("Job", ROWID(job), job.company).
/*    RUN schedule/sbHTML.p ("Job", ROWID(job), job.company).*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flag W-Win 
PROCEDURE check-flag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN v-start-flag = YES.

MESSAGE "ASSIGN FLAG" SKIP
    v-start-flag
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-enable-farm W-Win 
PROCEDURE disable-enable-farm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {est/farmTab.i}
  /* Indicates whether farm tab is visible */
  ll-farm-visible = ipPurchased.
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
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
/*  VIEW FRAME FRAME-B IN WINDOW W-Win.*/
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  FRAME FRAME-B:SENSITIVE = NO.
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-estimate W-Win 
PROCEDURE hide-estimate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN hide-page.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-page W-Win 
PROCEDURE hide-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN select-page (li-prev-page).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-page AS INT NO-UNDO.
  DEF VAR lv-row-list AS CHAR NO-UNDO.
  DEF VAR cLinkHnd AS CHAR.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN get-attribute ('current-page').
  ASSIGN li-prev-page = li-cur-page
         li-cur-page = INT(RETURN-VALUE)
         li-page = INT(RETURN-VALUE).

  RUN GET-ATTRIBUTE ('NEW-JOB').
  IF li-page = 1 AND RETURN-VALUE <> '' AND RETURN-VALUE <> ? THEN DO:
     STATUS INPUT 'Refreshing Browser..... ' .
     SESSION:SET-WAIT-STATE('general').
     lv-row-list = RETURN-VALUE.
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source', OUTPUT char-hdl).
     RUN record-added IN WIDGET-HANDLE(char-hdl).     
     RUN reopen-query IN WIDGET-HANDLE(char-hdl) (TO-ROWID(lv-row-list)).
     RUN set-attribute-list ('NEW-JOB = ' + '').
  END.
  IF li-page = 1 THEN
  DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source', OUTPUT char-hdl).

     IF char-hdl NE "" THEN
        RUN value-changed-proc IN WIDGET-HANDLE(char-hdl).
  END.
  ELSE IF li-page = 2 THEN
  DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'p2-source', OUTPUT char-hdl).
     IF char-hdl NE "" THEN
        RUN value-changed-proc IN WIDGET-HANDLE(char-hdl).
  END.
  ELSE
  IF li-page EQ 6 THEN DO:  /* estimate */
    
    li-last-page = li-prev-page.

    /* This attempts to clear error of fg item screen appearing when user clicks on estimate */
/*    RUN init-pages IN THIS-PROCEDURE ('8':U) NO-ERROR.*/
/*    RUN init-pages IN THIS-PROCEDURE ('7':U) NO-ERROR.*/
/*    RUN init-pages IN THIS-PROCEDURE ('6':U) NO-ERROR.*/

    FIND FIRST est NO-LOCK WHERE est.company EQ job.company
                             AND est.est-no EQ job.est-no NO-ERROR.
    IF AVAILABLE est THEN DO:
   
      IF est.est-type LE 4 THEN DO:

        RUN select-page (9).

        li-prev-page = li-last-page.
        RETURN.
      END. /* est-type le 4 */
    END. /* avail est */
    ELSE DO:
      MESSAGE 'SORRY, NO ESTIMATE EXISTS FOR THIS ORDER...'
           VIEW-AS ALERT-BOX ERROR.      
      RUN hide-page.
      RETURN NO-APPLY.        
    END.
  END. /* li-cur-page eq 6 */
  ELSE
  IF li-page EQ 7 THEN DO:  /* fg item */
    li-last-page = li-prev-page.
    IF NOT AVAIL job-hdr OR
       NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ job-hdr.company
                                   AND itemfg.i-no EQ job-hdr.i-no) /*OR
       NOT CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                                    AND oe-ordl.job-no EQ job-hdr.job-no
                                    AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                    AND oe-ordl.ord-no EQ job-hdr.ord-no
                                    AND oe-ordl.i-no EQ job-hdr.i-no)*/ THEN DO:
      IF AVAIL job-hdr THEN
        MESSAGE "Sorry, " + CAPS(TRIM(job-hdr.i-no)) + " is not in the FG File..."
            VIEW-AS ALERT-BOX ERROR.
      RUN hide-page.
      RETURN NO-APPLY.        
    END.
  END. /* li-cur-page eq 7 */


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF li-page EQ 8 THEN DO:  /* farm */
 
      IF NOT llJobFarmSec THEN DO:    
        /* Disable Update Panel */
        RUN get-link-handle IN adm-broker-hdl (h_p-updsav-2, 'TableIO':U, OUTPUT cLinkHnd).       
        
        RUN init-pages IN THIS-PROCEDURE ('8':U) NO-ERROR.
        IF VALID-HANDLE(HANDLE(cLinkHnd)) THEN
        RUN remove-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_b-jobfarm-3 ) NO-ERROR.
        IF VALID-HANDLE(h_p-updsav-2) THEN
            RUN set-buttons IN h_p-updsav-2 ('disable-all').
      END.
  END.
  {methods/winReSizePgChg.i}
  
  /* gdm - 05290901 */
  IF li-page = 2 AND NOT chk-date THEN DO:
         
    IF AVAIL job AND v-start-flag THEN
       RUN get-start-date IN h_v-job (YES).
  END.
  ASSIGN chk-date = NO .


  SESSION:SET-WAIT-STATE('').
  STATUS INPUT OFF.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/winReSizePgChg.i}

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
                             STRING(job-hdr.ftick-prnt) + ',' +  "0" ). /* gdm - 07130906 */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_add W-Win 
PROCEDURE select_add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  ASSIGN chk-date = YES .
  run select-page(2).
  run get-link-handle in adm-broker-hdl(this-procedure,"add-job-target", output char-hdl).
  run add-job in widget-handle(char-hdl).

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
  ASSIGN pcPageFrom = STRING(li-cur-page) .
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

