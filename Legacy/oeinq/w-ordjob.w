&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: oeinq\w-ordjob.w

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
DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

DEFINE VARIABLE misc_rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE misc_header_value AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-jhdrin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmati2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmatin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchci AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchfi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchli AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchqi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchvi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-jmchwi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-ordjob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-job-9 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 21.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.oe-ordl
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Jobs for Order Line Item"
         HEIGHT             = 21.43
         WIDTH              = 150
         MAX-HEIGHT         = 23.76
         MAX-WIDTH          = 158.6
         VIRTUAL-HEIGHT     = 23.76
         VIRTUAL-WIDTH      = 158.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Jobs for Order Line Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Jobs for Order Line Item */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl(this-procedure,"quote-source", output char-hdl).
  run hide-estimate in widget-handle(char-hdl).

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
             INPUT  'FOLDER-LABELS = ':U + 'View Job|Material|Matl Info|Mach Hrs|Mach Qtys|Waste|MachCosts|D.L.|Var OH|Fixed OH' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.43 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/q-ordjob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-ordjob ).
       RUN set-position IN h_q-ordjob ( 4.10 , 22.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q-ordjob. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-ordjob ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job ).
       RUN set-position IN h_v-job ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jhdrin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jhdrin ).
       RUN set-position IN h_b-jhdrin ( 6.95 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-jhdrin ( 15.00 , 144.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job ).

       /* Links to SmartBrowser h_b-jhdrin. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jhdrin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jhdrin ,
             h_v-job , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-2 ).
       RUN set-position IN h_v-job-2 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmatin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmatin ).
       RUN set-position IN h_b-jmatin ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmatin ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-2. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-2 ).

       /* Links to SmartBrowser h_b-jmatin. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmatin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmatin ,
             h_v-job-2 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-3 ).
       RUN set-position IN h_v-job-3 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmati2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmati2 ).
       RUN set-position IN h_b-jmati2 ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmati2 ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-3. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-3 ).

       /* Links to SmartBrowser h_b-jmati2. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmati2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmati2 ,
             h_v-job-3 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-4 ).
       RUN set-position IN h_v-job-4 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchin ).
       RUN set-position IN h_b-jmchin ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchin ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-4. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-4 ).

       /* Links to SmartBrowser h_b-jmchin. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchin ,
             h_v-job-4 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-5 ).
       RUN set-position IN h_v-job-5 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchqi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchqi ).
       RUN set-position IN h_b-jmchqi ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchqi ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-5. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-5 ).

       /* Links to SmartBrowser h_b-jmchqi. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchqi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchqi ,
             h_v-job-5 , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-6 ).
       RUN set-position IN h_v-job-6 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchwi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchwi ).
       RUN set-position IN h_b-jmchwi ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchwi ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-6. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-6 ).

       /* Links to SmartBrowser h_b-jmchwi. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchwi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-6 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchwi ,
             h_v-job-6 , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-7 ).
       RUN set-position IN h_v-job-7 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchci.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchci ).
       RUN set-position IN h_b-jmchci ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchci ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-7. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-7 ).

       /* Links to SmartBrowser h_b-jmchci. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchci ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-7 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchci ,
             h_v-job-7 , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-8 ).
       RUN set-position IN h_v-job-8 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchli.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchli ).
       RUN set-position IN h_b-jmchli ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchli ( 15.24 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-8. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-8 ).

       /* Links to SmartBrowser h_b-jmchli. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchli ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-8 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchli ,
             h_v-job-8 , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-9 ).
       RUN set-position IN h_v-job-9 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchvi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchvi ).
       RUN set-position IN h_b-jmchvi ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchvi ( 15.48 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-9. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-9 ).

       /* Links to SmartBrowser h_b-jmchvi. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchvi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-9 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchvi ,
             h_v-job-9 , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/v-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-job-10 ).
       RUN set-position IN h_v-job-10 ( 2.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.29 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jcinq/b-jmchfi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-jmchfi ).
       RUN set-position IN h_b-jmchfi ( 6.95 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-jmchfi ( 15.48 , 148.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-job-10. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_v-job-10 ).

       /* Links to SmartBrowser h_b-jmchfi. */
       RUN add-link IN adm-broker-hdl ( h_q-ordjob , 'Record':U , h_b-jmchfi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-job-10 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-jmchfi ,
             h_v-job-10 , 'AFTER':U ).
    END. /* Page 10 */

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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

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
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-g_rec_key W-Win 
PROCEDURE Get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MF-Message W-Win 
PROCEDURE MF-Message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Notes-Message W-Win 
PROCEDURE Notes-Message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-notes AS LOGICAL NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset-g_rec_key W-Win 
PROCEDURE Reset-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  {src/adm/template/snd-list.i "oe-ordl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Misc-Rec-Key_Header W-Win 
PROCEDURE Set-Misc-Rec-Key_Header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

   ASSIGN
      misc_rec_key_value = ip-rec_key
      misc_header_value = ip-header.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Rec-Key_Header W-Win 
PROCEDURE Set-Rec-Key_Header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

  ASSIGN
    rec_key_value = ip-rec_key
    header_value = ip-header.

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

