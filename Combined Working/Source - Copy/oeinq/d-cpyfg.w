&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF OUTPUT PARAM ip-all AS LOG NO-UNDO.
DEF OUTPUT PARAM ip-whbn AS LOG NO-UNDO.
DEF OUTPUT PARAM ip-cpyspc AS LOG NO-UNDO.
DEF OUTPUT PARAM ip-begspc AS CHAR NO-UNDO.
DEF OUTPUT PARAM ip-endspc AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF BUFFER bf-xf FOR ef.
DEF BUFFER b-tef FOR ef.
DEF BUFFER b-fef FOR ef.
DEF BUFFER b-feb FOR eb.

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 tb_cpyall tb_cpywhbn begin_spec ~
tb_cpyspcnts end_spec btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cpyall tb_cpywhbn begin_spec ~
tb_cpyspcnts end_spec 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_spec AS CHARACTER FORMAT "X(3)":U 
     LABEL "From Spec Code" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_spec AS CHARACTER INIT "zzz" FORMAT "X(3)":U 
     LABEL "To Spec Code" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 3.57.

DEFINE VARIABLE tb_cpyall AS LOGICAL INITIAL yes 
     LABEL "Copy View Item Costs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 65.6 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cpyspcnts AS LOGICAL INITIAL no 
     LABEL "Copy Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.6 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cpywhbn AS LOGICAL INITIAL no 
     LABEL "Copy Vend Cost Matrix?" 
     VIEW-AS TOGGLE-BOX
     SIZE 65.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_cpyall AT ROW 2.1 COL 69 RIGHT-ALIGNED WIDGET-ID 6
     tb_cpywhbn AT ROW 3.67 COL 69 RIGHT-ALIGNED WIDGET-ID 8
     tb_cpyspcnts AT ROW 5.71 COL 36 RIGHT-ALIGNED WIDGET-ID 14
     begin_spec AT ROW 5.57 COL 51.6 COLON-ALIGNED WIDGET-ID 18
     end_spec AT ROW 6.81 COL 51.6 COLON-ALIGNED WIDGET-ID 20
     btn-ok AT ROW 9.33 COL 15 WIDGET-ID 10
     btn-cancel AT ROW 9.33 COL 40.8 WIDGET-ID 12
     RECT-1 AT ROW 5.05 COL 3 WIDGET-ID 22
     SPACE(0.99) SKIP(2.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Copy Transactions"
         DEFAULT-BUTTON btn-ok CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_cpyall IN FRAME D-Dialog
   ALIGN-R                                                              */
/* SETTINGS FOR TOGGLE-BOX tb_cpyspcnts IN FRAME D-Dialog
   ALIGN-R                                                              */
/* SETTINGS FOR TOGGLE-BOX tb_cpywhbn IN FRAME D-Dialog
   ALIGN-R                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON GO OF FRAME D-Dialog /* Copy Transactions */
DO:
    IF tb_cpyall:SCREEN-VALUE EQ "YES" THEN
        ip-all = YES .
    ELSE ip-all = NO .
    IF tb_cpywhbn:SCREEN-VALUE EQ "YES" THEN
        ip-whbn = YES .
    ELSE ip-whbn = NO .
       
    IF tb_cpyspcnts:SCREEN-VALUE EQ "YES" THEN do:
        ASSIGN
            ip-cpyspc = YES 
            ip-begspc = begin_spec:SCREEN-VALUE
            ip-endspc = end_spec:SCREEN-VALUE .
    END.
    ELSE
        ASSIGN
            ip-cpyspc = NO 
            ip-begspc = ""
            ip-endspc = "" .

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Copy Transactions */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel D-Dialog
ON CHOOSE OF btn-cancel IN FRAME D-Dialog /* Cancel */
DO:
    ip-all = YES .
    ip-whbn = YES . 
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok D-Dialog
ON CHOOSE OF btn-ok IN FRAME D-Dialog /* OK */
DO:
    IF tb_cpyall:SCREEN-VALUE EQ "YES" THEN
        ip-all = YES .
    ELSE ip-all = NO .
    IF tb_cpywhbn:SCREEN-VALUE EQ "YES" THEN
        ip-whbn = YES .
    ELSE ip-whbn = NO .
    IF tb_cpyspcnts:SCREEN-VALUE EQ "YES" THEN do:
        ASSIGN
            ip-cpyspc = YES 
            ip-begspc = begin_spec:SCREEN-VALUE
            ip-endspc = end_spec:SCREEN-VALUE .
    END.
    ELSE
        ASSIGN
            ip-cpyspc = NO 
            ip-begspc = ""
            ip-endspc = "" .

 RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cpyall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cpyall D-Dialog
ON VALUE-CHANGED OF tb_cpyall IN FRAME D-Dialog /* Copy View Item Costs? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cpyspcnts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cpyspcnts D-Dialog
ON VALUE-CHANGED OF tb_cpyspcnts IN FRAME D-Dialog /* Copy Spec Notes? */
DO:
  assign {&self-name}.
  IF {&self-name} EQ YES THEN
      ASSIGN
      begin_spec:SENSITIVE = YES
      end_spec:SENSITIVE = YES .
  ELSE
       ASSIGN
      begin_spec:SENSITIVE = NO
      end_spec:SENSITIVE = NO .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cpywhbn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cpywhbn D-Dialog
ON VALUE-CHANGED OF tb_cpywhbn IN FRAME D-Dialog /* Copy Vend Cost Matrix? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
DEF VAR ll-tandem AS LOG NO-UNDO.

{src/adm/template/dialogmn.i}

    RUN enable_UI.

/*{methods/nowait.i}*/
    
 /* IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY tb_cpyall tb_cpywhbn begin_spec tb_cpyspcnts end_spec 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 tb_cpyall tb_cpywhbn begin_spec tb_cpyspcnts end_spec btn-ok 
         btn-cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

 /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
      {custom/usrprint.i}
          APPLY "entry" TO begin_spec.
  END.
  
  IF tb_cpyspcnts:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Yes" THEN
      ASSIGN
      begin_spec:SENSITIVE IN FRAME {&FRAME-NAME} = YES
      end_spec:SENSITIVE IN FRAME {&FRAME-NAME} = YES .
  ELSE
      ASSIGN
          begin_spec:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          end_spec:SENSITIVE IN FRAME {&FRAME-NAME} = NO .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

