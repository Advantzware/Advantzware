&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEF INPUT-OUTPUT PARAMETER io-rowid-list AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipd-start-date AS DATE NO-UNDO.
DEF OUTPUT PARAMETER opr-rowid-list AS CHAR NO-UNDO.

/*DEF INPUT PARAMETER ip-handle AS HANDLE NO-UNDO. */
/* Local Variable Definitions ---                                       */
DEF VAR io-qty-list AS CHAR NO-UNDO.
{custom/globdefs.i}
{sys/inc/var.i new shared}
ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR io-rowid AS ROWID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES cust
&Scoped-define FIRST-EXTERNAL-TABLE cust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_select 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-invapr AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_select 
     LABEL "&Select" 
     SIZE 39 BY 1.67
     FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btn_select AT ROW 19.1 COL 60
     SPACE(59.79) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Invoices For This Customer".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   External Tables: asi.cust
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Sales Analysis Inquiry for Customer: */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "go":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_select D-Dialog
ON CHOOSE OF btn_select IN FRAME D-Dialog /* Select */
DO:
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR historyQty AS DECIMAL NO-UNDO.
  DEF VAR historyPrice LIKE inv-line.price NO-UNDO.
  DEF VAR historyPrUOM LIKE inv-line.pr-uom NO-UNDO.
  DEF VAR setFromHistory AS LOGICAL NO-UNDO.


  /* DEF VAR io-rowid-list AS CHAR NO-UNDO. */
  DEF VAR li AS INT NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"row-id-source",OUTPUT char-hdl).
  RUN get-row-id IN WIDGET-HANDLE(char-hdl) (OUTPUT io-rowid-list, OUTPUT io-qty-list).
  opr-rowid-list = io-rowid-list.
  

  APPLY "go":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
io-rowid = TO-ROWID(io-rowid-list) NO-ERROR.
FIND inv-head WHERE ROWID(inv-head) EQ io-rowid NO-LOCK NO-ERROR.

IF AVAIL inv-head THEN
  FIND FIRST cust OF inv-head NO-LOCK NO-ERROR.

io-rowid = ?.

IF AVAIL cust THEN DO:
  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " " +
                              TRIM(cust.cust-no).
 
  {src/adm/template/dialogmn.i}
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-invapr.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-invapr ).
       RUN set-position IN h_b-invapr ( 1.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-invapr ( 17.86 , 155.00 ) NO-ERROR.
       IF VALID-HANDLE(h_b-invapr) THEN 
           RUN set-inv-date IN h_b-invapr (INPUT ipd-start-date).

       /* Links to SmartBrowser h_b-invapr. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_b-invapr ).
       RUN add-link IN adm-broker-hdl ( h_b-invapr , 'row-id':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-invapr ,
             btn_select:HANDLE , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeHistory D-Dialog 
PROCEDURE closeHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'CHOOSE' TO btn_select IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.

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
  ENABLE btn_select 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-item-no D-Dialog 
PROCEDURE get-item-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-ord-no AS INT NO-UNDO.
  DEF OUTPUT PARAM op-item-no AS cha NO-UNDO.


  ASSIGN
   op-ord-no = inv-head.bol-no.
   

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

