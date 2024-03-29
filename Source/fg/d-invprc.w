&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: fg\d-invprc.w
  
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

&SCOPED-DEFINE yellowColumnsName d-invprc
&SCOPED-DEFINE noSortbyField
&SCOPED-DEFINE SORTBY-PHRASE BY ar-invl.i-no
/* Parameters Definitions ---                                           */
DEF INPUT  PARAM ip-cust-no LIKE cust.cust-no NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-num-rec AS INT NO-UNDO.

DO TRANSACTION:
   {sys/inc/quoitem.i}
END.

{fg/d-invprc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-inv ar-invl itemfg

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ar-invl.i-no itemfg.i-name ar-invl.inv-qty ar-invl.unit-pr ar-invl.pr-qty-uom itemfg.stat  
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-inv, ~
                                   FIRST ar-invl NO-LOCK WHERE ROWID(ar-invl) EQ tt-inv.row-id, ~
                                   FIRST itemfg NO-LOCK                             WHERE itemfg.company EQ ar-invl.company                               AND itemfg.i-no    EQ ar-invl.i-no                             ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-inv, ~
                                   FIRST ar-invl NO-LOCK WHERE ROWID(ar-invl) EQ tt-inv.row-id, ~
                                   FIRST itemfg NO-LOCK                             WHERE itemfg.company EQ ar-invl.company                               AND itemfg.i-no    EQ ar-invl.i-no                             ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-inv ar-invl itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-inv
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 ar-invl
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-2 itemfg


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 Btn_OK Btn_Select Btn_Deselect ~
Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK  
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-inv, 
      ar-invl, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ar-invl.i-no          COLUMN-LABEL "FG Item#" LABEL-BGCOLOR 14 WIDTH 20
      itemfg.i-name         COLUMN-LABEL "FG Name" LABEL-BGCOLOR 14
      ar-invl.inv-qty       COLUMN-LABEL "Qty Invoiced" LABEL-BGCOLOR 14
      ar-invl.unit-pr       COLUMN-LABEL "Last Price" LABEL-BGCOLOR 14
      ar-invl.pr-qty-uom    COLUMN-LABEL "UOM" LABEL-BGCOLOR 14
      itemfg.stat           COLUMN-LABEL "Item Status" LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 105 BY 12.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 1
     Btn_OK AT ROW 14.81 COL 9
     Btn_Select AT ROW 14.81 COL 30
     Btn_Deselect AT ROW 14.81 COL 51
     Btn_Cancel AT ROW 14.81 COL 72
     SPACE(8.59) SKIP(0.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select FGs for Customer:"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME D-Dialog = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-inv,
                            FIRST ar-invl NO-LOCK WHERE ROWID(ar-invl) EQ tt-inv.row-id,
                            FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ ar-invl.company
                              AND itemfg.i-no    EQ ar-invl.i-no
                            ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Select FGs for Customer: */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON START-SEARCH OF BROWSE-2 IN FRAME D-Dialog
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect D-Dialog
ON CHOOSE OF Btn_Deselect IN FRAME D-Dialog /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN DO:
    MESSAGE "This will create line items from the FGs selected." SKIP
            "Do you want to continue?" VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO UPDATE ll-ans.

    IF ll-ans THEN
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
      IF itemfg.stat:SCREEN-VALUE IN BROWSE {&browse-name} EQ "I" THEN DO:
          MESSAGE "Item status is Inactive..." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      ELSE DO:
        IF AVAIL tt-inv THEN tt-inv.selekt = YES.
      APPLY "go" TO FRAME {&FRAME-NAME}.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select D-Dialog
ON CHOOSE OF Btn_Select IN FRAME D-Dialog /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

EMPTY TEMP-TABLE tt-inv.

FIND FIRST cust
    WHERE cust.company EQ cocode 
      AND cust.cust-no EQ ip-cust-no
    NO-LOCK NO-ERROR.

IF AVAIL cust THEN DO:
  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " " +
                              TRIM(cust.cust-no) + " " +
                              TRIM(cust.name).
  RUN build-table.
END.

IF lv-num-rec GT 0 THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF quoitem-log EQ NO THEN
     FOR EACH ar-invl FIELDS(x-no i-no inv-qty) WHERE
         ar-invl.company EQ cust.company AND
         ar-invl.posted  EQ YES AND
         ar-invl.cust-no EQ cust.cust-no AND
         ar-invl.i-no    NE "" AND
         ar-invl.inv-qty GT 0
         NO-LOCK,
         FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK

         BREAK BY ar-invl.i-no
               BY ar-inv.inv-date DESC
               BY ar-invl.x-no    DESC:

         IF FIRST-OF(ar-invl.i-no) THEN DO:
            CREATE tt-inv.
            ASSIGN
               tt-inv.selekt = NO
               tt-inv.row-id = ROWID(ar-invl)
               lv-num-rec    = lv-num-rec + 1.
            RELEASE tt-inv.
         END.
     END.
  ELSE
     FOR EACH itemfg FIELDS(i-no) WHERE
         itemfg.company EQ cust.company AND
         itemfg.cust-no EQ cust.cust-no
         NO-LOCK:
    
         RELEASE ar-invl.
    
         FOR EACH ar-invl WHERE
             ar-invl.company EQ cust.company AND
             ar-invl.posted EQ YES AND
             ar-invl.cust-no EQ cust.cust-no AND
             ar-invl.i-no EQ itemfg.i-no AND
             ar-invl.inv-qty GT 0
             NO-LOCK,
             FIRST ar-inv WHERE
                   ar-inv.x-no EQ ar-invl.x-no
                   NO-LOCK
             BREAK BY ar-inv.inv-date DESC
                   BY ar-invl.x-no    DESC:
    
             LEAVE.
         END.
    
         IF AVAIL ar-invl THEN
         DO:
            CREATE tt-inv.
            ASSIGN
               tt-inv.row-id = ROWID(ar-invl)
               lv-num-rec    = lv-num-rec + 1.
            RELEASE tt-inv.
         END.
     END.

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
  ENABLE BROWSE-2 Btn_OK Btn_Select Btn_Deselect Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
  {src/adm/template/snd-list.i "tt-inv"}
  {src/adm/template/snd-list.i "ar-invl"}
  {src/adm/template/snd-list.i "itemfg"}

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

