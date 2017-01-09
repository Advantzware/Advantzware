&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

/* Local Variable Definitions ---                                       */
DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE tt-rec FIELD old-i-no LIKE itemfg.i-no
                      FIELD new-i-no LIKE itemfg.i-no
                      INDEX old-i-no old-i-no
                      INDEX new-i-no IS PRIMARY new-i-no.

DEF BUFFER b-tt-rec FOR tt-rec.

DEF BUFFER bf-tt-rec FOR tt-rec.

{custom/globdefs.i}

{sys/inc/VAR.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER b-itemfg FOR itemfg.

{oe/oe-sysct1.i NEW}

RUN oe/oe-sysct.p.

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
&Scoped-define INTERNAL-TABLES tt-rec

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-rec.old-i-no tt-rec.new-i-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-rec.new-i-no   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-rec BY tt-rec.old-i-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.old-i-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-rec


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_cust-no btn_go BROWSE-2 Btn_process ~
Btn_Select Btn_Deselect Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_cust-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btn_go 
     LABEL "Go" 
     SIZE 11 BY 1.

DEFINE BUTTON Btn_process 
     LABEL "Change FG#s" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 20 BY 1.14.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-rec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-rec.old-i-no LABEL "Old FG Item#" WIDTH 40
     tt-rec.new-i-no LABEL "New FG Item#" WIDTH 40
     ENABLE tt-rec.new-i-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 83 BY 22.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_cust-no AT ROW 1.48 COL 27 COLON-ALIGNED
     btn_go AT ROW 1.48 COL 56
     BROWSE-2 AT ROW 2.91 COL 1
     Btn_process AT ROW 25.52 COL 1
     Btn_Select AT ROW 25.52 COL 22
     Btn_Deselect AT ROW 25.52 COL 43
     Btn_Cancel AT ROW 25.52 COL 64
     SPACE(0.39) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Correct FG Item#s for Customer"
         DEFAULT-BUTTON btn_go CANCEL-BUTTON Btn_Cancel.


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

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 btn_go D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.old-i-no
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

&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect D-Dialog
ON CHOOSE OF Btn_Deselect IN FRAME D-Dialog /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go D-Dialog
ON CHOOSE OF btn_go IN FRAME D-Dialog /* Go */
DO:
  IF CAN-FIND(FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.cust-no EQ fi_cust-no:SCREEN-VALUE
                AND INDEX(itemfg.i-no,fi_cust-no:SCREEN-VALUE) EQ 0)
  THEN RUN build-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_process D-Dialog
ON CHOOSE OF Btn_process IN FRAME D-Dialog /* Change FG#s */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  SESSION:SET-WAIT-STATE ("general").

  ll = {&browse-name}:NUM-SELECTED-ROWS GT 0.

  IF ll THEN DO:
    ll = NO.
    MESSAGE "Are you sure you wish renumber the selected FG Items?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
  END.

  IF ll THEN DO:
    ll = NO.
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

      IF AVAIL tt-rec THEN DO:
        FIND FIRST itemfg
             WHERE itemfg.company EQ cocode
               AND itemfg.i-no    EQ tt-rec.old-i-no
               AND NOT CAN-FIND(FIRST b-itemfg
                                WHERE b-itemfg.company EQ cocode
                                  AND b-itemfg.i-no    EQ tt-rec.new-i-no)
             NO-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
          RUN fg/updfgitm.p (recid(itemfg), tt-rec.new-i-no).
          ll = YES.
        END.
      END.
    END.
  END.

  SESSION:SET-WAIT-STATE ("").

  IF ll THEN MESSAGE "Selected FG Items renumbered..."
                 VIEW-AS ALERT-BOX.

  RUN build-table.
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


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no D-Dialog
ON HELP OF fi_cust-no IN FRAME D-Dialog /* Customer# */
DO:
    DEF VAR char-val AS CHAR NO-UNDO.

    RUN windows/l-cust.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" AND 
       FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) 
      THEN
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).

     APPLY "leave" TO fi_cust-no.
           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no D-Dialog
ON LEAVE OF fi_cust-no IN FRAME D-Dialog /* Customer# */
DO:
  fi_cust-no:SCREEN-VALUE = CAPS(fi_cust-no:SCREEN-VALUE).

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.cust-no EQ fi_cust-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
    MESSAGE "FG Item# does not exist with this customer..."
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO fi_cust-no.
    RETURN NO-APPLY.
  END.

  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


ON 'leave':U OF tt-rec.new-i-no IN BROWSE {&browse-name}
DO:
  tt-rec.new-i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
      CAPS(tt-rec.new-i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

  IF CAN-FIND(FIRST b-itemfg
              WHERE b-itemfg.company EQ cocode
                AND b-itemfg.i-no    EQ tt-rec.new-i-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
     CAN-FIND(FIRST b-tt-rec
              WHERE b-tt-rec.new-i-no EQ tt-rec.new-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND ROWID(b-tt-rec)   NE ROWID(tt-rec))
  THEN DO:
    MESSAGE "FG Item# already exists..."
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO tt-rec.new-i-no IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* ***************************  Main Block  *************************** */
IF v-est-fg1 NE "Fibre" THEN DO:
  MESSAGE "N-K-1 Parameter FGITEM# char value must be set to Fibre..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ELSE DO:
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
DEF VAR li    AS INT NO-UNDO.
DEF VAR li1   AS INT NO-UNDO.
DEF VAR li2   AS INT NO-UNDO. 
DEF VAR lv-i-no LIKE itemfg.i-no NO-UNDO.

/* gdm - 03300903 */
DEF VAR v-cnt AS INT NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-rec.

DO WITH FRAME {&FRAME-NAME}:
    IF fi_cust-no:SCREEN-VALUE NE "" 
      THEN
        FOR EACH itemfg NO-LOCK
           WHERE itemfg.company EQ cocode
             AND itemfg.cust-no EQ fi_cust-no:SCREEN-VALUE
             AND INDEX(itemfg.i-no,fi_cust-no:SCREEN-VALUE) EQ 0:

           CREATE tt-rec.

           ASSIGN
               tt-rec.old-i-no = itemfg.i-no
               tt-rec.new-i-no = itemfg.i-no
               tt-rec.new-i-no = SUBSTR(tt-rec.new-i-no,1,1) 
                               + TRIM(fi_cust-no:SCREEN-VALUE)
                               + SUBSTR(tt-rec.new-i-no,2 + 
                                        LENGTH(TRIM(fi_cust-no:SCREEN-VALUE))) 
              .

           IF CAN-FIND(FIRST b-itemfg
                           WHERE b-itemfg.company EQ cocode
                             AND b-itemfg.i-no    EQ tt-rec.new-i-no
                             AND ROWID(b-itemfg)  NE ROWID(itemfg)) 
             OR
              CAN-FIND(FIRST b-tt-rec
                          WHERE b-tt-rec.new-i-no EQ tt-rec.new-i-no
/*                             AND ROWID(b-tt-rec)   NE ROWID(tt-rec) */
                       )
             THEN DO:

               ASSIGN
                 li      = 0
                 lv-i-no = SUBSTR(itemfg.i-no,1,1) + 
                                  TRIM(fi_cust-no:SCREEN-VALUE)
                 li1     = LENGTH(TRIM(lv-i-no)) + 1
                 li2     = 0.

               FOR EACH b-itemfg NO-LOCK
                 WHERE b-itemfg.company          EQ cocode
                   AND b-itemfg.i-no               BEGINS TRIM(lv-i-no)
                   AND SUBSTR(b-itemfg.i-no,li1,4) GE "0000"
                   AND SUBSTR(b-itemfg.i-no,li1,4) LE "9999"
                 BY SUBSTR(b-itemfg.i-no,1,li1 + 3) DESC:

                 ASSIGN 
                     li = INT(SUBSTR(b-itemfg.i-no,li1,4)) NO-ERROR.
                 IF ERROR-STATUS:ERROR 
                   THEN li = 0.
                   ELSE LEAVE.
               END.

               FOR EACH b-tt-rec NO-LOCK
                 WHERE b-tt-rec.new-i-no               BEGINS TRIM(lv-i-no)
                   AND ROWID(b-tt-rec)                 NE ROWID(tt-rec)
                   AND SUBSTR(b-tt-rec.new-i-no,li1,4) GT STRING(li,"9999")
                   AND SUBSTR(b-tt-rec.new-i-no,li1,4) LE "9999"
                 BY SUBSTR(b-tt-rec.new-i-no,1,li1 + 3) DESC:

                 ASSIGN 
                     li2 = INT(SUBSTR(b-tt-rec.new-i-no,li1,4)) NO-ERROR. 
                 IF ERROR-STATUS:ERROR 
                   THEN li2 = 0.
                   ELSE LEAVE.
               END.
      
               ASSIGN 
                   li = MAX(li,li2)
                   tt-rec.new-i-no = lv-i-no + STRING(li + 1,"9999") + 
                                     SUBSTR(itemfg.i-no,li1 + 4,100).

           END. 

           ASSIGN
               tt-rec.old-i-no = CAPS(tt-rec.old-i-no)
               tt-rec.new-i-no = CAPS(tt-rec.new-i-no).
    END.
END.


  {&OPEN-QUERY-{&BROWSE-NAME}}

  SESSION:SET-WAIT-STATE ("").

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
  DISPLAY fi_cust-no 
      WITH FRAME D-Dialog.
  ENABLE fi_cust-no btn_go BROWSE-2 Btn_process Btn_Select Btn_Deselect 
         Btn_Cancel 
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
  {src/adm/template/snd-list.i "tt-rec"}

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

