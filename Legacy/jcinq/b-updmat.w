&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: b-updmat.w

  Description: Update Material Transactions
  
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
DEF INPUT PARAM ip-job-no AS CHAR NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.
DEF INPUT PARAM ip-sheet-num AS INT NO-UNDO.
DEF INPUT PARAM ip-blank-num AS INT NO-UNDO.
DEF INPUT PARAM ip-rm-i-no AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-mat-tran NO-UNDO
  FIELD tran-date AS DATE
  FIELD job-no AS CHAR
  FIELD job-no2 AS INT
  FIELD sheet-no AS INT
  FIELD blank-no AS INT
  FIELD i-no AS CHAR
  FIELD qty-posted AS DEC
  FIELD qty-waste AS DEC
  FIELD tag AS CHAR
  FIELD ext-cost AS DEC
  FIELD qty-uom AS CHAR
  FIELD ROWID AS ROWID
  INDEX tt-mat-tran-idx IS PRIMARY tran-date ASC job-no ASC job-no2 ASC
        sheet-no ASC blank-no ASC i-no ASC.

DEF VAR op-valid AS LOG NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define INTERNAL-TABLES tt-mat-tran

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-mat-tran.i-no tt-mat-tran.tran-date tt-mat-tran.job-no tt-mat-tran.job-no2 tt-mat-tran.sheet-no tt-mat-tran.blank-no tt-mat-tran.tag tt-mat-tran.qty-posted tt-mat-tran.qty-uom tt-mat-tran.ext-cost   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-mat-tran.tran-date tt-mat-tran.sheet-no ~
  tt-mat-tran.blank-no tt-mat-tran.i-no tt-mat-tran.tag tt-mat-tran.qty-posted ~
  tt-mat-tran.ext-cost tt-mat-tran.job-no tt-mat-tran.job-no2   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-mat-tran
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-mat-tran
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-mat-tran
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-mat-tran.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-mat-tran
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-mat-tran


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_add 
     LABEL "Add New Issue" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn_delete 
     LABEL "Delete Issue" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn_dril-dwn 
     LABEL "History" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE scr-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE scr-ext-cost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Ext. Cost" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE scr-loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Loc." 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE scr-qty-posted AS DECIMAL FORMAT "->>,>>>,>>9.9<<<<<":U INITIAL 0 
     LABEL "Qty. Posted" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE scr-qty-um AS CHARACTER FORMAT "X(4)":U 
     LABEL "Qty U/M" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE scr-tag AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE scr-tran-date AS DATE FORMAT "99/99/99":U 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-mat-tran SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-mat-tran.i-no      COLUMN-LABEL "RM Item #" FORMAT "X(15)" WIDTH 15   
   tt-mat-tran.tran-date    COLUMN-LABEL "Date" FORMAT "99/99/99"
   tt-mat-tran.job-no       COLUMN-LABEL "Job #" FORMAT "X(6)" WIDTH 8.5
   tt-mat-tran.job-no2      COLUMN-LABEL "" FORMAT ">9" WIDTH 2.8
   tt-mat-tran.sheet-no     COLUMN-LABEL "Sheet" FORMAT "ZZ9" WIDTH 7
   tt-mat-tran.blank-no     COLUMN-LABEL "Blank" FORMAT "ZZ9" WIDTH 7
   tt-mat-tran.tag          COLUMN-LABEL "Tag" FORMAT "X(20)" WIDTH 27
   tt-mat-tran.qty-posted   COLUMN-LABEL "Qty. Posted" FORMAT "->>,>>>,>>9.9<<<<<" WIDTH 23
   tt-mat-tran.qty-uom      COLUMN-LABEL "UOM" FORMAT "X(3)" WIDTH 9
   tt-mat-tran.ext-cost     COLUMN-LABEL "Ext. Cost" FORMAT "->>>,>>9.99" WIDTH 16
   ENABLE tt-mat-tran.tran-date tt-mat-tran.sheet-no
          tt-mat-tran.blank-no tt-mat-tran.i-no tt-mat-tran.tag tt-mat-tran.qty-posted
          tt-mat-tran.ext-cost tt-mat-tran.job-no tt-mat-tran.job-no2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 15.14
         BGCOLOR 8 FONT 2 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     scr-tran-date AT ROW 1.24 COL 17.4 COLON-ALIGNED
     scr-tag AT ROW 1.24 COL 37.6 COLON-ALIGNED
     scr-loc AT ROW 1.24 COL 75.6 COLON-ALIGNED
     scr-bin AT ROW 1.24 COL 94.2 COLON-ALIGNED
     btn_dril-dwn AT ROW 1.24 COL 117 WIDGET-ID 2
     scr-qty-posted AT ROW 2.67 COL 17.4 COLON-ALIGNED
     scr-qty-um AT ROW 2.67 COL 52.4 COLON-ALIGNED
     scr-ext-cost AT ROW 2.67 COL 75.2 COLON-ALIGNED
     btn_add AT ROW 2.67 COL 96
     btn_delete AT ROW 2.67 COL 117
     BROWSE-2 AT ROW 4.14 COL 2
     SPACE(0.59) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
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
/* BROWSE-TAB BROWSE-2 btn_add D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btn_add IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       btn_add:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR BUTTON btn_delete IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       btn_delete:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR BUTTON btn_dril-dwn IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       btn_dril-dwn:HIDDEN IN FRAME D-Dialog           = FALSE. 

/* SETTINGS FOR FILL-IN scr-bin IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-bin:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-ext-cost IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-ext-cost:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-loc IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-loc:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-qty-posted IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-qty-posted:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-qty-um IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-qty-um:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-tag IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-tag:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN scr-tran-date IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-tran-date:HIDDEN IN FRAME D-Dialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mat-tran
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
ON WINDOW-CLOSE OF FRAME D-Dialog
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME D-Dialog
DO:
  FIND FIRST users NO-LOCK WHERE 
         users.user_id EQ USERID(LDBNAME(1)) 
         NO-ERROR.
  IF AVAIL users AND users.securityLevel GT 899  AND
    AVAILABLE tt-mat-tran THEN DO:

    /*  RUN windows/ITEM.w .*/

   RUN set-read-only (INPUT NO).

   APPLY "entry" TO tt-mat-tran.tran-date IN BROWSE {&browse-name}.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



      &Scoped-define SELF-NAME tt-mat-tran.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mat-tran.job-no BROWSE-2 _BROWSE-COLUMN D-Dialog
ON LEAVE OF tt-mat-tran.job-no IN BROWSE BROWSE-2 /* Job No # */
DO:
    
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  RUN update-mat-act.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_add D-Dialog
ON CHOOSE OF btn_add IN FRAME D-Dialog /* Add New Issue */
DO:
   DEF VAR v-rowid AS ROWID NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      CREATE tt-mat-tran.
      CREATE mat-act.

      ASSIGN
         scr-tran-date
         scr-tag
         scr-qty-posted
         scr-qty-um
         scr-ext-cost
         scr-loc
         scr-bin
         tt-mat-tran.job-no = ip-job-no
         tt-mat-tran.job-no2 = ip-job-no2
         tt-mat-tran.sheet-no = ip-sheet-num
         tt-mat-tran.blank-no = ip-blank-num
         tt-mat-tran.i-no = ip-rm-i-no
         tt-mat-tran.tran-date = scr-tran-date
         tt-mat-tran.tag = scr-tag
         tt-mat-tran.qty-posted = scr-qty-posted
         tt-mat-tran.qty-uom = scr-qty-um
         tt-mat-tran.ext-cost = scr-ext-cost
         mat-act.company = cocode
         mat-act.mat-date  = scr-tran-date
         mat-act.job-no    = ip-job-no
         mat-act.job-no2   = ip-job-no2
         mat-act.s-num     = ip-sheet-num
         mat-act.b-num     = ip-blank-num
         mat-act.i-no      = ip-rm-i-no
         mat-act.rm-i-no   = ip-rm-i-no
         mat-act.tag       = scr-tag
         mat-act.loc       = scr-loc
         mat-act.loc-bin   = scr-bin
         mat-act.qty = scr-qty-posted
         mat-act.qty-uom   = scr-qty-um
         mat-act.ext-cost  = scr-ext-cost
         mat-act.opn       = yes
         mat-act.mat-time  = time
         tt-mat-tran.ROWID = ROWID(mat-act).

      FIND FIRST ITEM WHERE
           ITEM.company EQ cocode AND
           ITEM.i-no EQ ip-rm-i-no
           NO-LOCK NO-ERROR.

      IF AVAIL ITEM THEN
         ASSIGN
            mat-act.i-name    = ITEM.i-name
            mat-act.rm-i-name = ITEM.i-name.

      FIND FIRST job-hdr WHERE
           job-hdr.company EQ cocode AND
           job-hdr.job-no EQ ip-job-no AND
           job-hdr.job-no2 EQ ip-job-no2
           NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN
         mat-act.job = job-hdr.job.

      RELEASE mat-act.
      RELEASE tt-mat-tran.

      CLOSE QUERY browse-2.

      OPEN QUERY browse-2 FOR EACH tt-mat-tran.

      ASSIGN
         scr-tran-date:SCREEN-VALUE = ?
         scr-tag:SCREEN-VALUE = ""
         scr-qty-posted:SCREEN-VALUE = "0"
         scr-qty-um:SCREEN-VALUE = ?
         scr-ext-cost:SCREEN-VALUE = "0"
         scr-loc:SCREEN-VALUE = ""
         scr-bin:SCREEN-VALUE = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_delete D-Dialog
ON CHOOSE OF btn_delete IN FRAME D-Dialog /* Delete Issue */
DO:
   DEF VAR lv-rowid AS ROWID NO-UNDO.

   message "Delete Currently Selected Record(s)?" view-as alert-box question
          button yes-no update ll-ans as log.
 /*if not ll-ans then return error.*/
  IF ll-ans THEN do:

   DO WITH FRAME {&FRAME-NAME}:
       lv-rowid = ROWID(tt-mat-tran).
  
   FIND mat-act WHERE ROWID(mat-act) EQ tt-mat-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL mat-act THEN
       DELETE mat-act .
  END.

   FIND CURRENT tt-mat-tran EXCLUSIVE-LOCK NO-ERROR .
   IF AVAIL tt-mat-tran  THEN
       DELETE tt-mat-tran .
  
 END.
   
 CLOSE QUERY browse-2.

 OPEN QUERY browse-2 FOR EACH tt-mat-tran.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_dril-dwn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_dril-dwn D-Dialog
ON CHOOSE OF btn_dril-dwn IN FRAME D-Dialog /* DRILL DOWN */
DO:
    DEF VAR tag-no AS CHAR NO-UNDO.
    IF AVAIL tt-mat-tran THEN
        tag-no = tt-mat-tran.tag .
    ELSE tag-no = "" .
    
   IF /*USERID("nosweat") EQ "asi" AND*/
    AVAILABLE tt-mat-tran THEN DO:

      RUN rminq/b-rmhisp.w (ip-rm-i-no,tag-no,ip-job-no).

   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.


ON 'leave':U OF  tt-mat-tran.tran-date, tt-mat-tran.sheet-no, tt-mat-tran.blank-no,
     tt-mat-tran.i-no, tt-mat-tran.tag, tt-mat-tran.qty-posted, /*tt-mat-tran.job-no,*/ tt-mat-tran.job-no2
      IN BROWSE {&browse-name} DO: 

    RUN update-mat-act.
END.

ON 'leave':U OF tt-mat-tran.ext-cost IN BROWSE {&browse-name} DO:
    RUN update-record.
END.


ON 'return':U OF tt-mat-tran.tran-date, tt-mat-tran.sheet-no, tt-mat-tran.blank-no, tt-mat-tran.i-no,
   tt-mat-tran.tag, tt-mat-tran.qty-posted,tt-mat-tran.ext-cost,
   tt-mat-tran.qty-uom, /*tt-mat-tran.job-no,*/ tt-mat-tran.job-no2 IN BROWSE {&browse-name} DO:
   RUN update-record.
END.

FRAME {&FRAME-NAME}:TITLE = "Job #: " + ip-job-no + "-" + STRING(ip-job-no2)
                          + " Sheet: " + STRING(ip-sheet-num)
                          + " Blank: " + STRING(ip-blank-num)
                          + " RM Item #: " + ip-rm-i-no.

btn_dril-dwn:SENSITIVE = YES .

FIND FIRST users NO-LOCK WHERE 
         users.user_id EQ USERID(LDBNAME(1)) 
         NO-ERROR.
IF AVAIL users AND users.securityLevel GT 899 THEN
   ASSIGN
      btn_add:SENSITIVE = YES
      btn_add:HIDDEN = NO
      btn_delete:SENSITIVE = YES
      btn_delete:HIDDEN = NO
      scr-tran-date:SENSITIVE = YES
      scr-tran-date:HIDDEN = NO
      scr-tag:SENSITIVE = YES
      scr-tag:HIDDEN = NO 
      scr-qty-posted:SENSITIVE = YES
      scr-qty-posted:HIDDEN = NO
      scr-qty-posted:SCREEN-VALUE = "0"
      scr-qty-um:SENSITIVE = YES
      scr-qty-um:HIDDEN = NO
      scr-ext-cost:SENSITIVE = YES
      scr-ext-cost:HIDDEN = NO
      scr-ext-cost:SCREEN-VALUE = "0"
      scr-loc:SENSITIVE = YES
      scr-loc:HIDDEN = NO
      scr-bin:SENSITIVE = YES
      scr-bin:HIDDEN = NO.

RUN set-read-only(INPUT YES).

RUN build-table.

APPLY "entry" TO BROWSE {&browse-name}.

{src/adm/template/dialogmn.i}

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
  EMPTY TEMP-TABLE tt-mat-tran.

  for each mat-act WHERE
      mat-act.company eq cocode AND
      mat-act.job-no  eq ip-job-no AND
      mat-act.job-no2 EQ ip-job-no2 AND
      mat-act.s-num   EQ ip-sheet-num AND
      mat-act.b-num   EQ ip-blank-num AND
      mat-act.i-no    EQ ip-rm-i-no
      NO-LOCK:

      create tt-mat-tran.
      assign tt-mat-tran.job-no     = mat-act.job-no
             tt-mat-tran.job-no2    = mat-act.job-no2
             tt-mat-tran.tran-date  = mat-act.mat-date
             tt-mat-tran.sheet-no   = mat-act.s-num
             tt-mat-tran.blank-no   = mat-act.b-num
             tt-mat-tran.i-no       = mat-act.i-no
             tt-mat-tran.qty-posted = mat-act.qty
             tt-mat-tran.ROWID      = ROWID(mat-act)
             tt-mat-tran.tag        = mat-act.tag
             tt-mat-tran.ext-cost   = mat-act.ext-cost
             tt-mat-tran.qty-uom    = mat-act.qty-uom.
      RELEASE tt-mat-tran.
  end.

  OPEN QUERY {&browse-name} FOR EACH tt-mat-tran NO-LOCK.

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
  ENABLE BROWSE-2 
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
  {src/adm/template/snd-list.i "tt-mat-tran"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only D-Dialog 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
      tt-mat-tran.tran-date:read-only IN BROWSE {&browse-name}  = ip-log
      tt-mat-tran.sheet-no:read-only IN BROWSE {&browse-name}   = ip-log
      tt-mat-tran.job-no:read-only IN BROWSE {&browse-name}     = ip-log
      tt-mat-tran.job-no2:read-only IN BROWSE {&browse-name}    = ip-log
      tt-mat-tran.blank-no:read-only IN BROWSE {&browse-name}   = ip-log
      tt-mat-tran.i-no:read-only IN BROWSE {&browse-name}       = ip-log
      tt-mat-tran.qty-posted:read-only IN BROWSE {&browse-name} = ip-log
      tt-mat-tran.tag:READ-ONLY IN BROWSE {&browse-name}        = ip-log
      tt-mat-tran.ext-cost:READ-ONLY IN BROWSE {&browse-name}   = ip-log.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-mat-act D-Dialog 
PROCEDURE update-mat-act :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cOldJobNo LIKE mat-act.job-no NO-UNDO.
  DEF VAR iOldJobNo2 LIKE mat-act.job-no2 NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
       
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    FIND mat-act WHERE ROWID(mat-act) EQ tt-mat-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

    IF AVAILABLE mat-act THEN DO:
       
       ASSIGN
         cOldJobNo = tt-mat-tran.job-no
         iOldJobNo2 = tt-mat-tran.job-no2
         tt-mat-tran.tran-date   = DATE(tt-mat-tran.tran-date:SCREEN-VALUE IN BROWSE {&browse-name})
         tt-mat-tran.job-no      = tt-mat-tran.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
         tt-mat-tran.job-no2     = INT(tt-mat-tran.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
         tt-mat-tran.sheet-no    = INT(tt-mat-tran.sheet-no:SCREEN-VALUE IN BROWSE {&browse-name})
         tt-mat-tran.blank-no    = INT(tt-mat-tran.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
         tt-mat-tran.i-no        = tt-mat-tran.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
         tt-mat-tran.tag         = tt-mat-tran.tag:SCREEN-VALUE IN BROWSE {&browse-name}
         tt-mat-tran.qty-posted  = DEC(tt-mat-tran.qty-posted:SCREEN-VALUE IN BROWSE {&browse-name})
         tt-mat-tran.ext-cost    = DEC(tt-mat-tran.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name})
         mat-act.mat-date = tt-mat-tran.tran-date
         mat-act.job-no   = tt-mat-tran.job-no
         mat-act.job-no2  = tt-mat-tran.job-no2
         mat-act.s-num    = tt-mat-tran.sheet-no
         mat-act.b-num    = tt-mat-tran.blank-no
         mat-act.i-no     = tt-mat-tran.i-no
         mat-act.qty      = tt-mat-tran.qty-posted
         mat-act.tag      = tt-mat-tran.tag
         mat-act.ext-cost = tt-mat-tran.ext-cost.
        
       IF cOldJobNo NE mat-act.job-no OR iOldJobNo2 NE mat-act.job-no2 THEN DO:
           FIND FIRST job 
               WHERE job.company EQ mat-act.company
                 AND job.job-no  EQ mat-act.job-no
                 AND job.job-no2 EQ mat-act.job-no2
           NO-LOCK NO-ERROR.
           IF AVAIL job THEN
               mat-act.job = job.job.
       END.
       RELEASE mat-act.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record D-Dialog 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rowid AS ROWID NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:
       
    lv-rowid = ROWID(tt-mat-tran).

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN update-mat-act.    
    
    RUN set-read-only(INPUT YES).
    
    CLOSE QUERY {&browse-name}.
    OPEN QUERY {&browse-name} FOR EACH tt-mat-tran NO-LOCK.

    REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
    APPLY "ENTRY" TO {&browse-name} IN FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no D-Dialog 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-job-no LIKE oe-ordl.job-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-job-no = TRIM(tt-mat-tran.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-job-no = FILL(" ",6 - LENGTH(lv-job-no)) + lv-job-no
     tt-mat-tran.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = lv-job-no.

    FIND FIRST job-hdr 
          WHERE job-hdr.company  EQ cocode
            AND job-hdr.job-no   EQ lv-job-no
            NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
        MESSAGE TRIM(tt-mat-tran.job-no:LABEL IN BROWSE {&browse-name}) +
              " not Valid, try again..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-mat-tran.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

