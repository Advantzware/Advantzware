&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: jcinq\b-boardpo.w

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

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-set-component NO-UNDO
  FIELD i-no AS CHAR
  FIELD form-no AS INT
  FIELD blank-no AS INT
  FIELD first-po-no AS INT
  FIELD sheets-on-order AS INT
  INDEX i-no i-no form-no blank-no.

DEFINE TEMP-TABLE tt-po NO-UNDO
   FIELD i-no AS CHAR
   FIELD s-num AS INT
   FIELD b-num AS INT
   FIELD po-no AS INT
   FIELD ord-qty AS DEC
   FIELD pr-qty-uom AS CHAR
   INDEX po-no po-no.

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
&Scoped-define INTERNAL-TABLES tt-set-component tt-po

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-set-component.form-no tt-set-component.blank-no tt-set-component.i-no tt-set-component.first-po-no tt-set-component.sheets-on-order   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-set-component
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-set-component.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-set-component
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-set-component


/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 tt-po.s-num tt-po.b-num tt-po.i-no tt-po.po-no tt-po.ord-qty tt-po.pr-qty-uom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5   
&Scoped-define SELF-NAME BROWSE-5
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH tt-po
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY {&SELF-NAME} FOR EACH tt-po.
&Scoped-define TABLES-IN-QUERY-BROWSE-5 tt-po
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 tt-po


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-5}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BROWSE-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD poNum D-Dialog 
FUNCTION poNum RETURNS INTEGER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-set-component SCROLLING.

DEFINE QUERY BROWSE-5 FOR 
      tt-po SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-set-component.form-no COLUMN-LABEL "S" FORMAT "ZZ9" WIDTH 7
   tt-set-component.blank-no COLUMN-LABEL "B" FORMAT "ZZ9" WIDTH 7
   tt-set-component.i-no COLUMN-LABEL "RM Item #" FORMAT "X(15)" WIDTH 15
   tt-set-component.first-po-no COLUMN-LABEL "First Board P.O. #" FORMAT "ZZZZZ9" WIDTH 25
   tt-set-component.sheets-on-order COLUMN-LABEL "Sheets on Order" FORMAT "->>>,>>>,>>9" WIDTH 21
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 7.62
         BGCOLOR 8 FONT 2 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 D-Dialog _FREEFORM
  QUERY BROWSE-5 DISPLAY
      tt-po.s-num COLUMN-LABEL "S" FORMAT "ZZ9" WIDTH 7
   tt-po.b-num COLUMN-LABEL "B" FORMAT "ZZ9" WIDTH 7
   tt-po.i-no COLUMN-LABEL "RM Item #" FORMAT "X(15)" WIDTH 15
   tt-po.po-no COLUMN-LABEL "P.O. #" FORMAT "ZZZZZ9" WIDTH 17
   tt-po.ord-qty COLUMN-LABEL "Sheets on Order" FORMAT "->>>,>>>,>>9" WIDTH 21
   tt-po.pr-qty-uom COLUMN-LABEL "UOM" FORMAT "X(4)" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 7.62
         BGCOLOR 8 FONT 2 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 2.43 COL 2
     BROWSE-5 AT ROW 11.24 COL 2 WIDGET-ID 100
     "All P.O. Items" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.29 COL 36 WIDGET-ID 4
          FONT 6
     "Set Components" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.48 COL 36 WIDGET-ID 2
          FONT 6
     SPACE(36.59) SKIP(17.27)
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

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 TEXT-2 D-Dialog */
/* BROWSE-TAB BROWSE-5 BROWSE-2 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-set-component
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-po
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
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
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FRAME {&FRAME-NAME}:TITLE = "P.O. Information for Job : " + ip-job-no + "-" + STRING(ip-job-no2).

SESSION:SET-WAIT-STATE ("general").
RUN build-set-table.
RUN build-po-table.
SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-po-table D-Dialog 
PROCEDURE build-po-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-po.

   FOR EACH po-ordl FIELDS(po-no i-no s-num b-num ord-qty pr-qty-uom) WHERE
       po-ordl.company EQ cocode AND
       po-ordl.job-no EQ ip-job-no AND
       po-ordl.job-no2 EQ ip-job-no2
       NO-LOCK:

       CREATE tt-po.
       ASSIGN
          tt-po.po-no = po-ordl.po-no
          tt-po.i-no  = po-ordl.i-no
          tt-po.s-num = po-ordl.s-num
          tt-po.b-num = po-ordl.b-num
          tt-po.ord-qty = po-ordl.ord-qty
          tt-po.pr-qty-uom = po-ordl.pr-qty-uom.
       RELEASE tt-po.
   END.

   OPEN QUERY browse-5 FOR EACH tt-po NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-set-table D-Dialog 
PROCEDURE build-set-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE tt-set-component.

  DEF VAR v-po-no AS INT NO-UNDO.
  DEF VAR v-ea-qty AS DEC NO-UNDO.
  DEF VAR v-set-est AS LOG NO-UNDO.

  FIND FIRST job WHERE
       job.company EQ cocode AND
       job.job-no EQ ip-job-no AND
       job.job-no2 EQ ip-job-no2
       NO-LOCK NO-ERROR.

  IF AVAIL job THEN
  DO:
     FIND FIRST est WHERE
          est.company EQ cocode AND
          est.est-no EQ job.est-no
          NO-LOCK NO-ERROR.

     IF AVAIL est AND
        (est.est-type EQ 2 OR est.est-type EQ 6) THEN v-set-est = YES.
  END.

  IF v-set-est THEN
  FOR EACH eb WHERE
      eb.company EQ cocode AND
      eb.est-no EQ job.est-no AND
      eb.form-no NE 0
      NO-LOCK:

      v-po-no = poNum().

      CREATE tt-set-component.
      ASSIGN
         tt-set-component.i-no = eb.stock-no
         tt-set-component.form-no = eb.form-no
         tt-set-component.blank-no = eb.blank-no
         tt-set-component.first-po-no = v-po-no.

      job-mat-loop:
      FOR EACH job-mat FIELDS(i-no job-no job-no2 frm blank-no) WHERE
          job-mat.company EQ cocode AND
          job-mat.job-no EQ ip-job-no AND
          job-mat.job-no2 EQ ip-job-no2 AND
          job-mat.frm EQ eb.form-no
          NO-LOCK,
          FIRST ITEM fields(basis-w s-dep) WHERE
                ITEM.company EQ cocode AND
                ITEM.i-no EQ job-mat.i-no AND
                ITEM.mat-type EQ "B"
                NO-LOCK:

          IF NOT((job-mat.blank-no EQ eb.blank-no OR
             job-mat.blank-no EQ 0)) THEN NEXT.

          FOR EACH po-ordl FIELDS(pr-qty-uom s-len s-wid ord-qty s-num b-num) WHERE
              po-ordl.company EQ cocode AND
              po-ordl.i-no EQ job-mat.i-no AND
              po-ordl.po-no EQ v-po-no AND
              po-ordl.job-no EQ job-mat.job-no AND
              po-ordl.job-no2 EQ job-mat.job-no2 AND
              po-ordl.item-type EQ YES
              NO-LOCK:

              IF (po-ordl.s-num EQ job-mat.frm OR
                 po-ordl.s-num EQ ?) AND
                 (po-ordl.b-num EQ job-mat.blank-no OR
                  po-ordl.b-num EQ 0) THEN
                 DO:
                    IF po-ordl.pr-qty-uom NE "EA" THEN
                       RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                              "EA",
                                              item.basis-w,
                                              po-ordl.s-len,
                                              po-ordl.s-wid,
                                              item.s-dep,
                                              po-ordl.ord-qty,
                                              OUTPUT v-ea-qty).
                    ELSE
                       v-ea-qty = po-ordl.ord-qty.

                    {sys/inc/roundup.i v-ea-qty}

                    tt-set-component.sheets-on-order = tt-set-component.sheets-on-order
                                                     + v-ea-qty.
                 END.
          END. /*each po-ordl*/

          RELEASE tt-set-component.

          LEAVE job-mat-loop.
      END.
  END. /*end eb*/

  OPEN QUERY browse-2 FOR EACH tt-set-component NO-LOCK.

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
  ENABLE BROWSE-2 BROWSE-5 
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
  {src/adm/template/snd-list.i "tt-po"}
  {src/adm/template/snd-list.i "tt-set-component"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION poNum D-Dialog 
FUNCTION poNum RETURNS INTEGER
  ( ) :

   DEFINE VARIABLE rtnValue2 AS INT NO-UNDO.

   main-loop:
   FOR EACH job-mat FIELDS(i-no job-no job-no2 frm blank-no) WHERE
       job-mat.company EQ cocode AND
       job-mat.job-no EQ ip-job-no AND
       job-mat.job-no2 EQ ip-job-no2 AND
       job-mat.frm EQ eb.form-no AND
       (job-mat.blank-no EQ eb.blank-no OR
        job-mat.blank-no EQ 0)
       NO-LOCK,
       FIRST ITEM WHERE
             ITEM.company EQ cocode AND
             ITEM.i-no EQ job-mat.i-no AND
             ITEM.mat-type EQ "B"
             NO-LOCK:

       FOR EACH po-ordl FIELDS(s-num b-num po-no) WHERE
           po-ordl.company EQ cocode AND
           po-ordl.i-no EQ job-mat.i-no AND
           po-ordl.job-no EQ job-mat.job-no AND
           po-ordl.job-no2 EQ job-mat.job-no2 AND
           po-ordl.item-type EQ YES
           NO-LOCK
           BY po-ordl.po-no:

           IF (po-ordl.s-num EQ job-mat.frm OR
              po-ordl.s-num EQ ?) AND
              (po-ordl.b-num EQ job-mat.blank-no OR
               po-ordl.b-num EQ 0) THEN
              DO:
                 rtnValue2 = po-ordl.po-no.
                 LEAVE main-loop.
              END.
       END.
   END.

   RETURN rtnValue2.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

