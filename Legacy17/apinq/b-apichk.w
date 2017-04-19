&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  apinq\b-apichk.w

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

&SCOPED-DEFINE yellowColumnsName b-apichk
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-report    LIKE report
    FIELD check-no          LIKE ap-pay.check-no
    FIELD check-date        LIKE ap-pay.check-date
    FIELD inv-no            LIKE ap-payl.inv-no
    FIELD due-date          LIKE ap-payl.due-date
    FIELD gross-amt         LIKE ap-payl.amt-paid
    FIELD amt-disc          LIKE ap-payl.amt-disc
    FIELD amt-paid          LIKE ap-payl.amt-paid
    FIELD ap-inv-rec-key AS CHAR
    FIELD vend-no AS CHAR.

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.
DEF BUFFER b-vend FOR vend.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var phandle as widget-handle no-undo.

DEF BUFFER b-ap-inv FOR ap-inv.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-report.check-no tt-report.check-date tt-report.inv-no tt-report.due-date tt-report.gross-amt tt-report.amt-disc tt-report.amt-paid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-report


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table fi_vend fi_fchk fi_finv ~
fi_fdate fi_tchk fi_tinv fi_tdate btn_go btn_print RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_vend fi_name fi_fchk fi_finv fi_fdate ~
fi_tchk fi_tinv fi_tdate fi_sortby 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validDate B-table-Win 
FUNCTION validDate RETURNS LOGICAL
  ( INPUT pdDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_print 
     LABEL "&Print" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_fchk AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Check#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_fdate AS DATE FORMAT "99/99/99" 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_finv AS CHARACTER FORMAT "x(12)" 
     LABEL "From Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_tchk AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To Check#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_tdate AS DATE FORMAT "99/99/99" 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_tinv AS CHARACTER FORMAT "x(12)" INITIAL "zzzzzzzzzzzz" 
     LABEL "To Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 3.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-report.check-no   COLUMN-LABEL "Check#"     FORMAT ">>>>>>>>>>"
      tt-report.check-date COLUMN-LABEL "Chk Date"  LABEL-BGCOLOR 14
      tt-report.inv-no     COLUMN-LABEL "Invoice#"  
      tt-report.due-date   COLUMN-LABEL "Due Date"  LABEL-BGCOLOR 14
      tt-report.gross-amt  COLUMN-LABEL "Gross Amt" LABEL-BGCOLOR 14
      tt-report.amt-disc   COLUMN-LABEL "Discount"  LABEL-BGCOLOR 14
      tt-report.amt-paid   COLUMN-LABEL "Net Amt"   LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 4.86 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_vend AT ROW 1.24 COL 10 COLON-ALIGNED
     fi_name AT ROW 1.24 COL 30 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     fi_fchk AT ROW 2.43 COL 19 COLON-ALIGNED
     fi_finv AT ROW 2.43 COL 59 COLON-ALIGNED
     fi_fdate AT ROW 2.43 COL 100 COLON-ALIGNED WIDGET-ID 2
     fi_tchk AT ROW 3.62 COL 19 COLON-ALIGNED
     fi_tinv AT ROW 3.62 COL 59 COLON-ALIGNED
     fi_tdate AT ROW 3.62 COL 100 COLON-ALIGNED WIDGET-ID 4
     btn_go AT ROW 1.24 COL 121
     btn_print AT ROW 1.24 COL 135
     fi_sortby AT ROW 1.24 COL 83 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.57
         WIDTH              = 148.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK ~{&SORTBY-PHRASE}
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "fi_vend" THEN DO:
      RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE ""                                      AND
         TRIM(FOCUS:SCREEN-VALUE) NE TRIM(ENTRY(1,char-val)) THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-vend.
      END.
    END.  
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  IF AVAILABLE tt-report AND 
     tt-report.ap-inv-rec-key NE "" THEN DO:
      
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                   "(tt-report.ap-inv-rec-key,tt-report.inv-no)"}

     FIND FIRST b-vend WHERE
          b-vend.company EQ cocode AND
          b-vend.vend-no EQ tt-report.vend-no
          NO-LOCK NO-ERROR.

     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.
  ELSE
     RUN pushpin-image-proc(INPUT "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  RUN valid-vend NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_vend
     fi_name
     fi_fchk
     fi_tchk
     fi_finv
     fi_tinv
     fi_fdate
     fi_tdate.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_print B-table-Win
ON CHOOSE OF btn_print IN FRAME F-Main /* Print */
DO:
  {sys/form/r-topw.f}

  DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.

  DEF VAR lv-vend  LIKE vend.vend-no                      NO-UNDO.
  DEF VAR lv-name  LIKE vend.NAME                         NO-UNDO.
  DEF VAR lv-fchk  LIKE ap-pay.check-no FORMAT ">>>>>>>>" NO-UNDO.
  DEF VAR lv-tchk  LIKE lv-fchk                           NO-UNDO.
  DEF VAR lv-finv  LIKE ap-payl.inv-no                    NO-UNDO.
  DEF VAR lv-tinv  LIKE lv-finv                           NO-UNDO.

  FORM SKIP(1)
       lv-vend COLON 30 LABEL "Vendor#"
       lv-name          NO-LABEL
       lv-fchk COLON 30 LABEL "Beginning Check#"
       lv-tchk COLON 30 LABEL "Ending Check#"
       lv-finv COLON 30 LABEL "Beginning Inv#"
       lv-tinv COLON 30 LABEL "Ending Inv#"
       SKIP(1)

      WITH STREAM-IO WIDTH 80 FRAME ap-chkh SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "                 V E N D O R     C H E C K S / I N V O I C E S                ".

  FORM tt-report.check-no    FORMAT ">>>>>>>>"  COLUMN-LABEL "Check#"
       tt-report.check-date  FORMAT "99/99/99"  COLUMN-LABEL "Chk Date"
       tt-report.inv-no                         COLUMN-LABEL "Invoice#"
       tt-report.due-date    FORMAT "99/99/99"  COLUMN-LABEL "Due Date"
       tt-report.gross-amt                      COLUMN-LABEL "Gross Amt"
       tt-report.amt-disc                       COLUMN-LABEL "Discount"
       tt-report.amt-paid                       COLUMN-LABEL "Net Amt"
     
      WITH NO-BOX FRAME ap-chk DOWN WIDTH 80 STREAM-IO.

      
  FIND FIRST tt-report NO-ERROR.

  IF AVAIL tt-report THEN DO WITH FRAME ap-chkh:
    SESSION:SET-WAIT-STATE ("general").
        
    {sys/inc/print1.i}
    {sys/inc/outprint.i 56}

    DISPLAY fi_vend @ lv-vend
            fi_name @ lv-name
            fi_fchk @ lv-fchk
            fi_tchk @ lv-tchk
            fi_finv @ lv-finv
            fi_tinv @ lv-tinv.

    FOR EACH tt-report NO-LOCK
        WITH FRAME ap-chk
        BREAK BY tt-report.key-01
              BY tt-report.key-02:

      IF tt-report.key-03 EQ "TOTAL" THEN DO:
        UNDERLINE tt-report.check-no
                  tt-report.check-date
                  tt-report.gross-amt
                  tt-report.amt-disc
                  tt-report.amt-paid. 
        DOWN.
    
        CLEAR NO-PAUSE.
      END.

      DISPLAY tt-report.check-no
              tt-report.check-date
              tt-report.inv-no
              tt-report.due-date
              tt-report.gross-amt
              tt-report.amt-disc
              tt-report.amt-paid.
      DOWN.

      IF LAST-OF(tt-report.key-02) THEN DOWN 2.
    END.

    OUTPUT CLOSE.

    SESSION:SET-WAIT-STATE ("").

    run scr-rpt.w (list-name,TRIM(FRAME ap-chkh:TITLE),11,"P"). /* open file-name, title */ 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fchk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fchk B-table-Win
ON LEAVE OF fi_fchk IN FRAME F-Main /* From Check# */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fdate B-table-Win
ON LEAVE OF fi_fdate IN FRAME F-Main /* From Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_finv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_finv B-table-Win
ON LEAVE OF fi_finv IN FRAME F-Main /* From Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tchk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tchk B-table-Win
ON LEAVE OF fi_tchk IN FRAME F-Main /* To Check# */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tdate B-table-Win
ON LEAVE OF fi_tdate IN FRAME F-Main /* To Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tinv B-table-Win
ON LEAVE OF fi_tinv IN FRAME F-Main /* To Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
   /* APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON ENTRY OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN RUN new-vend.

  lv-save-char = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON LEAVE OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN new-vend.

    RUN valid-vend NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    /*APPLY "choose" TO btn_go.*/ /* task 01191305 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend B-table-Win
ON VALUE-CHANGED OF fi_vend IN FRAME F-Main /* Vendor# */
DO:
  RUN new-vend.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile B-table-Win 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR t-vendor     LIKE ap-pay.vend-no.
DEF VAR f-chk        LIKE ap-pay.check-no FORMAT ">>>>>>>>" INIT 0.
DEF VAR t-chk        LIKE f-chk INIT 99999999.
DEF VAR f-inv        LIKE ap-payl.inv-no INIT "".
DEF VAR t-inv        LIKE f-inv INIT "zzzzzzzzzzzz".
DEF VAR v-check-no   LIKE ap-pay.check-no.
DEF VAR v-disc-amt   LIKE ap-payl.amt-disc.
DEF VAR v-gross-amt  LIKE ap-payl.amt-paid.

DEF VAR li AS INT NO-UNDO.


ASSIGN
 t-vendor    = fi_vend
 f-inv       = fi_finv
 t-inv       = fi_tinv
 f-chk       = fi_fchk
 t-chk       = fi_tchk.

FOR EACH tt-report:
  DELETE tt-report.
END.

pay-block:
FOR EACH ap-pay
    WHERE ap-pay.company  EQ cocode
      AND ap-pay.vend-no  EQ t-vendor
      AND ap-pay.check-no GE f-chk
      AND ap-pay.check-no LE t-chk
      AND ap-pay.posted   EQ YES
      AND ap-pay.memo     EQ NO
    USE-INDEX vend-no NO-LOCK,
      
    EACH ap-payl
    WHERE ap-payl.c-no    EQ ap-pay.c-no
      AND ap-payl.inv-no  GE f-inv
      AND ap-payl.inv-no  LE t-inv
    NO-LOCK
      
    BREAK BY ap-pay.check-act
          BY ap-pay.check-no 
          BY ap-payl.inv-no  
          BY ap-payl.amt-paid :

    IF NOT validDate(ap-pay.check-date) THEN NEXT pay-block.

    li = li + 1.

    IF FIRST-OF(ap-pay.check-no) THEN v-check-no = ap-pay.check-no.
    
    v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).
    
    IF LAST-OF(ap-payl.inv-no) THEN DO:
        CREATE tt-report.
        ASSIGN
            tt-report.key-01     = ap-pay.check-act
            tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
            tt-report.check-no   = v-check-no
            tt-report.check-date = IF v-check-no NE 0 THEN ap-pay.check-date ELSE ?
                tt-report.inv-no     = ap-payl.inv-no
                tt-report.due-date   = ap-payl.due-date
                tt-report.gross-amt  = ap-payl.amt-paid + ap-payl.amt-disc
                tt-report.amt-disc   = ap-payl.amt-disc
                tt-report.amt-paid   = ap-payl.amt-paid
                tt-report.vend-no = ap-payl.vend-no
                v-disc-amt = v-disc-amt + ap-payl.amt-disc
                v-check-no = 0.

       FIND FIRST b-ap-inv WHERE
            b-ap-inv.company EQ ap-pay.company AND
            b-ap-inv.vend-no EQ ap-payl.vend-no AND
            b-ap-inv.inv-no  EQ ap-payl.inv-no
            NO-LOCK NO-ERROR.

       IF AVAIL b-ap-inv THEN
          tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
    END. /* IF LAST-OF(ap-payl.inv-no) */
            
    IF LAST-OF(ap-pay.check-no) THEN DO:
        IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN DO:
            CREATE tt-report.
            ASSIGN
                tt-report.key-01     = ap-pay.check-act
                tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
                tt-report.key-03     = "TOTAL"
                tt-report.check-no   = ap-pay.check-no
                tt-report.check-date = ap-pay.check-date
                tt-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
                tt-report.due-date   = ap-payl.due-date
                tt-report.gross-amt  = ap-pay.check-amt + v-disc-amt
                tt-report.amt-disc   = v-disc-amt
                tt-report.amt-paid   = ap-pay.check-amt.
        END. /* IF NOT FIRST-OF(ap-pay.check-no) */

        ASSIGN
            v-disc-amt  = 0
            v-gross-amt = 0. 
    END. /* IF LAST-OF(ap-pay.check-no) */
END. /* for each ap-pay */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*   sortBy = NOT sortBy.  */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_vend.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF fi_vend NE "" THEN RUN create-tempfile.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL tt-report AND tt-report.ap-inv-rec-key NE "" THEN
  DO:
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                   "(tt-report.ap-inv-rec-key,tt-report.inv-no)"}

     FIND FIRST b-vend WHERE
          b-vend.company EQ cocode AND
          b-vend.vend-no EQ tt-report.vend-no
          NO-LOCK NO-ERROR.
    
     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.
  ELSE
     RUN pushpin-image-proc(INPUT "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend B-table-Win 
PROCEDURE new-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND vend NO-LOCK
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ fi_vend:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL vend THEN fi_name:SCREEN-VALUE = vend.name.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR v-inv-no AS CHAR NO-UNDO.

   IF AVAIL tt-report THEN
      ASSIGN
         v-inv-no = STRING(tt-report.inv-no,"X(12)") + "APINV"
         v-att = CAN-FIND(FIRST attach WHERE
                 attach.company = cocode and
                 attach.rec_key = ip-rec_key AND
                 (attach.est-no eq v-inv-no OR ATTACH.est-no EQ "")).
   

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachinv-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{methods/setfocus.i {&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN apinq/apck-exp.w ("", "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend B-table-Win 
PROCEDURE valid-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_vend:SCREEN-VALUE = CAPS(fi_vend:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST vend WHERE vend.company EQ cocode
                                 AND vend.vend-no EQ fi_vend:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_vend.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validDate B-table-Win 
FUNCTION validDate RETURNS LOGICAL
  ( INPUT pdDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      /* If no date range was entered, then include check. */
      IF fi_fdate:SCREEN-VALUE = "" AND fi_tdate:SCREEN-VALUE = "" THEN
          RETURN TRUE.

      /* If start date is entered and check date is earlier, then omit. */
      IF fi_fdate:SCREEN-VALUE <> "" AND 
          pdDate < date(fi_fdate:SCREEN-VALUE) THEN
          RETURN FALSE.

      /* If end date is entered and check date is later, then omit. */
      IF fi_tdate:SCREEN-VALUE <> "" AND 
          pdDate > date(fi_tdate:SCREEN-VALUE) THEN
          RETURN FALSE.

  END.

  /* Otherwise include check. */
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

