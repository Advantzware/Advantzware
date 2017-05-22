&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: ap/w-seldue.w

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
&SCOPED-DEFINE yellowColumnsName w-seldue

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/prgsecur.i}               
{methods/defines/hndldefs.i}               
{sys/inc/VAR.i NEW SHARED}

DEF TEMP-TABLE tt-sel NO-UNDO LIKE ap-sel
           FIELD amt-due AS DEC FORM "->>,>>>,>>9.99" 
           FIELD inv-date LIKE ap-inv.inv-date
           FIELD tt-rowid AS ROWID
           FIELD tt-deleted AS LOG
           FIELD dsc-date LIKE ap-inv.inv-date.
                        .
DEF TEMP-TABLE tt-del-list
    FIELD tt-row AS ROWID.

ASSIGN cocode = g_company
       locode = g_loc.

DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */

DEF VAR v-show-disc AS LOG NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR lv-pre-disc AS DEC NO-UNDO.
DEF VAR lv-pre-paid AS DEC NO-UNDO.

DEF VAR lv-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-num-rec AS INT NO-UNDO.
DEF VAR lv-in-add AS LOG NO-UNDO.
DEF VAR lv-in-update AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sel ap-pay

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-sel.vend-no tt-sel.inv-no tt-sel.inv-date tt-sel.due-date tt-sel.dsc-date tt-sel.inv-bal tt-sel.amt-due tt-sel.disc-amt tt-sel.amt-paid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-sel.vend-no tt-sel.inv-no tt-sel.disc-amt tt-sel.amt-paid   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-sel WHERE tt-sel.tt-deleted = NO ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-sel WHERE tt-sel.tt-deleted = NO ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-sel


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}
&Scoped-define QUERY-STRING-F-Main FOR EACH ap-pay SHARE-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH ap-pay SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main ap-pay
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main ap-pay


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_chk-date fi_curr-code fi_due-date fi_age ~
lv-proamt tb_discount tb_cc btn-go BROWSE-1 btn-change btn-add btn-delete ~
btn-finish tb_cc-only tb_bp-only RECT-9 
&Scoped-Define DISPLAYED-OBJECTS fi_chk-date fi_curr-code c-desc ~
fi_due-date fi_age fi_amt lv-proamt tb_discount tb_cc fi_sortBy tb_cc-only ~
tb_bp-only 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi_chk-date fi_curr-code fi_due-date fi_age fi_amt ~
tb_discount tb_cc tb_cc-only tb_bp-only 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckForNegative W-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
  ( ipcCompany AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
     LABEL "Add" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-change 
     LABEL "Update" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-finish 
     LABEL "Finish" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-go 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE c-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_age AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "and next" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fi_amt AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_chk-date AS DATE FORMAT "99/99/9999":U 
     LABEL "AP Check Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fi_curr-code AS CHARACTER FORMAT "X(3)" 
     LABEL "Currency Code" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE fi_due-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Desired Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortBy AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 42.4 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-proamt AS DECIMAL FORMAT "$->>>,>>>,>>9.99":U INITIAL ? 
     LABEL "Projected Cash Balance" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 5.24.

DEFINE VARIABLE tb_bp-only AS LOGICAL INITIAL no 
     LABEL "Bill Pay Vendors" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cc AS LOGICAL INITIAL no 
     LABEL "Exclude Credit Card/ACH and Bill Pay Vendors" 
     VIEW-AS TOGGLE-BOX
     SIZE 61 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cc-only AS LOGICAL INITIAL no 
     LABEL "Credit Card/ACH Vendors" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .95 NO-UNDO.

DEFINE VARIABLE tb_discount AS LOGICAL INITIAL no 
     LABEL "Include Invoices Available for Discount" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-sel SCROLLING.

DEFINE QUERY F-Main FOR 
      ap-pay SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-sel.vend-no LABEL "Vendor" LABEL-BGCOLOR 14
    tt-sel.inv-no LABEL "Invoice Num" LABEL-BGCOLOR 14
    tt-sel.inv-date LABEL "Invoice Date" LABEL-BGCOLOR 14
    tt-sel.due-date LABEL "Due Date" LABEL-BGCOLOR 14
    tt-sel.dsc-date LABEL "Disc Date" LABEL-BGCOLOR 14
    tt-sel.inv-bal LABEL "Invoice Amt" LABEL-BGCOLOR 14
    tt-sel.amt-due LABEL "Balance Due" LABEL-BGCOLOR 14
    tt-sel.disc-amt LABEL "Discount" LABEL-BGCOLOR 14
    tt-sel.amt-paid LABEL "To Be Paid" LABEL-BGCOLOR 14
    ENABLE tt-sel.vend-no tt-sel.inv-no tt-sel.disc-amt tt-sel.amt-paid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 149 BY 16.67
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_chk-date AT ROW 1.48 COL 24 COLON-ALIGNED
     fi_curr-code AT ROW 1.48 COL 60 COLON-ALIGNED
     c-desc AT ROW 1.48 COL 69 COLON-ALIGNED NO-LABEL
     fi_due-date AT ROW 2.67 COL 24 COLON-ALIGNED
     fi_age AT ROW 2.67 COL 60 COLON-ALIGNED
     fi_amt AT ROW 2.71 COL 97 COLON-ALIGNED
     lv-proamt AT ROW 1.48 COL 118.8 COLON-ALIGNED
     tb_discount AT ROW 3.86 COL 5.8 WIDGET-ID 14
     tb_cc AT ROW 4.91 COL 65 WIDGET-ID 18
     btn-go AT ROW 2.67 COL 129
     BROWSE-1 AT ROW 6.24 COL 1
     fi_sortBy AT ROW 23.14 COL 14 COLON-ALIGNED WIDGET-ID 12
     btn-change AT ROW 23.14 COL 61.2
     btn-add AT ROW 23.14 COL 76.6 WIDGET-ID 16
     btn-cancel AT ROW 23.14 COL 92.6
     btn-delete AT ROW 23.14 COL 108.6
     btn-finish AT ROW 23.14 COL 129.6
     tb_cc-only AT ROW 3.86 COL 65 WIDGET-ID 20
     tb_bp-only AT ROW 3.86 COL 106 WIDGET-ID 22
     "Days" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.67 COL 72
     RECT-9 AT ROW 1 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 149 BY 23.71
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Payment Selection by Due Date"
         HEIGHT             = 23.71
         WIDTH              = 149
         MAX-HEIGHT         = 53.71
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.71
         VIRTUAL-WIDTH      = 384
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 btn-go F-Main */
ASSIGN 
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btn-cancel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_age IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_amt IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_chk-date IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_curr-code IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_due-date IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_bp-only IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_cc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_cc-only IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_discount IN FRAME F-Main
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-sel WHERE tt-sel.tt-deleted = NO ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "asi.ap-pay"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Payment Selection by Due Date */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Payment Selection by Due Date */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  /*  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
  */
     APPLY "choose" TO btn-finish IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main W-Win
ON HELP OF FRAME F-Main
DO:
  DEF VAR lw-focus AS HANDLE NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO.


  lw-focus = FOCUS.

  CASE FOCUS:NAME:
    WHEN "" THEN DO: 
    END.

    OTHERWISE DO:
      RUN applhelp.p.

      IF g_lookup-var NE ""                                AND
         TRIM(g_lookup-var) NE TRIM(lw-focus:SCREEN-VALUE) THEN DO:
        lw-focus:SCREEN-VALUE = g_lookup-var.
        APPLY "value-changed" TO lw-focus.
        APPLY "entry" TO lw-focus.
      END.
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON return OF BROWSE-1 IN FRAME F-Main
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-ENTRY OF BROWSE-1 IN FRAME F-Main
DO:
    IF NOT lv-in-update and NOT lv-in-add THEN 
           ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
                  tt-sel.vend-no:READ-ONLY = YES
                  /*tt-sel.due-date:READ-ONLY = YES
                  tt-sel.inv-bal:READ-ONLY = YES
                  tt-sel.amt-due:READ-ONLY = YES*/
                  tt-sel.disc-amt:READ-ONLY = YES
                  tt-sel.amt-paid:READ-ONLY = YES.

      /*  ELSE IF lv-in-update THEN  ASSIGN  tt-sel.disc-amt:READ-ONLY = NO
                                         tt-sel.amt-due:READ-ONLY = no
                                         tt-sel.amt-paid:READ-ONLY = no
                                         .
      ELSE IF lv-in-add THEN ASSIGN tt-sel.inv-no:READ-ONLY = NO
                    tt-sel.due-date:READ-ONLY = NO
                    tt-sel.inv-bal:READ-ONLY = NO
                    tt-sel.disc-amt:READ-ONLY = NO
                    tt-sel.amt-due:READ-ONLY = NO
                    tt-sel.amt-paid:READ-ONLY = NO
                    .     
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON START-SEARCH OF BROWSE-1 IN FRAME F-Main
DO:
  RUN startSearch.  /*
    DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by = lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ASSIGN
     lv-sort-by     = lv-column-nam.
 /*    lv-sort-by-lab = lv-column-lab.
*/


  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "choose" TO btn-inq. */
  CASE lv-column-nam:
      WHEN "actnum" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum {&sortby-des}.           
      END.
      WHEN "tr-date" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date {&sortby-des}.           
      END.
      WHEN "jrnl" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl {&sortby-des}.
      END.
      WHEN "tr-dscr" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr {&sortby-des}.
      END.
      WHEN "tr-amt" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-amt.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-amt {&sortby-des}.
      END.
  END CASE.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add W-Win
ON CHOOSE OF btn-add IN FRAME F-Main /* Add */
DO:
   IF {&browse-name}:NUM-SELECTED-ROWS = 0 THEN
        {&browse-name}:SELECT-FOCUSED-ROW().
   ASSIGN
   lv-in-add = YES
   tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = NO
   tt-sel.vend-no:READ-ONLY = NO
   /*tt-sel.due-date:READ-ONLY = NO
   tt-sel.inv-bal:READ-ONLY = NO 
   tt-sel.amt-due:READ-ONLY = NO*/
   tt-sel.disc-amt:READ-ONLY = NO
   tt-sel.amt-paid:READ-ONLY = NO.

   BROWSE {&browse-name}:INSERT-ROW("after").
   APPLY "entry" TO tt-sel.vend-no IN BROWSE {&browse-name}.
   btn-change:LABEL = "Save".
   ENABLE btn-change btn-cancel WITH FRAME {&FRAME-NAME}.
   DISABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.

   ASSIGN
      lv-first = no
      lv-in-update = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel W-Win
ON CHOOSE OF btn-cancel IN FRAME F-Main /* Cancel */
DO:
  IF AVAIL tt-sel THEN DO:
      tt-sel.disc-amt = lv-pre-disc.
      ASSIGN tt-sel.amt-due = (tt-sel.inv-bal - tt-sel.disc-amt)
             tt-sel.amt-paid = tt-sel.amt-due.

      DISPLAY tt-sel.disc-amt tt-sel.amt-due tt-sel.amt-paid WITH BROWSE {&browse-name}
              .    
      ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
             tt-sel.vend-no:READ-ONLY = YES
             /*tt-sel.due-date:READ-ONLY = YES
             tt-sel.inv-bal:READ-ONLY = YES
             tt-sel.amt-due:READ-ONLY = YES*/
             tt-sel.disc-amt:READ-ONLY = NO                 
             tt-sel.amt-paid:READ-ONLY = NO.

  END.
  ELSE DO:  /* add and cancel */
     IF lv-in-add THEN BROWSE {&browse-name}:DELETE-current-ROW().

  END.
  btn-change:LABEL = "Update".
  ENABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
  DISABLE btn-cancel WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-change
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-change W-Win
ON CHOOSE OF btn-change IN FRAME F-Main /* Update */
DO:
/*     IF AVAIL tt-sel THEN DO: */
       DEF BUFFER bf-tsel FOR tt-sel.

       IF SELF:LABEL = "Save" THEN DO:    
          fi_amt = 0.
          FOR EACH bf-tsel:
               fi_amt = fi_amt + bf-tsel.amt-paid.
          END.
          FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND ap-inv.posted  = YES
                        AND ap-inv.inv-no = (tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} ) NO-LOCK NO-ERROR.

          IF NOT AVAIL ap-inv THEN DO:
             MESSAGE "Invalid Invoice Number. Try Help. " VIEW-AS ALERT-BOX.
             APPLY "entry" TO tt-sel.inv-no .
             RETURN NO-APPLY.
          END.
          IF lv-in-add THEN DO:
             FIND FIRST ap-sel WHERE ap-sel.company = g_company
                        AND ap-sel.vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND ap-sel.man-check = NO
                        AND ap-sel.inv-no = tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.

             IF AVAIL ap-sel THEN DO:
                MESSAGE "The INVOICE NUMBER you entered has already been selected, please re-enter."
                       VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO tt-sel.inv-no.
                RETURN NO-APPLY.
             END.
          END.   

          IF dec(tt-sel.amt-paid:SCREEN-VALUE) + dec(tt-sel.disc-amt:SCREEN-VALUE) > dec(tt-sel.inv-bal:SCREEN-VALUE) THEN DO:
             MESSAGE "Over payment is not allowed!" VIEW-AS ALERT-BOX ERROR.
             APPLY "entry" TO tt-sel.amt-paid IN BROWSE {&browse-name}.
             RETURN NO-APPLY.
          END.

          lv-pre-paid = DEC(tt-sel.amt-paid:SCREEN-VALUE).
          FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND ap-inv.inv-no = (tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} ) 
                        NO-LOCK NO-ERROR.
          IF lv-in-add THEN DO:
             CREATE tt-sel.
             ASSIGN tt-sel.company = g_company
                    tt-sel.vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    tt-sel.inv-no = tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}
             /* need inv-no lookup and find ap-inv */                    
                    tt-sel.inv-bal = IF AVAIL ap-inv THEN ap-inv.due ELSE 0
                    tt-sel.due-date = IF AVAIL ap-inv THEN ap-inv.due-date ELSE ?

                    .
            /* need to create ap-sel */
          END.
          fi_amt = fi_amt - lv-pre-paid.
    /*   do when post
         find first ap-inv WHERE ap-inv.company = g_company                                                  
                              and ap-inv.vend-no eq tt-sel.vend-no
                              and ap-inv.inv-no  eq tt-sel.inv-no no-error.

          ASSIGN ap-inv.paid = ap-inv.paid - lv-pre-paid
                 ap-inv.due = ap-inv.due + lv-pre-paid + lv-pre-disc
                 .
*/
          ASSIGN tt-sel.disc-amt = dec(tt-sel.disc-amt:SCREEN-VALUE IN BROWSE {&browse-name})
              tt-sel.amt-due = dec(tt-sel.amt-due:SCREEN-VALUE)
              tt-sel.amt-paid = dec(tt-sel.amt-paid:SCREEN-VALUE).
          fi_amt = fi_amt + tt-sel.amt-paid.
 /*
          ASSIGN ap-inv.paid = ap-inv.paid + tt-sel.amt-paid
                 ap-inv.due = ap-inv.due - tt-sel.amt-paid - tt-sel.disc-amt */
           fi_amt = 0.
           FOR EACH bf-tsel:
                fi_amt = fi_amt + bf-tsel.amt-paid.
           END.

          DISPLAY fi_amt WITH FRAME {&FRAME-NAME}.
          ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
                 tt-sel.vend-no:READ-ONLY = YES
                 /*tt-sel.due-date:READ-ONLY = YES
                 tt-sel.inv-bal:READ-ONLY = YES
                 tt-sel.amt-due:READ-ONLY = YES*/
                 tt-sel.disc-amt:READ-ONLY = NO                 
                 tt-sel.amt-paid:READ-ONLY = NO.
          SELF:LABEL = "Update".
          disable btn-cancel WITH FRAME {&FRAME-NAME}.
          ENABLE btn-change btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
          ASSIGN
             lv-in-update = NO
             lv-in-add = NO.
       END.
       ELSE DO: 
           SELF:LABEL = "Save".
           ASSIGN
             lv-in-update = YES
             lv-in-add = NO
             tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
             tt-sel.vend-no:READ-ONLY = YES
             /*tt-sel.due-date:READ-ONLY = YES
             tt-sel.inv-bal:READ-ONLY = YES
             tt-sel.amt-due:READ-ONLY = YES*/
             tt-sel.disc-amt:READ-ONLY = NO                 
             tt-sel.amt-paid:READ-ONLY = NO.
          ENABLE btn-change btn-cancel WITH FRAME {&FRAME-NAME}.
          DISABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
          APPLY "entry" TO tt-sel.disc-amt IN BROWSE {&browse-name}.
       END.
/*     END. */

       FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.     /*Task# 01091401*/
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - fi_amt) .
          END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete W-Win
ON CHOOSE OF btn-delete IN FRAME F-Main /* Delete */
DO:
/*     DEF VAR ll-dum AS LOG NO-UNDO.                                          */
/*  IF AVAIL tt-sel THEN DO WITH FRAME {&FRAME-NAME}:                          */
/*     message "Delete Currently Selected Record?" view-as alert-box question  */
/*           button yes-no update ll-ans as log.                               */
/*     if not ll-ans then return .                                             */
/*                                                                             */
/*     tt-sel.tt-deleted = YES.                                                */
/*     fi_amt = DEC(fi_amt:SCREEN-VALUE) - tt-sel.amt-paid.                    */
/*     DISPLAY fi_amt.                                                         */
/*     FIND ap-sel WHERE ROWID(ap-sel) = tt-sel.tt-rowid NO-ERROR.             */
/*     IF AVAIL ap-sel THEN do:                                                */
/*         DELETE ap-sel.                                                      */
/*     END.                                                                    */
/*     DELETE tt-sel.                                                          */
/*     ASSIGN                                                                  */
/*        ll-dum = BROWSE {&browse-name}:DELETE-CURRENT-ROW()                  */
/*        lv-num-rec = lv-num-rec - 1.                                         */
/*     IF NUM-RESULTS("{&browse-name}") < 1 THEN                               */
/*          DISABLE btn-change btn-cancel btn-delete WITH FRAME {&FRAME-NAME}. */
/*  END.                                                                       */
  DEF VAR ll-dum AS LOG NO-UNDO.
  DEF VAR cQuery AS CHAR NO-UNDO.
  DEF VAR iNumSelected AS INT NO-UNDO.
  DEF VAR li AS INT  NO-UNDO.
  DEF VAR llNeedRedisplay AS LOG NO-UNDO.
  DEF VAR iLastRow AS INT NO-UNDO.

  llNeedRedisplay = NO.
  li = 0.
  IF {&browse-name}:NUM-SELECTED-ROWS EQ 0 THEN
  DO:
     MESSAGE "No Selected Records to Delete."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     RETURN.
  END.
  IF AVAIL tt-sel THEN DO WITH FRAME {&FRAME-NAME}:

      MESSAGE "Delete Currently Selected Record(s)?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll-ans AS LOG.

      IF NOT ll-ans THEN RETURN.
      EMPTY TEMP-TABLE tt-del-list.

      iLastRow = 0.
      /* Grab selected rows and add to a list */
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
          {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

         IF QUERY browse-1:CURRENT-RESULT-ROW GT iLastRow THEN
             iLastRow = QUERY browse-1:CURRENT-RESULT-ROW.

         FIND FIRST tt-del-list WHERE tt-del-list.tt-row EQ ROWID(tt-sel)
              NO-ERROR.
         IF NOT AVAIL tt-del-list THEN DO:      
             CREATE tt-del-list.
             ASSIGN tt-del-list.tt-row = ROWID(tt-sel).
         END.

      END. /* do li to numresults */


      /* Process those on list */
      FOR EACH tt-del-list:
          FIND tt-sel WHERE ROWID(tt-sel) EQ tt-del-list.tt-row NO-ERROR.
          IF AVAIL tt-sel THEN DO:
              tt-sel.tt-deleted = YES.
              FIND ap-sel WHERE ROWID(ap-sel) EQ tt-sel.tt-rowid NO-ERROR.
              IF AVAIL ap-sel THEN DO:
                  fi_amt:SCREEN-VALUE = STRING(DEC(fi_amt:SCREEN-VALUE) - tt-sel.amt-paid).
                  DISPLAY fi_amt.
                  DELETE ap-sel.
              END. /* if avail ap-sel */
              DELETE tt-sel.
              llNeedRedisplay = YES.
          END.
      END. /* each tt-del-list */

  END. /* If avail tt-sel */

  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.      /*Task# 01091401*/
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - fi_amt) .
          END.

  /* Since tt-sel records were deleted, reopen query */
/*   IF llNeedRedisplay THEN DO:                        */
/*       /* To prevent display error reopening query */ */
/*       cQuery = "{&QUERY-STRING-BROWSE-1}".           */
/*       QUERY browse-1:QUERY-PREPARE(cQuery) .         */
/*       QUERY BROWSE-1:QUERY-OPEN().                   */
/*                                                      */
/*   END.                                               */

  RUN openQuery.
  QUERY browse-1:REPOSITION-TO-ROW ( iLastRow ).

  IF NUM-RESULTS("{&browse-name}") < 1 THEN
           DISABLE btn-change btn-cancel btn-delete WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-finish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-finish W-Win
ON CHOOSE OF btn-finish IN FRAME F-Main /* Finish */
DO:
    FOR EACH tt-sel:
        FIND ap-sel WHERE ROWID(ap-sel) EQ tt-sel.tt-rowid NO-ERROR.
        IF NOT AVAIL ap-sel THEN DO:
            CREATE ap-sel.
            BUFFER-COPY tt-sel /*EXCEPT tt-recid tt-sel.amt-due*/ TO ap-sel.
        END.
        IF tt-sel.tt-deleted THEN DELETE ap-sel.
        ASSIGN
            ap-sel.amt-paid = tt-sel.amt-paid
            ap-sel.disc-amt = tt-sel.disc-amt.
        FIND CURRENT ap-sel NO-LOCK.
    END.
    /*SET Up AP-CHK*/
    FOR EACH ap-chk
        WHERE ap-chk.company   EQ cocode
          AND ap-chk.man-check EQ no:
        ap-chk.check-amt = 0.
    END.
    FOR EACH ap-sel
        WHERE ap-sel.company   EQ cocode
          and ap-sel.man-check EQ NO NO-LOCK:
        FIND FIRST ap-chk
            WHERE ap-chk.company   EQ cocode
              AND ap-chk.vend-no   EQ ap-sel.vend-no
              AND ap-chk.man-check EQ NO
            NO-ERROR.
        IF NOT AVAIL ap-chk THEN DO:
            CREATE ap-chk.
            ASSIGN 
                ap-chk.company   = cocode
                ap-chk.vend-no   = ap-sel.vend-no
                ap-chk.check-no  = ?
                ap-chk.man-check = NO.

        END.
        ap-chk.check-amt = ap-chk.check-amt + ap-sel.amt-paid.
        FIND CURRENT ap-chk NO-LOCK.
    END.
    IF NOT CheckForNegative(INPUT cocode) THEN
        APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go W-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
DO:
  RUN valid-fi_curr-code (fi_curr-code:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  ASSIGN {&list-1}.

  RUN ap/d-paysel.w (INPUT fi_due-date, 
                     INPUT fi_age, 
                     INPUT tb_discount, 
                     INPUT tb_cc, 
                     INPUT tb_cc-only,
                     INPUT tb_bp-only,
                     OUTPUT v-show-disc, 
                     OUTPUT choice).

  IF choice THEN DO:
    SESSION:SET-WAIT-STATE("general").
    RUN create-apsel.
    RUN build-table.
    SESSION:SET-WAIT-STATE("").

    IF CAN-FIND(FIRST tt-sel) THEN DO:
      {&open-query-{&browse-name}}

      DISABLE fi_chk-date fi_due-date fi_curr-code fi_age fi_amt lv-proamt btn-go tb_discount tb_cc tb_cc-only tb_bp-only
          WITH FRAME {&FRAME-NAME}.      

      APPLY "entry" TO btn-change IN FRAME {&FRAME-NAME}.  
      APPLY "entry" TO browse-1 IN FRAME {&FRAME-NAME}.  

    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_curr-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_curr-code W-Win
ON LEAVE OF fi_curr-code IN FRAME F-Main /* Currency Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_curr-code (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_curr-code W-Win
ON VALUE-CHANGED OF fi_curr-code IN FRAME F-Main /* Currency Code */
DO:
  FIND FIRST currency NO-LOCK
      WHERE currency.company EQ cocode
        AND currency.c-code  EQ {&self-name}:SCREEN-VALUE
      NO-ERROR.
  IF AVAIL currency THEN c-desc:SCREEN-VALUE = currency.c-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_bp-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_bp-only W-Win
ON VALUE-CHANGED OF tb_bp-only IN FRAME F-Main /* Bill Pay Vendors */
DO:
  ASSIGN {&self-name}.
  IF tb_bp-only THEN
      ASSIGN 
        tb_cc-only = NO
        tb_cc-only:SCREEN-VALUE = "NO"
        tb_cc = NO
        tb_cc:SCREEN-VALUE = "NO".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cc W-Win
ON VALUE-CHANGED OF tb_cc IN FRAME F-Main /* Exclude Credit Card/ACH and Bill Pay Vendors */
DO:
  ASSIGN {&self-name}.
  IF tb_cc THEN
      ASSIGN 
        tb_cc-only = NO
        tb_cc-only:SCREEN-VALUE = "NO"
        tb_bp-only = NO
        tb_bp-only:SCREEN-VALUE = "NO"
       .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cc-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cc-only W-Win
ON VALUE-CHANGED OF tb_cc-only IN FRAME F-Main /* Credit Card/ACH Vendors */
DO:
  ASSIGN {&self-name}.
  IF tb_cc-only THEN
      ASSIGN 
        tb_bp-only = NO
        tb_bp-only:SCREEN-VALUE = "NO"
        tb_cc = NO
        tb_cc:SCREEN-VALUE = "NO".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_discount W-Win
ON VALUE-CHANGED OF tb_discount IN FRAME F-Main /* Include Invoices Available for Discount */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
{custom/yellowColumns.i}
SESSION:DATA-ENTRY-RETURN = YES.

/* DO WITH FRAME {&FRAME-NAME}:       */
/*       fi_disc-date:SENSITIVE = NO. */
/*       fi_discdays:SENSITIVE = NO.  */
/* /*       TEXT-2:SENSITIVE = NO. */ */
/* END.                               */
ON 'leave':U OF tt-sel.inv-no IN BROWSE {&browse-name} 
DO:    
  DEF VAR ld-tot-line AS DEC NO-UNDO.
  DEF VAR ld-non-disc AS DEC NO-UNDO.
  DEF VAR lc-vend-no AS CHAR NO-UNDO.
  DEF VAR lc-inv-no AS CHAR NO-UNDO.

    ASSIGN 
        lc-vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
        lc-inv-no = tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF LASTKEY = -1 THEN RETURN.
    IF lc-inv-no EQ "" THEN DO:
        MESSAGE "Invoice number required." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.      
    IF lc-vend-no EQ "" THEN DO:
        MESSAGE "Vendor required." VIEW-AS ALERT-BOX.
        APPLY "entry" TO tt-sel.vend-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
    END.
    FIND FIRST ap-inv WHERE ap-inv.company = g_company 
                        AND ap-inv.posted  = YES
                        AND ap-inv.vend-no = lc-vend-no
                        AND ap-inv.inv-no = lc-inv-no NO-LOCK NO-ERROR.
    IF NOT AVAIL ap-inv THEN DO:
       MESSAGE "Invalid invoice number. Try help. " VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    IF lv-in-add THEN DO:
       FIND FIRST ap-sel WHERE ap-sel.company = g_company
                        AND ap-sel.man-check = NO
                        AND ap-sel.vend-no = lc-vend-no
                        AND ap-sel.inv-no = lc-inv-no NO-LOCK NO-ERROR.
       IF AVAIL ap-sel THEN DO:
          MESSAGE "The invoice number you entered has already been selected for this vendor, please re-enter."
                 VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-sel.disc-amt:SCREEN-VALUE = "0.00"
              tt-sel.amt-due:SCREEN-VALUE = string(ap-inv.due).
    END.   

    ASSIGN tt-sel.due-date:SCREEN-VALUE IN BROWSE {&browse-name} = string(ap-inv.due-date,"99/99/9999")
           tt-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)
           tt-sel.vend:SCREEN-VALUE = STRING(ap-inv.vend)
           tt-sel.inv-date:SCREEN-VALUE IN BROWSE {&browse-name} = string(ap-inv.inv-date,"99/99/9999") 
           tt-sel.dsc-date:SCREEN-VALUE IN BROWSE {&browse-name} = string(ap-inv.inv-date + ap-inv.disc-days ,"99/99/9999") 
           ld-tot-line = 0
           ld-non-disc = 0.

    FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK:
        ld-tot-line = ld-tot-line + ap-invl.amt.

        FIND FIRST account
            WHERE account.company EQ g_company
              AND account.actnum  EQ ap-invl.actnum
            NO-LOCK NO-ERROR.
        IF AVAIL account AND ap-invl.actnum NE "" THEN DO:
          FIND FIRST reftable
              WHERE reftable.reftable EQ "GLACCTDISC"
                AND reftable.company  EQ g_company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ account.actnum
              NO-LOCK NO-ERROR.
          IF AVAIL reftable AND reftable.val[1] EQ 1 THEN
            ld-non-disc = ld-non-disc + ap-invl.amt.
        END.
    END.

    ASSIGN
     ld-non-disc = IF ld-non-disc GE 0 THEN
                     (ld-non-disc * (ap-inv.net / ld-tot-line))
                   ELSE 0
     ld-tot-line = ap-inv.net - ld-non-disc.

    IF (TODAY - ap-inv.inv-date) <= ap-inv.disc-days AND ap-inv.due GT 0 THEN
          tt-sel.disc-amt:SCREEN-VALUE = STRING(ROUND((ld-tot-line * ap-inv.disc-% / 100),2) ).
    tt-sel.amt-paid:SCREEN-VALUE = string(ap-inv.due - DEC(tt-sel.disc-amt:SCREEN-VALUE)).
    fi_amt = fi_amt + dec(tt-sel.amt-paid:SCREEN-VALUE).

    FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
          IF AVAIL ap-ctrl THEN DO:                                             /*Task# 01091401*/
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN do:
                  DO WITH FRAME {&FRAME-NAME}:
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - fi_amt) .
                  END.
              END.
          END.
END.
ON 'leave':U OF tt-sel.vend-no IN BROWSE {&browse-name} 
DO:    
  DEF VAR ld-tot-line AS DEC NO-UNDO.
  DEF VAR ld-non-disc AS DEC NO-UNDO.
  DEF VAR lc-vend-no AS CHAR NO-UNDO.
  DEF VAR lc-inv-no AS CHAR NO-UNDO.

    ASSIGN 
        lc-vend-no = tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}
        lc-inv-no = tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF LASTKEY = -1 THEN RETURN.
    IF lc-vend-no EQ "" THEN DO:
        MESSAGE "Vendor required." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.        
    FIND FIRST vend WHERE vend.company = g_company
                          and vend.vend-no EQ lc-vend-no 
                          use-index vend no-lock no-error.
    IF NOT AVAIL vend THEN DO:
       MESSAGE "Invalid Vendor. Try Help. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    IF lc-inv-no EQ "" THEN DO:
        APPLY "entry" TO tt-sel.inv-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY. 
    END.
    FIND FIRST ap-inv WHERE ap-inv.company = g_company 
                        AND ap-inv.posted  = YES
                        AND ap-inv.inv-no = lc-inv-no 
                        AND ap-inv.vend-no = lc-vend-no NO-LOCK NO-ERROR.
    IF NOT AVAIL ap-inv THEN DO:
        MESSAGE "Invalid Vendor for this invoice number. Try Help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO tt-sel.inv-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.                                                  
    END.   
    IF lv-in-add THEN DO:
       FIND FIRST ap-sel WHERE ap-sel.company = g_company
                        AND ap-sel.man-check = NO
                        AND ap-sel.inv-no = lc-inv-no 
                        AND ap-sel.vend-no = lc-vend-no NO-LOCK NO-ERROR.
       IF AVAIL ap-sel THEN DO:
          MESSAGE "The INVOICE NUMBER you entered has already been selected for this vendor, please re-enter."
                 VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-sel.disc-amt:SCREEN-VALUE = "0.00"
              tt-sel.amt-due:SCREEN-VALUE = string(ap-inv.due).
    END.   

    ASSIGN tt-sel.due-date:SCREEN-VALUE IN BROWSE {&browse-name} = string(ap-inv.due-date,"99/99/9999")
           tt-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)
           tt-sel.vend-no:SCREEN-VALUE = STRING(ap-inv.vend-no) 
           ld-tot-line = 0
           ld-non-disc = 0.

    FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK:
        ld-tot-line = ld-tot-line + ap-invl.amt.

        FIND FIRST account
            WHERE account.company EQ g_company
              AND account.actnum  EQ ap-invl.actnum
            NO-LOCK NO-ERROR.
        IF AVAIL account AND ap-invl.actnum NE "" THEN DO:
          FIND FIRST reftable
              WHERE reftable.reftable EQ "GLACCTDISC"
                AND reftable.company  EQ g_company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ account.actnum
              NO-LOCK NO-ERROR.
          IF AVAIL reftable AND reftable.val[1] EQ 1 THEN
            ld-non-disc = ld-non-disc + ap-invl.amt.
        END.
    END.

    ASSIGN
     ld-non-disc = IF ld-non-disc GE 0 THEN
                     (ld-non-disc * (ap-inv.net / ld-tot-line))
                   ELSE 0
     ld-tot-line = ap-inv.net - ld-non-disc.

    IF (TODAY - ap-inv.inv-date) <= ap-inv.disc-days AND ap-inv.due GT 0 THEN
          tt-sel.disc-amt:SCREEN-VALUE = STRING(ROUND((ld-tot-line * ap-inv.disc-% / 100),2) ).
    tt-sel.amt-paid:SCREEN-VALUE = string(ap-inv.due - DEC(tt-sel.disc-amt:SCREEN-VALUE)).
    fi_amt = fi_amt + dec(tt-sel.amt-paid:SCREEN-VALUE).
END.
ON 'help':U OF  tt-sel.inv-no IN BROWSE {&browse-name} 
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.
    RUN ap/l-apinv2.w (g_company, tt-sel.vend-no:SCREEN-VALUE IN BROWSE {&browse-name}, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
    IF lk-recid <> ? THEN DO:
        FIND ap-inv WHERE RECID(ap-inv) = lk-recid NO-LOCK.
        ASSIGN tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}= ENTRY(1,char-val)
               tt-sel.inv-date:SCREEN-VALUE = string(ap-inv.inv-date,"99/99/9999")
               tt-sel.dsc-date:SCREEN-VALUE = string(ap-inv.inv-date + ap-inv.disc-days,"99/99/9999")
               tt-sel.due-date:SCREEN-VALUE  = string(ap-inv.due-date,"99/99/9999")
               tt-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)               
               tt-sel.disc-amt:SCREEN-VALUE = "0.00"
               tt-sel.amt-due:SCREEN-VALUE = string(ap-inv.due)
               .                                               
            IF (TODAY - ap-inv.inv-date) <= ap-inv.disc-days AND ap-inv.due GT 0 THEN
                  tt-sel.disc-amt:SCREEN-VALUE = STRING(ROUND((ap-inv.disc-% * ap-inv.net / 100),2) ).
            tt-sel.amt-paid:SCREEN-VALUE = string(ap-inv.due - DEC(tt-sel.disc-amt:SCREEN-VALUE)).

    END.
END.
ON 'help':U OF  tt-sel.vend-no IN BROWSE {&browse-name} 
DO:
    DO:
    DEF VAR char-val AS cha NO-UNDO.
    APPLY "entry" TO tt-sel.vend-no IN BROWSE {&browse-name} .
    RUN windows/l-vendno.w (g_company, "", FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN tt-sel.vend-no:SCREEN-VALUE = ENTRY(1,char-val).

END.
END.
ON 'leave':U OF tt-sel.disc-amt IN BROWSE {&browse-name}
DO:

    ASSIGN tt-sel.amt-due:SCREEN-VALUE IN BROWSE {&browse-name}
                  = string(dec(tt-sel.inv-bal:SCREEN-VALUE) - dec(tt-sel.disc-amt:SCREEN-VALUE))
           tt-sel.amt-paid:SCREEN-VALUE = tt-sel.amt-due:SCREEN-VALUE.

    lv-pre-disc = dec(tt-sel.disc-amt:SCREEN-VALUE).

    RETURN.
END.
ON 'leave':U OF tt-sel.amt-paid IN BROWSE {&browse-name}
DO:

    lv-pre-paid = dec(tt-sel.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}).
    RETURN.
END.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

/* Include custom  Main Block code for SmartWindows. */
IF AVAIL company THEN DO:
  ASSIGN
   fi_chk-date  = TODAY
   fi_due-date  = TODAY
   fi_curr-code = CAPS(company.curr-code).

  {src/adm/template/windowmn.i}
END.

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table W-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 EMPTY TEMP-TABLE tt-sel.

  lv-proamt = 0 .

 FOR EACH ap-sel NO-LOCK
     WHERE ap-sel.company   EQ g_company
       AND ap-sel.man-check EQ NO:

   FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    IF AVAIL ap-ctrl THEN DO:                                           /*Task# 01091401*/
        FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
            AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.   
        IF AVAIL bank THEN do:  
            DO WITH FRAME {&FRAME-NAME}:
                lv-proamt:SCREEN-VALUE = STRING(bank.bal - fi_amt) .
            END.
        END.
    END.

   FIND FIRST ap-inv NO-LOCK
       WHERE ap-inv.company eq cocode
         AND ap-inv.vend-no eq ap-sel.vend-no
         AND ap-inv.posted  eq yes
         AND ap-inv.inv-no  eq ap-sel.inv-no
       NO-ERROR.

   CREATE tt-sel.
   BUFFER-COPY ap-sel TO tt-sel
   ASSIGN
    tt-sel.tt-rowid = ROWID(ap-sel)
    tt-sel.amt-due  = ap-sel.inv-bal - ap-sel.disc-amt.

    fi_amt = fi_amt + ap-sel.amt-paid.
/*     tt-sel.dsc-date = ap-inv.inv-date. */

   IF AVAIL ap-inv THEN
     ASSIGN
      tt-sel.due-date = ap-inv.due-date
      tt-sel.dsc-date = ap-inv.inv-date + ap-inv.disc-days
      tt-sel.inv-date = ap-inv.inv-date.
 END.
DISPLAY fi_amt WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-apsel W-Win 
PROCEDURE create-apsel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ld-tot-line AS DEC NO-UNDO.
DEF VAR ld-non-disc AS DEC NO-UNDO.
DEF VAR lv-net LIKE ap-inv.net NO-UNDO.


fi_amt = 0.
    for each vend where vend.company eq cocode
                      and vend.acc-bal gt 0
                      AND ((vend.spare-int-1 NE 1 AND tb_cc) OR NOT tb_cc)
                      AND ((vend.spare-int-1 EQ 1 AND tb_cc-only) OR NOT tb_cc-only)
                      AND ((vend.spare-int-2 NE 1 AND tb_cc) OR NOT tb_cc)
                      AND ((vend.spare-int-2 EQ 1 AND tb_bp-only) OR NOT tb_bp-only)
                      no-lock,
          each ap-inv where ap-inv.company  eq cocode
                        and ap-inv.vend-no  eq vend.vend-no
                        and ap-inv.posted   eq yes
                        AND ap-inv.stat     EQ "R"
                        and ap-inv.due      ne 0
                        and (ap-inv.due-date le (fi_due-date + fi_age) 
                             OR ((ap-inv.inv-date + ap-inv.disc-days) LE (fi_due-date + fi_age) 
                                 AND (ap-inv.inv-date + ap-inv.disc-days) GE fi_due-date
                                 AND tb_discount))
                        AND (ap-inv.curr-code[1] EQ fi_curr-code OR
                             (ap-inv.curr-code[1] EQ "" AND
                              company.curr-code EQ fi_curr-code))
                    use-index ap-inv           
                     break by ap-inv.vend-no:

          find first ap-sel WHERE ap-sel.company = g_company                    
/*                               and ap-sel.man-check eq NO */
                              and ap-sel.vend-no eq ap-inv.vend-no
                              and ap-sel.inv-no  eq ap-inv.inv-no no-error.
          if not avail ap-sel then do:
              create ap-sel.
            ASSIGN ap-sel.company      = cocode
                 ap-sel.man-check    = no
                 ap-sel.vend-no      = ap-inv.vend-no
                 ap-sel.inv-no       = ap-inv.inv-no
                 ap-sel.due-date     = ap-inv.due-date
                 ap-sel.inv-bal      = ap-inv.due
                 ap-sel.curr-code[1] = ap-inv.curr-code[1]
                 ld-tot-line         = 0
                 ld-non-disc         = 0.
          END.
          FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK:
            ld-tot-line = ld-tot-line + ap-invl.amt.

            FIND FIRST account
                WHERE account.company EQ cocode
                  AND account.actnum  EQ ap-invl.actnum
                NO-LOCK NO-ERROR.
            IF AVAIL account AND ap-invl.actnum NE "" THEN DO:
              FIND FIRST reftable
                  WHERE reftable.reftable EQ "GLACCTDISC"
                    AND reftable.company  EQ cocode
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ account.actnum
                  NO-LOCK NO-ERROR.
              IF AVAIL reftable AND reftable.val[1] EQ 1 THEN
                ld-non-disc = ld-non-disc + ap-invl.amt.
            END.
          END.

          ASSIGN
           lv-net      = ap-inv.net - ap-inv.tax-amt
           ld-non-disc = IF ld-non-disc GE 0 THEN
                           (ld-non-disc * (lv-net / ld-tot-line))
                         ELSE 0
           ld-tot-line = lv-net - ld-non-disc.

          if ((today - ap-inv.inv-date) le ap-inv.disc-days or v-show-disc) 
              AND ap-inv.due GT 0 then
             ASSIGN ap-sel.disc-amt = (ld-tot-line * ap-inv.disc-% / 100)
                    ap-sel.amt-paid = ap-sel.inv-bal - ap-sel.disc-amt.
          ELSE ASSIGN ap-sel.disc-amt = 0
                      ap-sel.amt-paid = ap-sel.inv-bal.

        /*fi_amt = fi_amt + ap-sel.amt-paid.*/

        find first ap-chk
            where ap-chk.company   eq cocode
              and ap-chk.vend-no   eq ap-sel.vend-no
              and ap-chk.man-check eq no
            no-error.
        if not avail ap-chk then do:
          create ap-chk.
          ASSIGN ap-chk.company   = cocode
                 ap-chk.vend-no   = ap-sel.vend-no
                 ap-chk.check-no  = ?
                 ap-chk.man-check = NO.
        end.
        ap-chk.check-amt = ap-chk.check-amt + ap-sel.amt-paid.

        FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.    /*Task# 01091401*/
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN do:
                  DO WITH FRAME {&FRAME-NAME}:
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - fi_amt) .
                  END.
              END.

          END.

      end.  /* for each vend, ap-inv... */

      for each ap-chk
          where ap-chk.company   eq cocode
            and ap-chk.check-amt le 0
            and ap-chk.man-check eq no:
          for each ap-sel
              where ap-sel.company   eq cocode
                and ap-sel.vend-no   eq ap-chk.vend-no
                and ap-sel.man-check eq no:
           fi_amt = fi_amt - ap-sel.amt-paid.
           delete ap-sel.
        end.
        delete ap-chk.
      end.

DISPLAY fi_amt WITH FRAME {&FRAME-NAME}.

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
  DISPLAY fi_chk-date fi_curr-code c-desc fi_due-date fi_age fi_amt lv-proamt 
          tb_discount tb_cc fi_sortBy tb_cc-only tb_bp-only 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi_chk-date fi_curr-code fi_due-date fi_age lv-proamt tb_discount 
         tb_cc btn-go BROWSE-1 btn-change btn-add btn-delete btn-finish 
         tb_cc-only tb_bp-only RECT-9 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR ll-printed AS LOG NO-UNDO.
  DEF VAR v-msg AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF access-close THEN DO:
     APPLY "window-close" TO current-window.
     RETURN .
  END.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO fi_curr-code.
  END.

  ASSIGN uperiod = g_period.

  FOR EACH ap-sel
      WHERE ap-sel.company   EQ cocode
        AND ap-sel.man-check EQ NO
        AND NOT CAN-FIND(FIRST ap-inv
                         WHERE ap-inv.company EQ cocode
                           AND ap-inv.vend-no EQ ap-sel.vend-no
                           AND ap-inv.inv-no  EQ ap-sel.inv-no)
      TRANSACTION:
    DELETE ap-sel.
  END.

  IF CAN-FIND(FIRST ap-sel
              WHERE ap-sel.company   EQ cocode                    
                AND ap-sel.man-check EQ NO) THEN DO:

    ASSIGN
     ll         = ?
     ll-printed = CAN-FIND(FIRST ap-sel
                           WHERE ap-sel.company   EQ cocode
                             AND ap-sel.man-check EQ NO
                             AND ap-sel.check-no  NE ?
                             AND ap-sel.check-no  GT 0).

    MESSAGE "Checks have been" +
            (IF ll-printed THEN " " ELSE " SELECTED but not ") + 
            "PRINTED.  You should press NO to UPDATE or press CANCEL to do nothing.  " +
            "By pressing YES you will ERASE ALL SELECTED checks" +
            (IF ll-printed THEN " including checks that have already been printed" ELSE "") +
            "."
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL
        UPDATE ll.

    IF ll THEN DO:
      FOR EACH ap-sel
          WHERE ap-sel.company   EQ cocode
            AND ap-sel.man-check EQ NO :
        DELETE ap-sel.
      END.

      FOR EACH ap-chk
          WHERE ap-chk.company   EQ cocode
            AND ap-chk.man-check EQ NO:
        DELETE ap-chk.
      END.
    END.

    ELSE
    IF ll EQ ? THEN DO:
       APPLY "window-close" TO CURRENT-WINDOW.
       RETURN .
    END.
  END.

  RUN build-table.

  {methods/nowait.i}

  IF CAN-FIND(FIRST tt-sel) THEN DO WITH FRAME {&FRAME-NAME}: 
    {&open-query-{&browse-name}}
    ASSIGN fi_amt = 0.
    FOR EACH tt-sel:
        fi_amt = fi_amt + tt-sel.amt-paid.
    END.
    DISPLAY fi_amt.
    DISABLE fi_chk-date fi_due-date fi_curr-code fi_age fi_amt lv-proamt btn-go tb_discount tb_cc tb_cc-only tb_bp-only.
    APPLY "entry" TO btn-change.
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:
      APPLY "entry" TO fi_chk-date.
  END.

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
  {src/adm/template/snd-list.i "ap-pay"}
  {src/adm/template/snd-list.i "tt-sel"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fi_curr-code W-Win 
PROCEDURE valid-fi_curr-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF fi_curr-code NE company.curr-code                             AND
       NOT CAN-FIND(FIRST currency
                    WHERE currency.company EQ cocode
                      AND currency.c-code  EQ ip-focus:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckForNegative W-Win 
FUNCTION CheckForNegative RETURNS LOGICAL
  ( ipcCompany AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE lNegativeFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cVendor AS CHARACTER   NO-UNDO.

DEFINE BUFFER bf-ap-chk FOR ap-chk.

ASSIGN 
    lNegativeFound = NO
    cVendor = "".
FOR EACH bf-ap-chk
    WHERE bf-ap-chk.company EQ ipcCompany
      AND bf-ap-chk.man-check EQ NO
      AND bf-ap-chk.check-amt LT 0
    NO-LOCK:
    lNegativeFound = YES.
    IF cVendor NE "" THEN cVendor = cVendor + ",".
    cVendor = cVendor + bf-ap-chk.vend-no.
END.

IF lNegativeFound THEN 
    MESSAGE "Checks with a negative total have been found for vendor(s):" SKIP
        cVendor SKIP(1)
        "Would you like to return to the selection screen to correct this?"    
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lNegativeFound.
  RETURN lNegativeFound.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

