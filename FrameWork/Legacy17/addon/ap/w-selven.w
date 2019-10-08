&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: ap\w-selven.w

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
/*{custom/globdefs.i } */
{METHODS/PRGSECUR.I}               
/*DEF SHARED VAR g_company AS cha NO-UNDO.
DEF SHARED VAR g_loc AS cha NO-UNDO.
DEF SHARED VAR g_period AS INT NO-UNDO.
*/
{methods/defines/hndldefs.i}               

{sys/inc/VAR.i NEW SHARED}
{ap/d-selinv.i NEW}

DEF TEMP-TABLE tt-sel NO-UNDO LIKE ap-sel
           FIELD amt-due AS DEC FORM "->>,>>>,>>9.99" 
           FIELD tt-recid AS RECID
           FIELD tt-deleted AS LOG.
ASSIGN cocode = g_company
       locode = g_loc.

DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */
/*
&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING
*/
DEF VAR v-show-disc AS LOG NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR lv-pre-disc AS DEC NO-UNDO.
DEF VAR lv-pre-paid AS DEC NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR nufile AS LOG NO-UNDO.
DEF VAR lv-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-num-rec AS INT NO-UNDO.
DEF VAR lv-in-update AS LOG NO-UNDO.
DEF VAR lv-in-add AS LOG NO-UNDO.
DEF VAR ll-continue AS LOG NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-sel

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-sel.inv-no tt-sel.due-date tt-sel.inv-bal tt-sel.amt-due tt-sel.disc-amt tt-sel.amt-paid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-sel.inv-no ~
  tt-sel.disc-amt ~
  tt-sel.amt-paid   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-sel WHERE NOT tt-deleted
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-sel WHERE NOT tt-deleted .
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-sel
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-sel


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-vend-no tb_disc btn-go BROWSE-1 lv-proamt ~
btn-change btn-add btn-delete btn-continue btn-finish RECT-9 
&Scoped-Define DISPLAYED-OBJECTS lv-vend-no tb_disc vend_name lv-amount ~
lv-proamt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-vend-no tb_disc 

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

DEFINE BUTTON btn-continue 
     LABEL "Continue" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-finish 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-go 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-amount AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL ? 
     LABEL "Check Amount" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE lv-proamt AS DECIMAL FORMAT "$->>>,>>>,>>9.99":U INITIAL ? 
     LABEL "Projected Cash Balance" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE lv-vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE vend_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 3.57.

DEFINE VARIABLE tb_disc AS LOGICAL INITIAL no 
     LABEL "Show Discount Regardless of Discount Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-sel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-sel.inv-no
    tt-sel.due-date LABEL "Due Date"
    tt-sel.inv-bal LABEL "Invoice Amt"
    tt-sel.amt-due LABEL "Balance Due"
    tt-sel.disc-amt LABEL "Discount"
    tt-sel.amt-paid LABEL "To Be Paid"
    ENABLE tt-sel.inv-no
           tt-sel.disc-amt
           tt-sel.amt-paid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 148 BY 17.86
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lv-vend-no AT ROW 1.48 COL 17 COLON-ALIGNED
     tb_disc AT ROW 3.14 COL 15
     btn-go AT ROW 2.91 COL 75
     vend_name AT ROW 1.48 COL 36 COLON-ALIGNED NO-LABEL
     lv-amount AT ROW 1.48 COL 108 COLON-ALIGNED
     BROWSE-1 AT ROW 5.05 COL 1
     lv-proamt AT ROW 2.91 COL 119 COLON-ALIGNED
     btn-change AT ROW 23.14 COL 11
     btn-add AT ROW 23.14 COL 27
     btn-cancel AT ROW 23.14 COL 43
     btn-delete AT ROW 23.14 COL 59
     btn-continue AT ROW 23.14 COL 94
     btn-finish AT ROW 23.14 COL 128
     RECT-9 AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
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
         TITLE              = "Payment Selection by Vendor"
         HEIGHT             = 23.71
         WIDTH              = 149
         MAX-HEIGHT         = 24.19
         MAX-WIDTH          = 152.6
         VIRTUAL-HEIGHT     = 24.19
         VIRTUAL-WIDTH      = 152.6
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
/* BROWSE-TAB BROWSE-1 lv-amount F-Main */
/* SETTINGS FOR BUTTON btn-cancel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-amount IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-vend-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_disc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN vend_name IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-sel WHERE NOT tt-deleted .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Payment Selection by Vendor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Payment Selection by Vendor */
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
    /*
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
   ASSIGN
   lv-in-add = YES
   tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = NO
   /*tt-sel.due-date:READ-ONLY = NO
   tt-sel.inv-bal:READ-ONLY = NO 
   tt-sel.amt-due:READ-ONLY = NO*/
   tt-sel.disc-amt:READ-ONLY = NO
   tt-sel.amt-paid:READ-ONLY = NO.

   BROWSE {&browse-name}:INSERT-ROW("after").
   APPLY "entry" TO tt-sel.inv-no IN BROWSE {&browse-name}.
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
      btn-change:LABEL = "Update".
      ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
             /*tt-sel.due-date:READ-ONLY = YES
             tt-sel.inv-bal:READ-ONLY = YES
             tt-sel.amt-due:READ-ONLY = YES*/
             tt-sel.disc-amt:READ-ONLY = NO                 
             tt-sel.amt-paid:READ-ONLY = NO.

      ENABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:  /* add and cancel */
     IF lv-in-add THEN BROWSE {&browse-name}:DELETE-current-ROW().
     btn-change:LABEL = "Update".
     ENABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-change
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-change W-Win
ON CHOOSE OF btn-change IN FRAME F-Main /* Update */
DO:  
   /* IF AVAIL tt-sel THEN DO:
   */
   DEF BUFFER bf-tsel FOR tt-sel.

       IF SELF:LABEL = "Save"   THEN DO:  

          lv-amount = 0.
          FOR EACH bf-tsel where bf-tsel.vend-no = lv-vend-no:
               lv-amount = lv-amount + bf-tsel.amt-paid.
          END.

           /* ======= validation ==========*/
          FIND FIRST ap-inv WHERE ap-inv.company = g_company 
                        AND ap-inv.vend-no = lv-vend-no
                        AND ap-inv.posted  = YES
                        AND ap-inv.inv-no = (tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} ) NO-LOCK NO-ERROR.
          IF NOT AVAIL ap-inv THEN DO:
             MESSAGE "Invalid Invoice Number. Try Help. " VIEW-AS ALERT-BOX.
             APPLY "entry" TO tt-sel.inv-no .
             RETURN NO-APPLY.
          END.
          IF lv-in-add THEN DO:
             FIND FIRST ap-sel WHERE ap-sel.company = g_company
                        AND ap-sel.vend-no = lv-vend-no
                        AND ap-sel.man-check = NO
                        AND ap-sel.inv-no = tt-sel.inv-no:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF AVAIL ap-sel THEN DO:
                MESSAGE "The INVOICE NUMBER you entered has already been selected, please re-enter."
                       VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO tt-sel.inv-no.
                RETURN NO-APPLY.
             END.
          END.   

          /* If not credit, check for overpayment. */
          IF dec(tt-sel.inv-bal:SCREEN-VALUE) > 0 THEN DO:
              IF dec(tt-sel.amt-paid:SCREEN-VALUE) + dec(tt-sel.disc-amt:SCREEN-VALUE) > 
              dec(tt-sel.inv-bal:SCREEN-VALUE) THEN DO:
                 MESSAGE "Over payment is not allowed!" VIEW-AS ALERT-BOX ERROR.
                 APPLY "entry" TO tt-sel.amt-paid.
                 RETURN NO-APPLY.
              END.
          END.
          /* credit overpayment validation */
          IF dec(tt-sel.inv-bal:SCREEN-VALUE) < 0 THEN DO:
              /* If amt to be paid exceeds balance due, then error. */
              IF (dec(tt-sel.amt-paid:SCREEN-VALUE) * -1) > (dec(tt-sel.amt-due:SCREEN-VALUE) * -1) THEN DO:
                  MESSAGE "Credit Over payment is not allowed!"
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY "entry" TO tt-sel.amt-paid. 
                  RETURN NO-APPLY.
              END.
          END.

          /* end validation */
          lv-pre-paid = DEC(tt-sel.amt-paid:SCREEN-VALUE).
          FIND FIRST ap-inv WHERE ap-inv.company = g_company 
                        AND ap-inv.vend-no = lv-vend-no
                        AND ap-inv.inv-no = (tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} ) 
                        NO-LOCK NO-ERROR.
          IF lv-in-add THEN DO:
             CREATE tt-sel.
             ASSIGN tt-sel.company = g_company
                    tt-sel.vend-no = lv-vend-no
                    tt-sel.inv-no = tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}
             /* need inv-no lookup and find ap-inv */                    
                    tt-sel.inv-bal = IF AVAIL ap-inv THEN ap-inv.due ELSE 0
                    tt-sel.due-date = IF AVAIL ap-inv THEN ap-inv.due-date ELSE ?
                    .
                    /* need to create ap-sel */
          END.
          lv-amount = lv-amount - lv-pre-paid .

     /*  do when post
          ASSIGN ap-inv.paid = ap-inv.paid - lv-pre-paid
                 ap-inv.due = ap-inv.due + lv-pre-paid + lv-pre-disc
                 .
     */
           ASSIGN tt-sel.disc-amt = dec(tt-sel.disc-amt:SCREEN-VALUE IN BROWSE {&browse-name})
                  tt-sel.amt-due = dec(tt-sel.amt-due:SCREEN-VALUE)
                  tt-sel.amt-paid = dec(tt-sel.amt-paid:SCREEN-VALUE).

/* removed by stacey */
/*           lv-amount = lv-amount + tt-sel.amt-paid. */

           /* stacey: do this instead. */
           lv-amount = 0.
           FOR EACH bf-tsel where bf-tsel.vend-no = lv-vend-no:
                lv-amount = lv-amount + bf-tsel.amt-paid.
           END.
          DISPLAY lv-amount WITH FRAME {&FRAME-NAME}.

          ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
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
       /*
          ASSIGN ap-inv.paid = ap-inv.paid + tt-sel.amt-paid
                 ap-inv.due = ap-inv.due - tt-sel.amt-paid - tt-sel.disc-amt
                 .          
       */ 
       END.
       ELSE DO: 
          SELF:LABEL = "Save".
          ASSIGN
             lv-in-update = YES
             lv-in-add = NO
             tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
             /*tt-sel.due-date:READ-ONLY = YES
             tt-sel.inv-bal:READ-ONLY = YES
             tt-sel.amt-due:READ-ONLY = YES*/
             tt-sel.disc-amt:READ-ONLY = NO                 
             tt-sel.amt-paid:READ-ONLY = NO.

          ENABLE btn-change btn-cancel WITH FRAME {&FRAME-NAME}.
          DISABLE btn-delete btn-add btn-finish WITH FRAME {&FRAME-NAME}.
          APPLY "entry" TO tt-sel.disc-amt IN BROWSE {&browse-name}.
          RETURN NO-APPLY.
       END.
   /* END.  */
          FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - lv-amount) .
          END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-continue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-continue W-Win
ON CHOOSE OF btn-continue IN FRAME F-Main /* Continue */
DO:
    ENABLE lv-vend-no tb_disc btn-go WITH FRAME {&FRAME-NAME}.
    ASSIGN tt-sel.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
           /*tt-sel.due-date:READ-ONLY = YES
           tt-sel.inv-bal:READ-ONLY = YES
           tt-sel.amt-due:READ-ONLY = YES*/
           tt-sel.disc-amt:READ-ONLY = NO                 
           tt-sel.amt-paid:READ-ONLY = NO.

    DISABLE btn-change btn-add btn-cancel btn-delete WITH FRAME {&FRAME-NAME}.
    ll-continue = YES.
    APPLY "choose" TO btn-finish.
    APPLY "entry" TO lv-vend-no.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete W-Win
ON CHOOSE OF btn-delete IN FRAME F-Main /* Delete */
DO:
    DEF VAR ll-dum AS LOG NO-UNDO.
 IF AVAIL tt-sel THEN DO WITH FRAME {&FRAME-NAME}:
    message "Delete Currently Selected Record?" view-as alert-box question
          button yes-no update ll-ans as log.
    if not ll-ans then return .

    tt-sel.tt-deleted = YES.
    FIND ap-sel WHERE RECID(ap-sel) = tt-sel.tt-recid NO-ERROR.
    IF AVAIL ap-sel THEN do:
        lv-amount = DEC(lv-amount:SCREEN-VALUE) - tt-sel.amt-paid.
        DISPLAY lv-amount.
        ASSIGN ap-sel.amt-paid = tt-sel.amt-paid
               ap-sel.disc-amt = tt-sel.disc-amt.
        DELETE ap-sel.
    END.

    DELETE tt-sel.

    FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - lv-amount) .
          END.


    ASSIGN
       ll-dum = BROWSE {&browse-name}:DELETE-CURRENT-ROW()
       lv-num-rec = lv-num-rec - 1.
    IF NUM-RESULTS("{&browse-name}") < 1 THEN
         DISABLE btn-change btn-cancel btn-delete WITH FRAME {&FRAME-NAME}.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-finish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-finish W-Win
ON CHOOSE OF btn-finish IN FRAME F-Main /* Exit */
DO:
    DEFINE VARIABLE lNegCheck AS LOGICAL     NO-UNDO.
    SESSION:SET-WAIT-STATE("general").
    FOR EACH tt-sel:
        FIND ap-sel WHERE RECID(ap-sel) = tt-sel.tt-recid NO-ERROR.

        IF tt-sel.tt-deleted THEN DELETE ap-sel.
        IF NOT AVAIL ap-sel THEN DO:
            CREATE ap-sel.
            BUFFER-COPY tt-sel EXCEPT tt-recid tt-sel.amt-due TO ap-sel.
        END.
        ELSE ASSIGN ap-sel.amt-paid = tt-sel.amt-paid
                    ap-sel.disc-amt = tt-sel.disc-amt.

        FIND CURRENT ap-sel NO-LOCK.
    END.

    for each ap-chk
        where ap-chk.company   eq cocode
          and ap-chk.man-check eq no:
      ap-chk.check-amt = 0.
    end.

    for each ap-sel
        where ap-sel.company   eq cocode
          and ap-sel.man-check eq NO NO-LOCK:

        put screen row 21 columns 65 ap-sel.vend-no + "     ".

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

         FIND CURRENT ap-chk NO-LOCK.
    end.
    lNegCheck = CheckForNegative(INPUT cocode).
    IF ll-continue AND NOT lNegCheck THEN DO:
       FOR EACH tt-sel:
           DELETE tt-sel.
       END.
       {&open-query-{&browse-name}}

    END.
    SESSION:SET-WAIT-STATE("").
    IF NOT ll-continue AND NOT lNegCheck THEN APPLY "close" TO  this-procedure.
    ll-continue = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go W-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
DO:
    ASSIGN {&list-1}.

    FIND FIRST vend WHERE vend.company = g_company
                      and vend.vend-no EQ lv-vend-no 
                         use-index vend no-lock no-error.
    IF NOT AVAIL vend THEN DO:
       MESSAGE "Invalid Vendor. Try Help. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO lv-vend-no.
       RETURN NO-APPLY.
    END.
    ASSIGN
     vend_name = vend.NAME
     fil_id = if avail vend then recid(vend) else ?

    /*RUN ap/d-paysel.w (INPUT TODAY, INPUT 0, OUTPUT v-show-disc, OUTPUT choice).*/

    v-show-disc = tb_disc.
    choice = YES.

    IF choice THEN DO WITH FRAME {&FRAME-NAME}:
      RUN build-table.

      DISABLE lv-vend-no lv-amount tb_disc btn-go lv-proamt.

      IF lv-num-rec LE 0 THEN DO:
        RUN ap/d-selinv.w (fil_id, YES).
        RUN create-apsel.        
      END.

      IF lv-num-rec GT 0 THEN DO:
        {&open-query-{&browse-name}}
        btn-change:LABEL = "Update".
        APPLY "choose" TO btn-change.  
      END.
      ELSE APPLY "choose" TO btn-add.
      RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-vend-no W-Win
ON HELP OF lv-vend-no IN FRAME F-Main /* Vendor# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    APPLY "entry" TO {&self-name}.
    RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN lv-vend-no:SCREEN-VALUE = ENTRY(1,char-val)
                                vend_name:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-vend-no W-Win
ON LEAVE OF lv-vend-no IN FRAME F-Main /* Vendor# */
DO:
    IF LASTKEY = -1 THEN RETURN.

    FIND FIRST vend WHERE vend.company = g_company
                      and vend.vend-no EQ lv-vend-no:SCREEN-VALUE 
                         use-index vend no-lock no-error.
    IF NOT AVAIL vend THEN DO:
       MESSAGE "Invalid Vendor. Try Help. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    vend_name = vend.NAME.
    fil_id = if avail vend then recid(vend) else ?.

    APPLY "choose" TO btn-go.
    DISABLE lv-vend-no tb_disc btn-go WITH FRAME {&FRAME-NAME}.
    ENABLE btn-change btn-add btn-delete btn-continue btn-finish
           WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-vend-no W-Win
ON return OF lv-vend-no IN FRAME F-Main /* Vendor# */
DO:
  APPLY "leave" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-vend-no W-Win
ON VALUE-CHANGED OF lv-vend-no IN FRAME F-Main /* Vendor# */
DO:
   IF lv-vend-no:SCREEN-VALUE <> "" THEN DO:
      FIND vend WHERE vend.company = g_company
                      and vend.vend-no begins lv-vend-no:SCREEN-VALUE 
                         use-index vend no-lock no-error.
      IF AVAIL vend THEN DO:
         ASSIGN lv-vend-no = vend.vend-no
                             vend_name = vend.NAME.
         DISP lv-vend-no vend_name WITH FRAME {&FRAME-NAME}.
         fil_id = if avail vend then recid(vend) else ?.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disc W-Win
ON VALUE-CHANGED OF tb_disc IN FRAME F-Main /* Show Discount Regardless of Discount Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}



ON 'leave':U OF tt-sel.inv-no IN BROWSE {&browse-name} 
DO:    
  DEF VAR ld-tot-line AS DEC NO-UNDO.
  DEF VAR ld-non-disc AS DEC NO-UNDO.


    IF LASTKEY = -1 THEN RETURN.
    FIND FIRST ap-inv WHERE ap-inv.company = g_company 
                        AND ap-inv.vend-no = lv-vend-no
                        AND ap-inv.posted  = YES
                        AND ap-inv.inv-no = (tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} ) NO-LOCK NO-ERROR.
    IF NOT AVAIL ap-inv THEN DO:
       MESSAGE "Invalid Invoice Number. Try Help. " VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    IF lv-in-add THEN DO:
       FIND FIRST ap-sel WHERE ap-sel.company = g_company
                        AND ap-sel.vend-no = lv-vend-no
                        AND ap-sel.man-check = NO
                        AND ap-sel.inv-no = INPUT tt-sel.inv-no NO-LOCK NO-ERROR.
       IF AVAIL ap-sel THEN DO:
          MESSAGE "The INVOICE NUMBER you entered has already been selected, please re-enter."
                 VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-sel.disc-amt:SCREEN-VALUE = "0.00"
              tt-sel.amt-due:SCREEN-VALUE = string(ap-inv.due).
    END.   

    ASSIGN tt-sel.due-date:SCREEN-VALUE IN BROWSE {&browse-name} = string(ap-inv.due-date,"99/99/9999")
           tt-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)
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

    IF (TODAY - ap-inv.inv-date) <= ap-inv.disc-days THEN
          tt-sel.disc-amt:SCREEN-VALUE = STRING(ROUND((ld-tot-line * ap-inv.disc-% / 100),2) ).
    tt-sel.amt-paid:SCREEN-VALUE = string(ap-inv.due - DEC(tt-sel.disc-amt:SCREEN-VALUE)).
    lv-amount = lv-amount + dec(tt-sel.amt-paid:SCREEN-VALUE).

    FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN do:
                  DO WITH FRAME {&FRAME-NAME}:
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - lv-amount) .
                  END.
              END.
          END.

END.
ON 'help':U OF  tt-sel.inv-no IN BROWSE {&browse-name} 
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.
    RUN ap/l-apinv.w (g_company, lv-vend-no, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
    IF lk-recid <> ? THEN DO:
        FIND ap-inv WHERE RECID(ap-inv) = lk-recid NO-LOCK.
        ASSIGN tt-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}= ENTRY(1,char-val)
               tt-sel.due-date:SCREEN-VALUE  = string(ap-inv.due-date,"99/99/9999")
               tt-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)               
               tt-sel.disc-amt:SCREEN-VALUE = "0.00"
               tt-sel.amt-due:SCREEN-VALUE = string(ap-inv.due)
               .                                               
            IF (TODAY - ap-inv.inv-date) <= ap-inv.disc-days THEN
                  tt-sel.disc-amt:SCREEN-VALUE = STRING(ROUND((ap-inv.disc-% * ap-inv.net / 100),2) ).
            tt-sel.amt-paid:SCREEN-VALUE = string(ap-inv.due - DEC(tt-sel.disc-amt:SCREEN-VALUE)).

    END.
END.
ON 'leave':U OF tt-sel.disc-amt IN BROWSE {&browse-name}
DO:
    ASSIGN tt-sel.amt-due:SCREEN-VALUE IN BROWSE {&browse-name}
                  = string(
                      dec(tt-sel.inv-bal:SCREEN-VALUE) - dec(tt-sel.disc-amt:SCREEN-VALUE))
              tt-sel.amt-paid:SCREEN-VALUE = tt-sel.amt-due:SCREEN-VALUE.

    lv-pre-disc = IF AVAIL tt-sel THEN tt-sel.disc-amt ELSE 0.
END.
ON 'leave':U OF tt-sel.amt-paid IN BROWSE {&browse-name}
DO:
    ASSIGN lv-pre-paid = IF AVAIL tt-sel THEN tt-sel.amt-paid ELSE 0.
END.

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

 find vend where recid(vend) eq fil_id no-lock no-error.
 if not avail vend then RETURN error.

 for each ap-chk where ap-chk.company   eq cocode
                   and ap-chk.vend-no   eq vend.vend-no
                   and ap-chk.man-check eq no:
     delete ap-chk.
 end.

 ASSIGN
 lv-amount = 0
 lv-proamt = 0
 lv-num-rec = 0.
 for each ap-sel where ap-sel.company   eq cocode
                   and ap-sel.vend-no   eq vend.vend-no
                   and ap-sel.man-check eq NO NO-LOCK:

   lv-amount = lv-amount + ap-sel.amt-paid.

   FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    IF AVAIL ap-ctrl THEN DO:
     FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
         AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
     IF AVAIL bank THEN do:
         DO WITH FRAME {&FRAME-NAME}:
             lv-proamt:SCREEN-VALUE = STRING(bank.bal - lv-amount) .
         END.
     END.
   END.

   find first ap-inv
       where ap-inv.company eq cocode
         and ap-inv.vend-no eq ap-sel.vend-no
         and ap-inv.posted  eq yes
         and ap-inv.inv-no  eq ap-sel.inv-no
       no-lock no-error.

   CREATE tt-sel.
   BUFFER-COPY ap-sel TO tt-sel
      ASSIGN tt-sel.tt-recid = RECID(ap-sel)
             tt-sel.amt-due = ap-sel.inv-bal - ap-sel.disc-amt
             tt-sel.due-date = IF AVAIL ap-inv THEN ap-inv.due-date ELSE ap-sel.due-date
             lv-num-rec = lv-num-rec + 1.

 end.


 if lv-amount = 0 then nufile = yes.
 DISPLAY lv-amount WITH FRAME {&FRAME-NAME}.


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


  FOR EACH tt-inv WHERE tt-inv.selekt:
      RUN create-one (tt-inv.rec-id).

      /*
      ASSIGN ap-inv.paid = ap-inv.paid + ap-sel.amt-paid
             ap-inv.due = ap-inv.due - ap-sel.amt-paid - ap-sel.disc-amt
             .
      */       
      lv-num-rec = lv-num-rec + 1.
  END.
  DISPLAY lv-amount WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-one W-Win 
PROCEDURE create-one :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DEF VAR ld-tot-line AS DEC NO-UNDO.
  DEF VAR ld-non-disc AS DEC NO-UNDO.
  DEF VAR lv-net LIKE ap-inv.net NO-UNDO.


  FIND ap-inv WHERE RECID(ap-inv) EQ ip-recid NO-LOCK NO-ERROR.

  IF AVAIL ap-inv THEN DO:
      CREATE ap-sel.
      ASSIGN ap-sel.company   = g_company
             ap-sel.vend-no   = lv-vend-no
             ap-sel.inv-no    = ap-inv.inv-no
             ap-sel.inv-bal   = ap-inv.due
             ap-sel.due-date  = ap-inv.due-date
             ap-sel.man-check = NO
             ld-tot-line      = 0
             ld-non-disc      = 0.

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
       lv-net      = ap-inv.net - ap-inv.tax-amt
       ld-non-disc = IF ld-non-disc GE 0 THEN
                       (ld-non-disc * (lv-net / ld-tot-line))
                     ELSE 0
       ld-tot-line = lv-net - ld-non-disc.

      IF (TODAY - ap-inv.inv-date) LE ap-inv.disc-days OR v-show-disc THEN
            ap-sel.disc-amt = (ld-tot-line * ap-inv.disc-% / 100).

      ASSIGN
         ap-sel.amt-paid = ap-sel.inv-bal - ap-sel.disc-amt
         lv-amount = lv-amount + ap-sel.amt-paid.

      CREATE tt-sel.
      BUFFER-COPY ap-sel TO tt-sel.
      ASSIGN tt-sel.tt-recid = RECID(ap-sel)
             tt-sel.amt-due = ap-sel.inv-bal - ap-sel.disc-amt
             tt-sel.due-date = ap-inv.due-date.

          FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
          IF AVAIL ap-ctrl THEN DO:
              FIND FIRST bank WHERE bank.actnum EQ ap-ctrl.cash-act
                  AND bank.company EQ ap-ctrl.company NO-LOCK NO-ERROR.
              IF AVAIL bank THEN do:
                  DO WITH FRAME {&FRAME-NAME}:
                  lv-proamt:SCREEN-VALUE = STRING(bank.bal - lv-amount) .
                  END.
              END.

          END.
  END.

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
  DISPLAY lv-vend-no tb_disc vend_name lv-amount lv-proamt 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE lv-vend-no tb_disc btn-go BROWSE-1 lv-proamt btn-change btn-add 
         btn-delete btn-continue btn-finish RECT-9 
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

  for each ap-sel WHERE ap-sel.company = g_company
                    AND ap-sel.man-check = NO
              and not can-find(first ap-inv where ap-inv.company eq cocode
                                        and ap-inv.vend-no eq ap-sel.vend-no
                                        and ap-inv.inv-no  eq ap-sel.inv-no)
               transaction:
    delete ap-sel.
  end.

  IF CAN-FIND(FIRST ap-sel
              WHERE ap-sel.company   EQ g_company                    
                AND ap-sel.man-check EQ NO) THEN DO:

    ASSIGN
     ll         = ?
     ll-printed = CAN-FIND(FIRST ap-sel
                           WHERE ap-sel.company   EQ g_company
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
      for each ap-sel WHERE ap-sel.company = g_company                    
                        and ap-sel.man-check eq NO :
        delete ap-sel.
      end.

      for each ap-chk where ap-chk.company   eq g_company
                        and ap-chk.man-check eq no:
        delete ap-chk.
      end.
    END.

    ELSE
    IF ll EQ ? THEN DO:
       APPLY "window-close" TO current-window.
       RETURN .
    END.
  END.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE lv-amount lv-proamt btn-go btn-change btn-add btn-cancel
            btn-delete.
    APPLY "entry" TO lv-vend-no.
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
        "You will be returned to the selection screen to correct this."    
        VIEW-AS ALERT-BOX INFO BUTTONS OK-CANCEL.
  RETURN lNegativeFound.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

