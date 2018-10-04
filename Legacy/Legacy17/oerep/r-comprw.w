&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep/r-comprw.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iplUseInvoiceCost AS LOGICAL.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
   cocode = gcompany
   locode = gloc.

{ oerep/tt-comm-calc.i SHARED}
{ oerep/tt-commission.i "NEW SHARED" }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-comm-calc

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-comm-calc.slsm[1] tt-comm-calc.cust-no tt-comm-calc.inv-no tt-comm-calc.i-no tt-comm-calc.procat tt-comm-calc.set-sales-price * tt-comm-calc.set-sell-price-qty tt-comm-calc.commission-cost * tt-comm-calc.base-cost-qty tt-comm-calc.tot-item-resale tt-comm-calc.total-costs tt-comm-calc.profit-margin tt-comm-calc.commission   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-comm-calc                               BY tt-comm-calc.slsm[1]                               BY tt-comm-calc.cust-no                               BY tt-comm-calc.inv-no
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-comm-calc                               BY tt-comm-calc.slsm[1]                               BY tt-comm-calc.cust-no                               BY tt-comm-calc.inv-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-comm-calc
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-comm-calc


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-base-cost FI-fixed-gross-profit ~
FI-rebate-percent FI-sname FI-overhead FI-warehouse FI-set-sales-price ~
FI-misc FI-freight FI-base-cost-uom FI-industrial FI-comm-rate BROWSE-1 ~
FI-inv-no FI-qty FI-inv-price FI-ssp-uom 
&Scoped-Define DISPLAYED-OBJECTS FI-base-cost FI-fixed-gross-profit ~
FI-rebate-percent FI-sname FI-overhead FI-warehouse FI-set-sales-price ~
FI-misc FI-freight FI-base-cost-uom FI-industrial FI-comm-rate FI-inv-no ~
FI-qty FI-inv-price FI-ssp-uom 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-base-cost AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Base Cost" 
      VIEW-AS TEXT 
     SIZE 22.8 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-base-cost-uom AS CHARACTER FORMAT "X(256)":U 
     LABEL "UOM" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FI-comm-rate AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Commission Rate %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-fixed-gross-profit AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Fixed Gross Profit %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-freight AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Freight %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-industrial AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Industrial %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-inv-no AS DECIMAL FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Invoice No." 
      VIEW-AS TEXT 
     SIZE 10.4 BY .62 NO-UNDO.

DEFINE VARIABLE FI-inv-price AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Invoice Price" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE FI-misc AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Misc %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-overhead AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Overhead %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-qty AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Invoice Quantity" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE FI-rebate-percent AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Rebate%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-set-sales-price AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Set Sales Price" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-sname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Salesman" 
      VIEW-AS TEXT 
     SIZE 32 BY .62 NO-UNDO.

DEFINE VARIABLE FI-ssp-uom AS CHARACTER FORMAT "X(256)":U 
     LABEL "UOM" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FI-warehouse AS DECIMAL FORMAT "->>>9.99":U INITIAL 0 
     LABEL "Warehouse %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-comm-calc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-comm-calc.slsm[1]                                COLUMN-LABEL "SMan"
tt-comm-calc.cust-no                FORMAT "X(10)"  COLUMN-LABEL "Customer" 
tt-comm-calc.inv-no                 FORMAT ">>>>>9" COLUMN-LABEL "Invoice!Number"
tt-comm-calc.i-no                   
tt-comm-calc.procat
tt-comm-calc.set-sales-price     * tt-comm-calc.set-sell-price-qty  FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Set!Sales Price"
tt-comm-calc.commission-cost     * tt-comm-calc.base-cost-qty       FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Commission!Cost"
tt-comm-calc.tot-item-resale        FORMAT "->>,>>>,>>9.99"
tt-comm-calc.total-costs            FORMAT "->>,>>>,>>9.99" 
tt-comm-calc.profit-margin          FORMAT "->>,>>>,>>9.99"
tt-comm-calc.commission             FORMAT "->>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 146.6 BY 20.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-base-cost AT ROW 1.19 COL 98.4 COLON-ALIGNED
     FI-fixed-gross-profit AT ROW 2.24 COL 131.2 COLON-ALIGNED
     FI-rebate-percent AT ROW 3.48 COL 131.2 COLON-ALIGNED
     FI-sname AT ROW 2.19 COL 12 COLON-ALIGNED
     FI-overhead AT ROW 3.48 COL 22.8 COLON-ALIGNED
     FI-warehouse AT ROW 3.48 COL 52.8 COLON-ALIGNED
     FI-set-sales-price AT ROW 2.24 COL 67.8 COLON-ALIGNED
     FI-misc AT ROW 4.52 COL 22.8 COLON-ALIGNED
     FI-freight AT ROW 4.52 COL 52.8 COLON-ALIGNED
     FI-base-cost-uom AT ROW 1.19 COL 128.2 COLON-ALIGNED
     FI-industrial AT ROW 3.48 COL 87.2 COLON-ALIGNED
     FI-comm-rate AT ROW 4.52 COL 87.2 COLON-ALIGNED
     BROWSE-1 AT ROW 6 COL 2
     FI-inv-no AT ROW 1.19 COL 12 COLON-ALIGNED
     FI-qty AT ROW 1.19 COL 40.2 COLON-ALIGNED
     FI-inv-price AT ROW 1.19 COL 67.8 COLON-ALIGNED
     FI-ssp-uom AT ROW 2.19 COL 93.8 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.4 BY 25.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Commission Report"
         HEIGHT             = 25.29
         WIDTH              = 148.4
         MAX-HEIGHT         = 46.1
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.1
         VIRTUAL-WIDTH      = 256
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 FI-comm-rate DEFAULT-FRAME */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2
       BROWSE-1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-comm-calc
                              BY tt-comm-calc.slsm[1]
                              BY tt-comm-calc.cust-no
                              BY tt-comm-calc.inv-no
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Commission Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Commission Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON ITERATION-CHANGED OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
   IF AVAILABLE(tt-comm-calc) THEN DO:
      ASSIGN           
         FI-base-cost            = tt-comm-calc.base-cost
         FI-comm-rate            = tt-comm-calc.commission-rate
         FI-freight              = tt-comm-calc.freight
         FI-industrial           = tt-comm-calc.industrial 
         FI-misc                 = tt-comm-calc.misc
         FI-overhead             = tt-comm-calc.overhead
         FI-warehouse            = tt-comm-calc.warehouse
         FI-inv-no               = tt-comm-calc.inv-no
         FI-qty                  = tt-comm-calc.orig-inv-qty   
         FI-inv-price            = tt-comm-calc.amt  
         FI-base-cost-uom        = tt-comm-calc.base-cost-uom
         FI-SSP-uom              = tt-comm-calc.set-sell-price-uom
         FI-sname                = tt-comm-calc.sname
         FI-fixed-gross-profit   = tt-comm-calc.fixed-gross-profit
         FI-set-sales-price      = tt-comm-calc.set-sales-price
         FI-comm-rate            = tt-comm-calc.commission-rate
         FI-rebate-percent       = tt-comm-calc.rebate-percent.

      DISPLAY          
         FI-base-cost
         FI-comm-rate
         FI-freight
         FI-industrial 
         FI-misc
         FI-overhead
         FI-warehouse
         FI-inv-no
         FI-qty   
         FI-inv-price  
         FI-base-cost-uom
         FI-SSP-uom
         FI-sname
         FI-fixed-gross-profit
         FI-set-sales-price
         FI-rebate-percent
         WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-base-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-base-cost C-Win
ON LEAVE OF FI-base-cost IN FRAME DEFAULT-FRAME /* Base Cost */
OR RETURN OF FI-base-cost
DO:
   ASSIGN 
      FI-base-cost
      tt-comm-calc.base-cost = FI-base-cost.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-comm-rate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-comm-rate C-Win
ON LEAVE OF FI-comm-rate IN FRAME DEFAULT-FRAME /* Commission Rate % */
OR RETURN OF FI-comm-rate
DO:
   ASSIGN 
      FI-comm-rate
      tt-comm-calc.commission-rate = FI-comm-rate.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-fixed-gross-profit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-fixed-gross-profit C-Win
ON LEAVE OF FI-fixed-gross-profit IN FRAME DEFAULT-FRAME /* Fixed Gross Profit % */
OR RETURN OF FI-fixed-gross-profit
DO:
   ASSIGN 
      FI-fixed-gross-profit
      tt-comm-calc.fixed-gross-profit = FI-fixed-gross-profit.  
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-freight C-Win
ON LEAVE OF FI-freight IN FRAME DEFAULT-FRAME /* Freight % */
OR RETURN OF FI-freight
DO:
   ASSIGN 
      FI-freight
      tt-comm-calc.freight = FI-freight.
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-industrial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-industrial C-Win
ON LEAVE OF FI-industrial IN FRAME DEFAULT-FRAME /* Industrial % */
OR RETURN OF FI-industrial
DO:
   ASSIGN 
      FI-industrial
      tt-comm-calc.industrial = FI-industrial.
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-misc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-misc C-Win
ON LEAVE OF FI-misc IN FRAME DEFAULT-FRAME /* Misc % */
OR RETURN OF FI-misc
DO:
   ASSIGN 
      FI-misc
      tt-comm-calc.misc = FI-misc. 
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-overhead
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-overhead C-Win
ON LEAVE OF FI-overhead IN FRAME DEFAULT-FRAME /* Overhead % */
OR RETURN OF FI-overhead
DO:
   ASSIGN 
      FI-overhead
      tt-comm-calc.overhead = FI-overhead.  
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-rebate-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-rebate-percent C-Win
ON LEAVE OF FI-rebate-percent IN FRAME DEFAULT-FRAME /* Rebate% */
OR RETURN OF FI-rebate-percent
DO:
   ASSIGN 
      FI-rebate-percent
      tt-comm-calc.rebate-percent = FI-rebate-percent.
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-set-sales-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-set-sales-price C-Win
ON LEAVE OF FI-set-sales-price IN FRAME DEFAULT-FRAME /* Set Sales Price */
OR RETURN OF FI-set-sales-price
DO:
   ASSIGN 
      FI-set-sales-price
      tt-comm-calc.set-sales-price = FI-set-sales-price.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-warehouse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-warehouse C-Win
ON LEAVE OF FI-warehouse IN FRAME DEFAULT-FRAME /* Warehouse % */
OR RETURN OF FI-warehouse
DO:
   ASSIGN 
      FI-warehouse
      tt-comm-calc.warehouse = FI-warehouse.
   RUN cal-comm-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   /* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.

  {methods/nowait.i}
   RUN calculate-commission-costs.
   APPLY "ITERATION-CHANGED" TO BROWSE BROWSE-1.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cal-comm-rate C-Win 
PROCEDURE cal-comm-rate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
   ASSIGN {&displayed-objects}.
/*    IF tt-comm-calc.fixed-gross-profit > 0 THEN DO: */
      tt-comm-calc.commission-rate =  tt-comm-calc.fixed-gross-profit
                  - (tt-comm-calc.freight 
                  +  tt-comm-calc.industrial
                  +  tt-comm-calc.misc
                  +  tt-comm-calc.overhead
                  +  tt-comm-calc.warehouse).
/*    END. */
   ASSIGN FI-comm-rate = tt-comm-calc.commission-rate.

END.
DISPLAY FI-comm-rate WITH FRAME {&FRAME-NAME}.
RUN cal-commission.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cal-commission C-Win 
PROCEDURE cal-commission :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-percent    AS DECI NO-UNDO.
   DEF VAR v-qty        AS DECI NO-UNDO.
   DEF VAR v-rebate     AS DECI NO-UNDO.
   DEF VAR v-amt        AS DECI NO-UNDO.

   ASSIGN
      tt-comm-calc.amt = tt-comm-calc.orig-inv-amt
      v-rebate = (tt-comm-calc.amt * tt-comm-calc.rebate-percent) / 100
      tt-comm-calc.amt = tt-comm-calc.amt - v-rebate.
   ASSIGN
      tt-comm-calc.tot-item-resale = tt-comm-calc.amt * tt-comm-calc.orig-inv-qty /*tt-comm-calc.set-sell-price-qty*/
      tt-comm-calc.total-costs = tt-comm-calc.commission-cost * tt-comm-calc.base-cost-qty
      tt-comm-calc.profit-margin = tt-comm-calc.tot-item-resale - tt-comm-calc.total-costs
      tt-comm-calc.commission = (tt-comm-calc.profit-margin * tt-comm-calc.commission-rate) / 100.


   BROWSE browse-1:REFRESH().
   APPLY "ITERATION-CHANGED" TO BROWSE BROWSE-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculate-commission-costs C-Win 
PROCEDURE calculate-commission-costs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-percent    AS DECI NO-UNDO.
   DEF VAR v-qty        AS DECI NO-UNDO.
   DEF VAR v-rebate     AS DECI NO-UNDO.

   FOR EACH tt-comm-calc:



      FIND FIRST item-comm WHERE item-comm.company = tt-comm-calc.company
                             AND item-comm.cust-no = tt-comm-calc.cust-no
                             AND item-comm.i-no    = tt-comm-calc.i-no NO-LOCK NO-ERROR.
      IF AVAILABLE(item-comm) THEN DO:

         ASSIGN           
            tt-comm-calc.base-cost        =  item-comm.base-cost       
            tt-comm-calc.commission-rate  =  item-comm.comm-rate-percent
            tt-comm-calc.freight          =  item-comm.freight-percent
            tt-comm-calc.industrial       =  item-comm.industrial-percent 
            tt-comm-calc.misc             =  item-comm.misc-percent
            tt-comm-calc.overhead         =  item-comm.overhead-percent
            tt-comm-calc.warehouse        =  item-comm.warehouse-percent
            tt-comm-calc.fixed-gross-profit  = item-comm.fixed-gross-profit
            tt-comm-calc.set-sales-price     = item-comm.set-sales-price
/*             tt-comm-calc.commission-cost     = tt-comm-calc.base-cost */
            tt-comm-calc.rebate-percent      = item-comm.zz-dec[1]
            tt-comm-calc.set-sell-price-uom  = item-comm.zz-char[2]
            tt-comm-calc.base-cost-uom       = item-comm.zz-char[3]
            tt-comm-calc.base-cost-qty       = tt-comm-calc.qty
            tt-comm-calc.set-sell-price-qty  = tt-comm-calc.qty.


         IF item-comm.zz-char[2] = "" THEN
            tt-comm-calc.set-sell-price-uom = tt-comm-calc.pr-uom.
         IF item-comm.zz-char[3] = "" THEN
            tt-comm-calc.base-cost-uom = tt-comm-calc.cost-uom.

         /*if "locked" or not set to use invoice costs, use base cost from OF7
         tt-comm-calc.commission cost is set to invoice costs by default*/
         IF NOT iplUseInvoiceCost 
             OR item-comm.zz-char[4] = "YES" 
             OR tt-comm-calc.commission-cost = 0 THEN 
             tt-comm-calc.commission-cost = tt-comm-calc.base-cost.
         ELSE
             tt-comm-calc.base-cost-uom = tt-comm-calc.cost-uom.

         FIND FIRST itemfg
                   {sys/look/itemfgrlW.i}
               AND itemfg.i-no EQ tt-comm-calc.i-no NO-LOCK NO-ERROR.

/*          IF tt-comm-calc.qty > 0 THEN DO:                                                                */
/*             ASSIGN                                                                                       */
/*                tt-comm-calc.qty = IF tt-comm-calc.pr-uom BEGINS "L" AND tt-comm-calc.pr-uom NE "LB" THEN */
/*                           IF tt-comm-calc.qty LT 0 THEN -1 ELSE 1                                        */
/*                        ELSE                                                                              */
/*                           IF tt-comm-calc.pr-uom EQ "CS" THEN                                            */
/*                              tt-comm-calc.qty / (IF tt-comm-calc.cas-cnt NE 0 THEN                       */
/*                                                     tt-comm-calc.cas-cnt                                 */
/*                                                  ELSE                                                    */
/*                                                     IF AVAIL itemfg AND itemfg.case-count NE 0 THEN      */
/*                                                        itemfg.case-count                                 */
/*                                                     ELSE                                                 */
/*                                                     1)                                                   */
/*                           ELSE                                                                           */
/*                              IF tt-comm-calc.pr-uom EQ "C" THEN                                          */
/*                                 tt-comm-calc.qty / 100                                                   */
/*                              ELSE                                                                        */
/*                                 IF tt-comm-calc.pr-uom EQ "M" THEN                                       */
/*                                    tt-comm-calc.qty / 1000                                               */
/*                                 ELSE                                                                     */
/*                                    tt-comm-calc.qty.                                                     */

         IF tt-comm-calc.orig-inv-qty > 0 THEN DO:
            ASSIGN
               tt-comm-calc.orig-inv-qty = IF tt-comm-calc.uom BEGINS "L" AND tt-comm-calc.uom NE "LB" THEN
                          IF tt-comm-calc.orig-inv-qty LT 0 THEN -1 ELSE 1
                       ELSE
                          IF tt-comm-calc.uom EQ "CS" THEN
                             tt-comm-calc.orig-inv-qty / (IF tt-comm-calc.cas-cnt NE 0 THEN
                                                    tt-comm-calc.cas-cnt
                                                 ELSE
                                                    IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
                                                       itemfg.case-count
                                                    ELSE
                                                    1)
                          ELSE
                             IF tt-comm-calc.uom EQ "C" THEN
                                tt-comm-calc.orig-inv-qty / 100
                             ELSE
                                IF tt-comm-calc.uom EQ "M" THEN
                                   tt-comm-calc.orig-inv-qty / 1000
                                ELSE
                                   tt-comm-calc.orig-inv-qty.
         END.
         IF tt-comm-calc.base-cost-qty > 0 THEN DO:
            ASSIGN
               tt-comm-calc.base-cost-qty = IF tt-comm-calc.base-cost-uom BEGINS "L" AND tt-comm-calc.base-cost-uom NE "LB" THEN
                          IF tt-comm-calc.base-cost-qty LT 0 THEN -1 ELSE 1
                       ELSE
                          IF tt-comm-calc.base-cost-uom EQ "CS" THEN
                             tt-comm-calc.base-cost-qty / (IF tt-comm-calc.cas-cnt NE 0 THEN
                                                    tt-comm-calc.cas-cnt
                                                 ELSE
                                                    IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
                                                       itemfg.case-count
                                                    ELSE
                                                    1)
                          ELSE
                             IF tt-comm-calc.base-cost-uom EQ "C" THEN
                                tt-comm-calc.base-cost-qty / 100
                             ELSE
                                IF tt-comm-calc.base-cost-uom EQ "M" THEN
                                   tt-comm-calc.base-cost-qty / 1000
                                ELSE
                                   tt-comm-calc.base-cost-qty.
         END.

         IF tt-comm-calc.set-sell-price-qty > 0 THEN DO:
            ASSIGN
               tt-comm-calc.set-sell-price-qty = IF tt-comm-calc.set-sell-price-uom BEGINS "L" AND tt-comm-calc.set-sell-price-uom NE "LB" THEN
                          IF tt-comm-calc.set-sell-price-qty LT 0 THEN -1 ELSE 1
                       ELSE
                          IF tt-comm-calc.set-sell-price-uom EQ "CS" THEN
                             tt-comm-calc.set-sell-price-qty / (IF tt-comm-calc.cas-cnt NE 0 THEN
                                                    tt-comm-calc.cas-cnt
                                                 ELSE
                                                    IF AVAIL itemfg AND itemfg.case-count NE 0 THEN
                                                       itemfg.case-count
                                                    ELSE
                                                    1)
                          ELSE
                             IF tt-comm-calc.set-sell-price-uom EQ "C" THEN
                                tt-comm-calc.set-sell-price-qty / 100
                             ELSE
                                IF tt-comm-calc.set-sell-price-uom EQ "M" THEN
                                   tt-comm-calc.set-sell-price-qty / 1000
                                ELSE
                                   tt-comm-calc.set-sell-price-qty.
         END.
            RUN cal-commission.
/*          END. */
      END.
   END.
   BROWSE browse-1:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FI-base-cost FI-fixed-gross-profit FI-rebate-percent FI-sname 
          FI-overhead FI-warehouse FI-set-sales-price FI-misc FI-freight 
          FI-base-cost-uom FI-industrial FI-comm-rate FI-inv-no FI-qty 
          FI-inv-price FI-ssp-uom 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-base-cost FI-fixed-gross-profit FI-rebate-percent FI-sname 
         FI-overhead FI-warehouse FI-set-sales-price FI-misc FI-freight 
         FI-base-cost-uom FI-industrial FI-comm-rate BROWSE-1 FI-inv-no FI-qty 
         FI-inv-price FI-ssp-uom 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

