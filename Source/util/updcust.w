&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/rctd-purge.w

  Description: Delete "C" status receipts

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: S Brooks

  Created: 12/28/11

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiBeginCust fiEndCust fiOrderLimit ~
fiCreditLimit fiTerms fiGraceDays fiGraceDollars fiWhseDays fiLocation ~
frt-pay fob-code fiTaxCode fiCarrier fiDeliveryZone fiUnder-Pct fiOver-Pct ~
fiWarehouse fiBin btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fiBeginCust fiEndCust fiOrderLimit ~
fiCreditLimit fiTerms fiGraceDays fiGraceDollars fiWhseDays fiLocation ~
frt-pay fob-code fiTaxCode fiCarrier fiDeliveryZone fiUnder-Pct fiOver-Pct ~
fiWarehouse fiBin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valid-data C-Win 
FUNCTION valid-data RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiBeginCust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiBin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCarrier AS CHARACTER FORMAT "X(5)":U 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCreditLimit AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Credit Limit" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiDeliveryZone AS CHARACTER FORMAT "X(5)":U 
     LABEL "Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndCust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Ending Cust" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiGraceDays AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Grace Days" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiGraceDollars AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Grace Dollars" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(5)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrderLimit AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Order Limit" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiOver-Pct AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Overrun%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTaxCode AS CHARACTER FORMAT "X(3)":U 
     LABEL "Tax Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTerms AS CHARACTER FORMAT "X(5)":U 
     LABEL "Payment Terms" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiUnder-Pct AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Underrun%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiWarehouse AS CHARACTER FORMAT "X(5)":U 
     LABEL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhseDays AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Whse Days" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fob-code AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "No Update", "",
"Destination", "DEST",
"Origin", "ORIG"
     SIZE 41.4 BY 1.19 NO-UNDO.

DEFINE VARIABLE frt-pay AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "No Update", "",
"Bill", "B",
"Collect", "C",
"Prepaid", "P",
"3rd Party", "T"
     SIZE 58.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 20.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiBeginCust AT ROW 6.71 COL 22.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     fiEndCust AT ROW 6.71 COL 58.8 COLON-ALIGNED HELP
          "Enter ending customer number"
     fiOrderLimit AT ROW 8.38 COL 22.8 COLON-ALIGNED WIDGET-ID 2
     fiCreditLimit AT ROW 9.57 COL 22.8 COLON-ALIGNED WIDGET-ID 4
     fiTerms AT ROW 10.76 COL 22.8 COLON-ALIGNED WIDGET-ID 6
     fiGraceDays AT ROW 11.95 COL 22.8 COLON-ALIGNED WIDGET-ID 8
     fiGraceDollars AT ROW 13.14 COL 22.8 COLON-ALIGNED WIDGET-ID 38
     fiWhseDays AT ROW 14.62 COL 22.8 COLON-ALIGNED WIDGET-ID 10
     fiLocation AT ROW 15.81 COL 22.8 COLON-ALIGNED WIDGET-ID 12
     frt-pay AT ROW 17.05 COL 26.4 NO-LABEL WIDGET-ID 14
     fob-code AT ROW 17.91 COL 26.4 NO-LABEL WIDGET-ID 22
     fiTaxCode AT ROW 19.29 COL 22.8 COLON-ALIGNED WIDGET-ID 28
     fiCarrier AT ROW 20.48 COL 22.8 COLON-ALIGNED WIDGET-ID 30
     fiDeliveryZone AT ROW 21.67 COL 22.8 COLON-ALIGNED WIDGET-ID 32
     fiUnder-Pct AT ROW 19.24 COL 56 COLON-ALIGNED WIDGET-ID 34
     fiOver-Pct AT ROW 20.33 COL 56 COLON-ALIGNED WIDGET-ID 36
     fiWarehouse AT ROW 23.38 COL 23 COLON-ALIGNED WIDGET-ID 42
     fiBin AT ROW 23.38 COL 56 COLON-ALIGNED WIDGET-ID 44
     btn-process AT ROW 26.48 COL 21
     btn-cancel AT ROW 26.48 COL 53
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 18.14 COL 19.8 WIDGET-ID 26
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "Freight Payment:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 17.05 COL 8.6 WIDGET-ID 20
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 27.76.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.52 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.48 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "** Zero values will not be updated **" VIEW-AS TEXT
          SIZE 44 BY .95 AT ROW 3.57 COL 21 WIDGET-ID 2
          BGCOLOR 11 FGCOLOR 12 FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Update Customers"
         HEIGHT             = 27.76
         WIDTH              = 90
         MAX-HEIGHT         = 28.19
         MAX-WIDTH          = 108
         VIRTUAL-HEIGHT     = 28.19
         VIRTUAL-WIDTH      = 108
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".



DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (fiBeginCust:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Customers */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
    run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.


  RUN enable_UI.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fiBeginCust fiEndCust fiOrderLimit fiCreditLimit fiTerms fiGraceDays 
          fiGraceDollars fiWhseDays fiLocation frt-pay fob-code fiTaxCode 
          fiCarrier fiDeliveryZone fiUnder-Pct fiOver-Pct fiWarehouse fiBin 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fiBeginCust fiEndCust fiOrderLimit fiCreditLimit fiTerms fiGraceDays 
         fiGraceDollars fiWhseDays fiLocation frt-pay fob-code fiTaxCode 
         fiCarrier fiDeliveryZone fiUnder-Pct fiOver-Pct fiWarehouse fiBin 
         btn-process btn-cancel RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF var v-first-cust like cust.cust-no no-undo.
DEF var v-last-cust like cust.cust-no no-undo.
DEF VAR viCount AS INT NO-UNDO INIT 0.


    /* If entries are not valid, then abort. */
    IF NOT valid-data() THEN RETURN.

    /* Prompt if user wishes to update the entered range of customers. */
    MESSAGE "Are you sure you want to update the customers within the " +
            "selection parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

    /* If not, then abort. */
    IF NOT v-process THEN RETURN.


    DO WITH FRAME {&FRAME-NAME}:


      SESSION:set-wait-state("General").

      /* Save the range of customers. */
      ASSIGN v-first-cust = fiBeginCust:SCREEN-VALUE
             v-last-cust  = fiEndCust:SCREEN-VALUE.

      DO TRANSACTION ON ERROR UNDO, RETURN:

          /* Process each customer in the range. */
          FOR EACH cust EXCLUSIVE-LOCK
              WHERE cust.company = cocode 
                AND cust.cust-no >= v-first-cust
                AND cust.cust-no <= v-last-cust:

              /* Assign values if not blank or zero. */
              ASSIGN cust.ord-lim  = IF DEC(fiOrderLimit:SCREEN-VALUE) <> 0 THEN DEC(fiOrderLimit:SCREEN-VALUE) ELSE cust.ord-lim
                     cust.cr-lim   = IF DEC(fiCreditLimit:SCREEN-VALUE) <> 0 THEN DEC(fiCreditLimit:SCREEN-VALUE) ELSE cust.cr-lim
                     cust.terms    = IF fiTerms:SCREEN-VALUE <> "" THEN fiTerms:SCREEN-VALUE ELSE cust.terms
                     cust.cr-hold-invdays  = IF INT(fiGraceDays:SCREEN-VALUE) <> 0 THEN INT(fiGraceDays:SCREEN-VALUE) ELSE cust.cr-hold-invdays
                     cust.cr-hold-invdue   = IF DEC(fiGraceDollars:SCREEN-VALUE) <> 0 THEN DEC(fiGraceDollars:SCREEN-VALUE) ELSE cust.cr-hold-invdue
                     cust.ship-days  = IF INT(fiWhseDays:SCREEN-VALUE) <> 0 THEN INT(fiWhseDays:SCREEN-VALUE) ELSE cust.ship-days
                     cust.loc        = IF fiLocation:SCREEN-VALUE <> "" THEN fiLocation:SCREEN-VALUE ELSE cust.loc
                     cust.frt-pay    = IF frt-pay:SCREEN-VALUE = "" THEN cust.frt-pay ELSE frt-pay:SCREEN-VALUE
                     cust.fob-code   = IF fob-code:SCREEN-VALUE = "" THEN cust.fob-code ELSE fob-code:SCREEN-VALUE
                     cust.tax-gr     = IF fiTaxCode:SCREEN-VALUE = "" THEN cust.tax-gr ELSE fiTaxCode:SCREEN-VALUE
                     cust.carrier    = IF fiCarrier:SCREEN-VALUE = "" THEN cust.carrier ELSE fiCarrier:SCREEN-VALUE
                     cust.del-zone   = IF fiDeliveryZone:SCREEN-VALUE = "" THEN cust.del-zone ELSE fiDeliveryZone:SCREEN-VALUE
                     cust.under-pct  = IF DEC(fiUnder-Pct:SCREEN-VALUE) <> 0 THEN DEC(fiUnder-Pct:SCREEN-VALUE) ELSE cust.under-pct
                     cust.over-pct   = IF DEC(fiOver-Pct:SCREEN-VALUE) <> 0 THEN DEC(fiOver-Pct:SCREEN-VALUE) ELSE cust.over-pct.

              /* Count number of customers processed. */
              ASSIGN viCount = viCount + 1.

              /* If warehouse/bin information entered, then update shipto records. */
              IF fiWarehouse:SCREEN-VALUE <> "" OR fiBin:SCREEN-VALUE <> "" THEN DO:

                  FOR EACH shipto OF cust EXCLUSIVE-LOCK:
                      ASSIGN shipto.loc = IF fiWarehouse:SCREEN-VALUE <> "" THEN fiWarehouse:SCREEN-VALUE ELSE shipto.loc
                             shipto.loc-bin = IF fiBin:SCREEN-VALUE <> "" THEN fiBin:SCREEN-VALUE ELSE shipto.loc-bin.
                  END.
              END.

          END. /* for each cust */

      END. /* DO TRANSACTION */

    END. /*  DO WITH FRAME */

  SESSION:set-wait-state("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." SKIP
      viCount " customers updated."
      VIEW-AS ALERT-BOX.

  APPLY "close" TO THIS-PROCEDURE.

  RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valid-data C-Win 
FUNCTION valid-data RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Validate entries
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      /* Customer Range */
      IF fiBeginCust:SCREEN-VALUE = "" THEN DO:
          MESSAGE "Invalid beginning customer."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fiBeginCust.
          RETURN FALSE.
      END.

      IF fiEndCust:SCREEN-VALUE = "" THEN DO:
          MESSAGE "Invalid ending customer."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fiEndCust.
          RETURN FALSE.
      END.

      IF fiBeginCust:SCREEN-VALUE > fiEndCust:SCREEN-VALUE THEN DO:
          MESSAGE "Beginning customer cannot exceed ending customer."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fiBeginCust.
          RETURN FALSE.
      END.

      /* Terms */
      IF fiTerms:SCREEN-VALUE <> "" and
          NOT CAN-FIND(FIRST terms WHERE terms.t-code = fiterms:SCREEN-VALUE) THEN DO:
          MESSAGE "Invalid Terms." VIEW-AS ALERT-BOX ERROR.
          APPLY 'entry' TO fiTerms.
          RETURN FALSE.
    END.

    /* Location */
    IF fiLocation:screen-value <> "" AND
        NOT CAN-FIND(first loc where loc.company = gcompany and 
                                     loc.loc = fiLocation:screen-value) THEN DO:
        MESSAGE "Invalid Location"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiLocation.
        RETURN FALSE.
    END.

    /* Tax Code */
    IF fiTaxCode:SCREEN-VALUE <> "" AND
        NOT can-find(first stax-group where stax-group.tax-group = fiTaxCode:screen-value) THEN DO:
        MESSAGE "Invalid Tax Code"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiTaxCode.
        RETURN FALSE.
    END.
    /* Carrier */
    IF /*fiLocation:screen-value <> "" AND*/ fiCarrier:SCREEN-VALUE <> "" AND
        NOT can-find(first carrier where carrier.company = gcompany and 
/*                                      carrier.loc = fiLocation:screen-value and */
                                     carrier.carrier = fiCarrier:screen-value) THEN DO:
        MESSAGE "Invalid Carrier"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiCarrier.
        RETURN FALSE.
    END.

    /* Delivery Zone */
    IF /*fiLocation:screen-value <> "" AND fiCarrier:SCREEN-VALUE <> "" AND */ 
        fiDeliveryZone:SCREEN-VALUE <> "" AND
        NOT CAN-FIND(first carr-mtx where carr-mtx.company = gcompany and 
/*                                      carr-mtx.loc = fiLocation:screen-value and     */
/*                                      carr-mtx.carrier = fiCarrier:screen-value and  */
                                     carr-mtx.del-zone = fiDeliveryzone:screen-value) THEN DO:
        MESSAGE "Invalid Delivery Zone"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiDeliveryZone.
        RETURN FALSE.
    END.

    /* Warehouse */
    IF fiWarehouse:SCREEN-VALUE <> "" AND 
        NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                      AND loc.loc     EQ fiWarehouse:SCREEN-VALUE) THEN DO:
        MESSAGE "Invalid Warehouse"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiWarehouse.
        RETURN FALSE.
    END.

    /* Bin */

    IF fiBin:SCREEN-VALUE <> "" AND 
        NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode AND 
/*                           fg-bin.loc     EQ fiWarehouse:SCREEN-VALUE AND */
                          fg-bin.loc-bin EQ fiBin:SCREEN-VALUE) THEN DO:
        MESSAGE "Invalid Bin"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fiBin.
        RETURN FALSE.
    END.

  END. /* do with frame */

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

