&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\ar0purge.w

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

DEF STREAM excel.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-856 NO-UNDO
    FIELD trans-date AS DATE INIT TODAY
    FIELD trans-time AS CHAR
    FIELD supplier-no AS CHAR INIT "49783"
    FIELD prod-no AS CHAR INIT "04498051"
    FIELD i-no AS CHAR
    FIELD prod-descr AS CHAR
    FIELD po-no AS CHAR
    FIELD po-date AS DATE
    FIELD ord-no AS INT
    FIELD ship-date AS DATE
    FIELD expected-del-date AS DATE
    FIELD delivery-mode AS CHAR INIT "L"
    FIELD prod-date AS DATE
    FIELD expiry-date AS DATE
    FIELD shipped-qty AS DEC
    FIELD shipped-qty-uom AS CHAR INIT "CA"
    FIELD product-price AS DEC
    FIELD currency_code AS CHAR INIT "USD"
    FIELD cust-name AS CHAR INIT "Hub One Logistics"
    FIELD cust-no AS CHAR
    FIELD addr-1 AS CHAR
    FIELD addr-2 AS CHAR
    FIELD city AS CHAR
    FIELD state AS CHAR
    FIELD postal-code AS CHAR
    FIELD country-code AS CHAR INIT "USA"
    FIELD loc-code AS CHAR INIT "DC".

DEF TEMP-TABLE tt-boll NO-UNDO
    FIELD i-no AS CHAR
    FIELD LINE AS INT
    FIELD job-no AS CHAR
    FIELD job-no2 AS INT
    FIELD ord-no AS INT
    FIELD po-no AS CHAR
    FIELD ship-state AS CHAR
    FIELD addr-1 AS CHAR
    FIELD addr-2 AS CHAR
    FIELD city AS CHAR
    FIELD state AS CHAR
    FIELD zip AS CHAR
    INDEX ord-no ord-no po-no.

DEF VAR v-ship-time AS INT NO-UNDO.
DEF VAR v-ship-addr1 AS CHAR NO-UNDO.
DEF VAR v-ship-addr2 AS CHAR NO-UNDO.
DEF VAR v-ship-city AS CHAR NO-UNDO.
DEF VAR v-ship-state AS CHAR NO-UNDO.
DEF VAR v-ship-zip AS CHAR NO-UNDO.
DEF VAR v-prod-date AS DATE NO-UNDO.
DEF VAR v-ship-qty AS DEC NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_i-no scr-ship-date fi_file btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no scr-ship-date fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "O&k" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\856.csv" 
     LABEL "CSV File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE scr-ship-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 2.91 COL 34 COLON-ALIGNED HELP
          "Enter Item #" WIDGET-ID 2
     scr-ship-date AT ROW 4.1 COL 34 COLON-ALIGNED HELP
          "Enter Ship Date" WIDGET-ID 4
     fi_file AT ROW 6 COL 27 COLON-ALIGNED HELP
          "Enter CSV File Name" WIDGET-ID 6
     btn-process AT ROW 8.38 COL 21
     btn-cancel AT ROW 8.38 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2.6 WIDGET-ID 8
          BGCOLOR 2 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 12.5.


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
         TITLE              = "Metro 856 Report"
         HEIGHT             = 12.52
         WIDTH              = 90
         MAX-HEIGHT         = 12.52
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 12.52
         VIRTUAL-WIDTH      = 90
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Metro 856 Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Metro 856 Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Item# */
DO:
  assign {&self-name}.
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
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Ok */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&displayed-objects}.
   END.

   EMPTY TEMP-TABLE tt-856.
   EMPTY TEMP-TABLE tt-boll.

   SESSION:SET-WAIT-STATE ("general").

   OUTPUT STREAM excel TO VALUE(fi_file).

   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

   FIND FIRST itemfg WHERE
        itemfg.company EQ cocode AND
        itemfg.i-no EQ begin_i-no
        NO-LOCK NO-ERROR.

   FOR EACH oe-bolh FIELDS(company cust-no ship-id b-no) WHERE
       oe-bolh.company EQ cocode AND
       oe-bolh.bol-date EQ scr-ship-date AND
       oe-bolh.posted = YES
       USE-INDEX bol-date
       NO-LOCK:

       RELEASE shipto.

       RUN oe/custxship.p (oe-bolh.company,
                          oe-bolh.cust-no,
                          oe-bolh.ship-id,
                          BUFFER shipto).

      if avail shipto then
         assign
            v-ship-addr1 = shipto.ship-addr[1]
            v-ship-addr2 = shipto.ship-addr[2]
            v-ship-city = shipto.ship-city
            v-ship-state = shipto.ship-state
            v-ship-zip   = shipto.ship-zip.
      ELSE
         assign 
            v-ship-addr1 = ""
            v-ship-addr2 = ""
            v-ship-city = ""
            v-ship-state = ""
            v-ship-zip   = "".

       FOR EACH oe-boll WHERE
            oe-boll.company EQ oe-bolh.company AND
            oe-boll.b-no EQ oe-bolh.b-no AND
            oe-boll.i-no EQ begin_i-no
            NO-LOCK:

            CREATE tt-boll.
            BUFFER-COPY oe-boll TO tt-boll
               ASSIGN tt-boll.state = v-ship-state
                      tt-boll.addr-1 = v-ship-addr1
                      tt-boll.addr-2 = v-ship-addr2
                      tt-boll.city = v-ship-city
                      tt-boll.state = v-ship-state
                      tt-boll.zip = v-ship-zip.

            RELEASE tt-boll.
       END.
   END.

   FOR EACH tt-boll
       BREAK BY tt-boll.ord-no
             BY tt-boll.po-no:

       IF FIRST-OF(tt-boll.po-no) THEN
       DO:
          CREATE tt-856.
          ASSIGN
             v-ship-time = 0
             v-ship-qty = 0.

          FOR EACH fg-rcpth fields(rita-code r-no) WHERE
              fg-rcpth.company EQ cocode AND
              fg-rcpth.i-no EQ begin_i-no AND
              fg-rcpth.rita-code EQ "S" AND
              fg-rcpth.job-no EQ tt-boll.job-no AND
              fg-rcpth.job-no2 EQ tt-boll.job-no2 AND
              fg-rcpth.trans-date EQ scr-ship-date AND
              fg-rcpth.job-no-to EQ tt-boll.po-no
              USE-INDEX job
              NO-LOCK,
              FIRST fg-rdtlh FIELDS(trans-time qty) WHERE
                    fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                    fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                    NO-LOCK:

              IF fg-rdtlh.trans-time GT v-ship-time THEN
                 v-ship-time = fg-rdtlh.trans-time.

              v-ship-qty = v-ship-qty + fg-rdtlh.qty.
          END.

          ASSIGN
             tt-856.trans-date = scr-ship-date
             tt-856.trans-time = REPLACE(STRING(v-ship-time,"HH:MM"),":","")
             tt-856.i-no = itemfg.i-no
             tt-856.prod-descr = itemfg.i-name
             tt-856.po-no = tt-boll.po-no
             tt-856.ord-no  = tt-boll.ord-no
             tt-856.ship-date = scr-ship-date
             tt-856.expected-del-date = IF tt-boll.state EQ "PA" THEN tt-856.ship-date
                                        ELSE tt-856.ship-date + 2
             tt-856.expiry-date = tt-856.ship-date + 358
             tt-856.shipped-qty = v-ship-qty / 1000.0
             tt-856.state     = tt-boll.state
             tt-856.addr-1    = tt-boll.addr-1
             tt-856.addr-2    = tt-boll.addr-2
             tt-856.city      = tt-boll.city
             tt-856.state     = tt-boll.state
             tt-856.postal-code = tt-boll.zip
             tt-856.cust-no   = IF tt-856.state EQ "PA" THEN "40" ELSE "53"
             v-prod-date = ?.

          {sys/inc/roundup.i tt-856.shipped-qty}

          FOR EACH fg-rcpth FIELDS (r-no trans-date) NO-LOCK
              WHERE fg-rcpth.company      EQ cocode
                AND fg-rcpth.i-no         EQ itemfg.i-no
                AND fg-rcpth.job-no       EQ tt-boll.job-no
                AND fg-rcpth.job-no2      EQ tt-boll.job-no2,
               EACH fg-rdtlh NO-LOCK
              WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no           
                AND fg-rdtlh.rita-code    EQ "R"
              BREAK BY fg-rcpth.trans-date
                    BY fg-rdtlh.trans-time
                    BY fg-rcpth.r-no:

              ASSIGN v-prod-date = fg-rcpth.trans-date.
              LEAVE.      
          END.

          IF v-prod-date = ? THEN
             v-prod-date = scr-ship-date.

          tt-856.prod-date = v-prod-date.

          FIND FIRST oe-ordl WHERE
               oe-ordl.company EQ cocode AND
               oe-ordl.ord-no EQ tt-boll.ord-no AND
               oe-ordl.i-no EQ tt-boll.i-no AND
               oe-ordl.LINE EQ tt-boll.LINE
               NO-LOCK NO-ERROR.

          IF AVAIL oe-ordl THEN
             tt-856.product-price = oe-ordl.price.

          RELEASE tt-856.
       END.
   END.

   FOR EACH tt-856:

       PUT STREAM excel UNFORMATTED
            '"' STRING(YEAR(tt-856.trans-date),"9999") +
                STRING(MONTH(tt-856.trans-date),"99") +
                STRING(DAY(tt-856.trans-date),"99")              '",'
            '"' tt-856.trans-time                                '",'
            '"' tt-856.supplier-no                               '",'
            '"' tt-856.prod-no                                   '",'
            '"' tt-856.prod-descr                                '",'
            '"' tt-856.i-no                                      '",'
            '"' tt-856.po-no                                     '",'
            '"' /*p.o. date*/                                    '",'
            '"'  STRING(tt-856.ord-no)                           '",'
            '"'  STRING(YEAR(tt-856.ship-date),"9999") +
                 STRING(MONTH(tt-856.ship-date),"99") +
                 STRING(DAY(tt-856.ship-date),"99")             '",'
            '"'  STRING(YEAR(tt-856.expected-del-date),"9999") +
                 STRING(MONTH(tt-856.expected-del-date),"99") +
                 STRING(DAY(tt-856.expected-del-date),"99")      '",'
            '"'  tt-856.delivery-mode                            '",'
            '"'  (IF tt-856.prod-date NE ? THEN
                    STRING(YEAR(tt-856.prod-date),"9999") +
                    STRING(MONTH(tt-856.prod-date),"99") +
                    STRING(DAY(tt-856.prod-date),"99")
                    ELSE "")              '",'
            '"'  STRING(YEAR(tt-856.expiry-date),"9999") +
                 STRING(MONTH(tt-856.expiry-date),"99") +
                 STRING(DAY(tt-856.expiry-date),"99")            '",'
            '"'  STRING(INTEGER(tt-856.shipped-qty))             '",'
            '"'  tt-856.shipped-qty-uom                          '",'
            '"'  STRING(tt-856.product-price)                    '",'
            '"'  tt-856.currency_code                            '",'
            '"'  tt-856.cust-name                                '",'
            '"'  tt-856.cust-no                                  '",'
            '"'  tt-856.addr-1                                   '",'
            '"'  tt-856.addr-2                                   '",'
            '"'  tt-856.city                                     '",'
            '"'  tt-856.state                                    '",'
            '"'  tt-856.postal-code                              '",'
            '"'  tt-856.country-code                             '",'
            '"'  tt-856.loc-code                                 '",'
            '"'  '",'
            SKIP.
   END.

   OUTPUT STREAM excel CLOSE.

   SESSION:SET-WAIT-STATE ("").

   MESSAGE "CSV File Created."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* CSV File Name */
DO:
     assign {&self-name}.
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

  RUN enable_UI.
  {methods/nowait.i}

  excelheader = "Transaction_Date,Transaction_Time,Supplier_Number,Product_Number,"
               + "Supplier_Product_Number,Product_Description,Purchase_Order_No,"
               + "Purchase_Order_Date,Sales_Order_No,Shipment_Date,Expected_Delivery_Date,"
               + "Delivery_Mode,Production_Date,Expiry_Date,Shipped_Quantity,Shipped_Quantity_UOM,"
               + "Product_Price,Transaction_Currency,Customer_Name,Customer_Number,Customer_Address_1,"
               + "Customer_Address_2,City,State,Postal_Code,Country_Code,Location_Code,EDI_Mailbox".

  DO WITH FRAME {&FRAME-NAME}:
     {custom/usrprint.i}
     APPLY "entry" TO scr-ship-date IN FRAME {&FRAME-NAME}.
  END.

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
  DISPLAY begin_i-no scr-ship-date fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_i-no scr-ship-date fi_file btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

