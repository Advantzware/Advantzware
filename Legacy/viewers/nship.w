&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: viewers\nship.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER lv-ship-id    LIKE shipto.ship-id    NO-UNDO.
DEF INPUT PARAMETER lv-ship-name  LIKE shipto.ship-name  NO-UNDO.
DEF INPUT PARAMETER lv-ship-state LIKE shipto.ship-state NO-UNDO.

/* Local Variable Definitions ---                                       */

{sys/inc/var.i new shared}
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

ASSIGN
 cocode = g_company
 locode = g_loc
 gcompany = g_company
 gloc     = g_loc.

do transaction:
  {sys/inc/taxcode.i}
  {sys/inc/autopost.i}
  {sys/inc/bolwhse.i}
end.

DEF BUFFER bf-shipto FOR shipto.
DEF BUFFER bf-cust FOR cust.

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&SCOPED-DEFINE enable-shipto enable-shipto

&SCOPED-DEFINE where-jded-id WHERE reftable.reftable EQ "JDEDWARDCUST#" ~
                               AND reftable.company  EQ cocode          ~
                               AND reftable.loc      EQ ""              ~
                               AND reftable.code     EQ shipto.cust-no  ~
                               AND reftable.code2    EQ shipto.ship-id


/*DEF VAR char-hdl AS CHAR NO-UNDO.

DEF VAR lv-cust-no    LIKE shipto.cust-no    NO-UNDO.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES shipto

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH shipto SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH shipto SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame shipto
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame shipto


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-ship-id v-ship-name v-ship-addr-1 ~
v-ship-addr-2 v-ship-city v-ship-state v-ship-zip v-contact v-area-code ~
v-phone faxAreaCode faxNumber fi_jded-id v-tax-code tb_mandatory-tax ~
v-broker v-bill v-dock-loc v-dock-hour v-loc v-loc-bin v-carrier ~
v-dest-code v-pallet v-ship-meth v-del-chg v-del-time v-notes-1 v-notes-2 ~
v-notes-3 v-notes-4 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-ship-id v-ship-name v-ship-addr-1 ~
v-ship-addr-2 v-ship-city v-ship-state v-ship-zip v-contact v-area-code ~
v-phone faxAreaCode faxNumber fi_jded-id v-tax-code tb_mandatory-tax ~
v-broker v-bill v-dock-loc v-dock-hour v-loc v-loc-bin v-carrier ~
v-dest-code v-pallet v-ship-meth v-del-chg v-del-time v-notes-1 v-notes-2 ~
v-notes-3 v-notes-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-ship-id 
&Scoped-define List-2 fi_jded-id tb_mandatory-tax 
&Scoped-define List-4 v-ship-state v-tax-code v-loc v-carrier v-dest-code 
&Scoped-define List-5 faxAreaCode faxNumber 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE faxAreaCode AS CHARACTER FORMAT "(xxx)":U 
     LABEL "Fax #" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE faxNumber AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_jded-id AS CHARACTER FORMAT "X(256)":U 
     LABEL "JD Edw#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE v-area-code AS CHARACTER FORMAT "(xxx)" 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.

DEFINE VARIABLE v-carrier AS CHARACTER FORMAT "x(5)" 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     FONT 4.

DEFINE VARIABLE v-contact AS CHARACTER FORMAT "x(25)" 
     LABEL "Contact" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE v-del-chg AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Charge" 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1
     FONT 4.

DEFINE VARIABLE v-del-time AS DECIMAL FORMAT ">9.9" INITIAL 0 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     FONT 4.

DEFINE VARIABLE v-dest-code AS CHARACTER FORMAT "x(5)" 
     LABEL "Zone" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     FONT 4.

DEFINE VARIABLE v-dock-hour AS CHARACTER FORMAT "X(10)" 
     LABEL "Dock Hours" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE v-dock-loc AS CHARACTER FORMAT "x(10)" 
     LABEL "Dock#" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE v-loc AS CHARACTER FORMAT "x(5)" 
     LABEL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     FONT 4.

DEFINE VARIABLE v-loc-bin AS CHARACTER FORMAT "x(8)" 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE v-notes-1 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 102 BY 1.

DEFINE VARIABLE v-notes-2 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 102 BY 1.

DEFINE VARIABLE v-notes-3 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 102 BY 1.

DEFINE VARIABLE v-notes-4 AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 102 BY 1.

DEFINE VARIABLE v-pallet AS CHARACTER FORMAT "x(10)" 
     LABEL "Pallet" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE v-phone AS CHARACTER FORMAT "xxx-xxxx" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-ship-addr-1 AS CHARACTER FORMAT "x(30)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-addr-2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-city AS CHARACTER FORMAT "x(15)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-id LIKE shipto.ship-id
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE v-ship-name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FONT 4.

DEFINE VARIABLE v-tax-code AS CHARACTER FORMAT "x(3)" 
     LABEL "Tax Code" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 4.

DEFINE VARIABLE v-ship-meth AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Case", yes,
"Pallet", no
     SIZE 21 BY .95.

DEFINE VARIABLE tb_mandatory-tax AS LOGICAL INITIAL no 
     LABEL "Mandatory Tax?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE v-bill AS LOGICAL INITIAL no 
     LABEL "Billable" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81.

DEFINE VARIABLE v-broker AS LOGICAL INITIAL no 
     LABEL "Broker?" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      shipto SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-ship-id AT ROW 1.24 COL 14 COLON-ALIGNED HELP
          "Ship To Id, Maybe a Billable Customer Id."
          FONT 4
     v-ship-name AT ROW 2.19 COL 14 COLON-ALIGNED
     v-ship-addr-1 AT ROW 3.14 COL 14 COLON-ALIGNED
     v-ship-addr-2 AT ROW 4.1 COL 14 COLON-ALIGNED NO-LABEL
     v-ship-city AT ROW 5.05 COL 14 COLON-ALIGNED
     v-ship-state AT ROW 5.05 COL 34 COLON-ALIGNED NO-LABEL
     v-ship-zip AT ROW 5.05 COL 39 COLON-ALIGNED NO-LABEL
     v-contact AT ROW 1.24 COL 68 COLON-ALIGNED
     v-area-code AT ROW 2.43 COL 69 COLON-ALIGNED
     v-phone AT ROW 2.43 COL 79 COLON-ALIGNED NO-LABEL
     faxAreaCode AT ROW 3.38 COL 69 COLON-ALIGNED AUTO-RETURN 
     faxNumber AT ROW 3.38 COL 79 COLON-ALIGNED NO-LABEL
     fi_jded-id AT ROW 4.81 COL 68.4 COLON-ALIGNED
     v-tax-code AT ROW 6.57 COL 14 COLON-ALIGNED
     tb_mandatory-tax AT ROW 6.86 COL 36
     v-broker AT ROW 6.86 COL 64
     v-bill AT ROW 6.86 COL 96 RIGHT-ALIGNED
     v-dock-loc AT ROW 1.24 COL 118 COLON-ALIGNED
     v-dock-hour AT ROW 2.43 COL 118 COLON-ALIGNED
     v-loc AT ROW 3.38 COL 118 COLON-ALIGNED
     v-loc-bin AT ROW 4.33 COL 118 COLON-ALIGNED
     v-carrier AT ROW 5.29 COL 118 COLON-ALIGNED
     v-dest-code AT ROW 6.33 COL 118 COLON-ALIGNED
     v-pallet AT ROW 7.38 COL 118 COLON-ALIGNED
     v-ship-meth AT ROW 8.62 COL 122.6 NO-LABEL
     v-del-chg AT ROW 9.81 COL 125 COLON-ALIGNED
     v-del-time AT ROW 11 COL 125 COLON-ALIGNED
     v-notes-1 AT ROW 8 COL 4.4 COLON-ALIGNED NO-LABEL
     v-notes-2 AT ROW 8.95 COL 4.4 COLON-ALIGNED NO-LABEL
     v-notes-3 AT ROW 9.91 COL 4.4 COLON-ALIGNED NO-LABEL
     v-notes-4 AT ROW 10.86 COL 4.4 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 12.1 COL 54.6
     Btn_Cancel AT ROW 12.1 COL 70.6
     "S" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 11.05 COL 2.2
     "E" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 10.29 COL 2.4
     "O" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 8.86 COL 2.4
     "N" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 8.14 COL 2.4
     "T" VIEW-AS TEXT
          SIZE 4 BY .48 AT ROW 9.57 COL 2.4
     SPACE(138.59) SKIP(3.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "New ShipTo"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN faxAreaCode IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN faxNumber IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN fi_jded-id IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_mandatory-tax IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX v-bill IN FRAME Dialog-Frame
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN v-carrier IN FRAME Dialog-Frame
   4                                                                    */
/* SETTINGS FOR FILL-IN v-dest-code IN FRAME Dialog-Frame
   4                                                                    */
/* SETTINGS FOR FILL-IN v-loc IN FRAME Dialog-Frame
   4                                                                    */
/* SETTINGS FOR FILL-IN v-ship-id IN FRAME Dialog-Frame
   1 LIKE = asi.shipto.ship-id EXP-HELP EXP-SIZE                        */
/* SETTINGS FOR FILL-IN v-ship-state IN FRAME Dialog-Frame
   4                                                                    */
/* SETTINGS FOR FILL-IN v-tax-code IN FRAME Dialog-Frame
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.shipto"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* New ShipTo */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-handle AS HANDLE NO-UNDO.
   
   CASE FOCUS:NAME :
      when "v-ship-id" then do:
         run windows/l-cust.w  (gcompany,focus:screen-value, output char-val). 
         if char-val <> "" then DO:
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
            RUN display-new-shipto.
         END.
      end.
      when "v-tax-code" then do:
         run windows/l-stax.w  (gcompany,focus:screen-value, output char-val). 
         if char-val <> "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "v-loc" then do:
         run windows/l-loc.w  (gcompany,focus:screen-value, output char-val). 
         if char-val <> "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "v-loc-bin" then do:
         run windows/l-fgbin.w  (gcompany,v-loc:SCREEN-VALUE IN FRAME {&FRAME-NAME},focus:screen-value, output char-val). 
         if char-val <> "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "v-carrier" then do:
         run windows/l-carrie.w  (gcompany, v-loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
         if char-val <> "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "v-dest-code" then do:
         run windows/l-delzon.w  (gcompany, v-loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, v-carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
         if char-val <> "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "v-pallet" then do:
         run windows/l-itemp.w 
            (gcompany,"",focus:screen-value in frame {&frame-name}, output char-val).
         if char-val <> "" then 
            assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
         return no-apply.  
      end.
      otherwise do:
         lv-handle = focus:handle.
         run applhelp.p.
         
         if g_lookup-var <> "" THEN
            lv-handle:screen-value = g_lookup-var.        
         
         g_lookup-var = "".
         return no-apply.
      END.  /* otherwise */
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* New ShipTo */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   DEF BUFFER ycust FOR cust.
   DEF BUFFER yshipto FOR shipto.

   ASSIGN {&displayed-objects}.

   RUN ship-zip.

   RUN valid-ship-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-ship-state NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-tax-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-loc NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-loc-bin NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-carrier NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   RUN valid-dest-code NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   if v-pallet:screen-value IN FRAME {&FRAME-NAME} <> "" and
      not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                    item.i-no = v-pallet:screen-value) THEN
      do:
         message "Invalid Pallet Code. Try Help." view-as alert-box error.
         apply "entry" to v-pallet.
         RETURN NO-APPLY.     
      end.
  
    CREATE shipto.
  
    FIND LAST bf-shipto
        WHERE bf-shipto.company EQ cocode
          AND bf-shipto.cust-no EQ cust.cust-no
          AND ROWID(bf-shipto)  NE ROWID(shipto)
        USE-INDEX ship-no NO-LOCK NO-ERROR.

    ASSIGN
       shipto.company = cocode
       shipto.cust-no = cust.cust-no
       shipto.ship-no = (IF AVAIL bf-shipto THEN bf-shipto.ship-no ELSE 0) + 1
       shipto.ship-id = v-ship-id
       shipto.fax = faxAreaCode + faxNumber.
  
    RELEASE bf-shipto.
  
    IF cust.active EQ "X" THEN DO: 
       SESSION:SET-WAIT-STATE ("general").
  
       FOR EACH ycust NO-LOCK:
           FOR EACH yshipto OF ycust
              WHERE yshipto.loc-bin EQ "" 
                AND yshipto.loc     EQ shipto.loc
              EXCLUSIVE-LOCK:
              yshipto.loc-bin = shipto.loc-bin.
           END. /*inner for each*/
       END. /*outer for each*/
   END.

   ASSIGN
      shipto.ship-name = v-ship-name
      shipto.ship-addr[1] = v-ship-addr-1
      shipto.ship-addr[2] = v-ship-addr-2
      shipto.ship-city = v-ship-city
      shipto.ship-state = v-ship-state
      shipto.ship-zip = v-ship-zip
      shipto.contact = v-contact
      shipto.area-code = v-area-code
      shipto.phone = v-phone
      shipto.tax-code = v-tax-code
      shipto.broker = v-broker
      shipto.bill = v-bill
      shipto.dock-loc = v-dock-loc
      shipto.dock-hour = v-dock-hour
      shipto.loc = v-loc
      shipto.loc-bin = v-loc-bin
      shipto.carrier = v-carrier
      shipto.dest-code = v-dest-code
      shipto.pallet = v-pallet
      shipto.ship-meth = v-ship-meth
      shipto.notes[1] = v-notes-1
      shipto.notes[2] = v-notes-2
      shipto.notes[3] = v-notes-3
      shipto.notes[4] = v-notes-4
      shipto.del-chg = v-del-chg
      shipto.del-time = v-del-time
      shipto.tax-mandatory = tb_mandatory-tax.
  
   RUN reftable-values.

   RELEASE shipto.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxAreaCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxAreaCode Dialog-Frame
ON LEAVE OF faxAreaCode IN FRAME Dialog-Frame /* Fax # */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxNumber Dialog-Frame
ON LEAVE OF faxNumber IN FRAME Dialog-Frame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-broker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-broker Dialog-Frame
ON return OF v-broker IN FRAME Dialog-Frame /* Broker? */
DO:
    apply "tab" to self.
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-carrier Dialog-Frame
ON LEAVE OF v-carrier IN FRAME Dialog-Frame /* Carrier */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN valid-carrier NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-dest-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-dest-code Dialog-Frame
ON LEAVE OF v-dest-code IN FRAME Dialog-Frame /* Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dest-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-loc Dialog-Frame
ON LEAVE OF v-loc IN FRAME Dialog-Frame /* Warehouse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-loc-bin Dialog-Frame
ON LEAVE OF v-loc-bin IN FRAME Dialog-Frame /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-pallet Dialog-Frame
ON LEAVE OF v-pallet IN FRAME Dialog-Frame /* Pallet */
DO:
   if lastkey <> -1 and v-pallet:screen-value <> "" and
      not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                    item.i-no = v-pallet:screen-value) THEN
      do:
         message "Invalid Pallet Code. Try Help." view-as alert-box error.
         return no-apply.     
      end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ship-state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-state Dialog-Frame
ON LEAVE OF v-ship-state IN FRAME Dialog-Frame
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-state NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ship-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-zip Dialog-Frame
ON HELP OF v-ship-zip IN FRAME Dialog-Frame
DO:
  RUN applhelp.p.
  {&self-name}:SCREEN-VALUE = g_lookup-var.
  RUN ship-zip.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-zip Dialog-Frame
ON LEAVE OF v-ship-zip IN FRAME Dialog-Frame
DO:
  IF LASTKEY NE -1 THEN RUN ship-zip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-tax-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-tax-code Dialog-Frame
ON LEAVE OF v-tax-code IN FRAME Dialog-Frame /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
     RUN valid-tax-code NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  FIND FIRST cust WHERE
       ROWID(cust) = ip-rowid
       NO-LOCK NO-ERROR.

  IF NOT AVAIL cust THEN
     LEAVE.

  ASSIGN
     v-ship-id:SCREEN-VALUE = lv-ship-id
     v-ship-name:SCREEN-VALUE = lv-ship-name
     v-ship-state:SCREEN-VALUE = lv-ship-state
     v-ship-meth:SCREEN-VALUE = STRING(cust.ship-part)
     v-carrier:SCREEN-VALUE = cust.carrier
     v-loc:SCREEN-VALUE  = cust.loc
     v-dest-code:SCREEN-VALUE = cust.del-zone
     v-tax-code:SCREEN-VALUE = cust.tax-gr.

  FIND FIRST bf-cust NO-LOCK
       WHERE bf-cust.company EQ cocode
         AND bf-cust.active  EQ "X"
       NO-ERROR.
  
  IF AVAIL bf-cust THEN
     FOR EACH bf-shipto
        WHERE bf-shipto.company EQ bf-cust.company
          AND bf-shipto.cust-no EQ bf-cust.cust-no
        BREAK BY bf-shipto.ship-no DESC:
        IF bf-shipto.ship-id EQ bf-shipto.cust-no OR
           LAST(bf-shipto.ship-no) THEN DO:
           ASSIGN
              v-loc:SCREEN-VALUE = bf-shipto.loc
              v-loc-bin:SCREEN-VALUE = bf-shipto.loc-bin.
           LEAVE.
        END.
     END.

  IF cust.sort EQ "Y" THEN shipto.tax-code = cust.tax-gr.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY v-ship-id v-ship-name v-ship-addr-1 v-ship-addr-2 v-ship-city 
          v-ship-state v-ship-zip v-contact v-area-code v-phone faxAreaCode 
          faxNumber fi_jded-id v-tax-code tb_mandatory-tax v-broker v-bill 
          v-dock-loc v-dock-hour v-loc v-loc-bin v-carrier v-dest-code v-pallet 
          v-ship-meth v-del-chg v-del-time v-notes-1 v-notes-2 v-notes-3 
          v-notes-4 
      WITH FRAME Dialog-Frame.
  ENABLE v-ship-id v-ship-name v-ship-addr-1 v-ship-addr-2 v-ship-city 
         v-ship-state v-ship-zip v-contact v-area-code v-phone faxAreaCode 
         faxNumber fi_jded-id v-tax-code tb_mandatory-tax v-broker v-bill 
         v-dock-loc v-dock-hour v-loc v-loc-bin v-carrier v-dest-code v-pallet 
         v-ship-meth v-del-chg v-del-time v-notes-1 v-notes-2 v-notes-3 
         v-notes-4 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values Dialog-Frame 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL shipto THEN DO TRANSACTION:
      FIND FIRST reftable {&where-jded-id} NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
         CREATE reftable.
         ASSIGN
            reftable.reftable = "JDEDWARDCUST#"
            reftable.company  = cocode
            reftable.loc      = ""
            reftable.code     = shipto.cust-no
            reftable.code2    = shipto.ship-id.
      END.

      reftable.dscr = fi_jded-id.
     
      FIND CURRENT reftable NO-LOCK.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-zip Dialog-Frame 
PROCEDURE ship-zip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF v-ship-zip:SCREEN-VALUE NE "" THEN
         FIND FIRST nosweat.zipcode WHERE
              nosweat.zipcode.zipcode EQ v-ship-zip:SCREEN-VALUE
              NO-LOCK NO-ERROR.

      IF AVAIL nosweat.zipcode THEN do:
         v-ship-state:SCREEN-VALUE = nosweat.zipcode.state.
         IF v-ship-city:SCREEN-VALUE EQ "" THEN
            v-ship-city:SCREEN-VALUE = nosweat.zipcode.city.
      END.
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier Dialog-Frame 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
       v-carrier:SCREEN-VALUE = CAPS(v-carrier:SCREEN-VALUE).
      
       IF NOT CAN-FIND(FIRST carrier
                       WHERE carrier.company EQ cocode
                         AND carrier.loc     EQ v-loc:SCREEN-VALUE
                         AND carrier.carrier EQ v-carrier:SCREEN-VALUE) THEN DO:
          MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO v-carrier.
          RETURN ERROR.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dest-code Dialog-Frame 
PROCEDURE valid-dest-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      v-dest-code:SCREEN-VALUE = CAPS(v-dest-code:SCREEN-VALUE).
     
      IF NOT CAN-FIND(FIRST carr-mtx
                      WHERE carr-mtx.company  EQ cocode
                        AND carr-mtx.loc      EQ v-loc:SCREEN-VALUE
                        AND carr-mtx.carrier  EQ v-carrier:SCREEN-VALUE
                        AND carr-mtx.del-zone EQ v-dest-code:SCREEN-VALUE) THEN DO:
         MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-dest-code.
         RETURN ERROR.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc Dialog-Frame 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      v-loc:SCREEN-VALUE = CAPS(v-loc:SCREEN-VALUE).

      IF NOT CAN-FIND(FIRST loc WHERE
         loc.company EQ cocode AND
         loc.loc     EQ v-loc:SCREEN-VALUE) THEN DO:

         MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-loc.
         RETURN ERROR.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin Dialog-Frame 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      v-loc-bin:SCREEN-VALUE = CAPS(v-loc-bin:SCREEN-VALUE).

      IF NOT CAN-FIND(FIRST fg-bin
                      WHERE fg-bin.company EQ cocode
                        AND fg-bin.loc     EQ v-loc:SCREEN-VALUE
                        AND fg-bin.loc-bin EQ v-loc-bin:SCREEN-VALUE) THEN DO:
         MESSAGE "Bin does not exist in this warehouse..." VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id Dialog-Frame 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-shipto FOR shipto.
   DEF BUFFER b-cust   FOR cust.

   DO WITH FRAME {&FRAME-NAME}:
      v-ship-id:SCREEN-VALUE = CAPS(v-ship-id:SCREEN-VALUE).
     
      IF CAN-FIND(FIRST b-shipto
                  WHERE b-shipto.company       EQ cocode
                    AND b-shipto.cust-no       EQ cust.cust-no
                    AND TRIM(b-shipto.ship-id) EQ v-ship-id:SCREEN-VALUE
                    AND ROWID(b-shipto)        NE ROWID(shipto)) OR
         v-ship-id:SCREEN-VALUE EQ ""                       THEN DO:
        IF v-ship-id:SCREEN-VALUE EQ "" THEN
          MESSAGE "ShipTo ID may not be blank..." VIEW-AS ALERT-BOX ERROR.
        ELSE
          MESSAGE "ShipTo ID already exists for this customer..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO v-ship-id.
        RETURN ERROR.
      END.
     
      IF v-bill:SCREEN-VALUE EQ "Yes" AND
         NOT CAN-FIND(FIRST b-cust
                      WHERE b-cust.company EQ cocode
                        AND b-cust.cust-no EQ v-ship-id:SCREEN-VALUE) THEN DO:
        MESSAGE "Billable ShipTo must be a valid customer..." VIEW-AS ALERT-BOX ERROR.
        v-bill:SCREEN-VALUE = "No".
        APPLY "entry" TO v-ship-id.
        RETURN ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-state Dialog-Frame 
PROCEDURE valid-ship-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      v-ship-state:SCREEN-VALUE = CAPS(v-ship-state:SCREEN-VALUE).

      IF NOT CAN-FIND(FIRST state WHERE
         state.state EQ v-ship-state:SCREEN-VALUE) THEN DO:
         MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-ship-state.
         RETURN ERROR.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-code Dialog-Frame 
PROCEDURE valid-tax-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
       v-tax-code:SCREEN-VALUE = CAPS(v-tax-code:SCREEN-VALUE).
      
       IF NOT AVAIL cust THEN
          FIND FIRST cust
              WHERE cust.company EQ shipto.company
                AND cust.cust-no EQ shipto.cust-no
              NO-LOCK NO-ERROR.
      
       IF ((AVAIL cust AND cust.SORT EQ "Y") or v-tax-mand)                    AND
          (v-tax-code:SCREEN-VALUE EQ "" OR
           NOT CAN-FIND(FIRST stax
                        WHERE stax.company   EQ shipto.company
                          AND stax.tax-group EQ v-tax-code:SCREEN-VALUE)) THEN DO:
         MESSAGE "Must enter a valid tax code, try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-tax-code.
         RETURN ERROR.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

