&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: est\v-qthd.w

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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
&scoped-define proc-enable  ENABLE-detail

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/VAR.i NEW SHARED}

{est/printquo.i new}

def NEW SHARED buffer xquo for quotehd.

def var list-name as cha no-undo.
def var tmp-dir as cha no-undo.
def var init-dir as cha no-undo.
DEF VAR lv-ship-no LIKE shipto.ship-no NO-UNDO.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-part-no LIKE quoteitm.part-no NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

DEF TEMP-TABLE w-qqty NO-UNDO FIELD w-rowid AS ROWID.

{sa/sa-sls01.i}  /* report */

{fg/d-invprc.i NEW}
{sys/ref/CustList.i NEW}
ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES quotehd
&Scoped-define FIRST-EXTERNAL-TABLE quotehd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quotehd.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS quotehd.quo-date quotehd.est-no quotehd.expireDate ~
quotehd.del-date quotehd.cust-no quotehd.billto[1] quotehd.billto[2] ~
quotehd.billto[3] quotehd.billto[4] quotehd.contact quotehd.ship-id ~
quotehd.shipto[1] quotehd.shipto[2] quotehd.shipto[3] quotehd.shipto[4] ~
quotehd.sold-id quotehd.soldto[1] quotehd.soldto[2] quotehd.soldto[3] ~
quotehd.soldto[4] quotehd.sman quotehd.terms quotehd.carrier ~
quotehd.del-zone 
&Scoped-define ENABLED-TABLES quotehd
&Scoped-define FIRST-ENABLED-TABLE quotehd
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS quotehd.q-no quotehd.quo-date ~
quotehd.est-no quotehd.expireDate quotehd.del-date quotehd.cust-no ~
quotehd.billto[1] quotehd.billto[2] quotehd.billto[3] quotehd.billto[4] ~
quotehd.contact quotehd.ship-id quotehd.shipto[1] quotehd.shipto[2] ~
quotehd.shipto[3] quotehd.shipto[4] quotehd.sold-id quotehd.soldto[1] ~
quotehd.soldto[2] quotehd.soldto[3] quotehd.soldto[4] quotehd.sman ~
quotehd.terms quotehd.carrier quotehd.del-zone 
&Scoped-define DISPLAYED-TABLES quotehd
&Scoped-define FIRST-DISPLAYED-TABLE quotehd
&Scoped-Define DISPLAYED-OBJECTS ls-status sman_desc term_desc carrier_desc ~
zon_desc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define List-5 quotehd.cust-no quotehd.billto[1] quotehd.billto[2] ~
quotehd.billto[3] quotehd.billto[4] quotehd.shipto[1] quotehd.shipto[2] ~
quotehd.shipto[3] quotehd.shipto[4] quotehd.soldto[1] quotehd.soldto[2] ~
quotehd.soldto[3] quotehd.soldto[4] quotehd.carrier 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD are-items-for-cust V-table-Win
FUNCTION are-items-for-cust RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE carrier_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE ls-status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order Status" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sman_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE term_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE zon_desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 10.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     quotehd.q-no AT ROW 1.24 COL 11 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     quotehd.quo-date AT ROW 1.24 COL 41.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     quotehd.est-no AT ROW 1.24 COL 70.8 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     quotehd.expireDate AT ROW 1.24 COL 97.9 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     ls-status AT ROW 1.24 COL 128.8 COLON-ALIGNED
     quotehd.del-date AT ROW 2.19 COL 41.2 COLON-ALIGNED
          LABEL "Delivery Date"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     quotehd.cust-no AT ROW 4.33 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     quotehd.billto[1] AT ROW 5.29 COL 5 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.billto[2] AT ROW 6.24 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.billto[3] AT ROW 7.19 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.billto[4] AT ROW 8.14 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.contact AT ROW 2.19 COL 70.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 68 BY 1
     quotehd.ship-id AT ROW 4.33 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     quotehd.shipto[1] AT ROW 5.29 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.shipto[2] AT ROW 6.24 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.shipto[3] AT ROW 7.19 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.shipto[4] AT ROW 8.14 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.sold-id AT ROW 4.33 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     quotehd.soldto[1] AT ROW 5.29 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.soldto[2] AT ROW 6.24 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.soldto[3] AT ROW 7.19 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.soldto[4] AT ROW 8.14 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     quotehd.sman AT ROW 9.33 COL 16 COLON-ALIGNED
          LABEL "SalesGrp"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     sman_desc AT ROW 9.33 COL 25.4 COLON-ALIGNED NO-LABEL
     quotehd.terms AT ROW 10.29 COL 16 COLON-ALIGNED
          LABEL "Terms"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     term_desc AT ROW 10.29 COL 25.4 COLON-ALIGNED NO-LABEL
     quotehd.carrier AT ROW 9.33 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     carrier_desc AT ROW 9.33 COL 92 COLON-ALIGNED NO-LABEL
     quotehd.del-zone AT ROW 10.29 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     zon_desc AT ROW 10.29 COL 92 COLON-ALIGNED NO-LABEL
     "Bill To" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 3.38 COL 4
          FGCOLOR 9 
     "Ship To" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 3.38 COL 51
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Sold To" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 3.38 COL 98
          FGCOLOR 9 
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.quotehd
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.19
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN quotehd.billto[1] IN FRAME F-Main
   ALIGN-L 5                                                            */
/* SETTINGS FOR FILL-IN quotehd.billto[2] IN FRAME F-Main
   ALIGN-L 5                                                            */
/* SETTINGS FOR FILL-IN quotehd.billto[3] IN FRAME F-Main
   ALIGN-L 5                                                            */
/* SETTINGS FOR FILL-IN quotehd.billto[4] IN FRAME F-Main
   ALIGN-L 5                                                            */
/* SETTINGS FOR FILL-IN quotehd.carrier IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN carrier_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN quotehd.cust-no IN FRAME F-Main
   ALIGN-L 5 EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN quotehd.del-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN quotehd.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ls-status IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN quotehd.q-no IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN quotehd.shipto[1] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.shipto[2] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.shipto[3] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.shipto[4] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.sman IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN quotehd.soldto[1] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.soldto[2] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.soldto[3] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.soldto[4] IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN quotehd.terms IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN term_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN zon_desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var char-val as cha no-undo.
   def var lv-handle as handle no-undo.   
   DEF VAR rec-val AS RECID NO-UNDO.

   case focus:name :
        when "cust-no" then do:
              run windows/l-cust.w (gcompany, quotehd.cust-no:SCREEN-VALUE, output char-val).
              if char-val <> "" AND entry(1,char-val) NE quotehd.cust-no:SCREEN-VALUE then do:
                 quotehd.cust-no:SCREEN-VALUE = entry(1,char-val).
                 RUN new-cust-no.                                             
              end.
              return no-apply.
        end.
        when "ship-id" then do:           
              run windows/l-shipt2.w (gcompany,gloc, quotehd.cust-no:SCREEN-VALUE, quotehd.ship-id:SCREEN-VALUE, output char-val, output rec-val).
              find shipto where recid(shipto) = rec-val no-lock NO-ERROR.
              if AVAIL shipto AND shipto.ship-id NE quotehd.ship-id:SCREEN-VALUE then do:
                 quotehd.ship-id:SCREEN-VALUE = shipto.ship-id.
                 RUN new-ship-id.
              end.
              return no-apply.
        END.
        when "sold-id" then do:           
              run windows/l-soldto.w (gcompany, quotehd.cust-no:screen-value, quotehd.sold-id:SCREEN-VALUE, output char-val).
              if char-val <> "" AND entry(2,char-val) NE quotehd.sold-id:SCREEN-VALUE then do:
                 quotehd.sold-id:SCREEN-VALUE = entry(2,char-val).
                 RUN new-sold-id.
              end.
              return no-apply.
        END.
       WHEN "sman" THEN DO:
           RUN windows/l-sman.w (gcompany,OUTPUT char-val).
           IF char-val <> "" THEN ASSIGN quotehd.sman:SCREEN-VALUE = ENTRY(1,char-val)
                                         sman_desc:SCREEN-VALUE = ENTRY(2,char-val).
       END.
       when "del-zone" then do:
           run windows/l-delzon.w 
              (gcompany,gloc, quotehd.carrier:screen-value, quotehd.del-zone:SCREEN-VALUE, output char-val).
           if char-val <> "" then 
              assign quotehd.del-zone:SCREEN-VALUE = entry(1,char-val).
           return no-apply.  
       end.
       when "terms" then do:
           run windows/l-terms.w 
               (gcompany,quotehd.terms:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign quotehd.terms:screen-value in frame {&frame-name} = entry(1,char-val)
                     term_desc:SCREEN-VALUE = ENTRY(2,char-val).
           return no-apply.  
       end.
       WHEN "est-no" THEN DO:
              RUN windows/l-est.w (gcompany,gloc,quotehd.est-no:screen-value in frame {&frame-name}, OUTPUT char-val).
              IF char-val <> "" THEN DO:
                 FIND FIRST eb WHERE STRING(RECID(eb)) = char-val NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN DO:
                   quotehd.est-no:screen-value = eb.est-no.
                 END.
              END. 
              return no-apply.  
        END.
        when "carrier" then do:
           RUN windows/l-carrie.w (gcompany,gloc,quotehd.carrier:screen-value, OUTPUT char-val).
           if char-val <> "" then 
              assign quotehd.carrier:SCREEN-VALUE = entry(1,char-val).
           return no-apply.  
       end.

       otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.

           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.
       end.  /* otherwise */


   end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.carrier V-table-Win
ON LEAVE OF quotehd.carrier IN FRAME F-Main /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    carrier_desc:SCREEN-VALUE = ''.
    IF quotehd.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
    FIND FIRST carrier NO-LOCK WHERE carrier.company EQ gcompany
                                 AND carrier.loc EQ gloc
                                 AND carrier.carrier EQ quotehd.carrier:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE carrier THEN DO:
      MESSAGE 'Invalid Carrier Code. Try Help.' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    carrier_desc:SCREEN-VALUE = carrier.dscr.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.cust-no V-table-Win
ON LEAVE OF quotehd.cust-no IN FRAME F-Main /* Cust. # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-cust-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.cust-no V-table-Win
ON VALUE-CHANGED OF quotehd.cust-no IN FRAME F-Main /* Cust. # */
DO:
  RUN new-cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.del-zone V-table-Win
ON LEAVE OF quotehd.del-zone IN FRAME F-Main /* Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
   {&methods/lValidateError.i YES}
    zon_desc:SCREEN-VALUE = ''.
    IF quotehd.del-zone:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
    FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ gcompany
                                  AND carr-mtx.loc EQ gloc
                                  AND carr-mtx.carrier EQ quotehd.carrier:SCREEN-VALUE
                                  AND carr-mtx.del-zone EQ quotehd.del-zone:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE carr-mtx THEN DO:
      MESSAGE 'Invalid Delivey Zone. Try Help.' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    zon_desc:SCREEN-VALUE = carr-mtx.del-dscr.
   {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.ship-id V-table-Win
ON LEAVE OF quotehd.ship-id IN FRAME F-Main /* Ship To ID */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.ship-id V-table-Win
ON VALUE-CHANGED OF quotehd.ship-id IN FRAME F-Main /* Ship To ID */
DO:
  RUN new-ship-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.sman V-table-Win
ON LEAVE OF quotehd.sman IN FRAME F-Main /* Sales Rep */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
   sman_desc:SCREEN-VALUE = ''.
   IF quotehd.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
   FIND FIRST sman NO-LOCK WHERE sman.sman EQ quotehd.sman:SCREEN-VALUE NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
     MESSAGE 'Invalid SalesGrp. Try Help.' VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
   END.
   sman_desc:SCREEN-VALUE = sman.sname.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.sold-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.sold-id V-table-Win
ON LEAVE OF quotehd.sold-id IN FRAME F-Main /* Sold To # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sold-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.sold-id V-table-Win
ON VALUE-CHANGED OF quotehd.sold-id IN FRAME F-Main /* Sold To # */
DO:
  RUN new-sold-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotehd.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotehd.terms V-table-Win
ON LEAVE OF quotehd.terms IN FRAME F-Main /* Terms */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    term_desc:SCREEN-VALUE = ''.
    IF quotehd.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN RETURN.
    FIND FIRST terms NO-LOCK WHERE terms.t-code EQ quotehd.terms:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE terms THEN DO:
      MESSAGE 'Invalid Terms. Try Help.' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    term_desc:SCREEN-VALUE = terms.dscr.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
ASSIGN cocode = gcompany
       locode = gloc.
 {sys/inc/custlistform.i ""EQ"" }

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION are-items-for-cust V-table-Win
FUNCTION are-items-for-cust RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE VARIABLE result AS LOGICAL NO-UNDO.
 DEF BUFFER bfCust FOR cust.
     FOR FIRST ar-invl WHERE
         ar-invl.company EQ cocode AND
         ar-invl.posted EQ YES AND
         ar-invl.cust-no EQ cust.cust-no        
         NO-LOCK:
       RESULT = TRUE.
       LEAVE.
     END.

    IF RESULT = FALSE THEN DO:
    FOR EACH bfCust WHERE bfCust.ACTIVE = "X"
      NO-LOCK,
      FIRST ASI.itemfg WHERE itemfg.company = cocode 
          AND itemfg.cust-no EQ ASI.bfCust.cust-no 
          AND itemfg.stat = "A" NO-LOCK:
        RESULT = TRUE.
        LEAVE.
      END.
    END.

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlusButton V-table-Win 
PROCEDURE addPlusButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "quotehd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quotehd"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-line-items V-table-Win 
PROCEDURE create-line-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR cPriceType AS CHAR NO-UNDO.
  DEF VAR io-rowid-list AS CHAR NO-UNDO .
  DEF VAR lv-multi-select  AS LOG NO-UNDO.
  DEF VAR lv-qty AS DECIMAL NO-UNDO.
  DEF VAR lv-price AS DECIMAL NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR io-qty-list AS CHAR NO-UNDO.
  DEF VAR io-price-list AS CHAR NO-UNDO.
  DEF VAR io-uom-list AS CHAR NO-UNDO.
  DEF VAR hHand AS HANDLE NO-UNDO.
  DEFINE VARIABLE cNewRep AS CHARACTER   NO-UNDO.
  hHand = THIS-PROCEDURE.
  DEF BUFFER bfCust FOR cust.

  SESSION:SET-WAIT-STATE("general").

  /* Prompt for what items to list */
  FIND cust WHERE cust.company EQ quotehd.company
   AND cust.cust-no EQ quotehd.cust-no
   NO-LOCK NO-ERROR.

  IF are-items-for-cust() THEN 
      RUN est/d-qpriceType.w (OUTPUT cPriceType).
  ELSE
      cPriceType = "Manual".

  IF NOT cPriceType EQ "Manual" THEN DO:
      RUN est/addQuoteItems.p (INPUT ROWID(quotehd), INPUT cPriceType).

     /* {methods/run_link.i "bottom-TARGET" "add-items-by-selecting" "(cPriceType)"} */
      FIND FIRST quoteitm OF quotehd NO-LOCK NO-ERROR.
      IF AVAIL quoteitm THEN DO:
          RUN fg/fgSlsRep.p (INPUT quotehd.company,
                       INPUT quotehd.cust-no,
                       INPUT quoteitm.part-no,
                       INPUT quoteitm.i-no,
                       OUTPUT cNewRep).
          IF cNewRep GT "" THEN DO:
            RUN dispNewRep (INPUT cNewRep).
            FIND CURRENT quotehd EXCLUSIVE-LOCK.
            quotehd.sman = cNewRep.
            FIND CURRENT quotehd NO-LOCK.
          END.
      END.
  END. /* If not manual */
  /* If 'manual' was selected, no quoteitm records will be created and local-update */
  /* will then run local-add in the quoteitm browse                                 */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispNewRep V-table-Win 
PROCEDURE dispNewRep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcNewRep AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hWinHandle AS HANDLE      NO-UNDO.
  asi.quotehd.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipcNewRep.
  find first sman where sman.sman = quotehd.sman:screen-value in frame {&frame-name}
      no-lock no-error.
  if avail sman then sman_desc:screen-value = sman.sname.

  RUN get-link-handle in adm-broker-hdl
      (this-procedure,"container-source", output char-hdl).

  /* This is to prevent an error message about current buffer different */
  /* then result list */

  /*  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:   
    hWinHandle = WIDGET-HANDLE(char-hdl).
    IF INDEX(hWinHandle:NAME, "w-quote") GT 0 THEN
      RUN switchTabs IN WIDGET-HANDLE(char-hdl).
  END.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-detail V-table-Win 
PROCEDURE enable-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 IF AVAIL quotehd AND quotehd.est-no <> "" AND NOT adm-new-record THEN  /* update with est#*/
      DISABLE /*quotehd.cust-no quotehd.billto[1] quotehd.billto[2]
              quotehd.billto[3] quotehd.billto[4]
              quotehd.ship-id quotehd.shipto[1 FOR 4]*/
              {&list-5}
       WITH FRAME {&FRAME-NAME}.

 ELSE IF AVAIL quotehd AND quotehd.est-no <> "" AND adm-new-record AND NOT adm-adding-record THEN  /* copy with est#*/
      DISABLE /*quotehd.cust-no quotehd.billto[1] quotehd.billto[2]
              quotehd.billto[3] quotehd.billto[4]
              quotehd.ship-id quotehd.shipto[1 FOR 4]*/
              {&list-5}
              quotehd.est-no quotehd.expireDate quotehd.sman quotehd.terms
              quotehd.carrier quotehd.del-zone
       WITH FRAME {&FRAME-NAME}.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ship-id LIKE quotehd.ship-id NO-UNDO.
  DEF VAR lv-sman LIKE quotehd.sman NO-UNDO.
  DEF BUFFER bf-eb FOR eb.
  DEFINE BUFFER bQuoteItm FOR quoteitm.
  DEFINE BUFFER bQuoteQty FOR quoteqty.
  DEF BUFFER bf-quotehd FOR quotehd.
  DEF BUFFER bf-quoteitm FOR quoteitm.
  DEF BUFFER bf-quoteqty FOR quoteqty.
  DEF BUFFER bf-quotechg FOR quotechg.
  DEF VAR v-prev-q-no AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-ship-id = quotehd.ship-id
         lv-sman = quotehd.sman
         v-prev-q-no = quotehd.q-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(quotehd.est-no))) + TRIM(quotehd.est-no).

  IF quotehd.est-no <> "" THEN DO:
     IF /*lv-ship-no <> 0 AND*/ lv-ship-id <> quotehd.ship-id AND
        NOT CAN-FIND(FIRST bf-eb
                     WHERE bf-eb.company  EQ quotehd.company
                        AND bf-eb.est-no  EQ quotehd.est-no 
                        AND bf-eb.cust-no EQ quotehd.cust-no
                        AND bf-eb.ship-id NE lv-ship-id)
     THEN DO:
       FOR each bf-eb WHERE bf-eb.company = quotehd.company
                        AND bf-eb.est-no = quotehd.est-no 
                        AND bf-eb.cust-no = quotehd.cust-no:
         IF bf-eb.ship-id = lv-ship-id THEN DO:
            FIND FIRST shipto  WHERE shipto.company = quotehd.company
                                 AND shipto.cust-no = quotehd.cust-no
                                 AND shipto.ship-id = quotehd.ship-id 
                      NO-LOCK NO-ERROR.
            IF AVAIL shipto THEN ASSIGN bf-eb.ship-no = shipto.ship-no
                                        bf-eb.ship-id = quotehd.ship-id
                                        bf-eb.ship-name = quotehd.shipto[1]
                                        bf-eb.ship-addr[1] = quotehd.shipto[2]
                                        bf-eb.ship-addr[2] = quotehd.shipto[3]
                                        bf-eb.ship-city = shipto.ship-city
                                        bf-eb.ship-state = shipto.ship-state
                                        bf-eb.ship-zip = shipto.ship-zip
                                        .
         END.
       END.
     END.
     IF lv-sman <> quotehd.sman
     THEN FOR each bf-eb WHERE bf-eb.company = quotehd.company
                           AND bf-eb.est-no = quotehd.est-no :
          bf-eb.sman = quotehd.sman.        
     END.       
  END.  /* est-no <> "" */

  /* update rfqitem qty - start */

  IF quotehd.rfq NE '' THEN DO:
    FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
    IF AVAILABLE module AND module.is-used THEN DO:
      IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
        IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
        CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
        IF CONNECTED('rfq') THEN DO:
          FOR EACH bQuoteItm OF quotehd NO-LOCK,
              EACH bQuoteQty NO-LOCK WHERE bQuoteQty.company EQ bQuoteItm.company
                                       AND bQuoteQty.loc EQ bQuoteItm.loc
                                       AND bQuoteQty.q-no EQ bQuoteItm.q-no
                                       AND bQuoteQty.line EQ bQuoteItm.line:
            RUN custom/rfq-qty.p (quotehd.company,quotehd.loc,quotehd.est-no,
                                  quotehd.rfq,bQuoteItm.part-no,bQuoteQty.qty,
                                  bQuoteQty.price,bQuoteQty.uom,TODAY,bQuoteQty.rels).
          END. /* each quoteitm */
          /* DISCONNECT rfq. */
        END. /* if connected */
      END. /* expire-date */
    END. /* avail module */
  END. /* if rfq */
  /* update rfqitem qty - end */

  /* copy */
  IF adm-new-record AND NOT adm-adding-record THEN DO:
     find first bf-quotehd use-index q-no where bf-quotehd.company = gcompany 
                             AND bf-quotehd.loc = gloc
                             AND bf-quotehd.q-no = v-prev-q-no no-lock no-error.
     ASSIGN quotehd.comment[1] = bf-quotehd.comment[1]
            quotehd.comment[2] = bf-quotehd.comment[2]
            quotehd.comment[3] = bf-quotehd.comment[3]
            quotehd.comment[4] = bf-quotehd.comment[4]
            quotehd.comment[5] = bf-quotehd.comment[5].

     FOR EACH bf-quoteitm OF bf-quotehd NO-LOCK :
         CREATE quoteitm.
         BUFFER-COPY bf-quoteitm EXCEPT bf-quoteitm.q-no TO quoteitm.
         ASSIGN quoteitm.q-no = quotehd.q-no.

         FOR EACH bf-quoteqty WHERE bf-quoteqty.company = bf-quoteitm.company
                                AND bf-quoteqty.loc = bf-quoteitm.loc
                                AND bf-quoteqty.q-no = bf-quoteitm.q-no
                                AND bf-quoteqty.line = bf-quoteitm.line NO-LOCK:
             CREATE quoteqty.
             BUFFER-COPY bf-quoteqty EXCEPT bf-quoteqty.q-no TO quoteqty.
             ASSIGN quoteqty.q-no = quotehd.q-no.

             FOR EACH bf-quotechg WHERE bf-quotechg.company eq bf-quoteqty.company
                                    AND bf-quotechg.loc eq bf-quoteqty.loc
                                    AND bf-quotechg.q-no eq bf-quoteqty.q-no
                                    AND ((bf-quotechg.line eq bf-quoteqty.line AND bf-quotechg.qty eq bf-quoteqty.qty) OR
                                        (bf-quotechg.LINE eq 0                 AND bf-quotechg.qty eq 0               )) NO-LOCK:
                 CREATE quotechg.
                 BUFFER-COPY bf-quotechg EXCEPT bf-quotechg.q-no TO quotechg.
                 ASSIGN quotechg.q-no = quotehd.q-no.
             END.   
         END.
     END.
  END. /* copy*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-qno as int no-undo.
  def buffer bf-hd for quotehd.
    DEFINE VARIABLE cNotes LIKE quotehd.comment   NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /*find last quotehd use-index q-no where quotehd.company = gcompany and
                                   quotehd.loc = gloc no-lock no-error.
  li-next-qno = if avail quotehd then quotehd.q-no + 1 else 1.  */          

/*   find first bf-hd use-index q-no where bf-hd.company = gcompany and       */
/*                                         bf-hd.loc = gloc no-lock no-error. */
/*                                                                            */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign /*quotehd.company = gcompany
         quotehd.loc = gloc
         quotehd.q-no = li-next-qno*/
         quotehd.quo-date = today.
    RUN est/GetQuoteDefNotes.p (INPUT quotehd.company,
                                OUTPUT cNotes).
    ASSIGN  
        quotehd.comment[1] = cNotes[1]
        quotehd.comment[2] = cNotes[2]
        quotehd.comment[3] = cNotes[3]
        quotehd.comment[4] = cNotes[4]
        quotehd.comment[5] = cNotes[5]
        .
/*   if avail bf-hd then assign quotehd.comment[1] = bf-hd.comment[1] */
/*                              quotehd.comment[2] = bf-hd.comment[2] */
/*                              quotehd.comment[3] = bf-hd.comment[3] */
/*                              quotehd.comment[4] = bf-hd.comment[4] */
/*                              quotehd.comment[5] = bf-hd.comment[5] */
                             .          

  IF adm-new-record AND NOT adm-adding-record THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&ENABLED-FIELDS}.
  END.
  display quotehd.q-no with frame {&frame-name}.
  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ship-id LIKE quotehd.ship-id NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail quotehd then return.
  if quotehd.sman <> "" then do:
     find first sman where sman.sman = quotehd.sman:screen-value in frame {&frame-name}
          no-lock no-error.
     if avail sman then sman_desc:screen-value = sman.sname.

  end.
  if quotehd.terms <> "" then do:
     find first terms where terms.t-code = quotehd.terms:screen-value no-lock no-error.
     if avail terms then term_desc:screen-value = terms.dscr.
  end.
  if quotehd.carrier <> "" then do:
     find first carrier where carrier.company = gcompany and 
                                     carrier.loc = gloc and
                                     carrier.carrier = quotehd.carrier:screen-value
                                     no-lock no-error.
     if avail carrier then carrier_desc:screen-value = carrier.dscr.
  end.
  if quotehd.del-zone <> "" then do:
     find first carr-mtx where carr-mtx.company = gcompany and 
                                     carr-mtx.loc = gloc and
                                     carr-mtx.carrier = quotehd.carrier:screen-value and
                                     carr-mtx.del-zone = quotehd.del-zone:screen-value
                                     no-lock no-error.
     if avail carr-mtx then zon_desc:screen-value = carr-mtx.del-dscr.
  end.
  if quotehd.sts = "O" then ls-status = "Ordered".
  else ls-status = "Quote".

  lv-ship-id = quotehd.ship-id.
  IF lv-ship-id = "" THEN DO:  /* due to eariler bug */
     FIND FIRST shipto WHERE shipto.company = quotehd.company
                         AND shipto.cust-no = quotehd.cust-no
                         AND shipto.ship-id = quotehd.ship-id NO-LOCK NO-ERROR.
     IF AVAIL shipto THEN lv-ship-id = shipto.ship-id.
  END.
  display ls-status lv-ship-id @ quotehd.ship-id with frame {&frame-name}.

  IF quotehd.sold-id:SCREEN-VALUE EQ "" THEN
    ASSIGN
     quotehd.sold-id:SCREEN-VALUE   = quotehd.cust-no
     quotehd.soldto[1]:SCREEN-VALUE = quotehd.billto[1]:SCREEN-VALUE
     quotehd.soldto[2]:SCREEN-VALUE = quotehd.billto[2]:SCREEN-VALUE
     quotehd.soldto[3]:SCREEN-VALUE = quotehd.billto[3]:SCREEN-VALUE
     quotehd.soldto[4]:SCREEN-VALUE = quotehd.billto[4]:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE newRecord AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
   newRecord = adm-new-record.

  /* validation ===*/
   RUN valid-cust-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-cust-user NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-ship-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-sold-id NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   {&methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
     sman_desc:SCREEN-VALUE = ''.
     IF quotehd.sman:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE '' THEN DO:
       FIND FIRST sman NO-LOCK WHERE sman.sman EQ quotehd.sman:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE sman THEN DO:
         MESSAGE 'Invalid SalesGrp. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quotehd.sman.
         RETURN NO-APPLY.
       END.
       sman_desc:SCREEN-VALUE = sman.sname.
     END.

     term_desc:SCREEN-VALUE = ''.
     IF quotehd.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE '' THEN DO:
       FIND FIRST terms NO-LOCK WHERE terms.t-code EQ quotehd.terms:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
         MESSAGE 'Invalid Terms. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quotehd.terms.
         RETURN NO-APPLY.
       END.
       term_desc:SCREEN-VALUE = terms.dscr.
     END.

     carrier_desc:SCREEN-VALUE = ''.
     IF quotehd.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN DO:
       FIND FIRST carrier NO-LOCK WHERE carrier.company EQ gcompany
                                    AND carrier.loc EQ gloc
                                    AND carrier.carrier EQ quotehd.carrier:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
         MESSAGE 'Invalid Carrier Code. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quotehd.carrier.
         RETURN NO-APPLY.
       END.
       carrier_desc:SCREEN-VALUE = carrier.dscr.
     END.

     zon_desc:SCREEN-VALUE = ''.
     IF quotehd.del-zone:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ '' THEN DO:
       FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ gcompany
                                     AND carr-mtx.loc EQ gloc
                                     AND carr-mtx.carrier EQ quotehd.carrier:SCREEN-VALUE
                                     AND carr-mtx.del-zone EQ quotehd.del-zone:SCREEN-VALUE NO-ERROR.
       IF NOT AVAILABLE carr-mtx THEN DO:
         MESSAGE 'Invalid Delivey Zone. Try Help.' VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY' TO quotehd.del-zone.
         RETURN NO-APPLY.
       END.
       zon_desc:SCREEN-VALUE = carr-mtx.del-dscr.
     END.
   END.
  {&methods/lValidateError.i NO}
  /* end of validation ==== */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT CAN-FIND(FIRST quoteitm OF quotehd) THEN RUN create-line-items.

  IF newRecord THEN DO:
    IF CAN-FIND(FIRST quoteitm OF quotehd) THEN DO:
        {methods/run_link.i "RECORD-SOURCE" "resetQuery" (quotehd.q-no)} 
        {methods/run_link.i "bottom-TARGET" "local-open-query"}
        newRecord = NO.
    END.
    ELSE DO:

        {methods/run_link.i "bottom-TARGET" "local-add-record"}
    END.
  END.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no V-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    IF quotehd.cust-no:SCREEN-VALUE NE "TEMP" THEN
    DO:
       FIND FIRST cust
           WHERE cust.company EQ gcompany
             AND cust.cust-no EQ quotehd.cust-no:SCREEN-VALUE
           NO-LOCK NO-ERROR.
       IF AVAIL cust THEN DO:
         ASSIGN
          quotehd.billto[1]:SCREEN-VALUE = cust.name
          quotehd.billto[2]:SCREEN-VALUE = cust.addr[1]
          quotehd.billto[3]:SCREEN-VALUE = cust.addr[2]
          quotehd.billto[4]:SCREEN-VALUE = cust.city + ", " +
                                           cust.state + "  " +
                                           cust.zip
          quotehd.sman:SCREEN-VALUE      = cust.sman
          quotehd.terms:SCREEN-VALUE     = cust.terms
          quotehd.contact:SCREEN-VALUE   = cust.contact
          quotehd.del-zone:SCREEN-VALUE  = cust.del-zone
          quotehd.carrier:SCREEN-VALUE   = cust.carrier.

         FIND FIRST shipto
             WHERE shipto.company EQ gcompany
               AND shipto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
               AND shipto.ship-id EQ quotehd.ship-id:SCREEN-VALUE
             NO-LOCK NO-ERROR.
         IF NOT AVAIL shipto THEN
         FOR EACH shipto
             WHERE shipto.company EQ gcompany
               AND shipto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
               AND shipto.ship-id NE ""
             NO-LOCK:
           quotehd.ship-id:SCREEN-VALUE = shipto.ship-id.
           RUN new-ship-id.
           LEAVE.
         END.

         FIND FIRST soldto
             WHERE soldto.company EQ gcompany
               AND soldto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
               AND soldto.sold-id EQ quotehd.sold-id:SCREEN-VALUE
             NO-LOCK NO-ERROR.
         IF NOT AVAIL soldto THEN
         FOR EACH soldto
             WHERE soldto.company EQ gcompany
               AND soldto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
               AND soldto.sold-id NE ""
             NO-LOCK:
           quotehd.sold-id:SCREEN-VALUE = soldto.sold-id.
           RUN new-sold-id.
           LEAVE.
         END.
       END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-ship-id V-table-Win 
PROCEDURE new-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    IF quotehd.ship-id:SCREEN-VALUE NE "TEMP" THEN
    DO:
       FIND FIRST shipto WHERE
            shipto.company EQ gcompany AND
            shipto.cust-no EQ quotehd.cust-no:SCREEN-VALUE AND
            shipto.ship-id EQ quotehd.ship-id:SCREEN-VALUE
            NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN
          ASSIGN
           lv-ship-no                     = shipto.ship-no
           quotehd.shipto[1]:SCREEN-VALUE = shipto.ship-name
           quotehd.shipto[2]:SCREEN-VALUE = shipto.ship-addr[1]
           quotehd.shipto[3]:SCREEN-VALUE = shipto.ship-addr[2]
           quotehd.shipto[4]:SCREEN-VALUE = TRIM(shipto.ship-city) + ", " +
                                            TRIM(shipto.ship-state) + "  " +
                                            TRIM(shipto.ship-zip).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sold-id V-table-Win 
PROCEDURE new-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    IF quotehd.sold-id:SCREEN-VALUE NE "TEMP" AND
       quotehd.sold-id:SCREEN-VALUE NE "" THEN
    DO:
       FIND FIRST soldto WHERE
            soldto.company EQ gcompany AND
            soldto.cust-no EQ quotehd.cust-no:SCREEN-VALUE AND
            soldto.sold-id EQ quotehd.sold-id:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF AVAIL soldto THEN
           ASSIGN
           quotehd.soldto[1]:SCREEN-VALUE = soldto.sold-name
           quotehd.soldto[2]:SCREEN-VALUE = soldto.sold-addr[1]
           quotehd.soldto[3]:SCREEN-VALUE = soldto.sold-addr[2]
           quotehd.soldto[4]:SCREEN-VALUE = TRIM(soldto.sold-city) + ", " +
                                            TRIM(soldto.sold-state) + "  " +
                                            TRIM(soldto.sold-zip).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file V-table-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-quote V-table-Win 
PROCEDURE print-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&methods/lValidateError.i YES}
  IF quotehd.sman = "" THEN DO:
     MESSAGE "Invalid SalesGrp!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF quotehd.carrier = ""  THEN DO:
     MESSAGE "Invalid Carrier!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF quotehd.terms = "" THEN DO:
     MESSAGE "Invalid Terms!" VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  RUN est/r-quoprt.w (ROWID(quotehd)) .
  {methods/run_link.i "CONTAINER-SOURCE" "moveToTop"}
  {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE quoteitm-exists V-table-Win 
PROCEDURE quoteitm-exists :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oplExists AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oprCurrentQuote AS ROWID NO-UNDO.
DEFINE VARIABLE lCheckNew AS LOGICAL NO-UNDO.
FIND FIRST quoteitm WHERE quoteitm.company EQ quotehd.company
                      AND quoteitm.loc EQ quotehd.loc
                      AND quoteitm.q-no EQ quotehd.q-no
                      AND quoteitm.loc EQ quotehd.loc                      
                      AND (quoteitm.i-no GT ""
                      OR quoteitm.part-no GT ""
                      OR quoteitm.i-dscr GT ""
                      OR quoteitm.style GT ""
                      OR quoteitm.price GT 0)
                    NO-LOCK NO-ERROR.
ASSIGN oplExists = AVAIL(quoteitm)
       oprCurrentQuote = ROWID(quotehd).

run get-link-handle in adm-broker-hdl
         (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN quoteitm-check-new IN WIDGET-HANDLE(char-hdl) (OUTPUT lCheckNew).
     IF lCheckNew THEN
         oplExists = YES . 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetQuery V-table-Win 
PROCEDURE resetQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "RECORD-SOURCE" "resetQuery" (quotehd.q-no)}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report V-table-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  output to 
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "quotehd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    quotehd.cust-no:SCREEN-VALUE = CAPS(quotehd.cust-no:SCREEN-VALUE).

    IF quotehd.cust-no:SCREEN-VALUE NE "TEMP" THEN DO:
      FIND FIRST cust
          WHERE cust.company EQ gcompany
            AND cust.cust-no EQ quotehd.cust-no:SCREEN-VALUE 
          NO-LOCK NO-ERROR.
      IF NOT AVAIL cust THEN DO:
        MESSAGE "Invalid Bill To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quotehd.cust-no.
        RETURN ERROR.
      END.

      ASSIGN
       quotehd.billto[1]:screen-value = cust.name
       quotehd.billto[2]:screen-value = cust.addr[1]
       quotehd.billto[3]:screen-value = cust.addr[2]
       quotehd.billto[4]:screen-value = cust.city + ", " +
                                        cust.state + "  " +
                                        cust.zip.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user V-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
custcount = "".
DEF VAR lActive AS LOG NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EQ',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""EQ""}
  IF ou-log THEN do:
    DO WITH FRAME {&FRAME-NAME}:
     IF LOOKUP(quotehd.cust-no:SCREEN-VALUE,custcount) = 0 THEN do:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO quotehd.cust-no .
          RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id V-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    quotehd.ship-id:SCREEN-VALUE = CAPS(quotehd.ship-id:SCREEN-VALUE).

    IF quotehd.ship-id:SCREEN-VALUE NE "TEMP" THEN DO:
      IF NOT CAN-FIND(FIRST shipto
                      WHERE shipto.company EQ gcompany
                        AND shipto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
                        AND shipto.ship-id EQ quotehd.ship-id:SCREEN-VALUE)
      THEN DO:
        MESSAGE "Invalid Ship To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quotehd.ship-id.
        RETURN ERROR.
      END.

      RUN new-ship-id.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sold-id V-table-Win 
PROCEDURE valid-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    quotehd.sold-id:SCREEN-VALUE = CAPS(quotehd.sold-id:SCREEN-VALUE).

    IF quotehd.sold-id:SCREEN-VALUE NE "TEMP" AND
       quotehd.sold-id:SCREEN-VALUE NE ""     THEN DO:
      IF NOT CAN-FIND(FIRST soldto
                      WHERE soldto.company EQ gcompany
                        AND soldto.cust-no EQ quotehd.cust-no:SCREEN-VALUE
                        AND soldto.sold-id EQ quotehd.sold-id:SCREEN-VALUE)
      THEN DO:
        MESSAGE "Invalid Sold To, try help..."  VIEW-AS ALERT-BOX.
        APPLY "entry" TO quotehd.sold-id.
        RETURN ERROR.
      END.

      RUN new-sold-id.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

