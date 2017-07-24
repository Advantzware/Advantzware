&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
def var char-val as cha no-undo.
def var ls-prev-cust as cha no-undo.
def var ll-is-copy as logic no-undo.

{custom/gcompany.i}
{custom/gloc.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rfq
&Scoped-define FIRST-EXTERNAL-TABLE rfq


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfq.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfq.req-date rfq.due-date rfq.cust-no ~
rfq.sman rfq.comm rfq.fob-code rfq.chg-method rfq.wh-month rfq.inst 
&Scoped-define ENABLED-TABLES rfq
&Scoped-define FIRST-ENABLED-TABLE rfq
&Scoped-Define ENABLED-OBJECTS RECT-20 
&Scoped-Define DISPLAYED-FIELDS rfq.rfq-no rfq.req-date rfq.due-date ~
rfq.cust-no rfq.ship-name rfq.ship-addr[1] rfq.ship-addr[2] rfq.ship-city ~
rfq.ship-state rfq.ship-zip rfq.sman rfq.comm rfq.fob-code rfq.chg-method ~
rfq.wh-month rfq.inst 
&Scoped-define DISPLAYED-TABLES rfq
&Scoped-define FIRST-DISPLAYED-TABLE rfq
&Scoped-Define DISPLAYED-OBJECTS sman_sname 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define DISPLAY-FIELD rfq.sman 

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE VARIABLE sman_sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 139 BY 14.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfq.rfq-no AT ROW 1.95 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.req-date AT ROW 1.95 COL 59 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfq.due-date AT ROW 1.95 COL 104 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     rfq.cust-no AT ROW 3.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     rfq.ship-name AT ROW 4.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.ship-addr[1] AT ROW 5.05 COL 17 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.ship-addr[2] AT ROW 6 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.ship-city AT ROW 6.95 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.ship-state AT ROW 6.95 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.ship-zip AT ROW 6.95 COL 45 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfq.sman AT ROW 8.38 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     sman_sname AT ROW 8.38 COL 26 COLON-ALIGNED NO-LABEL
     rfq.comm AT ROW 8.38 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     rfq.fob-code AT ROW 3.38 COL 99 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Destination", "D":U,
"Origin", "O":U
          SIZE 34 BY 1.43
     rfq.chg-method AT ROW 4.81 COL 97 COLON-ALIGNED
          LABEL "Freight Charge?" FORMAT "x(13)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Bill","Prepaid","Collect","Third Party" 
          DROP-DOWN-LIST
          SIZE 23 BY 1
     rfq.wh-month AT ROW 5.76 COL 97 COLON-ALIGNED
          LABEL "Warehouse Month"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS " 0"," 1"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9" 
          DROP-DOWN-LIST
          SIZE 23 BY 1
     rfq.inst AT ROW 11.24 COL 7 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 500 SCROLLBAR-VERTICAL
          SIZE 117 BY 4
     "FOB:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.86 COL 91
     "Special Instruction" VIEW-AS TEXT
          SIZE 23 BY .95 AT ROW 10.05 COL 7
     RECT-20 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfq
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
         HEIGHT             = 15.76
         WIDTH              = 139.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX rfq.chg-method IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rfq.due-date IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       rfq.inst:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN rfq.req-date IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfq.rfq-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.ship-addr[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN rfq.ship-addr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.ship-city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.ship-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.ship-state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.ship-zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfq.sman IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN sman_sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX rfq.wh-month IN FRAME F-Main
   EXP-LABEL                                                            */
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
   def var lv-handle as widget-handle no-undo.
   case focus:name :
        when "sman" then do:
             run windows/l-sman.w (gcompany, output char-val).
             if char-val <> "" then 
                assign rfq.sman:screen-value = entry(1,char-val)
                       sman_sname:screen-value = entry(2,char-val)
                       rfq.comm:screen-value = entry(3,char-val).
                find FIRST sman where sman.company = gcompany AND                                      
                                sman.sman = FOCUS:SCREEN-VALUE
                        no-lock no-error.
                FIND FIRST cust WHERE cust.company = gcompany
                            AND cust.cust-no = rfq.cust-no:SCREEN-VALUE NO-LOCK NO-ERROR.
                IF AVAIL sman THEN 
                   FIND FIRST sman-mtx OF sman WHERE sman-mtx.custype = cust.type NO-LOCK NO-ERROR.
                IF AVAIL sman-mtx THEN ASSIGN rfq.comm:screen-value in frame {&frame-name} = string(sman-mtx.type-comm).
           .

             return no-apply.          
        end.
        when "req-date" or when "due-date" then do:
             /*{methods/calendar.i}  run on self's help trigger*/
        end.
        when "cust-no" then do:
             run windows/l-cust.w(gcompany, rfq.cust-no:screen-value in frame {&frame-name}, output char-val).
             if char-val <> "" THEN DO:            
                assign rfq.cust-no:screen-value = entry(1,char-val).
                FIND FIRST cust WHERE cust.company = gcompany
                            AND cust.cust-no = rfq.cust-no:SCREEN-VALUE NO-LOCK NO-ERROR.
                find FIRST sman where sman.company = gcompany AND                                     
                                sman.sman = cust.sman
                        no-lock no-error.            
                ASSIGN rfq.sman:SCREEN-VALUE = cust.sman 
                       rfq.comm:SCREEN-VALUE = IF AVAIL sman THEN STRING(sman.scomm,">>9.99") ELSE "" .
                if avail cust then 
                   assign rfq.ship-name:screen-value in frame {&frame-name} = cust.name
                            rfq.ship-addr[1]:screen-value in frame {&frame-name}  = cust.addr[1]
                            rfq.ship-addr[2]:screen-value in frame {&frame-name}  = cust.addr[2]
                            rfq.ship-city:screen-value in frame {&frame-name}     = cust.city
                            rfq.ship-state:screen-value in frame {&frame-name}    = cust.state
                            rfq.ship-zip:screen-value in frame {&frame-name}      = cust.zip
                            rfq.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "B" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else "".

                IF AVAIL sman THEN 
                   FIND FIRST sman-mtx OF sman WHERE sman-mtx.custype = cust.type NO-LOCK NO-ERROR.
                IF AVAIL sman-mtx THEN ASSIGN rfq.comm:screen-value in frame {&frame-name} = string(sman-mtx.type-comm).
             END.
             return no-apply.   
        end.
        otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
                 assign rfq.ship-name:screen-value = cust.name
                     rfq.ship-addr[1]:screen-value = cust.addr[1]
                     rfq.ship-addr[2]:screen-value = cust.addr[2]
                     rfq.ship-city:screen-value =    cust.city
                     rfq.ship-state:screen-value =   cust.state
                     rfq.ship-zip:screen-value =     cust.zip
                     rfq.sman:screen-value in frame {&frame-name} = if avail cust then cust.sman else ""
                     rfq.fob-code:screen-value in frame {&frame-name} = if cust.fob-code = "Dest" then "D"
                                                                        else if cust.fob-code = "orig" then "O"
                                                                        else ""
                     rfq.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "B" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else ""
                     .                

                 find sman where sman.company = gcompany and
                              sman.sman = rfq.sman:screen-value
                              no-lock no-error.
                 assign sman_sname:screen-value = if avail sman then sman.sname else ""
                     rfq.comm:screen-value = if avail sman then string(sman.scomm) else "0"
                     .
              end.  /* cust-no */
           end.   /* g_lookup-var <> "" */

        end.   
   end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON return OF FRAME F-Main
/*anywhere*/
DO:
  /*  if focus:type <> "editor" then do:
       apply "tab" to self.
       return no-apply.
    end.
    else do:
        apply lastkey to self.
        return no-apply.

    end.
   */
   return. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.chg-method
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.chg-method V-table-Win
ON return OF rfq.chg-method IN FRAME F-Main /* Freight Charge? */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.cust-no V-table-Win
ON ENTRY OF rfq.cust-no IN FRAME F-Main /* Cust.# */
DO:
    ls-prev-cust = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.cust-no V-table-Win
ON LEAVE OF rfq.cust-no IN FRAME F-Main /* Cust.# */
DO:
   def buffer b-rfq for rfq.
   if ls-prev-cust = self:screen-value and
      self:screen-value <> "Temp"   
   then return.
   {&methods/lValidateError.i YES}

   find cust where cust.company = gcompany     and
                   cust.cust-no = rfq.cust-no:screen-value in frame {&frame-name}
                      no-lock no-error.
   if not avail cust  then find first cust where cust.cust-no = rfq.cust-no:screen-value in frame {&frame-name}
        no-lock no-error.

   if avail cust and self:screen-value <> "TEMP" then do:
      assign rfq.ship-name:screen-value in frame {&frame-name} = cust.name
                             rfq.ship-addr[1]:screen-value in frame {&frame-name}  = cust.addr[1]
                             rfq.ship-addr[2]:screen-value in frame {&frame-name}  = cust.addr[2]
                             rfq.ship-city:screen-value in frame {&frame-name}     = cust.city
                             rfq.ship-state:screen-value in frame {&frame-name}    = cust.state
                             rfq.ship-zip:screen-value in frame {&frame-name}      = cust.zip
                             rfq.sman:screen-value in frame {&frame-name} = cust.sman
                             rfq.fob-code:screen-value in frame {&frame-name} = if cust.fob-code = "Dest" then "D"
                                                                                else if cust.fob-code = "orig" then "O"
                                                                                else ""
                             rfq.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "B" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else ""

                             .
      find b-rfq where recid(b-rfq) = recid(rfq).
      assign b-rfq.ship-name = cust.name
            b-rfq.ship-addr[1] = cust.addr[1]
            b-rfq.ship-addr[2] = cust.addr[2]
            b-rfq.ship-city = cust.city
            b-rfq.ship-state = cust.state
            b-rfq.ship-zip = cust.zip
            b-rfq.sman = cust.sman
            b-rfq.fob-code = if cust.fob-code = "Dest" then "D"
                             else if cust.fob-code = "orig" then "O"
                             else ""
            b-rfq.chg-method = if cust.frt-pay = "P" then "Prepaid"
                               else if cust.frt-pay = "C" then "Collect"
                               else if cust.frt-pay = "Bill" then "Bill"
                               else if cust.frt-pay = "T" then "Third Party"
                               else ""
            .      

      find sman where sman.company = gcompany and
                      sman.sman = b-rfq.sman
                      no-lock no-error.
      if avail sman then assign b-rfq.comm = sman.scomm
                                rfq.comm:screen-value in frame {&frame-name} = string(sman.scomm).
                                .
      sman_sname:screen-value = if avail sman then sman.sname else ""  .                        
      IF AVAIL sman THEN FIND FIRST sman-mtx OF sman WHERE sman-mtx.custype = cust.type NO-LOCK NO-ERROR.
      IF AVAIL sman-mtx THEN ASSIGN b-rfq.comm = sman-mtx.type-comm
                                    rfq.comm:screen-value in frame {&frame-name} = string(sman-mtx.type-comm).
   end.  
   if rfq.cust-no:screen-value = "TEMP" then do:
      assign  rfq.ship-name:sensitive = true
              rfq.ship-addr[1]:sensitive = true
              rfq.ship-addr[2]:sensitive = true
              rfq.ship-city:sensitive = true
              rfq.ship-state:sensitive = true
              rfq.ship-zip:sensitive = true
              rfq.ship-name:fgcolor = ?   
              rfq.ship-addr[1]:fgcolor = ?   
              rfq.ship-addr[2]:fgcolor = ?   
              rfq.ship-city:fgcolor = ?   
              rfq.ship-state:fgcolor = ?   
              rfq.ship-zip:fgcolor = ?    
              rfq.ship-name:bgcolor = 15   
              rfq.ship-addr[1]:bgcolor = 15  
              rfq.ship-addr[2]:bgcolor = 15  
              rfq.ship-city:bgcolor = 15  
              rfq.ship-state:bgcolor = 15  
              rfq.ship-zip:bgcolor = 15  
              .

      IF AVAIL cust THEN ASSIGN rfq.fob-code:screen-value in frame {&frame-name} = if cust.fob-code = "Dest" then "D"
                                                                else if cust.fob-code = "orig" then "O"
                                                                else ""
                                rfq.chg-method:screen-value in frame {&frame-name} = if cust.frt-pay = "P" then "Prepaid"
                                        else if cust.frt-pay = "C" then "Collect"
                                        else if cust.frt-pay = "B" then "Bill"
                                        else if cust.frt-pay = "T" then "Third Party"
                                        else "".

      apply "entry" to rfq.ship-name.    
      return no-apply.
   end.                  

   if not avail cust and rfq.cust-no:screen-value <> ""  then do:
      message "Invalid Customer number. Please try help!" view-as alert-box.
      apply "entry" to self.
      return no-apply.
   end.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.due-date V-table-Win
ON HELP OF rfq.due-date IN FRAME F-Main /* Due Date */
DO:
     {methods/calendar.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.fob-code V-table-Win
ON return OF rfq.fob-code IN FRAME F-Main /* FOB Code */
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.req-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.req-date V-table-Win
ON HELP OF rfq.req-date IN FRAME F-Main /* Request Date */
DO:
   {methods/calendar.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.sman V-table-Win
ON LEAVE OF rfq.sman IN FRAME F-Main /* Salesman */
DO:
 {methods/dispflds.i} 
 IF LASTKEY EQ -1 THEN Return .
 {&methods/lValidateError.i YES}
 IF NOT AVAIL sman THEN DO:
    MESSAGE "Invalid Sales Rep." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
 END.
 FIND FIRST cust WHERE cust.company = gcompany
                               AND cust.cust-no = rfq.cust-no:SCREEN-VALUE NO-LOCK NO-ERROR.
 IF AVAIL sman THEN rfq.comm:SCREEN-VALUE = string(sman.scomm,">>9.99").
 IF AVAIL sman THEN 
    FIND FIRST sman-mtx OF sman WHERE sman-mtx.custype = cust.type NO-LOCK NO-ERROR.
 IF AVAIL sman-mtx THEN ASSIGN rfq.comm:screen-value in frame {&frame-name} = string(sman-mtx.type-comm,">>9.99").
 {&methods/lValidateError.i NO}
 END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfq.wh-month
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfq.wh-month V-table-Win
ON return OF rfq.wh-month IN FRAME F-Main /* Warehouse Month */
DO:
   apply "tab" to self.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
    {sys/inc/f3help.i}
session:data-entry-return = true.  /* return key will be like tab key */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rfq V-table-Win 
PROCEDURE add-rfq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').

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
  {src/adm/template/row-list.i "rfq"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfq"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-company V-table-Win 
PROCEDURE assign-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  find first usr where usr.uid = userid(ldbname(1)) no-error.
  if avail usr then do:
  */
     assign rfq.company = gcompany /*usr.company*/
            rfq.loc = gloc /*usr.loc*/ 
            rfq.ship-name = rfq.ship-name:screen-value in frame {&frame-name}                                  
            rfq.ship-addr[1] = rfq.ship-addr[1]:screen-value in frame {&frame-name}
            rfq.ship-addr[2] = rfq.ship-addr[2]:screen-value in frame {&frame-name}
            rfq.ship-city   = rfq.ship-city:screen-value in frame {&frame-name}
            rfq.ship-state  = rfq.ship-state:screen-value in frame {&frame-name}
            rfq.ship-zip    = rfq.ship-zip:screen-value in frame {&frame-name}
            .


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-rfqno V-table-Win 
PROCEDURE get-next-rfqno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var new-rfq# like rfq.rfq-no no-undo.

  find first rfq-ctrl no-error.
  if avail rfq-ctrl then do:
     new-rfq# = rfq-ctrl.rfq-num.
     rfq-ctrl.rfq-num = rfq-ctrl.rfq-num + 1.   
     release rfq-ctrl.    

     return string(new-rfq#).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

    do with frame {&frame-name}:
     assign rfq.ship-name = rfq.ship-name:screen-value
             rfq.ship-addr[1] = rfq.ship-addr[1]:screen-value
             rfq.ship-addr[2] = rfq.ship-addr[2]:screen-value
             rfq.ship-city    = rfq.ship-city:screen-value
             rfq.ship-state   = rfq.ship-state:screen-value
             rfq.ship-zip     = rfq.ship-zip:screen-value
             .
 /*
     case rfq.chg-method:screen-value:
          when "Bill" then rfq.chg-method = "B".
          when "Prepaid" then rfq.chg-method =  "P".
          when "Collect" then rfq.chg-method = "C" .
          when "Third Party" then rfq.chg-method = "T".
     end.  
   */

  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  do with frame {&frame-name}:

     if rfq.cust-no = "temp" or rfq.ship-name:sensitive then                                 
         assign rfq.ship-name:sensitive = no
                rfq.ship-addr[1]:sensitive = no
                rfq.ship-addr[2]:sensitive = no
                rfq.ship-city:sensitive = no
                rfq.ship-state:sensitive = no
                rfq.ship-zip:sensitive = no
                rfq.ship-name:bgcolor = 7   
                rfq.ship-addr[1]:bgcolor = 7   
                rfq.ship-addr[2]:bgcolor = 7   
                rfq.ship-city:bgcolor = 7   
                rfq.ship-state:bgcolor = 7   
                rfq.ship-zip:bgcolor = 7   
                rfq.ship-name:fgcolor = 15   
                rfq.ship-addr[1]:fgcolor = 15   
                rfq.ship-addr[2]:fgcolor = 15   
                rfq.ship-city:fgcolor = 15   
                rfq.ship-state:fgcolor = 15   
                rfq.ship-zip:fgcolor = 15   
            .

  end. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var old-rfq-id as recid no-undo.
  DEF VAR ls-key AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  find first rfq-ctrl no-lock no-error.
  if not avail rfq-ctrl then do:
     message "No RFQ Control Exist. Please Register RFQ Control File First."
             view-as alert-box.
     return error.
  end.      
  old-rfq-id = recid(rfq).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */ 

  run get-next-rfqno.  /*moved to methods/viewers/add/rfq.i   */

  assign rfq.rfq-no:screen-value in frame {&frame-name} = (return-value)
/*         rfq.rfq-no = integer(rfq.rfq-no:screen-value in frame {&frame-name}) */
         rfq.rfq-no = integer(return-value) 
         .
  if not ll-is-copy then do:
     assign   
         rfq.ship-name:screen-value in frame {&frame-name} = ""
         rfq.ship-addr[1]:screen-value in frame {&frame-name} = ""
         rfq.ship-addr[2]:screen-value in frame {&frame-name} = ""
         rfq.ship-city:screen-value in frame {&frame-name} = ""
         rfq.ship-state:screen-value in frame {&frame-name} = ""
         rfq.ship-zip:screen-value in frame {&frame-name} = ""
         rfq.req-date:screen-value in frame {&frame-name} = string(today)
         rfq.due-date:screen-value in frame {&frame-name} = string(today)
         rfq.chg-method:screen-value in frame {&frame-name} = "Bill"
         rfq.wh-month:screen-value in frame {&frame-name} = "1"
         rfq.sman:SCREEN-VALUE = ""
         rfq.comm:SCREEN-VALUE = "0.00"
         sman_sname:SCREEN-VALUE = "".
        .
      assign rfq.req-date = today
             rfq.due-date = today  
             rfq.wh-month = 1
             rfq.comm = 0
             rfq.sman = "". 

       ls-key = string(today,"99999999") +
             string(next-value(rec_key_seq,nosweat),"99999999").
       rfq.rec_key = ls-key.

  end.
  run assign-company.  /* assign company,loc */

  if adm-new-record AND NOT adm-adding-record then do:  /* copy all rfqitem if "Copy" button selected */
     def buffer bf-rfq for rfq.
     def buffer bf-rfqitem for rfqitem.
     find bf-rfq where recid(bf-rfq) = old-rfq-id no-lock.   
     for each rfqitem of bf-rfq no-lock:
         create bf-rfqitem.
         buffer-copy rfqitem except rfqitem.rfq-no rfqitem.est-no to bf-rfqitem.
         assign bf-rfqitem.rfq-no = rfq.rfq-no.

     end.   
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .        

  /* Code placed here will execute AFTER standard behavior.    */
  if avail rfq then
  assign rfq.wh-month:screen-value in frame {&frame-name} = string(rfq.wh-month)
         rfq.chg-method:screen-value in frame {&frame-name} = rfq.chg-method
         rfq.fob-code:screen-value in frame {&frame-name} = rfq.fob-code
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* check mode where in update */
  DEFINE VARIABLE vlChangePages AS LOGICAL NO-UNDO.  
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   def var ll-new-record as log no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-is-copy = if adm-new-record and not adm-adding-record then yes else no.
  ll-new-record = adm-new-record.

  RUN validate-cust NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  do with frame {&frame-name}:        
     if rfq.cust-no = "temp" or rfq.ship-name:sensitive then                                 
         assign rfq.ship-name:sensitive = no
                rfq.ship-addr[1]:sensitive = no
                rfq.ship-addr[2]:sensitive = no
                rfq.ship-city:sensitive = no
                rfq.ship-state:sensitive = no
                rfq.ship-zip:sensitive = no
                rfq.ship-name:bgcolor = 7   
                rfq.ship-addr[1]:bgcolor = 7   
                rfq.ship-addr[2]:bgcolor = 7   
                rfq.ship-city:bgcolor = 7   
                rfq.ship-state:bgcolor = 7   
                rfq.ship-zip:bgcolor = 7   
                rfq.ship-name:fgcolor = 15   
                rfq.ship-addr[1]:fgcolor = 15   
                rfq.ship-addr[2]:fgcolor = 15   
                rfq.ship-city:fgcolor = 15   
                rfq.ship-state:fgcolor = 15   
                rfq.ship-zip:fgcolor = 15   
            .

  end. 


  if ll-new-record then do:
     def var source-str as cha no-undo.
     RUN get-link-handle IN adm-broker-hdl 
        (this-procedure, 'record-SOURCE':U, OUTPUT source-str).       
     run refreshRow in widget-handle(source-str) ("newRecord",rowid(rfq) ).

     RUN get-link-handle IN adm-broker-hdl 
        (this-procedure, 'CONTAINER-SOURCE':U, OUTPUT source-str).
     run select-page in widget-handle(source-str) (3).
  end.   

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
  {src/adm/template/snd-list.i "rfq"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-cust V-table-Win 
PROCEDURE validate-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  IF NOT CAN-FIND (cust where cust.company = gcompany     and
                   cust.cust-no = rfq.cust-no:screen-value in frame {&frame-name} )
     OR rfq.cust-no:SCREEN-VALUE = "" THEN DO:

     MESSAGE "Invalid Cust#. Try Help." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO rfq.cust-no.
     RETURN ERROR.
  END.

  IF rfq.cust-no:SCREEN-VALUE = "TEMP" THEN DO:
     IF rfq.ship-name:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Customer Name must be entered. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rfq.ship-name.
        RETURN ERROR.
     END.
     IF rfq.ship-city:SCREEN-VALUE = "" THEN DO:
        MESSAGE "City must be entered. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rfq.ship-city.
        RETURN ERROR.
     END.
     IF rfq.ship-state:SCREEN-VALUE = "" THEN DO:
        MESSAGE "State must be entered. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rfq.ship-state.
        RETURN ERROR.
     END.
     FIND FIRST statecod WHERE statecod.statecod = rfq.ship-state:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL statecod THEN DO:
        MESSAGE "Invalid State." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rfq.ship-state.
        RETURN ERROR.
     END.

  END.

 FIND sman where sman.company = gcompany and
                 sman.sman = rfq.sman:SCREEN-VALUE no-lock no-error.
 IF NOT AVAIL sman THEN DO:
    MESSAGE "Invalid Sales Rep. " VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO rfq.sman.
    RETURN ERROR.
 END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

