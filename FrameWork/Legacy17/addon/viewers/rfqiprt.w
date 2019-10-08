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

  File: addon/viewers/rfqiprt.w

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
&Scoped-define EXTERNAL-TABLES rfqitem
&Scoped-define FIRST-EXTERNAL-TABLE rfqitem


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfqitem.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.i-col rfqitem.i-pass rfqitem.i-coat ~
rfqitem.i-coldscr rfqitem.i-ps[1] rfqitem.i-code[1] rfqitem.i-dscr[1] ~
rfqitem.i-%[1] rfqitem.i-ps[2] rfqitem.i-code[2] rfqitem.i-dscr[2] ~
rfqitem.i-%[2] rfqitem.i-ps[3] rfqitem.i-code[3] rfqitem.i-dscr[3] ~
rfqitem.i-%[3] rfqitem.i-ps[4] rfqitem.i-code[4] rfqitem.i-dscr[4] ~
rfqitem.i-%[4] rfqitem.i-ps[5] rfqitem.i-code[5] rfqitem.i-dscr[5] ~
rfqitem.i-%[5] rfqitem.i-ps[6] rfqitem.i-code[6] rfqitem.i-dscr[6] ~
rfqitem.i-%[6] rfqitem.i-ps[7] rfqitem.i-code[7] rfqitem.i-dscr[7] ~
rfqitem.i-%[7] rfqitem.i-ps[8] rfqitem.i-code[8] rfqitem.i-dscr[8] ~
rfqitem.i-%[8] rfqitem.i-ps[9] rfqitem.i-code[9] rfqitem.i-dscr[9] ~
rfqitem.i-%[9] rfqitem.i-ps[10] rfqitem.i-code[10] rfqitem.i-dscr[10] ~
rfqitem.i-%[10] 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-27 
&Scoped-Define DISPLAYED-FIELDS rfqitem.i-col rfqitem.i-pass rfqitem.i-coat ~
rfqitem.i-coldscr rfqitem.i-ps[1] rfqitem.i-code[1] rfqitem.i-dscr[1] ~
rfqitem.i-%[1] rfqitem.i-ps[2] rfqitem.i-code[2] rfqitem.i-dscr[2] ~
rfqitem.i-%[2] rfqitem.i-ps[3] rfqitem.i-code[3] rfqitem.i-dscr[3] ~
rfqitem.i-%[3] rfqitem.i-ps[4] rfqitem.i-code[4] rfqitem.i-dscr[4] ~
rfqitem.i-%[4] rfqitem.i-ps[5] rfqitem.i-code[5] rfqitem.i-dscr[5] ~
rfqitem.i-%[5] rfqitem.i-ps[6] rfqitem.i-code[6] rfqitem.i-dscr[6] ~
rfqitem.i-%[6] rfqitem.i-ps[7] rfqitem.i-code[7] rfqitem.i-dscr[7] ~
rfqitem.i-%[7] rfqitem.i-ps[8] rfqitem.i-code[8] rfqitem.i-dscr[8] ~
rfqitem.i-%[8] rfqitem.i-ps[9] rfqitem.i-code[9] rfqitem.i-dscr[9] ~
rfqitem.i-%[9] rfqitem.i-ps[10] rfqitem.i-code[10] rfqitem.i-dscr[10] ~
rfqitem.i-%[10] 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS v-coat-ps 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE BUTTON bt-ink-detail 
     LABEL "&Ink Reset" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE v-coat-ps AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Pass" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 115 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.i-col AT ROW 1.48 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-pass AT ROW 1.48 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-coat AT ROW 1.48 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     v-coat-ps AT ROW 1.48 COL 61.6 COLON-ALIGNED
     rfqitem.i-coldscr AT ROW 2.52 COL 17.2 COLON-ALIGNED FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     rfqitem.i-ps[1] AT ROW 4.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[1] AT ROW 4.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[1] AT ROW 4.57 COL 41 COLON-ALIGNED NO-LABEL FORMAT "x(19)"
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[1] AT ROW 4.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     bt-ink-detail AT ROW 4.57 COL 98
     rfqitem.i-ps[2] AT ROW 5.52 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[2] AT ROW 5.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[2] AT ROW 5.52 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[2] AT ROW 5.52 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[3] AT ROW 6.48 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[3] AT ROW 6.48 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[3] AT ROW 6.48 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[3] AT ROW 6.48 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[4] AT ROW 7.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[4] AT ROW 7.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[4] AT ROW 7.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[4] AT ROW 7.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[5] AT ROW 8.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[5] AT ROW 8.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[5] AT ROW 8.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[5] AT ROW 8.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[6] AT ROW 9.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[6] AT ROW 9.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[6] AT ROW 9.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[6] AT ROW 9.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rfqitem.i-ps[7] AT ROW 10.52 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[7] AT ROW 10.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[7] AT ROW 10.52 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[7] AT ROW 10.52 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[8] AT ROW 11.48 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[8] AT ROW 11.48 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[8] AT ROW 11.48 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[8] AT ROW 11.48 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[9] AT ROW 12.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[9] AT ROW 12.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[9] AT ROW 12.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[9] AT ROW 12.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rfqitem.i-ps[10] AT ROW 13.57 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     rfqitem.i-code[10] AT ROW 13.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.i-dscr[10] AT ROW 13.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     rfqitem.i-%[10] AT ROW 13.57 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     "Description" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 3.62 COL 50
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 3.62 COL 88
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.62 COL 24
     "Pass" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.62 COL 12
     RECT-27 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfqitem
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
         HEIGHT             = 19.33
         WIDTH              = 118.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-ink-detail IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-ink-detail:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN rfqitem.i-coldscr IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.i-dscr[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN v-coat-ps IN FRAME F-Main
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
ON GO OF FRAME F-Main
DO:
   def var out-hdl-str as cha no-undo.
   run get-link-handle in adm-broker-hdl (this-procedure,"TABLEIO-SOURCE",output out-hdl-str).   
   run notify in widget-handle(out-hdl-str) ("update-record").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var lv-ind like style.industry no-undo.
    def var lv-handle as handle no-undo.

    lv-handle = focus:handle.

    case focus:name :
         when "i-code" then do:
             find style where style.company = rfqitem.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
             if avail style then lv-ind = style.industry.
             else lv-ind = "".  
             run windows/l-item2.w (rfqitem.company, lv-ind, "I",focus:screen-value, output char-val).
             if char-val <> "" then do:
                  assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                  case focus:index:        
                       when 1 then rfqitem.i-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 2 then rfqitem.i-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 3 then rfqitem.i-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 4 then rfqitem.i-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 5 then rfqitem.i-dscr[5]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 6 then rfqitem.i-dscr[6]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 7 then rfqitem.i-dscr[7]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 8 then rfqitem.i-dscr[8]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 9 then rfqitem.i-dscr[9]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 10 then rfqitem.i-dscr[10]:screen-value in frame {&frame-name} = entry(2,char-val) .
                  end case.       
             end.         
             return no-apply.                       

         end.



    end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ink-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ink-detail V-table-Win
ON CHOOSE OF bt-ink-detail IN FRAME F-Main /* Ink Reset */
DO:
    run calc-pass.
    /*run windows/rfqiprt.w  (recid(rfqitem)).  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[10] V-table-Win
ON LEAVE OF rfqitem.i-code[10] IN FRAME F-Main /* Code[10] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[10]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[10].
     return no-apply.
  end.
  if rfqitem.i-ps[10]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  rfqitem.i-dscr[10]:screen-value in frame {&frame-name} = item.i-name.
  end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[1] V-table-Win
ON LEAVE OF rfqitem.i-code[1] IN FRAME F-Main /* Code[1] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[1]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[1].
     return no-apply.
  end.
  if rfqitem.i-ps[1]:screen-value <> "0" and lastkey <> -1 then do:
   find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.

   if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
      message "Invalid Material Type. Try Help." view-as alert-box error.
      return no-apply.
   end.
   if not avail item then do:
      message "Invalid Item. Try Help." view-as alert-box error.
      return no-apply.
   end.

   if avail item then  rfqitem.i-dscr[1]:screen-value in frame {&frame-name} = item.i-name.
 end.
{&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[2] V-table-Win
ON LEAVE OF rfqitem.i-code[2] IN FRAME F-Main /* Code[2] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[2]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[2].
     return no-apply.
  end.

   if rfqitem.i-ps[2]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
      message "Invalid Material Type. Try Help." view-as alert-box error.
      return no-apply.
   end.
   if not avail item then do:
      message "Invalid Item. Try Help." view-as alert-box error.
      return no-apply.
   end.

   if avail item then  rfqitem.i-dscr[2]:screen-value in frame {&frame-name} = item.i-name.
  end.
 {&methods/lValidateError.i NO}
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[3] V-table-Win
ON LEAVE OF rfqitem.i-code[3] IN FRAME F-Main /* Code[3] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[3]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[3].
     return no-apply.
  end.

   if rfqitem.i-ps[3]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

   if avail item then  rfqitem.i-dscr[3]:screen-value in frame {&frame-name} = item.i-name.
  end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[4] V-table-Win
ON LEAVE OF rfqitem.i-code[4] IN FRAME F-Main /* Code[4] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[4]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
       apply "entry" to rfqitem.i-ps[4].
     return no-apply.
  end.

   if rfqitem.i-ps[4]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
      message "Invalid Material Type. Try Help." view-as alert-box error.
      return no-apply.
     end.
     if not avail item then do:
      message "Invalid Item. Try Help." view-as alert-box error.
      return no-apply.
     end.

     if avail item then  rfqitem.i-dscr[4]:screen-value in frame {&frame-name} = item.i-name.
   end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[5] V-table-Win
ON LEAVE OF rfqitem.i-code[5] IN FRAME F-Main /* Code[5] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[5]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[5].
     return no-apply.
  end.

    if rfqitem.i-ps[5]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  rfqitem.i-dscr[5]:screen-value in frame {&frame-name} = item.i-name.
    end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[6] V-table-Win
ON LEAVE OF rfqitem.i-code[6] IN FRAME F-Main /* Code[6] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[6]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[6].
     return no-apply.
  end.

    if rfqitem.i-ps[6]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  rfqitem.i-dscr[6]:screen-value in frame {&frame-name} = item.i-name.
  end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[7] V-table-Win
ON LEAVE OF rfqitem.i-code[7] IN FRAME F-Main /* Code[7] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[7]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[7].
     return no-apply.
  end.
  if rfqitem.i-ps[7]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.                 
     if avail item then  rfqitem.i-dscr[7]:screen-value in frame {&frame-name} = item.i-name.
   end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[8] V-table-Win
ON LEAVE OF rfqitem.i-code[8] IN FRAME F-Main /* Code[8] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[8]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[8].
    return no-apply.
  end.
 if rfqitem.i-ps[8]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
    if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
    end.

   if avail item then  rfqitem.i-dscr[8]:screen-value in frame {&frame-name} = item.i-name.
  end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.i-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.i-code[9] V-table-Win
ON LEAVE OF rfqitem.i-code[9] IN FRAME F-Main /* Code[9] */
DO:
if lastkey = -1 then return.
{&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and rfqitem.i-ps[9]:screen-value = "0" then do:
     message "Enter Valid Pass." view-as alert-box.
     apply "entry" to rfqitem.i-ps[9].
     return no-apply.
  end.
  if rfqitem.i-ps[9]:screen-value <> "0" and lastkey <> -1 then do:

     find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" and item.mat-type <> "V" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.

     if avail item then  rfqitem.i-dscr[9]:screen-value in frame {&frame-name} = item.i-name.
  end.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
  session:data-entry-return = true.  /* return key will be like tab key */
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "rfqitem"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfqitem"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pass V-table-Win 
PROCEDURE calc-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  message "Are you sure you want to reset all print codes?" view-as alert-box question
         button yes-no update ll-ans as log.
  if not ll-ans then return no-apply.       

/*********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.

      find first style where style.company = rfqitem.company and
                 style.style = rfqitem.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = rfqitem.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = rfqitem.company  and
                 alt-item.mat-type = "V"     and
                 alt-item.i-no     = style.material[6]
                 no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first rfq-ctrl where rfq-ctrl.company = rfqitem.company and
                                   rfq-ctrl.loc = rfqitem.loc
                                   no-lock no-error.
         if k = 0 then k = rfq-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = rfqitem.company and
                       item.i-no = rfq-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = rfqitem.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = rfq-ctrl.def-coat
                                      no-lock no-error.
      end.

      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (integer(rfqitem.i-col:screen-value in frame {&frame-name})
          + integer(rfqitem.i-coat:screen-value in frame {&frame-name})) 
          / integer(rfqitem.i-pass:screen-value in frame {&frame-name})
      counter = 1
      choice = true.

      {sys/inc/roundup.i j}

/*    do i = 1 to 10:
       if rfqitem.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */

/*    find bf-rfqitem of rfqitem exclusive-lock.    */
      if choice then do i = 1 to 10:
         if i le integer(rfqitem.i-col:screen-value) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
             /* assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = item.i-no
                     bf-rfqitem.i-dscr[i] = item.est-dscr
                     bf-rfqitem.i-%[i]    = k.
             */
             case string(i) :
                when "1" then assign rfqitem.i-ps[1]:screen-value   = string(counter)
                                     rfqitem.i-code[1]:screen-value = item.i-no
                                     rfqitem.i-dscr[1]:screen-value = item.est-dscr
                                     rfqitem.i-%[1]:screen-value    = string(k).
                when "2" then assign rfqitem.i-ps[2]:screen-value   = string(counter)
                                     rfqitem.i-code[2]:screen-value = item.i-no
                                     rfqitem.i-dscr[2]:screen-value = item.est-dscr
                                     rfqitem.i-%[2]:screen-value    = string(k).
                when "3" then assign rfqitem.i-ps[3]:screen-value   = string(counter)
                                     rfqitem.i-code[3]:screen-value = item.i-no
                                     rfqitem.i-dscr[3]:screen-value = item.est-dscr
                                     rfqitem.i-%[3]:screen-value    = string(k).
                when "4" then assign rfqitem.i-ps[4]:screen-value   = string(counter)
                                     rfqitem.i-code[4]:screen-value = item.i-no
                                     rfqitem.i-dscr[4]:screen-value = item.est-dscr
                                     rfqitem.i-%[4]:screen-value    = string(k).
                when "5" then assign rfqitem.i-ps[5]:screen-value   = string(counter)
                                     rfqitem.i-code[5]:screen-value = item.i-no
                                     rfqitem.i-dscr[5]:screen-value = item.est-dscr
                                     rfqitem.i-%[5]:screen-value    = string(k).
                when "6" then assign rfqitem.i-ps[6]:screen-value   = string(counter)
                                     rfqitem.i-code[6]:screen-value = item.i-no
                                     rfqitem.i-dscr[6]:screen-value = item.est-dscr
                                     rfqitem.i-%[6]:screen-value    = string(k).
                when "7" then assign rfqitem.i-ps[7]:screen-value   = string(counter)
                                     rfqitem.i-code[7]:screen-value = item.i-no
                                     rfqitem.i-dscr[7]:screen-value = item.est-dscr
                                     rfqitem.i-%[7]:screen-value    = string(k).
                when "8" then assign rfqitem.i-ps[8]:screen-value   = string(counter)
                                     rfqitem.i-code[8]:screen-value = item.i-no
                                     rfqitem.i-dscr[8]:screen-value = item.est-dscr
                                     rfqitem.i-%[8]:screen-value    = string(k).
                when "9" then assign rfqitem.i-ps[9]:screen-value   = string(counter)
                                     rfqitem.i-code[9]:screen-value = item.i-no
                                     rfqitem.i-dscr[9]:screen-value = item.est-dscr
                                     rfqitem.i-%[9]:screen-value    = string(k).
                when "10" then assign rfqitem.i-ps[10]:screen-value   = string(counter)
                                     rfqitem.i-code[10]:screen-value = item.i-no
                                     rfqitem.i-dscr[10]:screen-value = item.est-dscr
                                     rfqitem.i-%[10]:screen-value    = string(k).             
             end case.
         end.
         else if (i > integer(rfqitem.i-col:screen-value)) and
                 (i <= (integer(rfqitem.i-col:screen-value) + 
                       integer(rfqitem.i-coat:screen-value)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         /*     assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = alt-item.i-no
                     bf-rfqitem.i-dscr[i] = alt-item.est-dscr
                     bf-rfqitem.i-%[i]    = 100.
           */
              case string(i) :
                when "1" then assign rfqitem.i-ps[1]:screen-value   = string(counter)
                                     rfqitem.i-code[1]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[1]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[1]:screen-value    = "100".
                when "2" then assign rfqitem.i-ps[2]:screen-value   = string(counter)
                                     rfqitem.i-code[2]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[2]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[2]:screen-value    = "100".
                when "3" then assign rfqitem.i-ps[3]:screen-value   = string(counter)
                                     rfqitem.i-code[3]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[3]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[3]:screen-value    = "100".
                when "4" then assign rfqitem.i-ps[4]:screen-value   = string(counter)
                                     rfqitem.i-code[4]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[4]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[4]:screen-value    = "100".
                when "5" then assign rfqitem.i-ps[5]:screen-value   = string(counter)
                                     rfqitem.i-code[5]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[5]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[5]:screen-value    = "100".
                when "6" then assign rfqitem.i-ps[6]:screen-value   = string(counter)
                                     rfqitem.i-code[6]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[6]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[6]:screen-value    = "100".
                when "7" then assign rfqitem.i-ps[7]:screen-value   = string(counter)
                                     rfqitem.i-code[7]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[7]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[7]:screen-value    = "100".
                when "8" then assign rfqitem.i-ps[8]:screen-value   = string(counter)
                                     rfqitem.i-code[8]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[8]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[8]:screen-value    = "100".
                when "9" then assign rfqitem.i-ps[9]:screen-value   = string(counter)
                                     rfqitem.i-code[9]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[9]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[9]:screen-value    = "100".
                when "10" then assign rfqitem.i-ps[10]:screen-value   = string(counter)
                                     rfqitem.i-code[10]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[10]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[10]:screen-value    = "100".             
             end.                   
         end.
         else if (i >  integer(rfqitem.i-col:screen-value) + 
                       integer(rfqitem.i-coat:screen-value) )
         then do:
        /*    assign bf-rfqitem.i-ps[i]   = 0  
                     bf-rfqitem.i-code[i] = ""
                     bf-rfqitem.i-dscr[i] = "" 
                     bf-rfqitem.i-%[i]    = 0.  */
              case string(i) :
                   when "1" then assign rfqitem.i-ps[1]:screen-value   = "0"
                                        rfqitem.i-code[1]:screen-value = ""
                                        rfqitem.i-dscr[1]:screen-value = ""
                                        rfqitem.i-%[1]:screen-value    = "0".
                   when "2" then assign rfqitem.i-ps[2]:screen-value   = "0"
                                        rfqitem.i-code[2]:screen-value = ""
                                        rfqitem.i-dscr[2]:screen-value = ""
                                        rfqitem.i-%[2]:screen-value    = "0".
                   when "3" then assign rfqitem.i-ps[3]:screen-value   = "0"
                                        rfqitem.i-code[3]:screen-value = ""
                                        rfqitem.i-dscr[3]:screen-value = ""
                                        rfqitem.i-%[3]:screen-value    = "0".
                   when "4" then assign rfqitem.i-ps[4]:screen-value   = "0"
                                        rfqitem.i-code[4]:screen-value = ""
                                        rfqitem.i-dscr[4]:screen-value = ""
                                        rfqitem.i-%[4]:screen-value    = "0".
                   when "5" then assign rfqitem.i-ps[5]:screen-value   = "0"
                                        rfqitem.i-code[5]:screen-value = ""
                                        rfqitem.i-dscr[5]:screen-value = ""
                                        rfqitem.i-%[5]:screen-value    = "0".
                   when "6" then assign rfqitem.i-ps[6]:screen-value   = "0"
                                        rfqitem.i-code[6]:screen-value = ""
                                        rfqitem.i-dscr[6]:screen-value = ""
                                        rfqitem.i-%[6]:screen-value    = "0".
                   when "7" then assign rfqitem.i-ps[7]:screen-value   = "0"
                                        rfqitem.i-code[7]:screen-value = ""
                                        rfqitem.i-dscr[7]:screen-value = ""
                                        rfqitem.i-%[7]:screen-value    = "0".
                   when "8" then assign rfqitem.i-ps[8]:screen-value   = "0"
                                        rfqitem.i-code[8]:screen-value = ""
                                        rfqitem.i-dscr[8]:screen-value = ""
                                        rfqitem.i-%[8]:screen-value    = "0".
                   when "9" then assign rfqitem.i-ps[9]:screen-value   = "0"
                                        rfqitem.i-code[9]:screen-value = ""
                                        rfqitem.i-dscr[9]:screen-value = ""
                                        rfqitem.i-%[9]:screen-value    = "0".
                   when "10" then assign rfqitem.i-ps[10]:screen-value   = "0"
                                        rfqitem.i-code[10]:screen-value = ""
                                        rfqitem.i-dscr[10]:screen-value = ""
                                        rfqitem.i-%[10]:screen-value    = "0".

              end case.       

         end.
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(rfqitem.i-pass:screen-value) then counter = integer(rfqitem.i-pass:screen-value).

      end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-pass-update V-table-Win 
PROCEDURE calc-pass-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     copied from calc-pass.
             all same except not updating existing infos.
             i le integer(rfqitem.i-col:screen-value) => don't do anything
------------------------------------------------------------------------------*/
/*********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.

      find first style where style.company = rfqitem.company and
                 style.style = rfqitem.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).
         find first item where item.company = rfqitem.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).
         find first alt-item where alt-item.company  = rfqitem.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first rfq-ctrl where rfq-ctrl.company = rfqitem.company and
                                   rfq-ctrl.loc = rfqitem.loc
                                   no-lock no-error.
         if k = 0 then k = rfq-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = rfqitem.company and
                       item.i-no = rfq-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = rfqitem.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = rfq-ctrl.def-coat
                                      no-lock no-error.
      end.

      save_id = recid(item). save_id2 = recid(alt-item).
      j = (integer(rfqitem.i-col:screen-value in frame {&frame-name})
          + integer(rfqitem.i-coat:screen-value in frame {&frame-name})  ) 
          / integer(rfqitem.i-pass:screen-value in frame {&frame-name}).
      {sys/inc/roundup.i j}
      counter = 1.
      choice = true.
/*    do i = 1 to 10:
       if rfqitem.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */

/*    find bf-rfqitem of rfqitem exclusive-lock.    */
      if choice then do i = 1 to 10:
      /*=============== chagned from calc-pass ===========
           ============ don't update existing info in calc-pass-update
         if i le integer(rfqitem.i-col:screen-value) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
             /* assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = item.i-no
                     bf-rfqitem.i-dscr[i] = item.est-dscr
                     bf-rfqitem.i-%[i]    = k.
             */
             case string(i) :
                when "1" then assign rfqitem.i-ps[1]:screen-value   = string(counter)
                                     rfqitem.i-code[1]:screen-value = item.i-no
                                     rfqitem.i-dscr[1]:screen-value = item.est-dscr
                                     rfqitem.i-%[1]:screen-value    = string(k).
                when "2" then assign rfqitem.i-ps[2]:screen-value   = string(counter)
                                     rfqitem.i-code[2]:screen-value = item.i-no
                                     rfqitem.i-dscr[2]:screen-value = item.est-dscr
                                     rfqitem.i-%[2]:screen-value    = string(k).
                when "3" then assign rfqitem.i-ps[3]:screen-value   = string(counter)
                                     rfqitem.i-code[3]:screen-value = item.i-no
                                     rfqitem.i-dscr[3]:screen-value = item.est-dscr
                                     rfqitem.i-%[3]:screen-value    = string(k).
                when "4" then assign rfqitem.i-ps[4]:screen-value   = string(counter)
                                     rfqitem.i-code[4]:screen-value = item.i-no
                                     rfqitem.i-dscr[4]:screen-value = item.est-dscr
                                     rfqitem.i-%[4]:screen-value    = string(k).
                when "5" then assign rfqitem.i-ps[5]:screen-value   = string(counter)
                                     rfqitem.i-code[5]:screen-value = item.i-no
                                     rfqitem.i-dscr[5]:screen-value = item.est-dscr
                                     rfqitem.i-%[5]:screen-value    = string(k).
                when "6" then assign rfqitem.i-ps[6]:screen-value   = string(counter)
                                     rfqitem.i-code[6]:screen-value = item.i-no
                                     rfqitem.i-dscr[6]:screen-value = item.est-dscr
                                     rfqitem.i-%[6]:screen-value    = string(k).
                when "7" then assign rfqitem.i-ps[7]:screen-value   = string(counter)
                                     rfqitem.i-code[7]:screen-value = item.i-no
                                     rfqitem.i-dscr[7]:screen-value = item.est-dscr
                                     rfqitem.i-%[7]:screen-value    = string(k).
                when "8" then assign rfqitem.i-ps[8]:screen-value   = string(counter)
                                     rfqitem.i-code[8]:screen-value = item.i-no
                                     rfqitem.i-dscr[8]:screen-value = item.est-dscr
                                     rfqitem.i-%[8]:screen-value    = string(k).
                when "9" then assign rfqitem.i-ps[9]:screen-value   = string(counter)
                                     rfqitem.i-code[9]:screen-value = item.i-no
                                     rfqitem.i-dscr[9]:screen-value = item.est-dscr
                                     rfqitem.i-%[9]:screen-value    = string(k).
                when "10" then assign rfqitem.i-ps[10]:screen-value   = string(counter)
                                     rfqitem.i-code[10]:screen-value = item.i-no
                                     rfqitem.i-dscr[10]:screen-value = item.est-dscr
                                     rfqitem.i-%[10]:screen-value    = string(k).             
             end case.
         end.
        ======================================*/
         if i lt integer(rfqitem.i-col:screen-value) then do with frame {&frame-name}:

         end.
         else if i = integer(rfqitem.i-col:screen-value) /* and ... */ 
         then do with frame {&frame-name}:
              /* need to change varn to ink .. */
         end.
         else if (i > integer(rfqitem.i-col:screen-value)) and  
                 (i <= (integer(rfqitem.i-col:screen-value) + 
                       integer(rfqitem.i-coat:screen-value)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         /*     assign bf-rfqitem.i-ps[i]   = counter
                     bf-rfqitem.i-code[i] = alt-item.i-no
                     bf-rfqitem.i-dscr[i] = alt-item.est-dscr
                     bf-rfqitem.i-%[i]    = 100.
           */
              case string(i) :
                when "1" then assign rfqitem.i-ps[1]:screen-value   = string(counter)
                                     rfqitem.i-code[1]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[1]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[1]:screen-value    = "100".
                when "2" then assign rfqitem.i-ps[2]:screen-value   = string(counter)
                                     rfqitem.i-code[2]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[2]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[2]:screen-value    = "100".
                when "3" then assign rfqitem.i-ps[3]:screen-value   = string(counter)
                                     rfqitem.i-code[3]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[3]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[3]:screen-value    = "100".
                when "4" then assign rfqitem.i-ps[4]:screen-value   = string(counter)
                                     rfqitem.i-code[4]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[4]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[4]:screen-value    = "100".
                when "5" then assign rfqitem.i-ps[5]:screen-value   = string(counter)
                                     rfqitem.i-code[5]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[5]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[5]:screen-value    = "100".
                when "6" then assign rfqitem.i-ps[6]:screen-value   = string(counter)
                                     rfqitem.i-code[6]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[6]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[6]:screen-value    = "100".
                when "7" then assign rfqitem.i-ps[7]:screen-value   = string(counter)
                                     rfqitem.i-code[7]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[7]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[7]:screen-value    = "100".
                when "8" then assign rfqitem.i-ps[8]:screen-value   = string(counter)
                                     rfqitem.i-code[8]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[8]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[8]:screen-value    = "100".
                when "9" then assign rfqitem.i-ps[9]:screen-value   = string(counter)
                                     rfqitem.i-code[9]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[9]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[9]:screen-value    = "100".
                when "10" then assign rfqitem.i-ps[10]:screen-value   = string(counter)
                                     rfqitem.i-code[10]:screen-value = alt-item.i-no
                                     rfqitem.i-dscr[10]:screen-value = alt-item.est-dscr
                                     rfqitem.i-%[10]:screen-value    = "100".             
             end.                   
         end.
         else if (i >  integer(rfqitem.i-col:screen-value) + 
                       integer(rfqitem.i-coat:screen-value) )
         then do:
        /*    assign bf-rfqitem.i-ps[i]   = 0  
                     bf-rfqitem.i-code[i] = ""
                     bf-rfqitem.i-dscr[i] = "" 
                     bf-rfqitem.i-%[i]    = 0.  */
              case string(i) :
                   when "1" then assign rfqitem.i-ps[1]:screen-value   = "0"
                                        rfqitem.i-code[1]:screen-value = ""
                                        rfqitem.i-dscr[1]:screen-value = ""
                                        rfqitem.i-%[1]:screen-value    = "0".
                   when "2" then assign rfqitem.i-ps[2]:screen-value   = "0"
                                        rfqitem.i-code[2]:screen-value = ""
                                        rfqitem.i-dscr[2]:screen-value = ""
                                        rfqitem.i-%[2]:screen-value    = "0".
                   when "3" then assign rfqitem.i-ps[3]:screen-value   = "0"
                                        rfqitem.i-code[3]:screen-value = ""
                                        rfqitem.i-dscr[3]:screen-value = ""
                                        rfqitem.i-%[3]:screen-value    = "0".
                   when "4" then assign rfqitem.i-ps[4]:screen-value   = "0"
                                        rfqitem.i-code[4]:screen-value = ""
                                        rfqitem.i-dscr[4]:screen-value = ""
                                        rfqitem.i-%[4]:screen-value    = "0".
                   when "5" then assign rfqitem.i-ps[5]:screen-value   = "0"
                                        rfqitem.i-code[5]:screen-value = ""
                                        rfqitem.i-dscr[5]:screen-value = ""
                                        rfqitem.i-%[5]:screen-value    = "0".
                   when "6" then assign rfqitem.i-ps[6]:screen-value   = "0"
                                        rfqitem.i-code[6]:screen-value = ""
                                        rfqitem.i-dscr[6]:screen-value = ""
                                        rfqitem.i-%[6]:screen-value    = "0".
                   when "7" then assign rfqitem.i-ps[7]:screen-value   = "0"
                                        rfqitem.i-code[7]:screen-value = ""
                                        rfqitem.i-dscr[7]:screen-value = ""
                                        rfqitem.i-%[7]:screen-value    = "0".
                   when "8" then assign rfqitem.i-ps[8]:screen-value   = "0"
                                        rfqitem.i-code[8]:screen-value = ""
                                        rfqitem.i-dscr[8]:screen-value = ""
                                        rfqitem.i-%[8]:screen-value    = "0".
                   when "9" then assign rfqitem.i-ps[9]:screen-value   = "0"
                                        rfqitem.i-code[9]:screen-value = ""
                                        rfqitem.i-dscr[9]:screen-value = ""
                                        rfqitem.i-%[9]:screen-value    = "0".
                   when "10" then assign rfqitem.i-ps[10]:screen-value   = "0"
                                        rfqitem.i-code[10]:screen-value = ""
                                        rfqitem.i-dscr[10]:screen-value = ""
                                        rfqitem.i-%[10]:screen-value    = "0".

              end case.       

         end.
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(rfqitem.i-pass:screen-value) then counter = integer(rfqitem.i-pass:screen-value).

      end.

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
  bt-ink-detail:sensitive in frame {&frame-name} = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  bt-ink-detail:sensitive in frame {&frame-name} = false.
  /* maybe later...
  def var li-cnt as int  no-undo.
  do li-cnt = 1 to 10:
     IF rfqitem.i-ps[li-cnt]= 0
        then disable rfqitem.i-code[li-cnt] i-%[li-cnt] with frame {&frame-name}.
  end.
  */   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  v-coat-ps = IF rfqitem.i-coat:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "0" THEN 0
              ELSE 1.
  DISPLAY v-coat-ps WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-num-of-code as int no-undo.
{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
     li-num-of-code = 0.
     if rfqitem.i-code[1]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[2]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[3]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[4]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[5]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[6]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[7]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[8]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[9]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if rfqitem.i-code[10]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.

     if li-num-of-code <> (integer(rfqitem.i-col:screen-value) + 
                          integer(rfqitem.i-coat:screen-value) )
     then do:
          message "Invalid Number of Color and Coating." view-as alert-box.
          return no-apply.
     end.                     

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  bt-ink-detail:sensitive in frame {&frame-name} = true.
  assign rfqitem.i-dscr[1] = rfqitem.i-dscr[1]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[2] = rfqitem.i-dscr[2]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[3] = rfqitem.i-dscr[3]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[4] = rfqitem.i-dscr[4]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[5] = rfqitem.i-dscr[5]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[6] = rfqitem.i-dscr[6]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[7] = rfqitem.i-dscr[7]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[8] = rfqitem.i-dscr[8]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[9] = rfqitem.i-dscr[9]:screen-value in frame {&frame-name} 
         rfqitem.i-dscr[10] = rfqitem.i-dscr[10]:screen-value in frame {&frame-name} 
         .
  /* run calc-pass-update. /* update only for new infos */
  */
{&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view V-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  v-coat-ps = IF rfqitem.i-coat:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "0" THEN 0
              ELSE 1.
  DISPLAY v-coat-ps WITH FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "rfqitem"}

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

