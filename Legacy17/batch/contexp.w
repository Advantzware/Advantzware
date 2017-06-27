&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: batch\contexp.w

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
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttcontact NO-UNDO
         FIELD company        LIKE cust.company
         FIELD cust-no        AS CHAR     
         FIELD ship-id        AS CHAR     
         FIELD sman           AS CHAR     
         FIELD first-name     AS CHAR  
         FIELD last-name      AS CHAR   
         FIELD middle-initial AS CHAR
         FIELD sirname        AS CHAR
         FIELD type           AS CHAR
         FIELD cust-name      AS CHAR 
         FIELD addr1          AS CHAR 
         FIELD addr2          AS CHAR   
         FIELD city           AS CHAR      
         FIELD state          AS CHAR     
         FIELD zip            AS CHAR      
         FIELD country        AS CHAR 
         FIELD territory      AS CHAR 
         FIELD phone          AS CHAR  
         FIELD cell-phone     AS CHAR 
         FIELD fax            AS CHAR  
         FIELD extension      AS CHAR  
         FIELD email          AS CHAR   
         FIELD website        AS CHAR
         FIELD attention    LIKE phone.attention 
         FIELD titlcode     LIKE phone.titlcode  .

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_slmn ~
end_slmn fi_temp-cust fi_temp-shipto fi_temp-titles tit-code fi_add-book ~
list-name fi_file Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_slmn ~
end_slmn fi_temp-cust fi_temp-shipto fi_temp-titles tit-code fi_add-book ~
list-name fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6.

DEFINE VARIABLE begin_slmn AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE end_slmn AS CHARACTER FORMAT "X(3)" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Downloading Contact for Customer" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)" INITIAL "c:~\tmp~\r-contexp.csv" 
     LABEL "Output File Path" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE list-name AS CHARACTER FORMAT "X(50)":U 
     LABEL "List Name" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tit-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Title Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_add-book AS LOGICAL INITIAL no 
     LABEL "Download to Address Book?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40.2 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_temp-cust AS LOGICAL INITIAL yes 
     LABEL "Download Customer Contacts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45.2 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_temp-shipto AS LOGICAL INITIAL yes 
     LABEL "Download Shipto Contacts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45.2 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_temp-titles AS LOGICAL INITIAL no 
     LABEL "Select specific Contact Titles?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40.2 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 1.48 COL 35 COLON-ALIGNED
     begin_cust-no AT ROW 3.14 COL 23.2 COLON-ALIGNED WIDGET-ID 16
     end_cust-no AT ROW 3.14 COL 65.2 COLON-ALIGNED WIDGET-ID 24
     begin_slmn AT ROW 4.24 COL 23.2 COLON-ALIGNED WIDGET-ID 20
     end_slmn AT ROW 4.24 COL 65.4 COLON-ALIGNED WIDGET-ID 22
     fi_temp-cust AT ROW 5.52 COL 7.8
     fi_temp-shipto AT ROW 6.48 COL 7.8 WIDGET-ID 2
     fi_temp-titles AT ROW 7.43 COL 7.8 WIDGET-ID 4
     tit-code AT ROW 7.43 COL 59.8 COLON-ALIGNED WIDGET-ID 8
     fi_add-book AT ROW 8.38 COL 7.8 WIDGET-ID 12
     list-name AT ROW 8.38 COL 59.8 COLON-ALIGNED WIDGET-ID 14
     fi_file AT ROW 10 COL 22 COLON-ALIGNED WIDGET-ID 10
     Btn_OK AT ROW 11.57 COL 6
     Btn_Cancel AT ROW 11.57 COL 24
     Btn_Help AT ROW 11.57 COL 60
     "Export All Customer Contacts from Advantzware Database?" VIEW-AS TEXT
          SIZE 74 BY .62 AT ROW 1.71 COL 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.4 BY 13.43.


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
         TITLE              = "Export Contacts"
         COLUMN             = 75.8
         ROW                = 7.33
         HEIGHT             = 13.52
         WIDTH              = 101
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 119.6
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
   FRAME-NAME                                                           */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_Help:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Export Contacts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Export Contacts */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
DO:
  def var char-val as cha no-undo.
  def var look-recid as recid no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
  lw-focus = FOCUS.                         
    case lw-focus:name :
         WHEN "begin_cust-no" THEN DO:
              RUN windows/l-custact.w (gcompany, begin_cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
               begin_cust-no:SCREEN-VALUE = cust.cust-no.
              END.
         END.  
         WHEN "end_cust-no" THEN DO:
              RUN windows/l-custact.w (gcompany, end_cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
               end_cust-no:SCREEN-VALUE = cust.cust-no.
              END.
         END.
         WHEN "begin_slmn" THEN DO:
            RUN windows/l-sman2.w (g_company, OUTPUT char-val).
            IF char-val NE "" THEN 
            ASSIGN begin_slmn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
         END.

         WHEN "end_slmn" THEN DO:
            RUN windows/l-sman2.w (g_company, OUTPUT char-val).
            IF char-val NE "" THEN 
            ASSIGN end_slmn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
         END.

    end case.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME DEFAULT-FRAME /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slmn C-Win
ON LEAVE OF begin_slmn IN FRAME DEFAULT-FRAME /* Beginning Sales Rep# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  def var i as int initial 0 no-undo.
  def var num-imports as int initial 0 no-undo.
  DEF VAR excelheader AS CHAR NO-UNDO.

  fill-in-1:hidden = no.
  SESSION:SET-WAIT-STATE("GENERAL").

  ASSIGN num-imports = 0
         fi_temp-shipto
         fi_temp-cust 
         fi_temp-titles .

   OUTPUT STREAM excel TO VALUE(fi_file).
     excelheader = "Company,Customer,Ship ID,Sales Rep,First Name,Last Name,Middle Name,Sirname,E-mail Address,Contact Title,Title Code," +
                     "Type,Cust Name,Business Address,Business Street,Business City,Business State,Business Postal Code,Business Country," +
                     "territory,Home Phone,Mobile Phone,Business Fax,Extension" .
          PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

      for each cust where cust.company eq gcompany
            AND cust.cust-no GE begin_cust-no
            AND cust.cust-no LE end_cust-no
            AND cust.sman    GE begin_slmn
            AND cust.sman     LE end_slmn  no-lock:
         assign fill-in-1:screen-value = cust.cust-no .
       IF fi_temp-cust THEN DO:
          FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK:
             IF phone.e_mail = "" THEN NEXT.
             IF tit-code:screen-value <> "" THEN DO:
                IF LOOKUP(phone.titlcode, tit-code:screen-value) = 0 THEN NEXT.
             END.
             num-imports = num-imports + 1.

             create ttcontact.
             assign ttcontact.company        = cust.company
                    ttcontact.cust-no        = cust.cust-no
                    ttcontact.ship-id        = ""
                    ttcontact.sman           = cust.sman
                    ttcontact.first-name     = cust.contact
                    ttcontact.last-name      = ""
                    ttcontact.middle-initial = ""
                    ttcontact.sirname        = ""
                    ttcontact.type           = ""
                    ttcontact.cust-name      = cust.NAME
                    ttcontact.addr1          = cust.addr[1]
                    ttcontact.addr2          = cust.addr[2]
                    ttcontact.city           = cust.city
                    ttcontact.state          = cust.state    
                    ttcontact.zip            = cust.zip      
                    ttcontact.country        = cust.country  
                    ttcontact.territory      = cust.terr
                    ttcontact.phone          = cust.phone
                    ttcontact.cell-phone     = ""
                    ttcontact.fax            = cust.fax
                    ttcontact.extension      = cust.area-code + cust.phone
                    ttcontact.email          = phone.e_mail
                    ttcontact.attention      = phone.attention
                    ttcontact.titlcode       = phone.titlcode   .


         if cust.contact ne "" then do i = 1 to length(trim(cust.contact)):
            if substring(cust.contact,i,1) ne " " then      next.
            else do:
              assign ttcontact.first-name = substring(cust.contact,1,i)
                     ttcontact.last-name  = substring(cust.contact,i + 1,length(trim(cust.contact))).        
              leave.
            end.
         end.
      END. /* end of phone*/
   END.  /* end of fi_temp-cust*/
         /* ===== create contact for shipto -====*/
       IF fi_temp-shipto THEN do:
         for each shipto of cust no-lock:
            FOR EACH phone WHERE phone.table_rec_key = shipto.rec_key NO-LOCK:
               IF phone.e_mail = "" THEN NEXT.
               IF tit-code:screen-value <> "" THEN DO:
                  IF LOOKUP(phone.titlcode, tit-code:screen-value) = 0 THEN NEXT.
               END.

               num-imports = num-imports + 1.

               create ttcontact.
               assign ttcontact.company       = shipto.company
                     ttcontact.cust-no        = shipto.cust-no
                     ttcontact.ship-id        = shipto.ship-id
                     ttcontact.sman           = cust.sman
                     ttcontact.first-name     = ""
                     ttcontact.last-name      = ""
                     ttcontact.middle-initial = ""
                     ttcontact.sirname        = ""
                     ttcontact.type           = cust.type
                     ttcontact.cust-name      = shipto.ship-name
                     ttcontact.addr1          = shipto.ship-addr[1]
                     ttcontact.addr2          = shipto.ship-addr[2]
                     ttcontact.city           = shipto.ship-city 
                     ttcontact.state          = shipto.ship-state
                     ttcontact.zip            = shipto.ship-zip  
                     ttcontact.country        = shipto.country   
                     ttcontact.territory      = cust.terr
                     ttcontact.phone          = shipto.phone
                     ttcontact.cell-phone     = ""
                     ttcontact.fax            = shipto.fax
                     ttcontact.extension      = cust.area-code + cust.phone
                     ttcontact.email          = phone.e_mail   
                     ttcontact.attention      = phone.attention
                     ttcontact.titlcode       = phone.titlcode 
                                                      .

                    if shipto.contact ne "" then do i = 1 to length(trim(shipto.contact)):
                       if substring(shipto.contact,i,1) ne " " then      next.
                       else do:
                            assign ttcontact.first-name = substring(shipto.contact,1,i)
                                   ttcontact.last-name  = substring(shipto.contact,i + 1,length(trim(shipto.contact))).        
                            leave.
                       end.
                    end.    
            END. /* for each phone*/
         end. /* for each shipto */
    END. /* end of fi_temp-shipto*/
  END. /* end of cust*/

      FOR EACH ttcontact NO-LOCK:
            PUT STREAM excel UNFORMATTED
             '"' ttcontact.company               '",'
             '"' ttcontact.cust-no               '",'
             '"' ttcontact.ship-id               '",'
             '"' ttcontact.sman                  '",'
             '"' ttcontact.first-name            '",'
             '"' ttcontact.last-name             '",'
             '"' ttcontact.middle-initial        '",'
             '"' ttcontact.sirname                '",'
             '"' ttcontact.email                 '",'
             '"' ttcontact.attention             '",'
             '"' ttcontact.titlcode              '",'
             '"' ttcontact.type                    '",'
             '"' ttcontact.cust-name               '",'
             '"' ttcontact.addr1                 '",'
             '"' ttcontact.addr2                 '",'
             '"' ttcontact.city                  '",'
             '"' ttcontact.state                 '",'
             '"' ttcontact.zip                   '",'
             '"' ttcontact.country               '",'
             '"' ttcontact.territory             '",'
             '"' ttcontact.phone                 '",'
             '"' ttcontact.cell-phone            '",'
             '"' ttcontact.fax                   '",'
             '"' ttcontact.extension             '",'
             SKIP.
            END.

  OUTPUT STREAM excel CLOSE.

  IF fi_add-book THEN DO:
    {BATCH\contexp.i}
  END.


  SESSION:SET-WAIT-STATE("").
  message num-imports " Contacts Downloaded." view-as alert-box.
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME DEFAULT-FRAME /* Ending Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slmn C-Win
ON LEAVE OF end_slmn IN FRAME DEFAULT-FRAME /* Ending Sales Rep# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_add-book
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_add-book C-Win
ON VALUE-CHANGED OF fi_add-book IN FRAME DEFAULT-FRAME /* Download to Address Book? */
DO:
  assign {&self-name}.
  IF fi_add-book THEN 
     list-name:HIDDEN IN FRAME DEFAULT-FRAME           = NO.
  ELSE
     list-name:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_temp-titles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_temp-titles C-Win
ON VALUE-CHANGED OF fi_temp-titles IN FRAME DEFAULT-FRAME /* Select specific Contact Titles? */
DO:
  assign {&self-name}.
  IF fi_temp-titles THEN 
     tit-code:HIDDEN IN FRAME DEFAULT-FRAME           = NO.
  ELSE
     tit-code:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME list-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL list-name C-Win
ON LEAVE OF list-name IN FRAME DEFAULT-FRAME /* List Name */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tit-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tit-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tit-code C-Win
ON LEAVE OF tit-code IN FRAME DEFAULT-FRAME /* Title Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tit-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

SESSION:SET-WAIT-STATE("").

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

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.

  {methods/nowait.i}

  ASSIGN
   tit-code:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
   list-name:HIDDEN IN FRAME DEFAULT-FRAME          = TRUE.
   APPLY "entry" TO begin_cust-no.
    {methods/setButton.i Btn_Cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Help "Help"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_OK "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_cust-no end_cust-no begin_slmn end_slmn fi_temp-cust 
          fi_temp-shipto fi_temp-titles tit-code fi_add-book list-name fi_file 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_slmn end_slmn fi_temp-cust 
         fi_temp-shipto fi_temp-titles tit-code fi_add-book list-name fi_file 
         Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tit-code C-Win 
PROCEDURE valid-tit-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF (tit-code:SCREEN-VALUE) <> "" AND
       NOT CAN-FIND(FIRST titlcode
                WHERE titlcode.titlcode  EQ (tit-code:SCREEN-VALUE))
    THEN DO:
      MESSAGE  string(tit-code:SCREEN-VALUE) + " is  invalid, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tit-code.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

