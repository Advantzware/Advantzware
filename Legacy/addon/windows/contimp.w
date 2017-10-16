&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_temp-shipto Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi_temp-shipto 

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

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Importing Contact for Customer" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_temp-shipto AS LOGICAL INITIAL no 
     LABEL "Include Temp Shiptos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 1.48 COL 32 COLON-ALIGNED
     fi_temp-shipto AT ROW 2.91 COL 3.8
     Btn_OK AT ROW 4.33 COL 6
     Btn_Cancel AT ROW 4.33 COL 24
     Btn_Help AT ROW 4.33 COL 60
     "Import  All Customer Contacts from Advantzware Database?" VIEW-AS TEXT
          SIZE 74 BY .62 AT ROW 1.71 COL 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 5.76.


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
         TITLE              = "Import Customer Contacts"
         COLUMN             = 29
         ROW                = 9.38
         HEIGHT             = 5.05
         WIDTH              = 82
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
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
ON END-ERROR OF C-Win /* Import Customer Contacts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import Customer Contacts */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  def var i as int initial 0 no-undo.
  def var num-imports as int initial 0 no-undo.
  def var upd-contact as int no-undo.
  def buffer bf-cont for contact .

  fill-in-1:hidden = no.
  SESSION:SET-WAIT-STATE("GENERAL").

  ASSIGN upd-contact = 0
         fi_temp-shipto.

  for each cust where cust.company eq gcompany no-lock:
      assign fill-in-1:screen-value = cust.cust-no
            /* num-imports = num-imports + 1 */.

      find first contact where contact.company eq gcompany      
                         and contact.cust-no eq cust.cust-no
                         no-lock no-error.
      if not avail contact then
      do:
         num-imports = num-imports + 1.

         create contact.
         assign contact.company        = cust.company
             contact.cust-no        = cust.cust-no
             contact.ship-id        = ""
             contact.sman           = cust.sman
             contact.first-name     = cust.contact
             contact.last-name      = ""
             contact.middle-initial = ""
             contact.sirname        = ""
             contact.contact-title  = ""
             contact.maillist       = No
             contact.type           = cust.type
             contact.contact-loc    = "C"
             contact.cust-name      = cust.name
             contact.addr1          = cust.addr[1]
             contact.addr2          = cust.addr[2]
             contact.city           = cust.city
             contact.state          = cust.state
             contact.zip            = cust.zip
             contact.country        = cust.country
             contact.county         = ""
             contact.territory      = cust.terr
             contact.access-code    = ""
             contact.phone          = cust.area-code + cust.phone
             contact.cell-phone     = ""
             contact.fax            = cust.fax
             contact.extension      = ""
             contact.email          = ""
             contact.website        = "".

         if cust.contact ne "" then do i = 1 to length(trim(cust.contact)):
            if substring(cust.contact,i,1) ne " " then      next.
            else do:
              assign contact.first-name = substring(cust.contact,1,i)
                     contact.last-name  = substring(cust.contact,i + 1,length(trim(cust.contact))).        
              leave.
            end.
         end.
         /* ===== create contact for shipto -====*/
         for each shipto of cust no-lock:
             if shipto.ship-id = cust.cust-no and
                shipto.ship-addr[1] = cust.addr[1] and
                shipto.ship-addr[2] = cust.addr[2] and
                shipto.ship-city = cust.city and
                shipto.ship-state = cust.state and
                shipto.ship-zip = cust.zip 
             then next.

             num-imports = num-imports + 1.

             create contact.
             assign contact.company        = cust.company
                    contact.cust-no        = cust.cust-no
                    contact.ship-id        = shipto.ship-id
                    contact.sman           = cust.sman
                    contact.first-name     = ""
                    contact.last-name      = ""
                    contact.middle-initial = ""
                    contact.sirname        = ""
                    contact.contact-title  = ""
                    contact.maillist       = No
                    contact.type           = cust.type
                    contact.contact-loc    = "S"
                    contact.cust-name      = shipto.ship-name
                    contact.addr1          = shipto.ship-addr[1]
                    contact.addr2          = shipto.ship-addr[2]
                    contact.city           = shipto.ship-city
                    contact.state          = shipto.ship-state
                    contact.zip            = shipto.ship-zip
                    contact.country        = shipto.country
                    contact.county         = ""
                    contact.territory      = shipto.dest-code
                    contact.access-code    = ""
                    contact.phone          = cust.area-code + cust.phone
                    contact.cell-phone     = ""
                    contact.fax            = cust.fax
                    contact.extension      = ""
                    contact.email          = ""
                    contact.website        = "".
    /*========== no default 
                    if cust.contact ne "" then do i = 1 to length(trim(cust.contact)):
                       if substring(cust.contact,i,1) ne " " then      next.
                       else do:
                            assign contact.first-name = substring(cust.contact,1,i)
                                   contact.last-name  = substring(cust.contact,i + 1,length(trim(cust.contact))).        
                            leave.
                       end.
                    end.    
                =================*/     
         end. /* for each shipto */

      end. /* not avail contact */  
      else do:   /* can be customer or shipto */
           for each contact where contact.company eq gcompany
                              and contact.cust-no eq cust.cust-no
                              and contact.ship-id = "" :
               find first bf-cont where bf-cont.company = gcompany and
                                        bf-cont.cust-no = cust.cust-no and
                                        bf-cont.ship-id = "" and
                                        recid(bf-cont) <> recid(contact)
                                        no-lock no-error.
               if avail bf-cont then next.                                        

               upd-contact = upd-contact + 1.
               assign contact.addr1 = cust.addr[1]
                      contact.addr2 = cust.addr[2]
                      contact.city  = cust.city
                      contact.state = cust.state
                      contact.zip   = cust.zip
                      contact.country = cust.country
                      contact.phone = cust.area-code + cust.phone
                      contact.fax   = cust.fax
                      contact.sman  = cust.sman
                      contact.cust-name = cust.name
                      contact.type = cust.type
                      .
               if cust.contact ne "" then do i = 1 to length(trim(cust.contact)):
                  if substring(cust.contact,i,1) ne " " then      next.
                  else do:
                      assign contact.first-name = substring(cust.contact,1,i)
                             contact.last-name  = substring(cust.contact,i + 1,length(trim(cust.contact))).        
                      leave.
                 end.
               end.                      
           end. 
           for each shipto of cust no-lock:
               for each contact where contact.company eq gcompany
                                  and contact.cust-no eq cust.cust-no
                                  and contact.ship-id = shipto.ship-id :
                   upd-contact = upd-contact + 1.
                   assign contact.addr1 = shipto.ship-addr[1]
                          contact.addr2 = shipto.ship-addr[2]
                          contact.city  = shipto.ship-city
                          contact.state = shipto.ship-state
                          contact.zip   = shipto.ship-zip
                          contact.country = shipto.country
                          contact.phone = cust.area-code + cust.phone
                          contact.fax   = cust.fax
                          contact.sman  = cust.sman
                          contact.cust-name = ship.ship-name
                          contact.type = cust.type.
                          .
               end. 
           end.
      end.

  end.

  IF fi_temp-shipto THEN
  DO:
      FIND FIRST cust WHERE
           cust.company EQ gcompany AND
           cust.cust-no EQ "TEMP"
           NO-LOCK NO-ERROR.

      IF AVAIL cust THEN
      DO:
         FOR EACH shipto OF cust NO-LOCK:
             if shipto.ship-id = cust.cust-no and
                shipto.ship-addr[1] = cust.addr[1] and
                shipto.ship-addr[2] = cust.addr[2] and
                shipto.ship-city = cust.city and
                shipto.ship-state = cust.state and
                shipto.ship-zip = cust.zip 
             then next.

             FIND FIRST contact WHERE
                  contact.company EQ cust.company AND
                  contact.cust-no EQ cust.cust-no AND
                  contact.ship-id EQ shipto.ship-id
                  EXCLUSIVE-LOCK NO-ERROR.

             IF NOT AVAIL contact THEN
             DO:
                create contact.
                assign contact.company        = cust.company
                       contact.cust-no        = cust.cust-no
                       contact.ship-id        = shipto.ship-id
                       num-imports            = num-imports + 1.
             END.
             ELSE
                upd-contact = upd-contact + 1.

             ASSIGN contact.sman           = cust.sman
                    contact.maillist       = No
                    contact.type           = cust.type
                    contact.contact-loc    = "S"
                    contact.cust-name      = shipto.ship-name
                    contact.addr1          = shipto.ship-addr[1]
                    contact.addr2          = shipto.ship-addr[2]
                    contact.city           = shipto.ship-city
                    contact.state          = shipto.ship-state
                    contact.zip            = shipto.ship-zip
                    contact.country        = shipto.country
                    contact.territory      = shipto.dest-code
                    contact.phone          = cust.area-code + cust.phone
                    contact.fax            = cust.fax.
         END.

         RELEASE cust.
      END.
  END.

  SESSION:SET-WAIT-STATE("").
  message num-imports " Contacts Added." skip
          upd-contact " Contacts Updated" view-as alert-box.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}

SESSION:SET-WAIT-STATE("").

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
  RUN enable_UI.
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
  DISPLAY fi_temp-shipto 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi_temp-shipto Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

