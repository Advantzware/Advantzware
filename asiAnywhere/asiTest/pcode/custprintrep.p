
/*------------------------------------------------------------------------
    File        : custprintrep.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{sys/inc/VAR.i NEW SHARED}
DEFINE TEMP-TABLE ttcustprintreport NO-UNDO
    FIELD custprint       AS CHAR
    FIELD reprint        AS CHAR

    .

DEFINE DATASET dscustprintreport FOR ttcustprintreport.
    

DEFINE INPUT PARAMETER prmAction     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegCust    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBegName    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmEndCust    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmEndName    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmShiptoAdd  AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmSoldtoAdd  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmTotal      AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmListOrder  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmShowNote   AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmShowSelPar AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmMisc       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmShowAdd    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmShowPhone  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey     AS CHAR  NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustprintreport .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction         = ?  THEN ASSIGN prmAction       = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp         = "".
IF prmUser           = ?  THEN ASSIGN prmUser         = "".
IF prmBegCust        = ?  THEN ASSIGN prmBegCust      = "".
IF prmBegName        = ?  THEN ASSIGN prmBegName      = "".
IF prmEndCust        = ?  THEN ASSIGN prmEndCust      = "". 
IF prmEndName        = ?  THEN ASSIGN prmEndName      = "". 
IF prmShiptoAdd      = ?  THEN ASSIGN prmShiptoAdd    = "".
IF prmSoldtoAdd      = ?  THEN ASSIGN prmSoldtoAdd    = "".
IF prmTotal          = ?  THEN ASSIGN prmTotal        = "".
IF prmListOrder      = ?  THEN ASSIGN prmListOrder    = "".
IF prmShowNote       = ?  THEN ASSIGN prmShowNote     = "".
IF prmShowSelPar     = ?  THEN ASSIGN prmShowSelPar   = "".
IF prmMisc           = ?  THEN ASSIGN prmMisc         = "".
IF prmShowAdd        = ?  THEN ASSIGN prmShowAdd      = "".
IF prmShowPhone      = ?  THEN ASSIGN prmShowPhone    = "".
IF prmReckey         = ?  THEN ASSIGN prmReckey       = "".



DEF VAR tmp-dir AS cha NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR gcompany AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser
    gcompany  = cocode  .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode 
           v-today   = TODAY .


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

&Scoped-define PROGNAME cust_.
&Scoped-define LISTORDER Cust. #,Customer Name
&Scoped-define WHERE-STATEMENT cust.company = gcompany
&Scoped-define SHOWNOTES yes
&Scoped-define SHOWMISCFLDS yes
&Scoped-define SHOWADDRESSES no
&Scoped-define SHOWPHONES no

DEFINE QUERY cust-query FOR cust.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Beginning  Customer" NO-UNDO.
DEFINE VARIABLE begin_cust_name AS CHARACTER FORMAT "X(30)" LABEL "Beginning Customer Name" NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" LABEL "Ending Customer" NO-UNDO.
DEFINE VARIABLE end_cust_name AS CHARACTER FORMAT "X(30)" LABEL "Ending Customer Name" NO-UNDO.
DEFINE VARIABLE show-shipto AS LOGICAL FORMAT "yes/no" LABEL "Show Ship To Addresses" NO-UNDO.
DEFINE VARIABLE show-soldto AS LOGICAL FORMAT "yes/no" LABEL "Show Sold To Addresses" NO-UNDO.
DEFINE VARIABLE show-totals AS LOGICAL FORMAT "yes/no" LABEL "Show Totals" NO-UNDO.

/*{methods/lstlogic/lstlogic.i}*/

DEFINE BUFFER bprgrms FOR prgrms.

DEFINE VARIABLE output-option AS INTEGER NO-UNDO.
DEFINE VARIABLE spooled AS LOGICAL NO-UNDO.
DEFINE VARIABLE spool-date AS DATE NO-UNDO.
DEFINE VARIABLE spool-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE spool-ampm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
DEFINE VARIABLE orientation AS CHARACTER NO-UNDO.
DEFINE VARIABLE show-notes AS LOGICAL LABEL "Show Notes" NO-UNDO.
DEFINE VARIABLE notes-type AS INTEGER NO-UNDO.
DEFINE VARIABLE show-parameters AS LOGICAL NO-UNDO.
DEFINE VARIABLE show-misc-fields AS LOGICAL LABEL "Show Misc Fields" NO-UNDO.
DEFINE VARIABLE show-addresses AS LOGICAL LABEL "Show Addresses" NO-UNDO.
DEFINE VARIABLE show-phones AS LOGICAL LABEL "Show Phones" NO-UNDO.
DEFINE VARIABLE list-order AS INTEGER NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE misc-label AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE x-pos AS INTEGER NO-UNDO.
DEFINE VARIABLE y-pos AS INTEGER NO-UNDO.




IF prmAction = "custprint" THEN DO:
    
    ASSIGN
        begin_cust-no     = prmBegCust   
        begin_cust_name   = prmBegName   
        end_cust-no       = prmEndCust   
        end_cust_name     = prmEndName   
        show-shipto       = IF prmShiptoAdd = "True" THEN TRUE ELSE FALSE
        show-soldto       = IF prmSoldtoAdd = "True" THEN TRUE ELSE FALSE
        show-totals       = IF prmTotal  = "True" THEN TRUE ELSE FALSE   
        list-order        = IF prmListOrder = "1" THEN 1 ELSE 2
        show-notes        = IF prmShowNote = "True" THEN TRUE ELSE FALSE
        show-parameters   = IF prmShowSelPar = "True" THEN TRUE ELSE FALSE
        show-misc-fields  = IF prmMisc = "True" THEN TRUE ELSE FALSE     
        show-addresses    = IF prmShowAdd  = "True" THEN TRUE ELSE FALSE       
        show-phones       = IF prmShowPhone = "True" THEN TRUE ELSE FALSE 
        notes-type        =  int(prmReckey) .
        

        lines-per-page = 55 .


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "Invbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "Invbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        RUN run-report.


        CREATE ttcustprintreport.
        ASSIGN ttcustprintreport.custprint = vTextFile .

END.


PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    FIND bprgrms WHERE bprgrms.prgmname = "{&PROGNAME}" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bprgrms THEN
    RETURN. 

{methods/lstlogic/header.i}
{methods/lstlogic/footer.i}

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

    OUTPUT TO VALUE(list-name) PAGE-SIZE VALUE(lines-per-page) PAGED.

    DO:
        VIEW FRAME f-header.
        VIEW FRAME f-continued.
    END.

    IF show-parameters THEN
        DO:
        RUN Show-Parameters.
        END.
    
    RUN List-Logic.


        DO:
            HIDE FRAME f-continued NO-PAUSE.
            VIEW FRAME f-end-footer.
            OUTPUT CLOSE.
       END.

   
    
 END PROCEDURE.   /*end run report*/




 PROCEDURE List-Logic:
    
  
   CASE list-order:
    WHEN 1 THEN
    OPEN QUERY cust-query FOR EACH cust NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        cust.cust-no GE begin_cust-no AND
        cust.cust-no LE end_cust-no.
    WHEN 2 THEN
    OPEN QUERY cust-query FOR EACH cust NO-LOCK WHERE
        &IF "{&WHERE-STATEMENT}" NE "" &THEN
        {&WHERE-STATEMENT} AND
        &ENDIF
        cust.name GE begin_cust_name AND
        cust.name LE end_cust_name.
  END CASE.

  GET FIRST cust-query.
  DO WHILE AVAILABLE(cust)
      WITH FRAME {&FRAME-NAME} NO-BOX WIDTH 132 STREAM-IO DOWN:
    {methods/lstlogic/custom/cust_.i}
    DOWN.
    GET NEXT cust-query.
  END.
END PROCEDURE. /* List-Logic */


 PROCEDURE Show-Parameters:
  DEFINE VARIABLE notetype-descrip AS CHARACTER FORMAT "X(20)" NO-UNDO.

  ASSIGN
    notetype-descrip = IF notes-type = 1 THEN "(All)"
                  ELSE IF notes-type = 2 THEN "(Viewed Only)"
                  ELSE "(Not Viewed Only)"
    y-pos = 500.

  DO:
    DISPLAY SKIP(3)
      "SELECTED PARAMETERS:" TO 40 SKIP(1)
      ENTRY(list-order,"{&LISTORDER}") FORMAT "X(40)"
          LABEL "List Order" COLON 40 SKIP(1)
        WITH FRAME f1-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    RUN Show-Selections.
    DISPLAY SKIP(1)
&IF "{&SHOWNOTES}" = "yes" &THEN
      show-notes COLON 40
      notetype-descrip WHEN show-notes NO-LABEL SKIP
&ENDIF
&IF "{&SHOWMISCFLDS}" = "yes" &THEN
      show-misc-fields COLON 40 SKIP
&ENDIF
&IF "{&SHOWADDRESSES}" = "yes" &THEN
      show-addresses COLON 40 SKIP
&ENDIF
&IF "{&SHOWPHONES}" = "yes" &THEN
      show-phones COLON 40 SKIP
&ENDIF
        WITH FRAME f2-parameters SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    PAGE.
  END.
END PROCEDURE.




PROCEDURE Show-Selections:
  DISPLAY
    begin_cust-no COLON 40
    begin_cust_name COLON 40
    end_cust-no COLON 40
    end_cust_name COLON 40
    show-shipto COLON 40
    show-soldto COLON 40
    show-totals COLON 40
        WITH FRAME f-selections SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
END PROCEDURE.

