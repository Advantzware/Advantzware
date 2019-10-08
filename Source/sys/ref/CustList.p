&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS PROCEDURE 
/*------------------------------------------------------------------------
    File        : sys/ref/CustList.p
    Purpose     :  Builds a Customer List based on the values in an NK1
    Used initially for Statement Run but could have more applications

    Syntax      : run sys/ref/CustList.p (cocode, "NK1")

    Description : Mode should be able to prompt for a customer list or build 
    from NK1 view Form list

    Author(s)   : BV
    Created     : 09/15/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcMode AS CHARACTER NO-UNDO.
/*Can be NK1 or "Prompt" - which will present UI for seletion list building */
DEFINE INPUT PARAMETER iplBuild AS LOGICAL NO-UNDO.
/*YES will build temp-table, NO will only return whether Customer List is active f
for main NK1 defined in MODE.  Not applicable with "Prompt"*/
DEFINE OUTPUT PARAMETER oplActive AS LOGICAL NO-UNDO.

{sys/ref/CustList.i}  /*shared temp table ttCustList*/

DEFINE VARIABLE gcNK1Master AS CHARACTER INITIAL "CustomerList"  NO-UNDO.
DEFINE VARIABLE gcMsgReport AS CHARACTER INITIAL "NK1 CustList - Report Specific List"   NO-UNDO.
DEFINE VARIABLE gcMsgGlobal AS CHARACTER INITIAL "NK1 CustList - Global List"  NO-UNDO.
DEFINE VARIABLE gcMsgUser AS CHARACTER INITIAL "User Specific List"  NO-UNDO.
DEFINE VARIABLE gcMsgdisplay AS CHARACTER INITIAL "JQ1,JU1,OU1,OQ1,EQ,EC,EF,IF1,AF1,IL3,JC,OT1,AQ1,AQ2,IQ1,IQ2,OB3"   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE PROCEDURE
&SCOPED-DEFINE DB-AWARE NO



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetCustListActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCustListActive PROCEDURE 
FUNCTION GetCustListActive RETURNS LOGICAL
  ( ipcCompany AS CHARACTER, ipcNK1 AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.76
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK PROCEDURE 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iListCount AS INTEGER     NO-UNDO.

IF ipcMode NE "PROMPT" THEN DO:
    oplActive = GetCustListActive(INPUT ipcCompany,
                                  INPUT gcNK1Master).
    IF oplActive AND iplBuild THEN DO:
        EMPTY TEMP-TABLE ttCustList.
        /*Build list specific to report*/
        iListCount = 0.
        RUN BuildCustList(INPUT ipcCompany,
                          INPUT gcNK1Master,
                          INPUT ipcMode,
                          INPUT-OUTPUT iListCount).
        
        /*Build list for all reports*/
        RUN BuildCustList(INPUT ipcCompany,
                          INPUT gcNK1Master,
                          INPUT "",
                          INPUT-OUTPUT iListCount).
        IF LOOKUP(ipcMode,gcMsgdisplay) = 0 THEN
        IF iListCount EQ 0 THEN
            MESSAGE "No Customer List Specified for " ipcMode
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
ELSE DO:
    /*prompt for customer list selection*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList PROCEDURE 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:    Builds the customer list temp-table for given NK1 
  Parameters:  company and nk1
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNK1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCharFld AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiCount AS INTEGER NO-UNDO.

DEFINE BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto.

DEFINE VARIABLE cCustNo AS CHARACTER   NO-UNDO.

FOR EACH bf-sys-ctrl-shipto
    WHERE bf-sys-ctrl-shipto.company EQ ipcCompany
      AND bf-sys-ctrl-shipto.NAME EQ ipcNK1
      AND bf-sys-ctrl-shipto.char-fld EQ ipcCharFld
      AND bf-sys-ctrl-shipto.log-fld
    NO-LOCK
    BY bf-sys-ctrl-shipto.cust-vend-no:
    
    cCustNo = bf-sys-ctrl-shipto.cust-vend-no.
    IF cCustNo NE "" THEN DO:
        FIND FIRST ttCustList
            WHERE ttCustList.cust-no EQ cCustNo
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL ttCustList THEN DO:
            iopiCount = iopiCount + 1.
            CREATE ttCustList.
        END.
        ASSIGN
            ttCustList.cust-no = cCustNo
            ttCustList.log-fld = bf-sys-ctrl-shipto.log-fld
            ttCustList.char-fld = bf-sys-ctrl-shipto.char-fld
            ttCustList.dec-fld = bf-sys-ctrl-shipto.dec-fld
            ttCustList.int-fld = bf-sys-ctrl-shipto.int-fld
            ttCustList.date-fld = bf-sys-ctrl-shipto.date-fld
            custcount  = custcount + "," + cCustNo
            .
        IF ipcCharFld EQ "" THEN
            ttCustList.cSource = gcMsgGlobal.
        ELSE
            ttCustList.cSource = gcMsgReport.
    END.
    ELSE DO:
        IF bf-sys-ctrl-shipto.log-fld THEN
            RUN BuildUserCustList(INPUT ipcCompany,
                                  INPUT-OUTPUT iopiCount). 
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildUserCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildUserCustList PROCEDURE 
PROCEDURE BuildUserCustList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiCount AS INTEGER NO-UNDO.

DEFINE BUFFER bf-usercust FOR usercust.

FOR EACH bf-usercust 
    WHERE bf-usercust.company EQ ipcCompany 
      AND bf-usercust.user_id EQ USERID("nosweat") 
    NO-LOCK,
    FIRST cust WHERE cust.company EQ ipcCompany 
     AND cust.cust-no = bf-usercust.cust-no NO-LOCK :
    FIND FIRST ttCustList
        WHERE ttCustList.cust-no EQ bf-usercust.cust-no
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL ttCustList THEN DO:
        iopiCount = iopiCount + 1.
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-usercust.cust-no      
            .
            custcount = custcount +  "," + bf-usercust.cust-no .
    END.
    ASSIGN 
        ttCustList.log-fld = YES
        ttCustList.cSource = gcMsgUser.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetCustListActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCustListActive Procedure 
FUNCTION GetCustListActive RETURNS LOGICAL
  ( ipcCompany AS CHARACTER, ipcNK1 AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the logical of an NK1
    Notes:  Will also build NK1 if it doesn't exist
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCompany, 
                           INPUT ipcNK1,
                           INPUT "L",
                           INPUT NO, 
                           INPUT NO, 
                           INPUT "",
                           INPUT "", 
                           OUTPUT cReturn, 
                           OUTPUT lFound).
    IF lFound THEN
        lActive = cReturn EQ "YES".

    RETURN lActive.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

