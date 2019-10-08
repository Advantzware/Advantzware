&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/CheckAckPrint.p     
    Purpose     : Prompt for Creation of Acknowledgement

    Syntax      :

    Description :  If Ackhead int val = 1, user will be prompted to create ack.
                   If they say yes, the order Acknowledgment print parameter
                   screen will launch.

    Author(s)   :  BV 
    Created     :  04/04/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
DEFINE INPUT PARAMETER ipriOrder AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CheckAckheadNK1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckAckheadNK1 Procedure 
FUNCTION CheckAckheadNK1 RETURNS LOGICAL
  ( ipcCompany AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PromptForAck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PromptForAck Procedure 
FUNCTION PromptForAck RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE BUFFER bf-oe-ord FOR oe-ord.

FIND FIRST bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ipriOrder NO-LOCK NO-ERROR.
IF NOT AVAIL bf-oe-ord THEN RETURN.

IF CheckAckheadNK1(INPUT bf-oe-ord.company) THEN
    IF PromptForAck() THEN
        RUN PrintAck(BUFFER bf-oe-ord).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PrintAck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintAck Procedure 
PROCEDURE PrintAck :
/*------------------------------------------------------------------------------
  Purpose: Sets order context     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.

IF NOT AVAILABLE ipbf-oe-ord THEN RETURN.

RUN custom/setUserPrint.p (INPUT ipbf-oe-ord.company,
                           INPUT 'r-acknow.',
                           INPUT 'begin_ord-no,end_ord-no,begin_cust-no,end_cust-no,begin_due-date,end_due-date,tb_reprint',
                           INPUT STRING(ipbf-oe-ord.ord-no) + ',' 
                                + STRING(ipbf-oe-ord.ord-no) + ',' 
                                + ipbf-oe-ord.cust-no + ',' 
                                + ipbf-oe-ord.cust-no + ',' 
                                + STRING(ipbf-oe-ord.ord-date) + ',' 
                                + STRING(ipbf-oe-ord.ord-date) + ',' 
                                + STRING(ipbf-oe-ord.ack-prnt)).

RUN oerep/r-acknow.p.
/* RUN Get_Procedure IN Persistent-Handle ('r-acknow.',OUTPUT RUN-PROC, YES). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CheckAckheadNK1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckAckheadNK1 Procedure 
FUNCTION CheckAckheadNK1 RETURNS LOGICAL
  ( ipcCompany AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Test NK1 Ackhead integer value.  Return True if 1 False otherwise.
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (INPUT ipcCompany, 
                       INPUT "ACKHEAD", 
                       INPUT "I", 
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "", 
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).

IF lFound AND INT(cReturn) = 1 THEN
    RETURN TRUE.
ELSE
    RETURN FALSE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PromptForAck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PromptForAck Procedure 
FUNCTION PromptForAck RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE lContinue AS LOGICAL     NO-UNDO.
    
MESSAGE "Would you like to send an Order Acknowledgement?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lContinue.

RETURN lContinue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

