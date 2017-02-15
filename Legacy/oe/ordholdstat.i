&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : oe/ordholdstat.i
    Purpose     : List of order hold status types

    Syntax      : getOrdStatDescr(cOrdHoldStatCode)

    Description :

    Author(s)   : Stacey Brooks
    Created     : March, 2012
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


 DEFINE VARIABLE gcOrdStatList AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE gcOrdDescList AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE glStatTypeItemUpdate AS LOGICAL NO-UNDO INIT NO.
 DEFINE VARIABLE gc-char-val AS CHAR NO-UNDO INIT "".

/* AO = Active Order = Order is open, everything good to go                                                                                                                                     */
/* PA = Price Approval Needed= everything including shipping is allowed except for                                                                     invoicing, price must be approved first  */
/* OH = Order Hold = nothing can continue with the order due to any reason other than credit                                                                                                    */
/* CH = Credit Hold = nothing can continue with the order due to credit                                                                                                                         */
/* MH = Manufacturing Hold = order can be entered, purchasing can be done but cannot be converted                                                                                               */
/* SH = Shippiing Hold = order can be entered, purchasing can be done, product can be converted but not shipped                                                                                 */
/* FO = Finished Order & Closed = order is complete, shipped and invoiced                                                                                                                       */
/* CA = Cancelled  Order = order is cancelled                                                                                                                                                   */
/* NC = Non Compliance Issue Reported  = there is a NCR issue with the order                                                                                                                    */
/* LO = Late Order =   Information Only. Used by the expeditor to indicate that the customer has been called                                                                                    */
/*                     and informed that the order is late.                                                                                                                                     */


DEFINE BUFFER b-oe-ord FOR oe-ord.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdStatDescr Include 
FUNCTION getOrdStatDescr RETURNS CHARACTER
  ( INPUT pcOrdStatCode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD osGetNumItems Include 
FUNCTION osGetNumItems RETURNS INTEGER
  ( INPUT piCompany AS CHAR,
    INPUT piOrdNum AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 10.86
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */


 ASSIGN gcOrdStatList = "AO,PA,OH,CH,MH,SH,FO,CA,NC,LO"
        gcOrdDescList = "Active Order,Price Approval Needed,Order Hold,Credit Hold," +
                        "Manufacturing Hold,Shipping Hold,Finished Order & Closed," + 
                        "Canceled Order,Non-Compliance Issue Reported,Late Order".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE os-Get-Num-Items Include 
PROCEDURE os-Get-Num-Items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER piCompany LIKE oe-ord.company NO-UNDO.
   DEFINE INPUT PARAMETER piOrdNum  LIKE oe-ord.ord-no NO-UNDO.
   DEFINE OUTPUT PARAMETER piCount  AS INT NO-UNDO INIT 0.

   

   /* Count number of items on order. */
   FOR EACH b-oe-ordl NO-LOCK
      WHERE b-oe-ordl.company EQ piCompany
        AND b-oe-ordl.ord-no  EQ piOrdNum:

       ASSIGN piCount = piCount + 1.
   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE os-Process-Hold-Status Include 
PROCEDURE os-Process-Hold-Status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER piCompany LIKE oe-ord.company NO-UNDO.
   DEFINE INPUT PARAMETER piOrdNum  LIKE oe-ord.ord-no NO-UNDO.

   DEFINE VARIABLE vcHoldType AS CHAR NO-UNDO FORMAT "x(1)".
   DEFINE VARIABLE vcStatus   LIKE oe-ord.stat NO-UNDO.

   DEFINE VARIABLE vi AS INT NO-UNDO INIT 0.


   /* Find the order record. */
   FIND FIRST b-oe-ord NO-LOCK WHERE
              b-oe-ord.company EQ piCompany AND
              b-oe-ord.ord-no  EQ piOrdNum NO-ERROR.

   /* Message and abort if order not found. */
   IF NOT AVAILABLE b-oe-ord THEN DO:
       MESSAGE "Order " + string(piOrdNum) + " not found."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN.
   END.

  /* Count number of items on order. */ 
   RUN os-Get-Num-Items (INPUT b-oe-ord.company, INPUT b-oe-ord.ord-no, OUTPUT vi).

   /* If currently on hold, take off hold. */
   IF b-oe-ord.stat = "H" THEN DO:
       
       /* Prompt user to remove hold status. */
       MESSAGE "Take Order off Hold Status?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
       
       IF NOT lContinue THEN RETURN.

       ASSIGN vcStatus = "N"
              vcHoldType = "".

       /* Prompt to update items if more than one. */
       IF vi > 1 THEN
           RUN os-Prompt-Item-Update.
   END.

   ELSE
   /* If not on hold, place on hold. */
   IF b-oe-ord.stat <> "H" THEN DO:

       /* Prompt user to place hold status. */
       MESSAGE "Place Order on Hold?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lHold AS LOGICAL.
       
       IF NOT lHold THEN RETURN.

       /* Prompt the user to select order hold status type. */
       RUN os-Status-Type-Select (OUTPUT vcHoldType).

       /* If user selected a hold type, then change status to hold,
          else leave status at current status. */
       ASSIGN vcStatus = (IF vcHoldType <> "" THEN "H" ELSE b-oe-ord.stat).


       /* Prompt to update items if more than one. */
       IF vi > 1 THEN 
           RUN os-Prompt-Item-Update.
   END.

   /* Process the transaction to the database. */
   DO TRANSACTION ON ERROR UNDO, RETURN:

       /* Find the order record. */
       FIND FIRST b-oe-ord EXCLUSIVE-LOCK WHERE
                  b-oe-ord.company EQ piCompany AND
                  b-oe-ord.ord-no  EQ piOrdNum NO-ERROR NO-WAIT.

       /* Update the order record. */
       IF AVAILABLE b-oe-ord THEN
           ASSIGN b-oe-ord.stat = vcStatus
                  b-oe-ord.spare-char-2 = vcHoldType.
       IF AVAILABLE b-oe-ord AND vcStatus = "N" THEN
           b-oe-ord.approved-id   = USERID("nosweat").
            
       b-oe-ord.approved-date = TODAY.
       RUN oe/syncJobHold.p (INPUT b-oe-ord.company, INPUT b-oe-ord.ord-no, INPUT (IF b-oe-ord.stat EQ "H" THEN "Hold" ELSE "Release")).
       
       /* If updating all line items or if there is only one line item,
          then update the line items of order. */
       IF glStatTypeItemUpdate = YES OR vi = 1 THEN DO:

           /* Find the order lines of order. */
           FOR EACH b-oe-ordl EXCLUSIVE-LOCK
              WHERE b-oe-ordl.company EQ b-oe-ord.company
                AND b-oe-ordl.ord-no  EQ b-oe-ord.ord-no:
               
               /* update orderline hold type status. */
               ASSIGN b-oe-ordl.spare-char-1 = vcHoldType.
           END.
       END.
       FIND CURRENT b-oe-ord NO-LOCK.
       RELEASE b-oe-ord.
   END.

   /* reset item update flag. */
   ASSIGN glStatTypeItemUpdate = NO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE os-Prompt-Item-Update Include 
PROCEDURE os-Prompt-Item-Update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEFINE OUTPUT PARAMETER plItemUpdate AS LOGICAL NO-UNDO INIT NO.  */

 
 MESSAGE "Update ALL items for Order?"
     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE glStatTypeItemUpdate.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE os-Status-Type-Select Include 
PROCEDURE os-Status-Type-Select :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcHoldType AS CHAR NO-UNDO.

       /* Prompt the user to select order hold status type. */
       RUN windows/l-holdtype.w (OUTPUT gc-char-val).
       IF gc-char-val <> "" THEN
           ASSIGN pcHoldType = ENTRY(1,gc-char-val).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE os-Update-Line-Items Include 
PROCEDURE os-Update-Line-Items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER piCompany LIKE oe-ord.company NO-UNDO.
   DEFINE INPUT PARAMETER piOrdNum  LIKE oe-ord.ord-no NO-UNDO.
   DEFINE INPUT PARAMETER pcHoldType AS CHAR NO-UNDO.


       /* If updating all line items or if there is only one line item,
          then update the line items of order. */
       DO TRANSACTION ON ERROR UNDO, RETURN:

           /* Find the order lines of order. */
           FOR EACH b-oe-ordl EXCLUSIVE-LOCK
              WHERE b-oe-ordl.company EQ piCompany
                AND b-oe-ordl.ord-no  EQ piOrdNum:
               
               /* update orderline hold type status. */
               ASSIGN b-oe-ordl.spare-char-1 = pcHoldType.
           END.

           /* Reset status update flag. */
           ASSIGN glStatTypeItemUpdate = NO.
       END.

       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdStatDescr Include 
FUNCTION getOrdStatDescr RETURNS CHARACTER
  ( INPUT pcOrdStatCode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF pcOrdStatCode = "" THEN RETURN "".

  IF LOOKUP(pcOrdStatCode,gcOrdStatList) = 0 THEN RETURN "".


      
  RETURN ENTRY(LOOKUP(pcOrdStatCode,gcOrdStatList),gcOrdDescList). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION osGetNumItems Include 
FUNCTION osGetNumItems RETURNS INTEGER
  ( INPUT piCompany AS CHAR,
    INPUT piOrdNum AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE viCount  AS INT NO-UNDO INIT 0.

   

   /* Count number of items on order. */
   FOR EACH b-oe-ordl NO-LOCK
      WHERE b-oe-ordl.company EQ piCompany
        AND b-oe-ordl.ord-no  EQ piOrdNum:

       ASSIGN viCount = viCount + 1.
   END.

  RETURN viCount.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

