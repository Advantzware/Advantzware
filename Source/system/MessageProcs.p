/*------------------------------------------------------------------------
    File        : MessageProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : Thur Oct 24 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Property Variables*/
            
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

 

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetMessageProcs:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcMessageID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCurrentTitle AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCurrentMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuppressMessage AS LOGICAL NO-UNDO.
    
    FIND FIRST zMessage NO-LOCK
        WHERE zMessage.msgID EQ ipcMessageID       
        NO-ERROR.

    IF AVAIL zMessage then
        ASSIGN
        oplSuppressMessage = zMessage.userSuppress 
        opcCurrentTitle    = zMessage.currentTitle
        opcCurrentMessage  = zMessage.currMessage .

    IF opcCurrentMessage EQ "" AND AVAIL zMessage THEN
        ASSIGN
           opcCurrentTitle    = zMessage.defaultTitle
           opcCurrentMessage  = zMessage.defaultMsg .

    IF AVAIL zMessage AND NOT zMessage.displayOptions THEN
        oplSuppressMessage = YES .

    
END PROCEDURE.

/* ************************  Function Implementations ***************** */



