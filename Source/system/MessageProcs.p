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
    
    FIND FIRST zMessage NO-LOCK
        WHERE zMessage.msgID EQ ipcMessageID       
        NO-ERROR.

    IF AVAIL zMessage then
        ASSIGN
        opcCurrentTitle    = zMessage.currentTitle 
        opcCurrentMessage  = zMessage.currMessage .

    IF opcCurrentMessage EQ "" AND AVAIL zMessage THEN
        ASSIGN
           opcCurrentTitle    = zMessage.defaultTitle 
           opcCurrentMessage  = zMessage.defaultMsg .
   
    
END PROCEDURE.

PROCEDURE pGetMessageFlag:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcMessageID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuppressMessage AS LOGICAL NO-UNDO.
    
    FIND FIRST zMessage NO-LOCK
        WHERE zMessage.msgID EQ ipcMessageID       
        NO-ERROR.

    IF AVAIL zMessage then
        ASSIGN
        oplSuppressMessage = zMessage.userSuppress .
    
    IF AVAIL zMessage AND NOT zMessage.displayOptions THEN
        oplSuppressMessage = YES .

    
END PROCEDURE.

PROCEDURE pDisplayMessageGetOutput:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcMessageID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCurrentTitle    AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCurrentMessage  AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lSuppressMessage AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cParmList        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValues    AS CHARACTER NO-UNDO.

    RUN pGetMessageProcs(ipcMessageID , OUTPUT cCurrentTitle, OUTPUT cCurrentMessage ) . 

    RUN pGetMessageFlag(ipcMessageID , OUTPUT lSuppressMessage ) .

    IF NOT lSuppressMessage  THEN DO:
        
        cCurrentMessage = REPLACE(cCurrentMessage, ',', ' ').
        cCurrentMessage = REPLACE(cCurrentMessage, '=', ' ').

        IF length(cCurrentMessage) LE 193 THEN do:
          cParmList =
            "type=literal,name=fi1,row=3,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,1,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi2,row=4,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,75,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi3,row=5,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,149,45) + ",FORMAT=X(50)"
            + "|type=literal,name=fi4,row=5,col=72,enable=false,width=15,scrval=Message Id: " + ipcMessageID + " ,FORMAT=X(15)"
            + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
            + "|type=win,name=fi5,enable=true,label=" + cCurrentTitle + ",FORMAT=X(30),height=9".
        END.
        ELSE DO:
            cParmList =
            "type=literal,name=fi1,row=2,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,1,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi2,row=3,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,75,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi3,row=4,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,149,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi4,row=5,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,223,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi5,row=6,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,297,45) + ",FORMAT=X(50)"
            + "|type=literal,name=fi6,row=6,col=72,enable=false,width=15,scrval=Message Id: " + ipcMessageID + " ,FORMAT=X(15)"
            + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
            + "|type=win,name=fi7,enable=true,label=" + cCurrentTitle + ",FORMAT=X(30),height=10".
        END.
        
        RUN custom/d-prompt.w (INPUT "ok", cParmList, "", OUTPUT cReturnValues).

    END.


END PROCEDURE.



PROCEDURE pDisplayMessageGetYesNo:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcMessageID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValue AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cCurrentTitle    AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCurrentMessage  AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lSuppressMessage AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cParmList        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValues    AS CHARACTER NO-UNDO.
    
    RUN pGetMessageProcs(ipcMessageID , OUTPUT cCurrentTitle, OUTPUT cCurrentMessage ) . 
    
     RUN pGetMessageFlag(ipcMessageID , OUTPUT lSuppressMessage ) .

    IF NOT lSuppressMessage  THEN DO:
        cCurrentMessage = REPLACE(cCurrentMessage, ',', ' ').
        cCurrentMessage = REPLACE(cCurrentMessage, '=', ' ').

        IF length(cCurrentMessage) LE 193 THEN do:
          cParmList =
            "type=literal,name=fi1,row=3,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,1,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi2,row=4,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,75,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi3,row=5,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,149,45) + ",FORMAT=X(50)"
            + "|type=literal,name=fi4,row=5,col=72,enable=false,width=15,scrval=Message Id: " + ipcMessageID + " ,FORMAT=X(15)"
            + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
            + "|type=win,name=fi5,enable=true,label=" + cCurrentTitle + ",FORMAT=X(30),height=9".
        END.
        ELSE DO:
            cParmList =
            "type=literal,name=fi1,row=2,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,1,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi2,row=3,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,75,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi3,row=4,col=16,enable=false,width=72,scrval=" + SUBSTRING(cCurrentMessage,149,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi4,row=5,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,223,74) + ",FORMAT=X(74)"
            + "|type=literal,name=fi5,row=6,col=16,enable=false,width=55,scrval=" + SUBSTRING(cCurrentMessage,297,45) + ",FORMAT=X(50)"
            + "|type=literal,name=fi6,row=6,col=72,enable=false,width=15,scrval=Message Id: " + ipcMessageID + " ,FORMAT=X(15)"
            + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
            + "|type=win,name=fi7,enable=true,label=" + cCurrentTitle + ",FORMAT=X(30),height=10".
        END.
        
        RUN custom/d-prompt.w (INPUT "yes-no", cParmList, "", OUTPUT cReturnValues).
        oplValue = LOGICAL(ENTRY(2,cReturnValues)) NO-ERROR .

    END.
    


END PROCEDURE.

/* ************************  Function Implementations ***************** */



