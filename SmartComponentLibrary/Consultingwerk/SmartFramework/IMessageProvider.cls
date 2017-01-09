/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : IMessageProvider
    Purpose     : Interface for services that return message text, detail 
                  and type
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 20:43:48 CET 2013
    Notes       : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Accessing+Messages+from+the+SmartFramework+Message+table
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.IMessageProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER,
                                      pcParameter5 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER,
                                      pcParameter5 AS CHARACTER,
                                      pcParameter6 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER,
                                      pcParameter5 AS CHARACTER,
                                      pcParameter6 AS CHARACTER,
                                      pcParameter7 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER,
                                      pcParameter5 AS CHARACTER,
                                      pcParameter6 AS CHARACTER,
                                      pcParameter7 AS CHARACTER,
                                      pcParameter8 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @param pcParameter9 The nineth message parameter
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER,
                                      pcParameter1 AS CHARACTER,
                                      pcParameter2 AS CHARACTER,
                                      pcParameter3 AS CHARACTER,
                                      pcParameter4 AS CHARACTER,
                                      pcParameter5 AS CHARACTER,
                                      pcParameter6 AS CHARACTER,
                                      pcParameter7 AS CHARACTER,
                                      pcParameter8 AS CHARACTER,
                                      pcParameter9 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER).
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER,
                                              pcParameter5 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER,
                                              pcParameter5 AS CHARACTER,
                                              pcParameter6 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER,
                                              pcParameter5 AS CHARACTER,
                                              pcParameter6 AS CHARACTER,
                                              pcParameter7 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER,
                                              pcParameter5 AS CHARACTER,
                                              pcParameter6 AS CHARACTER,
                                              pcParameter7 AS CHARACTER,
                                              pcParameter8 AS CHARACTER).


    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @param pcParameter9 The nineth message parameter
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER,
                                              pcParameter1 AS CHARACTER,
                                              pcParameter2 AS CHARACTER,
                                              pcParameter3 AS CHARACTER,
                                              pcParameter4 AS CHARACTER,
                                              pcParameter5 AS CHARACTER,
                                              pcParameter6 AS CHARACTER,
                                              pcParameter7 AS CHARACTER,
                                              pcParameter8 AS CHARACTER,
                                              pcParameter9 AS CHARACTER).
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER,
                                            pcParameter5 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER,
                                            pcParameter5 AS CHARACTER,
                                            pcParameter6 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER,
                                            pcParameter5 AS CHARACTER,
                                            pcParameter6 AS CHARACTER,
                                            pcParameter7 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER,
                                            pcParameter5 AS CHARACTER,
                                            pcParameter6 AS CHARACTER,
                                            pcParameter7 AS CHARACTER,
                                            pcParameter8 AS CHARACTER).


    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @param pcParameter6 The sixth message parameter
        @param pcParameter7 The seventh message parameter
        @param pcParameter8 The eigth message parameter
        @param pcParameter9 The nineth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER,
                                            pcParameter1 AS CHARACTER,
                                            pcParameter2 AS CHARACTER,
                                            pcParameter3 AS CHARACTER,
                                            pcParameter4 AS CHARACTER,
                                            pcParameter5 AS CHARACTER,
                                            pcParameter6 AS CHARACTER,
                                            pcParameter7 AS CHARACTER,
                                            pcParameter8 AS CHARACTER,
                                            pcParameter9 AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the message type for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message type value: Error, Info, Question, Warning
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC MessageTypeEnum GetMessageType (pcMessageGroup AS CHARACTER,
                                                  piMessageNumber AS INTEGER).
                                              
END INTERFACE.
