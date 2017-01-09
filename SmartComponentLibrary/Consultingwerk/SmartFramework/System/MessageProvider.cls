/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : MessageProvider
    Purpose     : Default implementation of the IMessageProvider Interface
    Syntax      : 
    Description : Uses the MessageBusinessEntity to access messages stored
                  in the SmartDB, uses a local cache to store messages
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 21:39:17 CET 2013
    Notes       : The MessageProvider is primarily intended to be used from 
                  client side code. It can also be used on the AppServer,
                  however in those cases the caching (by the current language)
                  of message records that are disposed when the language 
                  changes may cause smoe overhead 
                  See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Accessing+Messages+from+the+SmartFramework+Message+table
                  Does also provide static versions of the GetMessageText
                  methods, which serve as a static wrapper to the GetMessageText
                  method of the current IMessageProvider implementation,
                  typically this is the MessageProvider class itself (ease of use).
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.Framework.*             FROM PROPATH . 
USING Consultingwerk.Framework.Session.*     FROM PROPATH . 
USING Consultingwerk.OERA.*                  FROM PROPATH . 
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.MessageProvider 
    IMPLEMENTS IMessageProvider: 

    { Consultingwerk/SmartFramework/System/eSmartMessage.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageProvider class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MessageProvider ():
		SUPER ().
		
        SessionManager:LanguageKeyChanged:Subscribe (LanguageKeyChangedHandler) .		
        
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fetches a message from the backend using the MessageBusinessEntity
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FetchMessageFromBusinessEntity (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER):
		
		DEFINE VARIABLE oGetMessageParameter AS GetMessageParameter NO-UNDO . 
		DEFINE VARIABLE hDataset             AS HANDLE              NO-UNDO .
		
		ASSIGN oGetMessageParameter = NEW GetMessageParameter (pcMessageGroup,
		                                                       piMessageNumber,
		                                                       SessionManager:LanguageKey) .

        IF CONNECTED ("SmartDB":U) THEN DO: 
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.MessageBusinessEntity":U,
                                           "GetMessage":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oGetMessageParameter) .
        END.                                                
        ELSE 
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.System.MessageBusinessEntity":U,
                                                           "GetMessage":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oGetMessageParameter) . 

        IF NOT VALID-HANDLE (hDataset) THEN 
            RETURN . 
            
        hDataset::eSmartMessage:FIND-FIRST () NO-ERROR . 
        
        IF hDataset::eSmartMessage:AVAILABLE THEN DO:
            CREATE eSmartMessage . 
            BUFFER eSmartMessage:BUFFER-COPY (hDataset::eSmartMessage:HANDLE) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Locates a eSmartMessageRecord in the cache or on the backend
        Notes:   When the backend has no record available, a dummy record will be 
                 created to avoid additional backend calls
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FindMessageRecord (pcMessageGroup AS CHARACTER,
                                             piMessageNumber AS INTEGER):
		
		FIND FIRST eSmartMessage WHERE eSmartMessage.MessageGroup  = pcMessageGroup
		                           AND eSmartMessage.MessageNumber = piMessageNumber
	        NO-ERROR . 
	        
        IF NOT AVAILABLE eSmartMessage THEN 
            THIS-OBJECT:FetchMessageFromBusinessEntity (pcMessageGroup,
                                                        piMessageNumber) .
            
        FIND FIRST eSmartMessage WHERE eSmartMessage.MessageGroup  = pcMessageGroup
                                   AND eSmartMessage.MessageNumber = piMessageNumber
            NO-ERROR . 
	        
        IF NOT AVAILABLE eSmartMessage THEN DO:
            CREATE eSmartMessage.
            ASSIGN eSmartMessage.MessageGroup  = pcMessageGroup
                   eSmartMessage.MessageNumber = piMessageNumber 
                   eSmartMessage.MessageType   = "Info":U
                   eSmartMessage.MessageText   = 
                        SUBSTITUTE ("Undefined message <&1/&2> ":U,
                                    pcMessageGroup,
                                    piMessageNumber) + 
                        Consultingwerk.Environment:NewLine +
                        "&1 &2 &3 &4 &5 &6 &7 &8 &9":U 
                   eSmartMessage.MessageDetail = 
                        SUBSTITUTE ("Message not available <&1/&2> ":U,
                                    pcMessageGroup,
                                    piMessageNumber) + 
                        Consultingwerk.Environment:NewLine +
                        "&1":U + Consultingwerk.Environment:NewLine + 
                        "&2":U + Consultingwerk.Environment:NewLine + 
                        "&3":U + Consultingwerk.Environment:NewLine + 
                        "&4":U + Consultingwerk.Environment:NewLine + 
                        "&5":U + Consultingwerk.Environment:NewLine + 
                        "&6":U + Consultingwerk.Environment:NewLine + 
                        "&7":U + Consultingwerk.Environment:NewLine + 
                        "&8":U + Consultingwerk.Environment:NewLine + 
                        "&9":U .            
                        .	        
        END.
    
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a Message structure for the specified message group and number
                 and parameters 
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message Structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Message GetMessage (pcMessageGroup AS CHARACTER,
                                      piMessageNumber AS INTEGER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           "":U, "":U, "":U,
                           "":U, "":U, "":U,
                           "":U, "":U, "":U) .                                          
    END METHOD .                                           

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
                                      pcParameter1 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, "":U, "":U,
                           "":U, "":U, "":U,
                           "":U, "":U, "":U) .                                          
    END METHOD .      

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
                                      pcParameter2 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, "":U,
                           "":U, "":U, "":U,
                           "":U, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter3 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           "":U, "":U, "":U,
                           "":U, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter4 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           pcParameter4, "":U, "":U,
                           "":U, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter5 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           pcParameter4, pcParameter5, "":U,
                           "":U, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter6 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           pcParameter4, pcParameter5, pcParameter6,
                           "":U, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter7 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           pcParameter4, pcParameter5, pcParameter6,
                           pcParameter7, "":U, "":U) .                                         
    END METHOD .      

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
                                      pcParameter8 AS CHARACTER):
                 
        RETURN GetMessage (pcMessageGroup,
                           piMessageNumber,
                           pcParameter1, pcParameter2, pcParameter3,
                           pcParameter4, pcParameter5, pcParameter6,
                           pcParameter7, pcParameter8, "":U) .                                         
    END METHOD .      

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
                                      pcParameter9 AS CHARACTER):
                 
        DEFINE VARIABLE oMessage AS Message NO-UNDO .                  
                                          
        ASSIGN oMessage = NEW Message () 
               
               oMessage:Text   = GetMessageText   (pcMessageGroup, piMessageNumber, pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5, pcParameter6, pcParameter7, pcParameter8, pcParameter9)
               oMessage:Detail = GetMessageDetail (pcMessageGroup, piMessageNumber, pcParameter1, pcParameter2, pcParameter3, pcParameter4, pcParameter5, pcParameter6, pcParameter7, pcParameter8, pcParameter9)
               oMessage:Type   = GetMessageType   (pcMessageGroup, piMessageNumber)
               .

        RETURN oMessage . 
                                          
    END METHOD .                                           
                                      
    /*------------------------------------------------------------------------------
        Purpose: Returns the message detail for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message detail
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageDetail (pcMessageGroup AS CHARACTER,
                                              piMessageNumber AS INTEGER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             "":U, "":U, "":U,
                                             "":U, "":U, "":U,
                                             "":U, "":U, "":U) .                                          
                                           
    END METHOD .                                            


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
                                              pcParameter1 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, "":U, "":U,
                                             "":U, "":U, "":U,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter2 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, "":U,
                                             "":U, "":U, "":U,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter3 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             "":U, "":U, "":U,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter4 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             pcParameter4, "":U, "":U,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  
  
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
                                              pcParameter5 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             pcParameter4, pcParameter5, "":U,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter6 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             pcParameter4, pcParameter5, pcParameter6,
                                             "":U, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter7 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             pcParameter4, pcParameter5, pcParameter6,
                                             pcParameter7, "":U, "":U) . 
                                           
    END METHOD .                                  

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
                                              pcParameter8 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageDetail (pcMessageGroup,
                                             piMessageNumber,
                                             pcParameter1, pcParameter2, pcParameter3,
                                             pcParameter4, pcParameter5, pcParameter6,
                                             pcParameter7, pcParameter8, "":U) . 
                                           
    END METHOD .                                  


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
                                              pcParameter9 AS CHARACTER):
                                                
        THIS-OBJECT:FindMessageRecord (pcMessageGroup,
                                       piMessageNumber) .
                                       
        RETURN SUBSTITUTE (eSmartMessage.MessageDetail,
                           pcParameter1,
                           pcParameter2,
                           pcParameter3,
                           pcParameter4,
                           pcParameter5,
                           pcParameter6,
                           pcParameter7,
                           pcParameter8,
                           pcParameter9) .                                       
                                           
    END METHOD .                                  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMessageText (pcMessageGroup AS CHARACTER,
                                            piMessageNumber AS INTEGER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           "":U, "":U, "":U,
                                           "":U, "":U, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD .                                            

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
                                            pcParameter1 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, "":U, "":U,
                                           "":U, "":U, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter2 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, "":U,
                                           "":U, "":U, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter3 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           "":U, "":U, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter4 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           pcParameter4, "":U, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter5 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           pcParameter4, pcParameter5, "":U,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter6 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           pcParameter4, pcParameter5, pcParameter6,
                                           "":U, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter7 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           pcParameter4, pcParameter5, pcParameter6,
                                           pcParameter7, "":U, "":U) .                                          
                                           
    END METHOD . 

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
                                            pcParameter8 AS CHARACTER):
                                                
        RETURN THIS-OBJECT:GetMessageText (pcMessageGroup,
                                           piMessageNumber,
                                           pcParameter1, pcParameter2, pcParameter3,
                                           pcParameter4, pcParameter5, pcParameter6,
                                           pcParameter7, pcParameter8, "":U) .                                          
                                           
    END METHOD . 


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

        THIS-OBJECT:FindMessageRecord (pcMessageGroup,
                                       piMessageNumber) .
                                       
        RETURN SUBSTITUTE (eSmartMessage.MessageText,
                           pcParameter1,
                           pcParameter2,
                           pcParameter3,
                           pcParameter4,
                           pcParameter5,
                           pcParameter6,
                           pcParameter7,
                           pcParameter8,
                           pcParameter9) +
               SUBSTITUTE (" (&1:&2)":U, pcMessageGroup, piMessageNumber) .                                       

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber) .

    END METHOD .                                            

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                "", 
                                                "", 
                                                "", 
                                                "", 
                                                "", 
                                                "", 
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                "",
                                                "",
                                                "",
                                                "",
                                                "",
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                "",
                                                "",
                                                "",
                                                "",
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER,
                                                          pcParameter4 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                "",
                                                "",
                                                "",
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @param pcParameter1 The first message parameter
        @param pcParameter2 The second message parameter
        @param pcParameter3 The third message parameter
        @param pcParameter4 The fourth message parameter
        @param pcParameter5 The fifth message parameter
        @return The message text
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER,
                                                          pcParameter4 AS CHARACTER,
                                                          pcParameter5 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                pcParameter5,
                                                "",
                                                "",
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
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
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER,
                                                          pcParameter4 AS CHARACTER,
                                                          pcParameter5 AS CHARACTER,
                                                          pcParameter6 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                pcParameter5,
                                                pcParameter6,
                                                "",
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
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
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER,
                                                          pcParameter4 AS CHARACTER,
                                                          pcParameter5 AS CHARACTER,
                                                          pcParameter6 AS CHARACTER,
                                                          pcParameter7 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                pcParameter5,
                                                pcParameter6,
                                                pcParameter7,
                                                "",
                                                "") .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
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
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
                                                          piMessageNumber AS INTEGER,
                                                          pcParameter1 AS CHARACTER,
                                                          pcParameter2 AS CHARACTER,
                                                          pcParameter3 AS CHARACTER,
                                                          pcParameter4 AS CHARACTER,
                                                          pcParameter5 AS CHARACTER,
                                                          pcParameter6 AS CHARACTER,
                                                          pcParameter7 AS CHARACTER,
                                                          pcParameter8 AS CHARACTER):

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                pcParameter5,
                                                pcParameter6,
                                                pcParameter7,
                                                pcParameter8,
                                                "") .

    END METHOD . 


    /*------------------------------------------------------------------------------
        Purpose: Returns the message text for the specified MessageGroup and Number 
                 and message parameters
        Notes:   Static wrapper to the GetMessageText method of the current IMessageProvider 
                 service (typically this class itself)
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
    METHOD PUBLIC STATIC CHARACTER GetMessageTextWrapper (pcMessageGroup AS CHARACTER,
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

        DEFINE VARIABLE oMessageProvider AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO . 

        oMessageProvider = {Consultingwerk/get-mandatory-service.i Consultingwerk.SmartFramework.IMessageProvider} .

        RETURN oMessageProvider:GetMessageText (pcMessageGroup,
                                                piMessageNumber,
                                                pcParameter1,
                                                pcParameter2,
                                                pcParameter3,
                                                pcParameter4,
                                                pcParameter5,
                                                pcParameter6,
                                                pcParameter7,
                                                pcParameter8,
                                                pcParameter9) .

    END METHOD .
                                                     
    /*------------------------------------------------------------------------------
        Purpose: Returns the message type for the specified MessageGroup and Number 
                 and message parameters
        Notes:   
        @param pcMessageGroup The message group
        @param piMessageNumber The message number
        @return The message type value: Error, Info, Question, Warning
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC MessageTypeEnum GetMessageType (pcMessageGroup AS CHARACTER,
                                                  piMessageNumber AS INTEGER):
                                                

        THIS-OBJECT:FindMessageRecord (pcMessageGroup,
                                       piMessageNumber) .
                                       
&IF DEFINED (NativeEnums) NE 0 &THEN
        RETURN CAST (Progress.Lang.Enum:ToObject ("Consultingwerk.SmartFramework.MessageTypeEnum":U,
                                                  eSmartMessage.MessageType), 
                     Consultingwerk.SmartFramework.MessageTypeEnum) . 
&ELSE            
        RETURN MessageTypeEnum:FromString (eSmartMessage.MessageType) .                                       
&ENDIF                                                
    END METHOD .                                                 

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LanguageKeyChanged event of the SessionManager
        Notes:   Cleares the Message Cache
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID LanguageKeyChangedHandler ():
		
		EMPTY TEMP-TABLE eSmartMessage . 

	END METHOD .
                                              
END CLASS.
