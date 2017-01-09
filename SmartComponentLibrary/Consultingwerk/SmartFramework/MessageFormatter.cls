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
    File        : MessageFormatter
    Purpose     : Provides methods that return a message structure to be 
                  expanded on the client using the IMessageProvider
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 21:50:52 CEST 2013
    Notes       : Does not actually call into the IMessageProvider to return
                  the user-friendly message
                  See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Accessing+Messages+from+the+SmartFramework+Message+table
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.MessageFormatter: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @return The message structure
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
	                                           piMessageNumber AS INTEGER):
		
		RETURN GetMessage (pcMessageGroup, 
		                   piMessageNumber, 
		                   "":U, "":U, "":U, 
		                   "":U, "":U, "":U, 
		                   "":U, "":U, "":U).

	END METHOD .
	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, "":U, "":U, 
                           "":U, "":U, "":U, 
                           "":U, "":U, "":U).

    END METHOD .	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, "":U, 
                           "":U, "":U, "":U, 
                           "":U, "":U, "":U).

    END METHOD .	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           "":U, "":U, "":U, 
                           "":U, "":U, "":U).

    END METHOD .
    	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           pcMessageParam4, "":U, "":U, 
                           "":U, "":U, "":U).

    END METHOD .
    	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @param pcMessageParam5 The fifth parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER,
                                               pcMessageParam5 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           pcMessageParam4, pcMessageParam5, "":U, 
                           "":U, "":U, "":U).

    END METHOD .
    	
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @param pcMessageParam5 The fifth parameter for the message
        @param pcMessageParam6 The sixt parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER,
                                               pcMessageParam5 AS CHARACTER,
                                               pcMessageParam6 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           pcMessageParam4, pcMessageParam5, pcMessageParam6, 
                           "":U, "":U, "":U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @param pcMessageParam5 The fifth parameter for the message
        @param pcMessageParam6 The sixt parameter for the message
        @param pcMessageParam7 The seventh parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER,
                                               pcMessageParam5 AS CHARACTER,
                                               pcMessageParam6 AS CHARACTER,
                                               pcMessageParam7 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           pcMessageParam4, pcMessageParam5, pcMessageParam6, 
                           pcMessageParam7, "":U, "":U).

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @param pcMessageParam5 The fifth parameter for the message
        @param pcMessageParam6 The sixt parameter for the message
        @param pcMessageParam7 The seventh parameter for the message
        @param pcMessageParam8 The eigth parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER,
                                               pcMessageParam5 AS CHARACTER,
                                               pcMessageParam6 AS CHARACTER,
                                               pcMessageParam7 AS CHARACTER,
                                               pcMessageParam8 AS CHARACTER):
         
        RETURN GetMessage (pcMessageGroup, 
                           piMessageNumber, 
                           pcMessageParam1, pcMessageParam2, pcMessageParam3, 
                           pcMessageParam4, pcMessageParam5, pcMessageParam6, 
                           pcMessageParam7, pcMessageParam8, "":U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a message structure to be expanded on the client using the 
                 IMessageProvider  
        Notes:   
        @param pcMessageGroup The message group to return
        @param piMessageNumber The message number to return
        @param pcMessageParam1 The first parameter for the message
        @param pcMessageParam2 The second parameter for the message
        @param pcMessageParam3 The third parameter for the message
        @param pcMessageParam4 The fourth parameter for the message
        @param pcMessageParam5 The fifth parameter for the message
        @param pcMessageParam6 The sixt parameter for the message
        @param pcMessageParam7 The seventh parameter for the message
        @param pcMessageParam8 The eigth parameter for the message
        @param pcMessageParam9 The ninth parameter for the message
        @return The message structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetMessage (pcMessageGroup AS CHARACTER,
                                               piMessageNumber AS INTEGER,
                                               pcMessageParam1 AS CHARACTER,
                                               pcMessageParam2 AS CHARACTER,
                                               pcMessageParam3 AS CHARACTER,
                                               pcMessageParam4 AS CHARACTER,
                                               pcMessageParam5 AS CHARACTER,
                                               pcMessageParam6 AS CHARACTER,
                                               pcMessageParam7 AS CHARACTER,
                                               pcMessageParam8 AS CHARACTER,
                                               pcMessageParam9 AS CHARACTER):
         
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcMessageGroup} .
        Assert:NotNullOrZero (piMessageNumber) . 
        
        RETURN TRIM (SUBSTITUTE ("SmartMessage~t&1~t&2":U, pcMessageGroup, piMessageNumber) + 
                     SUBSTITUTE ("~t&1~t&2~t&3~t&4~t&5~t&6~t&7~t&8~t&9":U, 
                                 pcMessageParam1,
                                 pcMessageParam2,
                                 pcMessageParam3,
                                 pcMessageParam4,
                                 pcMessageParam5,
                                 pcMessageParam6,
                                 pcMessageParam7,
                                 pcMessageParam8,
                                 pcMessageParam9), "~t":U).        

    END METHOD .

END CLASS.
