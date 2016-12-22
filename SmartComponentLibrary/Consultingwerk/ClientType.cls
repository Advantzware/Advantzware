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
    File        : ClientType
    Purpose     : An Enumeration of OpenEdge Client Types, see pseudo 
                  Widget property SESSION:CLIENT-TYPE
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 02 10:23:02 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.ClientType: 

    DEFINE PUBLIC STATIC PROPERTY APPSERVER           AS CHARACTER INIT "APPSERVER":U           GET. 
    DEFINE PUBLIC STATIC PROPERTY FourGLCLIENT        AS CHARACTER INIT "4GLCLIENT":U           GET. 
    DEFINE PUBLIC STATIC PROPERTY WEBCLIENT           AS CHARACTER INIT "WEBCLIENT":U           GET. 
    DEFINE PUBLIC STATIC PROPERTY WEBSPEED            AS CHARACTER INIT "WEBSPEED":U            GET. 
    DEFINE PUBLIC STATIC PROPERTY MULTI-SESSION-AGENT AS CHARACTER INIT "MULTI-SESSION-AGENT":U GET. 
      
    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ClientType ():
        SUPER ().
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in an 4GLClient
        Notes:   
        @return True if the current session is running in an 4GLClient 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL Is4GLClient ():
        
        RETURN IsClient (Consultingwerk.ClientType:FourGLCLIENT) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in an AppServer
        Notes:   
        @return True if the current session is running in an AppServer 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsAppServer ():
        
        RETURN IsClient (Consultingwerk.ClientType:APPSERVER) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is a known ABL client type 
        Notes:   
        @return True if the current session is a known ABL client type
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL IsKnownClientType ():
		
		RETURN ClientType:Is4GLClient() OR 
	           ClientType:IsAppServer() OR 
	           ClientType:IsWebClient() OR 
	           ClientType:IsWebSpeed()  OR 
	           ClientType:IsMultiSessionAgent() .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Determines if the given session is a known ABL client type 
        Notes:   
        @param pcClientType The value of the ABL SESSION:CLIENT-TYPE value for the session type to validate
        @return True if the current session is a known ABL client type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsKnownClientType (pcClientType AS CHARACTER):
        
        RETURN pcClientType = ClientType:FourGLCLIENT OR 
               pcClientType = ClientType:APPSERVER OR 
               pcClientType = ClientType:WEBCLIENT OR 
               pcClientType = ClientType:WEBSPEED OR 
               pcClientType = ClientType:MULTI-SESSION-AGENT .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in PASOE
        Notes:   
        @return True if the current session is running in an AppServer 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsMultiSessionAgent ():
        
        RETURN IsClient (Consultingwerk.ClientType:MULTI-SESSION-AGENT) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in an WebClient
        Notes:   
        @return True if the current session is running in an WebClient 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsWebClient ():
        
        RETURN IsClient (Consultingwerk.ClientType:WEBCLIENT) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in an WebSpeed
        Notes:   
        @return True if the current session is running in an WebSpeed 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsWebSpeed ():
        
        RETURN IsClient (Consultingwerk.ClientType:WEBSPEED) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if the current session is running in the specified client type
        Notes:   
        @param pcClientType The value to match with the ABL SESSION:CLIENT-TYPE value
        @return True if the current session is running in the specified client type 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsClient (pcClientType AS CHARACTER):
        
        RETURN SESSION:CLIENT-TYPE = pcClientType .

    END METHOD .

END CLASS.
