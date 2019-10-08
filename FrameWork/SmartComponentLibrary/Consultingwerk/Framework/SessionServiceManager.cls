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
    File        : SessionServiceManager
    Purpose     : Default AppServerServiceManager
    Syntax      : 
    Description : This AppServerServiceManager instance implements the 
                  IAppServerServiceManager interface. The only supported
                  AppServer Partition is "SESSION" for local (client side) 
                  operation. Attempts to "connect" to other partitions 
                  are gracefully ignored (SESSION will be used instead). 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 17:04:26 CEST 2009
    Notes       : No AppServer connection is established. The SESSION
                  handle is used as AppServer handle. 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.IAppServerServiceManager FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Framework.SessionServiceManager 
    IMPLEMENTS IAppServerServiceManager  : 
    
     /*------------------------------------------------------------------------------
        Purpose: Returns a list of AppServer partitions supported by the
                 ServiceManager implementation                                                                        
        Notes:   Always returns SESSION                                                                       
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Partitions AS CHARACTER NO-UNDO 
    GET:
        RETURN "SESSION":U .
    END.
    
     /*------------------------------------------------------------------------------
        Purpose: Connects a single AppServer partition                                                                        
        Notes:   Always returns the SESSION:HANDLE                                                                         
        @param pcServicePartition The partition to return a server handle for
        @return The SESSION:HANDLE                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE ConnectService (pcServicePartition AS CHARACTER):
        
        RETURN SESSION:HANDLE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disconnects a single AppServer partition                                                                        
        Notes:  
        @param pcServicePartition The partition to disconnect from
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID DisconnectService (pcServicePartition AS CHARACTER):
        
        RETURN . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition - if connected - 
                 returns ? when not connected.                                                                        
        Notes:   Always returns the SESSION:HANDLE
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @return The SESSION:HANDLE
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER):
        
        RETURN GetServiceHandle ("SESSION":U, FALSE) .

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: Returns the SESSION handle.                                                          
        Notes:   Always returns SESSION:HANDLE                                                                        
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @param plConnectIfRequired Connect if not already connected
        @return The SESSION:HANDLE
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER, 
                                           plConnectIfRequired AS LOGICAL):
        
        RETURN SESSION:HANDLE . 

    END METHOD.


 
    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the specified AppServer Partition is known and 
                 connected.                                                                        
        Notes:   The SESSION is always connected.      
        @param pcServicePartition The name of the partition to check if connection is established                                                                      
        @return True, as the SESSION:HANDLE is always connected
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC LOGICAL IsConnected (INPUT pcServicePartition AS CHARACTER ):
        
        RETURN TRUE . 

    END METHOD.

END CLASS.