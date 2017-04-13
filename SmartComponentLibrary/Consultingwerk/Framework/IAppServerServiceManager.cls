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
    File        : IAppServerServiceManager
    Purpose     : Interface describing the methods required by a service
                  manager used by the SmartComponent Library
    Syntax      : CLASS xyz IMPLEMENTS IAppServerServiceManager
    Description : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Managing+AppServer+Connections
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 16:45:19 CEST 2009
    Notes       : The service Manager interface can also be implemented
                  by an adapter to another service manager (Dynamics or
                  ADM2).
  ----------------------------------------------------------------------*/

INTERFACE Consultingwerk.Framework.IAppServerServiceManager:  
  
     /*------------------------------------------------------------------------------
        Purpose: Returns a list of AppServer partitions supported by the
                 ServiceManager implementation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Partitions AS CHARACTER NO-UNDO 
    GET.

     /*------------------------------------------------------------------------------
        Purpose: Connects a single AppServer partition                                                                        
        Notes:   Returns the Handle to the AppServer partition (Server handle or 
                 session handle). Throws error when connection fails   
        @param pcServicePartition The partition to return a server handle for
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE ConnectService (pcServicePartition AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Disconnects a single AppServer partition                                                                        
        Notes:                      
        @param pcServicePartition The partition to disconnect from
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID DisconnectService (pcServicePartition AS CHARACTER).
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition - if connected - 
                 returns ? when not connected.                                                                        
        Notes:  
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition                                                          
        Notes:   When plConnectIfRequired is set to TRUE, the partition will be 
                 connected if required.                                                                        
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @param plConnectIfRequired Connect if not already connected
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition  AS CHARACTER,
                                           plConnectIfRequired AS LOGICAL).

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the specified AppServer Partition is known and 
                 connected.                                                                        
        Notes:                  
        @param pcServicePartition The name of the partition to check if connection is established                                                                      
        @return Logical value indicating if the service is connected
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC LOGICAL IsConnected (pcServicePartition  AS CHARACTER).

 END INTERFACE.
 