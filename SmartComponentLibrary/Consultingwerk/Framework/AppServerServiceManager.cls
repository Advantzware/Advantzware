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
    File        : AppServerServiceManager
    Purpose     : IAppServerServiceManager Implementation, wrapper to 
                  adecomm/as-utils.w 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 17:53:10 CEST 2009
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Managing+AppServer+Connections 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.IAppServerServiceManager FROM PROPATH .
USING Consultingwerk.Framework.Exceptions.*             FROM PROPATH . 
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Framework.AppServerServiceManager 
    IMPLEMENTS IAppServerServiceManager: 
    
    DEFINE PRIVATE VARIABLE appSrvUtils AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppServerServiceManager class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC AppServerServiceManager ():
        SUPER ().
    
        RUN Consultingwerk/Framework/launch-appsrvutils.p (INPUT-OUTPUT appSrvUtils) .
        
        IF NOT VALID-HANDLE (appSrvUtils) THEN 
            UNDO, THROW NEW AppServerConfigurationException () .
        
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the DefaultPartition used when the request is not
                 specifying a partition name                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY DefaultPartition AS CHARACTER NO-UNDO 
    GET.
    SET. 

     /*------------------------------------------------------------------------------
        Purpose: Returns a list of AppServer partitions supported by the
                 ServiceManager implementation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Partitions AS CHARACTER NO-UNDO 
    GET:
        RETURN DYNAMIC-FUNCTION ("definedPartitions":U IN appSrvUtils) .
    END.
    
     /*------------------------------------------------------------------------------
        Purpose: Connects a single AppServer partition                                                                        
        Notes:   Returns the Handle of the AppServer partition (Server handle or 
                 session handle). Throws error when connection fails                                                                         
        @param pcServicePartition The partition to return a server handle for
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE ConnectService (pcServicePartition AS CHARACTER):
        
        DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
        
        IF pcServicePartition > "":U THEN . 
        ELSE 
            pcServicePartition = THIS-OBJECT:DefaultPartition . 
        
        RUN appServerConnect IN appSrvUtils 
            (pcServicePartition, ?, ?, OUTPUT hServer) .

        IF RETURN-VALUE = "ERROR":U THEN DO:

            IF pcServicePartition > "":U THEN             
                UNDO, THROW NEW AppServerConnectionException (pcServicePartition) . 
            ELSE 
                UNDO, THROW NEW AppServerConnectionException () . 
        END.

        RETURN hServer . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disconnects a single AppServer partition                                                                        
        Notes:      
        @param pcServicePartition The partition to disconnect from
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID DisconnectService (pcServicePartition AS CHARACTER ):
                        
        IF pcServicePartition > "":U THEN . 
        ELSE 
            pcServicePartition = THIS-OBJECT:DefaultPartition . 

        RUN appServerDisconnect IN appSrvUtils 
            (pcServicePartition) .

    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition - if connected - 
                 returns ? when not connected.                                                                        
        Notes:                                                                        
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER ):
        
        IF pcServicePartition > "":U THEN . 
        ELSE 
            pcServicePartition = THIS-OBJECT:DefaultPartition . 

        RETURN THIS-OBJECT:GetServiceHandle (pcServicePartition, FALSE) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition                                                          
        Notes:   When plConnectIfRequired is set to TRUE, the partition will be 
                 connected if required.      
        @param pcServicePartition The name of the partition to return the handle for                                                                      
        @param plConnectIfRequired Connect if not already connected
        @return The HANDLE of the server object                                                                               
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER, 
                                           plConnectIfRequired AS LOGICAL ):
                        
        IF pcServicePartition > "":U THEN . 
        ELSE 
            pcServicePartition = THIS-OBJECT:DefaultPartition . 
                        
        IF THIS-OBJECT:IsConnected (pcServicePartition) THEN 
            RETURN DYNAMIC-FUNCTION ("getServiceHandle":U IN appSrvUtils, pcServicePartition) .
        ELSE 
            RETURN THIS-OBJECT:ConnectService (pcServicePartition) . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the specified AppServer Partition is known and 
                 connected.                                                                        
        Notes:                                                                        
        @param pcServicePartition The name of the partition to check if connection is established                                                                      
        @return Logical value indicating if the service is connected
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC LOGICAL IsConnected (pcServicePartition AS CHARACTER ):
        
        DEFINE VARIABLE hService AS HANDLE NO-UNDO.
        
        IF pcServicePartition > "":U THEN . 
        ELSE 
            pcServicePartition = THIS-OBJECT:DefaultPartition . 
        
        ASSIGN hService = DYNAMIC-FUNCTION ("getServiceHandle":U IN appSrvUtils, pcServicePartition) .
        
        IF NOT VALID-HANDLE(hService) THEN RETURN FALSE . 
        
        ELSE RETURN hService:CONNECTED () .        

    END METHOD.

END CLASS.