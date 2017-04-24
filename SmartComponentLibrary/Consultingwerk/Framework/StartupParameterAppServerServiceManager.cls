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
    File        : StartupParameterAppServerServiceManager
    Purpose     : IAppServerServiceManager implementation that provides
                  two partitions: "client" and "server". The "server" partition
                  is the default partition.
    Syntax      : FrameworkSettings:AppServerServiceManager
    Description : The AppServer connection parameters for the "server"
                  partition are obtained from the -param startup parameter
                  in the form of: AppServer=AppServer://server/AppServerName or
                  AppServer=http://server/aia/Aia1?AppService=AppServerName
                  or another ABL supported AppServer URL syntax. The -param
                  can contain othegr settings as well - comma delimited
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 31 12:24:04 CEST 2015
    Notes       : SCL-994
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Framework.StartupParameterAppServerServiceManager
    IMPLEMENTS IAppServerServiceManager USE-WIDGET-POOL:

    DEFINE VARIABLE hAppServer    AS HANDLE    NO-UNDO .
    DEFINE VARIABLE cAppServerUrl AS CHARACTER NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of AppServer partitions supported by the
                 ServiceManager implementation
        Notes:   Returns "client,server"
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Partitions AS CHARACTER NO-UNDO INITIAL "client,server":U
    GET.


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StartupParameterAppServerServiceManager class
        Notes:   Fails when the startup parameter do not contain the -param AppServer=...
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StartupParameterAppServerServiceManager ():
        THIS-OBJECT (ListHelper:AlternatingListToDictionary (SESSION:PARAMETER, ",":U, "=":U)).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StartupParameterAppServerServiceManager class
        Notes:   Fails when the startup parameter do not contain the -param AppServer=...
                 Expects to find it in an entry with the key "AppServer" here
        @param poParameters The CharacterDictionary with the parsed -param values
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StartupParameterAppServerServiceManager (poParameters AS CharacterDictionary):
        THIS-OBJECT (poParameters:GetValue ("AppServer":U)).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StartupParameterAppServerServiceManager class
        Notes:   Fails when the URL provided is empty
        @param pcAppServerUrl The AppServer URL
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StartupParameterAppServerServiceManager (pcAppServerUrl AS CHARACTER):
        SUPER ().

        Assert:NotNullOrEmpty (pcAppServerUrl, "AppServer":U) .

        ASSIGN cAppServerUrl = pcAppServerUrl .

    END CONSTRUCTOR .

     /*------------------------------------------------------------------------------
        Purpose: Connects a single AppServer partition
        Notes:   Returns the Handle to the AppServer partition (Server handle or
                 session handle). Throws error when connection fails
        @param pcServicePartition The partition to return a server handle for
        @return The HANDLE of the server object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE ConnectService (pcServicePartition AS CHARACTER):

        IF pcServicePartition = "client":U THEN
            RETURN SESSION:HANDLE .

        IF NOT VALID-HANDLE (hAppServer) THEN
            CREATE SERVER hAppServer .

        IF NOT hAppServer:CONNECTED () THEN
            hAppServer:CONNECT (SUBSTITUTE ("-URL &1":U, cAppServerUrl)) .

        RETURN hAppServer .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Disconnects a single AppServer partition
        Notes:
        @param pcServicePartition The partition to disconnect from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisconnectService (pcServicePartition AS CHARACTER):

        IF pcServicePartition = "client":U THEN
            RETURN .

        IF NOT VALID-HANDLE (hAppServer) THEN
            RETURN .

        IF hAppServer:CONNECTED () THEN
            hAppServer:DISCONNECT () .

        GarbageCollectorHelper:DeleteObject (hAppServer) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition - if connected -
                 returns ? when not connected.
        Notes:
        @param pcServicePartition The name of the partition to return the handle for
        @return The HANDLE of the server object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER):

        IF pcServicePartition = "client":U THEN
            RETURN SESSION:HANDLE .

        IF VALID-HANDLE (hAppServer) AND hAppServer:CONNECTED () THEN
            RETURN hAppServer .

        RETURN ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle to a AppServer partition
        Notes:   When plConnectIfRequired is set to TRUE, the partition will be
                 connected if required.
        @param pcServicePartition The name of the partition to return the handle for
        @param plConnectIfRequired Connect if not already connected
        @return The HANDLE of the server object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetServiceHandle (pcServicePartition AS CHARACTER,
                                           plConnectIfRequired AS LOGICAL):

        IF pcServicePartition = "client":U THEN
            RETURN SESSION:HANDLE .

        IF VALID-HANDLE (hAppServer) AND hAppServer:CONNECTED () THEN
            RETURN hAppServer .

        RETURN THIS-OBJECT:ConnectService (pcServicePartition) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the specified AppServer Partition is known and
                 connected.
        Notes:
        @param pcServicePartition The name of the partition to check if connection is established
        @return Logical value indicating if the service is connected
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsConnected (pcServicePartition AS CHARACTER):

        IF VALID-HANDLE (hAppServer) AND hAppServer:CONNECTED () THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE .

    END METHOD .

END CLASS.
