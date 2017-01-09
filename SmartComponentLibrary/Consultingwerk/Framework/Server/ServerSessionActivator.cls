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
    File        : ServerSessionActivator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 11:32:03 CET 2013
    Notes       : Although in our default setup this class is loaded as a
                  service using services.xml there is no need to implement
                  an interface. This class subscribes itself to events of
                  the service interface and does not have any public methods
          Custom LogEntryTypes supported: ProVersion
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*         FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH .
USING Consultingwerk.Framework.Server.*  FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Framework.Server.ServerSessionActivator:

    DEFINE VARIABLE cAuthenticationDB AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogEntryTypes    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDebugMode        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustomLogEntries AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ActivateServerSession was not able to reactivate
                 a users client-principal because the length of the CP was 0 (no
                 CP available)
        Notes:   This event may be used to authenticate a technical user that is
                 authorized to run requests on behalf of unauthenticated sessions
        @param sender The object that raised the PerformTechnicalLogin event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PerformTechnicalLogin SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                              e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServerSessionActivator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServerSessionActivator ():

        DEFINE VARIABLE oConfiguration AS IConfigurationProvider NO-UNDO .

        SUPER ().

        ServiceInterface:Activated:Subscribe (ActivateServerSession) .
        ServiceInterface:Deactivated:Subscribe (DeactivateServerSession) .

        oConfiguration = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfiguration) THEN
            cAuthenticationDB = oConfiguration:GetValue ("authenticationDb":U, LDBNAME(1)) .
        ELSE
            cAuthenticationDB = LDBNAME(1) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates a server session
        Notes:   The output of the Clients and AppServers ProVersion requires the
                 "ProVersion" custom log entry type to be activated within the LogManager
                 class
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateServerSession (e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE hPrincipal AS HANDLE NO-UNDO.

        ASSIGN cLogEntryTypes    = LOG-MANAGER:LOG-ENTRY-TYPES
               lDebugMode        = FrameworkSettings:DebugMode
               cCustomLogEntries = LogManager:CustomLogEntries .

        IF VALID-HANDLE (SessionManager:ContextDataset) THEN DO:
            IF SESSION:REMOTE AND DatasetHelper:HasBuffer (SessionManager:ContextDataset, "eSessionContext":U) THEN
                ContextWrapper:AppServerProVersion          = PROVERSION .

            ASSIGN
                SessionManager:ClientPrincipal              = ContextWrapper:ClientPrincipal
                SessionManager:LanguageDisplayName          = ContextWrapper:LanguageDisplayName
                SessionManager:LanguageIsoCode              = ContextWrapper:LanguageIsoCode
                SessionManager:LanguageKey                  = ContextWrapper:LanguageKey
                SessionManager:LoginCompanyKey              = ContextWrapper:LoginCompanyKey
                SessionManager:LoginCompanyName             = ContextWrapper:LoginCompanyName
                SessionManager:LoginCompanyReferenceChar    = ContextWrapper:LoginCompanyReferenceChar
                SessionManager:LoginCompanyReferenceDecimal = ContextWrapper:LoginCompanyReferenceDecimal
                SessionManager:LoginCompanyReferenceInteger = ContextWrapper:LoginCompanyReferenceInteger
                SessionManager:LoginCompanyShortName        = ContextWrapper:LoginCompanyShortName
                SessionManager:ServerLogEntryTypes          = ContextWrapper:ServerLogEntryTypes
                SessionManager:ServerDebugMode              = ContextWrapper:ServerDebugMode
                SessionManager:ServerCustomLogEntries       = ContextWrapper:ServerCustomLogEntries
                SessionManager:UserName                     = ContextWrapper:UserName
                SessionManager:UserKey                      = ContextWrapper:UserKey
                SessionManager:UserGroupKeys                = ContextWrapper:UserGroupKeys
                SessionManager:UserGroupNames               = ContextWrapper:UserGroupNames
                .

            IF SessionManager:ServerLogEntryTypes > "":U THEN
                ASSIGN LOG-MANAGER:LOG-ENTRY-TYPES = SessionManager:ServerLogEntryTypes .

            IF SessionManager:ServerCustomLogEntries > "":U THEN
                ASSIGN LogManager:CustomLogEntries = SessionManager:ServerCustomLogEntries .

            IF SessionManager:ServerDebugMode <> ? THEN
                ASSIGN FrameworkSettings:DebugMode = SessionManager:ServerDebugMode .

            IF FrameworkSettings:DebugMode AND LOOKUP ("ProVersion":U, LogManager:CustomLogEntries) > 0 THEN DO:
                LogManager:WriteFormattedMessage ("[ProVersion] AppServer OpenEdge Version: &1":U, ContextWrapper:AppServerProVersion) .
                LogManager:WriteFormattedMessage ("[ProVersion] Client    OpenEdge Version: &1":U, ContextWrapper:ClientProVersion) .
            END.

            IF LENGTH (SessionManager:ClientPrincipal, "RAW":U) > 0 THEN DO ON ERROR UNDO, THROW:
                CREATE CLIENT-PRINCIPAL hPrincipal .
                hPrincipal:IMPORT-PRINCIPAL (SessionManager:ClientPrincipal) .

                SET-DB-CLIENT (hPrincipal, cAuthenticationDB) .

                IF FrameworkSettings:DebugMode THEN
                    LogManager:WriteFormattedMessage ("[ActivateServerSession] Autenticated as &1 / &2 / &3":U,
&IF NOT PROVERSION BEGINS "10.2":U &THEN
                                                      hPrincipal:QUALIFIED-USER-ID,
&ELSE
                                                      hPrincipal:USER-ID,
&ENDIF
                                                      hPrincipal:DOMAIN-TYPE,
                                                      hPrincipal:STATE-DETAIL) .

                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hPrincipal) .
                END FINALLY.
            END.
            ELSE
                OnPerformTechnicalLogin (Consultingwerk.EventArgs:Empty) .
        END.
        ELSE DO:
            /* Mike Fechner, Consultingwerk Ltd. 20.08.2013
               Don't write message to the AppServer logfile when invoked from the AppServer startup
               procedure - as it's expected that there is no ContextDataset in that case */
            IF NOT ErrorHelper:CurrentStacktrace MATCHES "*as_startup*":U THEN DO:
                LogManager:WriteMessage ("Unable to Activate Session without ContextDataset!!!"{&TRAN}) .
&IF DEFINED (AblReflection) NE 0 &THEN
                LogManager:WriteObjectProperties (SESSION:CURRENT-REQUEST-INFO, "SESSION:CURRENT-REQUEST-INFO":U) .
&ENDIF
                LogManager:WriteStackTrace() .
            END.

            THIS-OBJECT:DeactivateServerSession (e) .
        END.

        IF FrameworkSettings:DebugMode AND NOT ErrorHelper:CurrentStacktrace MATCHES "*as_startup*":U THEN
            SessionManager:LogSessionManagerProperties () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deactivates a server session
        Notes:
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeactivateServerSession (e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE hPrincipal AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cPropath   AS CHARACTER NO-UNDO .

        ASSIGN
            SessionManager:ClientPrincipal              = ?
            SessionManager:LanguageDisplayName          = ?
            SessionManager:LanguageIsoCode              = ?
            SessionManager:LanguageKey                  = ?
            SessionManager:LoginCompanyKey              = ?
            SessionManager:LoginCompanyName             = ?
            SessionManager:LoginCompanyReferenceChar    = ?
            SessionManager:LoginCompanyReferenceDecimal = ?
            SessionManager:LoginCompanyReferenceInteger = ?
            SessionManager:LoginCompanyShortName        = ?
            SessionManager:ServerLogEntryTypes          = ?
            SessionManager:UserName                     = ?
            SessionManager:UserKey                      = ?
            .

        IF cLogEntryTypes <> LOG-MANAGER:LOG-ENTRY-TYPES THEN
            LOG-MANAGER:LOG-ENTRY-TYPES = cLogEntryTypes .

        IF lDebugMode <> FrameworkSettings:DebugMode THEN
            FrameworkSettings:DebugMode = lDebugMode .

        IF cCustomLogEntries <> LogManager:CustomLogEntries THEN
            LogManager:CustomLogEntries = cCustomLogEntries .

&IF NOT PROVERSION BEGINS "10.2":U &THEN
        /* become nobody on the DB */
        CREATE CLIENT-PRINCIPAL hPrincipal.

        hPrincipal:INITIALIZE ("nobody":U) .

        hPrincipal:SEAL ("":U) .

        SET-DB-CLIENT (hPrincipal, cAuthenticationDB) .
&ENDIF

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hPrincipal) .

            /* Mike Fechner, Consultingwerk Ltd. 21.08.2013
               Restore startup propath */
            ASSIGN cPropath = FrameworkSettings:StartupPropath .

            IF cPropath > "":U THEN
                PROPATH = cPropath .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the PerformTechnicalLogin event
        Notes:
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPerformTechnicalLogin (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:PerformTechnicalLogin:Publish (THIS-OBJECT, e) .

    END METHOD .

END CLASS.
